"""
Etape 4/12 — Construction de la base individus.

Cree le jeu de donnees analytique au niveau individu a partir des donnees
nettoyees (etape 3). Chaque ligne represente une observation
individu-employeur-periode.

La base individus est le socle pour :
- L'estimation des poids IPW au niveau individu
- L'analyse de la distribution des salaires au niveau travailleur

References
----------
Wooldridge, J. M. (2010). *Econometric Analysis of Cross Section and Panel
    Data* (2nd ed.). MIT Press. — Chapitre 19 sur la ponderation par
    probabilite inverse (IPW).
"""

from __future__ import annotations

import polars as pl
from loguru import logger

from cnps.config import PipelineConfig, load_config
from cnps.storage import object_exists, read_parquet, write_parquet


def construire_base_individus(cfg: PipelineConfig) -> str:
    """
    Construit la base individus a partir des donnees nettoyees.

    Etapes
    ------
    1. Lecture du Parquet nettoye
    2. Creation d'un identifiant d'observation unique (ID_INDIV + ID_EMPLOYEUR + PERIOD)
    3. Initialisation des poids individuels a 1
    4. Ecriture de la base individus

    Parameters
    ----------
    cfg : PipelineConfig
        Configuration du pipeline.

    Returns
    -------
    str
        Nom de l'objet Parquet de la base individus sur MinIO.
    """
    bucket = cfg.minio.cleaned_bucket
    cleaned_object = f"{cfg.minio.cleaned_prefix}cnps_cleaned.parquet"
    if not object_exists(cfg.minio, bucket, cleaned_object):
        raise FileNotFoundError(f"Donnees nettoyees introuvables : {bucket}/{cleaned_object}")

    df = read_parquet(cfg.minio, bucket, cleaned_object)
    logger.info("Construction de la base individus a partir de {} lignes", df.height)

    # Identifiant d'observation
    id_parts = [c for c in ["ID_INDIV", "ID_EMPLOYEUR", "PERIOD"] if c in df.columns]
    if id_parts:
        df = df.with_columns(
            pl.concat_str(id_parts, separator="_").alias("OBS_ID")
        )
        logger.info("Identifiant OBS_ID cree a partir de {}", id_parts)
    else:
        logger.warning("Aucune colonne d'identifiant trouvee, OBS_ID non cree")

    # --- Indicateur de declaration individuelle S_IJT (annexe 3) ---
    #
    # S_IJT = 1 si le salaire de CE salarie est effectivement renseigne ce
    # mois-la, 0 sinon. C'est la variable expliquee du modele q_ijt de
    # l'etape 07b : parmi les salaries d'une entreprise qui a declare, lesquels
    # ont reellement un salaire ?
    #
    # Auparavant fixe a 1 pour toutes les lignes, ce qui rendait le second
    # etage inoperant (W_INDIV restait a 1, et W_FINAL = W_JT x 1). Un salaire
    # nul ou negatif ne compte pas comme declare : ce n'est pas une remuneration
    # exploitable (meme convention que le nettoyage de l'etape 03 et les
    # controles d'audit).
    _salary_candidates = [
        c for c in ("SALAIRE_BRUT_ESTIME_AU_MOIS", "SALAIRE_BRUT_MENS", "SALAIRE_BRUT")
        if c in df.columns
    ]
    if _salary_candidates:
        salary_col = _salary_candidates[0]
        df = df.with_columns(
            (pl.col(salary_col).is_not_null() & (pl.col(salary_col) > 0))
            .cast(pl.Int8)
            .alias("S_IJT")
        )
        n_declare = int(df["S_IJT"].sum())
        logger.info(
            "S_IJT calcule sur {} : {} lignes declarees sur {} ({:.2f}%), "
            "{} sans salaire exploitable",
            salary_col, n_declare, df.height,
            n_declare / df.height * 100 if df.height else 0.0,
            df.height - n_declare,
        )
    else:
        df = df.with_columns(pl.lit(1).cast(pl.Int8).alias("S_IJT"))
        logger.warning(
            "Aucune colonne de salaire trouvee : S_IJT force a 1 pour toutes les "
            "lignes. Le modele individuel de l'etape 07b sera inoperant."
        )

    # Poids individuel initial. Reste a 1.0 jusqu'a l'etape 07b, qui le
    # remplace par 1/q_ijt pour les salaries des entreprises declarantes.
    df = df.with_columns(pl.lit(1.0).alias("W_INDIV"))

    # Sous-variables de periode (si absentes)
    derived_period_cols = []
    for col in ["TRIMESTRE", "SEMESTRE"]:
        if col not in df.columns and "MOIS" in df.columns:
            diviseur = 3 if col == "TRIMESTRE" else 6
            df = df.with_columns(
                ((pl.col("MOIS") - 1) // diviseur + 1).cast(pl.Int32).alias(col)
            )
            derived_period_cols.append(col)
    if derived_period_cols:
        logger.info("Colonnes de periode derivees : {}", derived_period_cols)

    n_employeurs = df["ID_EMPLOYEUR"].n_unique() if "ID_EMPLOYEUR" in df.columns else None
    n_periodes = df["PERIOD"].n_unique() if "PERIOD" in df.columns else None

    out_object = f"{cfg.minio.cleaned_prefix}individual_base.parquet"
    write_parquet(cfg.minio, bucket, out_object, df)
    logger.info(
        "Base individus : {} lignes, {} colonnes, {} employeurs distincts, {} periodes -> {}",
        df.height, df.width, n_employeurs, n_periodes, out_object,
    )

    return out_object


if __name__ == "__main__":
    import argparse
    import sys
    from pathlib import Path

    parser = argparse.ArgumentParser(
        description=__doc__.strip().splitlines()[0] if __doc__ else None
    )
    parser.add_argument("--settings", "-s", type=Path, default=None)
    parser.add_argument("--dimensions", "-d", type=Path, default=None)
    parser.add_argument("--verbose", "-v", action="store_true")
    args = parser.parse_args()

    cfg = load_config(args.settings, args.dimensions)

    logger.remove()
    logger.add(
        sys.stderr,
        level="DEBUG" if args.verbose else "INFO",
        colorize=True,
        format="<green>{time:HH:mm:ss}</green> | <level>{level:<8}</level> | {message}",
    )
    logger.add(
        str(cfg.paths.logs / f"{Path(__file__).stem}.log"),
        level="DEBUG", rotation="10 MB", retention="30 days", encoding="utf-8",
    )

    try:
        construire_base_individus(cfg)
        logger.info("Termine avec succes.")
    except Exception as exc:
        logger.exception("Echec de l'etape: {}", exc)
        sys.exit(1)
