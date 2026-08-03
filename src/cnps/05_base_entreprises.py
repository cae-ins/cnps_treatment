"""
Etape 5/12 — Construction de la base entreprises.

Agrege les donnees individus (etape 4) au niveau entreprise-periode et
construit un panel equilibre incluant les entreprises non-declarantes
(D_jt = 0). Ce panel equilibre est indispensable pour le modele de
declaration (etape 7) : il faut observer aussi bien les periodes
declarees que non-declarees pour estimer le score de propension.

Agregats calcules par entreprise-periode
-----------------------------------------
- Moyenne, mediane, total, ecart-type du salaire
- Composition des effectifs (% femmes, age moyen, anciennete moyenne)
- Effectif observe vs declare

References
----------
Heckman, J. J. (1979). Sample selection bias as a specification error.
    *Econometrica*, 47(1), 153-161.
Wooldridge, J. M. (2007). Inverse probability weighted estimation for
    general missing data problems. *Journal of Econometrics*, 141(2), 1281-1301.
"""

from __future__ import annotations

import polars as pl
from loguru import logger

from cnps.config import PipelineConfig, load_config
from cnps.firm_panel import construire_panel_risque
from cnps.storage import object_exists, read_parquet, write_parquet


def construire_base_entreprises(cfg: PipelineConfig) -> str:
    """
    Construit le panel entreprise-periode.

    Etapes
    ------
    1. Lecture de la base individus
    2. Agregation au niveau entreprise-periode
    3. Construction du panel equilibre (produit cartesien entreprises x periodes)
    4. Marquage des periodes non-declarees (D_jt = 0)
    5. Ajout des variables retardees pour le modele de declaration
    6. Ecriture de la base entreprises

    Parameters
    ----------
    cfg : PipelineConfig
        Configuration du pipeline.

    Returns
    -------
    str
        Nom de l'objet Parquet de la base entreprises sur MinIO.
    """
    bucket = cfg.minio.cleaned_bucket
    indiv_object = f"{cfg.minio.cleaned_prefix}individual_base.parquet"
    if not object_exists(cfg.minio, bucket, indiv_object):
        raise FileNotFoundError(f"Base individus introuvable : {bucket}/{indiv_object}")

    df = read_parquet(cfg.minio, bucket, indiv_object)
    logger.info(
        "Construction de la base entreprises a partir de {} enregistrements individus", df.height
    )

    # --- Agregation au niveau entreprise-periode ---
    group_cols = [c for c in ["ID_EMPLOYEUR", "PERIOD", "MOIS", "ANNEE"] if c in df.columns]

    # Variable de salaire de reference, par ordre de preference. Doit rester
    # alignee sur celle de l'etape 04 (calcul de S_IJT) et de l'etape 10
    # (estimation) : si l'agregat entreprise et S_IJT reposaient sur des
    # variables differentes, S_IJT pourrait valoir 1 alors que le salaire
    # agrege est nul sur la meme ligne.
    #
    # SALAIRE_BRUT_ESTIME_AU_MOIS est prefere a SALAIRE_BRUT_MENS : il traite
    # chaque periodicite avec sa propre conversion et ne depend pas de
    # DUREE_TRAVAILLEE, incoherente sur 69% des horaires (audit 28/07/2026).
    _salary_candidates = [
        c
        for c in (
            "SALAIRE_BRUT_ESTIME_AU_MOIS_W",
            "SALAIRE_BRUT_ESTIME_AU_MOIS",
            "SALAIRE_BRUT_MENS",
            "SALAIRE_BRUT",
        )
        if c in df.columns
    ]
    salary_col = _salary_candidates[0] if _salary_candidates else "SALAIRE_BRUT"
    logger.info("Colonne de salaire retenue pour l'agregation : {}", salary_col)

    agg_exprs = [pl.len().alias("EFFECTIF_OBSERVE")]

    if salary_col in df.columns:
        agg_exprs.extend(
            [
                pl.col(salary_col).mean().alias("SALAIRE_MOYEN"),
                pl.col(salary_col).median().alias("SALAIRE_MEDIAN"),
                pl.col(salary_col).sum().alias("MASSE_SALARIALE"),
                pl.col(salary_col).std().alias("SALAIRE_SD"),
                # Effectif dont le salaire est reellement renseigne (et positif).
                # A distinguer de EFFECTIF_OBSERVE, qui compte les LIGNES presentes
                # dans le fichier, salaire renseigne ou non : c'est cet ecart qui
                # caracterise une declaration partielle (cf. annexe 3).
                (
                    (pl.col(salary_col).is_not_null() & (pl.col(salary_col) > 0))
                    .sum()
                    .cast(pl.Int64)
                    .alias("EFFECTIF_DECLARE")
                ),
            ]
        )

    if "SEXE" in df.columns:
        agg_exprs.append((pl.col("SEXE").cast(pl.Utf8) == "F").mean().alias("PCT_FEMMES"))

    if "AGE_EMPLOYE" in df.columns:
        agg_exprs.append(pl.col("AGE_EMPLOYE").mean().alias("AGE_MOYEN"))

    if "ANCIENNETE_ENTREPRISE" in df.columns:
        agg_exprs.append(pl.col("ANCIENNETE_ENTREPRISE").mean().alias("ANCIENNETE_MOYENNE"))

    # Valeurs du mois, propagees uniquement vers les mois futurs dans le panel.
    firm_attrs = [
        c
        for c in [
            "DATE_IMMAT_EMPLOYEUR",
            "SECTEUR_ACTIVITE",
            "COMMUNE",
            "CLASSE_EFFECTIF",
            "CLASSE_EFFECTIF_REDUITE",
        ]
        if c in df.columns
    ]

    for attr in firm_attrs:
        agg_exprs.append(pl.col(attr).drop_nulls().last().alias(attr))
    logger.info("Attributs entreprise reportes : {}", firm_attrs)

    firm_df = df.group_by(group_cols).agg(agg_exprs)
    logger.info("Agrege en {} enregistrements entreprise-periode", firm_df.height)

    # La fin commune reste celle du panel; K definit seulement la portee.
    # Aucune absence de declaration n'est interpretee comme une cessation.
    firm_df = construire_panel_risque(firm_df, cfg)

    # --- Log salaire pour la modelisation ---
    if "SALAIRE_MOYEN" in firm_df.columns:
        firm_df = firm_df.with_columns(pl.col("SALAIRE_MOYEN").log().alias("LOG_SALAIRE_MOYEN"))

    # --- Variables retardees ---
    if "ID_EMPLOYEUR" in firm_df.columns and "PERIOD" in firm_df.columns:
        firm_df = firm_df.sort(["ID_EMPLOYEUR", "PERIOD"])

        for col_name in ["D_JT", "SALAIRE_MOYEN", "EFFECTIF_OBSERVE"]:
            if col_name in firm_df.columns:
                firm_df = firm_df.with_columns(
                    pl.col(col_name).shift(1).over("ID_EMPLOYEUR").alias(f"LAG_{col_name}")
                )

        # Le taux passe ne compte que les mois appartenant a la portee K.
        # Les mois anterieurs a la premiere entree dans le champ ne sont pas
        # interpretes comme des non-reponses.
        if {"D_JT", "DANS_UNIVERS_RISQUE"} <= set(firm_df.columns):
            past_sum = (
                (pl.col("D_JT") * pl.col("DANS_UNIVERS_RISQUE"))
                .cum_sum()
                .over("ID_EMPLOYEUR")
                .shift(1)
                .over("ID_EMPLOYEUR")
            )
            past_count = (
                pl.col("DANS_UNIVERS_RISQUE")
                .cum_sum()
                .over("ID_EMPLOYEUR")
                .shift(1)
                .over("ID_EMPLOYEUR")
            )
            firm_df = firm_df.with_columns(
                pl.when(past_count > 0)
                .then(past_sum / past_count)
                .otherwise(None)
                .alias("TAUX_DECLARATION_PASSE")
            )
        logger.info(
            "Variables retardees ajoutees : LAG_D_JT, LAG_SALAIRE_MOYEN, "
            "LAG_EFFECTIF_OBSERVE, TAUX_DECLARATION_PASSE"
        )

    out_object = f"{cfg.minio.cleaned_prefix}firm_base.parquet"
    write_parquet(cfg.minio, bucket, out_object, firm_df)
    logger.info(
        "Base entreprises : {} lignes, {} colonnes -> {}", firm_df.height, firm_df.width, out_object
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
        level="DEBUG",
        rotation="10 MB",
        retention="30 days",
        encoding="utf-8",
    )

    try:
        construire_base_entreprises(cfg)
        logger.info("Termine avec succes.")
    except Exception as exc:
        logger.exception("Echec de l'etape: {}", exc)
        sys.exit(1)
