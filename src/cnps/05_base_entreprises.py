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

from cnps.config import PipelineConfig
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
    logger.info("Construction de la base entreprises a partir de {} enregistrements individus",
                df.height)

    # --- Agregation au niveau entreprise-periode ---
    group_cols = [c for c in ["NUMERO_EMPLOYEUR", "PERIOD", "MOIS", "ANNEE"]
                  if c in df.columns]

    salary_col = "SALAIRE_BRUT_MENS" if "SALAIRE_BRUT_MENS" in df.columns else "SALAIRE_BRUT"

    agg_exprs = [pl.len().alias("EFFECTIF_OBSERVE")]

    if salary_col in df.columns:
        agg_exprs.extend([
            pl.col(salary_col).mean().alias("SALAIRE_MOYEN"),
            pl.col(salary_col).median().alias("SALAIRE_MEDIAN"),
            pl.col(salary_col).sum().alias("MASSE_SALARIALE"),
            pl.col(salary_col).std().alias("SALAIRE_SD"),
        ])

    if "SEXE" in df.columns:
        agg_exprs.append(
            (pl.col("SEXE").cast(pl.Utf8) == "F").mean().alias("PCT_FEMMES")
        )

    if "AGE_EMPLOYE" in df.columns:
        agg_exprs.append(pl.col("AGE_EMPLOYE").mean().alias("AGE_MOYEN"))

    if "ANCIENNETE_ENTREPRISE" in df.columns:
        agg_exprs.append(
            pl.col("ANCIENNETE_ENTREPRISE").mean().alias("ANCIENNETE_MOYENNE")
        )

    # Attributs entreprise reportes tels quels (premiere valeur rencontree)
    firm_attrs = [c for c in [
        "SECTEUR_ACTIVITE_COD", "COMMUNE", "CLASSE_EFFECTIF",
        "CLASSE_EFFECTIF_REDUITE", "AGE_ENTREPRISE_IMMAT", "CL_AGE_ENTREPRISE",
    ] if c in df.columns]

    for attr in firm_attrs:
        agg_exprs.append(pl.col(attr).first().alias(attr))

    firm_df = df.group_by(group_cols).agg(agg_exprs)
    logger.info("Agrege en {} enregistrements entreprise-periode", firm_df.height)

    # --- Panel equilibre ---
    if "NUMERO_EMPLOYEUR" in firm_df.columns and "PERIOD" in firm_df.columns:
        all_firms = firm_df.select("NUMERO_EMPLOYEUR").unique()
        all_periods = firm_df.select(
            [c for c in ["PERIOD", "MOIS", "ANNEE"] if c in firm_df.columns]
        ).unique()

        balanced = all_firms.join(all_periods, how="cross")

        join_cols = [c for c in ["NUMERO_EMPLOYEUR", "PERIOD"] if c in firm_df.columns]
        firm_df = balanced.join(firm_df, on=join_cols, how="left")

        # Indicateur de declaration
        firm_df = firm_df.with_columns(
            pl.when(pl.col("EFFECTIF_OBSERVE").is_not_null())
            .then(pl.lit(1))
            .otherwise(pl.lit(0))
            .cast(pl.Int8)
            .alias("D_JT")
        )

        logger.info("Panel equilibre : {} lignes ({} declarantes, {} non-declarantes)",
                     firm_df.height,
                     firm_df.filter(pl.col("D_JT") == 1).height,
                     firm_df.filter(pl.col("D_JT") == 0).height)

    # --- Poids entreprise initiaux ---
    firm_df = firm_df.with_columns(pl.lit(1.0).alias("W_JT"))

    # --- Log salaire pour la modelisation ---
    if "SALAIRE_MOYEN" in firm_df.columns:
        firm_df = firm_df.with_columns(
            pl.col("SALAIRE_MOYEN").log().alias("LOG_SALAIRE_MOYEN")
        )

    # --- Variables retardees ---
    if "NUMERO_EMPLOYEUR" in firm_df.columns and "PERIOD" in firm_df.columns:
        firm_df = firm_df.sort(["NUMERO_EMPLOYEUR", "PERIOD"])

        for col_name in ["D_JT", "SALAIRE_MOYEN", "EFFECTIF_OBSERVE"]:
            if col_name in firm_df.columns:
                firm_df = firm_df.with_columns(
                    pl.col(col_name)
                    .shift(1)
                    .over("NUMERO_EMPLOYEUR")
                    .alias(f"LAG_{col_name}")
                )

        # Taux de declaration passe (moyenne cumulee de D_JT)
        if "D_JT" in firm_df.columns:
            firm_df = firm_df.with_columns(
                pl.col("D_JT")
                .cast(pl.Float64)
                .cum_mean()
                .over("NUMERO_EMPLOYEUR")
                .shift(1)
                .over("NUMERO_EMPLOYEUR")
                .alias("TAUX_DECLARATION_PASSE")
            )

    out_object = f"{cfg.minio.cleaned_prefix}firm_base.parquet"
    write_parquet(cfg.minio, bucket, out_object, firm_df)
    logger.info("Base entreprises : {} lignes -> {}", firm_df.height, out_object)

    return out_object
