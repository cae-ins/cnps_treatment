"""
Firm-time base construction.

Aggregates individual-level data to firm-period level and constructs
a balanced panel including non-declaring firm-periods (with D_jt = 0).

This balanced panel is essential for the declaration model: we need to
observe both declaring and non-declaring firm-periods to estimate the
propensity score (probability of declaration).

Key aggregates per firm-period:
- Mean, median, total, SD of salary
- Workforce composition (% female, mean age, mean tenure)
- Headcount (observed vs declared)

References
----------
Heckman, J. J. (1979). Sample selection bias as a specification error.
    *Econometrica*, 47(1), 153-161.
Wooldridge, J. M. (2007). Inverse probability weighted estimation for
    general missing data problems. *Journal of Econometrics*, 141(2), 1281-1301.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl
from loguru import logger

from cnps.config import PipelineConfig


def build_firm_base(cfg: PipelineConfig) -> Path:
    """
    Build the firm-time panel base.

    Steps
    -----
    1. Read individual base
    2. Aggregate to firm-period level
    3. Create balanced panel (cross-join of all firms x all periods)
    4. Flag non-declaring firm-periods (D_jt = 0)
    5. Add lagged variables for the declaration model
    6. Write firm base

    Parameters
    ----------
    cfg : PipelineConfig
        Pipeline configuration.

    Returns
    -------
    Path
        Path to the firm base Parquet file.
    """
    indiv_path = cfg.paths.cleaned_data / "individual_base.parquet"
    if not indiv_path.exists():
        raise FileNotFoundError(f"Individual base not found: {indiv_path}")

    df = pl.read_parquet(indiv_path)
    logger.info("Building firm base from {} individual records", df.height)

    # --- Aggregation to firm-period level ---
    group_cols = [c for c in ["NUMERO_EMPLOYEUR", "PERIOD", "MOIS", "ANNEE"]
                  if c in df.columns]

    salary_col = "SALAIRE_BRUT_MENS" if "SALAIRE_BRUT_MENS" in df.columns else "SALAIRE_BRUT"

    agg_exprs = [
        pl.len().alias("EFFECTIF_OBSERVE"),
    ]

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

    # Carry forward firm-level attributes (take first value)
    firm_attrs = [c for c in [
        "SECTEUR_ACTIVITE_COD", "COMMUNE", "CLASSE_EFFECTIF",
        "CLASSE_EFFECTIF_REDUITE", "AGE_ENTREPRISE_IMMAT", "CL_AGE_ENTREPRISE",
    ] if c in df.columns]

    for attr in firm_attrs:
        agg_exprs.append(pl.col(attr).first().alias(attr))

    firm_df = df.group_by(group_cols).agg(agg_exprs)
    logger.info("Aggregated to {} firm-period records", firm_df.height)

    # --- Balanced panel ---
    if "NUMERO_EMPLOYEUR" in firm_df.columns and "PERIOD" in firm_df.columns:
        all_firms = firm_df.select("NUMERO_EMPLOYEUR").unique()
        all_periods = firm_df.select(
            [c for c in ["PERIOD", "MOIS", "ANNEE"] if c in firm_df.columns]
        ).unique()

        balanced = all_firms.join(all_periods, how="cross")

        # Left join to identify non-declaring
        join_cols = [c for c in ["NUMERO_EMPLOYEUR", "PERIOD"] if c in firm_df.columns]
        firm_df = balanced.join(firm_df, on=join_cols, how="left")

        # Declaration indicator
        firm_df = firm_df.with_columns(
            pl.when(pl.col("EFFECTIF_OBSERVE").is_not_null())
            .then(pl.lit(1))
            .otherwise(pl.lit(0))
            .cast(pl.Int8)
            .alias("D_JT")
        )

        logger.info("Balanced panel: {} rows ({} declaring, {} non-declaring)",
                     firm_df.height,
                     firm_df.filter(pl.col("D_JT") == 1).height,
                     firm_df.filter(pl.col("D_JT") == 0).height)

    # --- Initialize firm weights ---
    firm_df = firm_df.with_columns(pl.lit(1.0).alias("W_JT"))

    # --- Log salary for modeling ---
    if "SALAIRE_MOYEN" in firm_df.columns:
        firm_df = firm_df.with_columns(
            pl.col("SALAIRE_MOYEN").log().alias("LOG_SALAIRE_MOYEN")
        )

    # --- Lagged variables ---
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

        # Past declaration rate (cumulative mean of D_JT)
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

    out_path = cfg.paths.cleaned_data / "firm_base.parquet"
    firm_df.write_parquet(out_path, compression="zstd")
    logger.info("Firm base: {} rows -> {}", firm_df.height, out_path)

    return out_path
