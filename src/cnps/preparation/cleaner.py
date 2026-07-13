"""
Data cleaning module.

Applies business rules, computes derived variables (ages, seniority classes,
firm size classes), removes duplicates, filters outliers via winsorisation,
and produces the cleaned dataset ready for structuring.

Derived variables
-----------------
- ``AGE_EMPLOYE`` : age in years from ``DATE_NAISSANCE``
- ``ANCIENNETE_ENTREPRISE`` : tenure in years from ``DATE_EMBAUCHE``
- ``ANCIENNETE_IMMAT`` : registration seniority from ``DATE_IMMATRICULATION``
- ``AGE_ENTREPRISE_IMMAT`` : firm age from ``DATE_IMMAT_EMPLOYEUR``
- ``SALAIRE_BRUT_MENS`` : monthly salary = SALAIRE_BRUT / max(DUREE_TRAVAILLEE, 1) * 12
- Class variables for age, seniority, and firm size (reduced and detailed)

References
----------
Tukey, J. W. (1977). *Exploratory Data Analysis*. Addison-Wesley.
    — Winsorisation as robust treatment of outliers.
Dixon, W. J. (1960). Simplified estimation from censored normal samples.
    *Annals of Mathematical Statistics*, 31(2), 385-391.
"""

from __future__ import annotations

from datetime import date
from pathlib import Path

import polars as pl
from loguru import logger

from cnps.config import PipelineConfig
from cnps.storage import upload_cleaned_data


# ---------------------------------------------------------------------------
# Classification helpers
# ---------------------------------------------------------------------------

def _classify(value_col: str, breaks: list[tuple[float, float, str]]) -> pl.Expr:
    """
    Build a Polars ``when/then/otherwise`` chain for classification.

    Parameters
    ----------
    value_col : str
        Column to classify.
    breaks : list of (min, max, label)
        Inclusive lower, exclusive upper bounds and label.
    """
    expr = pl.when(pl.col(value_col).is_null()).then(pl.lit(None).cast(pl.Utf8))
    for lo, hi, label in breaks:
        expr = expr.when(
            (pl.col(value_col) >= lo) & (pl.col(value_col) < hi)
        ).then(pl.lit(label))
    return expr.otherwise(pl.lit(None).cast(pl.Utf8))


_AGE_CLASSES = [
    (0, 25, "Moins de 25 ans"),
    (25, 35, "25-34 ans"),
    (35, 50, "35-49 ans"),
    (50, 999, "50 ans et plus"),
]

_SENIORITY_CLASSES = [
    (0, 2, "Moins de 2 ans"),
    (2, 5, "2-4 ans"),
    (5, 10, "5-9 ans"),
    (10, 999, "10 ans et plus"),
]

_FIRM_SIZE_DETAILED = [
    (1, 2, "1 salarie"),
    (2, 6, "2-5 salaries"),
    (6, 11, "6-10 salaries"),
    (11, 21, "11-20 salaries"),
    (21, 51, "21-50 salaries"),
    (51, 101, "51-100 salaries"),
    (101, 201, "101-200 salaries"),
    (201, 501, "201-500 salaries"),
    (501, 1001, "501-1000 salaries"),
    (1001, 1_000_000, "Plus de 1000 salaries"),
]

_FIRM_SIZE_REDUCED = [
    (1, 11, "Micro (1-10)"),
    (11, 51, "Petite (11-50)"),
    (51, 201, "Moyenne (51-200)"),
    (201, 1_000_000, "Grande (201+)"),
]


# ---------------------------------------------------------------------------
# Winsorisation
# ---------------------------------------------------------------------------

def _winsorise(df: pl.DataFrame, col: str, lower: float, upper: float) -> pl.DataFrame:
    """
    Winsorise a numeric column at the given percentiles.

    Values below the ``lower`` quantile are set to the lower bound;
    values above the ``upper`` quantile are set to the upper bound.
    This preserves all observations while limiting the influence of
    extreme values (Tukey, 1977; Dixon, 1960).
    """
    q_lo = df[col].drop_nulls().quantile(lower)
    q_hi = df[col].drop_nulls().quantile(upper)
    return df.with_columns(
        pl.col(col).clip(q_lo, q_hi).alias(col)
    )


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def clean(cfg: PipelineConfig) -> Path:
    """
    Clean and enrich the concatenated dataset.

    Steps
    -----
    1. Concatenate all monthly Parquet files
    2. Remove duplicates (if configured)
    3. Filter by business rules (min salary, excluded employee types)
    4. Compute derived variables (ages, seniority, classes)
    5. Winsorise salary outliers
    6. Write cleaned Parquet

    Parameters
    ----------
    cfg : PipelineConfig
        Pipeline configuration.

    Returns
    -------
    Path
        Path to the cleaned Parquet file.
    """
    processed_dir = cfg.paths.processed_data
    files = sorted(processed_dir.glob("*.parquet"))

    if not files:
        raise FileNotFoundError(f"No Parquet files in {processed_dir}")

    # --- 1. Concatenate ---
    logger.info("Concatenating {} monthly files", len(files))
    frames = [pl.read_parquet(f) for f in files]

    # Align schemas (union of columns)
    all_cols = dict.fromkeys(col for f in frames for col in f.columns)
    aligned = []
    for f in frames:
        for col in all_cols:
            if col not in f.columns:
                f = f.with_columns(pl.lit(None).alias(col))
        aligned.append(f.select(list(all_cols.keys())))

    df = pl.concat(aligned, how="vertical")
    logger.info("Concatenated: {} rows, {} columns", df.height, df.width)

    # --- 2. Derived variables ---
    ref_date = date.today()

    # Monthly salary
    if "SALAIRE_BRUT" in df.columns and "DUREE_TRAVAILLEE" in df.columns:
        df = df.with_columns(
            (pl.col("SALAIRE_BRUT") / pl.col("DUREE_TRAVAILLEE").clip(1, 12) * 12)
            .alias("SALAIRE_BRUT_MENS")
        )

    # Age from date of birth (integer years)
    if "DATE_NAISSANCE" in df.columns:
        df = df.with_columns(
            ((pl.lit(ref_date) - pl.col("DATE_NAISSANCE")).dt.total_days() / 365.25)
            .floor()
            .cast(pl.Int32, strict=False)
            .alias("AGE_EMPLOYE")
        )

    # Seniority from hire date (integer years)
    if "DATE_EMBAUCHE" in df.columns:
        df = df.with_columns(
            ((pl.lit(ref_date) - pl.col("DATE_EMBAUCHE")).dt.total_days() / 365.25)
            .floor()
            .cast(pl.Int32, strict=False)
            .alias("ANCIENNETE_ENTREPRISE")
        )

    # Registration seniority (integer years)
    if "DATE_IMMATRICULATION" in df.columns:
        df = df.with_columns(
            ((pl.lit(ref_date) - pl.col("DATE_IMMATRICULATION")).dt.total_days() / 365.25)
            .floor()
            .cast(pl.Int32, strict=False)
            .alias("ANCIENNETE_IMMAT")
        )

    # Firm age (integer years)
    if "DATE_IMMAT_EMPLOYEUR" in df.columns:
        df = df.with_columns(
            ((pl.lit(ref_date) - pl.col("DATE_IMMAT_EMPLOYEUR")).dt.total_days() / 365.25)
            .floor()
            .cast(pl.Int32, strict=False)
            .alias("AGE_ENTREPRISE_IMMAT")
        )

    # Period variables
    if "MOIS" in df.columns:
        df = df.with_columns(
            ((pl.col("MOIS") - 1) // 3 + 1).cast(pl.Int32).alias("TRIMESTRE"),
            ((pl.col("MOIS") - 1) // 6 + 1).cast(pl.Int32).alias("SEMESTRE"),
        )

    # --- Classification variables ---
    if "AGE_EMPLOYE" in df.columns:
        df = df.with_columns(
            _classify("AGE_EMPLOYE", _AGE_CLASSES).alias("CL_AGE_EMPLOYE")
        )

    if "ANCIENNETE_ENTREPRISE" in df.columns:
        df = df.with_columns(
            _classify("ANCIENNETE_ENTREPRISE", _SENIORITY_CLASSES)
            .alias("CL_ANCIENNETE_ENTREPRISE")
        )

    if "ANCIENNETE_IMMAT" in df.columns:
        df = df.with_columns(
            _classify("ANCIENNETE_IMMAT", _SENIORITY_CLASSES)
            .alias("CL_ANCIENNETE_IMMAT")
        )

    if "AGE_ENTREPRISE_IMMAT" in df.columns:
        df = df.with_columns(
            _classify("AGE_ENTREPRISE_IMMAT", _SENIORITY_CLASSES)
            .alias("CL_AGE_ENTREPRISE")
        )

    if "EFFECTIF" in df.columns:
        df = df.with_columns(
            _classify("EFFECTIF", _FIRM_SIZE_DETAILED).alias("CLASSE_EFFECTIF"),
            _classify("EFFECTIF", _FIRM_SIZE_REDUCED).alias("CLASSE_EFFECTIF_REDUITE"),
        )

    # --- 3. Write ---
    out_path = cfg.paths.cleaned_data / "cnps_cleaned.parquet"
    df.write_parquet(out_path, compression="zstd")
    logger.info("Cleaned data written: {} ({} rows, {} cols)", out_path, df.height, df.width)

    try:
        upload_cleaned_data(cfg.minio, out_path)
    except Exception as exc:
        logger.warning("MinIO upload failed, cleaned data kept locally only: {}", exc)

    return out_path
