"""
Type harmonisation module.

Reads raw Parquet files produced by ingestion and enforces a consistent
schema across all monthly files.  Handles:

- Column name normalisation (uppercase, strip whitespace)
- Date parsing from multiple formats (string, Excel serial, ISO)
- Numeric coercion for salary and duration columns
- Categorical encoding for classification columns

References
----------
Van Buuren, S. (2018). *Flexible Imputation of Missing Data* (2nd ed.).
    Chapman & Hall/CRC. — Chapter 2 on data preprocessing for imputation.
"""

from __future__ import annotations

import re
from datetime import datetime

import polars as pl
from loguru import logger

from cnps.config import PipelineConfig
from cnps.storage import list_objects, read_parquet, write_parquet

# ---------------------------------------------------------------------------
# Column schema definition
# ---------------------------------------------------------------------------

# Columns and their target types.  Columns not listed here are kept as-is.
_NUMERIC_COLS = [
    "SALAIRE_BRUT", "DUREE_TRAVAILLEE", "SALAIRE_BRUT_MENS",
    "EFFECTIF", "EFFECTIF_DECLARE", "EFFECTIF_SALARIES",
]

_DATE_COLS = [
    "DATE_NAISSANCE", "DATE_EMBAUCHE", "DATE_IMMATRICULATION",
    "DATE_IMMAT_EMPLOYEUR",
]

_ID_COLS = [
    "ID_INDIV", "ID_EMPLOI", "NUMERO_EMPLOYEUR",
]

# Excel serial date origin
_EXCEL_ORIGIN = datetime(1899, 12, 30)


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

def _normalize_column_names(df: pl.DataFrame) -> pl.DataFrame:
    """Uppercase, strip whitespace, replace spaces with underscores."""
    rename_map = {
        col: col.strip().upper().replace(" ", "_")
        for col in df.columns
    }
    return df.rename(rename_map)


_INTEGER_COLS = ["EFFECTIF", "EFFECTIF_DECLARE", "EFFECTIF_SALARIES"]


def _coerce_numeric(df: pl.DataFrame, cols: list[str]) -> pl.DataFrame:
    """Cast columns to Float64 (or Int64 for count columns), coercing non-numeric values to null."""
    for col in cols:
        if col in df.columns:
            df = df.with_columns(
                pl.col(col).cast(pl.Utf8).str.replace_all(r"[^\d.\-]", "")
                .cast(pl.Float64, strict=False)
                .alias(col)
            )
            if col in _INTEGER_COLS:
                df = df.with_columns(pl.col(col).cast(pl.Int64, strict=False).alias(col))
    return df


def _parse_date_column(series: pl.Series) -> pl.Series:
    """
    Attempt to parse a date column from multiple formats:
    1. Already Date/Datetime
    2. Duration type (Excel serial dates read as durations)
    3. ISO format string (YYYY-MM-DD)
    4. French format string (DD/MM/YYYY)
    5. Numeric Excel serial number

    Returns a Date series.
    """
    name = series.name
    _MS_PER_DAY = 86_400_000  # milliseconds in one day

    # If already a date type, return as-is
    if series.dtype in (pl.Date, pl.Datetime):
        return series.cast(pl.Date)

    # If Duration type (e.g. Excel serial dates stored as Duration("d")),
    # extract total days and convert to Date via millisecond Duration
    if series.dtype.base_type() == pl.Duration:
        temp = pl.DataFrame({name: series})
        result = temp.select(
            (pl.lit(_EXCEL_ORIGIN).cast(pl.Date)
             + (pl.col(name).dt.total_days() * _MS_PER_DAY)
               .cast(pl.Int64).cast(pl.Duration("ms")))
            .cast(pl.Date)
            .alias(name)
        )[name]
        return result

    # Cast to string for uniform processing
    s = series.cast(pl.Utf8)

    # Try each format in order, filling nulls progressively:
    #   1. ISO datetime  "YYYY-MM-DD HH:MM:SS"
    #   2. ISO date      "YYYY-MM-DD"
    #   3. French date   "DD/MM/YYYY"
    #   4. Excel serial number
    parsed = s.str.to_date("%Y-%m-%d %H:%M:%S", strict=False)

    still_null = parsed.is_null() & s.is_not_null()
    if still_null.any():
        iso_date = s.str.to_date("%Y-%m-%d", strict=False)
        parsed = parsed.zip_with(parsed.is_not_null(), iso_date)

    still_null = parsed.is_null() & s.is_not_null()
    if still_null.any():
        french = s.str.to_date("%d/%m/%Y", strict=False)
        parsed = parsed.zip_with(parsed.is_not_null(), french)

    # Try Excel serial dates for remaining nulls
    still_null = parsed.is_null() & s.is_not_null()
    if still_null.any():
        numeric = s.cast(pl.Float64, strict=False)
        # Convert days to milliseconds — Polars Duration only supports ns/us/ms
        temp = pl.DataFrame({"_parsed": parsed, "_days": numeric})
        excel_dates = temp.select(
            (pl.lit(_EXCEL_ORIGIN).cast(pl.Date)
             + (pl.col("_days") * _MS_PER_DAY)
               .cast(pl.Int64).cast(pl.Duration("ms")))
            .cast(pl.Date)
            .alias("_excel")
        )["_excel"]
        parsed = parsed.zip_with(parsed.is_not_null(), excel_dates)

    return parsed.alias(name)


def _coerce_dates(df: pl.DataFrame, cols: list[str]) -> pl.DataFrame:
    """Parse date columns from various formats."""
    for col in cols:
        if col in df.columns:
            try:
                df = df.with_columns(_parse_date_column(df[col]))
            except Exception as exc:
                logger.warning("Could not parse dates in column '{}': {}", col, exc)
    return df


def _ensure_string_ids(df: pl.DataFrame, cols: list[str]) -> pl.DataFrame:
    """Ensure identifier columns are strings."""
    for col in cols:
        if col in df.columns:
            df = df.with_columns(pl.col(col).cast(pl.Utf8).alias(col))
    return df


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def harmonize_types(cfg: PipelineConfig) -> list[str]:
    """
    Harmonise column types across all monthly Parquet files on MinIO.

    Reads each object from ``processed_prefix``, applies type coercion,
    and overwrites it in place.

    Parameters
    ----------
    cfg : PipelineConfig
        Pipeline configuration.

    Returns
    -------
    list[str]
        Object names of all harmonised Parquet files.
    """
    all_objects = list_objects(cfg.minio, cfg.minio.processed_prefix, recursive=False)
    files = sorted(obj for obj in all_objects if re.search(r"\.parquet$", obj))

    if not files:
        logger.warning("No Parquet files found under {}", cfg.minio.processed_prefix)
        return []

    result_objects: list[str] = []

    for object_name in files:
        logger.info("Harmonising types: {}", object_name)
        df = read_parquet(cfg.minio, object_name)

        df = _normalize_column_names(df)
        df = _ensure_string_ids(df, _ID_COLS)
        df = _coerce_numeric(df, _NUMERIC_COLS)
        df = _coerce_dates(df, _DATE_COLS)

        # Overwrite with harmonised types
        write_parquet(cfg.minio, object_name, df)
        result_objects.append(object_name)

        logger.debug("  {} -> {} rows, {} cols", object_name, df.height, df.width)

    logger.info("Type harmonisation complete for {} files", len(result_objects))
    return result_objects
