"""
Individual-level base construction.

Creates the individual-level analytical dataset from cleaned data.
Each row represents one individual-employer-period observation.

The individual base is the foundation for:
- Individual-level IPW weight estimation
- Salary distribution analysis at the worker level

References
----------
Wooldridge, J. M. (2010). *Econometric Analysis of Cross Section and Panel
    Data* (2nd ed.). MIT Press. — Chapter 19 on inverse probability weighting.
"""

from __future__ import annotations

from loguru import logger

from cnps.config import PipelineConfig
from cnps.storage import read_parquet, write_parquet
from cnps.storage.minio_client import object_exists


def build_individual_base(cfg: PipelineConfig) -> str:
    """
    Build the individual-level base from cleaned data.

    Steps
    -----
    1. Read cleaned Parquet
    2. Create unique observation ID (ID_INDIV + NUMERO_EMPLOYEUR + PERIOD)
    3. Initialise individual weights to 1
    4. Select and validate required columns
    5. Write individual base

    Parameters
    ----------
    cfg : PipelineConfig
        Pipeline configuration.

    Returns
    -------
    str
        Object name of the individual base Parquet file on MinIO.
    """
    cleaned_object = f"{cfg.minio.cleaned_prefix}cnps_cleaned.parquet"
    if not object_exists(cfg.minio, cleaned_object):
        raise FileNotFoundError(f"Cleaned data not found: {cleaned_object}")

    df = read_parquet(cfg.minio, cleaned_object)
    logger.info("Building individual base from {} rows", df.height)

    # Observation ID
    id_parts = []
    for col in ["ID_INDIV", "NUMERO_EMPLOYEUR", "PERIOD"]:
        if col in df.columns:
            id_parts.append(col)

    if id_parts:
        df = df.with_columns(
            pl.concat_str(id_parts, separator="_").alias("OBS_ID")
        )

    # Initialize weights
    df = df.with_columns(
        pl.lit(1.0).alias("W_INDIV"),
        pl.lit(1).cast(pl.Int8).alias("S_IJT"),  # observation indicator (1 = observed)
    )

    # Period sub-variables
    for col in ["TRIMESTRE", "SEMESTRE"]:
        if col not in df.columns and "MOIS" in df.columns:
            if col == "TRIMESTRE":
                df = df.with_columns(
                    ((pl.col("MOIS") - 1) // 3 + 1).cast(pl.Int32).alias(col)
                )
            else:
                df = df.with_columns(
                    ((pl.col("MOIS") - 1) // 6 + 1).cast(pl.Int32).alias(col)
                )

    out_object = f"{cfg.minio.cleaned_prefix}individual_base.parquet"
    write_parquet(cfg.minio, out_object, df)
    logger.info("Individual base: {} rows, {} cols -> {}", df.height, df.width, out_object)

    return out_object
