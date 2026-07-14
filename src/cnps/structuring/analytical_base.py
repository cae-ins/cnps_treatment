"""
Analytical base construction.

Merges individual-level and firm-level data into a single analytical
dataset suitable for weighted estimation.  Each row is an individual
observation enriched with firm-level weights, declaration indicators,
and all analytical dimensions.

References
----------
Brick, J. M. & Kalton, G. (1996). Handling missing data in survey research.
    *Statistical Methods in Medical Research*, 5(3), 215-238.
"""

from __future__ import annotations

import polars as pl
from loguru import logger

from cnps.config import PipelineConfig
from cnps.storage import read_parquet, write_parquet
from cnps.storage.minio_client import object_exists


def build_analytical_base(cfg: PipelineConfig) -> str:
    """
    Merge individual and firm bases into the analytical base.

    Parameters
    ----------
    cfg : PipelineConfig
        Pipeline configuration.

    Returns
    -------
    str
        Object name of the analytical base Parquet file on MinIO.
    """
    indiv_object = f"{cfg.minio.cleaned_prefix}individual_base.parquet"
    firm_object = f"{cfg.minio.cleaned_prefix}firm_base.parquet"

    for obj, label in [(indiv_object, "Individual"), (firm_object, "Firm")]:
        if not object_exists(cfg.minio, obj):
            raise FileNotFoundError(f"{label} base not found: {obj}")

    indiv = read_parquet(cfg.minio, indiv_object)
    firm = read_parquet(cfg.minio, firm_object)

    logger.info("Merging individual ({} rows) with firm ({} rows)",
                indiv.height, firm.height)

    # Select firm-level columns to join (avoid duplicates)
    indiv_cols = set(indiv.columns)
    firm_join_cols = ["NUMERO_EMPLOYEUR", "PERIOD"]
    firm_value_cols = [
        c for c in firm.columns
        if c not in indiv_cols or c in firm_join_cols
    ]

    firm_subset = firm.select(
        [c for c in firm_value_cols if c in firm.columns]
    )

    # Join
    join_on = [c for c in firm_join_cols if c in indiv.columns and c in firm_subset.columns]
    if join_on:
        analytical = indiv.join(firm_subset, on=join_on, how="left")
    else:
        logger.warning("No common join keys found. Returning individual base as-is.")
        analytical = indiv

    # Compute final weight placeholder (updated after modeling)
    analytical = analytical.with_columns(
        (pl.col("W_JT") * pl.col("W_INDIV")).alias("W_FINAL")
    )

    # Validate dimensions are present
    enabled_dims = [d for d in cfg.dimensions if d.enabled and d.group_by]
    for dim in enabled_dims:
        for col in dim.group_by:
            if col not in analytical.columns:
                logger.warning("Dimension column '{}' ({}) not found in analytical base",
                               col, dim.label)

    out_object = f"{cfg.minio.cleaned_prefix}analytical_base.parquet"
    write_parquet(cfg.minio, out_object, analytical)
    logger.info("Analytical base: {} rows, {} cols -> {}",
                analytical.height, analytical.width, out_object)

    return out_object
