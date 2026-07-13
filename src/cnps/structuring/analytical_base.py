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

from pathlib import Path

import polars as pl
from loguru import logger

from cnps.config import PipelineConfig


def build_analytical_base(cfg: PipelineConfig) -> Path:
    """
    Merge individual and firm bases into the analytical base.

    Parameters
    ----------
    cfg : PipelineConfig
        Pipeline configuration.

    Returns
    -------
    Path
        Path to the analytical base Parquet file.
    """
    indiv_path = cfg.paths.cleaned_data / "individual_base.parquet"
    firm_path = cfg.paths.cleaned_data / "firm_base.parquet"

    for p, label in [(indiv_path, "Individual"), (firm_path, "Firm")]:
        if not p.exists():
            raise FileNotFoundError(f"{label} base not found: {p}")

    indiv = pl.read_parquet(indiv_path)
    firm = pl.read_parquet(firm_path)

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

    out_path = cfg.paths.cleaned_data / "analytical_base.parquet"
    analytical.write_parquet(out_path, compression="zstd")
    logger.info("Analytical base: {} rows, {} cols -> {}",
                analytical.height, analytical.width, out_path)

    return out_path
