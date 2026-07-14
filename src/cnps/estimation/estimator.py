"""
Main estimation engine.

Computes all configured statistics across all analytical dimensions,
optionally combining results from multiple imputations via Rubin's rules.

This is the central estimation module that orchestrates:
1. Loading the analytical base (or imputed bases)
2. Iterating over enabled dimensions
3. Computing all statistics per group
4. Combining across imputations (if applicable)
5. Applying suppression rules for small cells
6. Returning structured results

References
----------
Lumley, T. (2010). *Complex Surveys: A Guide to Analysis Using R*. Wiley.
Heeringa, S. G., West, B. T. & Berglund, P. A. (2017). *Applied Survey
    Data Analysis* (2nd ed.). Chapman & Hall/CRC.
"""

from __future__ import annotations

import numpy as np
import polars as pl
from loguru import logger

from cnps.config import PipelineConfig, DimensionDef, StatDef
from cnps.storage import read_parquet
from cnps.storage.minio_client import object_exists
from cnps.estimation.weighted_stats import compute_statistic, weighted_variance
from cnps.estimation.confidence_intervals import combine_rubin, RubinResult


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

def _estimate_group(
    df: pl.DataFrame,
    salary_col: str,
    weight_col: str,
    statistics: list[StatDef],
    min_cell: int,
) -> dict[str, float | None]:
    """Compute all statistics for a single group."""
    y = df[salary_col].to_numpy().astype(float)
    w = df[weight_col].to_numpy().astype(float)

    n_obs = int(np.sum(np.isfinite(y) & (w > 0)))
    n_weighted = float(np.sum(w[np.isfinite(y) & (w > 0)]))

    # Suppress if below minimum cell size
    if n_weighted < min_cell:
        return {s.name: None for s in statistics}

    results: dict[str, float | None] = {}
    for stat in statistics:
        try:
            val = compute_statistic(stat.function, y, w, stat.params)
            results[stat.name] = val
        except Exception:
            results[stat.name] = None

    return results


def _estimate_dimension(
    df: pl.DataFrame,
    dim: DimensionDef,
    salary_col: str,
    weight_col: str,
    statistics: list[StatDef],
    min_cell: int,
) -> list[dict]:
    """Compute statistics for all groups within a dimension."""
    rows: list[dict] = []

    if not dim.group_by:
        # Global dimension: entire dataset
        results = _estimate_group(df, salary_col, weight_col, statistics, min_cell)
        rows.append({"dimension": dim.label, "group": "Total", **results})
    else:
        # Group by dimension columns
        group_cols = [c for c in dim.group_by if c in df.columns]
        if not group_cols:
            logger.warning("Dimension '{}': columns {} not found", dim.name, dim.group_by)
            return rows

        for group_vals, group_df in df.group_by(group_cols):
            if isinstance(group_vals, tuple):
                group_label = " / ".join(str(v) for v in group_vals)
            else:
                group_label = str(group_vals)

            results = _estimate_group(
                group_df, salary_col, weight_col, statistics, min_cell,
            )
            rows.append({"dimension": dim.label, "group": group_label, **results})

    return rows


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def estimate_all(cfg: PipelineConfig) -> pl.DataFrame:
    """
    Run the full estimation across all dimensions and statistics.

    If multiple imputations exist, estimates are combined via Rubin's rules.

    Parameters
    ----------
    cfg : PipelineConfig
        Pipeline configuration.

    Returns
    -------
    pl.DataFrame
        Results with columns: dimension, group, + one column per statistic.
    """
    analytical_object = f"{cfg.minio.cleaned_prefix}analytical_base.parquet"
    imputed_object = f"{cfg.minio.cleaned_prefix}firm_base_imputed.parquet"

    salary_col = "SALAIRE_BRUT_MENS" if True else "SALAIRE_BRUT"  # Default
    weight_col = "W_FINAL"
    min_cell = cfg.estimation.min_cell_size
    statistics = cfg.statistics
    enabled_dims = [d for d in cfg.dimensions if d.enabled]

    # Check for multiple imputations
    has_imputations = object_exists(cfg.minio, imputed_object)

    if has_imputations:
        logger.info("Multiple imputations detected, using Rubin's combination rules")
        return _estimate_with_imputations(
            cfg, imputed_object, analytical_object,
            salary_col, weight_col, min_cell, statistics, enabled_dims,
        )

    # Single dataset estimation
    if not object_exists(cfg.minio, analytical_object):
        raise FileNotFoundError(f"Analytical base not found: {analytical_object}")

    df = read_parquet(cfg.minio, analytical_object)

    # Check salary column availability
    if salary_col not in df.columns:
        salary_col = "SALAIRE_BRUT" if "SALAIRE_BRUT" in df.columns else None
        if salary_col is None:
            raise ValueError("No salary column found in analytical base")

    logger.info("Estimating {} statistics across {} dimensions on {} rows",
                len(statistics), len(enabled_dims), df.height)

    all_rows: list[dict] = []
    for dim in enabled_dims:
        rows = _estimate_dimension(df, dim, salary_col, weight_col, statistics, min_cell)
        all_rows.extend(rows)
        logger.debug("  {} '{}': {} groups", dim.name, dim.label, len(rows))

    result = pl.DataFrame(all_rows)
    logger.info("Estimation complete: {} rows", result.height)
    return result


def _estimate_with_imputations(
    cfg: PipelineConfig,
    imputed_object: str,
    analytical_object: str,
    salary_col: str,
    weight_col: str,
    min_cell: int,
    statistics: list[StatDef],
    enabled_dims: list[DimensionDef],
) -> pl.DataFrame:
    """Estimate with Rubin's combination across imputations."""
    imputed = read_parquet(cfg.minio, imputed_object)

    if "IMPUTATION_ID" not in imputed.columns:
        logger.warning("No IMPUTATION_ID found, treating as single imputation")
        imputed = imputed.with_columns(pl.lit(1).cast(pl.Int32).alias("IMPUTATION_ID"))

    m_values = sorted(imputed["IMPUTATION_ID"].unique().to_list())
    M = len(m_values)
    logger.info("Combining {} imputations via Rubin's rules", M)

    # For each imputation, estimate all dimensions
    imputation_results: list[list[dict]] = []

    for m in m_values:
        df_m = imputed.filter(pl.col("IMPUTATION_ID") == m)

        # Check salary column
        s_col = salary_col if salary_col in df_m.columns else "SALAIRE_MOYEN"
        if s_col not in df_m.columns:
            continue

        w_col = weight_col if weight_col in df_m.columns else "W_JT"
        if w_col not in df_m.columns:
            df_m = df_m.with_columns(pl.lit(1.0).alias(w_col))

        rows_m: list[dict] = []
        for dim in enabled_dims:
            rows = _estimate_dimension(df_m, dim, s_col, w_col, statistics, min_cell)
            rows_m.extend(rows)

        imputation_results.append(rows_m)

    if not imputation_results:
        return pl.DataFrame()

    # Combine via Rubin's rules
    # Build a lookup: (dimension, group) -> {stat_name: [values across imputations]}
    from collections import defaultdict
    combined: dict[tuple[str, str], dict[str, list[float]]] = defaultdict(
        lambda: defaultdict(list)
    )

    for rows_m in imputation_results:
        for row in rows_m:
            key = (row["dimension"], row["group"])
            for stat in statistics:
                val = row.get(stat.name)
                if val is not None:
                    combined[key][stat.name].append(val)

    # Build final result
    final_rows: list[dict] = []
    for (dim_label, group), stat_values in combined.items():
        row = {"dimension": dim_label, "group": group}
        for stat in statistics:
            vals = stat_values.get(stat.name, [])
            if len(vals) == M:
                # Apply Rubin's rules
                # Use simple variance estimate for within-imputation
                variances = [0.0] * len(vals)  # Simplified; ideal: bootstrap within
                rubin = combine_rubin(vals, variances, cfg.estimation.confidence_level)
                row[stat.name] = rubin.estimate
                row[f"{stat.name}_ci_lower"] = rubin.ci_lower
                row[f"{stat.name}_ci_upper"] = rubin.ci_upper
            elif vals:
                row[stat.name] = float(np.mean(vals))
            else:
                row[stat.name] = None
        final_rows.append(row)

    result = pl.DataFrame(final_rows)
    logger.info("Rubin estimation complete: {} rows", result.height)
    return result
