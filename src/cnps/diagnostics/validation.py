"""
Diagnostics and validation module.

Provides three levels of quality checks:

1. **Data validation** — pre/post-cleaning consistency checks
2. **Model diagnostics** — AUC, calibration, weight distribution
3. **Estimation validation** — plausibility checks on final indicators

Each check returns a structured report (list of issues) that can be
exported to Excel or logged.

References
----------
Steyerberg, E. W. et al. (2010). Assessing the performance of prediction
    models: a framework for some traditional and novel measures.
    *Epidemiology*, 21(1), 128-138.
"""

from __future__ import annotations

from dataclasses import dataclass, field

import numpy as np
import polars as pl
from loguru import logger

from cnps.config import PipelineConfig
from cnps.storage import read_parquet, read_pickle
from cnps.storage.minio_client import object_exists


@dataclass
class ValidationIssue:
    """A single validation issue."""
    level: str              # "ERROR", "WARNING", "INFO"
    stage: str              # "data", "model", "estimation"
    check: str              # Name of the check
    message: str            # Human-readable description
    details: dict = field(default_factory=dict)


@dataclass
class ValidationReport:
    """Collection of validation issues."""
    issues: list[ValidationIssue] = field(default_factory=list)

    @property
    def errors(self) -> list[ValidationIssue]:
        return [i for i in self.issues if i.level == "ERROR"]

    @property
    def warnings(self) -> list[ValidationIssue]:
        return [i for i in self.issues if i.level == "WARNING"]

    @property
    def is_valid(self) -> bool:
        return len(self.errors) == 0

    def summary(self) -> str:
        n_err = len(self.errors)
        n_warn = len(self.warnings)
        n_info = len(self.issues) - n_err - n_warn
        status = "PASS" if self.is_valid else "FAIL"
        return f"[{status}] {n_err} errors, {n_warn} warnings, {n_info} info"


# ---------------------------------------------------------------------------
# Data validation
# ---------------------------------------------------------------------------

def validate_data(cfg: PipelineConfig) -> ValidationReport:
    """Run data quality checks on the cleaned dataset."""
    report = ValidationReport()
    cleaned_object = f"{cfg.minio.cleaned_prefix}cnps_cleaned.parquet"

    if not object_exists(cfg.minio, cleaned_object):
        report.issues.append(ValidationIssue(
            "ERROR", "data", "file_exists", f"Cleaned data not found: {cleaned_object}",
        ))
        return report

    df = read_parquet(cfg.minio, cleaned_object)

    # Check row count
    if df.height == 0:
        report.issues.append(ValidationIssue(
            "ERROR", "data", "empty_data", "Cleaned dataset has 0 rows",
        ))
        return report

    report.issues.append(ValidationIssue(
        "INFO", "data", "row_count", f"Dataset has {df.height:,} rows",
    ))

    # Check required columns
    required = ["ID_INDIV", "NUMERO_EMPLOYEUR", "PERIOD"]
    for col in required:
        if col not in df.columns:
            report.issues.append(ValidationIssue(
                "WARNING", "data", "missing_column", f"Required column '{col}' not found",
            ))

    # Check salary range
    salary_col = "SALAIRE_BRUT_MENS" if "SALAIRE_BRUT_MENS" in df.columns else "SALAIRE_BRUT"
    if salary_col in df.columns:
        lo, hi = cfg.estimation.salary_plausible_range
        stats = df[salary_col].describe()

        below = df.filter(pl.col(salary_col) < lo).height
        above = df.filter(pl.col(salary_col) > hi).height
        if below > 0:
            report.issues.append(ValidationIssue(
                "WARNING", "data", "salary_below_min",
                f"{below:,} rows with {salary_col} < {lo:,.0f}",
            ))
        if above > 0:
            report.issues.append(ValidationIssue(
                "WARNING", "data", "salary_above_max",
                f"{above:,} rows with {salary_col} > {hi:,.0f}",
            ))

    # Check for duplicates
    id_cols = [c for c in ["ID_INDIV", "NUMERO_EMPLOYEUR", "PERIOD"] if c in df.columns]
    if id_cols:
        n_unique = df.select(id_cols).unique().height
        n_dup = df.height - n_unique
        if n_dup > 0:
            report.issues.append(ValidationIssue(
                "WARNING", "data", "duplicates",
                f"{n_dup:,} duplicate rows on {id_cols}",
            ))

    # Check null rates
    for col in df.columns:
        null_rate = df[col].null_count() / df.height
        if null_rate > 0.5:
            report.issues.append(ValidationIssue(
                "WARNING", "data", "high_null_rate",
                f"Column '{col}' has {null_rate:.1%} nulls",
            ))

    logger.info("Data validation: {}", report.summary())
    return report


# ---------------------------------------------------------------------------
# Model diagnostics
# ---------------------------------------------------------------------------

def validate_models(cfg: PipelineConfig) -> ValidationReport:
    """Run diagnostics on saved models."""
    report = ValidationReport()

    # Declaration model
    decl_object = f"{cfg.minio.models_prefix}declaration_model.pkl"
    if object_exists(cfg.minio, decl_object):
        model_data = read_pickle(cfg.minio, decl_object)

        auc = model_data.get("auc", 0)
        if auc < cfg.modeling.min_auc:
            report.issues.append(ValidationIssue(
                "WARNING", "model", "low_auc",
                f"Declaration model AUC={auc:.4f} < {cfg.modeling.min_auc}",
            ))
        else:
            report.issues.append(ValidationIssue(
                "INFO", "model", "auc_ok",
                f"Declaration model AUC={auc:.4f}",
            ))
    else:
        report.issues.append(ValidationIssue(
            "WARNING", "model", "no_declaration_model",
            "Declaration model not found",
        ))

    # Imputation model
    imp_object = f"{cfg.minio.models_prefix}imputation_model.pkl"
    if object_exists(cfg.minio, imp_object):
        model_data = read_pickle(cfg.minio, imp_object)

        r2 = model_data.get("r_squared", 0)
        report.issues.append(ValidationIssue(
            "INFO", "model", "imputation_r2",
            f"Imputation model R2={r2:.4f}",
        ))
    else:
        report.issues.append(ValidationIssue(
            "WARNING", "model", "no_imputation_model",
            "Imputation model not found",
        ))

    # Weight distribution
    firm_object = f"{cfg.minio.cleaned_prefix}firm_base.parquet"
    if object_exists(cfg.minio, firm_object):
        df = read_parquet(cfg.minio, firm_object)
        if "W_JT" in df.columns:
            w = df["W_JT"].drop_nulls().to_numpy()
            cv = float(np.std(w) / np.mean(w)) if np.mean(w) > 0 else np.inf
            if cv > 2.0:
                report.issues.append(ValidationIssue(
                    "WARNING", "model", "high_weight_cv",
                    f"Weight CV={cv:.2f} (high variance may indicate instability)",
                ))
            report.issues.append(ValidationIssue(
                "INFO", "model", "weight_stats",
                f"Weights: mean={np.mean(w):.3f}, median={np.median(w):.3f}, "
                f"range=[{np.min(w):.3f}, {np.max(w):.3f}]",
            ))

    logger.info("Model validation: {}", report.summary())
    return report


# ---------------------------------------------------------------------------
# Estimation validation
# ---------------------------------------------------------------------------

def validate_estimation(
    cfg: PipelineConfig,
    results: pl.DataFrame,
) -> ValidationReport:
    """Run plausibility checks on estimation results."""
    report = ValidationReport()

    if results.height == 0:
        report.issues.append(ValidationIssue(
            "ERROR", "estimation", "empty_results", "No estimation results",
        ))
        return report

    report.issues.append(ValidationIssue(
        "INFO", "estimation", "result_count",
        f"{results.height} result rows across dimensions",
    ))

    # Check salary plausibility
    lo, hi = cfg.estimation.salary_plausible_range
    if "mean" in results.columns:
        means = results["mean"].drop_nulls()
        outliers = means.filter((means < lo) | (means > hi))
        if outliers.len() > 0:
            report.issues.append(ValidationIssue(
                "WARNING", "estimation", "implausible_means",
                f"{outliers.len()} groups with mean salary outside [{lo:,.0f}, {hi:,.0f}]",
            ))

    # Check suppressed cells
    if "n_weighted" in results.columns:
        suppressed = results.filter(pl.col("n_weighted").is_null()).height
        if suppressed > 0:
            report.issues.append(ValidationIssue(
                "INFO", "estimation", "suppressed_cells",
                f"{suppressed} cells suppressed (n_weighted < {cfg.estimation.min_cell_size})",
            ))

    logger.info("Estimation validation: {}", report.summary())
    return report


def run_all_validations(
    cfg: PipelineConfig,
    results: pl.DataFrame | None = None,
) -> ValidationReport:
    """Run all validation checks and return a combined report."""
    combined = ValidationReport()

    for validator in [validate_data, validate_models]:
        report = validator(cfg)
        combined.issues.extend(report.issues)

    if results is not None:
        report = validate_estimation(cfg, results)
        combined.issues.extend(report.issues)

    logger.info("Full validation: {}", combined.summary())
    return combined
