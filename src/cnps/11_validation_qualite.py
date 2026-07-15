"""
Etape 11/12 — Validation qualite.

Fournit trois niveaux de controles qualite executes en fin de pipeline :

1. **Validation des donnees** — coherence du jeu de donnees nettoye
   (lignes, colonnes requises, plage de salaire, doublons, taux de nulls)
2. **Diagnostics des modeles** — AUC du modele de declaration, R2 du
   modele d'imputation, distribution des poids
3. **Validation de l'estimation** — plausibilite des indicateurs finaux

Chaque controle retourne un rapport structure (liste de problemes) qui
peut etre exporte en Excel (etape 12) ou simplement journalise.

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
from cnps.storage import object_exists, read_parquet, read_pickle


@dataclass
class ValidationIssue:
    """Un probleme de validation individuel."""
    level: str              # "ERROR", "WARNING", "INFO"
    stage: str              # "data", "model", "estimation"
    check: str               # nom du controle
    message: str             # description lisible
    details: dict = field(default_factory=dict)


@dataclass
class ValidationReport:
    """Ensemble de problemes de validation."""
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
        return f"[{status}] {n_err} erreurs, {n_warn} avertissements, {n_info} infos"


# ---------------------------------------------------------------------------
# 1. Validation des donnees
# ---------------------------------------------------------------------------

def valider_donnees(cfg: PipelineConfig) -> ValidationReport:
    """Execute les controles qualite sur le jeu de donnees nettoye."""
    report = ValidationReport()
    bucket = cfg.minio.cleaned_bucket
    cleaned_object = f"{cfg.minio.cleaned_prefix}cnps_cleaned.parquet"

    if not object_exists(cfg.minio, bucket, cleaned_object):
        report.issues.append(ValidationIssue(
            "ERROR", "data", "file_exists",
            f"Donnees nettoyees introuvables : {bucket}/{cleaned_object}",
        ))
        return report

    df = read_parquet(cfg.minio, bucket, cleaned_object)

    if df.height == 0:
        report.issues.append(ValidationIssue(
            "ERROR", "data", "empty_data", "Le jeu de donnees nettoye a 0 ligne",
        ))
        return report

    report.issues.append(ValidationIssue(
        "INFO", "data", "row_count", f"{df.height:,} lignes",
    ))

    required = ["ID_INDIV", "ID_EMPLOYEUR", "PERIOD"]
    for col in required:
        if col not in df.columns:
            report.issues.append(ValidationIssue(
                "WARNING", "data", "missing_column", f"Colonne requise '{col}' absente",
            ))

    salary_col = "SALAIRE_BRUT_MENS" if "SALAIRE_BRUT_MENS" in df.columns else "SALAIRE_BRUT"
    if salary_col in df.columns:
        lo, hi = cfg.estimation.salary_plausible_range

        below = df.filter(pl.col(salary_col) < lo).height
        above = df.filter(pl.col(salary_col) > hi).height
        if below > 0:
            report.issues.append(ValidationIssue(
                "WARNING", "data", "salary_below_min",
                f"{below:,} lignes avec {salary_col} < {lo:,.0f}",
            ))
        if above > 0:
            report.issues.append(ValidationIssue(
                "WARNING", "data", "salary_above_max",
                f"{above:,} lignes avec {salary_col} > {hi:,.0f}",
            ))

    id_cols = [c for c in ["ID_INDIV", "ID_EMPLOYEUR", "PERIOD"] if c in df.columns]
    if id_cols:
        n_unique = df.select(id_cols).unique().height
        n_dup = df.height - n_unique
        if n_dup > 0:
            report.issues.append(ValidationIssue(
                "WARNING", "data", "duplicates",
                f"{n_dup:,} lignes dupliquees sur {id_cols}",
            ))

    for col in df.columns:
        null_rate = df[col].null_count() / df.height
        if null_rate > 0.5:
            report.issues.append(ValidationIssue(
                "WARNING", "data", "high_null_rate",
                f"Colonne '{col}' a {null_rate:.1%} de valeurs manquantes",
            ))

    logger.info("Validation des donnees : {}", report.summary())
    return report


# ---------------------------------------------------------------------------
# 2. Diagnostics des modeles
# ---------------------------------------------------------------------------

def valider_modeles(cfg: PipelineConfig) -> ValidationReport:
    """Execute les diagnostics sur les modeles sauvegardes."""
    report = ValidationReport()

    models_bucket = cfg.minio.models_bucket
    decl_object = f"{cfg.minio.models_prefix}declaration_model.pkl"
    if object_exists(cfg.minio, models_bucket, decl_object):
        model_data = read_pickle(cfg.minio, models_bucket, decl_object)

        auc = model_data.get("auc", 0)
        if auc < cfg.modeling.min_auc:
            report.issues.append(ValidationIssue(
                "WARNING", "model", "low_auc",
                f"AUC du modele de declaration={auc:.4f} < {cfg.modeling.min_auc}",
            ))
        else:
            report.issues.append(ValidationIssue(
                "INFO", "model", "auc_ok", f"AUC du modele de declaration={auc:.4f}",
            ))
    else:
        report.issues.append(ValidationIssue(
            "WARNING", "model", "no_declaration_model", "Modele de declaration introuvable",
        ))

    imp_object = f"{cfg.minio.models_prefix}imputation_model.pkl"
    if object_exists(cfg.minio, models_bucket, imp_object):
        model_data = read_pickle(cfg.minio, models_bucket, imp_object)
        r2 = model_data.get("r_squared", 0)
        report.issues.append(ValidationIssue(
            "INFO", "model", "imputation_r2", f"R2 du modele d'imputation={r2:.4f}",
        ))
    else:
        report.issues.append(ValidationIssue(
            "WARNING", "model", "no_imputation_model", "Modele d'imputation introuvable",
        ))

    firm_object = f"{cfg.minio.cleaned_prefix}firm_base.parquet"
    if object_exists(cfg.minio, cfg.minio.cleaned_bucket, firm_object):
        df = read_parquet(cfg.minio, cfg.minio.cleaned_bucket, firm_object)
        if "W_JT" in df.columns:
            w = df["W_JT"].drop_nulls().to_numpy()
            cv = float(np.std(w) / np.mean(w)) if np.mean(w) > 0 else np.inf
            if cv > 2.0:
                report.issues.append(ValidationIssue(
                    "WARNING", "model", "high_weight_cv",
                    f"CV des poids={cv:.2f} (variance elevee, possible instabilite)",
                ))
            report.issues.append(ValidationIssue(
                "INFO", "model", "weight_stats",
                f"Poids : moyenne={np.mean(w):.3f}, mediane={np.median(w):.3f}, "
                f"plage=[{np.min(w):.3f}, {np.max(w):.3f}]",
            ))

    logger.info("Validation des modeles : {}", report.summary())
    return report


# ---------------------------------------------------------------------------
# 3. Validation de l'estimation
# ---------------------------------------------------------------------------

def valider_estimation(cfg: PipelineConfig, results: pl.DataFrame) -> ValidationReport:
    """Execute les controles de plausibilite sur les resultats d'estimation."""
    report = ValidationReport()

    if results.height == 0:
        report.issues.append(ValidationIssue(
            "ERROR", "estimation", "empty_results", "Aucun resultat d'estimation",
        ))
        return report

    report.issues.append(ValidationIssue(
        "INFO", "estimation", "result_count",
        f"{results.height} lignes de resultats sur toutes les dimensions",
    ))

    lo, hi = cfg.estimation.salary_plausible_range
    if "mean" in results.columns:
        means = results["mean"].drop_nulls()
        outliers = means.filter((means < lo) | (means > hi))
        if outliers.len() > 0:
            report.issues.append(ValidationIssue(
                "WARNING", "estimation", "implausible_means",
                f"{outliers.len()} groupes avec salaire moyen hors [{lo:,.0f}, {hi:,.0f}]",
            ))

    if "n_weighted" in results.columns:
        suppressed = results.filter(pl.col("n_weighted").is_null()).height
        if suppressed > 0:
            report.issues.append(ValidationIssue(
                "INFO", "estimation", "suppressed_cells",
                f"{suppressed} cellules supprimees (n_weighted < {cfg.estimation.min_cell_size})",
            ))

    logger.info("Validation de l'estimation : {}", report.summary())
    return report


def valider_tout(
    cfg: PipelineConfig,
    results: pl.DataFrame | None = None,
) -> ValidationReport:
    """Execute tous les controles de validation et retourne un rapport combine."""
    combined = ValidationReport()

    for validator in [valider_donnees, valider_modeles]:
        report = validator(cfg)
        combined.issues.extend(report.issues)

    if results is not None:
        report = valider_estimation(cfg, results)
        combined.issues.extend(report.issues)

    logger.info("Validation complete : {}", combined.summary())
    return combined
