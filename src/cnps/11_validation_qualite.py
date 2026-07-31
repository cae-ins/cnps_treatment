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

from cnps.config import PipelineConfig, load_config
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

    # Meme ordre de preference que les etapes 04, 05, 09 et 10 : la validation
    # doit porter sur la variable effectivement utilisee pour les estimations.
    # SALAIRE_BRUT_MENS est une variable historique, non winsorisee et sans
    # conversion de periodicite : la controler produisait des avertissements
    # sur des valeurs qu'aucune etape n'exploite.
    salary_col = next(
        (c for c in ("SALAIRE_BRUT_ESTIME_AU_MOIS", "SALAIRE_BRUT_MENS", "SALAIRE_BRUT")
         if c in df.columns),
        "SALAIRE_BRUT",
    )
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

    # --- Modele de declaration individuelle q_ijt (etape 07b, annexe 3) ---
    # Sans ce controle, une AUC insuffisante sur le second etage passerait
    # inapercue alors qu'elle concerne 65,3% des salaires manquants.
    decl_indiv_object = f"{cfg.minio.models_prefix}declaration_indiv_model.pkl"
    if object_exists(cfg.minio, models_bucket, decl_indiv_object):
        model_data = read_pickle(cfg.minio, models_bucket, decl_indiv_object)
        auc_indiv = model_data.get("auc", 0)
        taux = model_data.get("taux_declare")
        if auc_indiv < cfg.modeling.min_auc:
            report.issues.append(ValidationIssue(
                "WARNING", "model", "low_auc_indiv",
                f"AUC du modele de declaration individuelle={auc_indiv:.4f} "
                f"< {cfg.modeling.min_auc} : les poids W_INDIV corrigent mal la "
                "non-declaration partielle, enrichir les covariables",
            ))
        else:
            report.issues.append(ValidationIssue(
                "INFO", "model", "auc_indiv_ok",
                f"AUC du modele de declaration individuelle={auc_indiv:.4f}"
                + (f" (taux de declaration observe={taux:.2%})" if taux else ""),
            ))
    else:
        report.issues.append(ValidationIssue(
            "WARNING", "model", "no_declaration_indiv_model",
            "Modele de declaration individuelle introuvable : l'etape 07b n'a pas "
            "ete executee, le second etage de l'annexe 3 est inactif (W_INDIV=1)",
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
    for issue in combined.issues:
        level = issue.level if issue.level in ("INFO", "WARNING", "ERROR") else "INFO"
        logger.log(level, "  [{}/{}] {}: {}", issue.level, issue.stage, issue.check, issue.message)

    return combined


if __name__ == "__main__":
    import argparse
    import sys
    from pathlib import Path

    parser = argparse.ArgumentParser(
        description=__doc__.strip().splitlines()[0] if __doc__ else None
    )
    parser.add_argument("--settings", "-s", type=Path, default=None)
    parser.add_argument("--dimensions", "-d", type=Path, default=None)
    parser.add_argument("--verbose", "-v", action="store_true")
    args = parser.parse_args()

    cfg = load_config(args.settings, args.dimensions)

    logger.remove()
    logger.add(
        sys.stderr,
        level="DEBUG" if args.verbose else "INFO",
        colorize=True,
        format="<green>{time:HH:mm:ss}</green> | <level>{level:<8}</level> | {message}",
    )
    logger.add(
        str(cfg.paths.logs / f"{Path(__file__).stem}.log"),
        level="DEBUG", rotation="10 MB", retention="30 days", encoding="utf-8",
    )

    try:
        # results=None : validation donnees + modeles uniquement. Chainer
        # l'estimation de l'etape 10 ici defeaterait l'interet d'isoler
        # l'etape 11 (voir cas particulier etape 12 pour ce chainage).
        # valider_tout logue deja chaque issue en detail, rien a ajouter ici.
        valider_tout(cfg)
        logger.info("Termine avec succes.")
    except Exception as exc:
        logger.exception("Echec de l'etape: {}", exc)
        sys.exit(1)
