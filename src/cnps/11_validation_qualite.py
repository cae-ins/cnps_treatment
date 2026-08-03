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
from cnps.storage import object_exists, read_json, read_parquet


@dataclass
class ValidationIssue:
    """Un probleme de validation individuel."""

    level: str  # "ERROR", "WARNING", "INFO"
    stage: str  # "data", "model", "estimation"
    check: str  # nom du controle
    message: str  # description lisible
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
        report.issues.append(
            ValidationIssue(
                "ERROR",
                "data",
                "file_exists",
                f"Donnees nettoyees introuvables : {bucket}/{cleaned_object}",
            )
        )
        return report

    df = read_parquet(cfg.minio, bucket, cleaned_object)

    if df.height == 0:
        report.issues.append(
            ValidationIssue(
                "ERROR",
                "data",
                "empty_data",
                "Le jeu de donnees nettoye a 0 ligne",
            )
        )
        return report

    report.issues.append(
        ValidationIssue(
            "INFO",
            "data",
            "row_count",
            f"{df.height:,} lignes",
        )
    )

    required = ["ID_INDIV", "ID_EMPLOYEUR", "PERIOD"]
    for col in required:
        if col not in df.columns:
            report.issues.append(
                ValidationIssue(
                    "ERROR",
                    "data",
                    "missing_column",
                    f"Colonne requise '{col}' absente",
                )
            )

    # Meme ordre de preference que les etapes 04, 05, 09 et 10 : la validation
    # doit porter sur la variable effectivement utilisee pour les estimations.
    # SALAIRE_BRUT_MENS est une variable historique, non winsorisee et sans
    # conversion de periodicite : la controler produisait des avertissements
    # sur des valeurs qu'aucune etape n'exploite.
    salary_col = next(
        (
            c
            for c in ("SALAIRE_BRUT_ESTIME_AU_MOIS", "SALAIRE_BRUT_MENS", "SALAIRE_BRUT")
            if c in df.columns
        ),
        "SALAIRE_BRUT",
    )
    if salary_col in df.columns:
        lo, hi = cfg.estimation.salary_plausible_range

        below = df.filter(pl.col(salary_col) < lo).height
        above = df.filter(pl.col(salary_col) > hi).height
        if below > 0:
            report.issues.append(
                ValidationIssue(
                    "WARNING",
                    "data",
                    "salary_below_min",
                    f"{below:,} lignes avec {salary_col} < {lo:,.0f}",
                )
            )
        if above > 0:
            report.issues.append(
                ValidationIssue(
                    "WARNING",
                    "data",
                    "salary_above_max",
                    f"{above:,} lignes avec {salary_col} > {hi:,.0f}",
                )
            )

    id_cols = ["ID_INDIV", "ID_EMPLOYEUR", "PERIOD"]
    if set(id_cols) <= set(df.columns):
        complete = df.filter(pl.all_horizontal([pl.col(c).is_not_null() for c in id_cols]))
        n_unique = complete.select(id_cols).unique().height
        n_dup = complete.height - n_unique
        if n_dup > 0:
            report.issues.append(
                ValidationIssue(
                    "ERROR",
                    "data",
                    "duplicates_complete_keys",
                    f"{n_dup:,} lignes dupliquees sur des cles completes {id_cols}",
                )
            )
        incomplete = df.height - complete.height
        if incomplete:
            report.issues.append(
                ValidationIssue(
                    "INFO",
                    "data",
                    "incomplete_dedup_keys",
                    f"{incomplete:,} lignes ont une cle de deduplication incomplete; "
                    "elles sont conservees et doivent rester tracees.",
                )
            )

    for col in df.columns:
        null_rate = df[col].null_count() / df.height
        if null_rate > 0.5:
            report.issues.append(
                ValidationIssue(
                    "WARNING",
                    "data",
                    "high_null_rate",
                    f"Colonne '{col}' a {null_rate:.1%} de valeurs manquantes",
                )
            )

    logger.info("Validation des donnees : {}", report.summary())
    return report


# ---------------------------------------------------------------------------
# 2. Diagnostics des modeles
# ---------------------------------------------------------------------------


def _ajouter_diagnostics_modele_reponse(
    report: ValidationReport,
    model_data: dict,
    cfg: PipelineConfig,
    *,
    label: str,
    code_suffix: str,
) -> None:
    """Revalide le resume JSON d'un modele de reponse avant publication."""
    if model_data.get("schema_version") != 1:
        report.issues.append(
            ValidationIssue(
                "ERROR",
                "model",
                f"invalid_schema{code_suffix}",
                f"Version de schema invalide pour {label}.",
            )
        )
        return

    diagnostics = model_data.get("diagnostics_oof")
    required = {
        "auc",
        "calibration_in_large",
        "brier",
        "max_abs_smd",
        "propensity_min",
        "propensity_max",
        "n_splits",
    }
    if not isinstance(diagnostics, dict):
        missing = sorted(required)
    else:
        missing = sorted(required - set(diagnostics))
    if missing:
        report.issues.append(
            ValidationIssue(
                "ERROR",
                "model",
                f"missing_diagnostics{code_suffix}",
                f"Diagnostics OOF incomplets pour {label}: {', '.join(missing)}.",
            )
        )
        return

    try:
        auc = float(diagnostics["auc"])
        calibration_large = float(diagnostics["calibration_in_large"])
        brier = float(diagnostics["brier"])
        balance = float(diagnostics["max_abs_smd"])
        propensity_min = float(diagnostics["propensity_min"])
        propensity_max = float(diagnostics["propensity_max"])
        n_splits = int(diagnostics["n_splits"])
    except (TypeError, ValueError) as exc:
        report.issues.append(
            ValidationIssue(
                "ERROR",
                "model",
                f"invalid_diagnostics{code_suffix}",
                f"Diagnostics OOF non numeriques pour {label}: {exc}.",
            )
        )
        return

    numeric = np.array(
        [auc, calibration_large, brier, balance, propensity_min, propensity_max],
        dtype=float,
    )
    if not np.isfinite(numeric).all():
        report.issues.append(
            ValidationIssue(
                "ERROR",
                "model",
                f"nonfinite_diagnostics{code_suffix}",
                f"Diagnostics OOF non finis pour {label}.",
            )
        )
        return

    failures = []
    if not 0 <= auc <= 1:
        failures.append("AUC hors [0,1]")
    if not 0 <= brier <= 1:
        failures.append("Brier hors [0,1]")
    if not 0 < propensity_min <= propensity_max < 1:
        failures.append("propensions hors ]0,1[")
    if abs(calibration_large) > cfg.modeling.max_calibration_in_large:
        failures.append("calibration-in-the-large hors seuil")
    if balance > cfg.modeling.max_abs_smd:
        failures.append("equilibre SMD hors seuil")
    if n_splits < 2:
        failures.append("moins de deux plis OOF")
    if failures:
        report.issues.append(
            ValidationIssue(
                "ERROR",
                "model",
                f"failed_diagnostics{code_suffix}",
                f"{label}: " + ", ".join(failures) + ".",
            )
        )
        return

    auc_level = "WARNING" if auc < cfg.modeling.min_auc else "INFO"
    auc_code = "low_auc" if auc_level == "WARNING" else "auc_ok"
    report.issues.append(
        ValidationIssue(
            auc_level,
            "model",
            f"{auc_code}{code_suffix}",
            f"{label}: AUC OOF={auc:.4f} (descriptive, non bloquante), "
            f"Brier={brier:.4f}, SMD max={balance:.4f}, "
            f"calibration-large={calibration_large:.4f}.",
        )
    )


def valider_modeles(cfg: PipelineConfig) -> ValidationReport:
    """Execute les diagnostics sur les modeles sauvegardes."""
    report = ValidationReport()

    models_bucket = cfg.minio.models_bucket
    decl_object = f"{cfg.minio.models_prefix}declaration_model.json"
    if object_exists(cfg.minio, models_bucket, decl_object):
        model_data = read_json(cfg.minio, models_bucket, decl_object)
        _ajouter_diagnostics_modele_reponse(
            report,
            model_data,
            cfg,
            label="Modele de declaration entreprise",
            code_suffix="",
        )
    else:
        report.issues.append(
            ValidationIssue(
                "ERROR",
                "model",
                "no_declaration_model",
                "Modele de declaration introuvable",
            )
        )

    # --- Modele de declaration individuelle q_ijt (etape 07b, annexe 3) ---
    # L'AUC reste descriptive; recouvrement, calibration et equilibre sont
    # les controles methodologiques bloquants.
    decl_indiv_object = f"{cfg.minio.models_prefix}declaration_indiv_model.json"
    if object_exists(cfg.minio, models_bucket, decl_indiv_object):
        model_data = read_json(cfg.minio, models_bucket, decl_indiv_object)
        _ajouter_diagnostics_modele_reponse(
            report,
            model_data,
            cfg,
            label="Modele de declaration individuelle",
            code_suffix="_indiv",
        )
    else:
        report.issues.append(
            ValidationIssue(
                "ERROR",
                "model",
                "no_declaration_indiv_model",
                "Modele de declaration individuelle introuvable : l'etape 07b "
                "doit etre executee avant toute publication.",
            )
        )

    report.issues.append(
        ValidationIssue(
            "INFO",
            "model",
            "publication_point_only",
            "L'imputation salariale et les regles de Rubin sont hors du DAG de "
            "publication; aucune validation de modele d'imputation n'est requise.",
        )
    )

    firm_object = f"{cfg.minio.cleaned_prefix}firm_base.parquet"
    if object_exists(cfg.minio, cfg.minio.cleaned_bucket, firm_object):
        df = read_parquet(cfg.minio, cfg.minio.cleaned_bucket, firm_object)
        if "W_JT" in df.columns:
            w = df["W_JT"].drop_nulls().to_numpy()
            invalid = (~np.isfinite(w)) | (w <= 0)
            if w.size == 0 or invalid.any():
                report.issues.append(
                    ValidationIssue(
                        "ERROR",
                        "model",
                        "invalid_firm_weights",
                        "Poids entreprise absents, non finis ou non positifs.",
                    )
                )
            else:
                cv = float(np.std(w) / np.mean(w))
                if cv > 2.0:
                    report.issues.append(
                        ValidationIssue(
                            "WARNING",
                            "model",
                            "high_weight_cv",
                            f"CV des poids={cv:.2f} (variance elevee, possible instabilite)",
                        )
                    )
                report.issues.append(
                    ValidationIssue(
                        "INFO",
                        "model",
                        "weight_stats",
                        f"Poids entreprise : moyenne={np.mean(w):.3f}, "
                        f"mediane={np.median(w):.3f}, "
                        f"plage=[{np.min(w):.3f}, {np.max(w):.3f}]",
                    )
                )
        else:
            report.issues.append(
                ValidationIssue(
                    "ERROR",
                    "model",
                    "missing_firm_weights",
                    "W_JT absent de firm_base.",
                )
            )
    else:
        report.issues.append(
            ValidationIssue(
                "ERROR",
                "model",
                "missing_firm_base",
                "firm_base introuvable pour la validation des poids.",
            )
        )

    analytical_object = f"{cfg.minio.cleaned_prefix}analytical_base.parquet"
    if object_exists(cfg.minio, cfg.minio.cleaned_bucket, analytical_object):
        analytical = read_parquet(
            cfg.minio,
            cfg.minio.cleaned_bucket,
            analytical_object,
        )
        required_weights = {"W_FINAL", "W_FINAL_RAW", "D_JT", "S_IJT"}
        missing_weights = sorted(required_weights - set(analytical.columns))
        if missing_weights:
            report.issues.append(
                ValidationIssue(
                    "ERROR",
                    "model",
                    "missing_final_weights",
                    "Colonnes de poids final absentes: " + ", ".join(missing_weights),
                )
            )
        else:
            w_final = analytical["W_FINAL"].cast(pl.Float64, strict=False).to_numpy()
            w_raw = analytical["W_FINAL_RAW"].cast(pl.Float64, strict=False).to_numpy()
            d = analytical["D_JT"].to_numpy()
            s = analytical["S_IJT"].to_numpy()
            invalid_response = ~np.isin(d, [0, 1]) | ~np.isin(s, [0, 1])
            scope = (
                analytical["DANS_UNIVERS_RISQUE"].fill_null(0).to_numpy() == 1
                if "DANS_UNIVERS_RISQUE" in analytical.columns
                else np.ones(analytical.height, dtype=bool)
            )
            response = scope & (d == 1) & (s == 1)
            invalid_final = (
                ~np.isfinite(w_final) | ~np.isfinite(w_raw) | (w_final < 0) | (w_raw < 0)
            )
            mismatch = (response & (w_final <= 0)) | (~response & (w_final != 0))
            if invalid_final.any() or invalid_response.any() or mismatch.any():
                report.issues.append(
                    ValidationIssue(
                        "ERROR",
                        "model",
                        "invalid_final_weights",
                        f"Poids finaux invalides: {int(invalid_final.sum())} valeurs "
                        f"non finies/negatives, {int(invalid_response.sum())} facteurs "
                        f"D/S invalides, {int(mismatch.sum())} incoherences avec D*S.",
                    )
                )
            elif response.any():
                positive = w_final[response]
                raw_positive = w_raw[response]
                ess = float(positive.sum() ** 2 / np.sum(positive**2))
                trimmed_share = float(
                    np.mean(~np.isclose(positive, raw_positive, rtol=1e-12, atol=1e-12))
                )
                report.issues.append(
                    ValidationIssue(
                        "INFO",
                        "model",
                        "final_weight_stats",
                        f"Poids finaux positifs: n={positive.size:,}, "
                        f"moyenne={positive.mean():.3f}, mediane={np.median(positive):.3f}, "
                        f"max={positive.max():.3f}, ESS={ess:,.1f} "
                        f"({ess / positive.size:.1%}), part tronquee={trimmed_share:.2%}.",
                    )
                )
            else:
                report.issues.append(
                    ValidationIssue(
                        "ERROR",
                        "model",
                        "no_positive_final_weight",
                        "Aucun repondant avec poids final positif.",
                    )
                )
    else:
        report.issues.append(
            ValidationIssue(
                "ERROR",
                "model",
                "missing_analytical_base",
                "analytical_base introuvable pour la validation des poids finaux.",
            )
        )

    logger.info("Validation des modeles : {}", report.summary())
    return report


# ---------------------------------------------------------------------------
# 3. Validation de l'estimation
# ---------------------------------------------------------------------------


def valider_estimation(cfg: PipelineConfig, results: pl.DataFrame) -> ValidationReport:
    """Execute les controles de plausibilite sur les resultats d'estimation."""
    report = ValidationReport()

    if results.height == 0:
        report.issues.append(
            ValidationIssue(
                "ERROR",
                "estimation",
                "empty_results",
                "Aucun resultat d'estimation",
            )
        )
        return report

    report.issues.append(
        ValidationIssue(
            "INFO",
            "estimation",
            "result_count",
            f"{results.height} lignes de resultats sur toutes les dimensions",
        )
    )

    lo, hi = cfg.estimation.salary_plausible_range
    if "mean" in results.columns:
        means = results["mean"].drop_nulls()
        outliers = means.filter((means < lo) | (means > hi))
        if outliers.len() > 0:
            report.issues.append(
                ValidationIssue(
                    "WARNING",
                    "estimation",
                    "implausible_means",
                    f"{outliers.len()} groupes avec salaire moyen hors [{lo:,.0f}, {hi:,.0f}]",
                )
            )

    if "suppression_status" not in results.columns:
        report.issues.append(
            ValidationIssue(
                "ERROR",
                "estimation",
                "missing_disclosure_status",
                "Statut de secret statistique absent des resultats.",
            )
        )
    else:
        suppressed = results.filter(pl.col("suppression_status") != "publiee").height
        report.issues.append(
            ValidationIssue(
                "INFO",
                "estimation",
                "suppressed_cells",
                f"{suppressed} cellule(s) masquee(s) par secret primaire ou secondaire.",
            )
        )

    expected_statistics = [stat.name for stat in cfg.statistics]
    missing_statistics = sorted(set(expected_statistics) - set(results.columns))
    if missing_statistics:
        report.issues.append(
            ValidationIssue(
                "ERROR",
                "estimation",
                "missing_statistics",
                "Statistiques configurees absentes: " + ", ".join(missing_statistics),
            )
        )

    if "suppression_status" in results.columns and not missing_statistics:
        published = results.filter(pl.col("suppression_status") == "publiee")
        suppressed_rows = results.filter(pl.col("suppression_status") != "publiee")
        invalid_published = 0
        for column in expected_statistics:
            invalid_published += published.filter(
                pl.col(column).is_null()
                | ~pl.col(column).cast(pl.Float64, strict=False).is_finite().fill_null(False)
            ).height
        if invalid_published:
            report.issues.append(
                ValidationIssue(
                    "ERROR",
                    "estimation",
                    "nonfinite_published_statistics",
                    f"{invalid_published} valeur(s) publiee(s) sont nulles ou non finies.",
                )
            )

        leaked_suppressed = suppressed_rows.filter(
            pl.any_horizontal([pl.col(column).is_not_null() for column in expected_statistics])
        ).height
        if leaked_suppressed:
            report.issues.append(
                ValidationIssue(
                    "ERROR",
                    "estimation",
                    "unsafely_unmasked_statistics",
                    f"{leaked_suppressed} cellule(s) supprimee(s) contiennent encore "
                    "au moins une statistique.",
                )
            )

        if "gini" in published.columns:
            invalid_gini = published.filter((pl.col("gini") < 0) | (pl.col("gini") > 1)).height
            if invalid_gini:
                report.issues.append(
                    ValidationIssue(
                        "ERROR",
                        "estimation",
                        "invalid_gini",
                        f"{invalid_gini} coefficient(s) de Gini hors [0,1].",
                    )
                )
        if "variance" in published.columns:
            negative_variance = published.filter(pl.col("variance") < 0).height
            if negative_variance:
                report.issues.append(
                    ValidationIssue(
                        "ERROR",
                        "estimation",
                        "negative_variance",
                        f"{negative_variance} variance(s) negative(s).",
                    )
                )

        ordered = [
            name
            for name in ("min", "p10", "q1", "median", "q3", "p90", "max")
            if name in published.columns
        ]
        order_violations = 0
        for left, right in zip(ordered, ordered[1:]):
            order_violations += published.filter(pl.col(left) > pl.col(right)).height
        if order_violations:
            report.issues.append(
                ValidationIssue(
                    "ERROR",
                    "estimation",
                    "unordered_quantiles",
                    f"{order_violations} violation(s) de l'ordre min/quantiles/max.",
                )
            )

        if {"mean", "min", "max"} <= set(published.columns):
            invalid_mean = published.filter(
                (pl.col("mean") < pl.col("min")) | (pl.col("mean") > pl.col("max"))
            ).height
            if invalid_mean:
                report.issues.append(
                    ValidationIssue(
                        "ERROR",
                        "estimation",
                        "mean_outside_range",
                        f"{invalid_mean} moyenne(s) hors de la plage min-max.",
                    )
                )

    if "inference_status" not in results.columns:
        report.issues.append(
            ValidationIssue(
                "ERROR",
                "estimation",
                "missing_inference_status",
                "Statut d'inference absent des resultats.",
            )
        )
    elif results.filter(pl.col("inference_status") != "POINT_ONLY_F1_PENDING").height:
        report.issues.append(
            ValidationIssue(
                "ERROR",
                "estimation",
                "unexpected_inference_status",
                "Un statut d'inference non autorise est present.",
            )
        )

    interval_columns = [
        c for c in results.columns if c.endswith(("_ci_lower", "_ci_upper", "_std_error"))
    ]
    if interval_columns:
        report.issues.append(
            ValidationIssue(
                "ERROR",
                "estimation",
                "intervals_forbidden",
                "Des colonnes d'intervalle sont presentes alors que F.1 reste en attente: "
                + ", ".join(interval_columns),
            )
        )

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
        level="DEBUG",
        rotation="10 MB",
        retention="30 days",
        encoding="utf-8",
    )

    try:
        # results=None : validation donnees + modeles uniquement. Chainer
        # l'estimation de l'etape 10 ici defeaterait l'interet d'isoler
        # l'etape 11 (voir cas particulier etape 12 pour ce chainage).
        # valider_tout logue deja chaque issue en detail, rien a ajouter ici.
        report = valider_tout(cfg)
        if not report.is_valid:
            logger.error(
                "Validation echouee: le rapport contient {} erreur(s).",
                len(report.errors),
            )
            sys.exit(1)
        logger.info("Termine avec succes.")
    except Exception as exc:
        logger.exception("Echec de l'etape: {}", exc)
        sys.exit(1)
