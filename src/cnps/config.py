"""
Configuration loader for the CNPS Treatment Pipeline.

Loads YAML configuration files and resolves path variables.
Provides a typed, immutable configuration object used across all pipeline stages.
"""

from __future__ import annotations

import os
import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

import yaml
from dotenv import load_dotenv
from loguru import logger

# ---------------------------------------------------------------------------
# Dataclasses
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class PathsConfig:
    """Chemins locaux utilises par le pipeline.

    Toutes les donnees (brutes, intermediaires, modeles, exports) vivent
    sur MinIO (voir :class:`MinioConfig`) : le seul chemin local necessaire
    est celui des logs d'execution.
    """

    project_root: Path
    logs: Path


@dataclass(frozen=True)
class CleaningConfig:
    """Parameters governing the data-cleaning stage."""

    min_salary: float
    exclude_employee_types: list[str]
    winsor_lower: float
    winsor_upper: float
    remove_duplicates: bool
    max_duration: int
    numeric_parse_failure_threshold: float
    unknown_periodicity_assumption: str


@dataclass(frozen=True)
class ModelingConfig:
    """Parameters for the modeling and imputation stages."""

    n_imputations: int
    random_seed: int
    ipw_trim_lower: float
    ipw_trim_upper: float
    estimation_method: str  # "ipw" uniquement tant que les autres methodes sont differees
    min_auc: float
    calibration_slope_range: tuple[float, float]
    risk_window_months: int | None
    n_cv_splits: int
    propensity_clip: float
    max_clipped_share: float
    max_trimmed_share: float
    max_calibration_in_large: float
    max_abs_smd: float
    min_structural_stratum_size: int


@dataclass(frozen=True)
class EstimationConfig:
    """Parameters for the weighted-estimation stage."""

    min_cell_size: int
    confidence_level: float
    salary_plausible_range: tuple[float, float]
    min_distinct_individuals: int
    min_distinct_employers: int
    max_employer_wage_share: float
    inference_method: str


@dataclass(frozen=True)
class MinioConfig:
    """Parametres de connexion au serveur de stockage objet MinIO.

    Organisation en couches (medaillon) : chaque etape du pipeline lit et
    ecrit dans un bucket different, chacun avec son propre prefixe interne.
    Un couple (bucket, prefixe) localise donc chaque famille de fichiers,
    par exemple ``raw_bucket``/``raw_prefix`` pour les Excel bruts.

    Les identifiants ne sont jamais lus depuis le YAML : ils viennent des
    variables d'environnement ``MINIO_ACCESS_KEY`` / ``MINIO_SECRET_KEY``
    afin de pouvoir differer selon la machine et de rester hors du
    controle de version.
    """

    endpoint: str
    raw_bucket: str
    raw_prefix: str
    processed_bucket: str
    processed_prefix: str
    cleaned_bucket: str
    cleaned_prefix: str
    models_bucket: str
    models_prefix: str
    output_bucket: str
    output_prefix: str
    secure: bool
    access_key: str
    secret_key: str
    environment: str
    allow_insecure_minio: bool


@dataclass(frozen=True)
class ParallelConfig:
    """Parallelisation settings."""

    n_jobs: int
    backend: str


@dataclass(frozen=True)
class IngestionConfig:
    """Parameters for the data-ingestion stage."""

    filename_regex: str
    skip_sheets: list[str]


@dataclass(frozen=True)
class LoggingConfig:
    """Parametres des sorties de journalisation du pipeline."""

    level: str
    rotation: str
    retention: str


@dataclass(frozen=True)
class DimensionDef:
    """A single analytical dimension (axis of analysis)."""

    name: str
    label: str
    group_by: list[str]
    enabled: bool
    classes: list[dict[str, Any]] = field(default_factory=list)


@dataclass(frozen=True)
class StatDef:
    """A single statistical indicator to compute."""

    name: str
    label: str
    function: str
    variable: str
    params: dict[str, Any] = field(default_factory=dict)


@dataclass(frozen=True)
class PipelineConfig:
    """Top-level, immutable configuration object for the entire pipeline."""

    paths: PathsConfig
    ingestion: IngestionConfig
    cleaning: CleaningConfig
    modeling: ModelingConfig
    estimation: EstimationConfig
    minio: MinioConfig
    parallel: ParallelConfig
    logging: LoggingConfig
    dimensions: list[DimensionDef]
    statistics: list[StatDef]


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

_VAR_RE = re.compile(r"\$\{(\w+)\}")


def _resolve_paths(raw: dict[str, str]) -> dict[str, Path]:
    """Resolve ``${variable}`` references inside the *paths* section."""
    resolved: dict[str, str] = {}
    for key, value in raw.items():

        def _replacer(m: re.Match) -> str:
            ref = m.group(1)
            if ref in resolved:
                return resolved[ref]
            if ref in raw:
                return raw[ref]
            raise ValueError(f"Unresolved path variable: ${{{ref}}}")

        resolved[key] = _VAR_RE.sub(_replacer, value)
    return {k: Path(v) for k, v in resolved.items()}


def _validate_dimension_classes(name: str, classes: list[dict[str, Any]]) -> None:
    """Valide des classes dont les bornes inferieure et superieure sont inclusives."""
    previous_max: float | None = None
    for index, class_def in enumerate(classes, start=1):
        missing = {"label", "min", "max"} - class_def.keys()
        if missing:
            raise ValueError(
                f"Dimension '{name}', classe {index}: champs manquants "
                f"{sorted(missing)}. Chaque classe exige label, min et max."
            )

        lower = class_def["min"]
        upper = class_def["max"]
        if not isinstance(lower, (int, float)) or not isinstance(upper, (int, float)):
            raise ValueError(
                f"Dimension '{name}', classe {index}: min et max doivent etre numeriques."
            )
        if lower > upper:
            raise ValueError(
                f"Dimension '{name}', classe {index}: la borne min {lower} depasse "
                f"la borne max inclusive {upper}."
            )
        if previous_max is not None and lower <= previous_max:
            raise ValueError(
                f"Dimension '{name}', classe {index}: la borne min {lower} chevauche "
                f"la borne max inclusive precedente {previous_max}."
            )
        previous_max = upper


def _validate_estimation_method(value: str) -> str:
    """Refuse les methodes annoncees mais non encore implementees."""
    if value != "ipw":
        raise ValueError(
            "Methode d'estimation non implementee: "
            f"'{value}'. Seule la valeur 'ipw' est acceptee; "
            "'aipw' et 'tmle' restent differees."
        )
    return value


# ---------------------------------------------------------------------------
# Public API


def _parse_risk_window(value: Any) -> int | None:
    """Parse K; None represente une fenetre sans limite."""
    if value is None or str(value).strip().lower() in {"inf", "infinity", "none"}:
        return None
    if isinstance(value, bool):
        raise ValueError("modeling.risk_window_months doit etre un entier positif ou 'inf'.")
    try:
        parsed = int(value)
    except (TypeError, ValueError) as exc:
        raise ValueError(
            "modeling.risk_window_months doit etre un entier positif ou 'inf'."
        ) from exc
    if parsed <= 0:
        raise ValueError("modeling.risk_window_months doit etre strictement positif.")
    return parsed


# ---------------------------------------------------------------------------


def load_config(
    settings_path: str | Path | None = None,
    dimensions_path: str | Path | None = None,
) -> PipelineConfig:
    """
    Load and validate pipeline configuration from YAML files.

    Parameters
    ----------
    settings_path : path-like, optional
        Path to ``settings.yaml``.  Defaults to ``config/settings.yaml``
        relative to the package root.
    dimensions_path : path-like, optional
        Path to ``dimensions.yaml``.  Defaults to ``config/dimensions.yaml``.

    Returns
    -------
    PipelineConfig
        Fully resolved, immutable configuration.
    """
    repo_root = Path(__file__).resolve().parents[2]
    config_dir = repo_root / "config"
    settings_path = Path(settings_path) if settings_path else config_dir / "settings.yaml"
    dimensions_path = Path(dimensions_path) if dimensions_path else config_dir / "dimensions.yaml"

    load_dotenv(repo_root / ".env")

    with open(settings_path, encoding="utf-8") as fh:
        settings: dict = yaml.safe_load(fh)
    with open(dimensions_path, encoding="utf-8") as fh:
        dims_raw: dict = yaml.safe_load(fh)

    # --- Paths ---
    if not settings["paths"].get("project_root"):
        settings["paths"]["project_root"] = str(repo_root)
    resolved = _resolve_paths(settings["paths"])
    paths = PathsConfig(**resolved)

    paths.logs.mkdir(parents=True, exist_ok=True)

    # --- Sections ---
    ingestion = IngestionConfig(**settings["ingestion"])
    cleaning = CleaningConfig(**settings["cleaning"])
    if not 0 <= cleaning.numeric_parse_failure_threshold <= 1:
        raise ValueError("cleaning.numeric_parse_failure_threshold doit appartenir a [0, 1].")
    if cleaning.unknown_periodicity_assumption not in {"monthly", "daily"}:
        raise ValueError(
            "cleaning.unknown_periodicity_assumption doit valoir 'monthly' ou 'daily'."
        )

    m = settings["modeling"]
    estimation_method = _validate_estimation_method(m["estimation_method"])
    risk_window_months = _parse_risk_window(m["risk_window_months"])
    modeling = ModelingConfig(
        n_imputations=int(m["n_imputations"]),
        random_seed=int(m["random_seed"]),
        ipw_trim_lower=float(m["ipw_trim_lower"]),
        ipw_trim_upper=float(m["ipw_trim_upper"]),
        estimation_method=estimation_method,
        min_auc=float(m["min_auc"]),
        calibration_slope_range=tuple(m["calibration_slope_range"]),
        risk_window_months=risk_window_months,
        n_cv_splits=int(m["n_cv_splits"]),
        propensity_clip=float(m["propensity_clip"]),
        max_clipped_share=float(m["max_clipped_share"]),
        max_trimmed_share=float(m["max_trimmed_share"]),
        max_calibration_in_large=float(m["max_calibration_in_large"]),
        max_abs_smd=float(m["max_abs_smd"]),
        min_structural_stratum_size=int(m["min_structural_stratum_size"]),
    )
    if modeling.n_imputations < 1:
        raise ValueError("modeling.n_imputations doit etre positif.")
    if not 0 <= modeling.ipw_trim_lower < modeling.ipw_trim_upper <= 1:
        raise ValueError(
            "Les quantiles de trimming doivent verifier 0 <= ipw_trim_lower < ipw_trim_upper <= 1."
        )
    if not 0 <= modeling.min_auc <= 1:
        raise ValueError("modeling.min_auc doit appartenir a [0, 1].")
    slope_min, slope_max = modeling.calibration_slope_range
    if slope_min <= 0 or slope_min > slope_max:
        raise ValueError("calibration_slope_range doit etre positive et ordonnee.")
    if modeling.n_cv_splits < 2:
        raise ValueError("modeling.n_cv_splits doit etre au moins egal a 2.")
    if not 0 < modeling.propensity_clip < 0.5:
        raise ValueError("modeling.propensity_clip doit appartenir a ]0, 0.5[.")
    for name, value in (
        ("max_clipped_share", modeling.max_clipped_share),
        ("max_trimmed_share", modeling.max_trimmed_share),
        ("max_abs_smd", modeling.max_abs_smd),
    ):
        if not 0 <= value <= 1:
            raise ValueError(f"modeling.{name} doit appartenir a [0, 1].")
    if modeling.max_calibration_in_large < 0:
        raise ValueError("max_calibration_in_large doit etre positif ou nul.")
    if modeling.min_structural_stratum_size < 1:
        raise ValueError("min_structural_stratum_size doit etre positif.")

    e = settings["estimation"]
    estimation = EstimationConfig(
        min_cell_size=e["min_cell_size"],
        confidence_level=e["confidence_level"],
        salary_plausible_range=tuple(e["salary_plausible_range"]),
        min_distinct_individuals=int(e["min_distinct_individuals"]),
        min_distinct_employers=int(e["min_distinct_employers"]),
        max_employer_wage_share=float(e["max_employer_wage_share"]),
        inference_method=str(e["inference_method"]),
    )
    if estimation.inference_method != "point_only":
        raise ValueError(
            "Seule inference_method='point_only' est autorisee tant que F.1 "
            "n'est pas methodologiquement valide."
        )
    if not 0 < estimation.max_employer_wage_share <= 1:
        raise ValueError("max_employer_wage_share doit appartenir a ]0, 1].")
    if estimation.min_cell_size < 1:
        raise ValueError("estimation.min_cell_size doit etre positif.")
    if not 0 < estimation.confidence_level < 1:
        raise ValueError("estimation.confidence_level doit appartenir a ]0, 1[.")
    salary_lo, salary_hi = estimation.salary_plausible_range
    if salary_lo < 0 or salary_lo >= salary_hi:
        raise ValueError("salary_plausible_range doit etre positive et ordonnee.")
    if estimation.min_distinct_individuals < 1:
        raise ValueError("min_distinct_individuals doit etre positif.")
    if estimation.min_distinct_employers < 1:
        raise ValueError("min_distinct_employers doit etre positif.")

    mi = settings["minio"]
    environment = str(mi["environment"]).lower()
    allow_insecure = bool(mi["allow_insecure_minio"])
    access_key = os.environ.get("MINIO_ACCESS_KEY", "")
    secret_key = os.environ.get("MINIO_SECRET_KEY", "")
    if environment not in {"development", "production"}:
        raise ValueError("minio.environment doit valoir development ou production.")
    if environment == "production":
        if not access_key or not secret_key:
            raise ValueError("Secrets MinIO obligatoires en production.")
        if not mi["secure"]:
            raise ValueError("minio.secure doit valoir true en production.")
    elif not mi["secure"] and not allow_insecure:
        raise ValueError("HTTP MinIO en development exige allow_insecure_minio=true.")

    minio = MinioConfig(
        endpoint=mi["endpoint"],
        raw_bucket=mi["raw_bucket"],
        raw_prefix=mi["raw_prefix"],
        processed_bucket=mi["processed_bucket"],
        processed_prefix=mi["processed_prefix"],
        cleaned_bucket=mi["cleaned_bucket"],
        cleaned_prefix=mi["cleaned_prefix"],
        models_bucket=mi["models_bucket"],
        models_prefix=mi["models_prefix"],
        output_bucket=mi["output_bucket"],
        output_prefix=mi["output_prefix"],
        secure=mi["secure"],
        access_key=access_key,
        secret_key=secret_key,
        environment=environment,
        allow_insecure_minio=allow_insecure,
    )

    parallel = ParallelConfig(**settings["parallel"])

    logging_raw = settings["logging"]
    logging_level = str(logging_raw["level"]).upper()
    supported_logging_levels = {"DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"}
    if logging_level not in supported_logging_levels:
        raise ValueError(
            f"Niveau de logging non implemente: '{logging_raw['level']}'. "
            f"Valeurs acceptees: {', '.join(sorted(supported_logging_levels))}."
        )
    logging = LoggingConfig(
        level=logging_level,
        rotation=str(logging_raw["rotation"]),
        retention=str(logging_raw["retention"]),
    )

    # --- Dimensions ---
    dimensions = []
    for name, d in dims_raw["dimensions"].items():
        classes = d.get("classes", [])
        _validate_dimension_classes(name, classes)
        dimensions.append(
            DimensionDef(
                name=name,
                label=d["label"],
                group_by=d.get("group_by", []),
                enabled=d.get("enabled", True),
                classes=classes,
            )
        )

    # --- Statistics ---
    statistics = []
    for s in dims_raw["statistics"]:
        statistics.append(
            StatDef(
                name=s["name"],
                label=s["label"],
                function=s["function"],
                variable=s["variable"],
                params=s.get("params", {}),
            )
        )

    cfg = PipelineConfig(
        paths=paths,
        ingestion=ingestion,
        cleaning=cleaning,
        modeling=modeling,
        estimation=estimation,
        minio=minio,
        parallel=parallel,
        logging=logging,
        dimensions=dimensions,
        statistics=statistics,
    )

    logger.info("Configuration loaded from {}", settings_path)
    return cfg
