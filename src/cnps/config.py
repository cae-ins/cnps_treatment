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


@dataclass(frozen=True)
class ModelingConfig:
    """Parameters for the modeling and imputation stages."""
    n_imputations: int
    random_seed: int
    ipw_trim_lower: float
    ipw_trim_upper: float
    estimation_method: str          # "ipw" | "aipw" | "tmle"
    min_auc: float
    calibration_slope_range: tuple[float, float]


@dataclass(frozen=True)
class EstimationConfig:
    """Parameters for the weighted-estimation stage."""
    min_cell_size: int
    confidence_level: float
    salary_plausible_range: tuple[float, float]


@dataclass(frozen=True)
class MinioConfig:
    """Parametres de connexion au serveur de stockage objet MinIO.

    Les identifiants ne sont jamais lus depuis le YAML : ils viennent des
    variables d'environnement ``MINIO_ACCESS_KEY`` / ``MINIO_SECRET_KEY``
    afin de pouvoir differer selon la machine et de rester hors du
    controle de version.
    """
    endpoint: str
    bucket: str
    raw_prefix: str
    processed_prefix: str
    cleaned_prefix: str
    models_prefix: str
    output_prefix: str
    secure: bool
    access_key: str
    secret_key: str


@dataclass(frozen=True)
class ParallelConfig:
    """Parallelisation settings."""
    n_jobs: int
    backend: str


@dataclass(frozen=True)
class IngestionConfig:
    """Parameters for the data-ingestion stage."""
    file_pattern: str
    filename_regex: str
    skip_sheets: list[str]
    encoding: str


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


# ---------------------------------------------------------------------------
# Public API
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

    m = settings["modeling"]
    modeling = ModelingConfig(
        n_imputations=m["n_imputations"],
        random_seed=m["random_seed"],
        ipw_trim_lower=m["ipw_trim_lower"],
        ipw_trim_upper=m["ipw_trim_upper"],
        estimation_method=m["estimation_method"],
        min_auc=m["min_auc"],
        calibration_slope_range=tuple(m["calibration_slope_range"]),
    )

    e = settings["estimation"]
    estimation = EstimationConfig(
        min_cell_size=e["min_cell_size"],
        confidence_level=e["confidence_level"],
        salary_plausible_range=tuple(e["salary_plausible_range"]),
    )

    mi = settings["minio"]
    minio = MinioConfig(
        endpoint=mi["endpoint"],
        bucket=mi["bucket"],
        raw_prefix=mi["raw_prefix"],
        processed_prefix=mi["processed_prefix"],
        cleaned_prefix=mi["cleaned_prefix"],
        models_prefix=mi["models_prefix"],
        output_prefix=mi["output_prefix"],
        secure=mi["secure"],
        access_key=os.environ.get("MINIO_ACCESS_KEY", "minioadmin"),
        secret_key=os.environ.get("MINIO_SECRET_KEY", "minioadmin"),
    )

    parallel = ParallelConfig(**settings["parallel"])

    # --- Dimensions ---
    dimensions = []
    for name, d in dims_raw["dimensions"].items():
        dimensions.append(DimensionDef(
            name=name,
            label=d["label"],
            group_by=d.get("group_by", []),
            enabled=d.get("enabled", True),
            classes=d.get("classes", []),
        ))

    # --- Statistics ---
    statistics = []
    for s in dims_raw["statistics"]:
        statistics.append(StatDef(
            name=s["name"],
            label=s["label"],
            function=s["function"],
            params=s.get("params", {}),
        ))

    cfg = PipelineConfig(
        paths=paths,
        ingestion=ingestion,
        cleaning=cleaning,
        modeling=modeling,
        estimation=estimation,
        minio=minio,
        parallel=parallel,
        dimensions=dimensions,
        statistics=statistics,
    )

    logger.info("Configuration loaded from {}", settings_path)
    return cfg
