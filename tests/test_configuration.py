"""Tests de la validation de configuration et des classes inclusives."""

from __future__ import annotations

import importlib
from pathlib import Path
from uuid import uuid4

import polars as pl
import pytest
import yaml

from cnps.config import _validate_estimation_method, load_config

ROOT = Path(__file__).resolve().parents[1]
SETTINGS = ROOT / "config" / "settings.yaml"
DIMENSIONS = ROOT / "config" / "dimensions.yaml"


def test_default_configuration_uses_only_implemented_method_and_logging() -> None:
    cfg = load_config(SETTINGS, DIMENSIONS)
    assert cfg.modeling.estimation_method == "ipw"
    assert cfg.logging.level == "INFO"
    assert cfg.logging.rotation == "10 MB"
    assert cfg.logging.retention == "30 days"
    assert not hasattr(cfg.ingestion, "file_pattern")
    assert not hasattr(cfg.ingestion, "encoding")


def test_unimplemented_estimation_method_is_rejected() -> None:
    with pytest.raises(ValueError, match="Seule la valeur 'ipw' est acceptee"):
        _validate_estimation_method("aipw")


def test_yaml_classes_are_normative_and_inclusive() -> None:
    cleaning = importlib.import_module("cnps.03_nettoyage_donnees")
    cfg = load_config(SETTINGS, DIMENSIONS)
    breaks = cleaning._dimension_breaks(cfg.dimensions, "age_employee")
    values = pl.DataFrame({"age": [24, 25, 34, 35, 49, 50]})
    classified = values.with_columns(cleaning._classify("age", breaks).alias("class"))

    assert classified["class"].to_list() == [
        "Moins de 25 ans",
        "25-34 ans",
        "25-34 ans",
        "35-49 ans",
        "35-49 ans",
        "50 ans et plus",
    ]


@pytest.mark.parametrize(
    ("section", "key", "value", "message"),
    [
        ("modeling", "ipw_trim_lower", 1.0, "quantiles de trimming"),
        ("modeling", "propensity_clip", 0.5, "propensity_clip"),
        ("modeling", "n_cv_splits", 1, "n_cv_splits"),
        ("estimation", "confidence_level", 1.0, "confidence_level"),
        ("estimation", "min_distinct_employers", 0, "min_distinct_employers"),
    ],
)
def test_invalid_numeric_configuration_is_rejected(
    section: str,
    key: str,
    value,
    message: str,
) -> None:
    settings = yaml.safe_load(SETTINGS.read_text(encoding="utf-8"))
    settings[section][key] = value
    generated_dir = ROOT / ".task_tmp"
    generated_dir.mkdir(exist_ok=True)
    settings_path = generated_dir / f"settings-{uuid4().hex}.yaml"
    try:
        settings_path.write_text(yaml.safe_dump(settings), encoding="utf-8")
        with pytest.raises(ValueError, match=message):
            load_config(settings_path, DIMENSIONS)
    finally:
        settings_path.unlink(missing_ok=True)
