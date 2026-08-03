"""Tests de propagation des echecs par les commandes Typer."""

from __future__ import annotations

import importlib
from types import SimpleNamespace

import pytest
import typer

cli = importlib.import_module("cnps.cli")
validation = importlib.import_module("cnps.11_validation_qualite")


def _failed_pipeline_result() -> SimpleNamespace:
    return SimpleNamespace(
        success=False,
        stages=[],
        total_duration_seconds=0.0,
    )


@pytest.mark.parametrize("command_name", ["ingest", "clean", "model", "estimate"])
def test_pipeline_shortcuts_exit_one_on_failure(monkeypatch, command_name: str) -> None:
    monkeypatch.setattr(cli, "load_config", lambda *_args, **_kwargs: object())
    monkeypatch.setattr(cli, "_setup_logging", lambda *_args, **_kwargs: None)
    monkeypatch.setattr(
        cli,
        "run_pipeline",
        lambda *_args, **_kwargs: _failed_pipeline_result(),
    )

    with pytest.raises(typer.Exit) as exc_info:
        getattr(cli, command_name)(settings=None, verbose=False)

    assert exc_info.value.exit_code == 1


def test_run_command_exits_one_on_failure(monkeypatch) -> None:
    monkeypatch.setattr(cli, "load_config", lambda *_args, **_kwargs: object())
    monkeypatch.setattr(cli, "_setup_logging", lambda *_args, **_kwargs: None)
    monkeypatch.setattr(
        cli,
        "run_pipeline",
        lambda *_args, **_kwargs: _failed_pipeline_result(),
    )

    with pytest.raises(typer.Exit) as exc_info:
        cli.run(
            settings=None,
            dimensions=None,
            from_stage="LECTURE_FICHIERS",
            to_stage="EXPORT_EXCEL",
            verbose=False,
        )

    assert exc_info.value.exit_code == 1


def test_validate_command_exits_one_when_report_has_error(monkeypatch) -> None:
    report = validation.ValidationReport(
        issues=[
            validation.ValidationIssue(
                "ERROR",
                "data",
                "synthetic_error",
                "Erreur synthetique",
            )
        ]
    )
    validation_module = SimpleNamespace(valider_tout=lambda _cfg: report)
    monkeypatch.setattr(cli, "load_config", lambda *_args, **_kwargs: object())
    monkeypatch.setattr(cli, "_setup_logging", lambda *_args, **_kwargs: None)
    monkeypatch.setattr(
        cli.importlib,
        "import_module",
        lambda _name: validation_module,
    )

    with pytest.raises(typer.Exit) as exc_info:
        cli.validate(settings=None, verbose=False)

    assert exc_info.value.exit_code == 1


def test_enrich_anstat_import_target_exists() -> None:
    module = importlib.import_module("cnps.jointure_anstat")
    assert callable(module.enrichir_avec_anstat)
