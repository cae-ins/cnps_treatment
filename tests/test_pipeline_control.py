"""Tests du passage en memoire estimation -> validation -> export."""

from __future__ import annotations

import importlib
from types import SimpleNamespace

import polars as pl

pipeline = importlib.import_module("cnps.pipeline")
validation = importlib.import_module("cnps.11_validation_qualite")
storage = importlib.import_module("cnps.storage")


def _config() -> SimpleNamespace:
    return SimpleNamespace(minio=SimpleNamespace(output_prefix="", output_bucket="output"))


def test_estimation_is_computed_once_and_passed_to_validation_and_export(
    monkeypatch,
) -> None:
    estimated = pl.DataFrame({"dimension": ["National"], "mean": [100.0]})
    report = validation.ValidationReport()
    calls = {"estimate": 0, "validate": 0, "export": 0}

    def estimate(_cfg):
        calls["estimate"] += 1
        return estimated

    def validate(_cfg, results):
        calls["validate"] += 1
        assert results is estimated
        return report

    def export(_cfg, results, validated_report):
        calls["export"] += 1
        assert results is estimated
        assert validated_report is report
        return "indicateurs.xlsx"

    functions = {
        pipeline.Stage.ESTIMATION_INDICATEURS: estimate,
        pipeline.Stage.VALIDATION_QUALITE: validate,
        pipeline.Stage.EXPORT_EXCEL: export,
    }
    monkeypatch.setattr(pipeline, "_load_stage_function", functions.__getitem__)
    monkeypatch.setattr(storage, "write_json", lambda *_args, **_kwargs: None)

    result = pipeline.run_pipeline(
        _config(),
        pipeline.Stage.ESTIMATION_INDICATEURS,
        pipeline.Stage.EXPORT_EXCEL,
    )

    assert result.success
    assert calls == {"estimate": 1, "validate": 1, "export": 1}


def test_validation_error_blocks_export(monkeypatch) -> None:
    estimated = pl.DataFrame({"dimension": ["National"], "mean": [100.0]})
    report = validation.ValidationReport(
        issues=[
            validation.ValidationIssue(
                "ERROR",
                "estimation",
                "synthetic_error",
                "Erreur de validation synthetique",
            )
        ]
    )
    export_calls = 0

    def export(_cfg, _results, _report):
        nonlocal export_calls
        export_calls += 1
        return "indicateurs.xlsx"

    functions = {
        pipeline.Stage.ESTIMATION_INDICATEURS: lambda _cfg: estimated,
        pipeline.Stage.VALIDATION_QUALITE: lambda _cfg, _results: report,
        pipeline.Stage.EXPORT_EXCEL: export,
    }
    monkeypatch.setattr(pipeline, "_load_stage_function", functions.__getitem__)
    monkeypatch.setattr(storage, "write_json", lambda *_args, **_kwargs: None)

    result = pipeline.run_pipeline(
        _config(),
        pipeline.Stage.ESTIMATION_INDICATEURS,
        pipeline.Stage.EXPORT_EXCEL,
    )

    assert not result.success
    assert export_calls == 0
    assert [stage.status for stage in result.stages] == ["ok", "error"]
    assert "rapport de validation" in result.stages[-1].error
