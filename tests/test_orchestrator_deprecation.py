"""Tests de compatibilite de l'orchestrateur deprecie."""

from __future__ import annotations

import importlib
import sys
from pathlib import Path
from types import SimpleNamespace

import pytest


def _import_orchestrator_with_warning():
    sys.modules.pop("cnps.orchestrator", None)
    with pytest.warns(DeprecationWarning, match="deprecie"):
        return importlib.import_module("cnps.orchestrator")


def test_orchestrator_warns_when_module_starts() -> None:
    _import_orchestrator_with_warning()


def test_validation_failure_stops_before_export(monkeypatch) -> None:
    orchestrator = _import_orchestrator_with_warning()
    calls: list[Path] = []
    stages = [
        ("11", Path("11_validation_qualite.py")),
        ("12", Path("12_export_excel.py")),
    ]

    monkeypatch.setattr(orchestrator, "discover_stages", lambda: stages)

    def run(cmd):
        calls.append(Path(cmd[1]))
        return SimpleNamespace(returncode=1)

    monkeypatch.setattr(orchestrator.subprocess, "run", run)

    results = orchestrator.run_orchestrated("11", "12")

    assert [path.name for path in calls] == ["11_validation_qualite.py"]
    assert [result["status"] for result in results] == ["error"]
