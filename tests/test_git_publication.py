"""Gardes de l'auto-publication Git des rapports de recette."""

from __future__ import annotations

from pathlib import Path
from types import SimpleNamespace

import pytest

from cnps.git_publication import auto_publish_run_report


def _cfg(tmp_path: Path):
    return SimpleNamespace(
        paths=SimpleNamespace(project_root=tmp_path),
        minio=SimpleNamespace(output_prefix="exports/", output_bucket="output"),
        git_publication=SimpleNamespace(
            allowed=True,
            remote="origin",
            branch="fix/audit-phase-b",
            reports_directory=Path("run_reports"),
            include_estimates=False,
        ),
    )


def test_auto_publish_requires_environment_opt_in(monkeypatch, tmp_path: Path) -> None:
    monkeypatch.delenv("CNPS_AUTO_GIT_PUSH", raising=False)
    assert auto_publish_run_report(_cfg(tmp_path), "abc") is False


def test_failed_run_without_validation_is_published(monkeypatch, tmp_path: Path) -> None:
    monkeypatch.setenv("CNPS_AUTO_GIT_PUSH", "true")
    git_calls: list[tuple[str, ...]] = []

    def fake_git(_root, *args):
        git_calls.append(args)
        return "fix/audit-phase-b" if args == ("branch", "--show-current") else ""

    monkeypatch.setattr(
        "cnps.git_publication._git",
        fake_git,
    )
    metadata = {
        "run_report_path": "exports/sessions/abc/run_report.json",
        "validation_report_path": None,
        "estimation_results_path": None,
    }
    run_report = {"session_id": "abc", "run_status": "FAILURE", "error": "synthetic"}

    def fake_read_json(_cfg, _bucket, object_name):
        return metadata if object_name.endswith("metadata.json") else run_report

    monkeypatch.setattr("cnps.git_publication.read_json", fake_read_json)
    assert auto_publish_run_report(_cfg(tmp_path), "abc") is True
    report_dir = tmp_path / "run_reports" / "abc"
    assert (report_dir / "metadata.json").exists()
    assert (report_dir / "run_report.json").exists()
    assert not (report_dir / "validation_report.json").exists()
    assert any(args[:2] == ("commit", "-m") for args in git_calls)


def test_manifest_without_run_report_is_rejected(monkeypatch, tmp_path: Path) -> None:
    monkeypatch.setenv("CNPS_AUTO_GIT_PUSH", "true")
    monkeypatch.setattr(
        "cnps.git_publication._git",
        lambda _root, *args: "fix/audit-phase-b" if args == ("branch", "--show-current") else "",
    )
    monkeypatch.setattr(
        "cnps.git_publication.read_json",
        lambda *_args: {
            "run_report_path": None,
            "validation_report_path": None,
            "estimation_results_path": None,
        },
    )
    with pytest.raises(RuntimeError, match="Rapport d'execution absent"):
        auto_publish_run_report(_cfg(tmp_path), "abc")
