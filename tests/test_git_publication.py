"""Gardes de l'auto-publication Git des rapports de recette."""

from __future__ import annotations

from pathlib import Path
from types import SimpleNamespace

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


def test_partial_run_without_validation_is_not_published(monkeypatch, tmp_path: Path) -> None:
    monkeypatch.setenv("CNPS_AUTO_GIT_PUSH", "true")
    monkeypatch.setattr(
        "cnps.git_publication._git",
        lambda _root, *args: "fix/audit-phase-b" if args == ("branch", "--show-current") else "",
    )
    monkeypatch.setattr(
        "cnps.git_publication.read_json",
        lambda *_args: {
            "validation_report_path": None,
            "estimation_results_path": None,
        },
    )
    assert auto_publish_run_report(_cfg(tmp_path), "abc") is False
    assert not (tmp_path / "run_reports").exists()
