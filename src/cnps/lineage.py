"""Manifeste reproductible d'une session du pipeline, sans acces reseau."""

from __future__ import annotations

import hashlib
import json
import subprocess
from dataclasses import asdict, is_dataclass
from enum import Enum
from importlib.metadata import PackageNotFoundError, version
from pathlib import Path
from types import SimpleNamespace
from typing import Any
from uuid import uuid4

_REDACTED_KEYS = {"access_key", "secret_key", "password", "token"}
_PACKAGES = ("cnps-treatment", "numpy", "pandas", "polars", "scikit-learn", "scipy")


def new_session_id() -> str:
    """Retourne un identifiant non collisionnant, independant de l'horloge."""
    return uuid4().hex


def _jsonable(value: Any) -> Any:
    if is_dataclass(value):
        return _jsonable(asdict(value))
    if isinstance(value, SimpleNamespace):
        return _jsonable(vars(value))
    if isinstance(value, dict):
        return {
            str(key): ("<redacted>" if str(key).lower() in _REDACTED_KEYS else _jsonable(item))
            for key, item in value.items()
        }
    if isinstance(value, (list, tuple, set)):
        return [_jsonable(item) for item in value]
    if isinstance(value, Path):
        return str(value.resolve())
    if isinstance(value, Enum):
        return value.name
    if value is None or isinstance(value, (str, int, float, bool)):
        return value
    if hasattr(value, "__dict__"):
        return _jsonable(vars(value))
    return repr(value)


def config_sha256(cfg: Any) -> str:
    """Empreinte SHA-256 canonique, secrets et racine locale neutralises."""
    normalized = _jsonable(cfg)
    if isinstance(normalized, dict):
        paths = normalized.get("paths")
        if isinstance(paths, dict) and paths.get("project_root"):
            root = Path(paths["project_root"])
            for key, raw_path in list(paths.items()):
                if key == "project_root":
                    paths[key] = "${project_root}"
                    continue
                try:
                    relative = Path(raw_path).relative_to(root).as_posix()
                except (TypeError, ValueError):
                    continue
                paths[key] = f"${{project_root}}/{relative}"
    canonical = json.dumps(
        normalized,
        ensure_ascii=False,
        sort_keys=True,
        separators=(",", ":"),
    ).encode("utf-8")
    return hashlib.sha256(canonical).hexdigest()


def git_state(project_root: Path) -> dict[str, Any]:
    """Capture le commit et l'etat sale sans modifier le depot."""
    try:
        commit = subprocess.run(
            ["git", "rev-parse", "HEAD"],
            cwd=project_root,
            check=True,
            capture_output=True,
            text=True,
            timeout=5,
        ).stdout.strip()
        status = subprocess.run(
            ["git", "status", "--porcelain"],
            cwd=project_root,
            check=True,
            capture_output=True,
            text=True,
            timeout=5,
        ).stdout
        return {"commit": commit, "dirty": bool(status.strip())}
    except (OSError, subprocess.SubprocessError):
        return {"commit": None, "dirty": None}


def dependency_versions() -> dict[str, str | None]:
    """Versions minimales utiles a la reproduction du calcul."""
    found: dict[str, str | None] = {}
    for package in _PACKAGES:
        try:
            found[package] = version(package)
        except PackageNotFoundError:
            found[package] = None
    return found


def build_session_manifest(
    cfg: Any,
    *,
    session_id: str,
    start_time: str,
    end_time: str,
    total_duration_seconds: float,
    success: bool,
    stages: list[Any],
    validation_report_path: str | None = None,
    estimation_results_path: str | None = None,
    run_report_path: str | None = None,
) -> dict[str, Any]:
    """Construit le manifeste de session et la chaine de sorties declarees."""
    project_root = Path(getattr(getattr(cfg, "paths", None), "project_root", Path.cwd())).resolve()
    previous_output: str | None = None
    stage_rows = []
    for stage in stages:
        output_path = getattr(stage, "output_path", "") or None
        stage_rows.append(
            {
                "stage": stage.stage,
                "status": stage.status,
                "duration_seconds": stage.duration_seconds,
                "input_from_previous_stage": previous_output,
                "output_path": output_path,
                "error": stage.error or None,
            }
        )
        if stage.status == "ok" and output_path:
            previous_output = output_path

    return {
        "schema_version": 1,
        "session_id": session_id,
        "start_time": start_time,
        "end_time": end_time,
        "total_duration_seconds": total_duration_seconds,
        "success": success,
        "config_sha256": config_sha256(cfg),
        "git": git_state(project_root),
        "dependencies": dependency_versions(),
        "stages": stage_rows,
        "validation_report_path": validation_report_path,
        "estimation_results_path": estimation_results_path,
        "run_report_path": run_report_path,
        "artifact_contract": {
            "manifest_namespace_is_session_scoped": True,
            "canonical_stage_outputs_are_mutable": True,
        },
    }
