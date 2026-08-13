"""Publication Git opt-in des artefacts de recette d'une session."""

from __future__ import annotations

import json
import os
import subprocess
from pathlib import Path

from loguru import logger

from cnps.config import PipelineConfig
from cnps.storage import read_json


def _git(root: Path, *args: str) -> str:
    result = subprocess.run(
        ["git", *args],
        cwd=root,
        check=True,
        capture_output=True,
        text=True,
        timeout=120,
    )
    return result.stdout.strip()


def auto_publish_run_report(cfg: PipelineConfig, session_id: str) -> bool:
    """Commit et pousse les rapports agrégés si le double opt-in est actif."""
    enabled = os.environ.get("CNPS_AUTO_GIT_PUSH", "").strip().lower() in {
        "1",
        "true",
        "yes",
    }
    if not enabled or not cfg.git_publication.allowed:
        logger.info("Auto-publication Git inactive (double opt-in non active).")
        return False

    root = cfg.paths.project_root.resolve()
    current_branch = _git(root, "branch", "--show-current")
    expected_branch = cfg.git_publication.branch
    if not expected_branch or current_branch != expected_branch:
        raise RuntimeError(
            f"Auto-publication refusee: branche courante={current_branch!r}, "
            f"branche autorisee={expected_branch!r}."
        )
    if _git(root, "status", "--porcelain"):
        raise RuntimeError("Auto-publication refusee: le depot contient deja des modifications.")

    prefix = f"{cfg.minio.output_prefix}sessions/{session_id}"
    metadata_path = f"{prefix}/metadata.json"
    metadata = read_json(cfg.minio, cfg.minio.output_bucket, metadata_path)
    run_report_path = metadata.get("run_report_path")
    if not run_report_path:
        raise RuntimeError("Rapport d'execution absent du manifeste de session.")
    artifacts = {"metadata.json": metadata_path, "run_report.json": run_report_path}
    validation_path = metadata.get("validation_report_path")
    if validation_path:
        artifacts["validation_report.json"] = validation_path
    estimation_path = metadata.get("estimation_results_path")
    if cfg.git_publication.include_estimates:
        if not estimation_path:
            raise RuntimeError("Estimations sessionnees absentes du manifeste.")
        artifacts["estimation_results.json"] = estimation_path

    allowed_prefix = f"{cfg.minio.output_prefix}sessions/{session_id}/"
    if any(not str(path).startswith(allowed_prefix) for path in artifacts.values()):
        raise RuntimeError("Le manifeste reference un artefact hors de la session autorisee.")

    target = (root / cfg.git_publication.reports_directory / session_id).resolve()
    try:
        target.relative_to(root)
    except ValueError as exc:
        raise RuntimeError("Le dossier de rapports sort du depot Git.") from exc
    target.mkdir(parents=True, exist_ok=False)

    for filename, object_path in artifacts.items():
        payload = (
            metadata
            if filename == "metadata.json"
            else read_json(cfg.minio, cfg.minio.output_bucket, object_path)
        )
        (target / filename).write_text(
            json.dumps(payload, indent=2, ensure_ascii=False, allow_nan=False) + "\n",
            encoding="utf-8",
        )

    relative_target = target.relative_to(root).as_posix()
    _git(root, "add", "--", relative_target)
    _git(root, "commit", "-m", f"chore: ajoute la recette CNPS {session_id}")
    _git(root, "push", cfg.git_publication.remote, current_branch)
    logger.info(
        "Rapports d'execution commités et poussés sur {}/{} : {}",
        cfg.git_publication.remote,
        current_branch,
        relative_target,
    )
    return True
