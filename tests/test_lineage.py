"""Tests du manifeste de session, sans ecriture MinIO."""

from __future__ import annotations

from pathlib import Path
from types import SimpleNamespace

from cnps.lineage import build_session_manifest, config_sha256, new_session_id
from cnps.pipeline import StageResult


def test_session_ids_are_unique_uuid_hex() -> None:
    first = new_session_id()
    second = new_session_id()
    assert first != second
    assert len(first) == 32
    int(first, 16)


def test_config_fingerprint_redacts_credentials_and_is_stable() -> None:
    cfg = SimpleNamespace(
        paths=SimpleNamespace(project_root=Path.cwd()),
        minio=SimpleNamespace(access_key="public", secret_key="super-secret"),
        value=1,
    )
    assert config_sha256(cfg) == config_sha256(cfg)
    manifest = build_session_manifest(
        cfg,
        session_id="abc",
        start_time="2026-08-01T00:00:00+02:00",
        end_time="2026-08-01T00:00:01+02:00",
        total_duration_seconds=1.0,
        success=True,
        stages=[StageResult("test", "ok", 1.0, output_path="x.parquet")],
    )
    serialized = str(manifest)
    assert "super-secret" not in serialized
    assert "public" not in serialized
    assert manifest["stages"][0]["output_path"] == "x.parquet"
    assert len(manifest["config_sha256"]) == 64


def test_config_fingerprint_is_independent_of_checkout_location() -> None:
    first_root = Path("C:/work/machine-a/repo")
    second_root = Path("D:/work/machine-b/repo")
    cfg_a = SimpleNamespace(
        paths=SimpleNamespace(
            project_root=first_root,
            logs=first_root / "logs",
        ),
        minio=SimpleNamespace(access_key="", secret_key=""),
        value=1,
    )
    cfg_b = SimpleNamespace(
        paths=SimpleNamespace(
            project_root=second_root,
            logs=second_root / "logs",
        ),
        minio=SimpleNamespace(access_key="", secret_key=""),
        value=1,
    )
    assert config_sha256(cfg_a) == config_sha256(cfg_b)
