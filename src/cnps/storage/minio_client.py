"""
MinIO object storage client for the CNPS Treatment Pipeline.

Handles synchronising raw Excel declarations from the MinIO bucket down to
the local ``data/raw`` folder before ingestion, and uploading the cleaned
Parquet output back up after the cleaning stage.

Credentials are read from ``MINIO_ACCESS_KEY`` / ``MINIO_SECRET_KEY``
environment variables (see :class:`cnps.config.MinioConfig`); connection
parameters (endpoint, bucket, prefixes) come from ``settings.yaml``.
"""

from __future__ import annotations

from pathlib import Path

from loguru import logger
from minio import Minio

from cnps.config import MinioConfig


def _client(cfg: MinioConfig) -> Minio:
    return Minio(
        cfg.endpoint,
        access_key=cfg.access_key,
        secret_key=cfg.secret_key,
        secure=cfg.secure,
    )


def download_raw_data(cfg: MinioConfig, dest_dir: Path) -> list[Path]:
    """
    Download every object under ``raw_prefix`` into *dest_dir*.

    Objects already present locally with a matching size are skipped, so
    repeated calls only fetch new or changed files.

    Parameters
    ----------
    cfg : MinioConfig
        MinIO connection settings.
    dest_dir : Path
        Local directory to sync raw files into (created if missing).

    Returns
    -------
    list[Path]
        Local paths of files that were downloaded (excludes skipped files).
    """
    dest_dir.mkdir(parents=True, exist_ok=True)
    client = _client(cfg)

    downloaded: list[Path] = []
    objects = client.list_objects(cfg.bucket, prefix=cfg.raw_prefix, recursive=True)
    for obj in objects:
        name = obj.object_name[len(cfg.raw_prefix):]
        if not name or obj.object_name.endswith("/"):
            continue

        local_path = dest_dir / name
        if local_path.exists() and local_path.stat().st_size == obj.size:
            logger.debug("Skipping {} (already up to date)", name)
            continue

        logger.info("Downloading {} from MinIO ({} bytes)", name, obj.size)
        client.fget_object(cfg.bucket, obj.object_name, str(local_path))
        downloaded.append(local_path)

    logger.info(
        "MinIO sync complete: {} file(s) downloaded to {}", len(downloaded), dest_dir
    )
    return downloaded


def upload_cleaned_data(cfg: MinioConfig, file_path: Path) -> str:
    """
    Upload a single cleaned data file to the ``cleaned_prefix`` location.

    Parameters
    ----------
    cfg : MinioConfig
        MinIO connection settings.
    file_path : Path
        Local file to upload (e.g. ``data/cleaned/cnps_cleaned.parquet``).

    Returns
    -------
    str
        The destination object name in the bucket.
    """
    client = _client(cfg)
    if not client.bucket_exists(cfg.bucket):
        client.make_bucket(cfg.bucket)

    object_name = f"{cfg.cleaned_prefix}{file_path.name}"
    client.fput_object(cfg.bucket, object_name, str(file_path))
    logger.info("Uploaded {} to MinIO as {}/{}", file_path, cfg.bucket, object_name)
    return object_name
