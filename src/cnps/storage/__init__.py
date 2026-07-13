"""Integration avec le stockage objet (MinIO) pour le pipeline CNPS."""

from cnps.storage.minio_client import download_raw_data, upload_cleaned_data

__all__ = ["download_raw_data", "upload_cleaned_data"]
