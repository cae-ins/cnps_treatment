"""Object storage integration (MinIO) for the CNPS pipeline."""

from cnps.storage.minio_client import download_raw_data, upload_cleaned_data

__all__ = ["download_raw_data", "upload_cleaned_data"]
