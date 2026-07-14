"""Integration avec le stockage objet (MinIO) pour le pipeline CNPS."""

from cnps.storage.minio_client import (
    delete_object,
    get_client,
    list_objects,
    object_exists,
    read_bytes,
    write_bytes,
)
from cnps.storage.objects import (
    read_excel_bytes,
    read_json,
    read_parquet,
    read_pickle,
    write_json,
    write_parquet,
    write_pickle,
    write_workbook,
)

__all__ = [
    "get_client",
    "object_exists",
    "read_bytes",
    "write_bytes",
    "list_objects",
    "delete_object",
    "read_parquet",
    "write_parquet",
    "read_pickle",
    "write_pickle",
    "read_excel_bytes",
    "write_workbook",
    "read_json",
    "write_json",
]
