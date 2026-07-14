"""
Helpers de (de)serialisation par format, construits sur les primitives
bas niveau de :mod:`cnps.storage.minio_client`.

Chaque fonction lit/ecrit un objet MinIO en passant par un buffer memoire
(``io.BytesIO``) : aucun fichier temporaire n'est jamais cree sur disque.
"""

from __future__ import annotations

import io
import json
import pickle
from collections.abc import Callable
from typing import Any

import polars as pl

from cnps.config import MinioConfig
from cnps.storage.minio_client import read_bytes, write_bytes


def read_parquet(cfg: MinioConfig, object_name: str) -> pl.DataFrame:
    """Lit un objet Parquet MinIO directement en DataFrame Polars."""
    data = read_bytes(cfg, object_name)
    return pl.read_parquet(io.BytesIO(data))


def write_parquet(
    cfg: MinioConfig,
    object_name: str,
    df: pl.DataFrame,
    *,
    compression: str = "zstd",
) -> None:
    """Ecrit un DataFrame Polars vers un objet Parquet MinIO."""
    buf = io.BytesIO()
    df.write_parquet(buf, compression=compression)
    write_bytes(cfg, object_name, buf.getvalue())


def read_pickle(cfg: MinioConfig, object_name: str) -> Any:
    """Charge un objet Python serialise (pickle) depuis MinIO."""
    return pickle.loads(read_bytes(cfg, object_name))


def write_pickle(cfg: MinioConfig, object_name: str, obj: Any) -> None:
    """Serialise (pickle) un objet Python et l'envoie vers MinIO."""
    buf = io.BytesIO()
    pickle.dump(obj, buf)
    write_bytes(cfg, object_name, buf.getvalue())


def read_excel_bytes(cfg: MinioConfig, object_name: str) -> io.BytesIO:
    """Telecharge un classeur Excel MinIO et le retourne comme buffer memoire.

    Le buffer est pret pour ``openpyxl.load_workbook(buf)`` ou
    ``pl.read_excel(buf, ...)``.
    """
    return io.BytesIO(read_bytes(cfg, object_name))


def write_workbook(
    cfg: MinioConfig,
    object_name: str,
    write_fn: Callable[[io.BytesIO], None],
) -> None:
    """Construit un classeur Excel dans un buffer memoire puis l'envoie vers MinIO.

    ``write_fn`` recoit un ``io.BytesIO`` vide et doit y ecrire le classeur
    (via ``xlsxwriter.Workbook(buf)`` suivi de ``wb.close()``, ou
    ``pl.DataFrame.write_excel(buf)``).
    """
    buf = io.BytesIO()
    write_fn(buf)
    write_bytes(
        cfg,
        object_name,
        buf.getvalue(),
        content_type="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    )


def read_json(cfg: MinioConfig, object_name: str) -> dict:
    """Lit un objet JSON MinIO et le decode en dict."""
    return json.loads(read_bytes(cfg, object_name).decode("utf-8"))


def write_json(cfg: MinioConfig, object_name: str, data: dict) -> None:
    """Encode un dict en JSON et l'envoie vers MinIO."""
    payload = json.dumps(data, indent=2, ensure_ascii=False).encode("utf-8")
    write_bytes(cfg, object_name, payload, content_type="application/json")
