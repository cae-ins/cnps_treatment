"""
Stockage objet MinIO pour le pipeline CNPS.

Organisation en couches (medaillon) : les donnees du pipeline (brutes,
intermediaires, modeles, exports) vivent sur MinIO, reparties sur
plusieurs buckets distincts (ex: ``staging`` pour les Excel bruts,
``silver`` pour les Parquets intermediaires, ``gold`` pour les modeles
et exports finaux). Chaque fonction de ce module prend donc le nom du
bucket cible en parametre explicite, en plus du nom d'objet (chemin a
l'interieur du bucket).

Ce module expose :

- Des primitives bas niveau sur des octets bruts (lecture, ecriture,
  existence, listing) qui ne connaissent aucun format de fichier.
- Des helpers types par format (parquet, excel, json), construits
  sur les primitives ci-dessus, qui passent par un buffer memoire
  (``io.BytesIO``) : aucun fichier temporaire n'est jamais cree sur disque.

Les identifiants de connexion sont lus depuis les variables d'environnement
``MINIO_ACCESS_KEY`` / ``MINIO_SECRET_KEY`` (voir :class:`cnps.config.MinioConfig`) ;
les parametres de connexion (endpoint, buckets, prefixes) viennent de
``settings.yaml``.
"""

from __future__ import annotations

import io
import json
import math
from collections.abc import Callable
from numbers import Real

import polars as pl
from loguru import logger
from minio import Minio
from minio.error import S3Error

from cnps.config import MinioConfig

_NOT_FOUND_CODES = {"NoSuchKey", "NoSuchObject"}


# ---------------------------------------------------------------------------
# Primitives bas niveau (octets bruts)
# ---------------------------------------------------------------------------


def get_client(cfg: MinioConfig) -> Minio:
    """Construit un client MinIO a partir des parametres de connexion."""
    return Minio(
        cfg.endpoint,
        access_key=cfg.access_key,
        secret_key=cfg.secret_key,
        secure=cfg.secure,
    )


def _ensure_bucket(client: Minio, bucket: str) -> None:
    if not client.bucket_exists(bucket):
        client.make_bucket(bucket)


def object_exists(cfg: MinioConfig, bucket: str, object_name: str) -> bool:
    """Indique si un objet existe dans le bucket donne, sans le telecharger."""
    client = get_client(cfg)
    try:
        client.stat_object(bucket, object_name)
        return True
    except S3Error as exc:
        if exc.code in _NOT_FOUND_CODES:
            return False
        raise


def read_bytes(cfg: MinioConfig, bucket: str, object_name: str) -> bytes:
    """Telecharge un objet et retourne son contenu brut en memoire."""
    client = get_client(cfg)
    response = client.get_object(bucket, object_name)
    try:
        return response.read()
    finally:
        response.close()
        response.release_conn()


def write_bytes(
    cfg: MinioConfig,
    bucket: str,
    object_name: str,
    data: bytes,
    content_type: str = "application/octet-stream",
) -> None:
    """Envoie des octets bruts vers un objet du bucket (cree le bucket si absent)."""
    client = get_client(cfg)
    _ensure_bucket(client, bucket)
    client.put_object(
        bucket,
        object_name,
        io.BytesIO(data),
        length=len(data),
        content_type=content_type,
    )
    logger.debug("Objet ecrit sur MinIO : {}/{} ({} octets)", bucket, object_name, len(data))


def list_objects(cfg: MinioConfig, bucket: str, prefix: str, recursive: bool = True) -> list[str]:
    """Liste les noms d'objets sous un prefixe donne (exclut les entrees dossier)."""
    client = get_client(cfg)
    objects = client.list_objects(bucket, prefix=prefix, recursive=recursive)
    return [o.object_name for o in objects if not o.object_name.endswith("/")]


def delete_object(cfg: MinioConfig, bucket: str, object_name: str) -> None:
    """Supprime un objet du bucket (utilitaire de nettoyage, notamment pour les tests)."""
    client = get_client(cfg)
    client.remove_object(bucket, object_name)


# ---------------------------------------------------------------------------
# Helpers types par format
# ---------------------------------------------------------------------------


def read_parquet(cfg: MinioConfig, bucket: str, object_name: str) -> pl.DataFrame:
    """Lit un objet Parquet MinIO directement en DataFrame Polars."""
    data = read_bytes(cfg, bucket, object_name)
    return pl.read_parquet(io.BytesIO(data))


def write_parquet(
    cfg: MinioConfig,
    bucket: str,
    object_name: str,
    df: pl.DataFrame,
    *,
    compression: str = "zstd",
) -> None:
    """Ecrit un DataFrame Polars vers un objet Parquet MinIO."""
    buf = io.BytesIO()
    df.write_parquet(buf, compression=compression)
    write_bytes(cfg, bucket, object_name, buf.getvalue())


def read_excel_bytes(cfg: MinioConfig, bucket: str, object_name: str) -> io.BytesIO:
    """Telecharge un classeur Excel MinIO et le retourne comme buffer memoire.

    Le buffer est pret pour ``openpyxl.load_workbook(buf)`` ou
    ``pl.read_excel(buf, ...)``.
    """
    return io.BytesIO(read_bytes(cfg, bucket, object_name))


def write_workbook(
    cfg: MinioConfig,
    bucket: str,
    object_name: str,
    write_fn: Callable[[io.BytesIO], None],
) -> None:
    """Construit un classeur Excel dans un buffer memoire puis l'envoie vers MinIO.

    ``write_fn`` recoit un ``io.BytesIO`` vide et doit y ecrire le classeur
    (via ``xlsxwriter.Workbook(buf, {"in_memory": True})`` suivi de
    ``wb.close()``, ou ``pl.DataFrame.write_excel(buf)``).
    """
    buf = io.BytesIO()
    write_fn(buf)
    write_bytes(
        cfg,
        bucket,
        object_name,
        buf.getvalue(),
        content_type="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    )


def read_json(cfg: MinioConfig, bucket: str, object_name: str) -> dict:
    """Lit un objet JSON MinIO et le decode en dict."""
    return json.loads(read_bytes(cfg, bucket, object_name).decode("utf-8"))


def _json_safe(value):
    """Convertit recursivement les nombres non finis en null JSON."""
    if isinstance(value, dict):
        return {str(key): _json_safe(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [_json_safe(item) for item in value]
    if isinstance(value, (bool, int)):
        return value
    if isinstance(value, Real):
        return float(value) if math.isfinite(float(value)) else None
    return value


def write_json(cfg: MinioConfig, bucket: str, object_name: str, data: dict) -> None:
    """Encode un dict en JSON et l'envoie vers MinIO."""
    payload = json.dumps(
        _json_safe(data),
        indent=2,
        ensure_ascii=False,
        allow_nan=False,
    ).encode("utf-8")
    write_bytes(cfg, bucket, object_name, payload, content_type="application/json")
