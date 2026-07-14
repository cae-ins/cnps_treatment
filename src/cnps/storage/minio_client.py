"""
Primitives bas niveau pour le stockage objet MinIO.

Ce module ne connait aucun format de donnees (parquet, pickle, excel...) :
il expose uniquement des operations generiques sur des octets bruts
(lecture, ecriture, existence, listing). Les helpers types par format
vivent dans :mod:`cnps.storage.objects`.

Les identifiants sont lus depuis les variables d'environnement
``MINIO_ACCESS_KEY`` / ``MINIO_SECRET_KEY`` (voir :class:`cnps.config.MinioConfig`) ;
les parametres de connexion (endpoint, bucket, prefixes) viennent de
``settings.yaml``.
"""

from __future__ import annotations

from loguru import logger
from minio import Minio
from minio.error import S3Error

from cnps.config import MinioConfig

_NOT_FOUND_CODES = {"NoSuchKey", "NoSuchObject"}


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


def object_exists(cfg: MinioConfig, object_name: str) -> bool:
    """Indique si un objet existe dans le bucket, sans le telecharger."""
    client = get_client(cfg)
    try:
        client.stat_object(cfg.bucket, object_name)
        return True
    except S3Error as exc:
        if exc.code in _NOT_FOUND_CODES:
            return False
        raise


def read_bytes(cfg: MinioConfig, object_name: str) -> bytes:
    """Telecharge un objet et retourne son contenu brut en memoire."""
    client = get_client(cfg)
    response = client.get_object(cfg.bucket, object_name)
    try:
        return response.read()
    finally:
        response.close()
        response.release_conn()


def write_bytes(
    cfg: MinioConfig,
    object_name: str,
    data: bytes,
    content_type: str = "application/octet-stream",
) -> None:
    """Envoie des octets bruts vers un objet du bucket (cree le bucket si absent)."""
    import io

    client = get_client(cfg)
    _ensure_bucket(client, cfg.bucket)
    client.put_object(
        cfg.bucket,
        object_name,
        io.BytesIO(data),
        length=len(data),
        content_type=content_type,
    )
    logger.debug("Objet ecrit sur MinIO : {}/{} ({} octets)", cfg.bucket, object_name, len(data))


def list_objects(cfg: MinioConfig, prefix: str, recursive: bool = True) -> list[str]:
    """Liste les noms d'objets sous un prefixe donne (exclut les entrees dossier)."""
    client = get_client(cfg)
    objects = client.list_objects(cfg.bucket, prefix=prefix, recursive=recursive)
    return [o.object_name for o in objects if not o.object_name.endswith("/")]


def delete_object(cfg: MinioConfig, object_name: str) -> None:
    """Supprime un objet du bucket (utilitaire de nettoyage, notamment pour les tests)."""
    client = get_client(cfg)
    client.remove_object(cfg.bucket, object_name)
