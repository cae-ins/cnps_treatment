"""
Client de stockage objet MinIO pour le pipeline de traitement CNPS.

Gere la synchronisation des declarations Excel brutes depuis le bucket MinIO
vers le dossier local ``data/raw`` avant l'ingestion, ainsi que l'envoi du
fichier Parquet nettoye vers MinIO apres l'etape de nettoyage.

Les identifiants sont lus depuis les variables d'environnement
``MINIO_ACCESS_KEY`` / ``MINIO_SECRET_KEY`` (voir :class:`cnps.config.MinioConfig`) ;
les parametres de connexion (endpoint, bucket, prefixes) viennent de
``settings.yaml``.
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
    Telecharge tous les objets sous ``raw_prefix`` vers *dest_dir*.

    Les objets deja presents localement avec une taille identique sont
    ignores : les appels repetes ne recuperent donc que les fichiers
    nouveaux ou modifies.

    Parameters
    ----------
    cfg : MinioConfig
        Parametres de connexion MinIO.
    dest_dir : Path
        Dossier local a synchroniser avec les fichiers bruts (cree si absent).

    Returns
    -------
    list[Path]
        Chemins locaux des fichiers effectivement telecharges (les fichiers
        ignores car deja a jour ne sont pas inclus).
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
            logger.debug("Ignore {} (deja a jour)", name)
            continue

        logger.info("Telechargement de {} depuis MinIO ({} octets)", name, obj.size)
        client.fget_object(cfg.bucket, obj.object_name, str(local_path))
        downloaded.append(local_path)

    logger.info(
        "Synchronisation MinIO terminee : {} fichier(s) telecharge(s) vers {}",
        len(downloaded), dest_dir,
    )
    return downloaded


def upload_cleaned_data(cfg: MinioConfig, file_path: Path) -> str:
    """
    Envoie un fichier de donnees nettoyees vers l'emplacement ``cleaned_prefix``.

    Parameters
    ----------
    cfg : MinioConfig
        Parametres de connexion MinIO.
    file_path : Path
        Fichier local a envoyer (ex. ``data/cleaned/cnps_cleaned.parquet``).

    Returns
    -------
    str
        Nom de l'objet de destination dans le bucket.
    """
    client = _client(cfg)
    if not client.bucket_exists(cfg.bucket):
        client.make_bucket(cfg.bucket)

    object_name = f"{cfg.cleaned_prefix}{file_path.name}"
    client.fput_object(cfg.bucket, object_name, str(file_path))
    logger.info("Fichier {} envoye vers MinIO sous {}/{}", file_path, cfg.bucket, object_name)
    return object_name
