"""
Etape 1/12 — Lecture des fichiers Excel bruts et conversion en Parquet.

Lit les declarations de salaires CNPS brutes (fichiers Excel au format
``MM_YYYY.xlsx``) depuis MinIO, concatene toutes les feuilles de chaque
classeur, et ecrit le resultat au format Parquet (colonnaire, compresse)
sur MinIO pour le traitement en aval.

Aucun fichier ne touche jamais le disque local : tout transite par des
buffers memoire (``io.BytesIO``).

Traitement incremental : un registre JSON (stocke lui aussi sur MinIO)
retient le hash MD5 de chaque fichier deja traite, pour ne reconvertir
que les fichiers dont le contenu a change.

References
----------
Apache Parquet format specification:
    https://parquet.apache.org/documentation/latest/
"""

from __future__ import annotations

import hashlib
import re
from datetime import datetime

import polars as pl
from joblib import Parallel, delayed
from loguru import logger

from cnps.config import MinioConfig, PipelineConfig, load_config
from cnps.storage import (
    list_objects,
    object_exists,
    read_excel_bytes,
    read_json,
    write_json,
    write_parquet,
)

_REGISTRY_OBJECT_NAME = ".file_registry.json"


# ---------------------------------------------------------------------------
# Helpers internes
# ---------------------------------------------------------------------------

def _bytes_hash(data: bytes) -> str:
    """Hash MD5 des octets bruts, utilise pour detecter les changements."""
    return hashlib.md5(data).hexdigest()


def _parse_period(filename: str, regex: str) -> tuple[int, int]:
    """Extrait (mois, annee) d'un nom de fichier ``MM_YYYY.xlsx``."""
    m = re.match(regex, filename)
    if not m:
        raise ValueError(
            f"Le fichier '{filename}' ne correspond pas au motif attendu '{regex}'"
        )
    return int(m.group(1)), int(m.group(2))


def _read_single_excel(data: bytes, filename: str, skip_sheets: list[str]) -> pl.DataFrame:
    """
    Lit toutes les feuilles d'un classeur Excel en memoire et les concatene.

    Toutes les colonnes sont lues comme chaines de caracteres pour eviter
    les problemes de coercion de type entre feuilles heterogenes ; le
    typage definitif est fait par l'etape d'harmonisation (etape 2).
    """
    import io

    import openpyxl

    buf = io.BytesIO(data)
    wb = openpyxl.load_workbook(buf, read_only=True, data_only=True)
    sheet_names = [
        s for s in wb.sheetnames
        if not any(re.match(pat, s) for pat in skip_sheets)
    ]
    wb.close()

    frames: list[pl.DataFrame] = []
    for sheet in sheet_names:
        try:
            df = pl.read_excel(io.BytesIO(data), sheet_name=sheet, infer_schema_length=0)
            if df.height > 0:
                frames.append(df)
                logger.debug("  Feuille '{}': {} lignes", sheet, df.height)
        except Exception as exc:
            logger.warning("  Feuille ignoree '{}' dans {}: {}", sheet, filename, exc)

    if not frames:
        logger.warning("Aucune donnee trouvee dans {}", filename)
        return pl.DataFrame()

    # Aligne les colonnes (union de toutes les feuilles) puis concatene
    all_cols = dict.fromkeys(col for f in frames for col in f.columns)
    aligned = []
    for f in frames:
        for col in all_cols:
            if col not in f.columns:
                f = f.with_columns(pl.lit(None).cast(pl.Utf8).alias(col))
        aligned.append(f.select(list(all_cols.keys())))

    return pl.concat(aligned, how="vertical")


def _process_one_file(
    minio_cfg: MinioConfig,
    raw_bucket: str,
    object_name: str,
    processed_bucket: str,
    processed_prefix: str,
    filename_regex: str,
    skip_sheets: list[str],
) -> dict:
    """Traite un fichier Excel MinIO : lecture, tag periode, ecriture Parquet sur MinIO."""
    filename = object_name.rsplit("/", 1)[-1]
    month, year = _parse_period(filename, filename_regex)
    logger.info("Ingestion de {} (periode: {:02d}/{:04d})", filename, month, year)

    data = read_excel_bytes(minio_cfg, raw_bucket, object_name).getvalue()
    df = _read_single_excel(data, filename, skip_sheets)
    if df.height == 0:
        return {"file": filename, "status": "empty", "rows": 0}

    df = df.with_columns(
        pl.lit(month).cast(pl.Int32).alias("MOIS"),
        pl.lit(year).cast(pl.Int32).alias("ANNEE"),
        pl.lit(f"{year:04d}-{month:02d}").alias("PERIOD"),
    )

    out_object = f"{processed_prefix}{month:02d}_{year:04d}.parquet"
    write_parquet(minio_cfg, processed_bucket, out_object, df)

    return {
        "file": filename,
        "status": "ok",
        "rows": df.height,
        "columns": df.width,
        "output": out_object,
        "hash": _bytes_hash(data),
        "timestamp": datetime.now().isoformat(),
    }


class _FileRegistry:
    """Registre JSON (stocke sur MinIO) des fichiers deja traites et de leur hash."""

    def __init__(self, minio_cfg: MinioConfig, processed_bucket: str, processed_prefix: str) -> None:
        self._cfg = minio_cfg
        self._bucket = processed_bucket
        self._object_name = f"{processed_prefix}{_REGISTRY_OBJECT_NAME}"
        self._data: dict[str, str] = {}
        if object_exists(minio_cfg, processed_bucket, self._object_name):
            self._data = read_json(minio_cfg, processed_bucket, self._object_name)

    def needs_processing(self, filename: str, data: bytes) -> bool:
        return self._data.get(filename) != _bytes_hash(data)

    def update(self, file_name: str, file_hash: str) -> None:
        self._data[file_name] = file_hash
        write_json(self._cfg, self._bucket, self._object_name, self._data)


# ---------------------------------------------------------------------------
# API publique
# ---------------------------------------------------------------------------

def lire_fichiers(cfg: PipelineConfig) -> list[dict]:
    """
    Convertit tous les fichiers Excel bruts (MinIO) en Parquet (MinIO).

    Traitement incremental : seuls les fichiers dont le contenu a change
    (detecte par hash MD5) sont retraites.

    Parameters
    ----------
    cfg : PipelineConfig
        Configuration du pipeline.

    Returns
    -------
    list[dict]
        Metadonnees de chaque fichier traite (statut, lignes, hash...).
    """
    filename_regex = cfg.ingestion.filename_regex

    all_objects = list_objects(cfg.minio, cfg.minio.raw_bucket, cfg.minio.raw_prefix, recursive=False)
    files = sorted(
        obj for obj in all_objects
        if re.match(filename_regex, obj.rsplit("/", 1)[-1])
    )
    if not files:
        logger.warning(
            "Aucun objet correspondant a '{}' sous '{}/{}'",
            filename_regex, cfg.minio.raw_bucket, cfg.minio.raw_prefix,
        )
        return []

    registry = _FileRegistry(cfg.minio, cfg.minio.processed_bucket, cfg.minio.processed_prefix)

    # Ne garde que les fichiers modifies (necessite de les telecharger pour les hasher)
    to_process: list[str] = []
    for object_name in files:
        filename = object_name.rsplit("/", 1)[-1]
        data = read_excel_bytes(cfg.minio, cfg.minio.raw_bucket, object_name).getvalue()
        if registry.needs_processing(filename, data):
            to_process.append(object_name)
    logger.info("{} fichiers trouves, {} a traiter", len(files), len(to_process))

    if not to_process:
        logger.info("Tous les fichiers sont a jour, rien a ingerer.")
        return []

    # Traitement (parallele si plus d'un fichier)
    results: list[dict]
    if len(to_process) == 1 or cfg.parallel.n_jobs == 1:
        results = [
            _process_one_file(
                cfg.minio, cfg.minio.raw_bucket, object_name,
                cfg.minio.processed_bucket, cfg.minio.processed_prefix,
                filename_regex, cfg.ingestion.skip_sheets,
            )
            for object_name in to_process
        ]
    else:
        results = Parallel(n_jobs=cfg.parallel.n_jobs, backend=cfg.parallel.backend)(
            delayed(_process_one_file)(
                cfg.minio, cfg.minio.raw_bucket, object_name,
                cfg.minio.processed_bucket, cfg.minio.processed_prefix,
                filename_regex, cfg.ingestion.skip_sheets,
            )
            for object_name in to_process
        )

    for r in results:
        if r["status"] == "ok":
            registry.update(r["file"], r.get("hash", ""))

    ok = sum(1 for r in results if r["status"] == "ok")
    logger.info("Ingestion terminee : {}/{} fichiers traites avec succes", ok, len(to_process))

    return results


if __name__ == "__main__":
    import argparse
    import sys
    from pathlib import Path

    parser = argparse.ArgumentParser(
        description=__doc__.strip().splitlines()[0] if __doc__ else None
    )
    parser.add_argument("--settings", "-s", type=Path, default=None)
    parser.add_argument("--dimensions", "-d", type=Path, default=None)
    parser.add_argument("--verbose", "-v", action="store_true")
    args = parser.parse_args()

    cfg = load_config(args.settings, args.dimensions)

    logger.remove()
    logger.add(
        sys.stderr,
        level="DEBUG" if args.verbose else "INFO",
        colorize=True,
        format="<green>{time:HH:mm:ss}</green> | <level>{level:<8}</level> | {message}",
    )
    logger.add(
        str(cfg.paths.logs / f"{Path(__file__).stem}.log"),
        level="DEBUG", rotation="10 MB", retention="30 days", encoding="utf-8",
    )

    try:
        lire_fichiers(cfg)
        logger.info("Termine avec succes.")
    except Exception as exc:
        logger.exception("Echec de l'etape: {}", exc)
        sys.exit(1)
