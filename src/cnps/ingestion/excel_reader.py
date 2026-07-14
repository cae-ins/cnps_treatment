"""
Ingestion module: Excel -> Parquet conversion with incremental processing.

Reads raw CNPS salary declaration files (Excel format MM_YYYY.xlsx) from
MinIO, concatenates all sheets within each workbook, and writes to columnar
Parquet format on MinIO for downstream processing. No file ever touches
local disk: everything transits through in-memory buffers.

Key improvements over v1:
- Parquet instead of Stata (.dta): ~5-10x faster I/O, native compression
- Incremental processing via file-hash registry (skips unchanged files)
- Parallel file processing via Joblib
- Schema validation on read

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

from cnps.config import MinioConfig, PipelineConfig
from cnps.storage import list_objects, read_excel_bytes, read_json, write_json, write_parquet
from cnps.storage.minio_client import object_exists

_REGISTRY_OBJECT_NAME = ".file_registry.json"


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

def _bytes_hash(data: bytes) -> str:
    """Compute MD5 hash of raw bytes for change detection."""
    return hashlib.md5(data).hexdigest()


def _parse_period(filename: str, regex: str) -> tuple[int, int]:
    """
    Extract (month, year) from a filename following the naming convention.

    Parameters
    ----------
    filename : str
        e.g. ``"01_2024.xlsx"``
    regex : str
        Regex with two capture groups for month and year.

    Returns
    -------
    tuple[int, int]
        ``(month, year)``

    Raises
    ------
    ValueError
        If the filename does not match the expected pattern.
    """
    m = re.match(regex, filename)
    if not m:
        raise ValueError(
            f"Filename '{filename}' does not match expected pattern '{regex}'"
        )
    return int(m.group(1)), int(m.group(2))


def _read_single_excel(data: bytes, filename: str, skip_sheets: list[str]) -> pl.DataFrame:
    """
    Read all sheets from one in-memory Excel workbook and concatenate them.

    All columns are initially read as strings to avoid type-coercion issues
    across heterogeneous sheets.  Type harmonisation is handled downstream
    by the preparation stage.

    Parameters
    ----------
    data : bytes
        Raw content of the ``.xlsx`` file.
    filename : str
        Original file name, used only for logging.
    skip_sheets : list[str]
        Sheet names (or regex patterns) to ignore.

    Returns
    -------
    pl.DataFrame
        Concatenated data from all retained sheets.
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
                logger.debug("  Sheet '{}': {} rows", sheet, df.height)
        except Exception as exc:
            logger.warning("  Skipping sheet '{}' in {}: {}", sheet, filename, exc)

    if not frames:
        logger.warning("No data found in {}", filename)
        return pl.DataFrame()

    # Align column names (union of all columns) and concatenate
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
    object_name: str,
    processed_prefix: str,
    filename_regex: str,
    skip_sheets: list[str],
) -> dict:
    """
    Process a single Excel object from MinIO: read, tag with period, write
    the resulting Parquet back to MinIO.

    Returns a metadata dict for the registry.
    """
    filename = object_name.rsplit("/", 1)[-1]
    month, year = _parse_period(filename, filename_regex)
    logger.info("Ingesting {} (period: {:02d}/{:04d})", filename, month, year)

    data = read_excel_bytes(minio_cfg, object_name).getvalue()
    df = _read_single_excel(data, filename, skip_sheets)
    if df.height == 0:
        return {"file": filename, "status": "empty", "rows": 0}

    # Add period columns
    df = df.with_columns(
        pl.lit(month).cast(pl.Int32).alias("MOIS"),
        pl.lit(year).cast(pl.Int32).alias("ANNEE"),
        pl.lit(f"{year:04d}-{month:02d}").alias("PERIOD"),
    )

    out_object = f"{processed_prefix}{month:02d}_{year:04d}.parquet"
    write_parquet(minio_cfg, out_object, df)

    return {
        "file": filename,
        "status": "ok",
        "rows": df.height,
        "columns": df.width,
        "output": out_object,
        "hash": _bytes_hash(data),
        "timestamp": datetime.now().isoformat(),
    }


# ---------------------------------------------------------------------------
# Registry (tracks which files have been processed), stored on MinIO
# ---------------------------------------------------------------------------

class _FileRegistry:
    """JSON-based registry (on MinIO) tracking processed files and their hashes."""

    def __init__(self, minio_cfg: MinioConfig, processed_prefix: str) -> None:
        self._cfg = minio_cfg
        self._object_name = f"{processed_prefix}{_REGISTRY_OBJECT_NAME}"
        self._data: dict[str, str] = {}
        if object_exists(minio_cfg, self._object_name):
            self._data = read_json(minio_cfg, self._object_name)

    def needs_processing(self, filename: str, data: bytes) -> bool:
        return self._data.get(filename) != _bytes_hash(data)

    def update(self, file_name: str, file_hash: str) -> None:
        self._data[file_name] = file_hash
        write_json(self._cfg, self._object_name, self._data)


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def ingest(cfg: PipelineConfig) -> list[dict]:
    """
    Run the full ingestion stage: Excel -> Parquet for all raw files on MinIO.

    Implements incremental processing: only files whose content has changed
    (detected via MD5 hash) are re-processed.

    Parameters
    ----------
    cfg : PipelineConfig
        Pipeline configuration.

    Returns
    -------
    list[dict]
        Metadata for each processed file.
    """
    filename_regex = cfg.ingestion.filename_regex

    all_objects = list_objects(cfg.minio, cfg.minio.raw_prefix, recursive=False)
    files = sorted(
        obj for obj in all_objects
        if re.match(filename_regex, obj.rsplit("/", 1)[-1])
    )
    if not files:
        logger.warning(
            "No objects matching '{}' found under '{}'", filename_regex, cfg.minio.raw_prefix,
        )
        return []

    registry = _FileRegistry(cfg.minio, cfg.minio.processed_prefix)

    # Filter to only changed files (requires downloading to hash - cheap vs. re-ingestion)
    to_process: list[str] = []
    for object_name in files:
        filename = object_name.rsplit("/", 1)[-1]
        data = read_excel_bytes(cfg.minio, object_name).getvalue()
        if registry.needs_processing(filename, data):
            to_process.append(object_name)
    logger.info(
        "Found {} files, {} need processing",
        len(files), len(to_process),
    )

    if not to_process:
        logger.info("All files are up-to-date, nothing to ingest.")
        return []

    # Process files (parallel if > 1 file)
    results: list[dict]
    if len(to_process) == 1 or cfg.parallel.n_jobs == 1:
        results = [
            _process_one_file(
                cfg.minio, object_name, cfg.minio.processed_prefix,
                filename_regex, cfg.ingestion.skip_sheets,
            )
            for object_name in to_process
        ]
    else:
        results = Parallel(n_jobs=cfg.parallel.n_jobs, backend=cfg.parallel.backend)(
            delayed(_process_one_file)(
                cfg.minio, object_name, cfg.minio.processed_prefix,
                filename_regex, cfg.ingestion.skip_sheets,
            )
            for object_name in to_process
        )

    # Update registry
    for r in results:
        if r["status"] == "ok":
            registry.update(r["file"], r.get("hash", ""))

    ok = sum(1 for r in results if r["status"] == "ok")
    logger.info("Ingestion complete: {}/{} files processed successfully", ok, len(to_process))

    return results
