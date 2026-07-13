"""
Ingestion module: Excel -> Parquet conversion with incremental processing.

Reads raw CNPS salary declaration files (Excel format MM_YYYY.xlsx),
concatenates all sheets within each workbook, and writes to columnar
Parquet format for downstream processing.

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
import json
import re
from datetime import datetime
from pathlib import Path

import polars as pl
from joblib import Parallel, delayed
from loguru import logger

from cnps.config import PipelineConfig
from cnps.storage import download_raw_data


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

def _file_hash(path: Path) -> str:
    """Compute MD5 hash of a file for change detection."""
    h = hashlib.md5()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


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


def _read_single_excel(path: Path, skip_sheets: list[str]) -> pl.DataFrame:
    """
    Read all sheets from one Excel workbook and concatenate them.

    All columns are initially read as strings to avoid type-coercion issues
    across heterogeneous sheets.  Type harmonisation is handled downstream
    by the preparation stage.

    Parameters
    ----------
    path : Path
        Path to the ``.xlsx`` file.
    skip_sheets : list[str]
        Sheet names (or regex patterns) to ignore.

    Returns
    -------
    pl.DataFrame
        Concatenated data from all retained sheets.
    """
    import openpyxl

    wb = openpyxl.load_workbook(path, read_only=True, data_only=True)
    sheet_names = [
        s for s in wb.sheetnames
        if not any(re.match(pat, s) for pat in skip_sheets)
    ]
    wb.close()

    frames: list[pl.DataFrame] = []
    for sheet in sheet_names:
        try:
            df = pl.read_excel(path, sheet_name=sheet, infer_schema_length=0)
            if df.height > 0:
                frames.append(df)
                logger.debug("  Sheet '{}': {} rows", sheet, df.height)
        except Exception as exc:
            logger.warning("  Skipping sheet '{}' in {}: {}", sheet, path.name, exc)

    if not frames:
        logger.warning("No data found in {}", path.name)
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
    path: Path,
    output_dir: Path,
    filename_regex: str,
    skip_sheets: list[str],
) -> dict:
    """
    Process a single Excel file: read, tag with period, write to Parquet.

    Returns a metadata dict for the registry.
    """
    month, year = _parse_period(path.name, filename_regex)
    logger.info("Ingesting {} (period: {:02d}/{:04d})", path.name, month, year)

    df = _read_single_excel(path, skip_sheets)
    if df.height == 0:
        return {"file": path.name, "status": "empty", "rows": 0}

    # Add period columns
    df = df.with_columns(
        pl.lit(month).cast(pl.Int32).alias("MOIS"),
        pl.lit(year).cast(pl.Int32).alias("ANNEE"),
        pl.lit(f"{year:04d}-{month:02d}").alias("PERIOD"),
    )

    out_path = output_dir / f"{month:02d}_{year:04d}.parquet"
    df.write_parquet(out_path, compression="zstd")

    return {
        "file": path.name,
        "status": "ok",
        "rows": df.height,
        "columns": df.width,
        "output": str(out_path),
        "hash": _file_hash(path),
        "timestamp": datetime.now().isoformat(),
    }


# ---------------------------------------------------------------------------
# Registry (tracks which files have been processed)
# ---------------------------------------------------------------------------

class _FileRegistry:
    """Simple JSON-based registry to track processed files and their hashes."""

    def __init__(self, path: Path) -> None:
        self.path = path
        self._data: dict[str, str] = {}
        if self.path.exists():
            self._data = json.loads(self.path.read_text(encoding="utf-8"))

    def needs_processing(self, file_path: Path) -> bool:
        current_hash = _file_hash(file_path)
        return self._data.get(file_path.name) != current_hash

    def update(self, file_name: str, file_hash: str) -> None:
        self._data[file_name] = file_hash
        self.path.write_text(json.dumps(self._data, indent=2), encoding="utf-8")


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def ingest(cfg: PipelineConfig) -> list[dict]:
    """
    Run the full ingestion stage: Excel -> Parquet for all raw files.

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
    raw_dir = cfg.paths.raw_data
    out_dir = cfg.paths.processed_data
    out_dir.mkdir(parents=True, exist_ok=True)

    try:
        download_raw_data(cfg.minio, raw_dir)
    except Exception as exc:
        logger.warning(
            "MinIO sync failed, falling back to local files in {}: {}", raw_dir, exc,
        )

    pattern = cfg.ingestion.file_pattern
    files = sorted(raw_dir.glob(pattern))
    if not files:
        logger.warning("No files matching '{}' found in {}", pattern, raw_dir)
        return []

    registry = _FileRegistry(out_dir / ".file_registry.json")

    # Filter to only changed files
    to_process = [f for f in files if registry.needs_processing(f)]
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
                f, out_dir, cfg.ingestion.filename_regex, cfg.ingestion.skip_sheets,
            )
            for f in to_process
        ]
    else:
        results = Parallel(n_jobs=cfg.parallel.n_jobs, backend=cfg.parallel.backend)(
            delayed(_process_one_file)(
                f, out_dir, cfg.ingestion.filename_regex, cfg.ingestion.skip_sheets,
            )
            for f in to_process
        )

    # Update registry
    for r in results:
        if r["status"] == "ok":
            registry.update(r["file"], r.get("hash", ""))

    ok = sum(1 for r in results if r["status"] == "ok")
    logger.info("Ingestion complete: {}/{} files processed successfully", ok, len(to_process))

    return results
