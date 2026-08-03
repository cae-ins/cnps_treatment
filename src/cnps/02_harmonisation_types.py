"""
Etape 2/12 — Harmonisation des types de colonnes.

Relit chaque Parquet mensuel produit par l'etape 1 (lecture des fichiers,
qui lit tout en chaines de caracteres pour eviter les conflits de type
entre feuilles Excel heterogenes) et impose un schema coherent :

- Normalisation des noms de colonnes (majuscules, espaces retires)
- Parsing des dates depuis plusieurs formats (ISO, francais, serial Excel)
- Coercion numerique des colonnes de salaire et d'effectif
- Forçage des colonnes d'identifiant en chaines de caracteres

References
----------
Van Buuren, S. (2018). *Flexible Imputation of Missing Data* (2nd ed.).
    Chapman & Hall/CRC. — Chapitre 2 sur le pretraitement des donnees.
"""

from __future__ import annotations

import re
from datetime import datetime

import polars as pl
from loguru import logger

from cnps.config import PipelineConfig, load_config
from cnps.storage import list_objects, read_parquet, write_parquet

# ---------------------------------------------------------------------------
# Definition du schema cible
# ---------------------------------------------------------------------------

# Colonnes et leur type cible. Les colonnes non listees sont laissees telles quelles.
_NUMERIC_COLS = [
    "SALAIRE_BRUT",
    "DUREE_TRAVAILLEE",
    "SALAIRE_BRUT_MENS",
    "EFFECTIF",
    "EFFECTIF_DECLARE",
    "EFFECTIF_SALARIES",
]

_DATE_COLS = [
    "DATE_NAISSANCE",
    "DATE_EMBAUCHE",
    "DATE_IMMATRICULATION",
    "DATE_IMMAT_EMPLOYEUR",
]

_ID_COLS = [
    "ID_INDIV",
    "ID_EMPLOI",
    "ID_EMPLOYEUR",
]

_INTEGER_COLS = ["EFFECTIF", "EFFECTIF_DECLARE", "EFFECTIF_SALARIES"]

# Origine des dates seriel Excel (jour 0 = 30 decembre 1899)
_EXCEL_ORIGIN = datetime(1899, 12, 30)


# ---------------------------------------------------------------------------
# Helpers internes
# ---------------------------------------------------------------------------


def _normalize_column_names(df: pl.DataFrame) -> pl.DataFrame:
    """Majuscules, espaces en debut/fin retires, espaces internes en underscore."""
    rename_map = {col: col.strip().upper().replace(" ", "_") for col in df.columns}
    return df.rename(rename_map)


def _coerce_numeric(
    df: pl.DataFrame,
    cols: list[str],
    failure_threshold: float,
) -> pl.DataFrame:
    """Parse les formats francais/internationaux et bloque les pertes excessives."""
    for col in cols:
        if col in df.columns:
            raw = pl.col(col).cast(pl.Utf8).str.strip_chars()
            cleaned = raw.str.replace_all(r"[\s\u00A0\u202F']", "").str.replace_all(
                r"[^\d,.\-]", ""
            )
            european = cleaned.str.contains(r"^-?\d{1,3}(?:\.\d{3})+,\d+$")
            international = cleaned.str.contains(r"^-?\d{1,3}(?:,\d{3})+\.\d+$")
            comma_only = cleaned.str.contains(",") & ~cleaned.str.contains(r"\.")
            normalized = (
                pl.when(european)
                .then(cleaned.str.replace_all(r"\.", "").str.replace(",", "."))
                .when(international)
                .then(cleaned.str.replace_all(",", ""))
                .when(comma_only)
                .then(cleaned.str.replace(",", "."))
                .otherwise(cleaned.str.replace_all(",", ""))
            )
            parsed = normalized.cast(pl.Float64, strict=False)
            nonempty = raw.is_not_null() & (raw != "")
            n_nonempty = int(df.select(nonempty.sum()).item())
            n_failed = int(df.select((nonempty & parsed.is_null()).sum()).item())
            failure_share = n_failed / n_nonempty if n_nonempty else 0.0
            logger.info(
                "Parsing numerique {} : {} echec(s) sur {} valeurs non vides ({:.3%}).",
                col,
                n_failed,
                n_nonempty,
                failure_share,
            )
            if failure_share > failure_threshold:
                raise ValueError(
                    f"Echec de parsing numerique trop frequent pour {col}: "
                    f"{failure_share:.3%} > {failure_threshold:.3%}."
                )
            df = df.with_columns(parsed.alias(col))
            if col in _INTEGER_COLS:
                df = df.with_columns(pl.col(col).cast(pl.Int64, strict=False).alias(col))
    return df


def _parse_date_column(series: pl.Series) -> pl.Series:
    """
    Tente de parser une colonne de date depuis plusieurs formats, dans l'ordre :
    1. Deja de type Date/Datetime
    2. Duration (dates seriel Excel lues comme des durees)
    3. Chaine ISO (AAAA-MM-JJ)
    4. Chaine francaise (JJ/MM/AAAA)
    5. Nombre seriel Excel

    Retourne une serie de type Date.
    """
    name = series.name
    _MS_PER_DAY = 86_400_000  # millisecondes par jour

    if series.dtype in (pl.Date, pl.Datetime):
        return series.cast(pl.Date)

    # Dates seriel Excel lues comme Duration("d") par openpyxl/polars
    if series.dtype.base_type() == pl.Duration:
        temp = pl.DataFrame({name: series})
        result = temp.select(
            (
                pl.lit(_EXCEL_ORIGIN).cast(pl.Date)
                + (pl.col(name).dt.total_days() * _MS_PER_DAY)
                .cast(pl.Int64)
                .cast(pl.Duration("ms"))
            )
            .cast(pl.Date)
            .alias(name)
        )[name]
        return result

    s = series.cast(pl.Utf8)

    # Essaie chaque format, en ne remplissant que les valeurs encore nulles
    parsed = s.str.to_date("%Y-%m-%d %H:%M:%S", strict=False)

    still_null = parsed.is_null() & s.is_not_null()
    if still_null.any():
        iso_date = s.str.to_date("%Y-%m-%d", strict=False)
        parsed = parsed.zip_with(parsed.is_not_null(), iso_date)

    still_null = parsed.is_null() & s.is_not_null()
    if still_null.any():
        french = s.str.to_date("%d/%m/%Y", strict=False)
        parsed = parsed.zip_with(parsed.is_not_null(), french)

    still_null = parsed.is_null() & s.is_not_null()
    if still_null.any():
        numeric = s.cast(pl.Float64, strict=False)
        # Duration Polars ne supporte que ns/us/ms, donc conversion jours -> ms
        temp = pl.DataFrame({"_parsed": parsed, "_days": numeric})
        excel_dates = temp.select(
            (
                pl.lit(_EXCEL_ORIGIN).cast(pl.Date)
                + (pl.col("_days") * _MS_PER_DAY).cast(pl.Int64).cast(pl.Duration("ms"))
            )
            .cast(pl.Date)
            .alias("_excel")
        )["_excel"]
        parsed = parsed.zip_with(parsed.is_not_null(), excel_dates)

    return parsed.alias(name)


def _coerce_dates(df: pl.DataFrame, cols: list[str]) -> pl.DataFrame:
    """Parse les colonnes de date depuis leurs formats d'origine varies."""
    for col in cols:
        if col in df.columns:
            try:
                df = df.with_columns(_parse_date_column(df[col]))
            except Exception as exc:
                logger.warning("Parsing de dates impossible pour la colonne '{}': {}", col, exc)
    return df


def _ensure_string_ids(df: pl.DataFrame, cols: list[str]) -> pl.DataFrame:
    """Force les colonnes d'identifiant en chaines de caracteres."""
    for col in cols:
        if col in df.columns:
            df = df.with_columns(pl.col(col).cast(pl.Utf8).alias(col))
    return df


# ---------------------------------------------------------------------------
# API publique
# ---------------------------------------------------------------------------


def harmoniser_types(cfg: PipelineConfig) -> list[str]:
    """
    Harmonise les types de colonnes sur tous les Parquets mensuels de MinIO.

    Relit chaque objet sous ``processed_prefix``, applique la coercion de
    types, et le reecrit au meme endroit (mise a jour en place).

    Parameters
    ----------
    cfg : PipelineConfig
        Configuration du pipeline.

    Returns
    -------
    list[str]
        Noms des objets Parquet harmonises.
    """
    bucket = cfg.minio.processed_bucket
    all_objects = list_objects(cfg.minio, bucket, cfg.minio.processed_prefix, recursive=False)
    files = sorted(obj for obj in all_objects if re.search(r"\.parquet$", obj))

    if not files:
        logger.warning(
            "Aucun fichier Parquet trouve sous {}/{}", bucket, cfg.minio.processed_prefix
        )
        return []

    result_objects: list[str] = []
    total_rows = 0

    for object_name in files:
        df = read_parquet(cfg.minio, bucket, object_name)

        df = _normalize_column_names(df)
        df = _ensure_string_ids(df, _ID_COLS)
        df = _coerce_numeric(
            df,
            _NUMERIC_COLS,
            cfg.cleaning.numeric_parse_failure_threshold,
        )
        df = _coerce_dates(df, _DATE_COLS)

        write_parquet(cfg.minio, bucket, object_name, df)
        result_objects.append(object_name)
        total_rows += df.height

        logger.info("  {} -> {} lignes, {} colonnes", object_name, df.height, df.width)

    logger.info(
        "Harmonisation des types terminee pour {} fichiers, {} lignes au total",
        len(result_objects),
        total_rows,
    )
    return result_objects


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
        level="DEBUG",
        rotation="10 MB",
        retention="30 days",
        encoding="utf-8",
    )

    try:
        harmoniser_types(cfg)
        logger.info("Termine avec succes.")
    except Exception as exc:
        logger.exception("Echec de l'etape: {}", exc)
        sys.exit(1)
