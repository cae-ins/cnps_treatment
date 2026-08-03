"""
Etape 12/12 — Export Excel.

Genere des classeurs Excel formates a partir des resultats d'estimation
(etape 10) et du rapport de validation (etape 11) : une feuille par
dimension d'analyse, avec en-tetes stylises, formatage numerique,
volets figes et largeurs de colonnes ajustees.

Utilise XlsxWriter pour la performance (plus rapide qu'openpyxl en
ecriture seule), en ecrivant dans un buffer memoire avant l'envoi vers
MinIO — aucun fichier n'est jamais cree sur disque local.
"""

from __future__ import annotations

import importlib
import io
import math
from numbers import Real

import polars as pl
from loguru import logger

from cnps.config import PipelineConfig, load_config
from cnps.storage import write_workbook

_validation = importlib.import_module("cnps.11_validation_qualite")
ValidationReport = _validation.ValidationReport

_HEADER_COLOR = "#2C3E50"
_HEADER_FONT = "#FFFFFF"
_ALT_ROW_COLOR = "#F2F3F4"
_NUMBER_FMT = "#,##0"
_DECIMAL_FMT = "#,##0.00"


def _number_format_code(column: str) -> str:
    """Retourne le format Excel adapte a la statistique et a ses bornes."""
    base = column
    for suffix in ("_ci_lower", "_ci_upper", "_std_error"):
        if base.endswith(suffix):
            base = base[: -len(suffix)]
            break
    if base == "gini":
        return "0.000"
    if base in {"n_obs"}:
        return _NUMBER_FMT
    return _NUMBER_FMT


def exporter_indicateurs(
    cfg: PipelineConfig,
    results: pl.DataFrame,
    validation_report: ValidationReport,
    filename: str = "indicateurs_cnps.xlsx",
) -> str:
    """
    Exporte les resultats d'estimation vers un classeur Excel formate sur MinIO.

    Cree une feuille par dimension, avec en-tetes stylises, formatage
    numerique et largeurs de colonnes ajustees automatiquement.

    Parameters
    ----------
    cfg : PipelineConfig
        Configuration du pipeline.
    results : pl.DataFrame
        Resultats d'estimation (issus de ``estimer_indicateurs``).
    validation_report : ValidationReport
        Rapport produit par l'etape 11 a partir des memes resultats.
    filename : str
        Nom du fichier de sortie.

    Returns
    -------
    str
        Nom de l'objet Excel genere sur MinIO.
    """
    if not validation_report.is_valid:
        raise ValueError(
            "Export interdit: le rapport de validation contient "
            f"{len(validation_report.errors)} erreur(s). Executer et corriger "
            "l'etape 11_validation_qualite.py avant de relancer l'export."
        )

    out_object = f"{cfg.minio.output_prefix}{filename}"

    if "dimension" not in results.columns:
        results = results.with_columns(pl.lit("Resultats").alias("dimension"))

    dimensions = results["dimension"].unique().sort().to_list()
    logger.info(
        "Export de {} lignes sur {} feuilles ({})",
        results.height,
        len(dimensions),
        ", ".join(dimensions),
    )

    sheet_rows: dict[str, int] = {}
    n_non_finite_masked = 0

    def _write(buf: io.BytesIO) -> None:
        nonlocal n_non_finite_masked
        import xlsxwriter

        wb = xlsxwriter.Workbook(buf, {"in_memory": True})

        header_fmt = wb.add_format(
            {
                "bold": True,
                "font_color": _HEADER_FONT,
                "bg_color": _HEADER_COLOR,
                "border": 1,
                "text_wrap": True,
                "valign": "vcenter",
                "align": "center",
            }
        )
        text_fmt = wb.add_format({"border": 1, "text_wrap": True})
        alt_fmt = wb.add_format({"bg_color": _ALT_ROW_COLOR, "border": 1})
        number_formats: dict[tuple[str, bool], object] = {}

        def number_format(column: str, alternate: bool):
            key = (_number_format_code(column), alternate)
            if key not in number_formats:
                options = {"num_format": key[0], "border": 1}
                if alternate:
                    options["bg_color"] = _ALT_ROW_COLOR
                number_formats[key] = wb.add_format(options)
            return number_formats[key]

        stat_labels = {s.name: s.label for s in cfg.statistics}

        for dim_label in dimensions:
            sheet_name = dim_label[:31].replace("/", "-").replace("\\", "-")
            ws = wb.add_worksheet(sheet_name)

            dim_df = results.filter(pl.col("dimension") == dim_label)
            cols = [c for c in dim_df.columns if c != "dimension"]
            sheet_rows[dim_label] = dim_df.height

            for col_idx, col_name in enumerate(cols):
                label = stat_labels.get(col_name, col_name.replace("_", " ").title())
                ws.write(0, col_idx, label, header_fmt)

            for row_idx in range(dim_df.height):
                is_alt = row_idx % 2 == 1
                for col_idx, col_name in enumerate(cols):
                    value = dim_df[col_name][row_idx]

                    if value is None:
                        ws.write(row_idx + 1, col_idx, "—", text_fmt)
                    elif isinstance(value, Real):
                        if not math.isfinite(float(value)):
                            n_non_finite_masked += 1
                            ws.write(
                                row_idx + 1,
                                col_idx,
                                "—",
                                alt_fmt if is_alt else text_fmt,
                            )
                        else:
                            ws.write_number(
                                row_idx + 1,
                                col_idx,
                                float(value),
                                number_format(col_name, is_alt),
                            )
                    else:
                        fmt = alt_fmt if is_alt else text_fmt
                        ws.write(row_idx + 1, col_idx, str(value), fmt)

            for col_idx, col_name in enumerate(cols):
                max_len = max(
                    len(stat_labels.get(col_name, col_name)),
                    max(
                        (
                            len(str(dim_df[col_name][i] or ""))
                            for i in range(min(dim_df.height, 100))
                        ),
                        default=10,
                    ),
                )
                ws.set_column(col_idx, col_idx, min(max_len + 4, 30))

            ws.freeze_panes(1, 0)
            if cols:
                ws.autofilter(0, 0, dim_df.height, len(cols) - 1)

        wb.close()

    write_workbook(cfg.minio, cfg.minio.output_bucket, out_object, _write)
    for dim_label, n_rows in sheet_rows.items():
        logger.info("  Feuille '{}' : {} lignes", dim_label, n_rows)
    if n_non_finite_masked:
        logger.warning(
            "{} valeur(s) non finie(s) masquee(s) par un tiret dans l'export.",
            n_non_finite_masked,
        )
    logger.info("Indicateurs exportes vers {}", out_object)
    return out_object


def exporter_rapport_validation(
    cfg: PipelineConfig,
    report: ValidationReport,
    filename: str = "rapport_validation.xlsx",
) -> str:
    """Exporte le rapport de validation vers un classeur Excel sur MinIO."""
    out_object = f"{cfg.minio.output_prefix}{filename}"

    rows = [
        {
            "Niveau": issue.level,
            "Etape": issue.stage,
            "Verification": issue.check,
            "Message": issue.message,
        }
        for issue in report.issues
    ]
    n_errors = sum(1 for issue in report.issues if issue.level == "ERROR")
    n_warnings = sum(1 for issue in report.issues if issue.level == "WARNING")
    logger.info(
        "Export du rapport de validation : {} problemes ({} erreurs, {} avertissements)",
        len(rows),
        n_errors,
        n_warnings,
    )

    def _write(buf: io.BytesIO) -> None:
        if rows:
            pl.DataFrame(rows).write_excel(buf)
        else:
            import xlsxwriter

            wb = xlsxwriter.Workbook(buf, {"in_memory": True})
            ws = wb.add_worksheet("Validation")
            ws.write(0, 0, "Aucun probleme detecte")
            wb.close()

    write_workbook(cfg.minio, cfg.minio.output_bucket, out_object, _write)
    logger.info("Rapport de validation exporte vers {}", out_object)
    return out_object


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
        # En execution autonome, les resultats ne sont pas persistes. Ils sont
        # calcules une fois, valides, puis transmis a l'export.
        estimer = importlib.import_module("cnps.10_estimation_indicateurs").estimer_indicateurs
        results = estimer(cfg)
        report = _validation.valider_tout(cfg, results)
        exporter_indicateurs(cfg, results, report)
        logger.info("Termine avec succes.")
    except Exception as exc:
        logger.exception("Echec de l'etape: {}", exc)
        sys.exit(1)
