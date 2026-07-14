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

import io

import polars as pl
from loguru import logger

from cnps.config import PipelineConfig
from cnps.storage import write_workbook

import importlib

_validation = importlib.import_module("cnps.11_validation_qualite")
ValidationReport = _validation.ValidationReport

_HEADER_COLOR = "#2C3E50"
_HEADER_FONT = "#FFFFFF"
_ALT_ROW_COLOR = "#F2F3F4"
_NUMBER_FMT = "#,##0"
_DECIMAL_FMT = "#,##0.00"


def exporter_indicateurs(
    cfg: PipelineConfig,
    results: pl.DataFrame,
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
    filename : str
        Nom du fichier de sortie.

    Returns
    -------
    str
        Nom de l'objet Excel genere sur MinIO.
    """
    out_object = f"{cfg.minio.output_prefix}{filename}"

    if "dimension" not in results.columns:
        results = results.with_columns(pl.lit("Resultats").alias("dimension"))

    dimensions = results["dimension"].unique().sort().to_list()

    def _write(buf: io.BytesIO) -> None:
        import xlsxwriter

        wb = xlsxwriter.Workbook(buf, {"in_memory": True})

        header_fmt = wb.add_format({
            "bold": True,
            "font_color": _HEADER_FONT,
            "bg_color": _HEADER_COLOR,
            "border": 1,
            "text_wrap": True,
            "valign": "vcenter",
            "align": "center",
        })
        number_fmt = wb.add_format({"num_format": _NUMBER_FMT, "border": 1})
        text_fmt = wb.add_format({"border": 1, "text_wrap": True})
        alt_fmt = wb.add_format({"bg_color": _ALT_ROW_COLOR, "border": 1})
        alt_number_fmt = wb.add_format({
            "num_format": _NUMBER_FMT, "bg_color": _ALT_ROW_COLOR, "border": 1,
        })

        stat_labels = {s.name: s.label for s in cfg.statistics}

        for dim_label in dimensions:
            sheet_name = dim_label[:31].replace("/", "-").replace("\\", "-")
            ws = wb.add_worksheet(sheet_name)

            dim_df = results.filter(pl.col("dimension") == dim_label)
            cols = [c for c in dim_df.columns if c != "dimension"]

            for col_idx, col_name in enumerate(cols):
                label = stat_labels.get(col_name, col_name.replace("_", " ").title())
                ws.write(0, col_idx, label, header_fmt)

            for row_idx in range(dim_df.height):
                is_alt = row_idx % 2 == 1
                for col_idx, col_name in enumerate(cols):
                    value = dim_df[col_name][row_idx]

                    if value is None:
                        ws.write(row_idx + 1, col_idx, "—", text_fmt)
                    elif isinstance(value, (int, float)):
                        fmt = alt_number_fmt if is_alt else number_fmt
                        ws.write_number(row_idx + 1, col_idx, value, fmt)
                    else:
                        fmt = alt_fmt if is_alt else text_fmt
                        ws.write(row_idx + 1, col_idx, str(value), fmt)

            for col_idx, col_name in enumerate(cols):
                max_len = max(
                    len(stat_labels.get(col_name, col_name)),
                    max((len(str(dim_df[col_name][i] or ""))
                         for i in range(min(dim_df.height, 100))),
                        default=10),
                )
                ws.set_column(col_idx, col_idx, min(max_len + 4, 30))

            ws.freeze_panes(1, 0)
            if cols:
                ws.autofilter(0, 0, dim_df.height, len(cols) - 1)

        wb.close()

    write_workbook(cfg.minio, out_object, _write)
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

    def _write(buf: io.BytesIO) -> None:
        if rows:
            pl.DataFrame(rows).write_excel(buf)
        else:
            import xlsxwriter
            wb = xlsxwriter.Workbook(buf, {"in_memory": True})
            ws = wb.add_worksheet("Validation")
            ws.write(0, 0, "Aucun probleme detecte")
            wb.close()

    write_workbook(cfg.minio, out_object, _write)
    logger.info("Rapport de validation exporte vers {}", out_object)
    return out_object
