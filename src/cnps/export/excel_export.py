"""
Excel export module.

Generates formatted Excel workbooks with one sheet per analytical dimension.
Applies professional styling: headers, number formatting, frozen panes,
auto-width columns, and conditional formatting.

Uses XlsxWriter for performance (faster than openpyxl for write-only).
"""

from __future__ import annotations

import io

import polars as pl
from loguru import logger

from cnps.config import PipelineConfig
from cnps.diagnostics.validation import ValidationReport
from cnps.storage import write_workbook


# ---------------------------------------------------------------------------
# Formatting constants
# ---------------------------------------------------------------------------

_HEADER_COLOR = "#2C3E50"
_HEADER_FONT = "#FFFFFF"
_ALT_ROW_COLOR = "#F2F3F4"
_NUMBER_FMT = "#,##0"
_DECIMAL_FMT = "#,##0.00"
_PCT_FMT = "0.00%"


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def export_indicators(
    cfg: PipelineConfig,
    results: pl.DataFrame,
    filename: str = "indicateurs_cnps.xlsx",
) -> Path:
    """
    Export estimation results to a formatted Excel workbook.

    Creates one sheet per dimension with styled headers, number formatting,
    and auto-fit column widths.

    Parameters
    ----------
    cfg : PipelineConfig
        Pipeline configuration.
    results : pl.DataFrame
        Estimation results (from ``estimator.estimate_all``).
    filename : str
        Output filename.

    Returns
    -------
    str
        Object name of the generated Excel file on MinIO.
    """
    out_object = f"{cfg.minio.output_prefix}{filename}"

    # Group by dimension
    if "dimension" not in results.columns:
        results = results.with_columns(pl.lit("Resultats").alias("dimension"))

    dimensions = results["dimension"].unique().sort().to_list()

    def _write(buf: io.BytesIO) -> None:
        import xlsxwriter

        wb = xlsxwriter.Workbook(buf, {"in_memory": True})

        # Formats
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
        decimal_fmt = wb.add_format({"num_format": _DECIMAL_FMT, "border": 1})
        text_fmt = wb.add_format({"border": 1, "text_wrap": True})
        alt_fmt = wb.add_format({"bg_color": _ALT_ROW_COLOR, "border": 1})
        alt_number_fmt = wb.add_format({
            "num_format": _NUMBER_FMT, "bg_color": _ALT_ROW_COLOR, "border": 1,
        })

        # Label mapping for headers
        stat_labels = {s.name: s.label for s in cfg.statistics}

        for dim_label in dimensions:
            # Sanitize sheet name (max 31 chars, no special chars)
            sheet_name = dim_label[:31].replace("/", "-").replace("\\", "-")
            ws = wb.add_worksheet(sheet_name)

            dim_df = results.filter(pl.col("dimension") == dim_label)

            # Determine columns to write
            cols = [c for c in dim_df.columns if c != "dimension"]

            # Write headers
            for col_idx, col_name in enumerate(cols):
                label = stat_labels.get(col_name, col_name.replace("_", " ").title())
                ws.write(0, col_idx, label, header_fmt)

            # Write data
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

            # Auto-fit column widths (approximate)
            for col_idx, col_name in enumerate(cols):
                max_len = max(
                    len(stat_labels.get(col_name, col_name)),
                    max((len(str(dim_df[col_name][i] or ""))
                         for i in range(min(dim_df.height, 100))),
                        default=10),
                )
                ws.set_column(col_idx, col_idx, min(max_len + 4, 30))

            # Freeze top row
            ws.freeze_panes(1, 0)

            # Auto-filter
            if cols:
                ws.autofilter(0, 0, dim_df.height, len(cols) - 1)

        wb.close()

    write_workbook(cfg.minio, out_object, _write)
    logger.info("Indicators exported to {}", out_object)
    return out_object


def export_validation_report(
    cfg: PipelineConfig,
    report: ValidationReport,
    filename: str = "rapport_validation.xlsx",
) -> str:
    """Export validation report to Excel on MinIO."""
    out_object = f"{cfg.minio.output_prefix}{filename}"

    rows = []
    for issue in report.issues:
        rows.append({
            "Niveau": issue.level,
            "Etape": issue.stage,
            "Verification": issue.check,
            "Message": issue.message,
        })

    def _write(buf: io.BytesIO) -> None:
        if rows:
            df = pl.DataFrame(rows)
            df.write_excel(buf)
        else:
            # Write empty workbook
            import xlsxwriter
            wb = xlsxwriter.Workbook(buf, {"in_memory": True})
            ws = wb.add_worksheet("Validation")
            ws.write(0, 0, "Aucun probleme detecte")
            wb.close()

    write_workbook(cfg.minio, out_object, _write)
    logger.info("Validation report exported to {}", out_object)
    return out_object
