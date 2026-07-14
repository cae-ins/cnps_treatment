"""
Audit qualite des donnees (a la demande, hors sequence du pipeline).

Reprend les 8 controles qualite issus de CNPS_TREATMENT_PROJECT
(01_inconsistency_check.R), adaptes au pipeline V2 (Polars + Parquet).

Controles
---------
1. Doublons_lignes     — Detection de lignes dupliquees par fichier
2. Colonnes            — Coherence des colonnes entre fichiers (vs reference)
3. Types_variables     — Incoherences de type entre fichiers
4. Valeurs_manquantes  — Taux de valeurs manquantes par variable et fichier
5. Outliers_Salaire    — Detection de valeurs extremes (methode IQR) sur SALAIRE_BRUT
6. Unicite_ID          — Unicite de ID_INDIV par fichier
7. Top_doublons_ID     — Top 5% des individus les plus dupliques par mois
8. Transitions_ID      — Matrice de transition : proportion d'ID presents
                          d'un mois a l'autre (ligne = origine, colonne = destination)

Sortie : un classeur Excel avec une feuille par controle.
"""

from __future__ import annotations

import io
import re
from datetime import datetime

import polars as pl
from loguru import logger

from cnps.config import PipelineConfig
from cnps.storage import list_objects, read_parquet, write_workbook

_FILENAME_RE = re.compile(r"^(\d{2})_(\d{4})\.parquet$")

_HEADER_COLOR = "#2C3E50"
_HEADER_FONT = "#FFFFFF"
_ALT_ROW_COLOR = "#F2F3F4"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _parse_period(filename: str) -> tuple[int, int]:
    """Extrait (MOIS, ANNEE) d'un nom de fichier comme '01_2024.parquet'."""
    m = _FILENAME_RE.match(filename)
    if m:
        return int(m.group(1)), int(m.group(2))
    return 0, 0


def _load_files(minio_cfg, prefix: str) -> list[tuple[str, int, int, pl.DataFrame]]:
    """Charge tous les objets Parquet sous un prefixe MinIO, tries par (ANNEE, MOIS)."""
    objects = sorted(
        obj for obj in list_objects(minio_cfg, prefix, recursive=False)
        if obj.endswith(".parquet")
    )
    result = []
    for object_name in objects:
        filename = object_name.rsplit("/", 1)[-1]
        mois, annee = _parse_period(filename)
        df = read_parquet(minio_cfg, object_name)
        result.append((filename, mois, annee, df))
    result.sort(key=lambda x: (x[2], x[1]))
    return result


# ---------------------------------------------------------------------------
# Controles individuels
# ---------------------------------------------------------------------------

def _check_doublons(data: list[tuple[str, int, int, pl.DataFrame]]) -> pl.DataFrame:
    """1. Lignes dupliquees par fichier."""
    rows = []
    for fname, mois, annee, df in data:
        n_tot = df.height
        n_unique = df.unique().height
        n_dup = n_tot - n_unique
        rows.append({
            "fichier": fname,
            "ANNEE": annee,
            "MOIS": mois,
            "total_obs": n_tot,
            "nb_lignes_dupliquees": n_dup,
            "pct_lignes_dupliquees": round(n_dup / n_tot * 100, 2) if n_tot > 0 else 0.0,
        })
    return pl.DataFrame(rows)


def _check_colonnes(data: list[tuple[str, int, int, pl.DataFrame]]) -> pl.DataFrame:
    """2. Coherence des colonnes — chaque fichier compare au premier (reference)."""
    if not data:
        return pl.DataFrame()
    ref_cols = set(data[0][3].columns)
    rows = []
    for fname, mois, annee, df in data:
        file_cols = set(df.columns)
        rows.append({
            "fichier": fname,
            "ANNEE": annee,
            "MOIS": mois,
            "colonnes_manquantes": ", ".join(sorted(ref_cols - file_cols)) or "",
            "colonnes_en_plus": ", ".join(sorted(file_cols - ref_cols)) or "",
        })
    return pl.DataFrame(rows)


def _check_types(data: list[tuple[str, int, int, pl.DataFrame]]) -> pl.DataFrame:
    """3. Incoherences de type entre fichiers (vs reference)."""
    if not data:
        return pl.DataFrame()
    ref_types = {col: str(dtype) for col, dtype in zip(data[0][3].columns, data[0][3].dtypes)}
    rows = []
    for fname, mois, annee, df in data:
        file_types = {col: str(dtype) for col, dtype in zip(df.columns, df.dtypes)}
        common = set(ref_types) & set(file_types)
        for var in sorted(common):
            if file_types[var] != ref_types[var]:
                rows.append({
                    "fichier": fname,
                    "ANNEE": annee,
                    "MOIS": mois,
                    "variable": var,
                    "type_fichier": file_types[var],
                    "type_reference": ref_types[var],
                })
    if not rows:
        return pl.DataFrame(schema={
            "fichier": pl.Utf8, "ANNEE": pl.Int64, "MOIS": pl.Int64,
            "variable": pl.Utf8, "type_fichier": pl.Utf8, "type_reference": pl.Utf8,
        })
    return pl.DataFrame(rows)


def _check_valeurs_manquantes(data: list[tuple[str, int, int, pl.DataFrame]]) -> pl.DataFrame:
    """4. Compte des valeurs manquantes par variable et par fichier."""
    rows = []
    for fname, mois, annee, df in data:
        n_tot = df.height
        for col in df.columns:
            nb_na = df[col].null_count()
            rows.append({
                "fichier": fname,
                "ANNEE": annee,
                "MOIS": mois,
                "variable": col,
                "nb_na": nb_na,
                "total_obs": n_tot,
                "pct_na": round(nb_na / n_tot * 100, 2) if n_tot > 0 else 0.0,
            })
    return pl.DataFrame(rows)


def _check_outliers(data: list[tuple[str, int, int, pl.DataFrame]],
                    variable: str = "SALAIRE_BRUT",
                    iqr_multiplier: float = 1.5) -> pl.DataFrame:
    """5. Detection de valeurs extremes (methode IQR) sur une variable de salaire."""
    rows = []
    for fname, mois, annee, df in data:
        if variable not in df.columns:
            continue
        x = df[variable].drop_nulls()
        if x.len() == 0:
            continue
        q1 = x.quantile(0.25)
        q3 = x.quantile(0.75)
        iqr = q3 - q1
        lo = q1 - iqr_multiplier * iqr
        hi = q3 + iqr_multiplier * iqr
        nb_outliers = x.filter((x < lo) | (x > hi)).len()
        rows.append({
            "fichier": fname,
            "ANNEE": annee,
            "MOIS": mois,
            "variable": variable,
            "Q1": round(q1, 2),
            "Q3": round(q3, 2),
            "IQR": round(iqr, 2),
            "borne_basse": round(lo, 2),
            "borne_haute": round(hi, 2),
            "nb_outliers": nb_outliers,
            "pct_outliers": round(nb_outliers / x.len() * 100, 2),
        })
    if not rows:
        return pl.DataFrame(schema={
            "fichier": pl.Utf8, "ANNEE": pl.Int64, "MOIS": pl.Int64,
            "variable": pl.Utf8, "Q1": pl.Float64, "Q3": pl.Float64,
            "IQR": pl.Float64, "borne_basse": pl.Float64, "borne_haute": pl.Float64,
            "nb_outliers": pl.Int64, "pct_outliers": pl.Float64,
        })
    return pl.DataFrame(rows)


def _check_unicite_id(data: list[tuple[str, int, int, pl.DataFrame]],
                      id_var: str = "ID_INDIV") -> pl.DataFrame:
    """6. Unicite de l'identifiant par fichier."""
    rows = []
    for fname, mois, annee, df in data:
        if id_var not in df.columns:
            rows.append({
                "fichier": fname,
                "ANNEE": annee,
                "MOIS": mois,
                "erreur": f"{id_var} absent",
                "total_obs": df.height,
                "nb_unique": None,
                "nb_doublons": None,
            })
            continue
        n_tot = df.height
        n_unique = df[id_var].n_unique()
        rows.append({
            "fichier": fname,
            "ANNEE": annee,
            "MOIS": mois,
            "erreur": "",
            "total_obs": n_tot,
            "nb_unique": n_unique,
            "nb_doublons": n_tot - n_unique,
        })
    return pl.DataFrame(rows)


def _check_top_doublons_id(data: list[tuple[str, int, int, pl.DataFrame]],
                           id_var: str = "ID_INDIV",
                           top_pct: float = 0.05) -> pl.DataFrame:
    """7. Individus les plus dupliques par mois (top 5% par nombre d'occurrences)."""
    frames = []
    for fname, mois, annee, df in data:
        if id_var not in df.columns:
            continue

        counts = (
            df.group_by(id_var)
            .agg(pl.len().alias("nb_occurrences"))
            .filter(pl.col("nb_occurrences") > 1)
            .sort("nb_occurrences", descending=True)
        )

        if counts.height == 0:
            continue

        n_keep = max(1, int(counts.height * top_pct))
        top = counts.head(n_keep)

        top = top.with_columns(
            pl.lit(fname).alias("fichier"),
            pl.lit(annee).alias("ANNEE"),
            pl.lit(mois).alias("MOIS"),
        )
        frames.append(top)

    if not frames:
        return pl.DataFrame(schema={
            "fichier": pl.Utf8, "ANNEE": pl.Int64, "MOIS": pl.Int64,
            id_var: pl.Utf8, "nb_occurrences": pl.UInt32,
        })

    return pl.concat(frames).select("fichier", "ANNEE", "MOIS", id_var, "nb_occurrences")


def _check_transitions(data: list[tuple[str, int, int, pl.DataFrame]],
                       id_var: str = "ID_INDIV") -> pl.DataFrame:
    """
    8. Matrice de transition — proportion des ID du mois i presents au mois j.

    Matrice N×N ou lignes = periode d'origine, colonnes = periode de destination.
    Cellule (i, j) = |ID_i ∩ ID_j| / |ID_i| (proportion des ID de i retrouves en j).
    """
    periods: list[str] = []
    id_sets: list[set] = []

    for fname, mois, annee, df in data:
        label = f"{mois:02d}_{annee}"
        periods.append(label)
        if id_var not in df.columns:
            id_sets.append(set())
            continue
        id_sets.append(set(df[id_var].drop_nulls().to_list()))

    n = len(periods)
    rows = []
    for i in range(n):
        row: dict[str, object] = {"Periode": periods[i], "Nb_ID": len(id_sets[i])}
        n_i = len(id_sets[i])
        for j in range(n):
            if n_i == 0:
                row[periods[j]] = 0.0
            else:
                overlap = len(id_sets[i] & id_sets[j])
                row[periods[j]] = round(overlap / n_i * 100, 2)
        rows.append(row)

    return pl.DataFrame(rows)


# ---------------------------------------------------------------------------
# Export Excel
# ---------------------------------------------------------------------------

def _write_standard_sheet(
    wb, sheet_name: str, df: pl.DataFrame,
    header_fmt, number_fmt, decimal_fmt, text_fmt,
    alt_number_fmt, alt_decimal_fmt, alt_text_fmt,
) -> None:
    """Ecrit une feuille d'audit standard."""
    ws = wb.add_worksheet(sheet_name)

    if df.height == 0 and len(df.columns) == 0:
        ws.write(0, 0, "Aucun probleme detecte", text_fmt)
        return

    cols = df.columns

    for ci, col_name in enumerate(cols):
        ws.write(0, ci, col_name, header_fmt)

    for ri in range(df.height):
        is_alt = ri % 2 == 1
        for ci, col_name in enumerate(cols):
            value = df[col_name][ri]

            if value is None:
                ws.write(ri + 1, ci, "", text_fmt)
            elif isinstance(value, float):
                fmt = alt_decimal_fmt if is_alt else decimal_fmt
                ws.write_number(ri + 1, ci, value, fmt)
            elif isinstance(value, int):
                fmt = alt_number_fmt if is_alt else number_fmt
                ws.write_number(ri + 1, ci, value, fmt)
            else:
                fmt = alt_text_fmt if is_alt else text_fmt
                ws.write(ri + 1, ci, str(value), fmt)

    for ci, col_name in enumerate(cols):
        max_len = len(col_name)
        for ri in range(min(df.height, 200)):
            val = df[col_name][ri]
            max_len = max(max_len, len(str(val)) if val is not None else 0)
        ws.set_column(ci, ci, min(max_len + 4, 40))

    ws.freeze_panes(1, 0)
    if cols and df.height > 0:
        ws.autofilter(0, 0, df.height, len(cols) - 1)


def _write_transition_sheet(
    wb, df: pl.DataFrame, header_fmt, number_fmt, decimal_fmt, text_fmt,
) -> None:
    """Ecrit la feuille de matrice de transition avec degrade de couleur."""
    ws = wb.add_worksheet("Transitions_ID")

    if df.height == 0:
        ws.write(0, 0, "Aucune donnee de transition", text_fmt)
        return

    cols = df.columns  # ["Periode", "Nb_ID", "01_2024", "02_2024", ...]

    for ci, col_name in enumerate(cols):
        ws.write(0, ci, col_name, header_fmt)

    # Degrade rouge (faible retention) -> vert (forte retention)
    pct_fmt_low = wb.add_format({
        "num_format": "0.00", "border": 1, "bg_color": "#F1948A", "align": "center",
    })
    pct_fmt_mid_low = wb.add_format({
        "num_format": "0.00", "border": 1, "bg_color": "#F9E79F", "align": "center",
    })
    pct_fmt_mid = wb.add_format({
        "num_format": "0.00", "border": 1, "bg_color": "#ABEBC6", "align": "center",
    })
    pct_fmt_high = wb.add_format({
        "num_format": "0.00", "border": 1, "bg_color": "#82E0AA", "align": "center",
    })
    pct_fmt_diag = wb.add_format({
        "num_format": "0.00", "border": 1, "bg_color": "#5DADE2",
        "font_color": "#FFFFFF", "bold": True, "align": "center",
    })

    period_cols_start = 2  # colonnes 0=Periode, 1=Nb_ID, puis les periodes

    for ri in range(df.height):
        for ci, col_name in enumerate(cols):
            value = df[col_name][ri]

            if ci == 0:
                ws.write(ri + 1, ci, str(value), text_fmt)
            elif ci == 1:
                ws.write_number(ri + 1, ci, int(value) if value is not None else 0, number_fmt)
            else:
                v = float(value) if value is not None else 0.0
                if ci - period_cols_start == ri:
                    fmt = pct_fmt_diag
                elif v >= 75:
                    fmt = pct_fmt_high
                elif v >= 50:
                    fmt = pct_fmt_mid
                elif v >= 25:
                    fmt = pct_fmt_mid_low
                else:
                    fmt = pct_fmt_low
                ws.write_number(ri + 1, ci, v, fmt)

    ws.set_column(0, 0, 14)
    ws.set_column(1, 1, 12)
    ws.set_column(2, len(cols) - 1, 10)
    ws.freeze_panes(1, 2)


def _export_audit_excel(
    cfg: PipelineConfig,
    output_object: str,
    *,
    doublons: pl.DataFrame,
    colonnes: pl.DataFrame,
    types: pl.DataFrame,
    valeurs_manquantes: pl.DataFrame,
    outliers: pl.DataFrame,
    unicite_id: pl.DataFrame,
    top_doublons_id: pl.DataFrame,
    transitions: pl.DataFrame,
) -> None:
    """Ecrit tous les DataFrames d'audit dans un classeur Excel sur MinIO."""
    def _write(buf: io.BytesIO) -> None:
        import xlsxwriter

        wb = xlsxwriter.Workbook(buf, {"in_memory": True})

        header_fmt = wb.add_format({
            "bold": True, "font_color": _HEADER_FONT, "bg_color": _HEADER_COLOR,
            "border": 1, "text_wrap": True, "valign": "vcenter", "align": "center",
        })
        number_fmt = wb.add_format({"num_format": "#,##0", "border": 1})
        decimal_fmt = wb.add_format({"num_format": "#,##0.00", "border": 1})
        text_fmt = wb.add_format({"border": 1, "text_wrap": True})
        alt_number_fmt = wb.add_format({
            "num_format": "#,##0", "bg_color": _ALT_ROW_COLOR, "border": 1,
        })
        alt_decimal_fmt = wb.add_format({
            "num_format": "#,##0.00", "bg_color": _ALT_ROW_COLOR, "border": 1,
        })
        alt_text_fmt = wb.add_format({
            "bg_color": _ALT_ROW_COLOR, "border": 1, "text_wrap": True,
        })

        sheets = [
            ("Doublons_lignes", doublons),
            ("Colonnes", colonnes),
            ("Types_variables", types),
            ("Valeurs_manquantes", valeurs_manquantes),
            ("Outliers_Salaire", outliers),
            ("Unicite_ID", unicite_id),
            ("Top_doublons_ID", top_doublons_id),
        ]

        for sheet_name, df in sheets:
            _write_standard_sheet(wb, sheet_name, df,
                                  header_fmt, number_fmt, decimal_fmt, text_fmt,
                                  alt_number_fmt, alt_decimal_fmt, alt_text_fmt)

        _write_transition_sheet(wb, transitions, header_fmt, number_fmt, decimal_fmt, text_fmt)

        wb.close()

    write_workbook(cfg.minio, output_object, _write)


# ---------------------------------------------------------------------------
# API publique
# ---------------------------------------------------------------------------

def executer_audit(
    cfg: PipelineConfig,
    *,
    input_prefix: str | None = None,
    output_prefix: str | None = None,
    salary_var: str = "SALAIRE_BRUT",
    id_var: str = "ID_INDIV",
    iqr_multiplier: float = 1.5,
) -> str:
    """
    Execute l'audit qualite complet et exporte les resultats en Excel sur MinIO.

    Parameters
    ----------
    cfg : PipelineConfig
        Configuration du pipeline.
    input_prefix : str, optional
        Prefixe MinIO contenant les objets Parquet a auditer.
        Par defaut : ``cfg.minio.processed_prefix``.
    output_prefix : str, optional
        Prefixe MinIO pour le fichier Excel de sortie.
        Par defaut : ``cfg.minio.output_prefix``.
    salary_var : str
        Colonne utilisee pour la detection de valeurs extremes.
    id_var : str
        Colonne utilisee pour le controle d'unicite.
    iqr_multiplier : float
        Multiplicateur IQR pour les bornes de valeurs extremes (1.5 par defaut).

    Returns
    -------
    str
        Nom de l'objet Excel d'audit genere sur MinIO.
    """
    if input_prefix is None:
        input_prefix = cfg.minio.processed_prefix
    if output_prefix is None:
        output_prefix = cfg.minio.output_prefix

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    output_object = f"{output_prefix}audit_fichiers_cnps_{timestamp}.xlsx"

    logger.info("=" * 60)
    logger.info("AUDIT QUALITE DES DONNEES")
    logger.info("=" * 60)
    logger.info("Prefixe source : {}", input_prefix)

    data = _load_files(cfg.minio, input_prefix)
    if not data:
        logger.warning("Aucun fichier parquet trouve sous : {}", input_prefix)
        return output_object

    logger.info("Fichiers a auditer : {}", len(data))

    logger.info("1/8 - Verification des doublons...")
    df_doublons = _check_doublons(data)

    logger.info("2/8 - Verification des colonnes...")
    df_colonnes = _check_colonnes(data)

    logger.info("3/8 - Verification des types...")
    df_types = _check_types(data)

    logger.info("4/8 - Verification des valeurs manquantes...")
    df_missing = _check_valeurs_manquantes(data)

    logger.info("5/8 - Detection des outliers ({})...", salary_var)
    df_outliers = _check_outliers(data, variable=salary_var, iqr_multiplier=iqr_multiplier)

    logger.info("6/8 - Verification de l'unicite des ID ({})...", id_var)
    df_unicite = _check_unicite_id(data, id_var=id_var)

    logger.info("7/8 - Top 5% des ID les plus dupliques ({})...", id_var)
    df_top_dup = _check_top_doublons_id(data, id_var=id_var)

    logger.info("8/8 - Matrice de transitions des ID ({})...", id_var)
    df_transitions = _check_transitions(data, id_var=id_var)

    logger.info("Export Excel...")
    _export_audit_excel(
        cfg, output_object,
        doublons=df_doublons, colonnes=df_colonnes, types=df_types,
        valeurs_manquantes=df_missing, outliers=df_outliers,
        unicite_id=df_unicite, top_doublons_id=df_top_dup, transitions=df_transitions,
    )

    logger.info("Fichier d'audit genere : {}", output_object)
    return output_object
