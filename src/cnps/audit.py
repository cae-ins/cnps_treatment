"""
Audit qualite par etape (a la demande, hors sequence du pipeline).

Reprend les controles qualite issus de CNPS_TREATMENT_PROJECT
(01_inconsistency_check.R), adaptes au pipeline V2 (Polars + Parquet), et
les dispatche selon l'etape numerotee du pipeline dont on veut auditer la
sortie. A ce jour, seule l'etape 01 (01_lecture_fichiers.py) est
implementee : elle audite silver/cnps/, c'est-a-dire les fichiers Parquet
tels que convertis par l'etape 01, AVANT l'harmonisation de type de
l'etape 02 — le point le plus proche des fichiers CNPS originaux tout en
restant exploitable en Parquet.

Controles
---------
1. Doublons_lignes     — Detection de lignes dupliquees par fichier
2. Colonnes            — Coherence des colonnes entre fichiers (vs reference)
3. Types_variables     — Incoherences de type entre fichiers
4. Valeurs_manquantes  — Taux de valeurs manquantes par variable et fichier
5. Outliers_Salaire    — Detection de valeurs extremes (methode IQR) sur SALAIRE_BRUT,
                          calculee globalement et separement par periodicite
                          declaree (TYPE_SALARIE), pour ne pas melanger des
                          echelles de salaire incomparables (mensuel/journalier/horaire)
6. Unicite_ID          — Unicite de ID_INDIV par fichier
7. Top_doublons_ID     — Top 5% des individus les plus dupliques par mois
8. Transitions_ID      — Matrice de transition : proportion d'ID presents
                          d'un mois a l'autre (ligne = origine, colonne = destination)
9. Distribution        — Min/max/moyenne/mediane/ecart-type/quantiles de
                          chaque variable numerique, par fichier
10. Manquants_vs_Salaire — Taux de valeurs manquantes de chaque variable,
                          compare entre les lignes ou SALAIRE_BRUT est
                          renseigne et celles ou il est manquant
11. Analyse_Salaire    — Analyse ciblee de SALAIRE_BRUT croisee avec sa
                          periodicite declaree (TYPE_SALARIE) : seuils
                          plausibles derives du SMIG, salaires nuls/negatifs/
                          sous seuil, confusions d'unite suspectees (ex. un
                          taux journalier declare comme salaire mensuel)

Sortie : un classeur Excel avec une feuille par controle, precede d'une
feuille "Guide_Lecture" qui explique, pour chaque feuille du classeur,
son objectif et comment l'interpreter.

Pour une exploration plus approfondie (graphiques de distribution, croisements
avec le profil des employeurs/salaries, concentration des anomalies par
entreprise ou individu), voir le notebook ``analyse_incoherences_salaires.ipynb``
a la racine du depot, qui complete cet audit automatise plutot que de le
dupliquer.

Comme jointure_anstat.py, ce script est explicitement hors du DAG numerote
01-12 (voir orchestrator.py::discover_stages) : il ne sera jamais execute
automatiquement par l'orchestrateur, uniquement a la demande.

Usage
-----
    python src/cnps/audit.py --stage 01
    python src/cnps/audit.py --stage 01 --verbose
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

_ETAPES_DISPONIBLES = ("01",)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _parse_period(filename: str) -> tuple[int, int]:
    """Extrait (MOIS, ANNEE) d'un nom de fichier comme '01_2024.parquet'."""
    m = _FILENAME_RE.match(filename)
    if m:
        return int(m.group(1)), int(m.group(2))
    return 0, 0


def _load_files(minio_cfg, bucket: str, prefix: str) -> list[tuple[str, int, int, pl.DataFrame]]:
    """Charge tous les objets Parquet sous un bucket/prefixe MinIO, tries par (ANNEE, MOIS)."""
    objects = sorted(
        obj for obj in list_objects(minio_cfg, bucket, prefix, recursive=False)
        if obj.endswith(".parquet")
    )
    result = []
    for object_name in objects:
        filename = object_name.rsplit("/", 1)[-1]
        mois, annee = _parse_period(filename)
        df = read_parquet(minio_cfg, bucket, object_name)
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


def _check_manquants_vs_salaire(
    data: list[tuple[str, int, int, pl.DataFrame]],
    salary_var: str = "SALAIRE_BRUT",
) -> pl.DataFrame:
    """
    10. Compare, pour chaque variable, le taux de valeurs manquantes selon
    que salary_var est lui-meme renseigne ou manquant sur la meme ligne.

    Revele si l'absence de salaire est correlee a des lignes mal remplies
    en general (autres champs egalement manquants), ou si c'est un manque
    isole propre a salary_var.
    """
    rows = []
    for fname, mois, annee, df in data:
        if salary_var not in df.columns:
            continue

        avec_salaire = df.filter(pl.col(salary_var).is_not_null())
        sans_salaire = df.filter(pl.col(salary_var).is_null())
        n_avec = avec_salaire.height
        n_sans = sans_salaire.height

        for col in df.columns:
            if col == salary_var:
                continue
            pct_na_avec = round(avec_salaire[col].null_count() / n_avec * 100, 2) if n_avec > 0 else None
            pct_na_sans = round(sans_salaire[col].null_count() / n_sans * 100, 2) if n_sans > 0 else None
            ecart = (
                round(pct_na_sans - pct_na_avec, 2)
                if pct_na_avec is not None and pct_na_sans is not None
                else None
            )
            rows.append({
                "fichier": fname,
                "ANNEE": annee,
                "MOIS": mois,
                "variable": col,
                f"n_{salary_var}_renseigne": n_avec,
                f"n_{salary_var}_manquant": n_sans,
                "pct_na_si_salaire_renseigne": pct_na_avec,
                "pct_na_si_salaire_manquant": pct_na_sans,
                "ecart_pts": ecart,
            })

    if not rows:
        return pl.DataFrame(schema={
            "fichier": pl.Utf8, "ANNEE": pl.Int64, "MOIS": pl.Int64, "variable": pl.Utf8,
            f"n_{salary_var}_renseigne": pl.Int64, f"n_{salary_var}_manquant": pl.Int64,
            "pct_na_si_salaire_renseigne": pl.Float64, "pct_na_si_salaire_manquant": pl.Float64,
            "ecart_pts": pl.Float64,
        })
    return pl.DataFrame(rows)


# Duree standard utilisee pour deriver un seuil plausible journalier/horaire
# a partir du SMIG mensuel (cfg.cleaning.min_salary) : 26 jours ouvres/mois,
# 8h/jour (soit 208h/mois). Convention documentaire, ajustable si besoin.
_JOURS_OUVRES_PAR_MOIS = 26
_HEURES_PAR_MOIS = 208

_LABEL_PERIODICITE = {"M": "Mensuel", "J": "Journalier", "H": "Horaire"}


def _check_analyse_salaire(
    data: list[tuple[str, int, int, pl.DataFrame]],
    salary_var: str = "SALAIRE_BRUT",
    type_var: str = "TYPE_SALARIE",
    duree_var: str = "DUREE_TRAVAILLEE",
    smig_mensuel: float = 75_000.0,
) -> pl.DataFrame:
    """
    11. Analyse ciblee de SALAIRE_BRUT croisee avec sa periodicite declaree
    (``TYPE_SALARIE`` : M/J/H).

    Pour chaque fichier et chaque periodicite (plus une ligne "TOUTES"),
    calcule :
    - le nombre de declarations et le taux de valeurs manquantes ;
    - le seuil plausible derive du SMIG mensuel (identique, /26 ou /208
      selon la periodicite) ;
    - le nombre de salaires nuls, negatifs, ou sous ce seuil ;
    - le nombre de salaires "suspects" d'une confusion d'unite : un salaire
      mensuel dont le montant ressemble a un taux journalier/horaire, ou
      inversement un salaire journalier/horaire dont le montant ressemble a
      un salaire mensuel complet.

    Reprend la logique du notebook ``analyse_incoherences_salaires.ipynb``
    (section 4-5), condensee au niveau fichier pour rester exploitable dans
    un classeur d'audit plutot qu'en exploration ligne a ligne.
    """
    seuil_journalier = smig_mensuel / _JOURS_OUVRES_PAR_MOIS
    seuil_horaire = smig_mensuel / _HEURES_PAR_MOIS
    seuil_par_type = {"M": smig_mensuel, "J": seuil_journalier, "H": seuil_horaire}

    rows = []
    for fname, mois, annee, df in data:
        if salary_var not in df.columns:
            continue

        has_type = type_var in df.columns
        type_values = (
            df[type_var].unique().drop_nulls().sort().to_list() if has_type else []
        )
        groupes: list[tuple[str, pl.DataFrame]] = [("TOUTES", df)]
        if has_type:
            groupes.extend(
                (str(tv), df.filter(pl.col(type_var) == tv)) for tv in type_values
            )

        for label, sous_df in groupes:
            n_tot = sous_df.height
            if n_tot == 0:
                continue

            sal = sous_df[salary_var]
            n_manquant = sal.null_count()
            sal_non_null = sal.drop_nulls()
            n_negatif = sal_non_null.filter(sal_non_null < 0).len()
            n_nul = sal_non_null.filter(sal_non_null == 0).len()

            seuil = seuil_par_type.get(label)
            if seuil is not None:
                n_sous_seuil = sal_non_null.filter(
                    (sal_non_null > 0) & (sal_non_null < seuil)
                ).len()
            else:
                n_sous_seuil = None

            n_confusion_unite = None
            if label == "M":
                n_confusion_unite = sal_non_null.filter(
                    (sal_non_null > 0) & (sal_non_null < seuil_journalier * 3)
                ).len()
            elif label in ("J", "H"):
                n_confusion_unite = sal_non_null.filter(sal_non_null >= smig_mensuel).len()

            duree_incoherente = None
            if duree_var in sous_df.columns:
                duree = sous_df[duree_var]
                duree_incoherente = sous_df.filter(
                    (pl.col(duree_var) > 31)
                    | ((pl.col(duree_var) == 0) & pl.col(salary_var).is_not_null() & (pl.col(salary_var) > 0))
                ).height if duree.null_count() < n_tot else None

            rows.append({
                "fichier": fname,
                "ANNEE": annee,
                "MOIS": mois,
                "periodicite": _LABEL_PERIODICITE.get(label, label),
                "total_obs": n_tot,
                "seuil_plausible": round(seuil, 2) if seuil is not None else None,
                "nb_manquant": n_manquant,
                "pct_manquant": round(n_manquant / n_tot * 100, 2),
                "nb_negatif": n_negatif,
                "nb_nul_zero": n_nul,
                "nb_sous_seuil_plausible": n_sous_seuil,
                "pct_sous_seuil_plausible": (
                    round(n_sous_seuil / n_tot * 100, 2) if n_sous_seuil is not None else None
                ),
                "nb_confusion_unite_suspectee": n_confusion_unite,
                "pct_confusion_unite_suspectee": (
                    round(n_confusion_unite / n_tot * 100, 2) if n_confusion_unite is not None else None
                ),
                "nb_duree_travaillee_incoherente": duree_incoherente,
            })

    if not rows:
        return pl.DataFrame(schema={
            "fichier": pl.Utf8, "ANNEE": pl.Int64, "MOIS": pl.Int64, "periodicite": pl.Utf8,
            "total_obs": pl.Int64, "seuil_plausible": pl.Float64,
            "nb_manquant": pl.Int64, "pct_manquant": pl.Float64,
            "nb_negatif": pl.Int64, "nb_nul_zero": pl.Int64,
            "nb_sous_seuil_plausible": pl.Int64, "pct_sous_seuil_plausible": pl.Float64,
            "nb_confusion_unite_suspectee": pl.Int64, "pct_confusion_unite_suspectee": pl.Float64,
            "nb_duree_travaillee_incoherente": pl.Int64,
        })
    return pl.DataFrame(rows)


def _check_outliers(data: list[tuple[str, int, int, pl.DataFrame]],
                    variable: str = "SALAIRE_BRUT",
                    iqr_multiplier: float = 1.5,
                    type_var: str = "TYPE_SALARIE") -> pl.DataFrame:
    """
    5. Detection de valeurs extremes (methode IQR) sur une variable de salaire.

    Calcule les bornes IQR globalement (toutes periodicites confondues, ligne
    "TOUS") et separement pour chaque valeur de ``type_var`` presente (ex.
    "M"/"J"/"H") si la colonne existe : un salaire journalier melange a des
    salaires mensuels fausserait sinon completement les quartiles.
    """
    rows = []
    for fname, mois, annee, df in data:
        if variable not in df.columns:
            continue

        groupes: list[tuple[str, pl.Series]] = []
        x_tous = df[variable].drop_nulls()
        if x_tous.len() > 0:
            groupes.append(("TOUS", x_tous))

        if type_var in df.columns:
            for type_value in df[type_var].unique().drop_nulls().sort().to_list():
                x_type = df.filter(pl.col(type_var) == type_value)[variable].drop_nulls()
                if x_type.len() > 0:
                    groupes.append((str(type_value), x_type))

        for label, x in groupes:
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
                "periodicite": label,
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
            "variable": pl.Utf8, "periodicite": pl.Utf8, "Q1": pl.Float64, "Q3": pl.Float64,
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


_NUMERIC_DTYPES = (
    pl.Float32, pl.Float64,
    pl.Int8, pl.Int16, pl.Int32, pl.Int64,
    pl.UInt8, pl.UInt16, pl.UInt32, pl.UInt64,
)

# Part minimale de valeurs non-nulles qui doivent se convertir en nombre
# pour qu'une colonne Utf8/String soit consideree comme numerique (au lieu
# d'un identifiant alphanumerique ou d'un texte libre).
_NUMERIC_COERCION_THRESHOLD = 0.95


def _numeric_series(df: pl.DataFrame, col: str) -> pl.Series | None:
    """
    Retourne la colonne castee en Float64 si elle est deja numerique, ou si
    elle est Utf8/String avec au moins _NUMERIC_COERCION_THRESHOLD de ses
    valeurs non-nulles convertibles en nombre (cas de silver/cnps/, ou tout
    est lu en texte brut a l'etape 01, avant la coercition de l'etape 02).
    Retourne None si la colonne n'est pas exploitable comme numerique.
    """
    dt = df.schema[col]
    if dt in _NUMERIC_DTYPES:
        return df[col]

    if dt not in (pl.Utf8, pl.String):
        return None

    raw = df[col].drop_nulls()
    if raw.len() == 0:
        return None

    # Cast strict sur la valeur telle quelle (espaces de bordure retires) :
    # pas de nettoyage prealable des caracteres non numeriques, pour ne pas
    # confondre un identifiant alphanumerique (ex. "A001" -> "001" -> 1.0)
    # avec une vraie valeur numerique.
    coerced = raw.str.strip_chars().cast(pl.Float64, strict=False)
    n_convertible = coerced.drop_nulls().len()
    if n_convertible / raw.len() < _NUMERIC_COERCION_THRESHOLD:
        return None

    return coerced


def _check_distribution(data: list[tuple[str, int, int, pl.DataFrame]]) -> pl.DataFrame:
    """9. Distribution (min/max/moyenne/mediane/ecart-type/quantiles) de
    chaque variable numerique (ou numerique-comme-texte), par fichier."""
    rows = []
    for fname, mois, annee, df in data:
        for col in df.columns:
            x = _numeric_series(df, col)
            if x is None:
                continue
            x = x.drop_nulls()
            if x.len() == 0:
                continue
            rows.append({
                "fichier": fname,
                "ANNEE": annee,
                "MOIS": mois,
                "variable": col,
                "n": x.len(),
                "min": x.min(),
                "p1": x.quantile(0.01),
                "p5": x.quantile(0.05),
                "p10": x.quantile(0.10),
                "q1": x.quantile(0.25),
                "mediane": x.quantile(0.50),
                "moyenne": round(x.mean(), 2),
                "ecart_type": round(x.std(), 2) if x.len() > 1 else 0.0,
                "q3": x.quantile(0.75),
                "p90": x.quantile(0.90),
                "p95": x.quantile(0.95),
                "p99": x.quantile(0.99),
                "max": x.max(),
            })
    if not rows:
        return pl.DataFrame(schema={
            "fichier": pl.Utf8, "ANNEE": pl.Int64, "MOIS": pl.Int64, "variable": pl.Utf8,
            "n": pl.Int64, "min": pl.Float64, "p1": pl.Float64, "p5": pl.Float64,
            "p10": pl.Float64, "q1": pl.Float64, "mediane": pl.Float64, "moyenne": pl.Float64,
            "ecart_type": pl.Float64, "q3": pl.Float64, "p90": pl.Float64, "p95": pl.Float64,
            "p99": pl.Float64, "max": pl.Float64,
        })
    return pl.DataFrame(rows)


# ---------------------------------------------------------------------------
# Guide de lecture (feuille explicative statique)
# ---------------------------------------------------------------------------

# (Feuille, Objectif, Comment l'interpreter) — un triplet par controle,
# dans l'ordre ou les feuilles apparaissent dans le classeur.
_GUIDE_LECTURE: list[tuple[str, str, str]] = [
    (
        "Doublons_lignes",
        "Detecter les lignes strictement identiques (toutes colonnes confondues) "
        "au sein d'un meme fichier mensuel.",
        "Un pourcentage eleve indique un probleme d'extraction ou de doublon "
        "d'import cote source ; ces lignes gonflent artificiellement les effectifs "
        "si elles ne sont pas dedupliquees en amont.",
    ),
    (
        "Colonnes",
        "Verifier que chaque fichier mensuel possede le meme jeu de colonnes que "
        "le premier fichier de la serie (pris comme reference).",
        "Une colonne manquante peut faire disparaitre silencieusement une "
        "information dans les etapes suivantes du pipeline ; une colonne en plus "
        "peut signaler un changement de format source a documenter.",
    ),
    (
        "Types_variables",
        "Comparer, pour chaque variable commune, le type de donnees (texte, "
        "entier, decimal...) du fichier au type du fichier de reference.",
        "Un type qui change d'un mois a l'autre (ex. colonne lue comme texte "
        "un mois et comme nombre un autre) provoque des echecs ou des "
        "conversions silencieuses lors de l'harmonisation en etape 02.",
    ),
    (
        "Valeurs_manquantes",
        "Mesurer, pour chaque variable et chaque fichier, la part de valeurs "
        "non renseignees.",
        "Un taux de valeurs manquantes anormalement eleve ou qui augmente dans "
        "le temps peut reveler un champ mal collecte ou une variable devenue "
        "obsolete cote source.",
    ),
    (
        "Outliers_Salaire",
        "Reperer les valeurs extremes de SALAIRE_BRUT via la methode des "
        "quartiles (IQR) : tout ce qui sort de [Q1 - 1.5*IQR, Q3 + 1.5*IQR]. "
        "Calcule a la fois globalement (ligne 'TOUS') et separement pour "
        "chaque periodicite declaree (M/J/H, cf. TYPE_SALARIE).",
        "Une part importante de valeurs hors bornes peut signaler des erreurs "
        "de saisie (salaires en centimes au lieu de francs, doubles zeros, "
        "etc.) ou une population reellement heterogene a examiner au cas par cas. "
        "Comparer les lignes par periodicite evite qu'un salaire journalier, "
        "mecaniquement plus petit, ne soit compte a tort comme un outlier bas "
        "dans une distribution dominee par des salaires mensuels.",
    ),
    (
        "Unicite_ID",
        "Verifier que l'identifiant individuel (ID_INDIV) n'apparait qu'une "
        "seule fois par fichier mensuel.",
        "Des doublons d'identifiant faussent tout calcul agrege par individu "
        "(masse salariale, effectifs) : c'est un signal a traiter en priorite "
        "avant toute analyse en aval.",
    ),
    (
        "Top_doublons_ID",
        "Lister, pour chaque mois, les 5% d'identifiants les plus souvent "
        "dupliques.",
        "Utile pour cibler l'investigation : plutot que de traiter tous les "
        "doublons d'un coup, cette feuille pointe les cas les plus extremes, "
        "souvent revelateurs d'un probleme structurel (ex. fusion d'etablissements).",
    ),
    (
        "Distribution",
        "Donner les statistiques descriptives (min, max, moyenne, mediane, "
        "ecart-type, quantiles) de chaque variable numerique, par fichier.",
        "Permet de suivre l'evolution des ordres de grandeur dans le temps et "
        "de reperer une rupture (changement d'unite, de barème, de population) "
        "entre deux mois consecutifs.",
    ),
    (
        "Manquants_vs_Salaire",
        "Comparer le taux de valeurs manquantes de chaque variable selon que "
        "SALAIRE_BRUT est lui-meme renseigne ou non sur la meme ligne.",
        "Un ecart important indique que l'absence de salaire n'est pas isolee : "
        "les lignes sans salaire sont aussi mal remplies sur le reste, ce qui "
        "oriente vers un probleme de saisie globale plutot qu'un champ "
        "specifique.",
    ),
    (
        "Transitions_ID",
        "Mesurer, pour chaque paire de mois, la proportion d'identifiants du "
        "mois d'origine retrouves dans le mois de destination.",
        "Une retention faible d'un mois sur l'autre peut traduire un turnover "
        "reel, mais aussi un probleme de generation d'identifiant si la chute "
        "est brutale et generalisee sur toute une periode.",
    ),
    (
        "Analyse_Salaire",
        "Croiser SALAIRE_BRUT avec sa periodicite declaree (TYPE_SALARIE : "
        "Mensuel/Journalier/Horaire) : seuil plausible derive du SMIG pour "
        "chaque periodicite, salaires nuls/negatifs/sous ce seuil, et "
        "confusions d'unite suspectees (ex. un taux journalier declare comme "
        "salaire mensuel, ou l'inverse).",
        "Une periodicite non renseignee empeche toute verification de "
        "coherence du montant. Un volume important de confusions d'unite "
        "suspectees pointe vers un probleme de saisie du formulaire cote "
        "employeur plutot qu'une erreur isolee. Pour une exploration plus "
        "fine (par secteur, taille d'entreprise, concentration par employeur "
        "ou individu), voir le notebook analyse_incoherences_salaires.ipynb.",
    ),
]


def _write_guide_lecture_sheet(wb, header_fmt, text_fmt) -> None:
    """Ecrit une feuille explicative statique decrivant chaque feuille du classeur."""
    ws = wb.add_worksheet("Guide_Lecture")
    ws.set_tab_color("#2C3E50")

    title_fmt = wb.add_format({"bold": True, "font_size": 14})
    subtitle_fmt = wb.add_format({"italic": True, "font_color": "#7F8C8D"})
    ws.write(0, 0, "Guide de lecture du classeur d'audit qualite", title_fmt)
    ws.write(
        1, 0,
        "Cette feuille explique l'objectif de chaque feuille du classeur et "
        "comment en interpreter le contenu.",
        subtitle_fmt,
    )

    header_row = 3
    cols = ["Feuille", "Objectif", "Comment l'interpreter"]
    for ci, col_name in enumerate(cols):
        ws.write(header_row, ci, col_name, header_fmt)

    sheet_name_fmt = wb.add_format({
        "bold": True, "border": 1, "text_wrap": True, "valign": "top",
    })
    body_fmt = wb.add_format({"border": 1, "text_wrap": True, "valign": "top"})
    alt_sheet_name_fmt = wb.add_format({
        "bold": True, "border": 1, "text_wrap": True, "valign": "top",
        "bg_color": _ALT_ROW_COLOR,
    })
    alt_body_fmt = wb.add_format({
        "border": 1, "text_wrap": True, "valign": "top", "bg_color": _ALT_ROW_COLOR,
    })

    for ri, (feuille, objectif, interpretation) in enumerate(_GUIDE_LECTURE):
        is_alt = ri % 2 == 1
        fmt_name = alt_sheet_name_fmt if is_alt else sheet_name_fmt
        fmt_body = alt_body_fmt if is_alt else body_fmt
        row = header_row + 1 + ri
        ws.write(row, 0, feuille, fmt_name)
        ws.write(row, 1, objectif, fmt_body)
        ws.write(row, 2, interpretation, fmt_body)

    ws.set_column(0, 0, 22)
    ws.set_column(1, 1, 45)
    ws.set_column(2, 2, 55)
    ws.freeze_panes(header_row + 1, 0)


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
    output_bucket: str,
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
    distribution: pl.DataFrame,
    manquants_vs_salaire: pl.DataFrame,
    analyse_salaire: pl.DataFrame,
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
            ("Distribution", distribution),
            ("Manquants_vs_Salaire", manquants_vs_salaire),
            ("Analyse_Salaire", analyse_salaire),
        ]

        _write_guide_lecture_sheet(wb, header_fmt, text_fmt)

        for sheet_name, df in sheets:
            _write_standard_sheet(wb, sheet_name, df,
                                  header_fmt, number_fmt, decimal_fmt, text_fmt,
                                  alt_number_fmt, alt_decimal_fmt, alt_text_fmt)

        _write_transition_sheet(wb, transitions, header_fmt, number_fmt, decimal_fmt, text_fmt)

        wb.close()

    write_workbook(cfg.minio, output_bucket, output_object, _write)


# ---------------------------------------------------------------------------
# API publique
# ---------------------------------------------------------------------------

def executer_audit(
    cfg: PipelineConfig,
    *,
    input_bucket: str | None = None,
    input_prefix: str | None = None,
    output_bucket: str | None = None,
    output_prefix: str | None = None,
    salary_var: str = "SALAIRE_BRUT",
    id_var: str = "ID_INDIV",
    type_var: str = "TYPE_SALARIE",
    iqr_multiplier: float = 1.5,
) -> str:
    """
    Execute l'audit qualite complet et exporte les resultats en Excel sur MinIO.

    Parameters
    ----------
    cfg : PipelineConfig
        Configuration du pipeline.
    input_bucket : str, optional
        Bucket MinIO contenant les objets Parquet a auditer.
        Par defaut : ``cfg.minio.processed_bucket``.
    input_prefix : str, optional
        Prefixe MinIO contenant les objets Parquet a auditer.
        Par defaut : ``cfg.minio.processed_prefix``.
    output_bucket : str, optional
        Bucket MinIO pour le fichier Excel de sortie.
        Par defaut : ``cfg.minio.output_bucket``.
    output_prefix : str, optional
        Prefixe MinIO pour le fichier Excel de sortie.
        Par defaut : ``cfg.minio.output_prefix``.
    salary_var : str
        Colonne utilisee pour la detection de valeurs extremes.
    id_var : str
        Colonne utilisee pour le controle d'unicite.
    type_var : str
        Colonne de periodicite du salaire (M/J/H), utilisee pour ventiler
        Outliers_Salaire et Analyse_Salaire par periodicite.
    iqr_multiplier : float
        Multiplicateur IQR pour les bornes de valeurs extremes (1.5 par defaut).

    Returns
    -------
    str
        Nom de l'objet Excel d'audit genere sur MinIO.
    """
    if input_bucket is None:
        input_bucket = cfg.minio.processed_bucket
    if input_prefix is None:
        input_prefix = cfg.minio.processed_prefix
    if output_bucket is None:
        output_bucket = cfg.minio.output_bucket
    if output_prefix is None:
        output_prefix = cfg.minio.output_prefix

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    output_object = f"{output_prefix}audit_fichiers_cnps_{timestamp}.xlsx"

    logger.info("=" * 60)
    logger.info("AUDIT QUALITE DES DONNEES")
    logger.info("=" * 60)
    logger.info("Source : {}/{}", input_bucket, input_prefix)

    data = _load_files(cfg.minio, input_bucket, input_prefix)
    if not data:
        logger.warning("Aucun fichier parquet trouve sous : {}/{}", input_bucket, input_prefix)
        return output_object

    logger.info("Fichiers a auditer : {}", len(data))

    logger.info("1/11 - Verification des doublons...")
    df_doublons = _check_doublons(data)

    logger.info("2/11 - Verification des colonnes...")
    df_colonnes = _check_colonnes(data)

    logger.info("3/11 - Verification des types...")
    df_types = _check_types(data)

    logger.info("4/11 - Verification des valeurs manquantes...")
    df_missing = _check_valeurs_manquantes(data)

    logger.info("5/11 - Detection des outliers ({}, par periodicite {})...", salary_var, type_var)
    df_outliers = _check_outliers(
        data, variable=salary_var, iqr_multiplier=iqr_multiplier, type_var=type_var
    )

    logger.info("6/11 - Verification de l'unicite des ID ({})...", id_var)
    df_unicite = _check_unicite_id(data, id_var=id_var)

    logger.info("7/11 - Top 5% des ID les plus dupliques ({})...", id_var)
    df_top_dup = _check_top_doublons_id(data, id_var=id_var)

    logger.info("8/11 - Matrice de transitions des ID ({})...", id_var)
    df_transitions = _check_transitions(data, id_var=id_var)

    logger.info("9/11 - Distribution des variables numeriques...")
    df_distribution = _check_distribution(data)

    logger.info("10/11 - Valeurs manquantes selon presence de {}...", salary_var)
    df_manquants_vs_salaire = _check_manquants_vs_salaire(data, salary_var=salary_var)

    logger.info("11/11 - Analyse du salaire par periodicite declaree ({})...", type_var)
    df_analyse_salaire = _check_analyse_salaire(
        data, salary_var=salary_var, type_var=type_var, smig_mensuel=cfg.cleaning.min_salary
    )

    logger.info("Export Excel...")
    _export_audit_excel(
        cfg, output_bucket, output_object,
        doublons=df_doublons, colonnes=df_colonnes, types=df_types,
        valeurs_manquantes=df_missing, outliers=df_outliers,
        unicite_id=df_unicite, top_doublons_id=df_top_dup, transitions=df_transitions,
        distribution=df_distribution, manquants_vs_salaire=df_manquants_vs_salaire,
        analyse_salaire=df_analyse_salaire,
    )

    logger.info("Fichier d'audit genere : {}", output_object)
    return output_object


def executer_audit_etape(cfg: PipelineConfig, stage: str, **kwargs) -> str:
    """Dispatche l'audit vers l'implementation de l'etape demandee."""
    if stage == "01":
        # executer_audit() sans input_bucket/input_prefix explicites retombe
        # sur cfg.minio.processed_bucket/processed_prefix (silver/cnps/),
        # qui EST la sortie de l'etape 01 (01_lecture_fichiers.py). C'est ce
        # mapping implicite (defauts == sortie etape 01) qui fait de cet
        # appel "l'audit de l'etape 01" : une etape 02 future devra passer
        # input_bucket/input_prefix explicitement pour cibler sa propre
        # sortie (ex. gold/cnps/ apres harmonisation).
        return executer_audit(cfg, **kwargs)

    raise ValueError(
        f"Etape '{stage}' non implementee. "
        f"Etapes disponibles : {', '.join(_ETAPES_DISPONIBLES)}"
    )


if __name__ == "__main__":
    import argparse
    import sys
    from pathlib import Path

    from cnps.config import load_config

    parser = argparse.ArgumentParser(
        description=__doc__.strip().splitlines()[0] if __doc__ else None
    )
    parser.add_argument(
        "--stage", default="01",
        help="Etape a auditer, sur 2 chiffres (defaut: 01). "
             f"Disponibles : {', '.join(_ETAPES_DISPONIBLES)}",
    )
    parser.add_argument("--settings", "-s", type=Path, default=None)
    parser.add_argument("--dimensions", "-d", type=Path, default=None)
    parser.add_argument("--salary-var", default="SALAIRE_BRUT")
    parser.add_argument("--id-var", default="ID_INDIV")
    parser.add_argument("--type-var", default="TYPE_SALARIE",
                         help="Colonne de periodicite du salaire (M/J/H)")
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
        executer_audit_etape(
            cfg, args.stage,
            salary_var=args.salary_var,
            id_var=args.id_var,
            type_var=args.type_var,
        )
        logger.info("Termine avec succes.")
    except Exception as exc:
        logger.exception("Echec de l'audit: {}", exc)
        sys.exit(1)
