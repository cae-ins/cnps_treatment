"""
Audit qualite par etape (a la demande, hors sequence du pipeline).

Reprend les controles qualite issus de CNPS_TREATMENT_PROJECT
(01_inconsistency_check.R), adaptes au pipeline V2 (Polars + Parquet), et
les dispatche selon l'etape numerotee du pipeline dont on veut auditer la
sortie. Deux etapes sont implementees :

- Etape 01 (01_lecture_fichiers.py) : audite silver/cnps/, c'est-a-dire les
  fichiers Parquet tels que convertis par l'etape 01, AVANT l'harmonisation
  de type de l'etape 02 — le point le plus proche des fichiers CNPS
  originaux tout en restant exploitable en Parquet. Une entree par fichier
  mensuel (``MM_AAAA.parquet``).
- Etape 03 (03_nettoyage_donnees.py) : audite gold/cnps/cnps_cleaned.parquet,
  le fichier UNIQUE deja concatene et nettoye (doublons, TAG, types exclus,
  salaire minimum, variables derivees, winsorisation). Contrairement a
  l'etape 01, il n'existe pas de serie de fichiers mensuels a cette etape :
  le fichier est re-partitionne EN MEMOIRE par periode (colonne PERIOD ou
  ANNEE+MOIS, deja presentes dans les donnees) pour retomber sur le meme
  format qu'une serie mensuelle et reutiliser tous les controles ci-dessous
  sans modification. Aucune ecriture supplementaire sur MinIO.

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
    python src/cnps/audit.py --stage 03
    python src/cnps/audit.py --stage 01 --verbose
"""

from __future__ import annotations

import io
import re
from datetime import datetime

import polars as pl
from loguru import logger

from cnps.config import PipelineConfig
from cnps.storage import list_objects, object_exists, read_parquet, write_workbook

_FILENAME_RE = re.compile(r"^(\d{2})_(\d{4})\.parquet$")

_HEADER_COLOR = "#2C3E50"
_HEADER_FONT = "#FFFFFF"
_ALT_ROW_COLOR = "#F2F3F4"

_ETAPES_DISPONIBLES = ("01", "03")


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


def _load_cleaned_file(minio_cfg, bucket: str, prefix: str) -> list[tuple[str, int, int, pl.DataFrame]]:
    """
    Charge le fichier Parquet UNIQUE issu de l'etape 03 (cnps_cleaned.parquet,
    deja concatene sur tous les mois) et le re-partitionne en memoire par
    periode, pour retomber sur le meme format ``[(nom_virtuel, mois, annee,
    sous_df), ...]`` que ``_load_files`` (une entree par mois).

    Aucune ecriture n'est faite : le decoupage par periode est reconstruit a
    la volee a partir de la colonne PERIOD (ou ANNEE+MOIS a defaut), deja
    presente dans les donnees depuis les fichiers sources d'origine — ce
    n'est PAS une nouvelle segmentation en fichiers sur MinIO, uniquement une
    vue en memoire permettant de reutiliser tels quels tous les controles
    existants (par mois, matrice de transition, etc.).
    """
    object_name = f"{prefix}cnps_cleaned.parquet"
    if not object_exists(minio_cfg, bucket, object_name):
        return []

    df = read_parquet(minio_cfg, bucket, object_name)

    if "PERIOD" in df.columns:
        group_cols = ["PERIOD"]
    elif "ANNEE" in df.columns and "MOIS" in df.columns:
        group_cols = ["ANNEE", "MOIS"]
    else:
        # Pas de colonne de periode exploitable : un seul bloc, pas de
        # decoupage par mois possible (matrice de transition alors vide).
        return [("cnps_cleaned.parquet", 0, 0, df)]

    result = []
    for key, sous_df in df.group_by(group_cols, maintain_order=True):
        if group_cols == ["PERIOD"]:
            (period,) = key
            mois, annee = _parse_period_label(period)
        else:
            annee, mois = key
        label = f"cnps_cleaned_{annee:04d}-{mois:02d}.parquet"
        result.append((label, mois, annee, sous_df))

    result.sort(key=lambda x: (x[2], x[1]))
    return result


def _parse_period_label(period: str) -> tuple[int, int]:
    """Extrait (MOIS, ANNEE) d'une valeur PERIOD au format 'AAAA-MM'."""
    m = re.match(r"^(\d{4})-(\d{2})$", str(period))
    if m:
        return int(m.group(2)), int(m.group(1))
    return 0, 0


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
# a partir du SMIG mensuel (cfg.cleaning.min_salary) : 22,4 jours ouvres/mois
# (moyenne annuelle hors dimanches et jours feries), 8h/jour (179,2h/mois).
# DOIT rester identique aux constantes de 03_nettoyage_donnees.py : l'audit
# justifie la methode appliquee, il ne peut pas reposer sur une autre convention.
_JOURS_OUVRES_PAR_MOIS = 22.4
_HEURES_PAR_MOIS = 179.2

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


# ---------------------------------------------------------------------------
# Controles de couverture de declaration par individu (12, 13, 14)
#
# Ces trois controles raisonnent sur la serie temporelle d'un individu
# (``id_var``) et non plus fichier par fichier : ils concatenent donc d'abord
# tous les mois charges. Objectif : savoir, AVANT toute imputation
# (backward/forward de l'etape 03 ou modele de l'etape 08), combien
# d'individus sont concernes et par quel profil de manque, pour pouvoir
# trancher les cas au cas par cas plutot qu'en bloc.
# ---------------------------------------------------------------------------

def _concat_periodes(
    data: list[tuple[str, int, int, pl.DataFrame]],
    colonnes: list[str],
) -> pl.DataFrame | None:
    """
    Empile tous les mois charges en une seule table, restreinte a ``colonnes``.

    Retourne ``None`` si une colonne indispensable manque partout : les
    controles appelants ecrivent alors une feuille vide plutot que d'echouer.
    """
    frames = []
    for fname, mois, annee, df in data:
        manquantes = [c for c in colonnes if c not in df.columns]
        if manquantes:
            continue
        frames.append(
            df.select(colonnes).with_columns(
                pl.lit(fname).alias("_fichier"),
                pl.lit(mois, dtype=pl.Int64).alias("_MOIS"),
                pl.lit(annee, dtype=pl.Int64).alias("_ANNEE"),
            )
        )
    if not frames:
        return None
    return pl.concat(frames, how="vertical_relaxed")


def _ajouter_salaire_mensuel(
    df: pl.DataFrame, salary_var: str, type_var: str
) -> pl.DataFrame:
    """
    Ajoute ``_SAL_MENS`` : le salaire ramene a une base mensuelle selon la
    periodicite declaree, pour que les montants soient comparables d'un
    individu a l'autre.

    Memes conventions que ``_check_analyse_salaire`` et l'etape 03
    (``SALAIRE_BRUT_ESTIME_AU_MOIS``) : J -> x_JOURS_OUVRES_PAR_MOIS,
    H -> x_HEURES_PAR_MOIS, M ou non renseigne -> inchange.
    """
    if type_var not in df.columns:
        return df.with_columns(pl.col(salary_var).alias("_SAL_MENS"))
    return df.with_columns(
        pl.when(pl.col(type_var) == "J")
        .then(pl.col(salary_var) * _JOURS_OUVRES_PAR_MOIS)
        .when(pl.col(type_var) == "H")
        .then(pl.col(salary_var) * _HEURES_PAR_MOIS)
        .otherwise(pl.col(salary_var))
        .alias("_SAL_MENS")
    )


def _check_comparatif_periodicite(
    data: list[tuple[str, int, int, pl.DataFrame]],
    salary_var: str = "SALAIRE_BRUT",
    type_var: str = "TYPE_SALARIE",
    duree_var: str = "DUREE_TRAVAILLEE",
    smig_mensuel: float = 75_000.0,
) -> pl.DataFrame:
    """
    Comparatif des periodicites declarees, agrege sur toute la periode.

    Justifie le choix d'inclure ou d'exclure chaque type de salarie
    (``cleaning.exclude_employee_types``). La feuille ``Analyse_Salaire``
    contient les memes mesures, mais ventilees par fichier et par mois : sur
    23 mois, la comparaison entre periodicites y est illisible. Ici, une ligne
    par periodicite, sur l'ensemble de l'historique.

    Trois indicateurs decident :

    - ``pct_confusion_unite`` : montant incompatible avec la periodicite
      declaree (ex. un taux horaire de 200 000, ou un salaire mensuel de 500).
    - ``pct_duree_incoherente`` : ``DUREE_TRAVAILLEE`` hors de la plage
      plausible pour cette periodicite. C'est l'indicateur le plus discriminant :
      une duree incoherente rend le montant ininterpretable, donc non convertible.
    - ``pct_sous_seuil`` : montant sous le seuil de plausibilite derive du SMIG
      pour cette periodicite.

    Lecture : un type dont la majorite des lignes est incoherente ne peut pas
    etre converti de facon fiable -- la conversion au mois amplifierait
    l'erreur au lieu de la corriger. Un type dont les durees sont saines peut
    l'etre, meme si le taux de "confusion" parait eleve (l'heuristique de
    confusion produit des faux positifs : un journalier bien paye ressemble a
    un mensuel mal paye).
    """
    schema_vide = {
        "periodicite": pl.Utf8, "code": pl.Utf8, "nb_lignes": pl.Int64,
        "pct_des_salaires_renseignes": pl.Float64, "salaire_median_declare": pl.Float64,
        "equivalent_mensuel_median": pl.Float64, "nb_confusion_unite": pl.Int64,
        "pct_confusion_unite": pl.Float64, "nb_duree_incoherente": pl.Int64,
        "pct_duree_incoherente": pl.Float64, "nb_sous_seuil": pl.Int64,
        "pct_sous_seuil": pl.Float64, "seuil_plausible": pl.Float64,
    }

    colonnes = [c for c in (salary_var, type_var, duree_var) if c]
    df = _concat_periodes(data, colonnes)
    if df is None:
        df = _concat_periodes(data, [salary_var, type_var])
        duree_var = ""
    if df is None:
        return pl.DataFrame(schema=schema_vide)

    df = df.filter(pl.col(salary_var).is_not_null() & (pl.col(salary_var) > 0))
    if df.height == 0:
        return pl.DataFrame(schema=schema_vide)

    n_total = df.height
    seuils = {
        "M": smig_mensuel,
        "J": smig_mensuel / _JOURS_OUVRES_PAR_MOIS,
        "H": smig_mensuel / _HEURES_PAR_MOIS,
    }
    facteurs = {"M": 1, "J": _JOURS_OUVRES_PAR_MOIS, "H": _HEURES_PAR_MOIS}
    # Plage plausible de DUREE_TRAVAILLEE selon l'unite attendue : des mois
    # pour un mensuel, des jours pour un journalier, des heures pour un horaire.
    durees_max = {"M": 12, "J": 31, "H": 744}  # 744 = 31 j x 24 h

    rows = []
    for code in ("M", "J", "H"):
        sous = df.filter(pl.col(type_var) == code)
        n = sous.height
        if n == 0:
            continue

        seuil = seuils[code]
        sal = sous[salary_var]
        median_declare = float(sal.median()) if n else None
        median_mensuel = median_declare * facteurs[code] if median_declare else None

        # Confusion d'unite : le montant ressemble a une autre periodicite.
        # Un mensuel sous le seuil journalier, ou un taux J/H depassant le
        # seuil mensuel, sont suspects.
        if code == "M":
            confusion = sous.filter(pl.col(salary_var) < seuils["J"]).height
        else:
            confusion = sous.filter(pl.col(salary_var) >= smig_mensuel).height

        n_sous_seuil = sous.filter(pl.col(salary_var) < seuil).height

        if duree_var and duree_var in sous.columns:
            n_duree = sous.filter(
                pl.col(duree_var).is_not_null()
                & ((pl.col(duree_var) <= 0) | (pl.col(duree_var) > durees_max[code]))
            ).height
        else:
            n_duree = None

        rows.append({
            "periodicite": _LABEL_PERIODICITE.get(code, code),
            "code": code,
            "nb_lignes": n,
            "pct_des_salaires_renseignes": round(n / n_total * 100, 2),
            "salaire_median_declare": round(median_declare, 2) if median_declare else None,
            "equivalent_mensuel_median": round(median_mensuel, 2) if median_mensuel else None,
            "nb_confusion_unite": confusion,
            "pct_confusion_unite": round(confusion / n * 100, 2),
            "nb_duree_incoherente": n_duree,
            "pct_duree_incoherente": round(n_duree / n * 100, 2) if n_duree is not None else None,
            "nb_sous_seuil": n_sous_seuil,
            "pct_sous_seuil": round(n_sous_seuil / n * 100, 2),
            "seuil_plausible": round(seuil, 2),
        })

    # Lignes a periodicite non renseignee : traitees comme mensuelles par la
    # conversion, hypothese silencieuse qu'il faut pouvoir chiffrer.
    sans_type = df.filter(pl.col(type_var).is_null())
    if sans_type.height:
        rows.append({
            "periodicite": "NON RENSEIGNE (traite comme Mensuel)",
            "code": "",
            "nb_lignes": sans_type.height,
            "pct_des_salaires_renseignes": round(sans_type.height / n_total * 100, 2),
            "salaire_median_declare": round(float(sans_type[salary_var].median()), 2),
            "equivalent_mensuel_median": round(float(sans_type[salary_var].median()), 2),
            "nb_confusion_unite": None, "pct_confusion_unite": None,
            "nb_duree_incoherente": None, "pct_duree_incoherente": None,
            "nb_sous_seuil": sans_type.filter(pl.col(salary_var) < smig_mensuel).height,
            "pct_sous_seuil": round(
                sans_type.filter(pl.col(salary_var) < smig_mensuel).height
                / sans_type.height * 100, 2
            ),
            "seuil_plausible": smig_mensuel,
        })

    if not rows:
        return pl.DataFrame(schema=schema_vide)
    return pl.DataFrame(rows, schema_overrides=schema_vide)


def _check_couverture_declaration(
    data: list[tuple[str, int, int, pl.DataFrame]],
    salary_var: str = "SALAIRE_BRUT",
    id_var: str = "ID_INDIV",
    type_var: str = "TYPE_SALARIE",
    hire_date_var: str = "DATE_EMBAUCHE",
) -> pl.DataFrame:
    """
    12. Combien de personnes ont 1, 2, 3... mois de salaire manquant.

    Tous les fichiers mensuels sont concatenes en un seul bloc, puis on
    regarde chaque ``ID_INDIV`` sur l'ensemble de la periode : parmi les mois
    ou il **aurait du** etre declare (ceux posterieurs a sa ``DATE_EMBAUCHE``),
    combien n'ont pas de salaire renseigne.

    Resultat : une ligne par nombre de mois manquants, une colonne d'effectif.
    Lecture directe, du type « 1 050 personnes ont 1 mois manquant, 2 040 en
    ont 2 ». Filtrer sur ``nb_mois_manquants`` dans Excel donne le nombre de
    personnes concernees.

    Regles de comptage :

    - Un salaire compte comme declare s'il est renseigne ET strictement
      positif (une ligne a 0 ou negative n'est pas une declaration
      exploitable).
    - Seuls les mois **posterieurs a la date d'embauche** sont comptes : un
      mois anterieur n'est pas un manque, l'individu n'avait pas a y etre
      declare. ``DATE_EMBAUCHE`` est renseignee a 100% dans les fichiers
      sources ; si la colonne est absente, on retombe sur l'ensemble des mois
      observes (colonne ``base_de_comptage`` du resultat).
    - Si plusieurs dates d'embauche coexistent pour un individu (plusieurs
      employeurs), la plus ancienne definit le debut de sa vie salariee
      observee.
    - Les mois de la periode sont chaines d'une annee a l'autre : decembre
      2024 et janvier 2025 sont consecutifs.

    La derniere ligne (``nb_mois_manquants`` = nombre de mois eligibles)
    regroupe les individus qui n'ont jamais rien declare : cf. la feuille
    Non_Declarants pour leur decompte detaille.
    """
    schema_vide = {
        "nb_mois_manquants": pl.Int64, "nb_personnes": pl.Int64,
        "pct_personnes": pl.Float64, "cum_personnes": pl.Int64,
        "cum_pct_personnes": pl.Float64, "base_de_comptage": pl.Utf8,
        "nb_mois_periode": pl.Int64, "nb_personnes_total": pl.Int64,
    }

    colonnes = [c for c in (id_var, salary_var, type_var, hire_date_var) if c]
    df = _concat_periodes(data, colonnes)
    if df is None:
        # Repli : sans DATE_EMBAUCHE on compte sur toute la periode observee.
        df = _concat_periodes(data, [c for c in colonnes if c != hire_date_var])
        hire_date_var = ""
    if df is None:
        df = _concat_periodes(data, [id_var, salary_var])
        type_var = ""
    if df is None:
        return pl.DataFrame(schema=schema_vide)

    df = _ajouter_salaire_mensuel(df, salary_var, type_var)
    df = df.with_columns(
        (pl.col("_SAL_MENS").is_not_null() & (pl.col("_SAL_MENS") > 0)).alias("_DECLARE"),
        # Index chronologique absolu, continu d'une annee a l'autre
        (pl.col("_ANNEE") * 12 + pl.col("_MOIS")).alias("_T"),
    )

    n_mois_periode = int(df.select(pl.col("_T").n_unique()).item())
    t_min = int(df.select(pl.col("_T").min()).item())

    agg = [pl.col("_T").filter(pl.col("_DECLARE")).n_unique().alias("_n_declares")]
    if hire_date_var:
        agg.append(
            (
                pl.col(hire_date_var).dt.year() * 12 + pl.col(hire_date_var).dt.month()
            ).min().alias("_t_embauche")
        )

    par_indiv = df.group_by(id_var).agg(agg)

    if hire_date_var:
        # Mois eligibles = ceux de la periode qui suivent l'embauche. Une
        # embauche anterieure a l'historique rend tous les mois eligibles ;
        # une embauche posterieure a la fin n'en laisse aucun.
        mois_eligibles = (
            (pl.lit(n_mois_periode) - (pl.col("_t_embauche") - pl.lit(t_min)))
            .clip(0, n_mois_periode)
        )
        base = "Mois posterieurs a la date d'embauche"
    else:
        mois_eligibles = pl.lit(n_mois_periode)
        base = "Tous les mois de la periode (DATE_EMBAUCHE absente)"

    par_indiv = par_indiv.with_columns(
        (mois_eligibles - pl.col("_n_declares")).clip(0, None).alias("nb_mois_manquants")
    )

    n_total = par_indiv.height
    if n_total == 0:
        return pl.DataFrame(schema=schema_vide)

    dist = (
        par_indiv.group_by("nb_mois_manquants")
        .agg(pl.len().alias("nb_personnes"))
        .sort("nb_mois_manquants")
    )
    dist = dist.with_columns(
        (pl.col("nb_personnes") / n_total * 100).round(2).alias("pct_personnes"),
        pl.col("nb_personnes").cum_sum().alias("cum_personnes"),
    ).with_columns(
        (pl.col("cum_personnes") / n_total * 100).round(2).alias("cum_pct_personnes"),
        pl.lit(base).alias("base_de_comptage"),
        pl.lit(n_mois_periode, dtype=pl.Int64).alias("nb_mois_periode"),
        pl.lit(n_total, dtype=pl.Int64).alias("nb_personnes_total"),
    )

    return dist.select(
        pl.col("nb_mois_manquants").cast(pl.Int64),
        pl.col("nb_personnes").cast(pl.Int64),
        "pct_personnes",
        pl.col("cum_personnes").cast(pl.Int64),
        "cum_pct_personnes",
        "base_de_comptage",
        "nb_mois_periode",
        "nb_personnes_total",
    )


def _check_non_declarants(
    data: list[tuple[str, int, int, pl.DataFrame]],
    salary_var: str = "SALAIRE_BRUT",
    id_var: str = "ID_INDIV",
    type_var: str = "TYPE_SALARIE",
    hire_date_var: str = "DATE_EMBAUCHE",
) -> pl.DataFrame:
    """
    13. Individus qui n'ont aucun salaire declare alors qu'ils etaient
    embauches.

    Ne sont comptes que les individus ayant au moins un mois **posterieur a
    leur DATE_EMBAUCHE** dans le perimetre considere : sans mois eligible, il
    n'y a pas de declaration attendue, donc pas de defaut a constater. Meme
    regle que la feuille Couverture_Declaration, pour que les deux feuilles
    comptent la meme chose (colonne ``nb_individus_eligibles``).

    Trois lignes, via la colonne ``perimetre`` :

    - une ligne par annee : individus embauches et presents cette annee-la,
      sans aucun salaire declare sur l'annee ;
    - une ligne ``TOUTES_PERIODES`` : individus sans aucun salaire declare sur
      l'ensemble des mois charges. Ce sont les seuls veritablement
      **non imputables par continuite individuelle** (backward/forward de
      l'etape 03) : aucune valeur du meme individu n'existe nulle part pour
      servir de base. Ils relevent de l'imputation au niveau entreprise
      (etape 08) ou d'un traitement dedie.

    Les lignes annuelles et la ligne globale ne se somment pas : un individu
    muet en 2024 mais declarant en 2025 compte dans la premiere et pas dans
    la seconde. L'ecart mesure les individus rattrapables grace a l'autre
    annee.
    """
    schema_vide = {
        "perimetre": pl.Utf8, "nb_individus_presents": pl.Int64,
        "nb_individus_eligibles": pl.Int64, "nb_jamais_declarant": pl.Int64,
        "pct_jamais_declarant": pl.Float64, "nb_au_moins_une_declaration": pl.Int64,
    }

    colonnes = [c for c in (id_var, salary_var, type_var, hire_date_var) if c]
    df = _concat_periodes(data, colonnes)
    if df is None:
        df = _concat_periodes(data, [c for c in colonnes if c != hire_date_var])
        hire_date_var = ""
    if df is None:
        df = _concat_periodes(data, [id_var, salary_var])
        type_var = ""
    if df is None:
        return pl.DataFrame(schema=schema_vide)

    df = _ajouter_salaire_mensuel(df, salary_var, type_var)
    df = df.with_columns(
        (pl.col("_SAL_MENS").is_not_null() & (pl.col("_SAL_MENS") > 0)).alias("_DECLARE"),
        (pl.col("_ANNEE") * 12 + pl.col("_MOIS")).alias("_T"),
    )

    if hire_date_var:
        # Un mois n'est eligible que s'il suit le mois d'embauche : avant, il
        # n'y avait pas de declaration a attendre.
        df = df.with_columns(
            (
                pl.col("_T")
                >= (pl.col(hire_date_var).dt.year() * 12 + pl.col(hire_date_var).dt.month())
            )
            .fill_null(True)
            .alias("_ELIGIBLE")
        )
    else:
        df = df.with_columns(pl.lit(True).alias("_ELIGIBLE"))

    def _resume(perimetre: str, sous: pl.DataFrame) -> dict:
        par_indiv = sous.group_by(id_var).agg(
            pl.col("_ELIGIBLE").any().alias("_eligible"),
            (pl.col("_DECLARE") & pl.col("_ELIGIBLE")).any().alias("_a_declare"),
        )
        n_presents = par_indiv.height
        eligibles = par_indiv.filter(pl.col("_eligible"))
        n_eligibles = eligibles.height
        n_jamais = int(eligibles.filter(~pl.col("_a_declare")).height)
        return {
            "perimetre": perimetre,
            "nb_individus_presents": n_presents,
            "nb_individus_eligibles": n_eligibles,
            "nb_jamais_declarant": n_jamais,
            "pct_jamais_declarant": (
                round(n_jamais / n_eligibles * 100, 2) if n_eligibles else None
            ),
            "nb_au_moins_une_declaration": n_eligibles - n_jamais,
        }

    rows = [
        _resume(str(annee), df.filter(pl.col("_ANNEE") == annee))
        for annee in sorted(df["_ANNEE"].unique().to_list())
    ]
    rows.append(_resume("TOUTES_PERIODES", df))

    return pl.DataFrame(rows, schema_overrides=schema_vide)


def _check_declaration_entreprise(
    data: list[tuple[str, int, int, pl.DataFrame]],
    salary_var: str = "SALAIRE_BRUT",
    id_var: str = "ID_INDIV",
    type_var: str = "TYPE_SALARIE",
    employer_var: str = "ID_EMPLOYEUR",
    hire_date_var: str = "DATE_EMBAUCHE",
) -> pl.DataFrame:
    """
    15. Nature de la non-declaration au niveau entreprise-mois : totale ou
    partielle.

    Question tranchee par ce controle : le pipeline doit-il suivre l'annexe 2
    de la note methodologique (non-declaration **totale** de l'entreprise :
    IPW entreprise-mois + imputation multiple du salaire moyen) ou l'annexe 3
    (declaration **partielle**, qui ajoute un second etage individuel
    ``pi_ijt = p_jt x q_ijt`` conditionnel a la declaration de l'entreprise) ?

    Chaque couple (entreprise, mois) est classe en trois categories, d'apres
    la part de ses salaries eligibles dont le salaire est renseigne :

    - ``AUCUNE_DECLARATION`` : 0% des salaries ont un salaire. C'est le cas
      R_jt = 0 de l'annexe 2 — l'entreprise n'a rien transmis ce mois-la.
    - ``DECLARATION_PARTIELLE`` : entre 0% et 100% exclus. **Inexprimable dans
      l'annexe 2** : l'entreprise a bien declare, mais en omettant une partie
      de ses salaries. C'est le cas que l'annexe 3 traite explicitement.
    - ``DECLARATION_COMPLETE`` : 100% des salaries eligibles ont un salaire.

    Un salarie n'est compte comme eligible que si le mois suit sa
    ``DATE_EMBAUCHE`` (meme regle que les controles 12 et 13) : un salarie pas
    encore embauche n'est pas une omission de l'employeur. Les couples sans
    aucun salarie eligible sont exclus du decompte.

    Lecture : si ``DECLARATION_PARTIELLE`` est marginale, l'annexe 2 suffit.
    Si elle represente une part importante des couples ou, surtout, des
    salaries manquants (colonne ``nb_salaries_manquants``), alors traiter ces
    manquants comme une absence totale de declaration revient a ignorer les
    salaires reellement observes chez le meme employeur le meme mois, et
    l'annexe 3 s'impose (cf. son paragraphe d'introduction : « ne pas tenir
    compte de cette structure hierarchique [...] conduit a des biais »).

    Note : ce controle mesure la declaration au niveau du SALAIRE renseigne,
    ce qui est plus strict que l'indicateur ``D_JT`` de
    ``05_base_entreprises.py`` — lequel vaut 1 des que l'entreprise apparait
    dans le fichier, sans verifier qu'un salaire y figure.
    """
    schema_vide = {
        "categorie": pl.Utf8, "nb_entreprise_mois": pl.Int64,
        "pct_entreprise_mois": pl.Float64, "nb_salaries_eligibles": pl.Int64,
        "nb_salaries_declares": pl.Int64, "nb_salaries_manquants": pl.Int64,
        "pct_des_salaries_manquants": pl.Float64,
        "taux_declaration_moyen_pct": pl.Float64,
    }

    colonnes = [c for c in (id_var, employer_var, salary_var, type_var, hire_date_var) if c]
    df = _concat_periodes(data, colonnes)
    if df is None:
        df = _concat_periodes(data, [c for c in colonnes if c != hire_date_var])
        hire_date_var = ""
    if df is None:
        df = _concat_periodes(data, [id_var, employer_var, salary_var])
        type_var = ""
    if df is None:
        return pl.DataFrame(schema=schema_vide)

    df = _ajouter_salaire_mensuel(df, salary_var, type_var)
    df = df.filter(pl.col(employer_var).is_not_null())
    if df.height == 0:
        return pl.DataFrame(schema=schema_vide)

    df = df.with_columns(
        (pl.col("_SAL_MENS").is_not_null() & (pl.col("_SAL_MENS") > 0)).alias("_DECLARE"),
        (pl.col("_ANNEE") * 12 + pl.col("_MOIS")).alias("_T"),
    )

    if hire_date_var:
        df = df.filter(
            (
                pl.col("_T")
                >= (pl.col(hire_date_var).dt.year() * 12 + pl.col(hire_date_var).dt.month())
            ).fill_null(True)
        )
    if df.height == 0:
        return pl.DataFrame(schema=schema_vide)

    # Un couple (entreprise, mois) = une observation. On compte ses salaries
    # eligibles et combien d'entre eux ont un salaire renseigne.
    couples = df.group_by([employer_var, "_T"]).agg(
        pl.len().alias("_n_eligibles"),
        pl.col("_DECLARE").sum().alias("_n_declares"),
    )
    couples = couples.with_columns(
        (pl.col("_n_eligibles") - pl.col("_n_declares")).alias("_n_manquants"),
        (pl.col("_n_declares") / pl.col("_n_eligibles") * 100).alias("_taux"),
    ).with_columns(
        pl.when(pl.col("_n_declares") == 0)
        .then(pl.lit("AUCUNE_DECLARATION"))
        .when(pl.col("_n_declares") == pl.col("_n_eligibles"))
        .then(pl.lit("DECLARATION_COMPLETE"))
        .otherwise(pl.lit("DECLARATION_PARTIELLE"))
        .alias("categorie")
    )

    n_couples = couples.height
    n_manquants_total = int(couples["_n_manquants"].sum())

    resume = (
        couples.group_by("categorie")
        .agg(
            pl.len().alias("nb_entreprise_mois"),
            pl.col("_n_eligibles").sum().alias("nb_salaries_eligibles"),
            pl.col("_n_declares").sum().alias("nb_salaries_declares"),
            pl.col("_n_manquants").sum().alias("nb_salaries_manquants"),
            pl.col("_taux").mean().round(2).alias("taux_declaration_moyen_pct"),
        )
        .sort("categorie")
    )

    return resume.select(
        "categorie",
        pl.col("nb_entreprise_mois").cast(pl.Int64),
        (pl.col("nb_entreprise_mois") / n_couples * 100).round(2).alias("pct_entreprise_mois"),
        pl.col("nb_salaries_eligibles").cast(pl.Int64),
        pl.col("nb_salaries_declares").cast(pl.Int64),
        pl.col("nb_salaries_manquants").cast(pl.Int64),
        (
            pl.col("nb_salaries_manquants") / n_manquants_total * 100
            if n_manquants_total else pl.lit(None, dtype=pl.Float64)
        ).round(2).alias("pct_des_salaries_manquants"),
        "taux_declaration_moyen_pct",
    )


def _check_changement_employeur(
    data: list[tuple[str, int, int, pl.DataFrame]],
    salary_var: str = "SALAIRE_BRUT",
    id_var: str = "ID_INDIV",
    type_var: str = "TYPE_SALARIE",
    employer_var: str = "ID_EMPLOYEUR",
    seuil_rupture_pct: float = 50.0,
) -> pl.DataFrame:
    """
    14. Ampleur de la variation de salaire lors d'un changement d'employeur.

    Reconstitue la serie chronologique de chaque individu (un salaire median
    par individu/mois/employeur, ce qui aplatit les doublons intra-mois), puis
    compare chaque observation a la precedente du meme individu.

    Chaque transition est classee selon que l'employeur a change ou non, ce
    qui donne la comparaison utile : une variation de salaire forte est
    attendue lors d'un changement d'employeur, mais suspecte a employeur
    constant. Les deux lignes de la feuille se lisent donc l'une par rapport
    a l'autre, pas dans l'absolu.

    ``seuil_rupture_pct`` (50 % par defaut) definit ce qui est compte comme
    une rupture salariale : c'est un repere de lecture, pas un filtre — aucune
    ligne n'est exclue des donnees par ce controle.

    Enjeu pour l'imputation : si un changement d'employeur s'accompagne
    typiquement d'un saut important, alors reporter le salaire d'avant sur un
    mois manquant d'apres (backward/forward) transporte le salaire de
    l'ancien poste — la continuite individuelle n'est legitime qu'a employeur
    constant.
    """
    schema_vide = {
        "changement_employeur": pl.Utf8, "nb_transitions": pl.Int64,
        "nb_individus_concernes": pl.Int64, "variation_mediane_pct": pl.Float64,
        "variation_abs_mediane_pct": pl.Float64, "p25_variation_pct": pl.Float64,
        "p75_variation_pct": pl.Float64, "p90_variation_abs_pct": pl.Float64,
        "pct_salaire_stable": pl.Float64, "pct_rupture": pl.Float64,
        "pct_hausse": pl.Float64, "pct_baisse": pl.Float64,
        "pct_hausse_sup_100pct": pl.Float64, "pct_baisse_sup_50pct": pl.Float64,
    }

    colonnes = [c for c in (id_var, employer_var, salary_var, type_var) if c]
    df = _concat_periodes(data, colonnes)
    if df is None:
        df = _concat_periodes(data, [id_var, employer_var, salary_var])
    if df is None:
        return pl.DataFrame(schema=schema_vide)

    df = _ajouter_salaire_mensuel(df, salary_var, type_var)
    df = df.filter(
        pl.col("_SAL_MENS").is_not_null()
        & (pl.col("_SAL_MENS") > 0)
        & pl.col(employer_var).is_not_null()
        & pl.col(id_var).is_not_null()
    )
    if df.height == 0:
        return pl.DataFrame(schema=schema_vide)

    # Un point par individu/mois/employeur : la mediane neutralise les
    # doublons intra-mois sans privilegier un montant extreme.
    serie = (
        df.group_by([id_var, "_ANNEE", "_MOIS", employer_var])
        .agg(pl.col("_SAL_MENS").median().alias("_SAL"))
        .with_columns((pl.col("_ANNEE") * 12 + pl.col("_MOIS")).alias("_T"))
        .sort([id_var, "_T"])
    )

    serie = serie.with_columns(
        pl.col(employer_var).shift(1).over(id_var).alias("_EMP_PREC"),
        pl.col("_SAL").shift(1).over(id_var).alias("_SAL_PREC"),
    )

    transitions = serie.filter(
        pl.col("_EMP_PREC").is_not_null() & (pl.col("_SAL_PREC") > 0)
    ).with_columns(
        (pl.col(employer_var) != pl.col("_EMP_PREC")).alias("_CHANGEMENT"),
        ((pl.col("_SAL") - pl.col("_SAL_PREC")) / pl.col("_SAL_PREC") * 100).alias("_VAR_PCT"),
    )
    if transitions.height == 0:
        return pl.DataFrame(schema=schema_vide)

    resume = (
        transitions.group_by("_CHANGEMENT")
        .agg(
            pl.len().alias("nb_transitions"),
            pl.col(id_var).n_unique().alias("nb_individus_concernes"),
            pl.col("_VAR_PCT").median().round(2).alias("variation_mediane_pct"),
            pl.col("_VAR_PCT").abs().median().round(2).alias("variation_abs_mediane_pct"),
            pl.col("_VAR_PCT").quantile(0.25).round(2).alias("p25_variation_pct"),
            pl.col("_VAR_PCT").quantile(0.75).round(2).alias("p75_variation_pct"),
            pl.col("_VAR_PCT").abs().quantile(0.90).round(2).alias("p90_variation_abs_pct"),
            (pl.col("_VAR_PCT").abs() < 1).mean().alias("_stable"),
            (pl.col("_VAR_PCT").abs() > seuil_rupture_pct).mean().alias("_rupture"),
            (pl.col("_VAR_PCT") > 0).mean().alias("_hausse"),
            (pl.col("_VAR_PCT") < 0).mean().alias("_baisse"),
            (pl.col("_VAR_PCT") > 100).mean().alias("_hausse_100"),
            (pl.col("_VAR_PCT") < -50).mean().alias("_baisse_50"),
        )
        .sort("_CHANGEMENT")
    )

    return resume.select(
        pl.when(pl.col("_CHANGEMENT"))
        .then(pl.lit("OUI - changement d'employeur"))
        .otherwise(pl.lit("NON - meme employeur"))
        .alias("changement_employeur"),
        "nb_transitions",
        "nb_individus_concernes",
        "variation_mediane_pct",
        "variation_abs_mediane_pct",
        "p25_variation_pct",
        "p75_variation_pct",
        "p90_variation_abs_pct",
        (pl.col("_stable") * 100).round(2).alias("pct_salaire_stable"),
        (pl.col("_rupture") * 100).round(2).alias("pct_rupture"),
        (pl.col("_hausse") * 100).round(2).alias("pct_hausse"),
        (pl.col("_baisse") * 100).round(2).alias("pct_baisse"),
        (pl.col("_hausse_100") * 100).round(2).alias("pct_hausse_sup_100pct"),
        (pl.col("_baisse_50") * 100).round(2).alias("pct_baisse_sup_50pct"),
    )


def _build_synthese_methodologie(
    *,
    declaration_entreprise: pl.DataFrame,
    comparatif_periodicite: pl.DataFrame,
    non_declarants: pl.DataFrame,
    changement_employeur: pl.DataFrame,
    couverture_declaration: pl.DataFrame,
) -> pl.DataFrame:
    """
    Synthese : chaque decision methodologique, le chiffre qui la justifie, et
    la feuille ou le verifier.

    Les valeurs sont **relues depuis les autres controles** de ce meme
    classeur, jamais codees en dur : si les donnees changent, la justification
    suit. Une decision dont le chiffre ne serait plus verifie doit etre
    rediscutee, pas maintenue par habitude.
    """
    def _pct(df: pl.DataFrame, filtre: pl.Expr, col: str) -> float | None:
        if df.height == 0 or col not in df.columns:
            return None
        sous = df.filter(filtre)
        return float(sous[col][0]) if sous.height else None

    lignes: list[dict] = []

    # --- 1. Annexe 2 ou annexe 3 ---
    pct_partielle = _pct(
        declaration_entreprise,
        pl.col("categorie") == "DECLARATION_PARTIELLE",
        "pct_des_salaries_manquants",
    )
    lignes.append({
        "decision": "Traitement en DEUX etages (annexe 3) plutot qu'un seul (annexe 2)",
        "justification": (
            f"{pct_partielle:.1f}% des salaires manquants sont dans des entreprises "
            "qui ont pourtant declare ce mois-la. L'annexe 2 ne sait pas representer "
            "ce cas (elle suppose qu'une entreprise declare tout ou rien) et "
            "traiterait ces manquants comme une absence totale, en ignorant les "
            "salaires reellement observes chez le meme employeur."
            if pct_partielle is not None else "Mesure indisponible"
        ),
        "valeur_mesuree": f"{pct_partielle:.1f}%" if pct_partielle is not None else "n/d",
        "seuil_de_decision": "Marginal (<5%) -> annexe 2 suffirait",
        "feuille_de_verification": "Declaration_Entreprise",
    })

    # --- 2. Exclusion des horaires ---
    def _periodicite(code: str, col: str) -> float | None:
        return _pct(comparatif_periodicite, pl.col("code") == code, col)

    duree_h = _periodicite("H", "pct_duree_incoherente")
    duree_j = _periodicite("J", "pct_duree_incoherente")
    vol_h = _periodicite("H", "pct_des_salaires_renseignes")
    lignes.append({
        "decision": "Exclure les salaries HORAIRES (cleaning.exclude_employee_types)",
        "justification": (
            f"{duree_h:.1f}% des lignes horaires ont une DUREE_TRAVAILLEE "
            "incoherente : le montant declare n'est pas interpretable, et la "
            "conversion en equivalent mensuel amplifierait l'erreur au lieu "
            f"de la corriger. Ces lignes ne pesent que {vol_h:.1f}% des salaires "
            "renseignes."
            if duree_h is not None else "Mesure indisponible"
        ),
        "valeur_mesuree": f"{duree_h:.1f}% de durees incoherentes" if duree_h is not None else "n/d",
        "seuil_de_decision": "Une majorite de lignes incoherentes rend le type inexploitable",
        "feuille_de_verification": "Comparatif_Periodicite",
    })

    # --- 3. Maintien des journaliers ---
    lignes.append({
        "decision": (
            f"CONSERVER les salaries JOURNALIERS, convertis "
            f"x{_JOURS_OUVRES_PAR_MOIS:g} jours ouvres"
        ),
        "justification": (
            f"Seulement {duree_j:.2f}% de durees incoherentes : la conversion repose "
            "sur une base fiable, contrairement aux horaires. Les exclure biaiserait "
            "la representativite vers les seuls emplois stables."
            if duree_j is not None else "Mesure indisponible"
        ),
        "valeur_mesuree": f"{duree_j:.2f}% de durees incoherentes" if duree_j is not None else "n/d",
        "seuil_de_decision": "Durees saines -> conversion legitime",
        "feuille_de_verification": "Comparatif_Periodicite",
    })

    # --- 4. Pas d'imputation par continuite individuelle ---
    if changement_employeur.height:
        chg = changement_employeur.filter(
            pl.col("changement_employeur").str.starts_with("OUI")
        )
        same = changement_employeur.filter(
            pl.col("changement_employeur").str.starts_with("NON")
        )
        var_chg = float(chg["variation_abs_mediane_pct"][0]) if chg.height else None
        var_same = float(same["variation_abs_mediane_pct"][0]) if same.height else None
    else:
        var_chg = var_same = None
    lignes.append({
        "decision": (
            "NE PAS imputer un salaire manquant par report d'un autre mois du "
            "meme individu (backward/forward)"
        ),
        "justification": (
            f"Lors d'un changement d'employeur, la variation mediane du salaire est "
            f"de {var_chg:.1f}% contre {var_same:.1f}% a employeur constant. Reporter "
            "le salaire d'avant transporterait celui de l'ancien poste. Surtout, "
            "pre-remplir des manquants transforme des non-declarants en declarants "
            "aux yeux du modele de propension : les poids IPW seraient sous-estimes "
            "et la correction du biais echouerait silencieusement."
            if var_chg is not None and var_same is not None else "Mesure indisponible"
        ),
        "valeur_mesuree": (
            f"{var_chg:.1f}% vs {var_same:.1f}%" if var_chg is not None else "n/d"
        ),
        "seuil_de_decision": "Ecart important -> le report individuel n'est pas neutre",
        "feuille_de_verification": "Changement_Employeur",
    })

    # --- 5. Perimetre non imputable ---
    pct_jamais = _pct(
        non_declarants, pl.col("perimetre") == "TOUTES_PERIODES", "pct_jamais_declarant"
    )
    lignes.append({
        "decision": (
            "Reconnaitre un perimetre NON imputable par continuite individuelle"
        ),
        "justification": (
            f"{pct_jamais:.1f}% des individus eligibles n'ont aucun salaire declare "
            "sur toute la periode observee : aucune valeur du meme individu n'existe "
            "pour servir de base a un report. Ils relevent de l'imputation au niveau "
            "entreprise (etape 08)."
            if pct_jamais is not None else "Mesure indisponible"
        ),
        "valeur_mesuree": f"{pct_jamais:.1f}%" if pct_jamais is not None else "n/d",
        "seuil_de_decision": "Perimetre a documenter dans toute publication",
        "feuille_de_verification": "Non_Declarants",
    })

    # --- 6. Comptage a partir de la date d'embauche ---
    n_mois = (
        int(couverture_declaration["nb_mois_periode"][0])
        if couverture_declaration.height and "nb_mois_periode" in couverture_declaration.columns
        else None
    )
    lignes.append({
        "decision": (
            "Ne compter comme manquants que les mois POSTERIEURS a la date d'embauche"
        ),
        "justification": (
            "Un mois anterieur a l'embauche n'est pas un defaut de declaration : le "
            "salarie n'avait pas a y figurer. DATE_EMBAUCHE est renseignee a 100% "
            "dans les fichiers sources, ce denominateur est donc fiable. Sans cette "
            "regle, tout recrutement en cours de periode serait compte a tort comme "
            "une non-declaration."
        ),
        "valeur_mesuree": f"{n_mois} mois observes" if n_mois else "n/d",
        "seuil_de_decision": "Regle appliquee aux feuilles de couverture et de non-declaration",
        "feuille_de_verification": "Couverture_Declaration",
    })

    # --- 7. Winsorisation apres conversion ---
    lignes.append({
        "decision": (
            "Winsoriser APRES conversion de periodicite, sur l'equivalent mensuel"
        ),
        "justification": (
            "Winsoriser le salaire brut reviendrait a calculer des percentiles sur "
            "trois echelles incomparables (taux horaire, taux journalier, salaire "
            "mensuel). La borne basse tombe alors dans la masse des taux horaires : "
            "elle n'ecrete rien cote mensuel tout en remontant artificiellement les "
            "taux. Apres conversion, toutes les lignes sont sur la meme echelle."
        ),
        "valeur_mesuree": "Ordre des operations (etape 03)",
        "seuil_de_decision": "Prealable a toute comparaison de percentiles",
        "feuille_de_verification": "Comparatif_Periodicite",
    })

    return pl.DataFrame(lignes)


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
        "Synthese_Methodologie",
        "Recapituler chaque decision methodologique retenue, le chiffre mesure "
        "qui la justifie, et la feuille de ce classeur ou le verifier. Les "
        "valeurs sont relues depuis les autres controles a chaque execution : "
        "elles ne sont jamais codees en dur.",
        "C'est la feuille a lire en premier. Elle repond a la question « pourquoi "
        "ce traitement plutot qu'un autre ? » en renvoyant systematiquement a une "
        "mesure verifiable. Si un chiffre ne soutient plus la decision associee "
        "lors d'une execution ulterieure, cette decision doit etre rediscutee et "
        "non maintenue par habitude.",
    ),
    (
        "Declaration_Entreprise",
        "Classer chaque couple (entreprise, mois) selon la part de ses salaries "
        "dont le salaire est renseigne : AUCUNE_DECLARATION (0%), "
        "DECLARATION_PARTIELLE (entre 0 et 100% exclus) ou "
        "DECLARATION_COMPLETE (100%). Seuls les salaries dont le mois suit la "
        "DATE_EMBAUCHE sont comptes.",
        "Cette feuille justifie le choix d'un traitement a DEUX etages "
        "(probabilite que l'entreprise declare, puis probabilite que chaque "
        "salarie le soit) plutot qu'a un seul. Lire en priorite "
        "pct_des_salaries_manquants sur la ligne DECLARATION_PARTIELLE : c'est "
        "la part des salaires manquants qui se trouve dans des entreprises ayant "
        "pourtant declare ce mois-la. Un traitement a un seul etage ne sait pas "
        "representer ce cas et traiterait ces manquants comme une absence "
        "totale, en ignorant les salaires reellement observes chez le meme "
        "employeur. Comparer aussi nb_entreprise_mois et nb_salaries_eligibles : "
        "les declarations partielles sont peu nombreuses en couples mais "
        "concentrent l'essentiel des salaries, car ce sont les grandes "
        "entreprises.",
    ),
    (
        "Comparatif_Periodicite",
        "Comparer les periodicites declarees (Mensuel / Journalier / Horaire) "
        "sur l'ensemble de la periode : volume, salaire median declare et son "
        "equivalent mensuel, taux de confusion d'unite, taux de duree "
        "travaillee incoherente, taux de montants sous le seuil de plausibilite.",
        "Cette feuille justifie l'inclusion ou l'exclusion de chaque type de "
        "salarie. L'indicateur decisif est pct_duree_incoherente : une duree "
        "travaillee aberrante rend le montant ininterpretable, donc non "
        "convertible en equivalent mensuel — la conversion au mois "
        "amplifierait l'erreur au lieu de la corriger. Un taux eleve de "
        "confusion d'unite est en revanche a nuancer : l'heuristique produit des "
        "faux positifs, un journalier bien paye ressemblant a un mensuel mal "
        "paye. La derniere ligne chiffre les salaires sans periodicite "
        "renseignee, traites comme mensuels par defaut — hypothese silencieuse "
        "qu'il faut connaitre.",
    ),
    (
        "Changement_Employeur",
        "Comparer l'ampleur de la variation de salaire d'un mois au suivant "
        "selon que l'individu a change d'employeur ou non : mediane, quartiles, "
        "part de ruptures fortes.",
        "Cette feuille justifie le refus d'imputer un salaire manquant par "
        "report d'un autre mois du meme individu. Les deux lignes se lisent "
        "l'une par rapport a l'autre : une variation forte est attendue lors "
        "d'un changement d'employeur, mais suspecte a employeur constant. Si "
        "l'ecart est net, reporter le salaire d'avant sur un mois manquant "
        "d'apres transporte le salaire de l'ancien poste. S'ajoute une raison "
        "technique : pre-remplir des manquants transforme des non-declarants en "
        "declarants aux yeux du modele de propension, ce qui sous-estime les "
        "poids de correction et fait echouer silencieusement la correction du "
        "biais.",
    ),
    (
        "Couverture_Declaration",
        "Denombrer les personnes selon leur nombre de mois de salaire manquant, "
        "sur toute la periode d'un seul tenant. Une ligne par nombre de mois "
        "manquants, une colonne d'effectif. Seuls les mois posterieurs a la "
        "DATE_EMBAUCHE sont comptes.",
        "Lecture directe : filtrer sur nb_mois_manquants donne le nombre de "
        "personnes concernees ; les colonnes cumulees donnent le volume « au "
        "plus N mois manquants ». Cette feuille dimensionne l'ampleur du "
        "traitement a appliquer et justifie la regle de comptage : un mois "
        "anterieur a l'embauche n'est pas un defaut de declaration, le salarie "
        "n'avait pas a y figurer. Sans cette regle, tout recrutement en cours de "
        "periode serait compte a tort comme une non-declaration.",
    ),
    (
        "Non_Declarants",
        "Denombrer les individus qui n'ont aucun salaire declare alors qu'ils "
        "etaient embauches : une ligne par annee, plus une ligne "
        "TOUTES_PERIODES. Le pourcentage est calcule sur les individus "
        "eligibles, pas sur les presents.",
        "La ligne TOUTES_PERIODES delimite le perimetre qu'aucune methode de "
        "continuite individuelle ne peut atteindre : ces individus n'ont aucune "
        "valeur, nulle part, susceptible de servir de reference. Ils relevent "
        "d'un traitement au niveau entreprise. Ce perimetre doit etre "
        "explicitement documente dans toute publication. Les lignes annuelles et "
        "la ligne globale ne se somment pas : un individu muet une annee mais "
        "declarant l'autre reste rattrapable, d'ou un total inferieur a chaque "
        "annee prise isolement.",
    ),
    (
        "Analyse_Salaire",
        "Detailler, pour chaque fichier mensuel et chaque periodicite, le seuil "
        "de plausibilite derive du SMIG, les salaires nuls, negatifs ou sous ce "
        "seuil, et les confusions d'unite suspectees.",
        "Version detaillee de Comparatif_Periodicite, ventilee par mois. Utile "
        "pour situer dans le temps une anomalie reperee au niveau agrege : un "
        "taux qui se degrade brutalement a partir d'un mois donne oriente vers "
        "un changement de format source ou de pratique de saisie, non vers une "
        "erreur diffuse.",
    ),
    (
        "Valeurs_manquantes",
        "Mesurer, pour chaque variable et chaque fichier, la part de valeurs "
        "non renseignees.",
        "Permet de verifier que les variables sur lesquelles repose le "
        "traitement sont effectivement disponibles. C'est notamment ce qui a "
        "etabli que DATE_EMBAUCHE est renseignee a 100%, condition necessaire "
        "pour s'en servir comme reference de comptage des mois manquants.",
    ),
    (
        "Manquants_vs_Salaire",
        "Comparer le taux de valeurs manquantes de chaque variable selon que le "
        "salaire est lui-meme renseigne ou non sur la meme ligne.",
        "Renseigne sur le mecanisme de non-reponse. Un ecart important indique "
        "que les lignes sans salaire sont aussi mal remplies sur le reste : le "
        "manque n'est pas isole mais tient a un defaut de saisie global, ce qui "
        "oriente le choix des variables explicatives du modele de correction.",
    ),
    (
        "Distribution",
        "Donner les statistiques descriptives (min, max, moyenne, mediane, "
        "ecart-type, quantiles) de chaque variable numerique, par fichier.",
        "Permet de suivre les ordres de grandeur dans le temps et de reperer une "
        "rupture entre deux mois consecutifs (changement d'unite, de bareme, de "
        "population). Sert aussi de reference pour verifier l'effet de "
        "l'ecretage des valeurs extremes.",
    ),
    (
        "Outliers_Salaire",
        "Reperer les valeurs extremes par la methode des quartiles (IQR), "
        "globalement et separement pour chaque periodicite declaree.",
        "Justifie le principe d'un ecretage des valeurs extremes, et surtout le "
        "fait de le calculer APRES conversion en equivalent mensuel : comparer "
        "les lignes par periodicite montre qu'un taux journalier, mecaniquement "
        "plus petit, serait compte a tort comme une valeur extreme basse dans "
        "une distribution dominee par des salaires mensuels.",
    ),
    (
        "Doublons_lignes",
        "Detecter les lignes strictement identiques au sein d'un meme fichier "
        "mensuel.",
        "Quantifie le volume de redondance a retirer avant tout calcul "
        "d'effectif ou de masse salariale. Un pourcentage eleve indique un "
        "probleme d'extraction cote source plutot qu'une realite des donnees.",
    ),
    (
        "Transitions_ID",
        "Mesurer, pour chaque paire de mois, la proportion d'identifiants du "
        "mois d'origine retrouves dans le mois de destination.",
        "Verifie la stabilite des identifiants individuels dans le temps, "
        "condition necessaire pour suivre un meme salarie d'un mois a l'autre. "
        "Une retention faible peut traduire un turnover reel, mais une chute "
        "brutale et generalisee signale plutot un probleme de generation "
        "d'identifiant qui invaliderait tout suivi longitudinal.",
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
    valeurs_manquantes: pl.DataFrame,
    outliers: pl.DataFrame,
    transitions: pl.DataFrame,
    distribution: pl.DataFrame,
    manquants_vs_salaire: pl.DataFrame,
    analyse_salaire: pl.DataFrame,
    comparatif_periodicite: pl.DataFrame,
    couverture_declaration: pl.DataFrame,
    non_declarants: pl.DataFrame,
    changement_employeur: pl.DataFrame,
    declaration_entreprise: pl.DataFrame,
    synthese: pl.DataFrame,
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

        # Ordre de lecture : la synthese d'abord (quelles decisions, sur quels
        # chiffres), puis les feuilles qui les etablissent, puis le contexte.
        sheets = [
            # --- Ce qui justifie les choix methodologiques ---
            ("Synthese_Methodologie", synthese),
            ("Declaration_Entreprise", declaration_entreprise),
            ("Comparatif_Periodicite", comparatif_periodicite),
            ("Changement_Employeur", changement_employeur),
            ("Couverture_Declaration", couverture_declaration),
            ("Non_Declarants", non_declarants),
            # --- Ce qui caracterise les donnees ---
            ("Analyse_Salaire", analyse_salaire),
            ("Valeurs_manquantes", valeurs_manquantes),
            ("Manquants_vs_Salaire", manquants_vs_salaire),
            ("Distribution", distribution),
            ("Outliers_Salaire", outliers),
            ("Doublons_lignes", doublons),
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
    employer_var: str = "ID_EMPLOYEUR",
    hire_date_var: str = "DATE_EMBAUCHE",
    iqr_multiplier: float = 1.5,
    _load_fn=_load_files,
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
    employer_var : str
        Colonne identifiant l'employeur, utilisee par Changement_Employeur
        pour distinguer une transition a employeur constant d'un changement.
    hire_date_var : str
        Colonne de date d'embauche, utilisee par Couverture_Declaration pour
        ne compter comme manquants que les mois posterieurs a l'embauche.
    iqr_multiplier : float
        Multiplicateur IQR pour les bornes de valeurs extremes (1.5 par defaut).
    _load_fn : callable, optional
        Fonction de chargement ``(minio_cfg, bucket, prefix) -> data``, utilisee
        par ``executer_audit_etape`` pour adapter la source selon l'etape
        auditee (serie de fichiers mensuels pour l'etape 01 via ``_load_files``,
        fichier unique deja concatene re-partitionne par periode pour l'etape
        03 via ``_load_cleaned_file``). Usage interne, ne pas passer directement.

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

    data = _load_fn(cfg.minio, input_bucket, input_prefix)
    if not data:
        logger.warning("Aucun fichier parquet trouve sous : {}/{}", input_bucket, input_prefix)
        return output_object

    logger.info("Fichiers a auditer : {}", len(data))

    # --- Controles qui justifient les choix methodologiques ---
    logger.info("1/11 - Nature de la non-declaration entreprise-mois (totale/partielle)...")
    df_declaration_entreprise = _check_declaration_entreprise(
        data, salary_var=salary_var, id_var=id_var, type_var=type_var,
        employer_var=employer_var, hire_date_var=hire_date_var,
    )

    logger.info("2/11 - Comparatif des periodicites declarees ({})...", type_var)
    df_comparatif_periodicite = _check_comparatif_periodicite(
        data, salary_var=salary_var, type_var=type_var,
        smig_mensuel=cfg.cleaning.min_salary,
    )

    logger.info("3/11 - Variation de salaire lors d'un changement d'employeur...")
    df_changement_employeur = _check_changement_employeur(
        data, salary_var=salary_var, id_var=id_var, type_var=type_var,
        employer_var=employer_var,
    )

    logger.info("4/11 - Mois de salaire manquants par individu (toute la periode)...")
    df_couverture = _check_couverture_declaration(
        data, salary_var=salary_var, id_var=id_var, type_var=type_var,
        hire_date_var=hire_date_var,
    )

    logger.info("5/11 - Individus sans aucune declaration de salaire...")
    df_non_declarants = _check_non_declarants(
        data, salary_var=salary_var, id_var=id_var, type_var=type_var,
        hire_date_var=hire_date_var,
    )

    # --- Controles de caracterisation des donnees ---
    logger.info("6/11 - Analyse du salaire par periodicite declaree ({})...", type_var)
    df_analyse_salaire = _check_analyse_salaire(
        data, salary_var=salary_var, type_var=type_var, smig_mensuel=cfg.cleaning.min_salary
    )

    logger.info("7/11 - Verification des valeurs manquantes...")
    df_missing = _check_valeurs_manquantes(data)

    logger.info("8/11 - Valeurs manquantes selon presence de {}...", salary_var)
    df_manquants_vs_salaire = _check_manquants_vs_salaire(data, salary_var=salary_var)

    logger.info("9/11 - Distribution des variables numeriques...")
    df_distribution = _check_distribution(data)

    logger.info("10/11 - Detection des outliers ({}, par periodicite {})...", salary_var, type_var)
    df_outliers = _check_outliers(
        data, variable=salary_var, iqr_multiplier=iqr_multiplier, type_var=type_var
    )

    logger.info("11/11 - Doublons et rotation des identifiants...")
    df_doublons = _check_doublons(data)
    df_transitions = _check_transitions(data, id_var=id_var)

    # --- Synthese : relie chaque decision au chiffre qui la justifie ---
    logger.info("Construction de la synthese methodologique...")
    df_synthese = _build_synthese_methodologie(
        declaration_entreprise=df_declaration_entreprise,
        comparatif_periodicite=df_comparatif_periodicite,
        non_declarants=df_non_declarants,
        changement_employeur=df_changement_employeur,
        couverture_declaration=df_couverture,
    )

    logger.info("Export Excel...")
    _export_audit_excel(
        cfg, output_bucket, output_object,
        doublons=df_doublons,
        valeurs_manquantes=df_missing, outliers=df_outliers,
        transitions=df_transitions,
        distribution=df_distribution, manquants_vs_salaire=df_manquants_vs_salaire,
        analyse_salaire=df_analyse_salaire,
        comparatif_periodicite=df_comparatif_periodicite,
        couverture_declaration=df_couverture,
        non_declarants=df_non_declarants,
        changement_employeur=df_changement_employeur,
        declaration_entreprise=df_declaration_entreprise,
        synthese=df_synthese,
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

    if stage == "03":
        # Sortie de 03_nettoyage_donnees.py : UN SEUL fichier deja concatene
        # (cleaned_bucket/cleaned_prefix/cnps_cleaned.parquet), contrairement
        # a la serie mensuelle de l'etape 01. _load_cleaned_file re-partitionne
        # ce fichier unique par periode (PERIOD ou ANNEE+MOIS) pour reutiliser
        # tels quels tous les controles existants (par mois, matrice de
        # transition, etc.), sans qu'aucun nouveau fichier ne soit ecrit sur
        # MinIO : c'est une vue en memoire, pas une nouvelle segmentation.
        kwargs.setdefault("input_bucket", cfg.minio.cleaned_bucket)
        kwargs.setdefault("input_prefix", cfg.minio.cleaned_prefix)
        return executer_audit(cfg, _load_fn=_load_cleaned_file, **kwargs)

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
