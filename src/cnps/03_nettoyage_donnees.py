"""
Etape 3/12 — Nettoyage et enrichissement des donnees.

Concatene tous les Parquets mensuels harmonises (etape 2) en un seul
jeu de donnees, applique les regles metier (salaire minimum, types
d'employes exclus), calcule les variables derivees (ages, anciennete,
classes de taille d'entreprise) et winsorise les valeurs extremes de
salaire.

Variables derivees
------------------
- ``AGE_EMPLOYE`` : age en annees depuis ``DATE_NAISSANCE``
- ``ANCIENNETE_ENTREPRISE`` : anciennete en annees depuis ``DATE_EMBAUCHE``
- ``ANCIENNETE_IMMAT`` : anciennete d'immatriculation de l'employé 
- ``AGE_ENTREPRISE_IMMAT`` : age de l'entreprise depuis ``DATE_IMMAT_EMPLOYEUR``
- ``SALAIRE_BRUT_MENS`` : salaire mensuel = SALAIRE_BRUT / max(DUREE_TRAVAILLEE, 1) * 12
- Variables de classe (age, anciennete, taille d'entreprise)

References
----------
Tukey, J. W. (1977). *Exploratory Data Analysis*. Addison-Wesley.
    — Winsorisation comme traitement robuste des valeurs extremes.
Dixon, W. J. (1960). Simplified estimation from censored normal samples.
    *Annals of Mathematical Statistics*, 31(2), 385-391.
"""

from __future__ import annotations

import re
from datetime import date

import polars as pl
from loguru import logger

from cnps.config import PipelineConfig, load_config
from cnps.storage import list_objects, read_parquet, write_parquet


# ---------------------------------------------------------------------------
# Helpers de classification
# ---------------------------------------------------------------------------

def _classify(value_col: str, breaks: list[tuple[float, float, str]]) -> pl.Expr:
    """
    Construit une chaine Polars ``when/then/otherwise`` pour classifier une colonne.

    Parameters
    ----------
    value_col : str
        Colonne a classifier.
    breaks : list de (min, max, label)
        Bornes inferieure incluse, superieure exclue, et etiquette.
    """
    expr = pl.when(pl.col(value_col).is_null()).then(pl.lit(None).cast(pl.Utf8))
    for lo, hi, label in breaks:
        expr = expr.when(
            (pl.col(value_col) >= lo) & (pl.col(value_col) < hi)
        ).then(pl.lit(label))
    return expr.otherwise(pl.lit(None).cast(pl.Utf8))


_AGE_CLASSES = [
    (0, 25, "Moins de 25 ans"),
    (25, 35, "25-34 ans"),
    (35, 50, "35-49 ans"),
    (50, 999, "50 ans et plus"),
]

_SENIORITY_CLASSES = [
    (0, 2, "Moins de 2 ans"),
    (2, 5, "2-4 ans"),
    (5, 10, "5-9 ans"),
    (10, 999, "10 ans et plus"),
]

_FIRM_SIZE_DETAILED = [
    (1, 2, "1 salarie"),
    (2, 6, "2-5 salaries"),
    (6, 11, "6-10 salaries"),
    (11, 21, "11-20 salaries"),
    (21, 51, "21-50 salaries"),
    (51, 101, "51-100 salaries"),
    (101, 201, "101-200 salaries"),
    (201, 501, "201-500 salaries"),
    (501, 1001, "501-1000 salaries"),
    (1001, 1_000_000, "Plus de 1000 salaries"),
]

_FIRM_SIZE_REDUCED = [
    (1, 11, "Micro (1-10)"),
    (11, 51, "Petite (11-50)"),
    (51, 201, "Moyenne (51-200)"),
    (201, 1_000_000, "Grande (201+)"),
]


# ---------------------------------------------------------------------------
# API publique
# ---------------------------------------------------------------------------

def nettoyer_donnees(cfg: PipelineConfig) -> str:
    """
    Nettoie et enrichit le jeu de donnees concatene.

    Etapes
    ------
    1. Concatenation de tous les Parquets mensuels
    2. Regles metier : doublons, types d'employes exclus, salaire minimum
    3. Calcul des variables derivees (ages, anciennete, classes)
    4. Winsorisation des valeurs extremes de salaire
    5. Ecriture du Parquet nettoye

    Parameters
    ----------
    cfg : PipelineConfig
        Configuration du pipeline.

    Returns
    -------
    str
        Nom de l'objet Parquet nettoye sur MinIO.
    """
    processed_bucket = cfg.minio.processed_bucket
    all_objects = list_objects(cfg.minio, processed_bucket, cfg.minio.processed_prefix, recursive=False)
    files = sorted(obj for obj in all_objects if re.search(r"\.parquet$", obj))

    if not files:
        raise FileNotFoundError(
            f"Aucun fichier Parquet sous {processed_bucket}/{cfg.minio.processed_prefix}"
        )

    # --- 1. Concatenation ---
    logger.info("Concatenation de {} fichiers mensuels", len(files))
    frames = [read_parquet(cfg.minio, processed_bucket, f) for f in files]

    # Aligne les schemas (union des colonnes)
    all_cols = dict.fromkeys(col for f in frames for col in f.columns)
    aligned = []
    for f in frames:
        for col in all_cols:
            if col not in f.columns:
                f = f.with_columns(pl.lit(None).alias(col))
        aligned.append(f.select(list(all_cols.keys())))

    df = pl.concat(aligned, how="vertical")
    logger.info("Concatene : {} lignes, {} colonnes", df.height, df.width)

    # --- 1bis. Regles metier (filtres) ---
    if cfg.cleaning.remove_duplicates:
        n_avant = df.height
        df = df.unique()
        logger.info("Doublons supprimes : {} -> {} lignes", n_avant, df.height)

    if "TYPE_SALARIE" in df.columns and cfg.cleaning.exclude_employee_types:
        n_avant = df.height
        df = df.filter(
            ~pl.col("TYPE_SALARIE").is_in(cfg.cleaning.exclude_employee_types)
        )
        logger.info("Types d'employes exclus {} : {} -> {} lignes",
                     cfg.cleaning.exclude_employee_types, n_avant, df.height)

    if "SALAIRE_BRUT" in df.columns:
        n_avant = df.height
        df = df.filter(
            pl.col("SALAIRE_BRUT").is_null() | (pl.col("SALAIRE_BRUT") >= cfg.cleaning.min_salary)
        )
        logger.info("Salaires sous le seuil minimum ({:.0f}) exclus : {} -> {} lignes",
                     cfg.cleaning.min_salary, n_avant, df.height)

    # --- 2. Variables derivees ---
    ref_date = date.today()

    if "SALAIRE_BRUT" in df.columns and "DUREE_TRAVAILLEE" in df.columns:
        df = df.with_columns(
            (pl.col("SALAIRE_BRUT") / pl.col("DUREE_TRAVAILLEE").clip(1, cfg.cleaning.max_duration) * 12)
            .alias("SALAIRE_BRUT_MENS")
        )

        # --- Winsorisation des valeurs extremes de salaire (Tukey, 1977) ---
        # Ecrete (sans supprimer les lignes) au-dela des percentiles configures :
        # limite l'influence des erreurs de saisie / cas extremes sur la
        # moyenne, la variance et les autres estimateurs ponderes (etape 10).
        lo, hi = df.select(
            pl.col("SALAIRE_BRUT_MENS").quantile(cfg.cleaning.winsor_lower).alias("lo"),
            pl.col("SALAIRE_BRUT_MENS").quantile(cfg.cleaning.winsor_upper).alias("hi"),
        ).row(0)
        df = df.with_columns(
            pl.col("SALAIRE_BRUT_MENS").clip(lo, hi).alias("SALAIRE_BRUT_MENS")
        )
        logger.info("Winsorisation SALAIRE_BRUT_MENS : bornes [{:.0f}, {:.0f}] (p{:.0f}/p{:.0f})",
                    lo, hi, cfg.cleaning.winsor_lower * 100, cfg.cleaning.winsor_upper * 100)

    if "DATE_NAISSANCE" in df.columns:
        df = df.with_columns(
            ((pl.lit(ref_date) - pl.col("DATE_NAISSANCE")).dt.total_days() / 365.25)
            .floor()
            .cast(pl.Int32, strict=False)
            .alias("AGE_EMPLOYE")
        )

    if "DATE_EMBAUCHE" in df.columns:
        df = df.with_columns(
            ((pl.lit(ref_date) - pl.col("DATE_EMBAUCHE")).dt.total_days() / 365.25)
            .floor()
            .cast(pl.Int32, strict=False)
            .alias("ANCIENNETE_ENTREPRISE")
        )

    if "DATE_IMMATRICULATION" in df.columns:
        df = df.with_columns(
            ((pl.lit(ref_date) - pl.col("DATE_IMMATRICULATION")).dt.total_days() / 365.25)
            .floor()
            .cast(pl.Int32, strict=False)
            .alias("ANCIENNETE_IMMAT")
        )

    if "DATE_IMMAT_EMPLOYEUR" in df.columns:
        df = df.with_columns(
            ((pl.lit(ref_date) - pl.col("DATE_IMMAT_EMPLOYEUR")).dt.total_days() / 365.25)
            .floor()
            .cast(pl.Int32, strict=False)
            .alias("AGE_ENTREPRISE_IMMAT")
        )

    if "MOIS" in df.columns:
        df = df.with_columns(
            ((pl.col("MOIS") - 1) // 3 + 1).cast(pl.Int32).alias("TRIMESTRE"),
            ((pl.col("MOIS") - 1) // 6 + 1).cast(pl.Int32).alias("SEMESTRE"),
        )

    # --- Variables de classification ---
    if "AGE_EMPLOYE" in df.columns:
        df = df.with_columns(
            _classify("AGE_EMPLOYE", _AGE_CLASSES).alias("CL_AGE_EMPLOYE")
        )

    if "ANCIENNETE_ENTREPRISE" in df.columns:
        df = df.with_columns(
            _classify("ANCIENNETE_ENTREPRISE", _SENIORITY_CLASSES)
            .alias("CL_ANCIENNETE_ENTREPRISE")
        )

    if "ANCIENNETE_IMMAT" in df.columns:
        df = df.with_columns(
            _classify("ANCIENNETE_IMMAT", _SENIORITY_CLASSES)
            .alias("CL_ANCIENNETE_IMMAT")
        )

    if "AGE_ENTREPRISE_IMMAT" in df.columns:
        df = df.with_columns(
            _classify("AGE_ENTREPRISE_IMMAT", _SENIORITY_CLASSES)
            .alias("CL_AGE_ENTREPRISE")
        )

    if "EFFECTIF_SALARIES" in df.columns:
        df = df.with_columns(
            _classify("EFFECTIF_SALARIES", _FIRM_SIZE_DETAILED).alias("CLASSE_EFFECTIF"),
            _classify("EFFECTIF_SALARIES", _FIRM_SIZE_REDUCED).alias("CLASSE_EFFECTIF_REDUITE"),
        )

    # --- 3. Ecriture ---
    out_object = f"{cfg.minio.cleaned_prefix}cnps_cleaned.parquet"
    write_parquet(cfg.minio, cfg.minio.cleaned_bucket, out_object, df)
    logger.info("Donnees nettoyees ecrites : {} ({} lignes, {} colonnes)",
                out_object, df.height, df.width)

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
        level="DEBUG", rotation="10 MB", retention="30 days", encoding="utf-8",
    )

    try:
        nettoyer_donnees(cfg)
        logger.info("Termine avec succes.")
    except Exception as exc:
        logger.exception("Echec de l'etape: {}", exc)
        sys.exit(1)
