"""
Etape 4/12 — Construction de la base individus.

Cree le jeu de donnees analytique au niveau individu a partir des donnees
nettoyees (etape 3). Chaque ligne represente une observation
individu-employeur-periode.

La base individus est le socle pour :
- L'estimation des poids IPW au niveau individu
- L'analyse de la distribution des salaires au niveau travailleur

References
----------
Wooldridge, J. M. (2010). *Econometric Analysis of Cross Section and Panel
    Data* (2nd ed.). MIT Press. — Chapitre 19 sur la ponderation par
    probabilite inverse (IPW).
"""

from __future__ import annotations

import polars as pl
from loguru import logger

from cnps.config import PipelineConfig
from cnps.storage import object_exists, read_parquet, write_parquet


def construire_base_individus(cfg: PipelineConfig) -> str:
    """
    Construit la base individus a partir des donnees nettoyees.

    Etapes
    ------
    1. Lecture du Parquet nettoye
    2. Creation d'un identifiant d'observation unique (ID_INDIV + NUMERO_EMPLOYEUR + PERIOD)
    3. Initialisation des poids individuels a 1
    4. Ecriture de la base individus

    Parameters
    ----------
    cfg : PipelineConfig
        Configuration du pipeline.

    Returns
    -------
    str
        Nom de l'objet Parquet de la base individus sur MinIO.
    """
    bucket = cfg.minio.cleaned_bucket
    cleaned_object = f"{cfg.minio.cleaned_prefix}cnps_cleaned.parquet"
    if not object_exists(cfg.minio, bucket, cleaned_object):
        raise FileNotFoundError(f"Donnees nettoyees introuvables : {bucket}/{cleaned_object}")

    df = read_parquet(cfg.minio, bucket, cleaned_object)
    logger.info("Construction de la base individus a partir de {} lignes", df.height)

    # Identifiant d'observation
    id_parts = [c for c in ["ID_INDIV", "NUMERO_EMPLOYEUR", "PERIOD"] if c in df.columns]
    if id_parts:
        df = df.with_columns(
            pl.concat_str(id_parts, separator="_").alias("OBS_ID")
        )

    # Initialisation des poids
    df = df.with_columns(
        pl.lit(1.0).alias("W_INDIV"),
        pl.lit(1).cast(pl.Int8).alias("S_IJT"),  # indicateur d'observation (1 = observe)
    )

    # Sous-variables de periode (si absentes)
    for col in ["TRIMESTRE", "SEMESTRE"]:
        if col not in df.columns and "MOIS" in df.columns:
            diviseur = 3 if col == "TRIMESTRE" else 6
            df = df.with_columns(
                ((pl.col("MOIS") - 1) // diviseur + 1).cast(pl.Int32).alias(col)
            )

    out_object = f"{cfg.minio.cleaned_prefix}individual_base.parquet"
    write_parquet(cfg.minio, bucket, out_object, df)
    logger.info("Base individus : {} lignes, {} colonnes -> {}", df.height, df.width, out_object)

    return out_object
