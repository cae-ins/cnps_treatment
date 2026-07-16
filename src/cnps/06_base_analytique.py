"""
Etape 6/12 — Construction de la base analytique.

Fusionne la base individus (etape 4) et la base entreprises (etape 5) en
un seul jeu de donnees analytique pret pour l'estimation ponderee. Chaque
ligne est une observation individu enrichie des poids entreprise, des
indicateurs de declaration et de toutes les dimensions d'analyse.

References
----------
Brick, J. M. & Kalton, G. (1996). Handling missing data in survey research.
    *Statistical Methods in Medical Research*, 5(3), 215-238.
"""

from __future__ import annotations

import polars as pl
from loguru import logger

from cnps.config import PipelineConfig
from cnps.storage import object_exists, read_parquet, write_parquet


def construire_base_analytique(cfg: PipelineConfig) -> str:
    """
    Fusionne les bases individus et entreprises en base analytique.
Don
    Parameters
    ----------
    cfg : PipelineConfig
        Configuration du pipeline.

    Returns
    -------
    str
        Nom de l'objet Parquet de la base analytique sur MinIO.
    """
    bucket = cfg.minio.cleaned_bucket
    indiv_object = f"{cfg.minio.cleaned_prefix}individual_base.parquet"
    firm_object = f"{cfg.minio.cleaned_prefix}firm_base.parquet"

    for obj, label in [(indiv_object, "individus"), (firm_object, "entreprises")]:
        if not object_exists(cfg.minio, bucket, obj):
            raise FileNotFoundError(f"Base {label} introuvable : {bucket}/{obj}")

    indiv = read_parquet(cfg.minio, bucket, indiv_object)
    firm = read_parquet(cfg.minio, bucket, firm_object)

    logger.info("Fusion individus ({} lignes) avec entreprises ({} lignes)",
                indiv.height, firm.height)

    # Selectionne les colonnes entreprise a joindre (evite les doublons)
    indiv_cols = set(indiv.columns)
    firm_join_cols = ["ID_EMPLOYEUR", "PERIOD"]
    firm_value_cols = [
        c for c in firm.columns
        if c not in indiv_cols or c in firm_join_cols
    ]

    firm_subset = firm.select(
        [c for c in firm_value_cols if c in firm.columns]
    )

    join_on = [c for c in firm_join_cols if c in indiv.columns and c in firm_subset.columns]
    if join_on:
        analytical = indiv.join(firm_subset, on=join_on, how="left")
    else:
        logger.warning("Aucune cle de jointure commune trouvee. Base individus retournee telle quelle.")
        analytical = indiv

    # Poids final provisoire (mis a jour apres la modelisation, etape 9)
    analytical = analytical.with_columns(
        (pl.col("W_JT") * pl.col("W_INDIV")).alias("W_FINAL")
    )

    # Verifie la presence des dimensions d'analyse configurees
    enabled_dims = [d for d in cfg.dimensions if d.enabled and d.group_by]
    for dim in enabled_dims:
        for col in dim.group_by:
            if col not in analytical.columns:
                logger.warning("Colonne de dimension '{}' ({}) absente de la base analytique",
                               col, dim.label)

    out_object = f"{cfg.minio.cleaned_prefix}analytical_base.parquet"
    write_parquet(cfg.minio, bucket, out_object, analytical)
    logger.info("Base analytique : {} lignes, {} colonnes -> {}",
                analytical.height, analytical.width, out_object)

    return out_object
