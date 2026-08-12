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

from cnps.config import PipelineConfig, load_config
from cnps.firm_panel import ASOF_ATTRIBUTES
from cnps.storage import object_exists, read_parquet, write_parquet


def _joindre_attributs_entreprise(indiv: pl.DataFrame, firm: pl.DataFrame) -> pl.DataFrame:
    """
    Complete les individus avec les attributs entreprise disponibles a date.

    La valeur individuelle reste prioritaire. La valeur entreprise ne sert
    qu'a completer une valeur nulle d'un attribut as-of homonyme.

    Parameters
    ----------
    indiv : pl.DataFrame
        Base au niveau individu-periode.
    firm : pl.DataFrame
        Base au niveau entreprise-periode.

    Returns
    -------
    pl.DataFrame
        Base individuelle enrichie sans changement de cardinalite.
    """
    indiv_cols = set(indiv.columns)
    firm_join_cols = ["ID_EMPLOYEUR", "PERIOD"]
    shared_asof_cols = [
        c for c in ASOF_ATTRIBUTES if c in indiv_cols and c in firm.columns
    ]

    # Les autres homonymes restent exclus: leur arbitrage n'appartient pas a
    # ce lot et les joindre creerait silencieusement des colonnes concurrentes.
    firm_value_cols = [
        c
        for c in firm.columns
        if c not in indiv_cols or c in firm_join_cols or c in shared_asof_cols
    ]
    firm_subset = firm.select(firm_value_cols)

    join_on = [c for c in firm_join_cols if c in indiv.columns and c in firm_subset.columns]
    if join_on != firm_join_cols:
        missing = sorted(set(firm_join_cols) - set(join_on))
        raise ValueError(
            f"Impossible de construire la base analytique : cles de jointure absentes {missing}."
        )

    duplicate_firms = firm_subset.group_by(join_on).len().filter(pl.col("len") > 1).height
    if duplicate_firms:
        raise ValueError(
            "La base entreprise n'est pas unique sur "
            f"{join_on} : {duplicate_firms} cles dupliquees."
        )

    analytical = indiv.join(firm_subset, on=join_on, how="left", suffix="_FIRM")
    if shared_asof_cols:
        analytical = analytical.with_columns(
            [
                pl.coalesce([pl.col(attr), pl.col(f"{attr}_FIRM")]).alias(attr)
                for attr in shared_asof_cols
            ]
        ).drop([f"{attr}_FIRM" for attr in shared_asof_cols])

    if analytical.height != indiv.height:
        raise AssertionError(
            "Invariant de cardinalite viole pendant la jointure analytique : "
            f"{indiv.height} -> {analytical.height}."
        )

    return analytical


def construire_base_analytique(cfg: PipelineConfig) -> str:
    """
    Fusionne les bases individus et entreprises en base analytique.

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

    logger.info(
        "Fusion individus ({} lignes) avec entreprises ({} lignes)", indiv.height, firm.height
    )

    firm_join_cols = ["ID_EMPLOYEUR", "PERIOD"]
    analytical = _joindre_attributs_entreprise(indiv, firm)
    logger.info(
        "Jointure sur {} : {} colonnes entreprise ajoutees",
        firm_join_cols,
        analytical.width - indiv.width,
    )
    for attr in ASOF_ATTRIBUTES:
        if attr not in indiv.columns or attr not in firm.columns:
            continue
        n_completed = indiv[attr].null_count() - analytical[attr].null_count()
        logger.info(
            "Attribut as-of {} : {} valeurs individuelles nulles completees",
            attr,
            n_completed,
        )

    # W_JT et W_FINAL ne sont pas initialises ici. L'etape 09 joint les
    # poids entreprise valides et construit le poids d'analyse definitif.

    # Verifie la presence des dimensions d'analyse configurees
    enabled_dims = [d for d in cfg.dimensions if d.enabled and d.group_by]
    missing_dims = 0
    for dim in enabled_dims:
        for col in dim.group_by:
            if col not in analytical.columns:
                logger.warning(
                    "Colonne de dimension '{}' ({}) absente de la base analytique", col, dim.label
                )
                missing_dims += 1
    logger.info(
        "Dimensions d'analyse : {}/{} colonnes presentes",
        len(enabled_dims) - missing_dims,
        len(enabled_dims),
    )

    out_object = f"{cfg.minio.cleaned_prefix}analytical_base.parquet"
    write_parquet(cfg.minio, bucket, out_object, analytical)
    logger.info(
        "Base analytique : {} lignes, {} colonnes -> {}",
        analytical.height,
        analytical.width,
        out_object,
    )

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
        construire_base_analytique(cfg)
        logger.info("Termine avec succes.")
    except Exception as exc:
        logger.exception("Echec de l'etape: {}", exc)
        sys.exit(1)
