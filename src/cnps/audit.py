"""
Audit qualite par etape (a la demande, hors sequence du pipeline).

Dispatche vers l'implementation d'audit appropriee selon l'etape numerotee
du pipeline dont on veut auditer la sortie. A ce jour, seule l'etape 01
(01_lecture_fichiers.py) est implementee : elle audite silver/cnps/, c'est-
a-dire les fichiers Parquet tels que convertis par l'etape 01, AVANT
l'harmonisation de type de l'etape 02 — le point le plus proche des
fichiers CNPS originaux tout en restant exploitable en Parquet.

Comme jointure_anstat.py et audit_qualite.py, ce script est explicitement
hors du DAG numerote 01-12 (voir orchestrator.py::discover_stages) : il ne
sera jamais execute automatiquement par l'orchestrateur, uniquement a la
demande.

Usage
-----
    python src/cnps/audit.py --stage 01
    python src/cnps/audit.py --stage 01 --verbose
"""

from __future__ import annotations

from loguru import logger

from cnps.audit_qualite import executer_audit
from cnps.config import PipelineConfig

_ETAPES_DISPONIBLES = ("01",)


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
        )
        logger.info("Termine avec succes.")
    except Exception as exc:
        logger.exception("Echec de l'audit: {}", exc)
        sys.exit(1)
