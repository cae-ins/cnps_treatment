"""
Orchestrateur du pipeline.

Gere l'execution sequentielle des 12 etapes du pipeline avec :
- Journalisation et chronometrage par etape
- Gestion des erreurs et degradation gracieuse
- Suivi de session (entrees, sorties, duree par etape)
- Execution selective (d'une etape a une autre)

Le pipeline suit un DAG strict (graphe acyclique dirige) ou chaque
etape depend des sorties des etapes precedentes. Les modules d'etape
sont nommes ``NN_nom.py`` (ex: ``01_lecture_fichiers.py``) pour que
l'ordre d'execution soit visible directement dans l'explorateur de
fichiers. Un tel nom n'est pas un identifiant Python valide apres un
point (``cnps.01_lecture_fichiers`` ne s'importe pas avec une syntaxe
``import`` normale) : on les charge donc via ``importlib.import_module``.
"""

from __future__ import annotations

import importlib
import json
import time
from dataclasses import dataclass, field
from datetime import datetime
from enum import IntEnum
from typing import Any

from loguru import logger

from cnps.config import PipelineConfig


class Stage(IntEnum):
    """Etapes du pipeline, dans l'ordre d'execution.

    Les valeurs sont espacees de 10 en 10 (plutot que 1, 2, 3...) pour
    pouvoir inserer une etape supplementaire entre deux etapes existantes
    sans avoir a renumeroter les fichiers ``NN_nom.py`` deja en place.

    La jointure ANSTAT (``05_1_jointure_anstat.py``) n'est PAS une etape
    de ce pipeline : c'est un outil independant, lance a la demande via
    ``cnps enrich-anstat`` (voir ``cli.py``), qui enrichit ``firm_base.parquet``
    sans faire partie du DAG principal.
    """
    LECTURE_FICHIERS = 10
    HARMONISATION_TYPES = 20
    NETTOYAGE_DONNEES = 30
    BASE_INDIVIDUS = 40
    BASE_ENTREPRISES = 50
    BASE_ANALYTIQUE = 60
    MODELE_DECLARATION = 70
    IMPUTATION_SALAIRES = 80
    PONDERATION_FINALE = 90
    ESTIMATION_INDICATEURS = 100
    VALIDATION_QUALITE = 110
    EXPORT_EXCEL = 120


# Correspondance etape -> (module, fonction publique, libelle affiche)
_STAGE_MODULES: dict[Stage, tuple[str, str, str]] = {
    Stage.LECTURE_FICHIERS: ("cnps.01_lecture_fichiers", "lire_fichiers", "Lecture des fichiers"),
    Stage.HARMONISATION_TYPES: ("cnps.02_harmonisation_types", "harmoniser_types", "Harmonisation des types"),
    Stage.NETTOYAGE_DONNEES: ("cnps.03_nettoyage_donnees", "nettoyer_donnees", "Nettoyage des donnees"),
    Stage.BASE_INDIVIDUS: ("cnps.04_base_individus", "construire_base_individus", "Base individus"),
    Stage.BASE_ENTREPRISES: ("cnps.05_base_entreprises", "construire_base_entreprises", "Base entreprises"),
    Stage.BASE_ANALYTIQUE: ("cnps.06_base_analytique", "construire_base_analytique", "Base analytique"),
    Stage.MODELE_DECLARATION: ("cnps.07_modele_declaration", "ajuster_modele_declaration", "Modele de declaration"),
    Stage.IMPUTATION_SALAIRES: ("cnps.08_imputation_salaires", "imputer_salaires", "Imputation des salaires"),
    Stage.PONDERATION_FINALE: ("cnps.09_ponderation_finale", "calculer_poids_finaux", "Ponderation finale"),
    Stage.ESTIMATION_INDICATEURS: ("cnps.10_estimation_indicateurs", "estimer_indicateurs", "Estimation des indicateurs"),
    Stage.VALIDATION_QUALITE: ("cnps.11_validation_qualite", "valider_tout", "Validation qualite"),
    Stage.EXPORT_EXCEL: ("cnps.12_export_excel", "exporter_indicateurs", "Export Excel"),
}


def _load_stage_function(stage: Stage):
    """Charge dynamiquement la fonction publique d'un module d'etape numerote."""
    module_name, func_name, _ = _STAGE_MODULES[stage]
    module = importlib.import_module(module_name)
    return getattr(module, func_name)


@dataclass
class StageResult:
    """Resultat d'une etape unique du pipeline."""
    stage: str
    status: str                     # "ok", "error", "skipped"
    duration_seconds: float
    message: str = ""
    output_path: str = ""
    error: str = ""


@dataclass
class PipelineResult:
    """Resultat d'une execution complete du pipeline."""
    session_id: str
    start_time: str
    end_time: str
    total_duration_seconds: float
    stages: list[StageResult] = field(default_factory=list)

    @property
    def success(self) -> bool:
        return all(s.status in ("ok", "skipped") for s in self.stages)


def _run_stage(name: str, func, *args, **kwargs) -> StageResult:
    """Execute une etape avec chronometrage et gestion d'erreur."""
    logger.info("=" * 60)
    logger.info("ETAPE: {}", name)
    logger.info("=" * 60)

    t0 = time.perf_counter()
    try:
        result = func(*args, **kwargs)
        dt = time.perf_counter() - t0
        # result est soit un chemin d'objet MinIO (str), soit un DataFrame
        # Polars (etape d'estimation) : "if result" est ambigu sur un
        # DataFrame, on teste donc explicitement le type plutot que de le
        # serialiser en entier dans output_path.
        output = result if isinstance(result, str) else ""
        logger.info("Etape '{}' terminee en {:.1f}s", name, dt)
        return StageResult(name, "ok", dt, output_path=output)
    except Exception as exc:
        dt = time.perf_counter() - t0
        logger.error("Etape '{}' echouee apres {:.1f}s: {}", name, dt, exc)
        logger.exception(exc)
        return StageResult(name, "error", dt, error=str(exc))


def run_pipeline(
    cfg: PipelineConfig,
    from_stage: Stage = Stage.LECTURE_FICHIERS,
    to_stage: Stage = Stage.EXPORT_EXCEL,
) -> PipelineResult:
    """
    OExecute le pipeline de ``from_stage`` a ``to_stage``.

    Parameters
    ----------
    cfg : PipelineConfig
        Configuration du pipeline.
    from_stage : Stage
        Premiere etape a executer.
    to_stage : Stage
        Derniere etape a executer.

    Returns
    -------
    PipelineResult
        Resume de l'execution.
    """
    session_id = datetime.now().strftime("%Y%m%d_%H%M%S")
    start_time = datetime.now().isoformat()
    t_total = time.perf_counter()

    logger.info("Pipeline demarre : session={}, etapes={}->{}", session_id,
                from_stage.name, to_stage.name)

    results: list[StageResult] = []
    stages_to_run = [s for s in Stage if from_stage <= s <= to_stage]

    for stage in stages_to_run:
        _, _, label = _STAGE_MODULES[stage]

        if stage == Stage.EXPORT_EXCEL:
            # L'export a besoin des resultats d'estimation en memoire
            estimer = _load_stage_function(Stage.ESTIMATION_INDICATEURS)
            exporter = _load_stage_function(Stage.EXPORT_EXCEL)
            result = _run_stage(label, lambda: exporter(cfg, estimer(cfg)))
        else:
            func = _load_stage_function(stage)
            result = _run_stage(label, func, cfg)

        results.append(result)

        if result.status == "error" and stage < Stage.VALIDATION_QUALITE:
            logger.error("Pipeline arrete a l'etape '{}'", label)
            break

    dt_total = time.perf_counter() - t_total
    end_time = datetime.now().isoformat()

    pipeline_result = PipelineResult(
        session_id=session_id,
        start_time=start_time,
        end_time=end_time,
        total_duration_seconds=dt_total,
        stages=results,
    )

    # Sauvegarde des metadonnees de session sur MinIO
    from cnps.storage import write_json
    session_object = f"{cfg.minio.output_prefix}sessions/{session_id}/metadata.json"
    session_bucket = cfg.minio.output_bucket
    meta = {
        "session_id": session_id,
        "start_time": start_time,
        "end_time": end_time,
        "total_duration_seconds": dt_total,
        "success": pipeline_result.success,
        "stages": [
            {"stage": s.stage, "status": s.status, "duration": s.duration_seconds,
             "error": s.error}
            for s in results
        ],
    }
    write_json(cfg.minio, session_bucket, session_object, meta)

    status = "SUCCES" if pipeline_result.success else "ECHEC"
    logger.info("Pipeline {} : {:.1f}s au total", status, dt_total)

    return pipeline_result
