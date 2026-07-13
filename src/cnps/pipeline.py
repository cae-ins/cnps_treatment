"""
Pipeline orchestrator.

Manages the sequential execution of all pipeline stages with:
- Stage-level logging and timing
- Error handling and graceful degradation
- Session tracking (inputs, outputs, duration per stage)
- Selective stage execution (run from/to a specific stage)

The pipeline follows a strict DAG (directed acyclic graph) where each
stage depends on the outputs of previous stages.
"""

from __future__ import annotations

import json
import time
from dataclasses import dataclass, field
from datetime import datetime
from enum import IntEnum
from pathlib import Path
from typing import Any

from loguru import logger

from cnps.config import PipelineConfig


class Stage(IntEnum):
    """Pipeline stages in execution order."""
    INGEST = 1
    HARMONIZE = 2
    CLEAN = 3
    INDIVIDUAL_BASE = 4
    FIRM_BASE = 5
    ANALYTICAL_BASE = 6
    DECLARATION_MODEL = 7
    IMPUTATION = 8
    WEIGHTING = 9
    ESTIMATION = 10
    VALIDATION = 11
    EXPORT = 12


@dataclass
class StageResult:
    """Result of a single pipeline stage."""
    stage: str
    status: str                     # "ok", "error", "skipped"
    duration_seconds: float
    message: str = ""
    output_path: str = ""
    error: str = ""


@dataclass
class PipelineResult:
    """Result of a full pipeline run."""
    session_id: str
    start_time: str
    end_time: str
    total_duration_seconds: float
    stages: list[StageResult] = field(default_factory=list)

    @property
    def success(self) -> bool:
        return all(s.status in ("ok", "skipped") for s in self.stages)


# ---------------------------------------------------------------------------
# Stage runners
# ---------------------------------------------------------------------------

def _run_stage(name: str, func, *args, **kwargs) -> StageResult:
    """Execute a stage with timing and error handling."""
    logger.info("=" * 60)
    logger.info("STAGE: {}", name)
    logger.info("=" * 60)

    t0 = time.perf_counter()
    try:
        result = func(*args, **kwargs)
        dt = time.perf_counter() - t0
        output = str(result) if result else ""
        logger.info("Stage '{}' completed in {:.1f}s", name, dt)
        return StageResult(name, "ok", dt, output_path=output)
    except Exception as exc:
        dt = time.perf_counter() - t0
        logger.error("Stage '{}' failed after {:.1f}s: {}", name, dt, exc)
        return StageResult(name, "error", dt, error=str(exc))


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def run_pipeline(
    cfg: PipelineConfig,
    from_stage: Stage = Stage.INGEST,
    to_stage: Stage = Stage.EXPORT,
) -> PipelineResult:
    """
    Execute the pipeline from ``from_stage`` to ``to_stage``.

    Parameters
    ----------
    cfg : PipelineConfig
        Pipeline configuration.
    from_stage : Stage
        First stage to execute.
    to_stage : Stage
        Last stage to execute.

    Returns
    -------
    PipelineResult
        Summary of the pipeline run.
    """
    session_id = datetime.now().strftime("%Y%m%d_%H%M%S")
    start_time = datetime.now().isoformat()
    t_total = time.perf_counter()

    logger.info("Pipeline started: session={}, stages={}->{}", session_id,
                from_stage.name, to_stage.name)

    results: list[StageResult] = []

    # Import stage functions lazily to avoid circular imports
    stage_funcs: dict[Stage, tuple[str, Any]] = {}

    if Stage.INGEST >= from_stage and Stage.INGEST <= to_stage:
        from cnps.ingestion.excel_reader import ingest
        stage_funcs[Stage.INGEST] = ("Ingestion (Excel -> Parquet)", lambda: ingest(cfg))

    if Stage.HARMONIZE >= from_stage and Stage.HARMONIZE <= to_stage:
        from cnps.preparation.type_harmonizer import harmonize_types
        stage_funcs[Stage.HARMONIZE] = ("Type Harmonisation", lambda: harmonize_types(cfg))

    if Stage.CLEAN >= from_stage and Stage.CLEAN <= to_stage:
        from cnps.preparation.cleaner import clean
        stage_funcs[Stage.CLEAN] = ("Data Cleaning", lambda: clean(cfg))

    if Stage.INDIVIDUAL_BASE >= from_stage and Stage.INDIVIDUAL_BASE <= to_stage:
        from cnps.structuring.individual_base import build_individual_base
        stage_funcs[Stage.INDIVIDUAL_BASE] = (
            "Individual Base", lambda: build_individual_base(cfg),
        )

    if Stage.FIRM_BASE >= from_stage and Stage.FIRM_BASE <= to_stage:
        from cnps.structuring.firm_base import build_firm_base
        stage_funcs[Stage.FIRM_BASE] = ("Firm Base", lambda: build_firm_base(cfg))

    if Stage.ANALYTICAL_BASE >= from_stage and Stage.ANALYTICAL_BASE <= to_stage:
        from cnps.structuring.analytical_base import build_analytical_base
        stage_funcs[Stage.ANALYTICAL_BASE] = (
            "Analytical Base", lambda: build_analytical_base(cfg),
        )

    if Stage.DECLARATION_MODEL >= from_stage and Stage.DECLARATION_MODEL <= to_stage:
        from cnps.modeling.declaration_model import fit_declaration_model
        stage_funcs[Stage.DECLARATION_MODEL] = (
            "Declaration Model", lambda: fit_declaration_model(cfg),
        )

    if Stage.IMPUTATION >= from_stage and Stage.IMPUTATION <= to_stage:
        from cnps.modeling.imputation import impute_firm_salaries
        stage_funcs[Stage.IMPUTATION] = (
            "Multiple Imputation", lambda: impute_firm_salaries(cfg),
        )

    if Stage.WEIGHTING >= from_stage and Stage.WEIGHTING <= to_stage:
        from cnps.modeling.weighting import compute_final_weights
        stage_funcs[Stage.WEIGHTING] = (
            "Final Weighting", lambda: compute_final_weights(cfg),
        )

    if Stage.ESTIMATION >= from_stage and Stage.ESTIMATION <= to_stage:
        from cnps.estimation.estimator import estimate_all
        stage_funcs[Stage.ESTIMATION] = ("Estimation", lambda: estimate_all(cfg))

    if Stage.VALIDATION >= from_stage and Stage.VALIDATION <= to_stage:
        from cnps.diagnostics.validation import run_all_validations
        stage_funcs[Stage.VALIDATION] = (
            "Validation", lambda: run_all_validations(cfg),
        )

    if Stage.EXPORT >= from_stage and Stage.EXPORT <= to_stage:
        from cnps.estimation.estimator import estimate_all
        from cnps.export.excel_export import export_indicators
        stage_funcs[Stage.EXPORT] = (
            "Export", lambda: export_indicators(cfg, estimate_all(cfg)),
        )

    # Execute stages
    for stage_enum in sorted(stage_funcs.keys()):
        name, func = stage_funcs[stage_enum]
        result = _run_stage(name, func)
        results.append(result)

        # Stop on error (unless it's a non-critical stage)
        if result.status == "error" and stage_enum < Stage.VALIDATION:
            logger.error("Pipeline halted at stage '{}'", name)
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

    # Save session metadata
    session_dir = cfg.paths.sessions / session_id
    session_dir.mkdir(parents=True, exist_ok=True)
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
    (session_dir / "metadata.json").write_text(
        json.dumps(meta, indent=2, ensure_ascii=False), encoding="utf-8",
    )

    status = "SUCCESS" if pipeline_result.success else "FAILED"
    logger.info("Pipeline {}: {:.1f}s total", status, dt_total)

    return pipeline_result
