"""
Command-line interface for the CNPS Treatment Pipeline.

Usage examples::

    # Run the full pipeline
    cnps run

    # Run specific stages
    cnps run --from-stage CLEAN --to-stage ESTIMATION

    # Run only ingestion
    cnps ingest

    # Run only estimation and export
    cnps estimate

    # Data quality audit (Excel report with 6 checks)
    cnps audit
    cnps audit --input path/to/folder --salary-var SALAIRE_BRUT_MENS

    # Validate data and models
    cnps validate

    # Show configuration
    cnps config
"""

from __future__ import annotations

from pathlib import Path
from typing import Optional

import typer
from loguru import logger
from rich.console import Console
from rich.table import Table

from cnps.config import load_config
from cnps.pipeline import Stage, run_pipeline

app = typer.Typer(
    name="cnps",
    help="CNPS Treatment Pipeline v2.0 — Traitement statistique des declarations salariales",
    add_completion=False,
)
console = Console()


def _setup_logging(cfg, verbose: bool = False) -> None:
    """Configure loguru sinks."""
    logger.remove()
    level = "DEBUG" if verbose else cfg.logging.get("level", "INFO") \
        if hasattr(cfg, "logging") else "INFO"

    # Console sink
    logger.add(
        lambda msg: console.print(msg, end=""),
        level=level,
        format="<green>{time:HH:mm:ss}</green> | <level>{level:<8}</level> | {message}",
        colorize=True,
    )

    # File sink
    log_path = cfg.paths.logs / "pipeline.log"
    logger.add(
        str(log_path),
        level="DEBUG",
        rotation="10 MB",
        retention="30 days",
        encoding="utf-8",
    )


# ---------------------------------------------------------------------------
# Commands
# ---------------------------------------------------------------------------

@app.command()
def run(
    settings: Optional[Path] = typer.Option(
        None, "--settings", "-s", help="Path to settings.yaml",
    ),
    dimensions: Optional[Path] = typer.Option(
        None, "--dimensions", "-d", help="Path to dimensions.yaml",
    ),
    from_stage: str = typer.Option(
        "INGEST", "--from", "-f", help="First stage to run",
    ),
    to_stage: str = typer.Option(
        "EXPORT", "--to", "-t", help="Last stage to run",
    ),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """Run the full pipeline (or a subset of stages)."""
    cfg = load_config(settings, dimensions)
    _setup_logging(cfg, verbose)

    try:
        stage_from = Stage[from_stage.upper()]
        stage_to = Stage[to_stage.upper()]
    except KeyError as e:
        valid = ", ".join(s.name for s in Stage)
        console.print(f"[red]Invalid stage: {e}. Valid stages: {valid}[/red]")
        raise typer.Exit(1)

    result = run_pipeline(cfg, stage_from, stage_to)

    # Summary table
    table = Table(title=f"Pipeline {'OK' if result.success else 'FAILED'}")
    table.add_column("Stage", style="cyan")
    table.add_column("Status", justify="center")
    table.add_column("Duration", justify="right")

    for s in result.stages:
        status_style = "green" if s.status == "ok" else "red" if s.status == "error" else "yellow"
        table.add_row(
            s.stage,
            f"[{status_style}]{s.status.upper()}[/{status_style}]",
            f"{s.duration_seconds:.1f}s",
        )

    table.add_row("TOTAL", "", f"{result.total_duration_seconds:.1f}s", style="bold")
    console.print(table)


@app.command()
def ingest(
    settings: Optional[Path] = typer.Option(None, "--settings", "-s"),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """Run only the ingestion stage (Excel -> Parquet)."""
    cfg = load_config(settings)
    _setup_logging(cfg, verbose)
    run_pipeline(cfg, Stage.INGEST, Stage.HARMONIZE)


@app.command()
def clean(
    settings: Optional[Path] = typer.Option(None, "--settings", "-s"),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """Run data cleaning and structuring."""
    cfg = load_config(settings)
    _setup_logging(cfg, verbose)
    run_pipeline(cfg, Stage.CLEAN, Stage.ANALYTICAL_BASE)


@app.command()
def model(
    settings: Optional[Path] = typer.Option(None, "--settings", "-s"),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """Run modeling stages (declaration, imputation, weighting)."""
    cfg = load_config(settings)
    _setup_logging(cfg, verbose)
    run_pipeline(cfg, Stage.DECLARATION_MODEL, Stage.WEIGHTING)


@app.command()
def estimate(
    settings: Optional[Path] = typer.Option(None, "--settings", "-s"),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """Run estimation and export."""
    cfg = load_config(settings)
    _setup_logging(cfg, verbose)
    run_pipeline(cfg, Stage.ESTIMATION, Stage.EXPORT)


@app.command()
def audit(
    settings: Optional[Path] = typer.Option(None, "--settings", "-s"),
    input_prefix: Optional[str] = typer.Option(
        None, "--input", "-i", help="MinIO prefix with parquet files to audit (default: processed_prefix)",
    ),
    output_prefix: Optional[str] = typer.Option(
        None, "--output", "-o", help="MinIO prefix for the audit Excel (default: output_prefix)",
    ),
    salary_var: str = typer.Option("SALAIRE_BRUT", "--salary-var", help="Column for outlier detection"),
    id_var: str = typer.Option("ID_INDIV", "--id-var", help="Column for uniqueness check"),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """Run a full data quality audit and generate an Excel report."""
    cfg = load_config(settings)
    _setup_logging(cfg, verbose)

    from cnps.diagnostics.audit import run_audit
    out = run_audit(
        cfg,
        input_prefix=input_prefix,
        output_prefix=output_prefix,
        salary_var=salary_var,
        id_var=id_var,
    )
    console.print(f"[bold green]Audit report generated:[/bold green] {out}")


@app.command()
def validate(
    settings: Optional[Path] = typer.Option(None, "--settings", "-s"),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """Run all validation checks."""
    cfg = load_config(settings)
    _setup_logging(cfg, verbose)

    from cnps.diagnostics.validation import run_all_validations
    report = run_all_validations(cfg)

    table = Table(title=f"Validation Report ({report.summary()})")
    table.add_column("Level", style="bold", width=8)
    table.add_column("Stage", width=12)
    table.add_column("Check", width=25)
    table.add_column("Message")

    for issue in report.issues:
        level_style = {
            "ERROR": "red", "WARNING": "yellow", "INFO": "green",
        }.get(issue.level, "white")
        table.add_row(
            f"[{level_style}]{issue.level}[/{level_style}]",
            issue.stage, issue.check, issue.message,
        )

    console.print(table)


@app.command()
def config(
    settings: Optional[Path] = typer.Option(None, "--settings", "-s"),
) -> None:
    """Display current configuration."""
    cfg = load_config(settings)

    console.print("[bold]CNPS Pipeline Configuration[/bold]\n")
    console.print(f"  Project root:    {cfg.paths.project_root}")
    console.print(f"  Raw data:        {cfg.paths.raw_data}")
    console.print(f"  Output:          {cfg.paths.output}")
    console.print(f"  Models:          {cfg.paths.models}")
    console.print()
    console.print(f"  Estimation:      {cfg.modeling.estimation_method}")
    console.print(f"  Imputations:     {cfg.modeling.n_imputations}")
    console.print(f"  Min cell size:   {cfg.estimation.min_cell_size}")
    console.print(f"  Confidence:      {cfg.estimation.confidence_level}")
    console.print(f"  Parallelism:     {cfg.parallel.n_jobs} jobs ({cfg.parallel.backend})")
    console.print()

    dims_enabled = [d for d in cfg.dimensions if d.enabled]
    console.print(f"  Dimensions:      {len(dims_enabled)} enabled")
    for d in dims_enabled:
        console.print(f"    - {d.label} ({', '.join(d.group_by) or 'global'})")


if __name__ == "__main__":
    app()
