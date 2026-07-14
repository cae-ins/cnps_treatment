"""
Interface en ligne de commande du pipeline de traitement CNPS.

Exemples d'utilisation::

    # Pipeline complet
    cnps run

    # Etapes specifiques
    cnps run --from CLEAN --to EXPORT_EXCEL

    # Ingestion seule
    cnps ingest

    # Estimation et export seuls
    cnps estimate

    # Audit qualite (rapport Excel avec 8 controles)
    cnps audit
    cnps audit --input cnps/processed_data/ --salary-var SALAIRE_BRUT_MENS

    # Validation des donnees et modeles
    cnps validate

    # Afficher la configuration
    cnps config
"""

from __future__ import annotations

import importlib
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
    """Configure les sorties de journalisation (loguru)."""
    logger.remove()
    level = "DEBUG" if verbose else "INFO"

    logger.add(
        lambda msg: console.print(msg, end=""),
        level=level,
        format="<green>{time:HH:mm:ss}</green> | <level>{level:<8}</level> | {message}",
        colorize=True,
    )

    log_path = cfg.paths.logs / "pipeline.log"
    logger.add(
        str(log_path),
        level="DEBUG",
        rotation="10 MB",
        retention="30 days",
        encoding="utf-8",
    )


# ---------------------------------------------------------------------------
# Commandes
# ---------------------------------------------------------------------------

@app.command()
def run(
    settings: Optional[Path] = typer.Option(
        None, "--settings", "-s", help="Chemin vers settings.yaml",
    ),
    dimensions: Optional[Path] = typer.Option(
        None, "--dimensions", "-d", help="Chemin vers dimensions.yaml",
    ),
    from_stage: str = typer.Option(
        "LECTURE_FICHIERS", "--from", "-f", help="Premiere etape a executer",
    ),
    to_stage: str = typer.Option(
        "EXPORT_EXCEL", "--to", "-t", help="Derniere etape a executer",
    ),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """Execute le pipeline complet (ou un sous-ensemble d'etapes)."""
    cfg = load_config(settings, dimensions)
    _setup_logging(cfg, verbose)

    try:
        stage_from = Stage[from_stage.upper()]
        stage_to = Stage[to_stage.upper()]
    except KeyError as e:
        valid = ", ".join(s.name for s in Stage)
        console.print(f"[red]Etape invalide: {e}. Etapes valides: {valid}[/red]")
        raise typer.Exit(1)

    result = run_pipeline(cfg, stage_from, stage_to)

    table = Table(title=f"Pipeline {'OK' if result.success else 'ECHEC'}")
    table.add_column("Etape", style="cyan")
    table.add_column("Statut", justify="center")
    table.add_column("Duree", justify="right")

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
    """Execute uniquement l'ingestion (Excel -> Parquet + harmonisation des types)."""
    cfg = load_config(settings)
    _setup_logging(cfg, verbose)
    run_pipeline(cfg, Stage.LECTURE_FICHIERS, Stage.HARMONISATION_TYPES)


@app.command()
def clean(
    settings: Optional[Path] = typer.Option(None, "--settings", "-s"),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """Execute le nettoyage et la structuration des bases."""
    cfg = load_config(settings)
    _setup_logging(cfg, verbose)
    run_pipeline(cfg, Stage.NETTOYAGE_DONNEES, Stage.BASE_ANALYTIQUE)


@app.command()
def model(
    settings: Optional[Path] = typer.Option(None, "--settings", "-s"),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """Execute la modelisation (declaration, imputation, ponderation)."""
    cfg = load_config(settings)
    _setup_logging(cfg, verbose)
    run_pipeline(cfg, Stage.MODELE_DECLARATION, Stage.PONDERATION_FINALE)


@app.command()
def estimate(
    settings: Optional[Path] = typer.Option(None, "--settings", "-s"),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """Execute l'estimation des indicateurs et l'export Excel."""
    cfg = load_config(settings)
    _setup_logging(cfg, verbose)
    run_pipeline(cfg, Stage.ESTIMATION_INDICATEURS, Stage.EXPORT_EXCEL)


@app.command()
def audit(
    settings: Optional[Path] = typer.Option(None, "--settings", "-s"),
    input_prefix: Optional[str] = typer.Option(
        None, "--input", "-i", help="Prefixe MinIO des parquets a auditer (defaut: processed_prefix)",
    ),
    output_prefix: Optional[str] = typer.Option(
        None, "--output", "-o", help="Prefixe MinIO pour le rapport Excel (defaut: output_prefix)",
    ),
    salary_var: str = typer.Option("SALAIRE_BRUT", "--salary-var", help="Colonne pour la detection de valeurs extremes"),
    id_var: str = typer.Option("ID_INDIV", "--id-var", help="Colonne pour le controle d'unicite"),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """Execute un audit qualite complet et genere un rapport Excel."""
    cfg = load_config(settings)
    _setup_logging(cfg, verbose)

    audit_module = importlib.import_module("cnps.audit_qualite")
    out = audit_module.executer_audit(
        cfg,
        input_prefix=input_prefix,
        output_prefix=output_prefix,
        salary_var=salary_var,
        id_var=id_var,
    )
    console.print(f"[bold green]Rapport d'audit genere:[/bold green] {out}")


@app.command()
def validate(
    settings: Optional[Path] = typer.Option(None, "--settings", "-s"),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """Execute tous les controles de validation."""
    cfg = load_config(settings)
    _setup_logging(cfg, verbose)

    validation_module = importlib.import_module("cnps.11_validation_qualite")
    report = validation_module.valider_tout(cfg)

    table = Table(title=f"Rapport de validation ({report.summary()})")
    table.add_column("Niveau", style="bold", width=8)
    table.add_column("Etape", width=12)
    table.add_column("Controle", width=25)
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
    """Affiche la configuration courante."""
    cfg = load_config(settings)

    console.print("[bold]Configuration du pipeline CNPS[/bold]\n")
    console.print(f"  Racine projet:      {cfg.paths.project_root}")
    console.print(f"  Logs:               {cfg.paths.logs}")
    console.print()
    console.print(f"  MinIO endpoint:     {cfg.minio.endpoint}")
    console.print(f"  MinIO bucket:       {cfg.minio.bucket}")
    console.print(f"  Prefixe brut:       {cfg.minio.raw_prefix}")
    console.print(f"  Prefixe traite:     {cfg.minio.processed_prefix}")
    console.print(f"  Prefixe nettoye:    {cfg.minio.cleaned_prefix}")
    console.print(f"  Prefixe modeles:    {cfg.minio.models_prefix}")
    console.print(f"  Prefixe sortie:     {cfg.minio.output_prefix}")
    console.print()
    console.print(f"  Methode d'estimation: {cfg.modeling.estimation_method}")
    console.print(f"  Imputations:          {cfg.modeling.n_imputations}")
    console.print(f"  Taille min. cellule:  {cfg.estimation.min_cell_size}")
    console.print(f"  Niveau de confiance:  {cfg.estimation.confidence_level}")
    console.print(f"  Parallelisme:         {cfg.parallel.n_jobs} jobs ({cfg.parallel.backend})")
    console.print()

    dims_enabled = [d for d in cfg.dimensions if d.enabled]
    console.print(f"  Dimensions:      {len(dims_enabled)} activees")
    for d in dims_enabled:
        console.print(f"    - {d.label} ({', '.join(d.group_by) or 'global'})")


if __name__ == "__main__":
    app()
