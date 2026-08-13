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

    # Enrichissement ANSTAT (secteur CEPICI, RCCM, DFE) -- outil independant,
    # hors sequence du pipeline, a lancer apres "cnps clean"
    cnps enrich-anstat

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

import typer
from loguru import logger
from rich.console import Console
from rich.table import Table

from cnps.config import load_config
from cnps.git_publication import auto_publish_run_report
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
    level = "DEBUG" if verbose else cfg.logging.level

    logger.add(
        lambda msg: console.print(msg, end=""),
        level=level,
        format="<green>{time:HH:mm:ss}</green> | <level>{level:<8}</level> | {message}",
        colorize=True,
    )

    log_path = cfg.paths.logs / "pipeline.log"
    logger.add(
        str(log_path),
        level=level,
        rotation=cfg.logging.rotation,
        retention=cfg.logging.retention,
        encoding="utf-8",
    )


def _raise_on_pipeline_failure(result) -> None:
    """Propage un echec du pipeline au code de retour de la commande."""
    if not result.success:
        raise typer.Exit(1)


# ---------------------------------------------------------------------------
# Commandes
# ---------------------------------------------------------------------------


@app.command()
def run(
    settings: Path | None = typer.Option(
        None,
        "--settings",
        "-s",
        help="Chemin vers settings.yaml",
    ),
    dimensions: Path | None = typer.Option(
        None,
        "--dimensions",
        "-d",
        help="Chemin vers dimensions.yaml",
    ),
    from_stage: str = typer.Option(
        "LECTURE_FICHIERS",
        "--from",
        "-f",
        help="Premiere etape a executer",
    ),
    to_stage: str = typer.Option(
        "EXPORT_EXCEL",
        "--to",
        "-t",
        help="Derniere etape a executer",
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
    try:
        auto_publish_run_report(cfg, result.session_id)
    except Exception as exc:
        # La publication Git est un canal de collecte, pas une etape statistique.
        # Son echec ne doit ni masquer le code retour du pipeline ni transformer
        # un calcul valide en echec methodologique.
        logger.exception("Auto-publication Git echouee: {}", exc)
    _raise_on_pipeline_failure(result)


@app.command()
def ingest(
    settings: Path | None = typer.Option(None, "--settings", "-s"),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """Execute uniquement l'ingestion (Excel -> Parquet + harmonisation des types)."""
    cfg = load_config(settings)
    _setup_logging(cfg, verbose)
    result = run_pipeline(cfg, Stage.LECTURE_FICHIERS, Stage.HARMONISATION_TYPES)
    _raise_on_pipeline_failure(result)


@app.command()
def clean(
    settings: Path | None = typer.Option(None, "--settings", "-s"),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """Execute le nettoyage et la structuration des bases."""
    cfg = load_config(settings)
    _setup_logging(cfg, verbose)
    result = run_pipeline(cfg, Stage.NETTOYAGE_DONNEES, Stage.BASE_ANALYTIQUE)
    _raise_on_pipeline_failure(result)


@app.command()
def model(
    settings: Path | None = typer.Option(None, "--settings", "-s"),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """Execute les deux modeles de reponse puis la ponderation IPW."""
    cfg = load_config(settings)
    _setup_logging(cfg, verbose)
    result = run_pipeline(cfg, Stage.MODELE_DECLARATION, Stage.PONDERATION_FINALE)
    _raise_on_pipeline_failure(result)


@app.command()
def estimate(
    settings: Path | None = typer.Option(None, "--settings", "-s"),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """Execute l'estimation des indicateurs et l'export Excel."""
    cfg = load_config(settings)
    _setup_logging(cfg, verbose)
    result = run_pipeline(cfg, Stage.ESTIMATION_INDICATEURS, Stage.EXPORT_EXCEL)
    _raise_on_pipeline_failure(result)


@app.command()
def enrich_anstat(
    settings: Path | None = typer.Option(None, "--settings", "-s"),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """
    Enrichit firm_base.parquet avec le referentiel ANSTAT (secteur CEPICI,
    forme juridique, RCCM, DFE), via jointure sur RAISON_SOCIALE normalisee.

    Outil independant, hors de la sequence numerotee du pipeline (n'est PAS
    execute par `cnps run`) : lancer explicitement apres `cnps clean`
    (necessite que firm_base.parquet existe deja).
    """
    cfg = load_config(settings)
    _setup_logging(cfg, verbose)

    anstat_module = importlib.import_module("cnps.jointure_anstat")
    out = anstat_module.enrichir_avec_anstat(cfg)
    console.print(f"[bold green]Base entreprises enrichie (ANSTAT):[/bold green] {out}")


@app.command()
def audit(
    settings: Path | None = typer.Option(None, "--settings", "-s"),
    input_bucket: str | None = typer.Option(
        None,
        "--input-bucket",
        help="Bucket MinIO des parquets a auditer (defaut: processed_bucket)",
    ),
    input_prefix: str | None = typer.Option(
        None,
        "--input",
        "-i",
        help="Prefixe MinIO des parquets a auditer (defaut: processed_prefix)",
    ),
    output_bucket: str | None = typer.Option(
        None,
        "--output-bucket",
        help="Bucket MinIO pour le rapport Excel (defaut: output_bucket)",
    ),
    output_prefix: str | None = typer.Option(
        None,
        "--output",
        "-o",
        help="Prefixe MinIO pour le rapport Excel (defaut: output_prefix)",
    ),
    salary_var: str = typer.Option(
        "SALAIRE_BRUT", "--salary-var", help="Colonne pour la detection de valeurs extremes"
    ),
    id_var: str = typer.Option("ID_INDIV", "--id-var", help="Colonne pour le controle d'unicite"),
    type_var: str = typer.Option(
        "TYPE_SALARIE", "--type-var", help="Colonne de periodicite du salaire (M/J/H)"
    ),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """Execute un audit qualite complet et genere un rapport Excel."""
    cfg = load_config(settings)
    _setup_logging(cfg, verbose)

    audit_module = importlib.import_module("cnps.audit")
    out = audit_module.executer_audit(
        cfg,
        input_bucket=input_bucket,
        input_prefix=input_prefix,
        output_bucket=output_bucket,
        output_prefix=output_prefix,
        salary_var=salary_var,
        id_var=id_var,
        type_var=type_var,
    )
    console.print(f"[bold green]Rapport d'audit genere:[/bold green] {out}")


@app.command()
def validate(
    settings: Path | None = typer.Option(None, "--settings", "-s"),
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
            "ERROR": "red",
            "WARNING": "yellow",
            "INFO": "green",
        }.get(issue.level, "white")
        table.add_row(
            f"[{level_style}]{issue.level}[/{level_style}]",
            issue.stage,
            issue.check,
            issue.message,
        )

    console.print(table)
    if not report.is_valid:
        raise typer.Exit(1)


@app.command()
def config(
    settings: Path | None = typer.Option(None, "--settings", "-s"),
) -> None:
    """Affiche la configuration courante."""
    cfg = load_config(settings)

    console.print("[bold]Configuration du pipeline CNPS[/bold]\n")
    console.print(f"  Racine projet:      {cfg.paths.project_root}")
    console.print(f"  Logs:               {cfg.paths.logs}")
    console.print()
    console.print(f"  MinIO endpoint:     {cfg.minio.endpoint}")
    console.print(f"  Brut:       {cfg.minio.raw_bucket}/{cfg.minio.raw_prefix}")
    console.print(f"  Traite:     {cfg.minio.processed_bucket}/{cfg.minio.processed_prefix}")
    console.print(f"  Nettoye:    {cfg.minio.cleaned_bucket}/{cfg.minio.cleaned_prefix}")
    console.print(f"  Modeles:    {cfg.minio.models_bucket}/{cfg.minio.models_prefix}")
    console.print(f"  Sortie:     {cfg.minio.output_bucket}/{cfg.minio.output_prefix}")
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
