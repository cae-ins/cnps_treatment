#!/usr/bin/env python3
"""
CNPS Treatment Pipeline v2.0 — Reset utility.

Resets the project to its original state (raw data only) or to a specific
pipeline stage, removing all downstream artifacts.

Usage:
    python reset.py origin                          # Keep only raw data
    python reset.py stage INGEST                    # Reset to after INGEST (keep processed/)
    python reset.py stage CLEAN                     # Reset to after CLEAN (keep cleaned/)
    python reset.py stage ANALYTICAL_BASE           # Reset to after structuring
    python reset.py stage WEIGHTING                 # Reset to after modeling
    python reset.py --dry-run origin                # Preview what would be deleted
    python reset.py --yes origin                    # Skip confirmation prompt
"""

from __future__ import annotations

import shutil
from pathlib import Path

import typer
from rich.console import Console
from rich.table import Table

console = Console()

# Project root resolved from this file's location
PROJECT_ROOT = Path(__file__).resolve().parent

# ---------------------------------------------------------------------------
# What each stage produces — used to decide what to delete
# ---------------------------------------------------------------------------
# Mapping: stage_value -> list of (path_relative_to_project, description)
# Stages are cumulative: resetting to stage N deletes outputs of stages > N.

STAGE_ORDER = [
    "ORIGIN",            # 0 — raw data only
    "INGEST",            # 1
    "HARMONIZE",         # 2
    "CLEAN",             # 3
    "INDIVIDUAL_BASE",   # 4
    "FIRM_BASE",         # 5
    "ANALYTICAL_BASE",   # 6
    "DECLARATION_MODEL", # 7
    "IMPUTATION",        # 8
    "WEIGHTING",         # 9
    "ESTIMATION",        # 10
    "VALIDATION",        # 11
    "EXPORT",            # 12
]

# Artifacts produced at or after each stage.
# "dir:..." means delete everything inside the directory (keep the dir).
# "file:..." means delete that single file.
# "glob:..." means delete matching files inside a directory.
STAGE_ARTIFACTS: dict[str, list[str]] = {
    "INGEST": [
        "dir:data/processed",
    ],
    "HARMONIZE": [
        # Harmonize modifies processed/ in-place, so no separate artifacts.
        # Resetting harmonize means re-doing ingest too (processed/ wiped above).
    ],
    "CLEAN": [
        "glob:data/cleaned/cnps_cleaned.parquet",
    ],
    "INDIVIDUAL_BASE": [
        "glob:data/cleaned/individual_base.parquet",
    ],
    "FIRM_BASE": [
        "glob:data/cleaned/firm_base.parquet",
    ],
    "ANALYTICAL_BASE": [
        "glob:data/cleaned/analytical_base.parquet",
    ],
    "DECLARATION_MODEL": [
        "file:models/declaration_model.pkl",
        "glob:data/cleaned/firm_base_propensity.parquet",
    ],
    "IMPUTATION": [
        "file:models/imputation_model.pkl",
        "glob:data/cleaned/firm_base_imputed.parquet",
    ],
    "WEIGHTING": [
        "glob:data/cleaned/analytical_base_weighted.parquet",
    ],
    "ESTIMATION": [
        # Estimation produces in-memory results consumed by export.
        # No persistent artifact to delete.
    ],
    "VALIDATION": [
        "glob:data/output/rapport_validation.xlsx",
    ],
    "EXPORT": [
        "glob:data/output/indicateurs_cnps.xlsx",
    ],
}

# Always-cleanable artifacts (logs, sessions, caches)
ANCILLARY_ARTIFACTS = [
    "dir:logs",
    "dir:sessions",
    "dir:__pycache__",
    "dir:src/cnps/__pycache__",
]


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _resolve(spec: str) -> list[Path]:
    """Resolve an artifact spec to concrete paths."""
    kind, rel = spec.split(":", 1)
    target = PROJECT_ROOT / rel

    if kind == "dir":
        if target.is_dir():
            return [target]
        return []
    elif kind == "file":
        if target.is_file():
            return [target]
        return []
    elif kind == "glob":
        parent = target.parent
        pattern = target.name
        if parent.is_dir():
            return list(parent.glob(pattern))
        return []
    return []


def _collect_deletions(reset_to: str, include_ancillary: bool = True) -> list[tuple[str, Path]]:
    """
    Collect all paths that should be deleted when resetting to a given stage.

    Returns a list of (description, path) tuples.
    """
    if reset_to == "ORIGIN":
        target_idx = 0
    else:
        target_idx = STAGE_ORDER.index(reset_to)

    deletions: list[tuple[str, Path]] = []

    # Delete artifacts from all stages after the target
    for stage_name in STAGE_ORDER[target_idx + 1:]:
        for spec in STAGE_ARTIFACTS.get(stage_name, []):
            for path in _resolve(spec):
                deletions.append((f"Stage {stage_name}", path))

    # If resetting to ORIGIN, also delete the first stage's artifacts
    if reset_to == "ORIGIN":
        for spec in STAGE_ARTIFACTS.get("INGEST", []):
            for path in _resolve(spec):
                deletions.append(("Stage INGEST", path))

    if include_ancillary:
        for spec in ANCILLARY_ARTIFACTS:
            for path in _resolve(spec):
                deletions.append(("Ancillary (logs/sessions)", path))

    return deletions


def _delete(path: Path) -> int:
    """Delete a path (file or directory contents). Returns count of items removed."""
    if path.is_file():
        path.unlink()
        return 1
    elif path.is_dir():
        count = sum(1 for _ in path.rglob("*") if _.is_file())
        # Remove contents but keep the directory
        for child in list(path.iterdir()):
            if child.is_file():
                child.unlink()
            elif child.is_dir():
                shutil.rmtree(child)
        return count
    return 0


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

app = typer.Typer(
    name="reset",
    help="Reset the CNPS pipeline to its original state or to a specific stage.",
    add_completion=False,
)


@app.command()
def origin(
    dry_run: bool = typer.Option(False, "--dry-run", "-n", help="Preview without deleting"),
    yes: bool = typer.Option(False, "--yes", "-y", help="Skip confirmation"),
    keep_logs: bool = typer.Option(False, "--keep-logs", help="Keep logs and sessions"),
) -> None:
    """Reset to origin: remove ALL generated data, keep only raw data and source code."""
    _do_reset("ORIGIN", dry_run=dry_run, yes=yes, include_ancillary=not keep_logs)


@app.command()
def stage(
    target: str = typer.Argument(..., help="Stage to reset TO (keep its output, delete everything after)"),
    dry_run: bool = typer.Option(False, "--dry-run", "-n", help="Preview without deleting"),
    yes: bool = typer.Option(False, "--yes", "-y", help="Skip confirmation"),
    keep_logs: bool = typer.Option(False, "--keep-logs", help="Keep logs and sessions"),
) -> None:
    """Reset to a specific stage: keep that stage's output, delete all downstream artifacts."""
    target_upper = target.upper()
    if target_upper not in STAGE_ORDER or target_upper == "ORIGIN":
        valid = ", ".join(s for s in STAGE_ORDER if s != "ORIGIN")
        console.print(f"[red]Invalid stage: '{target}'. Valid stages: {valid}[/red]")
        raise typer.Exit(1)
    _do_reset(target_upper, dry_run=dry_run, yes=yes, include_ancillary=not keep_logs)


def _do_reset(
    reset_to: str,
    *,
    dry_run: bool,
    yes: bool,
    include_ancillary: bool,
) -> None:
    """Core reset logic."""
    deletions = _collect_deletions(reset_to, include_ancillary=include_ancillary)

    if not deletions:
        console.print("[green]Nothing to reset — project is already clean.[/green]")
        raise typer.Exit(0)

    # Display what will be deleted
    label = "raw data only" if reset_to == "ORIGIN" else f"after stage {reset_to}"
    action = "Would delete" if dry_run else "Will delete"

    table = Table(title=f"Reset to {label} — {action}:")
    table.add_column("Source", style="cyan", width=25)
    table.add_column("Path", style="white")
    table.add_column("Type", justify="center", width=8)

    total_files = 0
    for desc, path in deletions:
        if path.is_dir():
            n = sum(1 for _ in path.rglob("*") if _.is_file())
            total_files += n
            ptype = f"dir ({n})"
        else:
            total_files += 1
            ptype = "file"
        table.add_row(desc, str(path.relative_to(PROJECT_ROOT)), ptype)

    console.print(table)
    console.print(f"\n[bold]Total: {total_files} file(s) to remove.[/bold]")

    if dry_run:
        console.print("\n[yellow]Dry run — no files were deleted.[/yellow]")
        return

    if not yes:
        confirm = typer.confirm("Proceed with deletion?")
        if not confirm:
            console.print("[yellow]Aborted.[/yellow]")
            raise typer.Exit(0)

    # Execute deletions
    removed = 0
    for desc, path in deletions:
        removed += _delete(path)

    console.print(f"\n[green]Done. Removed {removed} file(s). Project reset to {label}.[/green]")


if __name__ == "__main__":
    app()
