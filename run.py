#!/usr/bin/env python3
"""
CNPS Treatment Pipeline v2.0 — Point d'entree principal.

Usage:
    python run.py                              # Pipeline complet
    python run.py run --from CLEAN --to EXPORT # Stages specifiques
    python run.py ingest                       # Ingestion seule
    python run.py estimate                     # Estimation + export
    python run.py audit                        # Audit qualite (rapport Excel)
    python run.py audit --input data/cleaned   # Audit sur un dossier specifique
    python run.py validate                     # Validation
    python run.py config                       # Voir la configuration

    # Reset
    python run.py reset origin                 # Reinitialiser (garder raw data uniquement)
    python run.py reset stage CLEAN            # Reinitialiser apres un stage specifique
    python run.py reset origin --dry-run       # Apercu sans suppression
    python run.py reset origin --yes           # Sans confirmation
"""

from cnps.cli import app
from reset import app as reset_app

app.add_typer(reset_app, name="reset", help="Reinitialiser le projet (origin ou stage)")

if __name__ == "__main__":
    app()
