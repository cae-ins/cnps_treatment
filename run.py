#!/usr/bin/env python3
"""
CNPS Treatment Pipeline v2.0 — Point d'entree principal.

Usage:
    python run.py                              # Pipeline complet
    python run.py run --from CLEAN --to EXPORT # Stages specifiques
    python run.py ingest                       # Ingestion seule
    python run.py estimate                     # Estimation + export
    python run.py audit                        # Audit qualite (rapport Excel)
    python run.py validate                     # Validation
    python run.py config                       # Voir la configuration
"""

from cnps.cli import app

if __name__ == "__main__":
    app()
