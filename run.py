#!/usr/bin/env python3
"""
Point d'entree du pipeline CNPS v2.

Le DAG de publication comporte 12 etapes effectives, avec une numerotation qui
conserve le saut 08 : 01, 02, 03, 04, 05, 06, 07, 07b, 09, 10, 11 et 12.
Le module 08 d'imputation est un prototype experimental hors publication.

Commandes principales
---------------------
    python run.py run
    python run.py run --from NETTOYAGE_DONNEES --to EXPORT_EXCEL
    python run.py ingest       # 01-02
    python run.py clean        # 03-06
    python run.py model        # 07, 07b, 09
    python run.py estimate     # 10-12, validation obligatoire avant export
    python run.py validate     # controles sur les artefacts existants
    python run.py enrich-anstat
    python run.py audit
    python run.py config

Etapes valides pour --from/--to
-------------------------------
LECTURE_FICHIERS, HARMONISATION_TYPES, NETTOYAGE_DONNEES, BASE_INDIVIDUS,
BASE_ENTREPRISES, BASE_ANALYTIQUE, MODELE_DECLARATION,
MODELE_DECLARATION_INDIV, PONDERATION_FINALE, ESTIMATION_INDICATEURS,
VALIDATION_QUALITE, EXPORT_EXCEL.

Toutes les donnees de production sont lues et ecrites sur MinIO; seuls les
logs sont locaux. Les resumes de modeles sont des JSON non executables. Les
secrets viennent de l'environnement. TLS est obligatoire en production.
"""

from cnps.cli import app

if __name__ == "__main__":
    app()
