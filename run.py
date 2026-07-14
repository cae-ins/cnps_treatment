#!/usr/bin/env python3
"""
CNPS Treatment Pipeline v2.0 — Point d'entree principal.

Toutes les donnees (entrees et sorties) vivent sur MinIO — voir la section
``minio:`` de ``config/settings.yaml`` pour les prefixes exacts. Aucune
commande ci-dessous ne lit ni n'ecrit sur le disque local, a l'exception
des logs (``logs/pipeline.log``).

Usage — pipeline complet ou par etapes
----------------------------------------
    python run.py run
        Lance les 12 etapes dans l'ordre (01 lecture_fichiers -> 12 export_excel).
        Chaque etape lit la sortie de la precedente sur MinIO : elles sont
        sequentiellement DEPENDANTES, on ne peut pas sauter une etape dont
        la sortie n'existe pas encore.

    python run.py run --from NETTOYAGE_DONNEES --to EXPORT_EXCEL
        Lance seulement les etapes 03 a 12. Necessite que la sortie de
        l'etape 02 (cnps/processed_data/*.parquet) existe deja sur MinIO.
        Noms d'etape valides : LECTURE_FICHIERS, HARMONISATION_TYPES,
        NETTOYAGE_DONNEES, BASE_INDIVIDUS, BASE_ENTREPRISES, BASE_ANALYTIQUE,
        MODELE_DECLARATION, IMPUTATION_SALAIRES, PONDERATION_FINALE,
        ESTIMATION_INDICATEURS, VALIDATION_QUALITE, EXPORT_EXCEL.

Usage — raccourcis (groupes d'etapes courants)
------------------------------------------------
    python run.py ingest
        Etapes 01 + 02. LIT : cnps/fichiers_mensuels/*.xlsx (raw_prefix).
        ECRIT : cnps/processed_data/MM_YYYY.parquet (un par mois traite).
        Independant : peut tourner seul, c'est le point d'entree du pipeline.

    python run.py clean
        Etapes 03 a 06. LIT : cnps/processed_data/*.parquet (sortie de ingest).
        ECRIT : cnps/cleaned_data/{cnps_cleaned,individual_base,firm_base,
        analytical_base}.parquet.
        DEPENDANT : echoue si `ingest` n'a jamais ete lance (aucun parquet
        source a nettoyer).

    python run.py model
        Etapes 07 a 09. LIT : cnps/cleaned_data/{firm_base,analytical_base}.parquet
        (sortie de clean). ECRIT : cnps/cleaned_data/firm_base.parquet (poids
        IPW ajoutes en place), cnps/cleaned_data/firm_base_imputed.parquet,
        cnps/models/{declaration_model,imputation_model}.pkl,
        cnps/cleaned_data/analytical_base.parquet (poids W_FINAL ajoutes en place).
        DEPENDANT : echoue si `clean` n'a jamais ete lance.

    python run.py estimate
        Etapes 10 + 12. LIT : cnps/cleaned_data/analytical_base.parquet
        (avec W_FINAL, donc necessite `model` prealable). ECRIT :
        cnps/output/indicateurs_cnps.xlsx. L'etape 10 elle-meme ne produit
        rien sur MinIO (resultat garde en memoire, transmis directement a
        l'etape 12 dans le meme appel).
        DEPENDANT : echoue si `model` n'a jamais ete lance.

Usage — outils independants (hors sequence 01-12)
-----------------------------------------------------
    python run.py audit
        LIT : cnps/processed_data/*.parquet par defaut (ou --input <prefixe>).
        ECRIT : cnps/output/audit_fichiers_cnps_<horodatage>.xlsx.
        INDEPENDANT de tout run precedent, sauf qu'il lui faut au moins
        des parquets sous processed_data/ (sortie de `ingest`).

    python run.py audit --input cnps/cleaned_data/ --salary-var SALAIRE_BRUT_MENS

    python run.py validate
        LIT : cnps/cleaned_data/cnps_cleaned.parquet, cnps/cleaned_data/firm_base.parquet,
        cnps/models/*.pkl (tout ce qui existe deja). N'ECRIT RIEN — affiche
        le rapport dans le terminal uniquement.
        INDEPENDANT : ne fait que lire ce qui existe deja, ne fait rien
        planter si certains fichiers sont absents (juste des avertissements).

    python run.py config
        Ne lit ni n'ecrit aucune donnee. Affiche la configuration resolue
        (bucket, prefixes MinIO, parametres) depuis config/settings.yaml.
"""

from cnps.cli import app

if __name__ == "__main__":
    app()
