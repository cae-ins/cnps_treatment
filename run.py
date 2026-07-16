#!/usr/bin/env python3
"""
CNPS Treatment Pipeline v2.0 — Point d'entree principal.

Toutes les donnees (entrees et sorties) vivent sur MinIO, reparties sur
plusieurs buckets (architecture medaillon) — voir la section ``minio:``
de ``config/settings.yaml`` pour les buckets/prefixes exacts. Aucune
commande ci-dessous ne lit ni n'ecrit sur le disque local, a l'exception
des logs (``logs/pipeline.log``).

Buckets utilises (voir ``config/settings.yaml``) :
    raw_bucket/raw_prefix            staging/cnps/fichiers_mensuels/  (Excel bruts)
    processed_bucket/processed_prefix silver/cnps/                    (parquets mensuels)
    cleaned_bucket/cleaned_prefix     gold/cnps/                      (bases nettoyees/structurees)
    models_bucket/models_prefix       models/cnps/                    (modeles .pkl)
    output_bucket/output_prefix       staging/cnps/exports_gold/      (exports Excel, sessions)

Usage — pipeline complet ou par etapes
----------------------------------------
    python run.py run
        Lance les 12 etapes dans l'ordre (01 lecture_fichiers -> 12 export_excel).
        Chaque etape lit la sortie de la precedente sur MinIO : elles sont
        sequentiellement DEPENDANTES, on ne peut pas sauter une etape dont
        la sortie n'existe pas encore.

    python run.py run --from NETTOYAGE_DONNEES --to EXPORT_EXCEL
        Lance seulement les etapes 03 a 12. Necessite que la sortie de
        l'etape 02 (bucket processed_bucket/processed_prefix, *.parquet)
        existe deja sur MinIO.
        Noms d'etape valides : LECTURE_FICHIERS, HARMONISATION_TYPES,
        NETTOYAGE_DONNEES, BASE_INDIVIDUS, BASE_ENTREPRISES,
        BASE_ANALYTIQUE, MODELE_DECLARATION, IMPUTATION_SALAIRES,
        PONDERATION_FINALE, ESTIMATION_INDICATEURS, VALIDATION_QUALITE,
        EXPORT_EXCEL.

Usage — raccourcis (groupes d'etapes courants)
------------------------------------------------
    python run.py ingest
        Etapes 01 + 02. LIT : raw_bucket/raw_prefix/*.xlsx.
        ECRIT : processed_bucket/processed_prefix/MM_YYYY.parquet (un par mois traite).
        Independant : peut tourner seul, c'est le point d'entree du pipeline.

    python run.py clean
        Etapes 03 a 06. LIT : processed_bucket/processed_prefix/*.parquet
        (sortie de ingest). ECRIT : cleaned_bucket/cleaned_prefix/
        {cnps_cleaned,individual_base,firm_base,analytical_base}.parquet.
        DEPENDANT : echoue si `ingest` n'a jamais ete lance (aucun parquet
        source a nettoyer).

    python run.py model
        Etapes 07 a 09. LIT : cleaned_bucket/cleaned_prefix/{firm_base,analytical_base}.parquet
        (sortie de clean). ECRIT : cleaned_bucket/cleaned_prefix/firm_base.parquet (poids
        IPW ajoutes en place), cleaned_bucket/cleaned_prefix/firm_base_imputed.parquet,
        models_bucket/models_prefix/{declaration_model,imputation_model}.pkl,
        cleaned_bucket/cleaned_prefix/analytical_base.parquet (poids W_FINAL ajoutes en place).
        DEPENDANT : echoue si `clean` n'a jamais ete lance.

    python run.py estimate
        Etapes 10 + 12. LIT : cleaned_bucket/cleaned_prefix/analytical_base.parquet
        (avec W_FINAL, donc necessite `model` prealable). ECRIT :
        output_bucket/output_prefix/indicateurs_cnps.xlsx. L'etape 10 elle-meme
        ne produit rien sur MinIO (resultat garde en memoire, transmis directement
        a l'etape 12 dans le meme appel).
        DEPENDANT : echoue si `model` n'a jamais ete lance.

Usage — outils independants (hors sequence 01-12)
-----------------------------------------------------
    python run.py enrich-anstat
        LIT : cleaned_bucket/cleaned_prefix/{firm_base,cnps_cleaned}.parquet
        (sortie de `clean`) + raw_bucket/cnps/REQUETES_ANSTAT_MODULE_EMPLOYEURS.xlsx
        (referentiel externe, depose manuellement sur MinIO).
        ECRIT : cleaned_bucket/cleaned_prefix/firm_base.parquet (ecrasement,
        colonnes SECTEUR_ACTIVITE_ANSTAT, FORME_JURIDIQUE_ANSTAT, NUMERO_RCCM,
        NUMERO_DFE ajoutees). Jointure sur RAISON_SOCIALE normalisee (~96.5%
        de correspondance) : n'affecte pas SECTEUR_ACTIVITE (deja quasi complet
        nativement dans les donnees CNPS, ~0.01% de valeurs manquantes) --
        ajoute une nomenclature CEPICI complementaire, pas un correctif.
        HORS SEQUENCE : n'est jamais execute par `run`, `clean`, ou aucun
        autre raccourci. A lancer explicitement, apres `clean`.

    python run.py audit
        LIT : processed_bucket/processed_prefix/*.parquet par defaut
        (ou --input-bucket/--input pour un autre bucket/prefixe).
        ECRIT : output_bucket/output_prefix/audit_fichiers_cnps_<horodatage>.xlsx.
        INDEPENDANT de tout run precedent, sauf qu'il lui faut au moins
        des parquets sous processed_bucket/processed_prefix (sortie de `ingest`).

    python run.py audit --input-bucket gold --input cnps/ --salary-var SALAIRE_BRUT_MENS

    python run.py validate
        LIT : cleaned_bucket/cleaned_prefix/cnps_cleaned.parquet,
        cleaned_bucket/cleaned_prefix/firm_base.parquet,
        models_bucket/models_prefix/*.pkl (tout ce qui existe deja). N'ECRIT RIEN —
        affiche le rapport dans le terminal uniquement.
        INDEPENDANT : ne fait que lire ce qui existe deja, ne fait rien
        planter si certains fichiers sont absents (juste des avertissements).

    python run.py config
        Ne lit ni n'ecrit aucune donnee. Affiche la configuration resolue
        (buckets, prefixes MinIO, parametres) depuis config/settings.yaml.
"""

from cnps.cli import app

if __name__ == "__main__":
    app()
