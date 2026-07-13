# CNPS Treatment Pipeline v2.0

Pipeline de traitement statistique des declarations salariales de la **Caisse Nationale de Prevoyance Sociale (CNPS)** — Cote d'Ivoire.

## Objectif

Produire des indicateurs statistiques fiables sur la distribution des salaires a partir des declarations mensuelles des employeurs, en corrigeant le biais de selection induit par la non-declaration.

## Methodologie

- **Estimation doublement robuste (AIPW)** : combine un modele de propension (logit) et un modele de resultat (OLS log-lineaire) pour une protection contre la mauvaise specification de l'un ou l'autre modele (Bang & Robins, 2005).
- **Imputation multiple** (M=5) avec bootstrap residuel et regles de combinaison de Rubin (1987).
- **Ponderation IPW stabilisee** avec troncature parametrique (Cole & Hernan, 2008).

Voir [docs/methodology.md](docs/methodology.md) pour la note methodologique detaillee avec toutes les references.

## Installation

```bash
# Cloner le projet
cd c:/Users/e_koffie/Documents/Salaires/CNPS_TREATMENT_V2

# Creer un environnement virtuel
python -m venv .venv
.venv/Scripts/activate      # Windows
# source .venv/bin/activate  # Linux/Mac

# Installer les dependances
pip install -e .
```

### Dependances principales

| Package | Usage |
|---------|-------|
| polars | Traitement de donnees (10-100x plus rapide que pandas) |
| scikit-learn | Modeles de classification et regression |
| statsmodels | Modeles econometriques |
| scipy | Tests statistiques et distributions |
| typer + rich | Interface en ligne de commande |
| loguru | Logging structure |
| joblib | Parallelisation |
| xlsxwriter | Export Excel formate |
| minio | Synchronisation des donnees avec le stockage objet MinIO |

## Configuration

Toute la configuration est centralisee dans deux fichiers YAML :

| Fichier | Contenu |
|---------|---------|
| `config/settings.yaml` | Chemins, parametres de nettoyage, modelisation, parallelisation |
| `config/dimensions.yaml` | Dimensions analytiques et statistiques a calculer |

### Modifier les chemins

Dans `config/settings.yaml`, section `paths` :

```yaml
paths:
  project_root: "c:/Users/e_koffie/Documents/Salaires/CNPS_TREATMENT_V2"
  raw_data: "${project_root}/data/raw"
  # ... autres chemins
```

### Modifier les parametres

```yaml
modeling:
  n_imputations: 5          # Nombre d'imputations (Rubin)
  estimation_method: "aipw"  # "ipw", "aipw", ou "tmle"
  ipw_trim_lower: 0.01      # Troncature des poids
  ipw_trim_upper: 0.99

cleaning:
  min_salary: 75000          # Seuil minimum salaire mensuel (FCFA)
  winsor_lower: 0.01         # Winsorisation (percentiles)
  winsor_upper: 0.99
```

## Stockage objet MinIO

Les fichiers de donnees (Excel bruts, Parquet nettoye) ne sont **pas** versionnes dans Git : ils sont trop volumineux (plusieurs Go) et vivent a la place sur un serveur **MinIO** interne. Le code, lui, est le seul element qui transite par GitHub — ce qui permet de le recuperer facilement sur n'importe quelle machine (y compris un serveur Jupyter distant plus puissant) sans avoir a deplacer les donnees.

### Fonctionnement

Le pipeline se synchronise automatiquement avec MinIO a deux moments precis :

| Etape | Module | Sens | Description |
|-------|--------|------|-------------|
| Avant `INGEST` | `src/cnps/ingestion/excel_reader.py` | MinIO -> local | Telecharge les fichiers Excel manquants ou modifies depuis le bucket vers `data/raw/` |
| Apres `CLEAN` | `src/cnps/preparation/cleaner.py` | local -> MinIO | Envoie `data/cleaned/cnps_cleaned.parquet` vers le bucket |

Cette logique est centralisee dans `src/cnps/storage/minio_client.py` (fonctions `download_raw_data` et `upload_cleaned_data`). Si le serveur MinIO n'est pas joignable (hors du reseau interne, VPN non connecte, etc.), le pipeline **n'echoue pas** : il affiche un avertissement dans les logs et continue avec les fichiers locaux deja presents.

### Configuration

Les parametres de connexion (adresse du serveur, bucket, prefixes) sont dans `config/settings.yaml`, section `minio` :

```yaml
minio:
  endpoint: "192.168.1.230:31157"
  bucket: "admindataanstat"
  raw_prefix: "CNPS/raw_data/"
  cleaned_prefix: "CNPS/cleaned_data/"
  secure: false
```

Les **identifiants** (access key / secret key) ne sont volontairement **jamais** places dans ce fichier versionne. Ils doivent etre definis via des variables d'environnement avant de lancer le pipeline :

```bash
export MINIO_ACCESS_KEY="votre_access_key"
export MINIO_SECRET_KEY="votre_secret_key"
```

Sans ces variables, le client utilise `minioadmin` / `minioadmin` par defaut.

> Le serveur MinIO tourne sur le reseau interne (`192.168.1.230`) : il faut etre connecte a ce reseau (ou au VPN correspondant) pour que la synchronisation fonctionne. Sur le serveur Jupyter distant, s'assurer que les variables d'environnement `MINIO_ACCESS_KEY` / `MINIO_SECRET_KEY` y sont aussi definies.

## Utilisation

### Donnees d'entree

Placer les fichiers Excel dans `data/raw/` au format `MM_YYYY.xlsx` :
```
data/raw/
  01_2024.xlsx
  02_2024.xlsx
  ...
  11_2025.xlsx
```

### Commandes

```bash
# Pipeline complet (ingestion -> export)
python run.py run

# Avec mode verbose
python run.py run --verbose

# Stages specifiques
python run.py run --from CLEAN --to ESTIMATION

# Ingestion seule (Excel -> Parquet)
python run.py ingest

# Nettoyage et structuration
python run.py clean

# Modelisation (declaration + imputation + ponderation)
python run.py model

# Estimation et export Excel
python run.py estimate

# Audit qualite des donnees (rapport Excel avec 6 controles)
python run.py audit

# Audit sur un dossier specifique avec variables personnalisees
python run.py audit --input data/cleaned --salary-var SALAIRE_BRUT_MENS --id-var ID_INDIV

# Validation (donnees + modeles + resultats)
python run.py validate

# Voir la configuration active
python run.py config

# --- Reset / Reinitialisation ---

# Reinitialiser completement (ne garder que les donnees brutes)
python run.py reset origin

# Apercu de ce qui sera supprime (sans rien supprimer)
python run.py reset origin --dry-run

# Sans demande de confirmation
python run.py reset origin --yes

# Garder les logs et sessions lors du reset
python run.py reset origin --keep-logs

# Reinitialiser a un stage specifique (supprime tout en aval)
python run.py reset stage INGEST         # Garder processed/, supprimer le reste
python run.py reset stage CLEAN          # Garder cleaned base, supprimer structuration+
python run.py reset stage ANALYTICAL_BASE # Garder les bases, supprimer modeles+
python run.py reset stage WEIGHTING      # Garder modeles, supprimer export
python run.py reset stage CLEAN --dry-run # Apercu sans suppression
```

### Niveaux de reset

| Reset vers | Conserve | Supprime |
|------------|----------|----------|
| `origin` | `data/raw/`, code source, config | `data/processed/`, `data/cleaned/`, `data/output/`, `models/`, `logs/`, `sessions/` |
| `INGEST` | + parquets ingeres | cleaned, modeles, output |
| `CLEAN` | + base nettoyee | bases structurees, modeles, output |
| `INDIVIDUAL_BASE` | + base individuelle | firm_base, analytical_base, modeles, output |
| `FIRM_BASE` | + panel entreprise-mois | analytical_base, modeles, output |
| `ANALYTICAL_BASE` | + toutes les bases structurees | modeles, poids, output |
| `DECLARATION_MODEL` | + modele de propension | modele d'imputation, estimation, export |
| `IMPUTATION` | + modele d'imputation | poids finaux, estimation, export |
| `WEIGHTING` | + poids finaux (`W_FINAL`) | estimation, export |

> ⚠️ **Incoherence connue** : `reset.py` cible des fichiers `firm_base_propensity.parquet` et `analytical_base_weighted.parquet` pour les stages `DECLARATION_MODEL` et `WEIGHTING`. Ces fichiers ne sont **jamais crees** par le pipeline reel : `declaration_model.py` et `weighting.py` ecrivent leurs resultats **directement dans** `firm_base.parquet` et `analytical_base.parquet` (colonnes ajoutees en place). Consequence : `python run.py reset stage DECLARATION_MODEL` et `reset stage WEIGHTING` ne suppriment pas reellement les colonnes `W_JT`/`P_HAT_JT`/`W_FINAL` deja calculees — seuls les `.pkl` de `models/` sont retires. A corriger dans `reset.py` avant de se fier a ces deux niveaux de reset.

### Stages du pipeline

```
 data/raw/*.xlsx
 (01_2024.xlsx, 02_2024.xlsx, ...)
       |
       |  [1. INGEST]  ingestion/excel_reader.py -> ingest()
       v
 data/processed/MM_YYYY.parquet
 (01_2024.parquet, 02_2024.parquet, ...)
       |
       |  [2. HARMONIZE]  preparation/type_harmonizer.py -> harmonize_types()
       v
 data/processed/*.parquet  (types corriges en place)
       |
       |  [3. CLEAN]  preparation/cleaner.py -> clean()
       v
 data/cleaned/cnps_cleaned.parquet
       |
       +------------------------------------------+
       |                                          |
       |  [4. INDIVIDUAL_BASE]                    |
       |  structuring/individual_base.py          |
       |  -> build_individual_base()              |
       v                                          |
 data/cleaned/individual_base.parquet             |
       |                                          |
       |  [5. FIRM_BASE]                          |
       |  structuring/firm_base.py                |
       |  -> build_firm_base()                    |
       v                                          |
 data/cleaned/firm_base.parquet                   |
       |                                          |
       +------------------+-----------------------+
       |                  |
       |  [6. ANALYTICAL_BASE]
       |  structuring/analytical_base.py
       |  -> build_analytical_base()
       v
 data/cleaned/analytical_base.parquet
       |
       |            +-- data/cleaned/firm_base.parquet
       |            |
       |            |  [7. DECLARATION_MODEL]
       |            |  modeling/declaration_model.py
       |            |  -> fit_declaration_model()
       |            v
       |     data/cleaned/firm_base.parquet  (+ W_JT, P_HAT_JT)
       |     models/declaration_model.pkl
       |            |
       |            |  [8. IMPUTATION]
       |            |  modeling/imputation.py
       |            |  -> impute_firm_salaries()
       |            v
       |     data/cleaned/firm_base_imputed.parquet
       |     models/imputation_model.pkl
       |
       |  [9. WEIGHTING]
       |  modeling/weighting.py
       |  -> compute_final_weights()
       v
 data/cleaned/analytical_base.parquet  (+ W_FINAL)
       |
       |  [10. ESTIMATION]
       |  estimation/estimator.py -> estimate_all()
       v
 (DataFrame en memoire : indicateurs par dimension)
       |
       +------------------------------------------+
       |                                          |
       |  [11. VALIDATION]                        |  [12. EXPORT]
       |  diagnostics/validation.py               |  export/excel_export.py
       |  -> run_all_validations()                |  -> export_indicators()
       v                                          v
 data/output/rapport_validation.xlsx     data/output/indicateurs_cnps.xlsx
```

## Structure du projet

```
CNPS_TREATMENT_V2/
|-- config/
|   |-- settings.yaml          # Configuration principale
|   |-- dimensions.yaml        # Dimensions et statistiques
|-- src/cnps/
|   |-- __init__.py
|   |-- config.py              # Chargement configuration
|   |-- cli.py                 # Interface ligne de commande
|   |-- pipeline.py            # Orchestrateur du pipeline
|   |-- ingestion/
|   |   |-- excel_reader.py    # Excel -> Parquet
|   |-- preparation/
|   |   |-- type_harmonizer.py # Harmonisation des types
|   |   |-- cleaner.py         # Nettoyage et enrichissement
|   |-- structuring/
|   |   |-- individual_base.py # Base individuelle
|   |   |-- firm_base.py       # Panel entreprise-mois
|   |   |-- analytical_base.py # Base analytique fusionnee
|   |-- modeling/
|   |   |-- declaration_model.py  # Logit (propension)
|   |   |-- imputation.py        # Imputation multiple
|   |   |-- weighting.py         # IPW / AIPW
|   |-- estimation/
|   |   |-- weighted_stats.py     # Estimateurs ponderes
|   |   |-- confidence_intervals.py # Regles de Rubin
|   |   |-- estimator.py         # Moteur d'estimation
|   |-- diagnostics/
|   |   |-- validation.py        # Controles qualite
|   |-- export/
|   |   |-- excel_export.py      # Export Excel formate
|   |-- storage/
|       |-- minio_client.py      # Synchronisation avec MinIO
|-- data/
|   |-- raw/                   # Fichiers Excel source
|   |-- processed/             # Parquet intermediaires
|   |-- cleaned/               # Donnees nettoyees
|   |-- output/                # Indicateurs finaux
|-- docs/
|   |-- methodology.md         # Note methodologique detaillee
|-- models/                    # Modeles sauvegardes (.pkl)
|-- logs/                      # Logs d'execution
|-- sessions/                  # Historique des sessions
|-- run.py                     # Point d'entree
|-- reset.py                   # Reinitialisation du projet
|-- pyproject.toml             # Dependances et configuration
|-- README.md
```

## Sorties

| Fichier | Contenu |
|---------|---------|
| `data/output/indicateurs_cnps.xlsx` | Indicateurs par dimension (un onglet par dimension) |
| `data/output/rapport_validation.xlsx` | Rapport de validation |
| `logs/pipeline.log` | Log detaille de l'execution |
| `sessions/{ID}/metadata.json` | Metadata de session |

## Differences avec la v1

| Aspect | v1 (R) | v2 (Python) |
|--------|--------|-------------|
| Langage | R + dplyr | Python + Polars |
| Format intermediaire | Stata .dta | Parquet (zstd) |
| Performance | ~45s / 1M lignes | ~3s / 1M lignes |
| Estimation | IPW pur | AIPW (doublement robuste) |
| Configuration | Scripts R | YAML versionnable |
| Interface | source() dans R | CLI avec Typer |
| Parallelisation | Non | Joblib + Polars multi-thread |
| Logging | print() | Loguru structure |

## References cles

- Robins, Rotnitzky & Zhao (1994). *JASA*, 89(427).
- Bang & Robins (2005). *Biometrics*, 61(4).
- Rubin (1987). *Multiple Imputation for Nonresponse in Surveys.* Wiley.
- Cole & Hernan (2008). *AJE*, 168(6).

Voir [docs/methodology.md](docs/methodology.md) pour la bibliographie complete (26 references).
