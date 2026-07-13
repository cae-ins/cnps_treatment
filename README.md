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
| `ANALYTICAL_BASE` | + toutes les bases structurees | modeles, poids, output |
| `WEIGHTING` | + modeles et poids | estimation, export |

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
|       |-- excel_export.py      # Export Excel formate
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
