# CNPS_TREATMENT

## Pipeline de Traitement des Données Salariales CNPS

**Version :** 1.0.0  
**Auteur :** Direction des Statistiques - CNPS Côte d'Ivoire  
**Date :** 2025  

---

## Table des matières

1. [Description du projet](#description-du-projet)
2. [Architecture du projet](#architecture-du-projet)
3. [Prérequis](#prérequis)
4. [Installation](#installation)
5. [Structure des dossiers](#structure-des-dossiers)
6. [Description des scripts](#description-des-scripts)
7. [Flow d'exécution](#flow-dexécution)
8. [Tutoriel d'utilisation](#tutoriel-dutilisation)
9. [Fonctionnalités](#fonctionnalités)
10. [Configuration](#configuration)
11. [Gestion des sessions](#gestion-des-sessions)
12. [Modèles statistiques](#modèles-statistiques)
13. [Outputs et rapports](#outputs-et-rapports)
14. [Dépannage](#dépannage)
15. [Contribuer](#contribuer)

---

## Description du projet

### Objectif

Le projet **CNPS_TREATMENT** est un système analytique intégré, continu et reproductible pour le traitement des données administratives mensuelles de salaires déclarés par les employeurs à la Caisse Nationale de Prévoyance Sociale (CNPS) de Côte d'Ivoire.

### Problématiques adressées

1. **Non-déclaration totale** : Certaines entreprises ne déclarent pas leurs salariés pour certains mois
2. **Déclaration partielle** : Au sein des entreprises déclarantes, tous les salariés ne sont pas systématiquement déclarés

### Approche méthodologique

Le pipeline implémente une correction statistique rigoureuse basée sur :

- **Inverse Probability Weighting (IPW)** : Pondération pour corriger le biais de sélection
- **Imputation multiple** : Génération de M imputations pour les valeurs manquantes
- **Règles de Rubin** : Combinaison des estimations issues des imputations multiples

### Statistiques produites

Pour chaque période et chaque catégorie analytique :

| Statistique | Description | Formule |
|-------------|-------------|---------|
| N_w | Effectif pondéré | Σ w_i |
| Moyenne | Moyenne pondérée | Σ(w_i × Y_i) / Σ w_i |
| Q1, Médiane, Q3 | Quantiles pondérés | Quantiles de la distribution pondérée |
| P10, P90 | Percentiles extrêmes | 10ème et 90ème percentiles |
| Gini | Coefficient d'inégalité | Mesure de dispersion des salaires |

---

## Architecture du projet
```
┌─────────────────────────────────────────────────────────────────────┐
│                         CNPS_TREATMENT                               │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ┌──────────┐    ┌──────────┐    ┌──────────┐    ┌──────────┐      │
│  │  Excel   │───▶│  Stata   │───▶│ Nettoyé  │───▶│ Analytique│      │
│  │  (raw)   │    │  (.dta)  │    │          │    │           │      │
│  └──────────┘    └──────────┘    └──────────┘    └──────────┘      │
│       │                                               │              │
│       │              ┌─────────────────┐              │              │
│       │              │    MODÈLES      │              │              │
│       │              ├─────────────────┤              │              │
│       └─────────────▶│ • Déclaration   │◀─────────────┘              │
│                      │ • Imputation    │                             │
│                      │ • Individuel    │                             │
│                      └────────┬────────┘                             │
│                               │                                      │
│                               ▼                                      │
│                      ┌─────────────────┐                             │
│                      │   ESTIMATION    │                             │
│                      ├─────────────────┤                             │
│                      │ • Pondération   │                             │
│                      │ • Statistiques  │                             │
│                      │ • Agrégation    │                             │
│                      └────────┬────────┘                             │
│                               │                                      │
│                               ▼                                      │
│                      ┌─────────────────┐                             │
│                      │    OUTPUTS      │                             │
│                      ├─────────────────┤                             │
│                      │ • Indicateurs   │                             │
│                      │ • Rapports      │                             │
│                      │ • Diagnostics   │                             │
│                      └─────────────────┘                             │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

---

## Prérequis

### Logiciels requis

| Logiciel | Version minimum | Usage |
|----------|-----------------|-------|
| R | 4.0+ | Traitement principal |
| Python | 3.8+ | Conversion Excel → Stata |
| Stata | 14+ | Format de sortie |

### Packages R requis
```r
# Installation des packages
install.packages(c(
  "haven",       # Lecture/écriture Stata
  "dplyr",       # Manipulation de données
  "tidyr",       # Restructuration de données
  "purrr",       # Programmation fonctionnelle
  "stringr",     # Manipulation de chaînes
  "lubridate",   # Manipulation de dates
  "readxl",      # Lecture Excel
  "openxlsx",    # Écriture Excel
  "pROC",        # Courbes ROC
  "jsonlite",    # Gestion JSON
  "digest",      # Calcul de hash
  "ggplot2"      # Visualisation
))
```

### Packages Python requis
```bash
pip install pandas openpyxl
```

---

## Installation

### 1. Cloner ou télécharger le projet
```bash
git clone https://github.com/cnps-ci/CNPS_TREATMENT.git
```

### 2. Créer la structure des dossiers

Exécuter le script batch (Windows) :
```batch
create_structure.bat
```

Ou exécuter dans R :
```r
source("create_project_structure.R")
```

### 3. Configurer les chemins

Modifier `config/paths.R` :
```r
PROJECT_ROOT <- "C:/Chemin/Vers/Votre/Projet/CNPS_TREATMENT"
```

### 4. Placer les données sources

- Fichiers Excel mensuels dans : `data/raw/excel/`
- Format de nommage : `MM_YYYY.xlsx` (ex: `01_2024.xlsx`)

---

## Structure des dossiers
```
CNPS_TREATMENT/
│
├── config/                          # Configuration
│   ├── config.R                     # Configuration centrale
│   ├── paths.R                      # Définition des chemins
│   ├── dictionaries.R               # Dictionnaires de variables
│   ├── stat_dimensions.R            # Dimensions analytiques
│   ├── stat_definitions.R           # Statistiques à calculer
│   ├── model_specs.R                # Spécifications des modèles
│   ├── estimators.R                 # Fonctions d'estimation
│   └── validation_rules.R           # Règles de validation
│
├── data/                            # Données
│   ├── raw/                         # Données brutes
│   │   └── excel/                   # Fichiers Excel sources
│   ├── processed/                   # Données traitées
│   │   ├── dta/                     # Fichiers Stata convertis
│   │   └── registry/                # Registre de traitement
│   ├── cleaned/                     # Données nettoyées
│   │   ├── individual/              # Base individuelle
│   │   ├── firm_time/               # Base entreprise-temps
│   │   ├── analytical/              # Base analytique
│   │   ├── imputations/             # Données imputées
│   │   ├── monthly/                 # Données mensuelles
│   │   └── final/                   # Données finales
│   └── archive/                     # Archives
│
├── models/                          # Modèles statistiques
│   ├── declaration/                 # Modèle de déclaration
│   │   ├── fitted/                  # Modèles estimés
│   │   ├── diagnostics/             # Diagnostics
│   │   └── summaries/               # Résumés
│   ├── imputation/                  # Modèle d'imputation
│   │   ├── fitted/
│   │   ├── diagnostics/
│   │   └── summaries/
│   └── registry/                    # Registre des modèles
│       └── model_registry.csv
│
├── sessions/                        # Sessions d'exécution
│   ├── registry/                    # Index des sessions
│   │   └── sessions_index.csv
│   ├── active/                      # Session courante
│   │   └── current_session.txt
│   └── <SESSION_ID>/                # Dossier par session
│       ├── config_snapshot/
│       ├── data_snapshot/
│       ├── models/
│       ├── outputs/
│       ├── logs/
│       └── session_metadata.json
│
├── output/                          # Sorties
│   ├── indicators/                  # Indicateurs statistiques
│   ├── analytical_reports/          # Rapports analytiques
│   └── inconsistencies/             # Rapports d'incohérences
│
├── scripts/                         # Scripts R et Python
│   ├── ingestion/                   # Ingestion des données
│   ├── preparation/                 # Préparation des données
│   ├── structuring/                 # Structuration analytique
│   ├── modeling/                    # Modélisation statistique
│   ├── diagnostics/                 # Diagnostics et validation
│   ├── estimation/                  # Estimation des statistiques
│   └── pipeline/                    # Orchestration
│
├── run_pipeline.R                   # Script principal
├── create_structure.bat             # Création des dossiers (Windows)
├── create_project_structure.R       # Création des dossiers (R)
└── README.md                        # Documentation
```

---

## Description des scripts

### Scripts d'ingestion (`scripts/ingestion/`)

| Script | Description |
|--------|-------------|
| `01_excel_to_dta.py` | Convertit les fichiers Excel en format Stata (.dta). Lit toutes les feuilles de chaque fichier Excel, les concatène et sauvegarde en Stata. Gère le traitement incrémental via un registre de fichiers traités. |
| `01_from_excel_to_dta.R` | Wrapper R qui appelle le script Python. Permet l'intégration dans le pipeline R. |

### Scripts de préparation (`scripts/preparation/`)

| Script | Description |
|--------|-------------|
| `01_column_types_matching.R` | Harmonise les types de colonnes selon le dictionnaire défini. Convertit les dates Excel, normalise les chaînes de caractères, assure la cohérence des types numériques. |
| `02_add_mois_annee.R` | Extrait le mois et l'année depuis le nom des fichiers (format `MM_YYYY.dta`) et ajoute les colonnes correspondantes aux données. |
| `03_data_cleaning.R` | Nettoie les données : suppression des valeurs manquantes/nulles, exclusion des types de salariés non pertinents (H, J), application du seuil SMIG, calcul du salaire mensuel brut. |

### Scripts de structuration (`scripts/structuring/`)

| Script | Description |
|--------|-------------|
| `01_create_individual_base.R` | Crée la base individuelle nettoyée avec les variables analytiques : ancienneté, classes d'effectif, identifiants uniques, poids initiaux. |
| `02_create_firm_time_base.R` | Agrège les données au niveau entreprise-mois (B_jt). Calcule les statistiques par entreprise : effectifs, salaire moyen, caractéristiques. Crée le panel complet avec indicateur de déclaration. |
| `03_create_analytical_base.R` | Fusionne les bases individuelles et entreprise-temps. Vérifie la disponibilité des dimensions analytiques. Calcule les poids finaux. |
| `04_concat_databases.R` | Concatène les fichiers mensuels en une base unique. Supporte le mode incrémental (ajout des nouveaux fichiers uniquement). |
| `05_merge_sector_codes.R` | Fusionne les données avec les codes secteurs d'activité depuis un fichier externe de référence. |

### Scripts de modélisation (`scripts/modeling/`)

| Script | Description |
|--------|-------------|
| `01_declaration_model.R` | Estime le modèle de probabilité de déclaration entreprise-mois π_jt. Modèle logit avec variables entreprise et historique. Calcule les poids IPW stabilisés et tronqués. |
| `02_imputation_firm.R` | Impute le salaire moyen pour les entreprises non-déclarantes. Modèle linéaire sur log(salaire moyen). Génère M imputations avec bruit résiduel. |
| `03_individual_model.R` | Estime le modèle de déclaration individuelle conditionnelle ρ_ijt. Calcule les poids individuels pour la déclaration partielle. |
| `04_imputation_individual.R` | Impute les salaires individuels manquants. Modèle linéaire avec caractéristiques individuelles et entreprise. |

### Scripts de diagnostics (`scripts/diagnostics/`)

| Script | Description |
|--------|-------------|
| `01_inconsistency_check.R` | Audit complet des fichiers : doublons, colonnes manquantes, types incohérents, valeurs manquantes, outliers, unicité des ID. Export Excel détaillé. |
| `02_model_diagnostics.R` | Calcule les diagnostics des modèles : AUC, calibration, Brier score pour classification ; R², RMSE, résidus pour régression. Génère des rapports. |
| `03_estimation_validation.R` | Valide les estimations produites : taille des cellules, plausibilité des moyennes, cohérence des quantiles. Compare les résultats entre sessions. |

### Scripts d'estimation (`scripts/estimation/`)

| Script | Description |
|--------|-------------|
| `01_weighted_estimation.R` | Calcule les statistiques salariales pondérées. Applique les poids finaux w_i = w_jt × w_ijt. Combine les résultats des imputations multiples selon les règles de Rubin. |
| `02_calc_indicators.R` | Génère le fichier Excel final des indicateurs. Une feuille par dimension analytique. Inclut métadonnées et paramètres. |

### Scripts de pipeline (`scripts/pipeline/`)

| Script | Description |
|--------|-------------|
| `session_manager.R` | Gère les sessions d'exécution : création, fermeture, rollback, suppression. Chaque session est un snapshot complet et autonome. |
| `model_registry.R` | Registre centralisé des modèles estimés. Permet le suivi, la comparaison et le rechargement des modèles. |

---

## Flow d'exécution

### Diagramme de flux
```
┌─────────────────────────────────────────────────────────────────────┐
│                           RUN_PIPELINE.R                             │
└─────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│ ÉTAPE 1: INITIALISATION                                              │
│ • Création de session                                                │
│ • Initialisation des registres                                       │
│ • Snapshot de la configuration                                       │
└─────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│ ÉTAPE 2: INGESTION                                                   │
│ • Lecture des fichiers Excel (MM_YYYY.xlsx)                         │
│ • Concaténation des feuilles                                        │
│ • Conversion en format Stata (.dta)                                 │
│ • Mise à jour du registre de traitement                             │
└─────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│ ÉTAPE 3: PRÉPARATION                                                 │
│ • Harmonisation des types de colonnes                               │
│ • Ajout des colonnes MOIS/ANNEE                                     │
│ • Contrôle d'incohérences (audit)                                   │
└─────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│ ÉTAPE 4: STRUCTURATION                                               │
│ • Concaténation des bases mensuelles                                │
│ • Nettoyage des données                                             │
│ • Fusion avec codes secteurs                                        │
│ • Création base individuelle (Y_ijt)                                │
│ • Création base entreprise-temps (B_jt)                             │
│ • Création base analytique                                          │
└─────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│ ÉTAPE 5: MODÉLISATION DÉCLARATION ENTREPRISE                        │
│ • Estimation P(D_jt = 1 | Z_jt, H_j,t-1) = π_jt                     │
│ • Modèle logit/probit                                               │
│ • Diagnostics (ROC, AUC, calibration)                               │
│ • Calcul poids IPW : w_jt = 1/π_jt (stabilisé, tronqué)            │
└─────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│ ÉTAPE 6: IMPUTATION ENTREPRISE                                       │
│ • Pour D_jt = 0 : imputer log(Ȳ_jt)                                 │
│ • Modèle : log(Ȳ_jt) = θ + Z'η + H'κ + δ_t + ε_jt                  │
│ • Génération de M imputations                                       │
└─────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│ ÉTAPE 7: MODÉLISATION INDIVIDUELLE                                   │
│ • Estimation P(S_ijt = 1 | R_jt = 1, X_ijt) = ρ_ijt                 │
│ • Calcul poids individuels conditionnels                            │
│ • Imputation salaires individuels (optionnel)                       │
└─────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│ ÉTAPE 8: PONDÉRATION                                                 │
│ • Calcul poids final : w_i = w_jt × w_ijt                           │
│ • Normalisation des poids                                           │
└─────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│ ÉTAPE 9: ESTIMATION                                                  │
│ • Pour chaque imputation m = 1...M :                                │
│   - Calculer statistiques pondérées par dimension                   │
│ • Combiner selon règles de Rubin :                                  │
│   - θ̂ = moyenne des θ̂_m                                            │
│   - Var(θ̂) = Var_intra + (1 + 1/M) × Var_inter                     │
└─────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│ ÉTAPE 10: AGRÉGATION                                                 │
│ • Par dimension analytique (secteur, CSP, sexe, etc.)               │
│ • Par période (mois, année)                                         │
│ • Application des seuils de cellule minimum                         │
└─────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│ ÉTAPE 11: VALIDATION                                                 │
│ • Contrôle de cohérence des estimations                             │
│ • Détection des anomalies                                           │
│ • Suppression des cellules non publiables                           │
└─────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│ ÉTAPE 12: REPORTING                                                  │
│ • Génération fichier Excel des indicateurs                          │
│ • Rapports de diagnostics des modèles                               │
│ • Rapport de validation                                             │
│ • Fermeture de la session                                           │
└─────────────────────────────────────────────────────────────────────┘
```

### Correspondance mathématique ↔ fichiers
```
Y_ijt (salaire individuel brut)
    │
    ▼
┌─────────────────────────────────────┐
│ Base individuelle nettoyée          │  → cleaned/individual/
│ 01_create_individual_base.R         │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ B_jt = (N_jt, n_jt, D_jt, Ȳ_jt, Z_jt)│  → cleaned/firm_time/
│ 02_create_firm_time_base.R          │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ π_jt = P(D_jt = 1)                  │  → models/declaration/
│ 01_declaration_model.R              │
│                                     │
│ w_jt = stabilize(1/π_jt)            │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ log(Ȳ_jt) ~ modèle d'imputation     │  → models/imputation/
│ 02_imputation_firm.R                │
│                                     │
│ M imputations générées              │  → cleaned/imputations/
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ ρ_ijt = P(S_ijt = 1 | D_jt = 1)     │  → models/declaration/
│ 03_individual_model.R               │
│                                     │
│ w_ijt = stabilize(1/ρ_ijt)          │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Y*_ijt (salaire corrigé/imputé)     │  → cleaned/analytical/
│ w_i = w_jt × w_ijt                  │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Statistiques pondérées :            │  → output/indicators/
│ • N_w = Σ w_i                       │
│ • Ȳ_w = Σ(w_i × Y*_i) / Σ w_i       │
│ • Q_w(p) = quantile pondéré         │
│ 01_weighted_estimation.R            │
│ 02_calc_indicators.R                │
└─────────────────────────────────────┘
```

---

## Tutoriel d'utilisation

### Utilisation basique

#### 1. Préparation des données

Placez vos fichiers Excel dans `data/raw/excel/` avec le format de nommage `MM_YYYY.xlsx` :
```
data/raw/excel/
├── 01_2024.xlsx
├── 02_2024.xlsx
├── 03_2024.xlsx
└── ...
```

#### 2. Exécution du pipeline complet
```r
# Définir le chemin du projet
PIPELINE_ROOT <- "C:/Users/e_koffie/Documents/Salaires/CNPS_TREATMENT"

# Charger et exécuter
source(file.path(PIPELINE_ROOT, "run_pipeline.R"))

# Lancer le pipeline complet
run_pipeline()
```

#### 3. Récupérer les résultats

Les résultats sont disponibles dans :
- `output/indicators/` : Fichiers Excel des statistiques
- `output/analytical_reports/` : Rapports d'analyse
- `output/inconsistencies/` : Rapports d'audit

### Utilisation avancée

#### Mode incrémental

Pour traiter uniquement les nouveaux fichiers :
```r
run_incremental()
```

#### Retraitement complet

Pour forcer le retraitement de tous les fichiers :
```r
run_full()

# Ou
run_pipeline(force_reprocess = TRUE)
```

#### Exécution d'étapes spécifiques
```r
# Exécuter seulement certaines étapes
run_pipeline(steps = c("ingestion", "preparation", "structuring"))

# Sans les contrôles d'incohérence
run_pipeline(run_checks = FALSE)
```

#### Gestion des sessions
```r
# Lister les sessions
list_sessions()

# Voir les sessions terminées
list_sessions(status = "completed")

# Rollback vers une session précédente
rollback_session("20250101_120000_abc123")

# Supprimer une session
delete_session("20250101_120000_abc123")

# Comparer deux sessions
compare_sessions("session_id_1", "session_id_2")
```

#### Gestion des modèles
```r
# Lister tous les modèles
list_models()

# Lister les modèles d'un type
list_models(model_name = "declaration_firm")

# Charger un modèle spécifique
model <- load_model("declaration_firm_20250101_120000_abc123")

# Charger les diagnostics d'un modèle
diag <- load_model_diagnostics("declaration_firm_20250101_120000_abc123")

# Obtenir le dernier modèle d'un type
latest_id <- get_latest_model("declaration_firm")
```

### Exemples de personnalisation

#### Ajouter une nouvelle dimension analytique

Dans `config/stat_dimensions.R` :
```r
STAT_DIMENSIONS$region <- list(
  enabled = TRUE,
  variable = "REGION_CODE",
  label = "Région",
  min_cell_size = 50,
  include_in_ipw = TRUE,
  include_in_imputation = TRUE
)
```

#### Ajouter une nouvelle statistique

Dans `config/stat_definitions.R` :
```r
STATISTICS$p95 <- list(
  type = "quantile_weighted",
  p = 0.95,
  label = "P95",
  description = "95ème percentile pondéré",
  combine_imputations = TRUE
)
```

#### Modifier les paramètres de traitement

Dans `config/config.R` :
```r
PROCESSING$n_imputations <- 10        # Nombre d'imputations
PROCESSING$winsor_percentile <- 0.02  # Winsorisation à 2%
PROCESSING$min_salaire_mensuel <- 80000  # Nouveau SMIG
```

---

## Fonctionnalités

### Traitement des données

| Fonctionnalité | Description |
|----------------|-------------|
| Conversion Excel → Stata | Lecture multi-feuilles, concaténation automatique |
| Traitement incrémental | Détection des fichiers nouveaux/modifiés via hash MD5 |
| Harmonisation des types | Conversion automatique selon dictionnaire |
| Détection des dates | Support formats Excel, DD/MM/YYYY, YYYY-MM-DD |
| Nettoyage automatique | Suppression NA, doublons, valeurs aberrantes |
| Winsorisation | Troncature des extrêmes configurable |

### Modélisation statistique

| Fonctionnalité | Description |
|----------------|-------------|
| Modèle de déclaration | Logit/Probit avec historique de déclaration |
| IPW stabilisé | Poids inversés stabilisés et tronqués |
| Imputation multiple | M imputations avec bruit résiduel |
| Règles de Rubin | Combinaison correcte des estimations |
| Diagnostics automatiques | AUC, calibration, R², RMSE |

### Estimation

| Fonctionnalité | Description |
|----------------|-------------|
| Statistiques pondérées | Moyenne, quantiles, Gini pondérés |
| Multi-dimensions | Calcul par secteur, CSP, sexe, etc. |
| Seuils de publication | Suppression automatique des petites cellules |
| Validation | Contrôle de cohérence des résultats |

### Traçabilité

| Fonctionnalité | Description |
|----------------|-------------|
| Sessions | Chaque exécution est une session autonome |
| Snapshots | Configuration et données sauvegardées |
| Registre des modèles | Historique complet des modèles estimés |
| Rollback | Retour à une session précédente |
| Comparaison | Diff entre sessions |

### Reporting

| Fonctionnalité | Description |
|----------------|-------------|
| Excel formaté | Tableaux avec styles, freeze panes |
| Rapports d'audit | Doublons, types, valeurs manquantes |
| Diagnostics modèles | Métriques de qualité |
| Métadonnées | Paramètres et configuration inclus |

---

## Configuration

### Configuration centrale (`config/config.R`)
```r
# Paramètres de traitement
PROCESSING <- list(
  remove_duplicates = TRUE,          # Supprimer les doublons
  duplicate_scope = "file",          # "file" ou "global"
  apply_winsor = TRUE,               # Appliquer la winsorisation
  winsor_percentile = 0.01,          # Percentile de winsorisation
  min_salaire_mensuel = 75000,       # SMIG
  exclude_type_salarie = c("H", "J"), # Types exclus
  n_imputations = 5,                 # Nombre d'imputations
  ipw_trim_quantile = c(0.01, 0.99), # Troncature des poids
  stabilize_weights = TRUE           # Stabiliser les poids IPW
)
```

### Dimensions analytiques (`config/stat_dimensions.R`)
```r
STAT_DIMENSIONS <- list(
  sector = list(
    enabled = TRUE,
    variable = "SECTOR_CODE",
    label = "Secteur d'activité",
    min_cell_size = 30,
    include_in_ipw = TRUE,
    include_in_imputation = TRUE
  ),
  # ...
)
```

### Spécifications des modèles (`config/model_specs.R`)
```r
MODEL_SPECS <- list(
  declaration_firm = list(
    type = "logit",
    covariates = list(
      firm_characteristics = c("SECTOR_CODE", "CLASSE_EFFECTIF_REDUITE"),
      history = c("D_jt_lag1", "D_jt_lag2"),
      time = c("MOIS", "ANNEE")
    ),
    ipw = list(
      stabilize = TRUE,
      trim_quantiles = c(0.01, 0.99)
    )
  ),
  # ...
)
```

### Règles de validation (`config/validation_rules.R`)
```r
VALIDATION_RULES <- list(
  model_diagnostics = list(
    auc_minimum = list(
      metric = "AUC",
      rule = "x >= 0.6",
      message = "AUC trop faible"
    )
  ),
  estimation_output = list(
    cell_size_minimum = list(
      metric = "n_weighted",
      rule = "x >= 30",
      action = "suppress"
    )
  )
)
```

---

## Gestion des sessions

### Concept

Chaque exécution du pipeline crée une **session** autonome qui contient :

- Snapshot de la configuration
- Snapshot des données (optionnel)
- Modèles estimés
- Outputs produits
- Logs d'exécution
- Métadonnées

### Identifiant de session

Format : `YYYYMMDD_HHMMSS_<hash_config>`

Exemple : `20250115_143022_a1b2c3d4`

### Structure d'une session
```
sessions/20250115_143022_a1b2c3d4/
├── config_snapshot/          # Copie de config/
│   ├── config.R
│   ├── paths.R
│   └── ...
├── data_snapshot/            # Données de la session
├── models/                   # Modèles estimés
├── outputs/                  # Résultats produits
├── logs/                     # Journaux d'exécution
└── session_metadata.json     # Métadonnées
```

### Métadonnées de session
```json
{
  "session_id": "20250115_143022_a1b2c3d4",
  "created_at": "2025-01-15 14:30:22",
  "description": "Traitement mensuel janvier 2025",
  "status": "completed",
  "closed_at": "2025-01-15 14:45:33",
  "user": "e_koffie",
  "r_version": "R version 4.3.1"
}
```

---

## Modèles statistiques

### Modèle de déclaration entreprise (π_jt)

**Objectif** : Estimer la probabilité qu'une entreprise j déclare ses salariés au mois t.

**Spécification** :
```
logit(π_jt) = β₀ + Z_jt'β₁ + H_{j,t-1}'β₂ + δ_t
```

Où :
- Z_jt : caractéristiques de l'entreprise (secteur, taille, âge)
- H_{j,t-1} : historique de déclaration (lag1, lag2, % passé)
- δ_t : effets temporels

**Poids IPW** :
```
w_jt = π̄ / π_jt   (stabilisé)
w_jt = max(min(w_jt, q_99), q_01)   (tronqué)
```

### Modèle d'imputation entreprise

**Objectif** : Imputer le salaire moyen pour les entreprises non-déclarantes.

**Spécification** :
```
log(Ȳ_jt) = θ + Z_jt'η + H_{j,t-1}'κ + δ_t + ε_jt
```

**Imputation multiple** :

Pour m = 1, ..., M :
```
log(Ȳ_jt)^(m) = X_jt'β̂ + ε^(m),  ε^(m) ~ N(0, σ̂²)
```

### Modèle individuel conditionnel (ρ_ijt)

**Objectif** : Estimer la probabilité qu'un salarié i soit observé sachant que l'entreprise a déclaré.

**Spécification** :
```
logit(ρ_ijt) = γ₀ + X_ijt'γ₁ + W_jt'γ₂
```

**Probabilité totale d'observation** :
```
P(Y_ijt observé) = π_jt × ρ_ijt
```

**Poids final** :
```
w_i = w_jt × w_ijt
```

---

## Outputs et rapports

### Fichier d'indicateurs (`output/indicators/`)

Format : `stats_salaires_cnps_MM_YYYY-MM_YYYY_YYYYMMDD.xlsx`

**Structure** :
- Une feuille par dimension analytique
- Feuille "Metadata" avec paramètres

**Contenu par feuille** :

| Groupe | Statistique | 2024_01 | 2024_02 | ... |
|--------|-------------|---------|---------|-----|
| Agriculture | N | 15,234 | 15,456 | ... |
| Agriculture | N_weighted | 18,567 | 18,890 | ... |
| Agriculture | Moyenne | 245,678 | 248,901 | ... |
| Agriculture | Q1 | 125,000 | 127,500 | ... |
| ... | ... | ... | ... | ... |

### Rapport d'audit (`output/inconsistencies/`)

Format : `audit_fichiers_cnps_YYYYMMDD_HHMMSS.xlsx`

**Feuilles** :
- Doublons_lignes
- Colonnes
- Types_variables
- Valeurs_manquantes
- Outliers_Salaire
- Unicite_ID

### Rapports de modèles (`models/*/diagnostics/`)

**Fichiers** :
- `<model_id>_diagnostics.rds` : Objet R complet
- `<model_id>_summary.txt` : Résumé textuel

---

## Dépannage

### Erreurs courantes

#### "Fichier externe non trouvé"
```
Erreur : Fichier externe non trouvé: data/raw/CNPS_M_2024.xlsx
```

**Solution** : Vérifier que le fichier de codes secteurs existe et que le chemin dans `config/paths.R` est correct.

#### "Variables manquantes"
```
Erreur : Variables numériques manquantes: SALAIRE_BRUT
```

**Solution** : Vérifier que les fichiers Excel contiennent les colonnes requises définies dans `config/dictionaries.R`.

#### "Aucun fichier .dta trouvé"

**Solution** : Exécuter d'abord l'étape d'ingestion :
```r
source(file.path(PATHS$scripts_ingestion, "01_from_excel_to_dta.R"))
convert_excel_to_dta()
```

#### Erreur Python
```
Le script Python a retourné des erreurs (code: 1)
```

**Solutions** :
1. Vérifier que Python est installé et accessible
2. Vérifier que pandas est installé : `pip install pandas openpyxl`
3. Exécuter le script Python directement pour voir l'erreur détaillée

### Logs et debugging

Activer le mode verbose :
```r
PROCESSING$verbose <- TRUE
```

Consulter les logs de session :
```r
session_id <- get_current_session()
log_dir <- file.path(PATHS$sessions, session_id, "logs")
```

### Réinitialisation

Pour réinitialiser complètement :
```r
# Supprimer le registre de traitement
file.remove(PATHS$registry_file)

# Réinitialiser le registre des modèles
init_model_registry(force_reset = TRUE)

# Relancer le pipeline complet
run_full()
```

---

## Contribuer

### Structure du code

- Chaque script est un module autonome
- Les fonctions principales sont documentées
- La configuration est centralisée dans `config/`

### Conventions

- Nommage : `snake_case` pour fonctions et variables
- Documentation : commentaires en français
- Messages : `message()` pour info, `warning()` pour alertes, `stop()` pour erreurs

### Tests

Avant de soumettre une modification :

1. Exécuter le pipeline complet sur des données de test
2. Vérifier que les diagnostics des modèles sont acceptables
3. Comparer les résultats avec une session précédente

### Contact

Pour toute question ou contribution :
- **Email** : e_koffie@cnps.ci
- **Direction** : Direction des Statistiques - CNPS Côte d'Ivoire

---

## Licence

Ce projet est la propriété de la CNPS Côte d'Ivoire. Tous droits réservés.

---

*Dernière mise à jour : Janvier 2025*