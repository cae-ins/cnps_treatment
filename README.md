# CNPS_TREATMENT

**But du projet**

CNPS_TREATMENT est un pipeline R/Python pour traiter, auditer et calculer des indicateurs à partir des fichiers salariaux CNPS (Excel → Stata → nettoyage → indicateurs). Le pipeline est orchestré par `run_pipeline.R` et organisé en étapes réutilisables et incrémentales.

---

## Prérequis

- R (>= 4.0) avec les packages listés en début des scripts (ex.: `dplyr`, `readr`, `haven`, `openxlsx`, `lubridate`, ...)
- Python (pour la conversion Excel → DTA) — le script Python appelé se trouve dans `scripts/01_excel_to_dta.py`.
- OS: les chemins par défaut utilisent des chemins Windows (modifiable dans `config/paths.R`).

---

## Structure importante

- `config/`
  - `config.R` : paramètres globaux, fonctions utilitaires et validation de la config.
  - `paths.R`  : chemins centralisés (`PATHS`) et fichiers externes (`EXTERNAL_FILES`).
- `data/`
  - `raw/excel/` : fichiers Excel d'entrée (nommage attendu `MM_YYYY.xlsx`).
  - `processed/dta/` : fichiers `.dta` générés par la conversion.
  - `cleaned/monthly/` et `cleaned/final/` : données nettoyées.
- `scripts/` : étapes du pipeline (R scripts orchestrés par `run_pipeline.R`).
- `output/` : `inconsistencies/` (rapports d'audit) et `results/` (résultats indicateurs).
- `data/processed/registry/processed_index.csv` : registry qui trace le statut des fichiers.

---

## Flux principal (expliqué)

Le fichier `run_pipeline.R` orchestre ces étapes (par défaut `run_pipeline()` exécute toutes les étapes) :

1. `init` : création des dossiers et initialisation du `registry` (scripts/`00_init_project.R`).
2. `excel_to_dta` : appelle le wrapper R qui lance le script Python `scripts/01_excel_to_dta.py` pour convertir les Excel en `.dta`.
3. `check_1` : premier audit d'incohérences sur les fichiers bruts.
4. `types_matching` : harmonisation des types de variables (`02_column_types_matching.R`).
5. `check_2` : second audit (post-typage).
6. `add_mois_annee` : ajout des colonnes `MOIS`/`ANNEE` (`03_add_mois_annee.R`).
7. `concat` : concaténation des fichiers mensuels (`04_concat_databases.R`).
8. `cleaning` : nettoyage des données (`data_cleaning.R`).
9. `merge_sectors` : fusion avec les codes secteurs (`merge_sector_cod.R`).
10. `indicators` : calcul des indicateurs finaux (`05_calc_indicators.R`).

Chaque étape met à jour le `registry` pour tracer le traitement des fichiers (hash, status, date, etc.).

---

## Schéma du process global (inputs → scripts → outputs)

Voici un schéma explicite du flux de données et des scripts responsables à chaque étape.

```
INPUTS
  data/raw/excel/*.xlsx  (naming: MM_YYYY.xlsx)
    |
    v
  scripts/01_from_excel_to_dta.R  (R wrapper)
    -> appelle `python scripts/01_excel_to_dta.py` via `system2()`
    |
    v
  data/processed/dta/*.dta  (fichiers Stata générés)
    |
    v
  scripts/02_column_types_matching.R  (harmonisation des types)
    |
    v
  scripts/03_add_mois_annee.R  (ajout des colonnes MOIS/ANNEE)
    |
    v
  scripts/04_concat_databases.R  (concaténation, génération des bases mensuelles)
    |
    v
  scripts/data_cleaning.R  (nettoyage, winsorisation, filtres)
    |
    v
  scripts/merge_sector_cod.R  (fusion avec codes secteurs externes)
    |
    v
  scripts/05_calc_indicators.R  (calcul des indicateurs finaux)
    |
    v
  OUTPUTS
    - data/cleaned/monthly/     (bases nettoyées par mois)
    - data/cleaned/final/       (base finale consolidée)
    - output/inconsistencies/   (rapports d'audit Excel, logs d'erreurs)
    - output/results/           (fichiers de résultats / indicateurs)

ADDITIONAL
  - PATHS$registry_file (data/processed/registry/processed_index.csv) est mis à jour
    par `update_registry()` à chaque étape importante (hash, status, steps completed)
  - Les fichiers remplacés peuvent être archivés automatiquement dans `data/archive/`
    si `PROCESSING$archive_replaced_files = TRUE`.
  - Les audits (pré/post typage) sont produits par `scripts/inconsistency_check.R`
    et exportés dans `output/inconsistencies/`.

Notes:
  - `run_pipeline()` (dans `run_pipeline.R`) orchestre le pipeline : il `source()` les
    scripts R listés et appelle leurs fonctions (par ex. `convert_excel_to_dta()`).
  - Le seul appel à Python est la conversion Excel → Stata; le reste est natif R.
  - Pour un traitement incrémental, `run_incremental()` vérifie `get_files_to_process()`
    et ne traite que les nouveaux/fichiers modifiés.

---

## Commandes d'exécution

Lancer le pipeline complet (interactif) :

```r
# Depuis R interactif
source('run_pipeline.R')
# Le script appelle run_pipeline() par défaut
```

Exécuter uniquement certaines étapes :

```r
# Exemple: exécuter uniquement init + excel_to_dta
run_pipeline(steps = c('init', 'excel_to_dta'))

# Forcer retraitement de toutes les étapes
run_pipeline(force_reprocess = TRUE)
```

Mode incrémental :

```r
# Lancer le mode incrémental (vérifie nouveaux/fichiers modifiés et traite uniquement ceux-ci)
run_incremental()
```

Appeler le wrapper Python manuellement (si besoin) :

```powershell
# PowerShell
python scripts/01_excel_to_dta.py "C:/chemin/vers/PROJECT_ROOT" --force
```

> Remarque: `01_from_excel_to_dta.R` est un wrapper qui exécute le script Python via `system2()`.

---

## Configuration

- Modifier `config/paths.R` pour adapter `PROJECT_ROOT` et les chemins locaux.
- `config/config.R` contient les paramètres de traitement (seuils, options de déduplication, audits, version Stata, etc.).
- Les fichiers externes pour merges (codes secteur, communes) sont définis dans `config/paths.R` → `EXTERNAL_FILES`.

---

## Registry & suivi

- Le `registry` (fichier CSV `data/processed/registry/processed_index.csv`) contient : nom de fichier source, hash, date traitement, étape complétée, status, période, taille etc.
- Fonctions utiles : `get_files_to_process()` (liste nouveaux / modifiés / à jour), `update_registry()`.

---

## Dépannage (troubleshooting)

- `Aucun fichier Excel trouvé`: vérifier que `PATHS$raw_excel` contient les `.xlsx` corrects et qu'ils ne commencent pas par `~$`.
- Erreurs Python lors de la conversion: exécuter `python scripts/01_excel_to_dta.py` manuellement et vérifier l'environnement Python et les dépendances.
- Fichiers manquants référencés dans `EXTERNAL_FILES`: corriger les chemins ou placer les fichiers attendus.
- Pour forcer reprise: `run_pipeline(force_reprocess = TRUE)` ou supprimer l'entrée correspondante du registry.

---

## Bonnes pratiques

- Garder un historique : `PROCESSING$archive_replaced_files = TRUE` archivera automatiquement les fichiers remplacés.
- Respecter le nommage des fichiers d'entrée (`MM_YYYY.xlsx`), utilisé pour extraire `MOIS`/`ANNEE`.
- Exécuter les contrôles d'incohérence (`run_inconsistency_check()`) avant et après le typage.

---

_Petit rappel: adaptez `PROJECT_ROOT` dans `config/paths.R` si vous bougez le projet vers un autre chemin._

© CNPS_TREATMENT
