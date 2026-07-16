# CNPS Treatment Pipeline v2.0

Pipeline de traitement statistique des declarations salariales de la **Caisse Nationale de Prevoyance Sociale (CNPS)** — Cote d'Ivoire.

## Objectif

Produire des indicateurs statistiques fiables sur la distribution des salaires a partir des declarations mensuelles des employeurs, en corrigeant le biais de selection induit par la non-declaration.

## Methodologie (resume)

- **Estimation doublement robuste (AIPW)** : combine un modele de propension (logit) et un modele de resultat (OLS log-lineaire) pour une protection contre la mauvaise specification de l'un ou l'autre modele (Bang & Robins, 2005).
- **Imputation multiple** (M=5) avec bootstrap residuel et regles de combinaison de Rubin (1987).
- **Ponderation IPW stabilisee** avec troncature parametrique (Cole & Hernan, 2008).

Voir [docs/methodology.md](docs/methodology.md) pour la note methodologique detaillee avec toutes les references.

## Toutes les donnees vivent sur MinIO — jamais sur disque local

Ce pipeline ne lit et n'ecrit **aucun fichier de donnees en local**. Excel bruts, Parquets intermediaires, modeles `.pkl`, exports Excel finaux : tout transite par un serveur de stockage objet **MinIO**, via des buffers memoire (`io.BytesIO`). Le seul fichier local produit est le journal d'execution (`logs/pipeline.log`).

Consequence pratique : le code (leger, versionne dans Git) peut tourner depuis n'importe quelle machine ayant acces au reseau MinIO — poste local, serveur Jupyter distant, etc. — sans jamais avoir a copier de donnees.

## Installation

```bash
git clone <url-du-depot>
cd CNPS_TREATMENT_V2

# Environnement virtuel (Python 3.11+, prefer un interpreteur natif —
# voir la note Apple Silicon plus bas)
python3 -m venv .venv
source .venv/bin/activate      # Linux/Mac
# .venv\Scripts\activate       # Windows

# Installer le projet et ses dependances
pip install -e .
```

> **Apple Silicon (M1/M2/M3/...)** : si `python3` resout vers un interpreteur x86_64 execute sous Rosetta (verifiable avec `python3 -c "import platform; print(platform.machine())"` — doit afficher `arm64`, pas `x86_64`), Polars peut planter sur les gros volumes (SIGBUS/SIGSEGV). Utiliser un Python natif arm64 (ex: `/opt/homebrew/bin/python3.12`) pour creer le `.venv`.

### Dependances principales

| Package | Usage |
|---------|-------|
| polars | Traitement de donnees (10-100x plus rapide que pandas) |
| minio | Client de stockage objet — lecture/ecriture de toutes les donnees |
| scikit-learn | Modeles de classification et regression |
| scipy | Tests statistiques et distributions |
| typer + rich | Interface en ligne de commande |
| loguru | Logging structure |
| joblib | Parallelisation |
| xlsxwriter | Export Excel formate |
| openpyxl + fastexcel | Lecture des classeurs Excel bruts |
| python-dotenv | Chargement des identifiants MinIO depuis `.env` |

## Configuration

### `.env` — identifiants MinIO (jamais versionnes)

Creer un fichier `.env` a la racine du projet (deja dans `.gitignore`) :

```bash
MINIO_ACCESS_KEY=votre_access_key
MINIO_SECRET_KEY=votre_secret_key
```

Sans ce fichier (ou ces variables d'environnement deja definies), le client utilise `minioadmin` / `minioadmin` par defaut, qui echouera contre un serveur reel.

### `config/settings.yaml` — buckets et prefixes MinIO (organisation medaillon)

Chaque famille de donnees vit dans un **bucket** MinIO distinct, avec son propre **prefixe** (chemin interne au bucket) :

```yaml
minio:
  endpoint: "192.168.1.230:30137"    # adresse du serveur MinIO (reseau interne)

  raw_bucket: "staging"               # Excel bruts (MM_YYYY.xlsx)
  raw_prefix: "cnps/fichiers_mensuels/"

  processed_bucket: "silver"          # Parquets issus de l'ingestion
  processed_prefix: "cnps/"

  cleaned_bucket: "gold"              # donnees nettoyees et bases structurees
  cleaned_prefix: "cnps/"

  models_bucket: "models"             # modeles sauvegardes (.pkl)
  models_prefix: "cnps/"

  output_bucket: "staging"            # exports Excel finaux
  output_prefix: "cnps/exports_gold/"

  secure: false                       # HTTP (true si le serveur exige HTTPS)
```

**Pour changer ou le pipeline lit ses fichiers bruts** : modifier `raw_bucket`/`raw_prefix`.
**Pour changer ou les resultats sont stockes** : modifier le bucket et/ou le prefixe de la famille concernee (`processed_*`, `cleaned_*`, `models_*`, `output_*`) independamment des autres.

> **Attention** : un *alias* `mc` (ex: `datalab` dans `~/.mc/config.json`) n'est **jamais** un bucket — c'est un raccourci local vers un serveur. Ne jamais faire figurer un alias dans `raw_bucket`/`raw_prefix`/etc., seulement le vrai nom du bucket sur le serveur (verifiable avec `mc ls <alias>/` : chaque ligne listee est un bucket).
>
> Le prefixe `raw_prefix` peut contenir d'autres fichiers sans rapport avec le pipeline (CSV, sous-dossiers). Le code ne liste jamais ce prefixe en confiance aveugle : il filtre systematiquement par extension `.xlsx` et par le motif de nom `MM_YYYY.xlsx` (regex `ingestion.filename_regex`).

### `config/dimensions.yaml` — dimensions d'analyse et statistiques

Definit les axes de croisement (secteur, age, sexe, commune...) et les statistiques calculees (moyenne, mediane, Gini...) a l'etape 10.

### Autres parametres notables (`settings.yaml`)

```yaml
modeling:
  n_imputations: 5           # nombre d'imputations multiples (Rubin)
  estimation_method: "aipw"  # "ipw" ou "aipw"
  ipw_trim_lower: 0.01       # troncature des poids (percentiles)
  ipw_trim_upper: 0.99

estimation:
  min_cell_size: 30          # suppression des cellules sous ce seuil pondere
  salary_plausible_range: [75000, 50000000]
```

## Utilisation

### Le fil d'execution : 12 etapes numerotees, dependantes entre elles

Chaque etape lit la sortie MinIO de la precedente et ecrit la sienne. **Elles sont sequentiellement dependantes** : lancer l'etape 5 sans avoir jamais lance l'etape 4 echoue (fichier source introuvable sur MinIO). Rappel des buckets par defaut : `raw`=staging, `processed`=silver, `cleaned`=gold, `models`=models, `output`=staging.

Les numeros d'etape internes (`Stage`, dans `pipeline.py`) sont espaces de 10 en 10 (10, 20, 30...) pour
permettre d'inserer une future etape sans renumeroter les fichiers existants.

| # | Fichier | Fonction | Lit (bucket/prefixe) | Ecrit (bucket/prefixe) |
|---|---------|----------|------------------------|--------------------------|
| 01 | `01_lecture_fichiers.py` | `lire_fichiers` | `raw_bucket/raw_prefix/*.xlsx` (filtre `MM_YYYY.xlsx`) | `processed_bucket/processed_prefix/MM_YYYY.parquet` + `.file_registry.json` |
| 02 | `02_harmonisation_types.py` | `harmoniser_types` | `processed_bucket/processed_prefix/*.parquet` | memes fichiers, types corriges (ecrasement) |
| 03 | `03_nettoyage_donnees.py` | `nettoyer_donnees` | `processed_bucket/processed_prefix/*.parquet` (tous) | `cleaned_bucket/cleaned_prefix/cnps_cleaned.parquet` |
| 04 | `04_base_individus.py` | `construire_base_individus` | `cleaned_bucket/cleaned_prefix/cnps_cleaned.parquet` | `cleaned_bucket/cleaned_prefix/individual_base.parquet` |
| 05 | `05_base_entreprises.py` | `construire_base_entreprises` | `cleaned_bucket/cleaned_prefix/individual_base.parquet` | `cleaned_bucket/cleaned_prefix/firm_base.parquet` |
| 06 | `06_base_analytique.py` | `construire_base_analytique` | `cleaned_bucket/cleaned_prefix/{individual_base,firm_base}.parquet` | `cleaned_bucket/cleaned_prefix/analytical_base.parquet` |
| 07 | `07_modele_declaration.py` | `ajuster_modele_declaration` | `cleaned_bucket/cleaned_prefix/firm_base.parquet` | meme fichier (+ `W_JT`, `P_HAT_JT`) + `models_bucket/models_prefix/declaration_model.pkl` |
| 08 | `08_imputation_salaires.py` | `imputer_salaires` | `cleaned_bucket/cleaned_prefix/firm_base.parquet` | `cleaned_bucket/cleaned_prefix/firm_base_imputed.parquet` + `models_bucket/models_prefix/imputation_model.pkl` |
| 09 | `09_ponderation_finale.py` | `calculer_poids_finaux` | `cleaned_bucket/cleaned_prefix/analytical_base.parquet` | meme fichier (+ `W_FINAL`) |
| 10 | `10_estimation_indicateurs.py` | `estimer_indicateurs` | `cleaned_bucket/cleaned_prefix/analytical_base.parquet` (+ `firm_base_imputed.parquet` si present) | **rien** — DataFrame en memoire |
| 11 | `11_validation_qualite.py` | `valider_tout` | `cleaned_bucket/cleaned_prefix/*.parquet`, `models_bucket/models_prefix/*.pkl` | **rien** — rapport en memoire |
| 12 | `12_export_excel.py` | `exporter_indicateurs` | resultats de l'etape 10 (en memoire) | `output_bucket/output_prefix/indicateurs_cnps.xlsx` |

Hors sequence numerotee (outils a la demande) :

| Fichier | Fonction | Lit | Ecrit |
|---------|----------|-----|-------|
| `05_1_jointure_anstat.py` | `enrichir_avec_anstat` | `cleaned_bucket/cleaned_prefix/{firm_base,cnps_cleaned}.parquet` + `raw_bucket/cnps/REQUETES_ANSTAT_MODULE_EMPLOYEURS.xlsx` (referentiel externe, depose manuellement) | `cleaned_bucket/cleaned_prefix/firm_base.parquet` (ecrasement, colonnes `SECTEUR_ACTIVITE_ANSTAT`, `FORME_JURIDIQUE_ANSTAT`, `NUMERO_RCCM`, `NUMERO_DFE` ajoutees) |
| `audit_qualite.py` | `executer_audit` | `processed_bucket/processed_prefix/*.parquet` (ou bucket/prefixe custom via `--input-bucket`/`--input`) | `output_bucket/output_prefix/audit_fichiers_cnps_<horodatage>.xlsx` |
| `storage.py` | primitives `read_*`/`write_*` (chaque fonction prend un `bucket` explicite) | — | — (utilise par toutes les etapes ci-dessus) |

**Note sur la jointure ANSTAT** : `SECTEUR_ACTIVITE` (nomenclature CNPS) existe deja nativement dans les
donnees CNPS brutes, avec seulement ~0.01% de valeurs manquantes (verifie sur le fichier Excel source).
Cette jointure n'est donc **pas** un correctif de donnees manquantes : elle ajoute une nomenclature
sectorielle complementaire (CEPICI/ANSTAT) ainsi que des identifiants legaux absents cote CNPS (RCCM,
DFE, forme juridique), via un matching approche sur `RAISON_SOCIALE` normalisee (~96.5% de
correspondance). C'est pourquoi elle reste un outil independant, jamais execute automatiquement par
`run`/`clean`.

### Commandes CLI

```bash
# --- Pipeline complet ou par plage d'etapes ---
python run.py run                                          # les 12 etapes
python run.py run --from NETTOYAGE_DONNEES --to EXPORT_EXCEL
python run.py run --verbose                                 # logs detailles

# --- Raccourcis (groupes d'etapes) ---
python run.py ingest      # etapes 01-02 : Excel -> Parquet + harmonisation
python run.py clean       # etapes 03-06 : nettoyage + bases structurees
python run.py model       # etapes 07-09 : declaration + imputation + ponderation
python run.py estimate    # etapes 10+12 : estimation + export Excel

# --- Outils independants ---
python run.py enrich-anstat                                  # enrichissement secteur CEPICI/RCCM/DFE
python run.py audit                                          # 8 controles qualite
python run.py audit --input-bucket gold --input cnps/ --salary-var SALAIRE_BRUT_MENS
python run.py validate                                       # controles donnees/modeles

# --- Configuration ---
python run.py config      # affiche buckets, prefixes, parametres actifs
```

Noms d'etape valides pour `--from`/`--to` : `LECTURE_FICHIERS`, `HARMONISATION_TYPES`, `NETTOYAGE_DONNEES`, `BASE_INDIVIDUS`, `BASE_ENTREPRISES`, `BASE_ANALYTIQUE`, `MODELE_DECLARATION`, `IMPUTATION_SALAIRES`, `PONDERATION_FINALE`, `ESTIMATION_INDICATEURS`, `VALIDATION_QUALITE`, `EXPORT_EXCEL`.

## Structure du projet

Le pipeline est organise en fichiers **a plat**, numerotes dans l'ordre d'execution : ouvrir `src/cnps/` dans un explorateur de fichiers donne directement la sequence du traitement, sans avoir a naviguer dans des sous-dossiers.

```
CNPS_TREATMENT_V2/
|-- config/
|   |-- settings.yaml            # bucket/prefixes MinIO, parametres pipeline
|   |-- dimensions.yaml          # dimensions d'analyse et statistiques
|-- src/cnps/
|   |-- config.py                # chargement de la configuration (YAML + .env)
|   |-- storage.py                # primitives de lecture/ecriture MinIO
|   |-- pipeline.py               # orchestrateur (enchaine les etapes 01-12)
|   |-- cli.py                    # commandes CLI (typer)
|   |-- 01_lecture_fichiers.py    # Excel -> Parquet
|   |-- 02_harmonisation_types.py # types uniformes (dates, numeriques, ID)
|   |-- 03_nettoyage_donnees.py   # concatenation + variables derivees
|   |-- 04_base_individus.py      # vue par salarie
|   |-- 05_base_entreprises.py    # panel entreprise-mois equilibre
|   |-- 05_1_jointure_anstat.py   # outil independant : secteur CEPICI + RCCM/DFE (referentiel ANSTAT)
|   |-- 06_base_analytique.py     # fusion individus + entreprises
|   |-- 07_modele_declaration.py  # score de propension (logit) + poids IPW
|   |-- 08_imputation_salaires.py # imputation multiple (salaires manquants)
|   |-- 09_ponderation_finale.py  # poids final IPW/AIPW
|   |-- 10_estimation_indicateurs.py # estimateurs ponderes + regles de Rubin
|   |-- 11_validation_qualite.py  # controles qualite donnees/modeles/resultats
|   |-- 12_export_excel.py        # export Excel formate
|   |-- audit_qualite.py          # audit qualite a la demande (8 controles)
|-- docs/
|   |-- methodology.md            # note methodologique detaillee (26 references)
|-- logs/                         # logs d'execution (local, non versionne)
|-- run.py                        # point d'entree (python run.py <commande>)
|-- pyproject.toml                # dependances et configuration du paquet
|-- README.md
```

> Les fichiers `NN_nom.py` (commencant par un chiffre) ne sont pas des identifiants Python valides pour un `import` classique. `pipeline.py` et `cli.py` les chargent via `importlib.import_module("cnps.NN_nom")`.

## Sorties finales

| Fichier (sur MinIO, `output_bucket/output_prefix`) | Contenu |
|---------|---------|
| `indicateurs_cnps.xlsx` | Indicateurs par dimension (un onglet par dimension) |
| `rapport_validation.xlsx` | Rapport de validation (si exporte via `exporter_rapport_validation`) |
| `audit_fichiers_cnps_<horodatage>.xlsx` | Rapport d'audit qualite (8 controles) |
| `sessions/{ID}/metadata.json` | Metadonnees de chaque execution du pipeline (duree, statut par etape) |

En local : `logs/pipeline.log` (journal detaille, non versionne).

## References cles

- Robins, Rotnitzky & Zhao (1994). *JASA*, 89(427).
- Bang & Robins (2005). *Biometrics*, 61(4).
- Rubin (1987). *Multiple Imputation for Nonresponse in Surveys.* Wiley.
- Cole & Hernan (2008). *AJE*, 168(6).

Voir [docs/methodology.md](docs/methodology.md) pour la bibliographie complete.
