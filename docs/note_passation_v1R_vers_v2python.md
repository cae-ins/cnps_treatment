# Note de passation — Pipeline CNPS : de la v1 R à la v2 Python

**Dépôt** : `github.com/cae-ins/cnps_treatment`
**Rédigée le** : 1er août 2026
**Objet** : documenter la transition entre la version R du pipeline (branche `master`, figée au
4 janvier 2026) et la version Python en cours de développement (branche `refacto-cnps-minio`,
dernier commit le 31 juillet 2026).

---

## 1. À qui s'adresse cette note

À toute personne qui connaît le pipeline R et doit reprendre le travail sur la version active, ou
qui doit interpréter des résultats produits par l'une ou l'autre version.

**Avertissement préalable** : les deux versions n'ont **aucun ancêtre commun dans Git**. Aucun
`git log`, `git diff` ou `git blame` ne reliera jamais la v1 à la v2. Cette note est le seul pont
documentaire entre les deux.

### État du dépôt distant

Le dépôt héberge cinq branches, réparties en **quatre lignées d'historique disjointes** —
l'historique a été réinitialisé trois fois.

| Branche | Commits | Dernier commit | Auteur | Statut |
|---|---|---|---|---|
| `refacto-cnps-minio` | 71 | 31/07/2026 | jauresmanouan | **Active — version Python** |
| `traitement-cnps` | 11 | 14/07/2026 | jauresmanouan | Ancêtre de la précédente (60 commits de retard) |
| `main` ⭐ *branche par défaut* | 2 | 22/03/2026 | CAE - ANStat CI | Dormante — version R |
| `master` | 1 | 04/01/2026 | fmigone | Dormante — version R (commit orphelin) |
| `feat_data_from_minio` | 8 | 25/12/2025 | fmigone | Lignée originelle (Ezechiel KOFFIE) |

Points d'attention pratiques :

- `main` reste la **branche par défaut** du dépôt alors que le travail réel est sur
  `refacto-cnps-minio`. Son dernier commit est un *« Update print statement from 'Hello' to
  'Goodbye' »* du 22/03/2026, qui ressemble à un commit de test.
- Un clone local positionné sur `master` ne voit que deux des cinq branches. Un `git fetch --prune`
  est nécessaire pour récupérer les autres.
- Un `main` local hérité de l'ancienne lignée n'a **aucun ancêtre commun** avec `origin/main` : un
  simple `fetch` ne le réalignera pas.

---

## 2. Correspondance étape par étape

Le portage reprend fidèlement la découpe du pipeline R. Les 16 étapes de `run_pipeline.R` deviennent
12 fichiers numérotés dans `src/cnps/`, plus des outils hors séquence.

| Pipeline R (v1) | Python (v2, `src/cnps/`) |
|---|---|
| `ingestion/01_from_excel_to_dta.R` | `01_lecture_fichiers.py` — Excel → **Parquet** (plus de `.dta`) |
| `preparation/01_column_types_matching.R` | `02_harmonisation_types.py` |
| `preparation/02_add_mois_annee.R` + `03_data_cleaning.R` + `structuring/04_concat_databases.R` | `03_nettoyage_donnees.py` (les trois fusionnées) |
| `structuring/01_create_individual_base.R` | `04_base_individus.py` |
| `structuring/02_create_firm_time_base.R` | `05_base_entreprises.py` |
| `structuring/03_create_analytical_base.R` | `06_base_analytique.py` |
| `structuring/05_merge_references.R`, `05_merge_sector_codes.R` | `05_1_jointure_anstat.py` — **sorti de la séquence**, outil à la demande |
| `modeling/01_declaration_model.R` | `07_modele_declaration.py` |
| `modeling/03_individual_model.R` | `07b_modele_declaration_indiv.py` |
| `modeling/02_imputation_firm.R` | `08_imputation_salaires.py` |
| `modeling/04_imputation_individual.R` | **aucun équivalent** — voir §6 |
| `estimation/01_weighted_estimation.R` | `09_ponderation_finale.py` |
| `estimation/02_calc_indicators.R`, `03_calc_indicators_with_ci.R` | `10_estimation_indicateurs.py` |
| `diagnostics/01` à `03` | `11_validation_qualite.py` + `audit.py` |
| (copie manuelle en fin de `run_pipeline.R`) | `12_export_excel.py` — étape dédiée |
| `pipeline/session_manager.R`, `model_registry.R` | `pipeline.py` (métadonnées de session sur MinIO) |
| `scripts_2/*.R` (7 analyses ad hoc du 30/12/2025) | absorbées dans `audit.py` et 3 notebooks |

Les numéros d'étape internes sont espacés de 10 en 10 dans `pipeline.py`, et le motif de découverte
accepte un suffixe alphabétique (`07b`), pour permettre d'insérer une étape sans renuméroter.

---

## 3. Ce qui est conservé à l'identique

- **La problématique** : correction du biais de sélection induit par la non-déclaration des
  employeurs.
- **Le modèle de déclaration entreprise-mois** : régression logistique sur secteur, classe de
  taille, classe d'âge, déclaration retardée et taux de déclaration passé.
- **L'IPW stabilisé** : `w_jt = P(D=1) / p̂_jt`, avec troncature aux percentiles 1 % et 99 %.
  Implémentation R dans `config/estimators.R:87-103` (`compute_ipw_weight`), Python dans
  `07_modele_declaration.py` (`ajuster_modele_declaration`) et `09_ponderation_finale.py`
  (`_compute_ipw_weights`).
- **L'imputation multiple** M = 5 sur `log(salaire)` avec bootstrap résiduel.
- **Les règles de combinaison de Rubin** pour agréger les M estimations.
- **La batterie de statistiques** : effectif pondéré, moyenne, quantiles (Q1, médiane, Q3, P10,
  P90), Gini.
- **Le seuil de secret statistique** : suppression des cellules dont l'effectif pondéré est
  inférieur à 30.

---

## 4. Ce qui change d'infrastructure

| | v1 R | v2 Python |
|---|---|---|
| **Traitement** | `dplyr`, évaluation immédiate, mono-thread | `polars`, évaluation paresseuse, multi-thread |
| **Format intermédiaire** | Stata `.dta` via `haven` | Parquet compressé `zstd` |
| **Stockage** | disque local, **chemin en dur** : `run_pipeline.R:7` pointe vers `C:/Users/e_koffie/Documents/...` | **MinIO exclusivement**, via buffers mémoire ; seul fichier local produit : `logs/pipeline.log` |
| **Organisation des données** | dossiers `data/raw`, `data/processed`, `output/` | médaillon : buckets `staging` → `silver` → `gold` → `models` → `staging/exports_gold` |
| **Orchestration** | `source()` des 16 scripts, état circulant en mémoire dans une liste `results$...` | deux orchestrateurs : `pipeline.py` (in-process) et `orchestrator.py` (chaque étape en sous-processus isolé) |
| **Reprise après échec** | impossible — si l'étape 16 échoue, tout l'état est perdu | possible — chaque étape lit et écrit sur MinIO : `run --from NETTOYAGE_DONNEES --to EXPORT_EXCEL` |
| **Configuration** | 9 fichiers `config/*.R` sourcés | `config/settings.yaml` + `config/dimensions.yaml` + `.env` pour les identifiants |
| **Interface** | fonctions R interactives (`run_pipeline()`, `run_full()`) | CLI `typer` : `run`, `ingest`, `clean`, `model`, `estimate`, `audit`, `validate`, `config` |

Conséquence pratique de la bascule MinIO : le code, léger et versionné, peut tourner depuis
n'importe quelle machine ayant accès au réseau MinIO, sans jamais copier de données.

---

## 5. Ce qui change de méthode

### 5.1. Le nettoyage : désactivé en v1, actif et justifié en v2

En v1, le nettoyage est **désactivé** : `run_pipeline.R:159` appelle `clean_data(apply_filters=FALSE)`
avec le commentaire « Ne pas nettoyer la base de donnée pour l'instant ». Aucun filtre n'est donc
appliqué en pratique.

En v2, il est actif et chaque décision est documentée dans `config/settings.yaml` :

- salaire mensuel minimum : 75 000 FCFA ; plage plausible [75 000 ; 50 000 000] ;
- winsorisation aux percentiles 1 % et 99 % ;
- **exclusion des employés horaires (`H`)**, sur la base de l'audit du 28/07/2026 (23 fichiers,
  feuille `Analyse_Salaire`) : sur 157 054 lignes horaires, 65,5 % présentent une confusion d'unité
  suspectée et 69,0 % une `DUREE_TRAVAILLEE` incohérente. Pour 1,4 % de l'effectif, le rapport
  bénéfice/risque a été jugé défavorable ;
- **conservation des journaliers (`J`)** avec conversion mensuelle ×22,4 jours ouvrés : leur
  `DUREE_TRAVAILLEE` est saine (0,12 % d'incohérence) et ils représentent 5,7 % des salaires à
  périodicité connue.

Un drapeau `--include-hj-estimated` permet de relancer l'étape 03 sans aucune exclusion, pour
analyse de sensibilité uniquement.

### 5.2. La correction de la non-déclaration partielle (annexe 2 → annexe 3)

C'est le changement méthodologique le plus important, et il est bien étayé.

La v1 ne connaît qu'un indicateur `R_jt ∈ {0,1}` : une entreprise a déclaré, ou non. La v2 introduit
une propension **à deux étages** (`07b_modele_declaration_indiv.py`) :

```
π_ijt = p_jt × q_ijt
```

où `p_jt` = P(l'entreprise j déclare au mois t), estimé à l'étape 07, et `q_ijt` = P(le salarié i
est déclaré | son entreprise a déclaré), estimé à l'étape 07b sur le seul domaine des entreprises
déclarantes (`D_JT = 1`).

Justification empirique donnée dans le fichier : **65,3 % des salaires manquants se trouvent dans
des entreprises qui ont pourtant déclaré ce mois-là**. Ces déclarations partielles ne représentent
que 17,9 % des couples entreprise-mois, mais concentrent 74 % des salariés (20,1 M sur 26,9 M) — ce
sont les grandes entreprises, celles qui pèsent le plus dans une moyenne pondérée par les effectifs.
L'approche v1 traitait ces cas comme une absence totale de déclaration, alors que près de 10 M de
salaires réels y sont observés.

### 5.3. Les estimateurs : trois écarts numériques

Les deux versions ne produiront pas les mêmes chiffres, même sur des données identiques.

| | v1 R | v2 Python |
|---|---|---|
| **Variance pondérée** (dispersion des salaires) | `Σw(y-μ)²/Σw`, sans correction — `config/estimators.R:17-20` | correction de Kish/Bessel : `Σw/((Σw)²-Σw²) · Σw(y-μ)²` — `weighted_variance()` |
| **Quantiles pondérés** | plus petite valeur telle que CDF ≥ p, **sans interpolation** — `config/estimators.R:23-36` | interpolation linéaire sur la CDF pondérée, avec décalage de continuité `(cum_w − 0,5·w)/Σw` — `weighted_quantile()` |
| **Gini** | formule par covariance cumulée : `G = 1 − 2·Σ(w·cumsum(w·y))/(Σw·Σwy)` — `config/estimators.R:39-57` | Lerman & Yitzhaki (1989) : `G = 2·cov(y, F(y))/μ`, borné à [0, 1] — `weighted_gini()` |
| **Logit de déclaration** | `glm` binomial **non pénalisé** — `config/model_specs.R:36-40` | régression logistique **pénalisée L2** (`C=1.0`, solveur lbfgs), encodage one-hot des catégorielles |

Les écarts sur les quantiles sont les plus visibles : sur des cellules de petite taille, une médiane
sans interpolation peut différer sensiblement d'une médiane interpolée.

### 5.4. Les covariables du modèle de déclaration ont été réduites

| v1 R (`config/model_specs.R:17-33`) | v2 Python (`07_modele_declaration.py`) |
|---|---|
| `SECTOR_CODE` | `SECTEUR_ACTIVITE` |
| `CLASSE_EFFECTIF_REDUITE` | `CLASSE_EFFECTIF_REDUITE` |
| `AGE_ENTREPRISE` | `CL_AGE_ENTREPRISE` |
| `D_jt_lag1` | `LAG_D_JT` |
| `D_jt_lag2` | **absent** |
| `N_DECLARATIONS_PASSEES` | **absent** |
| `PCT_DECLARATIONS_PASSEES` | `TAUX_DECLARATION_PASSE` |
| `MOIS`, `ANNEE` (effets temporels) | **absents** |
| — | `SANS_HISTORIQUE` (**nouveau**) |

La v2 ajoute un indicateur `SANS_HISTORIQUE` pour les lignes de la première période du panel, où les
variables retardées sont nulles par construction. Sans cet indicateur, le modèle interpréterait
« pas d'historique » comme « n'a pas déclaré le mois dernier » et attribuerait à ces entreprises des
poids IPW gonflés. C'est une amélioration réelle sur la v1.

En revanche, la disparition des effets fixes temporels est à signaler (voir §8).

---

## 6. Ce qui est abandonné, et pourquoi

**Le modèle mixte d'imputation individuelle.** La v1 définit `imputation_individual_salary`
(`config/model_specs.R:117-145`) : un modèle `lmer` sur `log(Y_ijt)` avec effets aléatoires
`(1|NUMERO_EMPLOYEUR)` et `(1|PERIOD)`. Il n'a pas d'équivalent en v2.

Ce n'est pas un oubli. `07b_modele_declaration_indiv.py` documente le choix : les salariés non
déclarés sont pris en charge par le poids entreprise `W_JT` et par l'imputation au niveau entreprise
(étape 08), pas par une imputation individuelle. Hors du domaine d'estimation, `W_INDIV` vaut 1,0.
La v2 remplace donc une imputation individuelle par une **repondération** à deux étages.

**Autres abandons** : le format Stata et `outbot.ado` ; les dossiers `sessions/` versionnés dans Git
(les métadonnées de session vont désormais sur MinIO) ; les 7 scripts `scripts_2/`.

---

## 7. Nouveautés sans équivalent en v1

- **`audit.py`** (90 Ko) : batterie de contrôles qualité avec export Excel horodaté sur MinIO. C'est
  ce module qui a produit les chiffres ayant guidé les décisions méthodologiques de fin juillet
  (exclusion des horaires, bascule vers l'annexe 3).
- **`jointure_anstat.py`** : appariement approché sur `RAISON_SOCIALE` normalisée (~96,5 % de
  correspondance) contre le référentiel `REQUETES ANSTAT_MODULE EMPLOYEURS.xlsx`. Apporte une
  nomenclature sectorielle CEPICI/ANSTAT et des identifiants légaux (RCCM, DFE, forme juridique).
  Ce n'est **pas** un correctif de données manquantes : `SECTEUR_ACTIVITE` existe nativement côté
  CNPS avec seulement ~0,01 % de valeurs manquantes. L'outil n'est jamais exécuté automatiquement.
- **`docs/methodology.md`** + note méthodologique LaTeX/PDF, avec 26 références bibliographiques.
- **`12_export_excel.py`** : étape d'export dédiée, là où la v1 faisait un `file.copy()` en fin de
  `run_pipeline.R`.
- **Contrôles d'intégrité bloquants** dans `09_ponderation_finale.py` : un `W_FINAL` nul ou non fini
  lève une exception plutôt que d'exclure silencieusement des lignes des statistiques pondérées.
  La journalisation explicite le volume de poids nuls et sa justification.

---

## 8. Points de vigilance

Cette section relève les écarts entre ce que la documentation de la v2 **annonce** et ce que le code
**fait**. Ils ont été vérifiés en lisant les fichiers source de `refacto-cnps-minio`, pas seulement
`docs/methodology.md`.

### 8.1. Les intervalles de confiance de la v2 ne mesurent que la variabilité d'imputation

**C'est le point le plus important de cette note.**

Dans `10_estimation_indicateurs.py`, la fonction `combine_rubin()` implémente correctement les règles
de Rubin. Mais son appel, dans `_estimate_with_imputations()`, lui passe :

```python
variances = [0.0] * len(vals)
rubin = combine_rubin(vals, variances, cfg.estimation.confidence_level)
```

La variance intra-imputation `U_m` est donc forcée à zéro (le commentaire du code l'assume :
« variance intra-imputation simplifiée à 0 : pas de bootstrap intra »). La variance totale se réduit
à `T = (1 + 1/M)·B` : elle ne capture que la dispersion **entre** imputations, et ignore entièrement
la variance d'échantillonnage.

Conséquences :

- les intervalles de confiance produits par la v2 sont **substantiellement trop étroits** et ne
  doivent pas être diffusés tels quels comme des IC à 95 % ;
- la fraction d'information manquante vaut mécaniquement `FMI = (B + B/M)/T = 1`, soit 100 %, quelle
  que soit la cellule — cet indicateur est donc ininterprétable en l'état.

**Sur ce point précis, la v1 R était plus complète** : `scripts/estimation/03_calc_indicators_with_ci.R`
calcule une vraie variance de l'estimateur — variance de Horvitz-Thompson pour la moyenne avec
correction d'échantillon fini (l. 16-38), et **bootstrap à 200 réplications** pour les quantiles
(l. 57-92) — puis alimente `rubin_combine_extended()` (l. 235-286) avec ces `U_m` réels.

Rétablir un `U_m` non nul en v2 est le correctif prioritaire avant toute diffusion d'IC.

### 8.2. L'AIPW n'est pas réellement appliqué aux poids finaux

`09_ponderation_finale.py` calcule bien l'estimateur AIPW de la moyenne :

```python
mu_aipw = float(np.mean(ipw_component - augmentation))
```

mais cette valeur est **uniquement journalisée** (`logger.info("Estimation AIPW du salaire moyen : ...")`)
et n'est utilisée nulle part en aval.

Ce qui alimente réellement `W_FINAL`, c'est un poids IPW multiplié par un facteur d'augmentation
heuristique :

```python
aug_ratio = 1.0 - (1.0 - p_clipped) * y_imputed / y_safe
aug_ratio = np.clip(aug_ratio, 0.5, 2.0)  # stabilisation
```

Ce facteur, borné arbitrairement à [0,5 ; 2,0], n'est pas l'estimateur AIPW et ne porte pas sa
propriété de double robustesse. Les statistiques finales de l'étape 10 sont donc calculées avec des
poids **IPW ajustés**, pas avec un estimateur doublement robuste.

Point connexe : le modèle de résultat utilisé pour `y_imputed` est la colonne `SALAIRE_MOYEN` de la
base analytique, et non la sortie de l'étape 08 (`firm_base_imputed.parquet`). Le modèle
d'imputation n'alimente donc pas la composante d'augmentation.

**En pratique** : décrire la v2 comme « doublement robuste » dans un document de diffusion serait
inexact en l'état. La mention `estimation_method: "aipw"` dans `settings.yaml` décrit une intention,
pas encore le comportement effectif.

### 8.3. Les seuils de validation du modèle ne sont pas bloquants

`settings.yaml` définit `min_auc: 0.60` et `calibration_slope_range: [0.8, 1.2]`.

Pour l'**AUC** : elle est bien contrôlée, mais jamais de façon bloquante. `07_modele_declaration.py:190`
et `07b_modele_declaration_indiv.py:353` émettent un simple `logger.warning` ;
`11_validation_qualite.py:167` et `:189` la remontent dans le rapport de qualité — mais l'étape 11
est non bloquante par conception (`orchestrator.py` poursuit explicitement après son échec pour ne
pas empêcher l'export de l'étape 12).

Pour la **pente de calibration** : le paramètre `calibration_slope_range` est chargé dans
`config.py:57` et `:225`, puis **n'est référencé nulle part ailleurs dans `src/`**. Il n'est ni
calculé, ni vérifié, à aucune étape. C'est un paramètre de configuration mort.

Un modèle de déclaration mal calibré peut donc produire des poids IPW et traverser tout le pipeline
jusqu'à l'export sans que rien ne l'arrête.

### 8.4. Les effets fixes temporels ont disparu du modèle de déclaration

`docs/methodology.md` spécifie le modèle avec des effets fixes temporels `γ_t` (mois). Les listes
`_CATEGORICAL_FEATURES` et `_NUMERIC_FEATURES` de `07_modele_declaration.py` n'en contiennent aucun,
alors que la v1 incluait `MOIS` et `ANNEE`. Si la propension à déclarer présente une saisonnalité ou
une tendance, elle n'est plus captée par le modèle.

### 8.5. Divergence de référence sur les degrés de liberté

Les deux versions citent Barnard & Rubin (1999) pour l'ajustement des degrés de liberté en petit
échantillon, mais **implémentent l'une comme l'autre la formule classique de Rubin (1987)** :
`df = (M−1)(1 + 1/r)²` côté Python, `df = (M−1)/λ²` côté R — deux écritures de la même quantité.
L'ajustement de Barnard & Rubin n'est présent dans aucune des deux. Écart documentaire mineur, mais
à corriger dans la note méthodologique.

### 8.6. Un fichier de 42,6 Mo est versionné dans Git

`REQUETES ANSTAT_MODULE EMPLOYEURS.xlsx` est commité sur `refacto-cnps-minio` et représente à lui
seul la quasi-totalité des 42,7 Mo du dépôt — alors que le commit racine de cette lignée
(13/07/2026) avait précisément pour objet la *« réinitialisation de l'historique : exclusion des
fichiers de données volumineux »*. Ce référentiel gagnerait à vivre sur MinIO comme le reste des
données.

### 8.7. Les résultats v1 et v2 ne sont pas comparables chiffre à chiffre

Compte tenu des §5.1 (nettoyage désactivé en v1), §5.2 (annexe 2 → annexe 3), §5.3 (estimateurs) et
§5.4 (covariables), tout écart entre un indicateur v1 et son équivalent v2 est attendu. Un
rapprochement des deux séries ne constitue pas un test de non-régression valide.

---

## 9. Prise en main de la branche active

```bash
# 1. Récupérer les branches absentes du clone local
git fetch --prune
git checkout refacto-cnps-minio

# 2. Environnement Python (3.11+)
python -m venv .venv
.venv\Scripts\activate          # Windows
# source .venv/bin/activate     # Linux/Mac
pip install -e .

# 3. Identifiants MinIO — fichier .env à la racine, jamais versionné
#    MINIO_ACCESS_KEY=...
#    MINIO_SECRET_KEY=...

# 4. Vérifier la configuration et la connexion
python run.py config

# 5. Exécuter, en tout ou en partie
python run.py run                                        # les 12 étapes
python run.py run --from NETTOYAGE_DONNEES --to EXPORT_EXCEL
python run.py ingest     # étapes 01-02
python run.py clean      # étapes 03-06
python run.py model      # étapes 07-09
python run.py estimate   # étapes 10 et 12
python run.py audit      # contrôles qualité, hors séquence
```

Noms d'étape valides pour `--from` / `--to` : `LECTURE_FICHIERS`, `HARMONISATION_TYPES`,
`NETTOYAGE_DONNEES`, `BASE_INDIVIDUS`, `BASE_ENTREPRISES`, `BASE_ANALYTIQUE`, `MODELE_DECLARATION`,
`IMPUTATION_SALAIRES`, `PONDERATION_FINALE`, `ESTIMATION_INDICATEURS`, `VALIDATION_QUALITE`,
`EXPORT_EXCEL`.

Les étapes sont **séquentiellement dépendantes** : lancer l'étape 05 sans avoir jamais exécuté
l'étape 04 échoue, le fichier source étant introuvable sur MinIO.

**Note sur Apple Silicon** : si `python3` résout vers un interpréteur x86_64 sous Rosetta
(`python3 -c "import platform; print(platform.machine())"` doit afficher `arm64`), Polars peut
planter sur les gros volumes. Utiliser un Python natif arm64 pour créer le `.venv`.

---

## 10. Recommandations

Par ordre de priorité :

1. **Rétablir la variance intra-imputation** dans `10_estimation_indicateurs.py` (§8.1) avant toute
   diffusion d'intervalles de confiance. La v1 fournit une implémentation de référence
   (variance de Horvitz-Thompson + bootstrap sur les quantiles).
2. **Trancher sur l'AIPW** (§8.2) : soit implémenter réellement l'estimateur doublement robuste,
   soit renommer la méthode et retirer la mention de double robustesse de la documentation.
3. **Rendre bloquants les contrôles de qualité du modèle** (§8.3), ou au minimum calculer la pente
   de calibration qui est configurée mais jamais vérifiée.
4. **Réintroduire les effets temporels** dans le modèle de déclaration (§8.4), ou documenter leur
   retrait dans `docs/methodology.md`.
5. **Sortir le référentiel ANSTAT de Git** vers MinIO (§8.6).
6. **Faire de `refacto-cnps-minio` la branche par défaut** du dépôt, ou fusionner son contenu dans
   `main`, pour qu'un nouvel arrivant tombe sur la version vivante.

---

## Annexe — sources de cette note

Fichiers de la v1 lus dans le dépôt local (branche `master`) :
`run_pipeline.R`, `config/estimators.R`, `config/model_specs.R`,
`scripts/estimation/03_calc_indicators_with_ci.R`.

Fichiers de la v2 lus sur `refacto-cnps-minio` : `README.md`, `docs/methodology.md`,
`config/settings.yaml`, `src/cnps/orchestrator.py`, `src/cnps/07_modele_declaration.py`,
`src/cnps/07b_modele_declaration_indiv.py`, `src/cnps/09_ponderation_finale.py`,
`src/cnps/10_estimation_indicateurs.py`.

Les affirmations de la §8 reposent sur la lecture du **code**, pas de la documentation. Les étapes
`08_imputation_salaires.py`, `11_validation_qualite.py` et `12_export_excel.py` n'ont pas été lues
en détail : les affirmations les concernant proviennent du README et de la note méthodologique de la
branche, et restent à vérifier.

L'état des branches correspond à `git ls-remote origin` du 1er août 2026.
