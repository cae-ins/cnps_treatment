# Pipeline CNPS — traitement des déclarations salariales

Pipeline Python/Polars de traitement des déclarations mensuelles de la CNPS de Côte d'Ivoire. Le chemin de publication produit des statistiques ponctuelles sur les salaires observés, corrigées de la non-réponse par pondération IPW à deux étages.

## D'où vient cette version et pourquoi elle change

Le projet vient d'un pipeline **R** conservé sur les branches historiques `master`/`main`. Cette
première version travaillait sur des fichiers Stata locaux, avec des chemins propres à la machine
d'exécution, un nettoyage désactivé dans le chemin nominal et une correction de la non-réponse au
seul niveau employeur. La refonte **Python/Polars/MinIO**, développée sur
`refacto-cnps-minio`, a apporté le stockage objet, la reprise par étape, une CLI et l'intention d'une
correction à deux étages.

La présente branche part du commit `2ed8ae5` de cette refonte. Un audit statique croisé réalisé le
1er août 2026 a toutefois montré que plusieurs promesses méthodologiques n'étaient pas reliées au
chemin réellement exécuté. Le problème central n'était donc pas le portage R vers Python en lui-même,
mais l'écart entre la méthode annoncée, les objets transmis d'une étape à l'autre et les contrôles de
publication.

| Avant la correction | Risque pour les résultats | Décision dans cette version |
|---|---|---|
| La CLI sautait le modèle individuel `07b` et l'étape 09 ne récupérait pas effectivement les propensions employeur. | Des statistiques pouvaient être présentées comme corrigées alors que le chemin nominal conservait des poids unitaires. | `07b` appartient au DAG ; l'étape 09 joint explicitement les deux propensions et calcule `D×S/(p×q)`. Toute provenance manquante fait échouer le pipeline. |
| Le panel entreprise–mois reposait sur un produit cartésien intégral et certaines covariables utilisaient une information contemporaine ou future. | Le modèle pouvait apprendre des mois artificiels ou une fuite temporelle au lieu du comportement déclaratif. | Le champ à risque est défini par une fenêtre rétrospective configurable `K`, les historiques sont calculés par mois civil et les covariables sont propagées *as-of*. |
| Le pipeline annonçait de l'AIPW, une imputation multiple et des intervalles de confiance, mais l'imputation n'alimentait pas correctement l'estimation et la variance intra-imputation était nulle. | Des intervalles trop étroits, voire de largeur nulle, pouvaient donner une apparence de précision injustifiée. | Le chemin de publication est limité à l'IPW à deux étages et aux estimations ponctuelles. L'étape 08 reste un prototype hors DAG ; l'inférence est différée jusqu'à validation du lot F.1. |
| Une même variable winsorisée alimentait toutes les statistiques. | Le Gini, les quantiles et les extrêmes étaient artificiellement comprimés. | Chaque statistique déclare sa variable : salaire winsorisé pour moyenne/variance, salaire non winsorisé pour quantiles, Gini et extrêmes observés. |
| Le secret statistique et la validation n'étaient pas des préconditions effectives de l'export. | Des cellules fragiles ou des résultats non finis pouvaient atteindre le classeur final. | Seuils sur individus et employeurs distincts, contrôle de dominance, suppression secondaire déterministe et export bloqué si l'étape 11 échoue. |
| La filiation d'une exécution et la sécurité des artefacts de modèle étaient incomplètes. | Une sortie était difficile à rattacher exactement à son code et à sa configuration ; des objets sérialisés distants étaient chargés. | Manifeste de session avec empreinte de configuration et commit Git, modèles résumés en JSON non exécutable, secrets hors configuration et garde TLS en production. |

Ces corrections rendent le chemin de calcul cohérent et testable hors ligne ; elles ne valent pas
encore homologation sur données réelles. Aucun rejeu complet MinIO n'a été effectué pendant l'audit,
le modèle `q` ne peut pas représenter un salarié dont aucune ligne n'existe, l'inférence reste
`point_only`, et les seuils de confidentialité doivent être confirmés par la doctrine ANStat/CNPS.
Le détail des constats, des arbitrages et de la transition se trouve dans le
[rapport d'audit](docs/rapport_audit_et_decisions.md), le
[plan de correction](docs/plan_correction_v2.md) et la
[note de passation R vers Python](docs/note_passation_v1R_vers_v2python.md).

## Statut méthodologique

L'estimand est la distribution des salaires des couples **salarié–employeur–mois** représentables par les employeurs présents dans les sources. Pour le mois `t`, le champ glissant retient les employeurs ayant déclaré au moins une fois pendant les `K=12` mois strictement précédents. `K` est configurable et doit faire l'objet d'une sensibilité `6/12/24/inf` avant diffusion.

Le poids final est :

```text
R_ijt = D_jt × S_ijt
W_FINAL_RAW = R_ijt / (p_hat_jt × q_hat_ijt)
```

- `D_jt` : au moins un salaire positif déclaré par l'employeur au mois `t` ;
- `S_ijt` : salaire positif renseigné sur la ligne salarié existante ;
- `p_hat_jt` : propension de réponse de l'employeur ;
- `q_hat_ijt` : propension conditionnelle de réponse de la ligne salarié, sachant `D_jt=1`.

Le poids n'est pas stabilisé ni normalisé à moyenne 1. Le produit final est tronqué aux quantiles configurés. Les non-répondants et les lignes hors champ reçoivent un poids final nul.

Le modèle `q` ne corrige que le salaire manquant sur une **ligne salarié existante**. Il ne représente pas les salariés dont aucune ligne n'existe dans les fichiers. Les employeurs absents de tout le panel sont également hors champ.

### Inférence

La configuration impose `inference_method: point_only`. Aucun intervalle de confiance n'est diffusé : la linéarisation conjointe des deux modèles de réponse, dans un cadre de population finie, reste à spécifier et valider (lot F.1). L'ancien module `08_imputation_salaires.py` est un prototype expérimental hors DAG ; il n'alimente ni les poids ni les indicateurs publiés.

## Contrôles de publication

- validation croisée hors échantillon groupée par employeur pour `p` et `q` ;
- blocage sur cible constante, prédictions non finies, absence de recouvrement, mauvaise calibration, déséquilibre résiduel et clipping/trimming excessif ;
- AUC descriptive, jamais bloquante ;
- variable propre à chaque statistique : moyenne/variance sur salaire winsorisé, quantiles/Gini/extrêmes sur salaire non winsorisé ;
- minimum et maximum étiquetés « observés » ;
- secret primaire : au moins 30 individus et 3 employeurs distincts, dominance maximale de 85 % de la masse salariale observée ;
- suppression secondaire déterministe dans chaque marge, y compris par mois ;
- aucune cellule contenant `NaN`/`inf` n'est écrite comme nombre dans Excel.

Les seuils de confidentialité doivent être remplacés si une doctrine ANStat/CNPS officielle plus contraignante existe.

## Installation

Python 3.11 ou supérieur :

```powershell
python -m venv .venv
.\.venv\Scripts\python.exe -m pip install -e ".[dev]"
```

Les tests sont entièrement synthétiques et n'appellent pas MinIO :

```powershell
.\.venv\Scripts\python.exe -m pytest -q
```

## Configuration et sécurité

Les paramètres sont dans `config/settings.yaml` et les dimensions/statistiques dans `config/dimensions.yaml`. Les secrets ne doivent jamais être placés dans le YAML ni versionnés :

```text
MINIO_ACCESS_KEY=...
MINIO_SECRET_KEY=...
```

En `production`, les deux secrets sont obligatoires et `minio.secure` doit être `true`. En `development`, HTTP exige l'option explicite `allow_insecure_minio: true`. Les modèles sont conservés sous forme de résumés JSON non exécutables ; aucun pickle distant n'est chargé.

## DAG de publication

| Ordre | Module | Rôle principal |
|---:|---|---|
| 01 | `01_lecture_fichiers.py` | Excel vers Parquet et registre d'ingestion |
| 02 | `02_harmonisation_types.py` | types, dates et parsing numérique contrôlé |
| 03 | `03_nettoyage_donnees.py` | filtres, déduplication, périodicités, variables dérivées |
| 04 | `04_base_individus.py` | base salarié–employeur–mois et `S_IJT` |
| 05 | `05_base_entreprises.py` | panel entreprise, champ à risque et covariables as-of |
| 06 | `06_base_analytique.py` | jointure analytique, cardinalité de gauche conservée |
| 07 | `07_modele_declaration.py` | modèle employeur `p_jt` et facteur `1/p_hat` |
| 07b | `07b_modele_declaration_indiv.py` | modèle ligne salarié `q_ijt` et facteur `1/q_hat` |
| 09 | `09_ponderation_finale.py` | `R/(p×q)`, trimming et poids nuls hors réponse |
| 10 | `10_estimation_indicateurs.py` | estimateurs ponctuels et secret statistique |
| 11 | `11_validation_qualite.py` | garde de prépublication |
| 12 | `12_export_excel.py` | export seulement si la validation passe |

Le saut de numérotation 08 est volontaire. `pipeline.Stage` ne contient aucune étape d'imputation.

## Exécution

```powershell
python run.py ingest
python run.py clean
python run.py model
python run.py estimate

# ou pipeline complet
python run.py run

# plage explicite
python run.py run --from NETTOYAGE_DONNEES --to EXPORT_EXCEL
```

Étapes valides : `LECTURE_FICHIERS`, `HARMONISATION_TYPES`, `NETTOYAGE_DONNEES`, `BASE_INDIVIDUS`, `BASE_ENTREPRISES`, `BASE_ANALYTIQUE`, `MODELE_DECLARATION`, `MODELE_DECLARATION_INDIV`, `PONDERATION_FINALE`, `ESTIMATION_INDICATEURS`, `VALIDATION_QUALITE`, `EXPORT_EXCEL`.

Les commandes `enrich-anstat` et `audit` sont hors DAG. L'enrichissement ANStat fait une égalité après normalisation de la raison sociale ; ce n'est pas un appariement flou.

## Sorties et filiation

Les données de production vivent dans les buckets/prefixes MinIO définis par la configuration. Chaque exécution écrit un manifeste `sessions/{UUID}/metadata.json` comprenant :

- empreinte SHA-256 de la configuration, secrets exclus ;
- commit Git et indicateur d'arbre sale ;
- versions des dépendances ;
- statut, durée, entrée héritée et sortie déclarée de chaque étape.

Le manifeste est immuable par identifiant de session, mais les objets canoniques d'étape restent actuellement réécrits en place. Cette limite est explicitement enregistrée dans `artifact_contract` et doit être levée avant une réplication historique intégrale.

## Documentation

- `docs/methodology.md` : spécification technique et méthodologique ;
- `docs/note_methodologique_traitement.pdf` : note de diffusion ;
- `docs/rapport_audit_et_decisions.md` : constats ayant motivé la correction ;
- `docs/plan_correction_v2.md` : arbitrages, lots et critères de recette ;
- `docs/note_passation_v1R_vers_v2python.md` : pont historique entre la v1 R et la refonte Python ;
- `docs/protocole_tests_dgp.md` : protocole de validation par processus générateurs ;
- `docs/protocole_recette_jhub.md` : exécution réelle, artefacts sessionnés et sensibilités ;
- `docs/rapport_resultats_controles_suivi_ciap.docx` : résultats de l'audit, contrôles historiques et pont CIAP/comptabilité nationale.

Aucune valeur de production récente n'est embarquée dans ce dépôt. Les chiffres historiques cités dans les rapports doivent être identifiés par période, session et empreinte des entrées, puis recalculés après rétablissement de l'accès MinIO/VPN.
