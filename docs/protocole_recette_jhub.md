# Protocole de recette sur JupyterHub

Ce protocole produit les éléments techniques nécessaires à l'évaluation d'un run réel. Il ne
vaut pas homologation institutionnelle et ne lève pas le statut `point_only` de l'inférence.

## Préparation

Après avoir récupéré la branche, installer le projet et définir les secrets hors du dépôt :

```bash
git pull --ff-only
python -m pip install -e ".[dev]"
export MINIO_ACCESS_KEY='...'
export MINIO_SECRET_KEY='...'
python -m pytest -q
```

Ne jamais écrire les secrets dans `config/settings.yaml`, un notebook ou une cellule enregistrée.

## Run nominal

```bash
python run.py run 2>&1 | tee run_k12.log
```

Pour committer et pousser automatiquement les rapports agrégés après un run complet réussi :

```bash
export CNPS_AUTO_GIT_PUSH=true
python run.py run 2>&1 | tee run_k12.log
```

L'auto-publication refuse un dépôt déjà modifié, une autre branche que celle autorisée dans
`git_publication.branch` ou un artefact extérieur à la session. Le `run_report.json` est publié
quel que soit le succès du pipeline ; `validation_report.json` n'est ajouté que si l'étape 11 a
été atteinte.
Par défaut, les estimations ne sont pas ajoutées à Git (`include_estimates: false`).

La première ligne du pipeline donne le `session_id`. Le préfixe MinIO suivant contient les preuves
propres à cette exécution :

```text
<output_prefix>/sessions/<session_id>/metadata.json
<output_prefix>/sessions/<session_id>/run_report.json
<output_prefix>/sessions/<session_id>/validation_report.json
<output_prefix>/sessions/<session_id>/estimation_results.json
```

Le rapport de validation contient notamment : paramètres d'estimation, couverture du champ,
réponse aux deux étages, diagnostics OOF, distribution des poids, ESS, part tronquée et détail de
chaque garde. `technical_validation=PASS` signifie que les contrôles automatisés passent ; le
champ `official_publication_readiness` reste bloqué jusqu'à F.1 et aux validations externes.

## Sensibilités obligatoires

Exécuter des runs séparés pour `risk_window_months` égal à `6`, `12`, `24` et `inf`. Conserver le
`session_id` de chaque run. Pour chaque valeur de K, répéter au minimum avec les bornes nominales
de trimming, puis avec des bornes alternatives décidées dans le protocole statistique.

Avant chaque run, modifier uniquement les paramètres concernés dans `config/settings.yaml`, puis
consigner le diff :

```bash
git diff -- config/settings.yaml
python run.py run 2>&1 | tee run_sensibilite.log
```

Les artefacts sessionnés empêchent l'écrasement des estimations lors du run suivant. Comparer les
lignes de `estimation_results.json` par `(dimension, group)` et relever au minimum l'écart absolu
et relatif de la moyenne, de la médiane et du Gini, ainsi que l'ESS et la part tronquée du rapport.

## Éléments à transmettre après le run

- les quatre `session_id` nominaux ;
- les fichiers de log correspondants, secrets vérifiés absents ;
- les `metadata.json`, `run_report.json` et, s'il existe, `validation_report.json` ;
- les erreurs éventuelles avec leur première trace complète ;
- les agrégats externes autorisés pour le rapprochement, accompagnés de leur millésime et de leur
  définition de champ.
