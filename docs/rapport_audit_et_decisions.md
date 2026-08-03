# Pipeline CNPS v2 — Rapport d'audit et décisions

**Branche auditée** : `refacto-cnps-minio`, commit `2ed8ae5` (31/07/2026)
**Date de l'audit** : 1er août 2026
**Nature** : audit statique du code. Aucun accès au serveur MinIO ni aux données réelles ; le
pipeline n'a pas été exécuté.

---

## 1. Méthode

Quatre analyses indépendantes du même code, menées à froid et sans se voir les unes les autres :

| # | Analyste | Portée | Résultat |
|---|---|---|---|
| 1 | Claude (Opus 5) | audit complet | 4 constats critiques, 6 importants |
| 2 | OpenAI `gpt-5.6-sol` (effort xhigh) | audit complet, brief identique | 12 constats critiques, 7 importants |
| 3 | Google `gemini-3.1-pro-high` | réfutation adversariale de 2 constats | 1 partiellement réfuté, 1 non réfuté |
| 4 | Google `gemini-3.6-flash-high` | même réfutation, brief identique | 2 non réfutés |

Les analystes 3 et 4 avaient pour consigne explicite de **démonter** les constats soumis, et de ne
conclure à leur solidité qu'après avoir réellement cherché la faille.

Toutes les affirmations reprises ci-dessous portent une mention de fiabilité :

- **[vérifié]** — j'ai lu le code moi-même et confirmé le constat ;
- **[convergent]** — plusieurs analystes indépendants aboutissent au même diagnostic ;
- **[non vérifié]** — rapporté par un analyste, plausible, non recoupé.

---

## 2. Verdict

**Les indicateurs produits par ce pipeline ne sont pas publiables en l'état.**

Ce jugement ne repose pas sur un défaut isolé mais sur une chaîne : l'univers statistique sur
lequel les modèles sont estimés est en partie fictif ; la correction du biais de sélection n'est
pas appliquée par le chemin d'exécution documenté ; l'imputation multiple est calculée puis jetée ;
et les intervalles de confiance ne mesurent pas la variance d'échantillonnage.

Chacun de ces points suffirait à interdire la diffusion. Leur conjonction fait qu'aucun chiffre
actuellement produit — moyenne, médiane, Gini, effectif pondéré — ne peut être interprété comme
une estimation corrigée de la non-déclaration.

**Ce constat n'est pas un jugement sur la qualité du travail.** Le code est lisible, bien
structuré, et remarquablement documenté : plusieurs décisions méthodologiques y sont justifiées
chiffres à l'appui, avec une honnêteté qu'on rencontre rarement. La note méthodologique est sérieuse
et correctement référencée. Les défauts relevés sont pour l'essentiel des ruptures entre une
intention juste et son implémentation — pas des choix méthodologiques erronés.

---

## 3. Constats bloquants

### B1 — Par la CLI, tous les poids valent 1 : aucune correction n'est appliquée

**[vérifié] [convergent : analystes 1 et 2]**

`pipeline.py:34-74` définit douze étapes. **Aucune entrée pour `07b_modele_declaration_indiv.py`**,
ni dans l'énumération `Stage`, ni dans `_STAGE_MODULES`. Or `cli.py:45` importe `run_pipeline`
depuis `cnps.pipeline` et toutes les commandes y passent (`cli.py:109`, `:136`, `:147`, `:158`,
`:169`). `orchestrator.py`, lui, découvre les fichiers par motif et exécute bien 07b : **les deux
orchestrateurs n'exécutent pas le même pipeline.**

Le problème dépasse la sous-étape manquante. L'étape 07 écrit `W_JT` et `P_HAT_JT` dans
`firm_base.parquet`. L'étape 09 ne lit **que** `analytical_base.parquet` (`09:134-139`). Le seul
pont entre les deux se trouve dans `07b:242-264`. Résultat par la CLI :

1. `analytical_base` conserve `W_JT = 1.0` (valeur de remplissage, `05:222`) et `W_INDIV = 1.0`
   (`04:107`), sans colonne `P_HAT_JT` ;
2. le test `if method == "aipw" and "P_HAT_JT" in df.columns` (étape 09) est faux ;
3. branche `else` : `W_FINAL = 1.0 × 1.0 = 1.0` ;
4. la normalisation par période divise par 1.0 ;
5. le contrôle d'intégrité passe — ni null, ni valeur non finie, ni poids nul ;
6. l'étape 10 calcule des statistiques « pondérées » avec des poids unitaires.

**Et c'est silencieux.** L'étape 09 journalise `"Calcul des poids finaux avec la methode : aipw"`
puis prend la branche IPW sans avertissement. Le travail de l'étape 07 est intégralement produit,
sauvegardé, puis ignoré.

### B2 — L'univers statistique est en partie fictif, et le modèle de propension l'apprend

**[vérifié] [convergent : analystes 2, 3 et 4 — les deux réfutations ont échoué]**

`05_base_entreprises.py:140` construit le panel par produit cartésien intégral
(`all_firms.join(all_periods, how="cross")`), sans aucun filtre sur la période d'activité de
l'entreprise. Une société créée en 2023 existe dans le panel en 2020. La jointure gauche de la
ligne 147 laisse alors `null` sur tous les attributs agrégés — `SECTEUR_ACTIVITE`,
`CLASSE_EFFECTIF_REDUITE`, `CL_AGE_ENTREPRISE` (`firm_attrs`, `05:119-125`) — et `D_JT` vaut 0 sur
ces lignes par construction (`05:166-173`).

L'étape 07 remplace ces `null` par la modalité `"INCONNU"` (`07:111-115`), qu'elle donne ensuite au
modèle logistique.

**Conséquence** : `"INCONNU"` et `D_JT = 0` sont quasi parfaitement colinéaires — le README indique
~0,01 % de manquants sur `SECTEUR_ACTIVITE` dans les lignes réelles. La régression dispose d'un
séparateur presque parfait, fabriqué par le pipeline lui-même. Elle apprend l'artefact de
construction, pas le comportement déclaratif.

**Effet de second ordre, décisif** : l'AUC est calculée *in-sample* (`07:182-190`), sans découpage
ni validation croisée. Elle sera excellente — le modèle sépare trivialement des lignes qu'il a
étiquetées lui-même. **Le garde-fou `min_auc: 0.60` ne se déclenchera donc jamais**, non parce que
le modèle est bon, mais parce qu'il mesure un artefact. Le seul contrôle de qualité existant est
neutralisé par le défaut qu'il devrait détecter.

Corollaire : la probabilité marginale `P(D=1)` qui sert à stabiliser les poids IPW (`07:196`) est
calculée sur cette population partiellement fictive. Plus le panel contient de mois artificiels,
plus elle est tirée vers le bas, et plus les poids sont faux.

### B3 — L'imputation multiple est calculée puis intégralement jetée

**[vérifié] [convergent : analystes 2, 3 et 4]**

`04_base_individus.py:54` construit la base individus depuis `cnps_cleaned.parquet`, qui ne contient
que les lignes réellement transmises. Une entreprise totalement non déclarante un mois donné n'y a
aucune ligne. `06_base_analytique.py:67` construit la base analytique par **jointure gauche depuis
cette base individus** : ces entreprises-mois n'ont donc aucune ligne dans `analytical_base`.

`08_imputation_salaires.py:92` impute pourtant un salaire moyen pour toutes les lignes `D_JT = 0`
et les écrit dans `firm_base_imputed.parquet`. Mais `10_estimation_indicateurs.py:343-349` réinjecte
ces imputations par une **jointure gauche dont la table de gauche est `analytical_base`**. Une
jointure gauche ne crée aucune ligne : les imputations des entreprises jamais déclarantes sont
éliminées en totalité.

**L'objection naturelle a été testée et écartée.** L'analyste 3 a d'abord objecté que la
repondération IPW dispense d'imputer les absents — ce qui est exact en théorie des sondages pour la
non-réponse totale d'unité. Cette objection ne tient pas ici, pour trois raisons :

1. la repondération invoquée est **inerte** (constat B1) ;
2. le projet revendique de l'**AIPW**, dont la composante d'augmentation exige le modèle de résultat
   `m(X)` *pour les unités non déclarantes* — impossible si elles n'ont aucune ligne ;
3. les poids invoqués sont estimés sur l'univers fictif du constat B2.

L'analyste 4, sur le même brief, a conclu directement **NON RÉFUTÉ** en relevant que la promesse
d'imputation multiple par les règles de Rubin est *« totalement inopérante »* pour ces entreprises.

### B4 — Les intervalles de confiance ne mesurent pas la variance d'échantillonnage

**[vérifié] [convergent : analystes 1 et 2]**

Deux défauts cumulatifs.

`10_estimation_indicateurs.py:465-467` passe `variances = [0.0] * len(vals)` à `combine_rubin()`.
La variance intra-imputation `U_m` est donc nulle et `T = (1 + 1/M)·B` : les IC ne capturent que la
dispersion entre imputations. La fraction d'information manquante vaut mécaniquement
`FMI = (B + B/M)/T = 1`, soit 100 % dans chaque cellule — l'indicateur est ininterprétable.

**Conséquence extrême** : dans une cellule où aucune entreprise imputée ne contribue, les M
estimations sont identiques, donc `B = 0`, donc `T = 0` — **l'intervalle de confiance a une largeur
exactement nulle**.

`08_imputation_salaires.py:155` ajuste le modèle **une seule fois** et `:185` calcule `y_hat` une
seule fois ; seule la perturbation résiduelle varie entre imputations (`:202-219`). L'incertitude
sur `β̂` n'est jamais propagée : c'est une imputation multiple *improper* au sens de Rubin, et `B`
est lui-même structurellement sous-estimé.

Deux divergences documentaires associées : la docstring annonce un « bootstrap des résidus » alors
que le code tire `rng.normal(0, sigma_hat, ...)` (tirage paramétrique) ; et les degrés de liberté
implémentés sont ceux de Rubin (1987), pas l'ajustement de Barnard & Rubin (1999) revendiqué.

### B5 — Le Gini et les extrêmes sont calculés sur le salaire winsorisé

**[vérifié]**

`03_nettoyage_donnees.py` écrase en place les salaires sous p1 et au-dessus de p99. Son propre
commentaire prévient explicitement :

> *« Les statistiques d'inégalité (Gini, ratios inter-déciles, part du dernier centile) ne doivent
> PAS être calculées sur cette variable — utiliser SALAIRE_BRUT ou une version non winsorisée. »*

Or `10_estimation_indicateurs.py:520` fixe `salary_col = "SALAIRE_BRUT_ESTIME_AU_MOIS"` — cette
variable exactement — pour **toutes** les statistiques, Gini, minimum, maximum et quantiles compris.

Le minimum et le maximum exportés sont donc les bornes de winsorisation, pas des valeurs observées ;
le Gini est mécaniquement comprimé. Le code contredit son propre avertissement.

---

## 4. Constats sérieux

| # | Constat | Localisation | Fiabilité |
|---|---|---|---|
| S1 | **L'AIPW est calculé puis jeté.** `mu_aipw` n'est que journalisé ; `W_FINAL` repose sur un facteur `aug_ratio = 1 − (1−p)·y_imp/y_obs` borné arbitrairement à [0,5 ; 2,0], sans fondement théorique. `y_imputed` vient de `SALAIRE_MOYEN` de la base analytique, pas de l'étape 08. | `09:97`, `:106-108`, `:163-165`, `:198` | [vérifié] [convergent 1,2] |
| S2 | **Fuite de données dans le modèle individuel.** `completude.shift(1).over("ID_EMPLOYEUR")` décale d'**une ligne**, pas d'une période, sur un DataFrame individuel. Dans une entreprise de 200 salariés, 199 lignes reçoivent la complétude du **mois courant** — fonction directe du `S_IJT` à prédire. Le commentaire annonce pourtant l'inverse. | `07b:167-172` | [vérifié] [analyste 2] |
| S3 | **Les âges dépendent de la date d'exécution.** `ref_date = date.today()` : âge du salarié, ancienneté et âge de l'entreprise sont calculés par rapport à aujourd'hui, pas au mois de déclaration. Un retraitement dans six mois déplace les observations entre classes et change les tableaux publiés. | `03:416` | [vérifié] [analyste 2] |
| S4 | **Le secret statistique porte sur une somme de poids**, pas sur un nombre de contributeurs distincts. Deux salariés observés quinze mois font trente lignes et déverrouillent la cellule. Aucune suppression secondaire. | `10:261-266` | [vérifié] [analyste 2] |
| S5 | **La CLI sort en code 0 malgré un échec** (`cli.py:109-125`, pas de `typer.Exit(1)`), et l'étape 11 reçoit `valider_tout(cfg)` sans le DataFrame de résultats : la validation des estimations ne s'exécute jamais. | `cli.py:109-125`, `pipeline.py:165-175` | [vérifié] [analyste 2] |
| S6 | **Le Gini est exporté sans décimale.** `_NUMBER_FMT = "#,##0"` s'applique à toutes les valeurs numériques : un Gini de 0,37 s'affiche **0**. `_DECIMAL_FMT` est déclaré et jamais utilisé. Par ailleurs `write_number()` sans `nan_inf_to_errors` échoue sur un `NaN`. | `12:32`, `:86`, `:112-116` | [vérifié] [analyste 2] |
| S7 | **La commande `enrich-anstat` est cassée** : `cli.py:188` importe `cnps.05_1_jointure_anstat`, le fichier s'appelle `jointure_anstat.py`. `ModuleNotFoundError` avant toute lecture. Le README documente l'ancien nom. | `cli.py:188` | [vérifié] [analyste 2] |
| S8 | **Asymétrie 07 / 08 sur l'absence d'historique.** L'étape 07 crée un indicateur `SANS_HISTORIQUE` en expliquant pourquoi remplir une variable retardée par 0 fausse le modèle ; l'étape 08 fait exactement cela sur `LAG_SALAIRE_MOYEN`, sans indicateur. Biais systématique à la baisse pour les entreprises sans historique. | `07:82-101` vs `08:78-79` | [vérifié] [analyste 1] |
| S9 | **Perte silencieuse de lignes à l'étape 08.** Les entreprises déclarantes à masse salariale nulle (`LOG_SALAIRE_MOYEN` non fini) et les lignes à `D_JT` null n'appartiennent ni à `df_declaring` ni à `df_missing` : elles disparaissent du jeu imputé. | `08:87-92`, `:224-230` | [vérifié] [analyste 1] |
| S10 | **Paramètres de configuration morts.** `calibration_slope_range` est chargé puis jamais utilisé ; `tmle` est annoncé comme méthode possible et tombe silencieusement dans la branche IPW ; `file_pattern`, `encoding` et la section `logging` ne pilotent rien ; les classes de `dimensions.yaml` sont chargées mais les bornes codées en dur à l'étape 03. | `config.py:57`, `:225`, divers | [vérifié pour la calibration] [analyste 2 pour le reste] |

---

## 5. Constats mineurs

- **`pandas` est une dépendance de fait non déclarée** — quatre appels `.to_pandas()` (`07:182`,
  `07b:346`, `08:154`, `08:165`) et absence dans `pyproject.toml`. Une installation propre casse à
  l'étape 07. Symétriquement, `statsmodels` est déclaré et jamais importé. **[vérifié]**
- **Aucun test** — `pytest`, `pytest-cov` et `ruff` sont en dépendances `dev`, aucun fichier de test
  dans le dépôt. **[vérifié]**
- **`ddof=len(features)`** (`08:160`) compte les noms de variables bruts, pas les colonnes après
  encodage one-hot. Sans effet numérique notable vu la taille de l'échantillon. **[vérifié]**
- **Virgules décimales françaises écrasées** — `str.replace_all(r"[^\d.\-]", "")` transforme
  `75 000,50` en `7500050` (`02:68-73`). Ne se déclenche que si les fichiers source utilisent la
  virgule décimale : invérifiable sans les données. **[vérifié dans le code, conditionnel en effet]**
- **Déduplication sur clés incomplètes** — la clé inclut `ID_INDIV` sans exiger qu'il soit
  renseigné ; plusieurs salariés non identifiés du même employeur au même mois seraient fusionnés.
  **[non vérifié]**
- **Sécurité** — repli sur `minioadmin/minioadmin` en l'absence de secrets, `secure: false` (trafic
  en clair), chargement de pickles depuis MinIO (exécution de code arbitraire possible).
  **[non vérifié]**
- **Performance** — matérialisation eager de chaque Parquet, conversion intégrale en pandas à
  l'étape 07b, et estimation de l'étape 10 exécutée **deux fois** (une fois pour elle-même, une fois
  pour l'export, `pipeline.py:168-172`). **[vérifié pour la double exécution]**

---

## 6. Décisions

L'ordre compte : plusieurs correctifs sont sans effet tant que les précédents ne sont pas faits.

### Phase 0 — Empêcher la diffusion de chiffres faux *(immédiat, quelques heures)*

Objectif : qu'aucune sortie erronée ne puisse plus être produite silencieusement.

1. Faire **échouer** l'étape 09 si `estimation_method == "aipw"` et que `P_HAT_JT` est absent.
2. Faire **échouer** l'étape 09 si tous les `W_JT` valent 1 alors que `D_JT` prend plusieurs valeurs.
3. Faire **échouer** l'export si un IC est non fini ou de largeur nulle, ou si le rapport de
   validation contient une erreur.
4. Terminer la CLI avec un code de retour non nul quand `PipelineResult.success` est faux.
5. Marquer explicitement toute sortie existante comme non diffusable.

### Phase 1 — Reconstruire l'univers statistique *(préalable à tout le reste)*

C'est la seule décision qui n'est pas purement technique et qui appelle un arbitrage métier.

6. Restreindre le panel entreprise aux mois où l'entreprise est **active** (date d'immatriculation,
   cessation éventuelle) au lieu du produit cartésien.
7. Propager les attributs constants de l'entreprise (secteur, classe de taille) sur les mois non
   déclarants, pour supprimer la modalité `"INCONNU"` corrélée à la cible.
8. **Trancher l'estimand** : soit IPW/AIPW sur les répondants avec estimateurs spécifiques, soit
   imputation multiple sur jeux complets avec micro-enregistrements créés pour les effectifs
   attendus. Le mélange actuel des deux est ce qui produit le constat B3.
9. Évaluer le modèle de propension **hors échantillon** (validation croisée groupée par entreprise
   ou par bloc temporel) et rendre le seuil d'AUC bloquant une fois qu'il mesure quelque chose.

### Phase 2 — Rétablir la chaîne de pondération

10. Déplacer le rafraîchissement `W_JT` / `P_HAT_JT` **dans l'étape 09**, qui consomme ces colonnes
    — plutôt que de le laisser dépendre de 07b.
11. Corriger la fuite de `07b:167-172` (construire le lag sur une table entreprise-période unique,
    puis la joindre aux individus) **avant** d'activer l'étape.
12. Ajouter `MODELE_DECLARATION_INDIV = 75` à `Stage` et `_STAGE_MODULES`.
13. Trancher sur l'AIPW : l'implémenter réellement (`phi = m_hat + d/p_hat·(y − m_hat)`), ou
    renommer la méthode et retirer la mention de double robustesse de la documentation.

### Phase 3 — Rétablir la mesure de l'incertitude

14. Rendre l'imputation *proper* : rééchantillonner (bootstrap par entreprise) ou tirer `β*` de sa
    loi a posteriori à chaque imputation.
15. Calculer de vraies variances complètes `U_m` — fonction d'influence pour la moyenne, bootstrap
    groupé pour quantiles et Gini — et les passer à `combine_rubin()`.
16. Aligner la documentation sur les degrés de liberté réellement implémentés, ou implémenter
    Barnard & Rubin.

### Phase 4 — Fiabiliser la publication

17. Séparer salaire brut et salaire winsorisé en deux colonnes ; publier le Gini et les statistiques
    de queue sur la version non winsorisée.
18. Fonder le secret statistique sur un nombre minimal d'individus **et** d'employeurs distincts,
    contrôler la dominance des plus gros contributeurs, ajouter une suppression secondaire.
19. Formats Excel par statistique (décimales pour le Gini) et gestion explicite des `NaN`.

### Phase 5 — Exploitation et hygiène

20. Calculer les durées par rapport au mois de déclaration, pas à `date.today()`.
21. Corriger l'import de `enrich-anstat` ; aligner le README sur le comportement réel du matching.
22. Déclarer `pandas`, retirer `statsmodels` ; supprimer ou brancher les paramètres morts ; refuser
    au chargement toute valeur de configuration non implémentée.
23. Exiger les secrets MinIO, activer TLS hors développement, remplacer pickle par un format sûr.
24. Versionner les artefacts par session avec un manifeste (hash des entrées, configuration
    résolue, commit) pour empêcher le mélange de générations.
25. Ne calculer l'estimation qu'une seule fois et la transmettre à l'export.

### Tests — transversal, à démarrer en parallèle de la phase 0

26. Tests unitaires sur les estimateurs pondérés (moyenne de Hájek, variance de Kish, quantiles
    interpolés, Gini) contre des cas calculés à la main. Meilleur rapport valeur/effort du projet :
    ce sont des fonctions pures.
27. Tests sur processus générateurs synthétiques : MCAR/MAR, modèle de propension mal spécifié,
    modèle de résultat mal spécifié, déclaration partielle, entreprise jamais déclarante, poids
    extrêmes, Gini connu, couverture empirique des IC.

---

## 7. Points d'arbitrage à remonter

Trois décisions dépassent la correction technique et appellent une position de l'équipe :

1. **L'estimand** (décision 8). Que prétend-on estimer : la distribution des salaires des salariés
   déclarés, ou celle de l'ensemble des salariés du champ ? La seconde exige un dénominateur — un
   registre des entreprises actives et des effectifs à risque — dont l'existence n'a pas été
   vérifiée. Si ce registre n'existe pas, il faut restreindre explicitement la portée des résultats
   plutôt que de prétendre corriger une population inconnue.

2. **La stratégie de correction** (décisions 8 et 13). Le pipeline mêle aujourd'hui imputation,
   poids de non-répondants et poids AIPW artificiels. Il faut choisir une approche cohérente et
   s'y tenir.

3. **Le sort des sorties déjà produites**. Si des indicateurs issus de ce pipeline ont déjà été
   diffusés ou transmis, la question de leur retrait se pose.

---

## 8. Ce qui reste à vérifier sur données réelles

- La fréquence réelle des virgules décimales dans les fichiers source.
- La part de lignes à `ID_INDIV` null, pour mesurer l'effet de la déduplication.
- La distribution des poids IPW une fois B1 et B2 corrigés — dispersion, valeurs extrêmes.
- Les coefficients de la régression logistique : vérifier que celui de `"INCONNU"` domine
  effectivement la décision, comme le prédit B2.
- L'écart entre l'estimation actuelle et une estimation correctement pondérée, pour mesurer
  l'ampleur du biais dans les sorties déjà produites.

---

## Annexe — traçabilité

Rapports bruts des analystes délégués conservés dans le répertoire de travail temporaire de la
session :

- `codex-audit-cnps.md` — audit complet, OpenAI `gpt-5.6-sol`
- `agy-contradictoire.md` — réfutation, `gemini-3.1-pro-high`
- `agy-contradictoire-36.md` — réfutation, `gemini-3.6-flash-high`

Fichiers du dépôt lus intégralement lors de cet audit : `pipeline.py`, `orchestrator.py`, `cli.py`
(partiel), `04_base_individus.py`, `06_base_analytique.py`, `07_modele_declaration.py`,
`08_imputation_salaires.py`, `09_ponderation_finale.py`, `10_estimation_indicateurs.py`,
`12_export_excel.py`, `config/settings.yaml`, `README.md`, `docs/methodology.md`. Lus partiellement :
`05_base_entreprises.py`, `07b_modele_declaration_indiv.py`, `03_nettoyage_donnees.py`. Non lus :
`01`, `02`, `11`, `audit.py`, `jointure_anstat.py`, `storage.py`, `config.py` — les constats les
concernant proviennent des analystes délégués et sont signalés comme non vérifiés.

Voir [`note_passation_v1R_vers_v2python.md`](note_passation_v1R_vers_v2python.md) pour la
comparaison avec la version R du pipeline.
