# Méthodologie du pipeline CNPS

Version alignée sur le code de la branche `fix/audit-phase-b` au 1er août 2026.

## 1. Objet, unité et population visée

Le pipeline décrit la distribution des salaires des couples **salarié–employeur–mois** observables dans les déclarations CNPS. Un cumul sur plusieurs mois décrit un ensemble de lignes salarié–employeur–mois : une personne présente plusieurs mois contribue plusieurs fois. Les tableaux non mensuels portent donc explicitement la mention « cumul sur la période ».

L'univers ne couvre pas :

- les employeurs absents de tous les fichiers ;
- les salariés dont aucune ligne n'existe dans les sources ;
- les salaires manquants qui ne peuvent être représentés par une ligne salarié existante.

Cette portée interdit d'interpréter les résultats comme une mesure exhaustive de tous les salariés ou de toute l'économie ivoirienne.

## 2. Données et traitement

### 2.1 Ingestion et types

Les classeurs mensuels sont convertis en Parquet. Les noms de colonnes sont normalisés. Les montants acceptent les écritures françaises et internationales (`1.234,56`, `1,234.56`, espaces simples ou insécables). Le taux d'échec de parsing est journalisé et devient bloquant au-delà de `numeric_parse_failure_threshold`.

Les durées et âges sont calculés par rapport au dernier jour du mois de déclaration, jamais par rapport à la date d'exécution. Une réexécution sur les mêmes entrées produit donc les mêmes classes d'âge et d'ancienneté.

### 2.2 Déduplication

Les doublons stricts sont retirés. Pour une clé complète `(ID_INDIV, ID_EMPLOYEUR, période)`, une seule ligne est conservée, celle au salaire le plus élevé. Les cumuls d'emplois auprès d'employeurs différents ne sont pas dédupliqués.

Une clé contenant au moins un identifiant nul n'est pas dédupliquée : la ligne est conservée et marquée `CLE_DEDUP_INCOMPLETE=1`. Cette règle évite que plusieurs personnes inconnues soient fusionnées par l'égalité artificielle de valeurs nulles.

### 2.3 Périodicité et salaire mensuel

Le salaire de référence non winsorisé est `SALAIRE_BRUT_ESTIME_AU_MOIS` :

- mensuel : montant inchangé ;
- journalier : montant multiplié par 22,4 jours ouvrés ;
- horaire : montant multiplié par 179,2 heures ;
- périodicité inconnue : hypothèse explicite `monthly` par défaut, configurable en `daily` pour sensibilité.

Le seuil de salaire est appliqué dans l'unité cohérente avec la périodicité. Le nombre de lignes à périodicité inconnue et le nombre exclu sous cette hypothèse sont journalisés.

`SALAIRE_BRUT_ESTIME_AU_MOIS_W` est une copie winsorisée après conversion. La moyenne et la variance descriptive utilisent cette colonne. Les quantiles, le Gini et les extrêmes observés utilisent la colonne non winsorisée.

## 3. Panel entreprise et champ à risque

### 3.1 Définition de la réponse employeur

Pour l'employeur `j` au mois `t` :

```text
D_jt = 1 si EFFECTIF_DECLARE > 0, sinon 0
```

`D_jt=1` implique par construction un salaire moyen positif et fini. Toute coexistence de `D_jt=1` avec un salaire moyen nul, négatif, manquant ou non fini provoque un échec.

### 3.2 Bornes du panel

La borne gauche est le **plus tôt** entre le mois d'immatriculation de l'employeur et son premier mois de déclaration observé ; à défaut de date d'immatriculation, la première apparition. Une déclaration observée prouve l'existence de l'entreprise : elle ne peut pas être écartée du panel par une date d'immatriculation qui la dirait postérieure. Ces employeurs sont marqués `DECLARATION_AVANT_IMMAT` et dénombrés dans les journaux. La date d'immatriculation est lue sur le premier mois où elle est renseignée, et non sur le premier mois observé. Une troncature au début du panel et l'imputation de cette borne sont marquées séparément.

Le panel couvre par construction 100 % des couples (employeur, mois) observés ; un écart interrompt l'étape 05.

Aucune cessation n'est inférée de la dernière déclaration. Faute de registre de radiation, chaque entreprise est prolongée jusqu'à la fin commune du panel. Les journaux distinguent troncatures gauches, débuts imputés, cessations observées (zéro en l'absence de registre) et fins censurées.

### 3.3 Fenêtre glissante

Pour le mois `t`, `DANS_UNIVERS_RISQUE=1` si l'employeur a déclaré au moins une fois pendant les `K` mois **strictement antérieurs**. La valeur par défaut est `K=12`; `inf` conserve toutes les périodes postérieures à une déclaration passée. Le mois courant n'entre jamais dans la définition du champ, car cela ferait dépendre l'univers de la cible à prédire.

`FENETRE_RISQUE_EXTENSIBLE` signale les mois où moins de `K` mois d'historique sont disponibles. L'ancienneté se mesure depuis l'entrée de l'employeur dans le panel, et non depuis le début commun : une entreprise entrée tardivement a bien un historique tronqué, quelle que soit la profondeur du panel.

L'amorce, où moins de `K` mois antérieurs existent, est marquée `FENETRE_RISQUE_EXTENSIBLE`. Avant diffusion, les indicateurs centraux doivent être rejoués pour `K ∈ {6, 12, 24, inf}`.

### 3.4 Covariables as-of

Secteur, commune et taille sont propagés uniquement par dernière valeur antérieure connue. Aucune rétropropagation ne vient du futur. L'âge de l'entreprise est recalculé à chaque mois. Des indicateurs `JAMAIS_OBSERVE_AVANT_*` distinguent l'absence d'information antérieure d'une modalité économique réelle.

## 4. Modèles de réponse

### 4.1 Premier étage : employeur

Le premier modèle estime :

```text
p_jt = P(D_jt = 1 | X_jt)
```

par régression logistique L2. Les covariables disponibles à date incluent secteur, taille, âge d'entreprise, réponse du mois précédent, taux de réponse passé et indicateurs de début/manque d'historique.

### 4.2 Second étage : ligne salarié

Pour une ligne salarié existante dans une entreprise déclarante :

```text
S_ijt = 1 si le salaire est positif et renseigné
q_ijt = P(S_ijt = 1 | D_jt = 1, X_ijt, Z_jt)
```

L'historique est calculé par couple `(ID_INDIV, ID_EMPLOYEUR)`. Un décalage n'est accepté comme « mois précédent » que si l'écart calendaire vaut exactement un. La complétude de l'entreprise vient de `firm_base` et est décalée d'un mois civil avant d'être jointe à toutes les lignes du couple entreprise–mois.

`q_ijt` ne modélise pas un salarié entièrement omis du fichier, puisqu'aucun panel salarié exhaustif n'est disponible.

### 4.3 Diagnostics hors échantillon

Les prédictions sont out-of-fold avec plis groupés par employeur. Sont calculés : AUC, score de Brier, pente de calibration, calibration-in-the-large, support des propensions et déséquilibre maximal des covariables après pondération.

L'AUC est descriptive. Une AUC de 0,5 est compatible avec un mécanisme MCAR et des poids corrects. Les motifs de blocage sont : classe cible unique, prédictions non finies, absence de recouvrement, mauvaise calibration, déséquilibre résiduel excessif, part de propensions clippées trop élevée et strate structurellement jamais répondante.

Les résumés de modèles sont des JSON non exécutables. Une pente non identifiable sous score quasi constant est enregistrée comme `null`.

## 5. Poids et estimateurs

### 5.1 Poids final

L'indicateur de réponse complet est :

```text
R_ijt = D_jt × S_ijt
```

Le poids brut est :

```text
W_FINAL_RAW = R_ijt / (p_hat_jt × q_hat_ijt)
```

Le facteur `q` est neutre hors du domaine conditionnel (`q=1`, `W_INDIV=1`), mais `R_ijt=0` y impose un poids final nul. Les lignes hors univers à risque reçoivent également zéro. Les poids positifs sont tronqués aux quantiles configurés; la part tronquée est contrôlée.

Il ne s'agit pas d'un poids stabilisé : aucun taux marginal ne figure au numérateur. Il n'est pas non plus normalisé à moyenne 1. `n_weighted` n'est pas publié.

### 5.2 Statistiques

La moyenne est l'estimateur ratio de Hájek :

```text
mu_hat = somme(w_i y_i) / somme(w_i)
```

La variance publiée est la dispersion descriptive pondérée avec correction de Kish. Ce n'est pas la variance de l'estimateur de moyenne.

Les quantiles utilisent une fonction de répartition pondérée centrée :

```text
F_i = (cumul_w_i - w_i/2) / somme(w)
```

Le Gini est pondéré selon la formulation de Lerman–Yitzhaki. Les minimum et maximum sont des extrêmes **observés** parmi les contributeurs à poids positif.

### 5.3 Dimension temporelle

Des séries mensuelles sont produites pour : national, secteur, sexe et taille réduite. Les dimensions fines restent cumulées et leur libellé indique « cumul sur la période ».

## 6. Inférence : état volontairement limité

Les données sont traitées comme une population finie affectée par la non-réponse, non comme un échantillon tiré d'une superpopulation. Un bootstrap naïf d'employeurs ajouterait une variance de tirage qui ne correspond pas à ce cadre.

L'incertitude pertinente vient notamment de l'estimation conjointe de `p_hat` et `q_hat`, de leur covariance au niveau employeur et des non-linéarités introduites par clipping, trimming, quantiles et Gini. Cette variance n'est pas encore spécifiée de façon suffisamment complète.

Conséquence :

- `inference_method` doit valoir `point_only` ;
- chaque ligne publiée porte `POINT_ONLY_F1_PENDING` ;
- toute colonne d'intervalle ou d'erreur-type est rejetée à la validation ;
- l'imputation et les règles de Rubin sont hors publication ;
- aucune revendication AIPW ou de double robustesse n'est faite.

## 7. Secret statistique

Une cellule est supprimée pour toutes ses statistiques si au moins une règle échoue :

- moins de 30 individus distincts contributeurs ;
- moins de 3 employeurs distincts contributeurs ;
- un employeur représente plus de 85 % de la masse salariale observée non pondérée.

Les identifiants nuls ne comptent pas. Un contributeur doit avoir un salaire fini positif et un poids final positif. Si une seule cellule est supprimée dans une marge additive, la plus petite cellule publiée de cette même marge est supprimée en secondaire. Pour les tableaux mensuels croisés, cette règle s'applique séparément dans chaque mois.

Les seuils sont une garde de prépublication et non une doctrine institutionnelle définitive.

## 8. Validation, export et filiation

L'étape 11 revalide le schéma et les diagnostics OOF des résumés JSON, exige la
présence des deux modèles, vérifie la cohérence de `W_FINAL` avec `D×S`, calcule
l'effectif efficace (ESS), contrôle que les statistiques publiées sont finies et
ordonnées, puis vérifie le masquage des cellules. Toute erreur empêche l'étape 12.

Excel applique trois décimales au Gini, des formats adaptés aux autres statistiques, et remplace les valeurs nulles ou non finies par un tiret.

Chaque session reçoit un UUID et un manifeste comprenant l'empreinte de configuration, le commit, l'état sale du dépôt, les versions logicielles et la chaîne des sorties déclarées. Les secrets sont exclus. Les sorties canoniques restent réécrites en place : le manifeste le signale comme limite de reproductibilité historique.

## 9. Validation hors ligne et limites ouvertes

La suite synthétique compte 71 tests. Elle couvre notamment : parsing numérique,
déduplication à clés nulles, cardinalité et unicité des jointures, calcul temporel,
fenêtre de risque, absence d'information future, historique multi-employeur,
contexte mensuel, MCAR, séparation des propensions, formule et provenance des
poids, secret primaire/secondaire, routage des variables, revalidation des
diagnostics sauvegardés, validation des sorties, export et configuration invalide.

Le protocole `docs/protocole_tests_dgp.md` définit les scénarios Monte-Carlo,
graines, sorties et critères de recette encore nécessaires pour étudier le biais,
les limites d'identification et, après implémentation de F.1, la couverture.

Les vérifications sur valeurs réelles, les sensibilités `K` et la comparaison CIAP/comptabilité nationale nécessitent les entrées MinIO et ne sont pas revendiquées dans cette version hors VPN.

Les limites méthodologiques ouvertes sont :

1. variance F.1 et validation de couverture ;
2. absence de registre de cessation ;
3. absence de panel salarié exhaustif pour les lignes totalement omises ;
4. objets canoniques MinIO non immuables ;
5. validation institutionnelle des seuils de confidentialité et de la table de passage CIAP–NAPCN.

## Références principales

- Hájek, J. (1971), *Comment on An Essay on the Logical Foundations of Survey Sampling*.
- Kish, L. (1965), *Survey Sampling*.
- Lerman, R. I. et Yitzhaki, S. (1989), « Improving the Accuracy of Estimates of Gini Coefficients ».
- Lumley, T. (2010), *Complex Surveys*.
- Wooldridge, J. M. (2007), « Inverse Probability Weighted Estimation for General Missing Data Problems ».
