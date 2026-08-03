# Protocole de tests synthétiques et de simulation du pipeline CNPS

**Version :** 1er août 2026
**Statut :** protocole exécutable hors données réelles ; couverture de l'inférence F.1 différée.

## 1. Objet

Ce protocole vérifie séparément :

1. les invariants de données et de temporalité ;
2. le comportement des modèles de réponse \(p_{jt}\) et \(q_{ijt}\) ;
3. le biais des estimations ponctuelles IPW à deux étages ;
4. les garde-fous de positivité, de confidentialité et de publication ;
5. à terme, la couverture de la variance F.1.

Les données CNPS sont traitées comme une population finie soumise à non-réponse. Les
simulations ne doivent donc pas ajouter une variance de tirage d'employeurs à l'estimand.
Chaque réplication génère une population finie complète, applique un mécanisme de
non-réponse, puis compare l'estimation à la valeur exacte de cette même population.

## 2. Reproductibilité

- graine maîtresse : `20260801` ;
- générateur : `numpy.random.Generator(PCG64)` ;
- une sous-graine déterministe par scénario et réplication ;
- nombre minimal de réplications : `R = 2 000` pour les tableaux finaux ;
- exécution rapide de CI : jeux déterministes et réplications réduites ;
- conservation : paramètres du DGP, version du code, graine, vérité, estimation,
  diagnostics et motif d'arrêt pour chaque réplication.

Le nombre de réplications doit être augmenté si l'erreur Monte-Carlo ne permet pas de
trancher le critère d'acceptation.

## 3. Structure commune des populations

Chaque population contient des employeurs \(j\), des mois \(t\) et des lignes
salarié–employeur–mois \(i,j,t\). Les scénarios font varier :

- le nombre d'employeurs et la concentration des tailles ;
- la corrélation intra-employeur des salaires et des réponses ;
- la persistance temporelle de \(D_{jt}\) et \(S_{ijt}\) ;
- la part de grandes entreprises ;
- la distribution des salaires, incluant une queue droite ;
- la force du recouvrement des propensions ;
- la présence de covariables catégorielles rares ;
- les mois manquants et les changements d'employeur.

La vérité de référence comprend au minimum la moyenne, les quantiles, le Gini et la
masse salariale de la population finie représentée dans le DGP.

## 4. Scénarios obligatoires

| Code | Mécanisme | Résultat attendu |
|---|---|---|
| S0 | Réponse complète | Poids égaux à un, statistiques égales à la vérité numérique. |
| S1 | MCAR aux deux étages | AUC proche de 0,5 autorisée ; estimation ponctuelle sans biais systématique. |
| S2 | MAR logistique correctement spécifié | Calibration, équilibre et biais conformes aux critères. |
| S3 | MAR non linéaire omis du modèle | Dégradation détectée par calibration/équilibre ou biais documenté ; aucune revendication de robustesse. |
| S4 | Propensions proches de zéro | Clipping/positivité déclenchent le garde-fou selon la configuration. |
| S5 | Strate catégorielle sans répondant | Arrêt pour violation structurelle de positivité. |
| S6 | Déclarations partielles dans les grandes entreprises | Le second étage réduit le biais par rapport au premier étage seul. |
| S7 | Ligne salarié totalement absente | Non-identifiabilité démontrée : \(q\) ne peut pas corriger une unité sans ligne. |
| S8 | Ruptures calendaires et multi-employeurs | Aucun historique futur ou provenant d'un autre employeur n'est utilisé. |
| S9 | Sensibilité du champ \(K\in\{6,12,24,\infty\}\) | Écarts rapportés sans choisir automatiquement une valeur de \(K\). |
| S10 | Périodicité inconnue | Résultats comparés sous hypothèses `monthly` et `daily`. |
| S11 | Cellules petites ou dominées | Secret primaire et secondaire appliqué sans fuite. |

## 5. Critères d'acceptation des points estimés

Pour chaque statistique et scénario où l'identification est satisfaite :

- biais absolu inférieur à
  \(\max(2\times SE_{MC}(\bar{\theta}), 1\%\times|\theta|)\) ;
- biais relatif, RMSE et quantiles de l'erreur toujours publiés dans le rapport de
  simulation ;
- aucune valeur non finie silencieusement convertie en résultat ;
- taux de déclenchement des garde-fous rapporté avec son erreur Monte-Carlo ;
- comparaison systématique avant/après clipping et trimming.

Le seuil de 1 % est un seuil de recette technique proposé, pas une tolérance métier.
Il doit être validé par les responsables statistiques avant homologation.

## 6. Critères futurs pour F.1

La couverture n'est pas testée tant que la linéarisation conjointe des modèles pénalisés
\(p\) et \(q\), leur covariance au niveau employeur et le traitement des statistiques
non lisses ne sont pas spécifiés.

Lorsque F.1 sera implémenté :

- couverture nominale visée : 95 % ;
- intervalle de recette proposé : 93 % à 97 % sur 2 000 réplications ;
- longueur moyenne et médiane des intervalles rapportée ;
- erreur-type analytique comparée à l'écart-type Monte-Carlo ;
- évaluation distincte pour moyenne, quantiles et Gini ;
- scénarios avec clipping/trimming analysés séparément, car la non-lissité change
  l'influence statistique.

Un bootstrap naïf de grappes d'employeurs n'est pas un substitut accepté.

## 7. Couverture actuelle de la suite hors ligne

La suite `tests/` couvre déjà : parsing numérique, clés de déduplication incomplètes,
cardinalité des jointures, âges à date, fenêtre de risque strictement passée,
covariables as-of, historique individuel multi-employeur, contexte entreprise décalé
d'un mois civil, MCAR, séparation extrême, diagnostics OOF, formule
\(D\times S/(p\times q)\), provenance des poids, trimming, secret primaire et
secondaire, variables propres à chaque statistique, validation des sorties, export
Excel, configuration, filiation et absence de pickle distant.

Les scénarios Monte-Carlo S2, S3, S6, S7, S9 et S10 restent à exécuter sur une
implémentation dédiée. Le présent document définit leur recette ; il ne prétend pas
que leurs résultats existent déjà.

## 8. Sortie attendue

Chaque campagne produit un tableau versionné contenant :

`scenario`, `replication`, `seed`, `n_employers`, `n_rows`, `truth`, `estimate`,
`error`, `relative_error`, `p_min`, `p_max`, `q_min`, `q_max`, `clipped_share`,
`trimmed_share`, `ess`, `guardrail_status`, `git_commit`, `config_hash`.

Le rapport de synthèse distingue obligatoirement :

- **vérification de code** ;
- **performance sous DGP** ;
- **hypothèse d'identification** ;
- **décision institutionnelle**.
