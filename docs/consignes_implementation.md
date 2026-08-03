# Consignes d'implémentation — correction du pipeline CNPS

**Destinataire** : tout agent de codage ou développeur qui implémente `plan_correction_v2.md`.
**Branche de travail** : `fix/audit-phase-b`, créée depuis `refacto-cnps-minio` au commit `2ed8ae5`.

Ce document dit **comment** implémenter le plan. Le **quoi** est dans
[`plan_correction_v2.md`](plan_correction_v2.md), le **pourquoi** dans
[`rapport_audit_et_decisions.md`](rapport_audit_et_decisions.md).

---

## 1. État d'avancement

| Mission | Lots | Statut |
|---|---|---|
| **1** | B.1, B.2, B.3, B.4, A.3, G.3, T.1 | **Livrée** dans l'arbre de travail, non commitée |
| **2** | A.1, A.2, A.4, C.1, C.2, D.1, D.2, D.3, D.4, E.1, E.2, E.3, E.4, E.5 | En cours |
| **3** | F.1, F.2, F.3, G.1, G.2, G.4, H.1, H.2, H.3, I.1, I.2, T.2, T.3, Z.1 | À faire |

Le découpage n'est pas arbitraire. **F dépend de E** : calculer une variance sur une chaîne de
pondération encore en chantier produit des intervalles faux d'apparence crédible. **Z.1 vient en
dernier** : il documente le résultat des autres.

Un premier essai en une seule mission de 28 lots a été interrompu par une panne réseau après 1 h 30,
sans rien produire. Découper limite la perte en cas d'incident.

---

## 2. Règles permanentes

1. **Ne pas commiter, ne pas pousser, ne pas changer de branche.** Le travail reste dans l'arbre de
   travail pour permettre la relecture du diff avant intégration.
2. **Ne jamais modifier** `plan_correction_v2.md`, `rapport_audit_et_decisions.md`,
   `note_passation_v1R_vers_v2python.md`, ni le présent fichier. Ce sont des documents d'analyse.
3. **Le lot Z.1 fait exception** : il doit au contraire modifier `note_methodologique_traitement.tex`
   et son PDF, `methodology.md`, et le `README.md` à la racine.
4. **Style du dépôt** : commentaires et docstrings en français sans accents, `loguru` pour la
   journalisation, `polars` pour les données. Les messages de log doivent être explicatifs — ce
   dépôt journalise le *pourquoi*, pas seulement le *quoi*, et cette convention a de la valeur.
5. **Pas d'accès MinIO.** Ne pas tenter d'exécuter le pipeline. Les vérifications passent par des
   tests unitaires sur jeux synthétiques.
6. **Un lot mal spécifié ne s'implémente pas au jugé.** Le laisser de côté, poursuivre les autres,
   et expliquer ce qui bloque. Un lot non fait et signalé vaut mieux qu'un lot fait sur une
   hypothèse inventée — a fortiori en phase F.

---

## 3. Les pièges — contraintes contre-intuitives

Ces points ont été établis par un audit croisé puis une évaluation critique qui a **invalidé dix
propositions d'une première version du plan**. Ce sont précisément les endroits où un implémenteur
compétent mais sans contexte prendra la mauvaise décision. Ne les réinterpréter pas.

### F.1 — Population finie, pas superpopulation

Les données sont un **recensement** des déclarations, pas un échantillon. **Un bootstrap naïf
d'employeurs serait faux** : il injecterait une variance de tirage qui n'existe pas. L'incertitude
réelle vient de ce que `p̂` et `q̂` sont *estimés*.

Utiliser une variance par linéarisation propageant l'incertitude d'estimation des modèles de
réponse. Tout rééchantillonnage doit être justifié explicitement et ne servir qu'à cette
propagation.

### C.1 — Ne jamais inférer une cessation d'une absence de déclaration

Une dernière déclaration ancienne peut être une longue **non-réponse terminale**, pas une fermeture.
Aucune source de cessation n'existe dans les données — vérifié : seules `DATE_IMMAT_EMPLOYEUR`,
`DATE_IMMATRICULATION`, `DATE_NAISSANCE` et `DATE_EMBAUCHE` sont disponibles, et le référentiel
ANSTAT n'apporte aucun statut d'activité.

La fenêtre glissante de **K = 12 mois** est un **énoncé de portée** — « entreprises ayant déclaré au
moins une fois dans les 12 mois précédant le mois estimé » — et non une inférence sur la cessation.
Borner à droite par la dernière déclaration effacerait le phénomène même que le modèle doit
expliquer.

- borne gauche : `DATE_IMMAT_EMPLOYEUR`, à défaut première apparition, avec indicateur de début imputé ;
- borne droite : fin du panel pour toutes les entreprises ;
- fenêtre extensible sur l'amorce du panel, où 12 mois d'historique n'existent pas ;
- **K configurable**, une analyse de sensibilité sur K ∈ {6, 12, 24, ∞} devant rester possible.

### C.2 — `CL_AGE_ENTREPRISE` n'est pas un attribut invariant

Elle dérive d'une durée (`03:577`, `:604-607`) et varie mécaniquement chaque mois : la recalculer
pour chaque mois du panel, jamais la propager.

Ne propager que les attributs réellement constants, et uniquement par **dernière valeur antérieure
connue** — jamais une valeur postérieure au mois considéré, ce qui introduirait de l'information
future.

Créer des indicateurs « jamais observé auparavant » plutôt qu'une modalité `"INCONNU"` fourre-tout :
c'est cette modalité, quasi colinéaire avec la non-déclaration, qui fausse aujourd'hui le modèle de
propension.

### D.3 — Le seuil d'AUC ne doit pas être bloquant

Sous un mécanisme MCAR, le vrai modèle a une propension constante, une AUC de 0,5, et produit des
poids **parfaitement corrects**. À l'inverse, une AUC élevée peut accompagner un défaut de
positivité. L'AUC reste **descriptive**.

Bloquer sur : classe cible unique, prédictions non finies, absence de recouvrement, calibration
grossièrement fausse. Le lot vise les **deux** modèles, 07 et 07b — le second calcule lui aussi son
AUC en apprentissage.

### D.1 — La table entreprise-période vient de `firm_base`, pas d'`analytical_base`

`analytical_base` part des individus observés (`06:46`, `:67`) et ne contient pas les mois totalement
absents. Un décalage calculé dessus donnerait le *mois observé précédent*, pas le *mois civil
précédent*.

### E.2 — N'activer `07b` qu'après avoir livré D.1

`07b` contient aujourd'hui une fuite de données : `completude.shift(1).over("ID_EMPLOYEUR")` décale
d'une **ligne** sur un DataFrame individuel, si bien que dans une entreprise de 200 salariés, 199
lignes reçoivent la complétude du mois courant — fonction directe de la variable à prédire.
L'ajouter au `Stage` enum avant correction brancherait un modèle qui triche.

### E.3 — Le poids d'analyse doit contenir les facteurs de réponse

`w_ijt = R_ijt / (p̂_jt · q̂_ijt)` avec `R_ijt = D_jt × S_ijt`.

Aujourd'hui les lignes non répondantes gardent un poids strictement positif **et** reçoivent un
salaire imputé à l'étape 10 : c'est un double comptage. Noter l'incohérence interne actuelle — le
chemin AIPW met bien les non-répondants à zéro (`09:74`), la branche IPW ne le fait pas (`09:205`).

### E.1 — Interaction connue avec le garde-fou déjà livré

L'étape 07 remplit `W_JT = 1.0` sur les lignes non modélisées en laissant `P_HAT_JT` à null. Le
contrôle de provenance du lot B.1, déjà livré, rejettera ces lignes. Leur sort doit être **décidé
explicitement et documenté** — pas rempli par défaut en silence.

### E.4 — L'AIPW est abandonné, pas réparé

Supprimer le facteur `aug_ratio` et son écrêtage arbitraire à [0,5 ; 2,0], ainsi que le calcul de
`mu_aipw` qui n'est que journalisé. Le cadrage retenu est un **IPW à deux étages**. Ne pas chercher à
corriger l'estimateur doublement robuste : il exigerait un `m(X)` pour toutes les unités, que
l'étape 08 ne produit pas.

### A.1 — `StatDef` n'a pas de champ `variable`

Modifier `config.py` — la dataclass **et** le chargeur — pas seulement `dimensions.yaml`. Le lot
doit couvrir *toutes* les statistiques, y compris variance, médiane, quartiles et décomptes. Les
extrêmes (minimum, maximum) ne sont pas affectés par les poids : les étiqueter « observés » ou les
retirer.

### B.1 — Un échec du chemin nominal est le comportement attendu

Les contrôles de provenance des poids font échouer `python run.py run`, parce que `P_HAT_JT`
n'atteint jamais la base analytique tant que E.1 n'est pas livré. **C'est voulu** : le pipeline
produit aujourd'hui des statistiques non pondérées sans le signaler. Ne pas relâcher le contrôle
pour faire passer le pipeline.

### B.1 bis — Ne pas tester la constance des poids

Les poids sont stabilisés (`w = P(D=1)/p̂`). Sous MCAR à propension constante, **tous les poids
valent légitimement 1**. Un test bloquant produirait des faux positifs. Journaliser en
avertissement, jamais en erreur.

### B.2 — Ne pas tester l'égalité des estimations entre imputations

Avec une variance intra-imputation correcte (lot F.2), des estimations identiques peuvent produire
un intervalle parfaitement valide. Tester `total_var`, l'erreur-type et les bornes — pas l'égalité
des `Q_m`.

---

## 4. Environnement de test

Le Python système ne dispose pas de `loguru`. La mission 1 a été validée avec un substitut de
journalisation temporaire : **ses 40 tests n'ont pas été rejoués dans un environnement propre**.

Créer un environnement virtuel et y installer les dépendances (`pip install -e .`) pour que les
tests s'exécutent réellement. En cas d'échec — réseau indisponible, résolution hors ligne — le dire
explicitement dans le compte rendu plutôt que de contourner en silence. **Ne pas laisser
d'environnement résiduel dans le dépôt.**

---

## 5. Livrable attendu de chaque mission

Un compte rendu en français contenant :

1. les fichiers modifiés ou créés, avec en une phrase ce qui a changé dans chacun ;
2. **pour chaque lot** : atteint, partiellement atteint ou non fait, et pourquoi ;
3. les décisions prises faute de spécification, et ce qui a été retenu ;
4. les lots non faits et ce qui les bloque ;
5. le résultat de l'exécution des tests, en précisant l'environnement utilisé ;
6. les points où le plan lui-même paraît discutable.

Les points 4 et 6 sont les plus utiles. Un compte rendu qui ne signale aucune difficulté sur un lot
de cette nature est un compte rendu incomplet.

---

## 6. Relecture avant intégration

Aucun lot ne doit être commité sans que le **diff** ait été relu — pas seulement le compte rendu de
l'agent. La relecture de la mission 1 a montré pourquoi : le compte rendu était fidèle, mais deux
éléments n'y figuraient pas.

- Une décision substantielle non listée : `_has_valid_rubin_interval` rejette aussi les intervalles
  à borne basse négative.
- Un gain non identifié : sous l'ancienne sémantique à borne haute exclusive, les classes d'effectif
  `{1,1}`, `{2,5}`, `{6,10}`… laissaient **sans classe** les entreprises de 1, 5, 10, 20, 50, 100,
  200, 500 et 1000 salariés. Elles devenaient `"INCONNU"` dans le modèle de propension. Le passage
  aux bornes inclusives corrige un défaut réel que l'audit n'avait pas vu.

Points de vigilance pour la relecture des missions suivantes : **C.1**, où une erreur de bornage
effacerait le phénomène étudié, et **F.1**, où une variance fausse produirait des intervalles
crédibles mais erronés — le pire cas, puisque rien ne le signalerait.
