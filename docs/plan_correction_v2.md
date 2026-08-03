# Plan de correction — pipeline CNPS v2

**Base** : `refacto-cnps-minio`, commit `2ed8ae5`
**Référence des constats** : `rapport_audit_et_decisions.md`
**Version** : 2 — révisée après évaluation critique par `gpt-5.6-sol`, qui a invalidé plusieurs lots
de la version 1. Les changements de fond sont récapitulés en annexe.

Chaque lot est indépendamment livrable et testable, et précise le fichier visé, le changement
attendu et le **critère d'acceptation** — la condition vérifiable qui permet de le déclarer terminé.

---

## 0. Décision de cadrage — à valider avant toute implémentation

La version 1 de ce plan proposait un AIPW au niveau entreprise. **C'était une erreur** : l'estimand
publié est au niveau salarié — l'étape 10 estime sur la base individuelle
(`10_estimation_indicateurs.py:379`) et ventile par sexe, âge et profession
(`config/dimensions.yaml:22`, `:88`). Une moyenne de pseudo-résultats entreprise estimerait une
**moyenne de moyennes d'entreprises**, pas la moyenne des salariés.

S'y ajoute un obstacle matériel : l'AIPW exige un modèle de résultat `m(X)` pour **toutes** les
unités. Or l'étape 08 ne prédit que les non-déclarants et recopie la valeur observée pour les
déclarants (`08_imputation_salaires.py:185`, `:224`) : `firm_base_imputed.parquet` ne contient donc
pas `m(X)` pour les unités observées.

### Cadrage retenu

> **Estimand** : distribution des salaires des couples **salarié-employeur-mois** représentables par
> les employeurs présents au moins une fois dans les fichiers sources.
>
> **Méthode** : pondération par probabilité inverse à deux étages, sans imputation dans le chemin de
> publication.
>
> ```
> R_ijt = D_jt × S_ijt        (indicateur de réponse observée)
> w_ijt = R_ijt / (p̂_jt × q̂_ijt)
> μ̂     = Σ w_ijt · Y_ijt / Σ w_ijt        (estimateur ratio de Hájek)
> ```
>
> **Hors champ, à déclarer explicitement dans toute diffusion** : les employeurs absents de
> l'intégralité du panel, et les salariés dont aucune ligne n'existe dans aucune source.

### Arbitrages rendus — méthodologie de révision

Décisions prises le 01/08/2026, après vérification dans le code et lecture de la note
méthodologique. Chacune indique les lots qu'elle débloque. **Toutes doivent être répercutées dans
`docs/note_methodologique_traitement.tex` en fin de chantier — voir le lot Z.1.**

1. **Unité temporelle — `PERIOD` devient une dimension d'analyse.**
   Aucune dimension temporelle n'est configurée aujourd'hui : les dix dimensions actives sont
   National, Secteur, Tranche d'âge, Ancienneté entreprise, Ancienneté d'immatriculation, Âge
   entreprise, Taille détaillée, Taille réduite, Sexe et Commune. Les tableaux publiés **cumulent
   donc tous les mois du panel**, et un salarié présent 24 mois y pèse 24 fois plus qu'un salarié
   présent un mois.

   Décision : publier une **série mensuelle** pour les agrégats robustes (National, Secteur, Sexe,
   Taille réduite) et conserver les ventilations fines en cumul. Le cumul n'est pas faux, il est
   aujourd'hui **silencieux** : tout tableau cumulé doit porter la mention « cumul sur la période ».
   L'estimand devient explicite : *salaire moyen des salariés déclarés du mois t*.

   Effet de bord à anticiper : mensualiser divise les effectifs par le nombre de mois et **augmente
   mécaniquement la suppression pour secret**. Les croisements fins (Commune × mois, Âge × mois)
   seront largement vides — d'où la restriction aux agrégats robustes.
   → *Débloque* : F.1, G.1, G.2.

2. **Cadre d'inférence — population finie avec non-réponse.**
   Les données CNPS sont un **recensement** des déclarations, pas un échantillon : il n'existe pas
   de variance d'échantillonnage au sens classique. L'incertitude réelle vient de ce que la
   probabilité de déclarer est **modélisée** : `p̂` et `q̂` sont estimés, et cette erreur se propage
   aux poids puis aux indicateurs.

   Conséquence directe pour le lot F.1 : **un bootstrap naïf d'employeurs serait faux** — il
   injecterait une variance de tirage qui n'existe pas. Il faut une variance par linéarisation
   propageant l'incertitude d'estimation des modèles de réponse.

   *Réserve documentée* : si l'objectif devenait de décrire la structure des salaires du pays en
   général, et non les salariés déclarés d'une période donnée, le cadre superpopulation se
   défendrait. Pour une statistique officielle décrivant une période observée, la population finie
   est le bon choix.
   → *Débloque* : toute la phase F.
3. ~~**Registre d'activité.**~~ **TRANCHÉ (01/08/2026) : il n'existe aucune source de cessation.**
   Vérification faite sur le code et la configuration, les seules dates disponibles sont
   `DATE_IMMAT_EMPLOYEUR`, `DATE_IMMATRICULATION`, `DATE_NAISSANCE` et `DATE_EMBAUCHE`. Aucun champ
   de radiation, de cessation ni de statut d'activité employeur. Le référentiel ANSTAT n'apporte que
   secteur, forme juridique, RCCM et DFE (`jointure_anstat.py:41-45`).

   **Conséquence** : la borne gauche du panel est identifiable, la borne droite ne l'est pas.
   Cessation et non-réponse terminale sont observationnellement identiques. Voir le lot C.1, révisé
   en conséquence.

   *Question résiduelle pour l'équipe* : le fichier ANSTAT source contient-il d'autres colonnes que
   les quatre reprises aujourd'hui ? Une date de radiation y serait décisive.

4. **Univers à risque — fenêtre d'activité glissante de K = 12 mois.**
   Faute de registre de cessation, le champ est défini comme : *entreprises ayant déclaré au moins
   une fois dans les 12 mois précédant le mois estimé*.

   **Distinction essentielle** : on ne prétend pas qu'une entreprise a fermé. On énonce une
   **portée**, pas une inférence sur la cessation. C'est ce qui rend le choix défendable sans
   registre, et ce qui évite le piège signalé lors de l'évaluation du plan v1 — confondre cessation
   et non-réponse terminale.

   Pourquoi 12 : cale sur le cycle annuel, absorbe saisonnalité et déclarations annuelles, et reste
   très en deçà de la longueur du panel (~23 fichiers mensuels).

   Deux points d'exécution obligatoires :
   - **troncature à gauche** : les premiers mois du panel n'ont pas 12 mois d'historique. Utiliser
     une fenêtre extensible sur cette amorce et marquer ces mois dans la sortie ;
   - **analyse de sensibilité** : rejouer avec K ∈ {6, 12, 24, ∞} et publier l'écart sur les
     indicateurs principaux. Un écart faible clôt la question ; un écart fort impose de rechercher
     une source de radiation.
   → *Débloque* : C.1, C.2, phase D.

5. **Seuils de secret statistique.**

   | Paramètre | Valeur | Justification |
   |---|---|---|
   | `min_distinct_individuals` | 30 | Conserve l'intention du seuil actuel, mais sur des personnes distinctes et non des lignes |
   | `min_distinct_employers` | 3 | Empêche qu'une cellule révèle la masse salariale d'une entreprise identifiable |
   | `max_employer_wage_share` | 0,85 | Règle de dominance (1,85) usuelle |

   **La dominance porte sur la masse salariale observée**, non pondérée : le risque de divulgation
   concerne ce qu'un lecteur peut déduire d'un employeur **réel**, donc sa contribution effective et
   non une contribution estimée par un modèle.

   **Suppression secondaire indispensable** : `National` étant publiée à côté de `Secteur`, `Sexe`
   et `Commune`, une cellule masquée se reconstitue par différence des marges. Règle retenue : si
   une seule cellule d'une ventilation est supprimée en primaire, supprimer aussi la suivante la
   plus petite.

   **Réserve** : ces seuils relèvent normalement d'une politique institutionnelle. Si l'ANStat
   dispose d'une doctrine de confidentialité écrite, elle **prime** sur ces valeurs, à vérifier
   avant de les figer.
   → *Débloque* : G.1.

6. **`orchestrator.py` est déprécié.** Maintenir deux orchestrateurs en cohérence est une dette
   permanente, et `pipeline.py` couvre l'ensemble des besoins.
   → *Débloque* : B.3, E.2.

7. **`dimensions.yaml` devient normatif, avec bornes inclusives**, et `_classify` (`03:54`) est
   corrigé pour s'y conformer. Les bornes codées en dur à l'étape 03 sont supprimées.
   → *Débloque* : A.3.

**L'AIPW est différé.** Il pourra être repris une fois l'estimand stabilisé, mais il exigera alors
soit un registre salarié-employeur-mois avec un `m(X_ijt)` par unité, soit une modélisation séparée
de la masse salariale et de l'effectif par domaine publié. Dans les deux cas le chantier porte sur
les étapes 08, 09 **et** 10, pas sur la seule 09.

---

## Phase A — Fondations *(préalable à toute modélisation)*

Ces lots changent les variables et les identifiants sur lesquels les modèles seront estimés. Les
placer après la modélisation obligerait à tout réestimer.

### Lot A.1 — Séparer salaire brut et salaire winsorisé
**Fichiers** : `03_nettoyage_donnees.py`, `10_estimation_indicateurs.py`, `config.py`,
`config/dimensions.yaml`

L'étape 03 écrase le salaire en place tout en avertissant, dans son propre commentaire, que le Gini
et les statistiques de queue ne doivent pas être calculés sur cette variable. L'étape 10 l'utilise
pourtant pour tout (`10:520`).

Conserver deux colonnes : `SALAIRE_BRUT_ESTIME_AU_MOIS` (non winsorisé) et
`..._W` (winsorisé). Ajouter un champ `variable` à `StatDef` — il n'en possède aucun aujourd'hui
(`config.py:124`) — et le faire lire par le chargeur (`config.py:268`). Déclarer dans
`dimensions.yaml` la variable de **chaque** statistique, y compris variance, médiane, quartiles et
décomptes, que la version 1 omettait.

`weighted_quantile` étant appliqué au minimum et au maximum sans effet des poids
(`10:147`), ces deux extrêmes doivent être étiquetés « observés » dans l'export, ou retirés : ils ne
peuvent pas être présentés comme corrigés de la non-réponse.

**Critère d'acceptation** : chaque statistique déclare sa variable ; minimum et maximum ne
coïncident plus avec les bornes de winsorisation ; aucune statistique n'utilise une variable
implicite.

### Lot A.2 — Rendre les durées indépendantes de la date d'exécution
**Fichier** : `03_nettoyage_donnees.py`

`ref_date = date.today()` (`03:416`) rend les classes d'âge et d'ancienneté dépendantes du jour de
retraitement.

> Date de référence = dernier jour civil du couple `ANNEE`–`MOIS`. Calculer le nombre d'années
> révolues, sans approximation `jours / 365,25`. Refuser ou masquer les dates postérieures à la
> référence.

Le lot couvre l'âge du salarié, son ancienneté, sa date d'immatriculation et l'âge de l'employeur.

**Critère d'acceptation** : un retraitement à une date simulée ultérieure reproduit exactement les
mêmes classes sur les mêmes observations, vérifié par test.

### Lot A.3 — Valider la configuration au chargement
**Fichiers** : `pyproject.toml`, `config.py`, `config/settings.yaml`

Décision par paramètre, au lieu du « supprimer ou brancher » non exécutable de la version 1 :

- ajouter `pandas` aux dépendances — quatre `.to_pandas()` l'utilisent et il est absent ;
- **conserver** `statsmodels` jusqu'à ce que la phase F ait tranché l'implémentation de la variance ;
- supprimer `file_pattern` et `encoding`, inutiles pour l'ingestion Excel actuelle ;
- brancher réellement la section `logging` ;
- valider l'énumération `estimation_method ∈ {ipw}` au chargement — `aipw` et `tmle` doivent être
  refusés avec un message explicite tant que la décision de cadrage n'est pas levée ;
- fixer la convention des bornes de classes : `min: 1, max: 1` dans `dimensions.yaml:67` est
  inclusif alors que `_classify` utilise une borne haute exclusive (`03:54`). Trancher, documenter,
  et faire de `dimensions.yaml` la source unique.

**Critère d'acceptation** : une installation vierge exécute le pipeline jusqu'à l'étape 08 ; toute
valeur de configuration non implémentée est refusée au chargement.

### Lot A.4 — Filiation des artefacts
**Fichiers** : `pipeline.py`, `storage.py`

Les sorties portent des noms fixes : l'étape 10 active les règles de Rubin dès que
`firm_base_imputed.parquet` existe, sans savoir de quelle exécution il provient.

> Tous les artefacts sont immuables sous `sessions/{uuid}/…`. Chaque artefact porte les
> identifiants/ETag de ses parents, le hash canonique des configurations, le commit Git et l'état
> *dirty*, la graine et la version des dépendances. Les commandes partielles exigent un `session_id`
> compatible ou résolvent explicitement une session complète. Aucun « dernier fichier existant »
> implicite.

**Critère d'acceptation** : l'étape 10 refuse un artefact dont le manifeste ne correspond pas à la
base analytique courante ; deux exécutions concurrentes ne s'écrasent pas.

---

## Phase B — Garde-fous

Ces lots ne corrigent rien : ils rendent les défauts bruyants. Livrables immédiatement.

### Lot B.1 — Contrôler la provenance des poids, pas leur constance
**Fichier** : `09_ponderation_finale.py`

La version 1 proposait de lever une exception si tous les `W_JT` valent 1. **C'était faux** : les
poids sont stabilisés (`w = P(D=1)/p̂`, `07:193`), donc sous un mécanisme MCAR à propension
constante, tous les poids valent légitimement 1 alors que `D_JT` varie.

Contrôler la provenance et la jointure, non la distribution :

- colonnes `P_HAT_JT` et `W_JT` obligatoires, quel que soit `estimation_method` ;
- clé `(ID_EMPLOYEUR, PERIOD)` unique côté `firm_base` ;
- jointure plusieurs-vers-un sans perte ni doublon ;
- **aucun remplissage par défaut à 1,0 après la jointure** — une clé sans correspondance est une
  erreur, pas une valeur manquante ;
- `0 < P_HAT_JT < 1`, poids finis, conformité au manifeste de session.

Le test de constance est conservé comme **avertissement** diagnostique, jamais comme blocage.

**Critère d'acceptation** : une base analytique dépourvue de `P_HAT_JT` fait échouer l'étape 09 avec
un message nommant l'étape à exécuter ; un jeu MCAR à poids unitaires légitimes passe sans erreur.

### Lot B.2 — Supprimer les intervalles dégénérés sur le bon critère
**Fichier** : `10_estimation_indicateurs.py`

La version 1 proposait de supprimer l'IC quand les M estimations sont identiques. **Ce critère
casserait le lot F.2** : après calcul d'une vraie variance intra-imputation, des `Q_m` identiques
peuvent parfaitement donner une variance totale positive et un IC valide.

Tester `total_var` directement : supprimer l'IC s'il est non fini, négatif, ou nul dans un cas non
déclaré dégénéré. Préciser dans la sortie si seul l'IC est masqué ou également l'estimation
ponctuelle.

**Critère d'acceptation** : une cellule à variance totale positive conserve son IC même si les `Q_m`
coïncident ; une variance non finie produit une cellule masquée avec avertissement.

### Lot B.3 — Propager l'échec jusqu'au code de retour
**Fichiers** : `cli.py`, `pipeline.py`, `11_validation_qualite.py`, `orchestrator.py`

`run()` se termine sans `typer.Exit(1)` quand `PipelineResult.success` est faux (`cli.py:109-125`).
Étendre à `ingest`, `clean`, `model`, `estimate`.

Le problème dépasse la CLI. `pipeline.py:117` et `:123` jettent la valeur renvoyée par chaque étape ;
`orchestrator.py:76` autorise explicitement l'export après un échec de validation ; l'étape 11
autonome quitte en succès malgré un rapport en erreur (`11:333`) ; l'étape 12 autonome recalcule
l'estimation (`12:207`).

**Décision requise** : soit déprécier `orchestrator.py`, soit persister l'artefact de résultats de
l'étape 10 et le rapport de validation sous le même identifiant de session, de sorte que les deux
chemins se comportent identiquement.

**Critère d'acceptation** : dans les deux orchestrateurs, un rapport de validation invalide empêche
l'export et produit un code de retour non nul.

### Lot B.4 — Ne calculer l'estimation qu'une fois
**Fichiers** : `pipeline.py`, `12_export_excel.py`

`pipeline.py:168-172` exécute l'estimation une fois pour l'étape 10, puis une seconde fois pour
l'export. Conserver le DataFrame, le transmettre à l'étape 11 pour que `valider_estimation()`
s'exécute réellement, puis à l'étape 12.

**Critère d'acceptation** : une exécution complète ne calcule l'estimation qu'une fois, tracé dans
les logs.

---

## Phase C — Univers et covariables

### Lot C.1 — Borner le panel à la période à risque
**Fichier** : `05_base_entreprises.py`

Le produit cartésien intégral (`05:140`) crée des lignes pour des mois où l'entreprise n'existait
pas. La version 1 proposait de borner à droite par la dernière apparition : **c'était une erreur
grave**. Une dernière apparition précoce peut être une longue non-réponse terminale — précisément le
`D_JT = 0` que le modèle doit expliquer. La supprimer reviendrait à effacer le phénomène étudié.

- **début** : mois d'immatriculation employeur (`DATE_IMMAT_EMPLOYEUR`, présente dans les données
  nettoyées mais non transportée aujourd'hui dans les attributs entreprise, `05:119`) ; à défaut,
  première apparition, avec un indicateur de début imputé ;
- **fin** : mois de cessation issu d'un **registre explicite** ; à défaut, fin du panel pour toutes
  les entreprises ;
- **ne jamais inférer une cessation de la dernière déclaration** ;
- journaliser séparément troncature gauche, cessations observées et fins censurées.

**Critère d'acceptation** : aucune ligne ne précède l'immatriculation ; aucune entreprise n'est
tronquée à droite sans source de cessation ; les trois décomptes apparaissent dans les logs.

### Lot C.2 — Covariables *as-of*, sans information future
**Fichier** : `05_base_entreprises.py`

La version 1 traitait `CL_AGE_ENTREPRISE` comme un attribut invariant à propager. **C'est faux** :
elle dérive d'une durée (`03:577`, `:604-607`) et varie mécaniquement chaque mois.

- **recalculer** l'âge de l'entreprise à chaque mois du panel, à partir de la date
  d'immatriculation et du mois courant ;
- **propager** taille, secteur et commune uniquement par **dernière valeur antérieure connue** ;
- ne jamais rétropropager une valeur postérieure à sa première observation ;
- créer des indicateurs distincts « jamais observé auparavant » plutôt qu'une modalité `"INCONNU"`
  fourre-tout.

Le critère empirique de la version 1 — « moins de 1 % d'INCONNU, fréquence comparable entre
`D_JT = 0` et `D_JT = 1` » — est retiré : cette comparabilité n'est pas garantie et ne doit surtout
pas être forcée. Elle est remplacée par des invariants structurels.

**Critère d'acceptation** : aucune covariable d'un mois `t` ne dépend d'une observation postérieure
à `t`, vérifié par test sur jeu synthétique ; l'âge de l'entreprise croît d'un mois par mois.

---

## Phase D — Modèles de réponse

### Lot D.1 — Corriger la fuite du modèle individuel
**Fichier** : `07b_modele_declaration_indiv.py`

`completude.shift(1).over("ID_EMPLOYEUR")` (`07b:167-172`) décale d'une **ligne** sur un DataFrame
individuel : dans une entreprise de 200 salariés, 199 lignes reçoivent la complétude du mois
courant, fonction directe du `S_IJT` à prédire. Le commentaire du code annonce pourtant l'inverse.

**Précision par rapport à la version 1** : la table entreprise-période doit être construite sur
`firm_base` **après le lot C.1**, et non sur `analytical_base`. Cette dernière part des individus
observés (`06:46`, `:67`) et ne contient pas les mois totalement absents : le décalage donnerait le
*mois observé précédent*, pas le mois civil précédent.

Ajouter un indicateur séparé pour le premier mois à risque.

Documenter par ailleurs que `S_IJT = 0` représente un **salaire manquant sur une ligne existante**.
Il ne corrige pas un salarié dont la ligne entière est omise, l'étape 04 ne construisant aucun
panel salarié.

**Critère d'acceptation** : toutes les lignes d'un même couple entreprise-mois portent la même
valeur de `TAUX_COMPLETUDE_ENTREPRISE`, égale à la complétude du mois civil précédent. Test sur jeu
synthétique.

**Dépendance** : C.1, et à livrer avant E.2.

### Lot D.2 — Historique individuel en cas de cumul d'emplois
**Fichier** : `07b_modele_declaration_indiv.py`

Les variables retardées sont groupées par le seul `ID_INDIV` (`07b:117`, `:126`). Un salarié
cumulant deux emplois le même mois produit un décalage d'une ligne au lieu d'un mois.

**TRANCHÉ (01/08/2026)** — historique par couple **individu-employeur**. La note méthodologique
lève le doute : le cumul d'emplois est « une réalité, non une erreur », et le filtre de
déduplication le préserve délibérément, ne dédupliquant que sur le triplet
individu / employeur / mois (`note_methodologique_traitement.tex`, Filtre 3). Un salarié à deux
employeurs produit donc légitimement deux lignes le même mois, et le lag groupé par le seul
`ID_INDIV` est un défaut avéré — non plus une hypothèse.

**Critère d'acceptation** : un salarié à deux employeurs le même mois obtient un lag correct pour
chacun, vérifié par test.

### Lot D.3 — Évaluation hors échantillon et diagnostics
**Fichiers** : `07_modele_declaration.py`, `07b_modele_declaration_indiv.py`

Les deux modèles calculent leur AUC *in-sample* (`07:182-188`, `07b:346-351`). La version 1 ne
visait que l'étape 07.

- produire des prédictions *out-of-fold* par validation croisée **groupée par employeur** ;
- calculer la **pente de calibration** — paramètre `calibration_slope_range` chargé et jamais
  utilisé — comme coefficient d'une régression non pénalisée de `D_JT` sur `logit(p_oof)`, après
  clipping documenté ;
- ajouter *calibration-in-the-large*, score de Brier et diagnostics d'équilibre des covariables
  après pondération.

**Ne pas rendre le seuil d'AUC bloquant.** La version 1 le proposait : c'est méthodologiquement
faux. Sous MCAR, le vrai modèle a une propension constante, une AUC de 0,5 et produit des poids
unitaires corrects ; à l'inverse une AUC élevée peut accompagner un défaut de positivité. L'AUC
reste **descriptive**.

Bloquer sur : classe cible unique, prédictions non finies, absence de recouvrement, calibration
grossièrement fausse, déséquilibre résiduel des covariables après pondération.

**Critère d'acceptation** : AUC et pente de calibration hors échantillon journalisées pour les deux
modèles ; un jeu MCAR à AUC 0,5 ne déclenche aucun blocage ; un jeu sans recouvrement en déclenche un.

### Lot D.4 — Diagnostic de positivité
**Fichiers** : `07_modele_declaration.py`, `09_ponderation_finale.py`

Le clipping à `10⁻⁶` (`07:197`) et le trimming aux percentiles masquent les propensions extrêmes
sans jamais mesurer la part des unités concernées. Or ces poids déterminent tous les indicateurs
publiés.

Journaliser et rendre bloquant au-delà d'un seuil configurable : part des observations clippées,
part tronquées, propension minimale et maximale, et existence de strates structurellement jamais
répondantes.

**Critère d'acceptation** : une strate à propension structurellement nulle interrompt le pipeline
avec un message la nommant, au lieu d'être silencieusement clippée.

---

## Phase E — Pondération

### Lot E.1 — Rapatrier les poids dans l'étape qui les consomme
**Fichier** : `09_ponderation_finale.py`, `07b_modele_declaration_indiv.py`

> À l'entrée de l'étape 09, joindre `firm_base[ID_EMPLOYEUR, PERIOD, P_HAT_JT, W_JT]` en
> plusieurs-vers-un. Refuser doublons, clés sans correspondance, nulls et valeurs non finies ; ne
> jamais remplir par 1,0. Supprimer de `07b:242-264` le rafraîchissement des poids et le calcul
> provisoire de `W_FINAL`.

**Critère d'acceptation** : l'étape 09 produit des poids corrects même si 07b n'a jamais été
exécutée ; le garde-fou B.1 ne se déclenche pas sur une exécution nominale.

### Lot E.2 — Intégrer 07b au pipeline
**Fichier** : `pipeline.py`, `README.md`

Ajouter `MODELE_DECLARATION_INDIV = 75` à `Stage` et à `_STAGE_MODULES`. Aligner la liste des noms
d'étape du README.

**Critère d'acceptation** : un test compare les séquences de `pipeline.py` et `orchestrator.py` et
les déclare identiques.

**Dépendance** : D.1 impérativement livré avant — activer 07b sans corriger la fuite reviendrait à
brancher un modèle qui triche.

### Lot E.3 — Intégrer les facteurs de réponse au poids d'analyse
**Fichier** : `09_ponderation_finale.py`

**Lot absent de la version 1, et c'est le défaut de pondération le plus direct.**

La branche IPW calcule `W_FINAL = W_JT.fill_null(1.0) × W_INDIV.fill_null(1.0)` (`09:205-210`) sans
facteur de réponse. Les lignes `D_JT = 0` ou `S_IJT = 0` conservent donc un poids strictement
positif, et l'étape 10 leur attribue ensuite un salaire imputé (`10:351`). Le pipeline cumule ainsi
repondération et imputation sur les mêmes unités — un double comptage.

Incohérence interne à relever : la fonction `_compute_ipw_weights` du chemin AIPW met bien les
non-répondants à zéro (`09:74`, `np.where(d == 1, ...)`), la branche IPW ne le fait pas.

Appliquer le poids d'analyse défini au cadrage : `w_ijt = R_ijt / (p̂_jt · q̂_ijt)` avec
`R_ijt = D_jt × S_ijt`.

**Critère d'acceptation** : toute ligne à `D_JT = 0` ou `S_IJT = 0` porte un poids nul ; la somme
des poids sur les répondants est cohérente avec l'effectif attendu ; test sur jeu synthétique.

### Lot E.4 — Supprimer le facteur d'augmentation heuristique
**Fichier** : `09_ponderation_finale.py`

Supprimer `aug_ratio` et son écrêtage arbitraire à [0,5 ; 2,0] (`09:106-108`), ainsi que le calcul
de `mu_aipw` qui n'est que journalisé (`09:97`, `:198`).

Retirer `"aipw"` des valeurs acceptées de `estimation_method` (voir lot A.3), et corriger
`docs/methodology.md` et le `README.md`, qui annoncent une double robustesse non acquise.

**Critère d'acceptation** : aucune statistique n'est produite par un facteur de pondération borné
arbitrairement ; la documentation ne revendique plus la double robustesse.

### Lot E.5 — Redéfinir ou retirer l'effectif pondéré
**Fichiers** : `09_ponderation_finale.py`, `10_estimation_indicateurs.py`

**Lot absent de la version 1.** Les poids sont stabilisés puis **renormalisés à moyenne 1 par
période** (`09:212`), tandis que `weighted_count` est leur simple somme (`10:124`). L'« effectif
pondéré » publié est donc essentiellement le **nombre de lignes**, remis à l'échelle — pas le nombre
de salariés représentés.

C'est d'autant plus grave que le seuil de secret statistique se compare à cette quantité.

**TRANCHÉ (01/08/2026) — retirer `n_weighted` des sorties en l'état.** La normalisation de l'étape 09
divise `W_FINAL` par sa moyenne au sein de chaque période (`09:212`). Par construction, la somme des
poids d'une période vaut donc **exactement le nombre de lignes de cette période**. L'« effectif
pondéré » publié est identiquement le décompte de lignes : il ne porte aucune information, et le
seuil de secret statistique qui s'y compare équivaut à « au moins 30 lignes ».

La note méthodologique ne définit nulle part cette quantité, et sa section « point de vigilance non
résolu » confirme que les poids ne sont pas d'échelle populationnelle : poids médian 2,77, poids
**moyen 1 492,73**, maximum 8 231,55. Pour des poids stabilisés `P(D=1)/p̂`, un tel écart
médiane/moyenne est le signe d'une masse de propensions quasi nulles, non d'une échelle de
population.

Republier un effectif interprétable suppose de renoncer à la normalisation par période et de
disposer de poids calibrés sur un total de population connu. C'est un chantier distinct, à ouvrir
après la phase F.

**Critère d'acceptation** : `n_weighted` ne figure plus dans l'export, ou y figure sous un libellé
qui dit ce qu'il est réellement (« nombre d'observations »).

---

## Phase F — Inférence

**Préalable** : la question 2 du cadrage — population finie ou superpopulation — doit être tranchée
avant d'écrire une ligne de cette phase.

### Lot F.1 — Choisir et implémenter un estimateur de variance
**Fichier** : `10_estimation_indicateurs.py`

La version 1 mélangeait une moyenne AIPW, une variance de ratio de Hájek et des règles de Rubin
appliquées à toutes les statistiques. Ce ne sont pas les mêmes estimateurs.

Sous le cadrage IPW retenu :

- **moyenne** : variance de l'estimateur ratio de Hájek par linéarisation ;
- **quantiles et Gini** : bootstrap groupé par employeur, **réestimant `p̂`, `q̂`, le trimming et
  tous les indicateurs à chaque réplication** — un bootstrap qui fige les poids sous-estime la
  variance ;
- nombre de réplications, tolérances et graines fixés dans la configuration.

Ne pas confondre `weighted_variance` (`10:57`), qui est la variance **descriptive des salaires**,
avec la variance de l'estimateur. Ce sont deux quantités publiées distinctes.

Définir explicitement le traitement de `n_obs`, `n_weighted`, minimum et maximum — omis par la
version 1.

**Critère d'acceptation** : sur un processus générateur synthétique de vérité connue, la couverture
empirique des IC approche le niveau nominal pour la moyenne, la médiane et le Gini.

### Lot F.2 — Retirer les règles de Rubin du chemin de publication
**Fichier** : `10_estimation_indicateurs.py`

Le cadrage retenu écarte l'imputation du chemin de publication. Les règles de Rubin
(`10:465`) ne s'appliquent donc plus : elles seront réintroduites si et quand l'AIPW le sera.

Traiter au passage le repli silencieux de `10:472` : lorsqu'une statistique manque dans certaines
imputations, le code publie la moyenne des autres **sans intervalle de confiance**, ce qui peut
contourner la suppression des petites cellules. Exiger les `M` valeurs ou supprimer la cellule.

**Critère d'acceptation** : aucune statistique publiée ne provient d'une combinaison partielle
d'imputations.

### Lot F.3 — Aligner la documentation des degrés de liberté
**Fichiers** : `10_estimation_indicateurs.py`, `docs/methodology.md`

> Conserver les degrés de liberté classiques de Rubin (1987) et remplacer la référence
> Barnard & Rubin (1999) dans le code et la note méthodologique. L'ajustement en petit échantillon
> est reporté jusqu'à définition des degrés de liberté complets au niveau employeur.

Corriger également l'affirmation d'effets fixes temporels de `docs/methodology.md:68`, absents des
covariables de l'étape 07 (`07:57`) : soit les implémenter, soit retirer la mention.

---

## Phase G — Publication

### Lot G.1 — Fonder le secret statistique sur les contributeurs
**Fichier** : `10_estimation_indicateurs.py`

Le critère actuel compare une **somme de poids** à 30 (`10:261-266`) : deux salariés observés quinze
mois font trente lignes et déverrouillent la cellule. Le lot E.5 aggrave le constat — cette somme
n'est même pas un effectif.

> Ajouter `min_distinct_individuals`, `min_distinct_employers` et `max_employer_wage_share`. Les
> identifiants nuls ne comptent pas. Supprimer **toute la cellule, pour toutes ses statistiques**,
> dès qu'un seuil primaire échoue. Définir ensuite les seules marges additives protégées et
> l'algorithme déterministe de suppression secondaire.

**Décision requise** : la dominance porte-t-elle sur la masse salariale observée, la masse pondérée,
ou les effectifs ?

**Critère d'acceptation** : une cellule de deux individus sur quinze mois est supprimée ; une cellule
dont un employeur dépasse le seuil de dominance est supprimée ; une cellule masquée ne peut pas être
reconstruite à partir des marges publiées.

**Position** : garde de prépublication, à livrer avec la phase F et non en fin de projet.

### Lot G.2 — Corriger l'export Excel
**Fichier** : `12_export_excel.py`

Le format `#,##0` (`12:32`) s'applique à toutes les valeurs numériques : un Gini de 0,37 s'affiche
`0`. `_DECIMAL_FMT` est déclaré et jamais utilisé.

Table de formats par statistique, couvrant explicitement les suffixes `_ci_lower` et `_ci_upper`.
`n_weighted` ne doit être formaté en entier que si le lot E.5 lui rend le sens d'un effectif.

Les valeurs non finies sont converties en **cellule masquée `—` avec avertissement**, jamais en
erreur Excel — `write_number()` échoue sinon sans l'option adéquate.

**Critère d'acceptation** : un Gini s'affiche avec ses décimales ; un résultat contenant un `NaN`
s'exporte sans exception, la cellule concernée étant masquée.

### Lot G.3 — Réparer `enrich-anstat`
**Fichiers** : `cli.py`, `README.md`

`cli.py:188` importe `cnps.05_1_jointure_anstat` ; le fichier s'appelle `jointure_anstat.py`.
Corriger l'import et aligner le README, qui décrit un appariement approché là où le code fait une
égalité après normalisation.

**Critère d'acceptation** : la commande atteint la lecture des données ; le README décrit la méthode
réellement implémentée.

### Lot G.4 — Sécuriser l'accès au stockage
**Fichiers** : `config.py`, `storage.py`, `07_modele_declaration.py`,
`07b_modele_declaration_indiv.py`, `08_imputation_salaires.py`, `11_validation_qualite.py`

> Ajouter `environment: development|production`. Aucun identifiant par défaut. En production,
> secrets non vides et `secure=true` ; en développement, HTTP exige `allow_insecure_minio=true`.

Les modèles sont sérialisés en pickle par trois étapes (`07:237`, `07b:415`, `08:255`) et rechargés
depuis MinIO par l'étape 11 — vecteur d'exécution de code arbitraire. Écrire les diagnostics de
modèle en JSON et ne faire lire que ces JSON par l'étape 11.

**Critère d'acceptation** : le démarrage échoue en l'absence de secrets en production ; aucun
`pickle.load` sur un objet distant non vérifié.

---

## Phase H — Contrôles conditionnels

Deux défauts dont l'effet réel est invérifiable sans les données. Les traiter par des contrôles
bloquants plutôt que par une correction à l'aveugle.

### Lot H.1 — Virgules décimales
**Fichier** : `02_harmonisation_types.py`

`str.replace_all(r"[^\d.\-]", "")` (`02:68`) transforme `75 000,50` en `7500050`. Détecter le format,
retirer les seuls séparateurs de milliers, convertir la virgule décimale. Journaliser séparément les
valeurs impossibles à analyser et **bloquer au-delà d'un seuil de fréquence**.

### Lot H.2 — Déduplication : identifiants nuls
**Fichier** : `03_nettoyage_donnees.py`

**La conception de la déduplication n'est pas en cause** — elle est documentée et bien argumentée
dans la note méthodologique. Le filtre 2 ne retire que le niveau 3 des doublons signalés à la source
(13 560 lignes, 0,06 %) au motif qu'« un signalement produit en amont indique un doute, pas un
verdict » ; le filtre 3 déduplique sur le triplet individu / employeur / mois, critère vérifiable, en
conservant le salaire le plus élevé ; et le cumul d'emplois chez des employeurs distincts est
délibérément préservé. Ces arbitrages sont solides.

**Le défaut porte uniquement sur les identifiants nuls.** La clé est
`["ID_INDIV", "ID_EMPLOYEUR", *_cols_periode]` avec `.unique(subset=..., keep="first")` (`03:273-278`)
et **n'exclut pas les nulls**. Polars regroupant les nulls entre eux, toutes les lignes à
`ID_INDIV` null d'un même employeur sur un même mois s'effondrent en **une seule ligne** — celle au
salaire le plus élevé. Des salariés distincts mais non identifiés seraient donc supprimés, et la
note ne traite pas ce cas.

Dédupliquer uniquement les clés complètes ; conserver séparément les lignes à clé incomplète avec un
indicateur qualité ; journaliser leur volume, seule façon de mesurer l'ampleur réelle du problème.

**Critère d'acceptation** : deux lignes à `ID_INDIV` null, même employeur, même mois, salaires
différents, survivent toutes deux à l'étape 03 et sont marquées.

### Lot H.3 — Périodicité non renseignée sur 56 % des lignes
**Fichier** : `03_nettoyage_donnees.py`

La note méthodologique donne la répartition des périodicités de rémunération : **non renseignée
56,2 %**, mensuelle 40,9 %, journalière 2,3 %, horaire 0,6 %.

Le code ne convertit explicitement que les journaliers (×22,4) et exclut les horaires. Les lignes à
périodicité inconnue — la **majorité du fichier** — sont implicitement traitées comme mensuelles :
le seuil de plausibilité leur applique `pl.lit(cfg.cleaning.min_salary)`, soit 75 000 FCFA/mois
(`03:371`).

C'est un choix défendable — 93 % des périodicités *connues* sont mensuelles — mais il n'est écrit
nulle part et il a deux conséquences. Une ligne réellement journalière classée inconnue voit son
taux quotidien lu comme un salaire mensuel, donc **exclue** par le seuil de 75 000 FCFA : on perd un
travailleur réel. À l'inverse, celles qui passent le seuil contribuent à la moyenne avec une valeur
sous-estimée d'un facteur ~22.

Rendre l'hypothèse explicite dans la configuration et dans la note ; journaliser le volume concerné
et la part exclue par le seuil ; prévoir une analyse de sensibilité traitant les inconnues comme
journalières.

**Critère d'acceptation** : l'hypothèse « périodicité inconnue = mensuelle » est un paramètre
documenté, et le nombre de lignes qu'elle fait exclure par le seuil de salaire minimum est
journalisé.

---

## Phase I — Invariants durs

### Lot I.1 — Faire échouer sur les états impossibles
**Fichiers** : `05_base_entreprises.py`, `08_imputation_salaires.py`

La version 1 proposait de « ne plus perdre » les entreprises déclarantes à masse salariale nulle.
Vérification faite, **ce cas est en principe inatteignable** : `D_JT` vaut toujours 0 ou 1
(`05:166`, `otherwise(0)`) et `D_JT = 1` exige au moins un salaire positif, donc une moyenne nulle
est impossible avec des salaires finis.

Conserver silencieusement ces lignes n'est donc pas une correction mais un masquage. Faire échouer
l'étape si `D_JT` est null ou hors `{0, 1}`, ou si `D_JT = 1` coexiste avec un salaire moyen non
positif ou non fini. Toute reclassification doit être une décision explicite.

Conserver en revanche le test de cardinalité : `firm_base_imputed` doit contenir `M × N` lignes.

### Lot I.2 — Distinguer les quatre régimes d'historique manquant
**Fichier** : `08_imputation_salaires.py`

La version 1 proposait de recopier l'indicateur `SANS_HISTORIQUE` de l'étape 07. **C'est
insuffisant** : après un mois `D_JT = 0`, `LAG_SALAIRE_MOYEN` et `LAG_EFFECTIF_OBSERVE` sont à
nouveau nuls (`08:53`, `:78`), et un indicateur unique classerait ces lignes à tort comme premières
observations.

Encoder distinctement : premier mois à risque ; salaire précédent manquant pour cause de
non-réponse ; effectif précédent manquant ; valeur réellement égale à zéro.

**Critère d'acceptation** : test synthétique où les quatre régimes reçoivent des encodages
distincts. Le critère de la version 1 — « distribution imputée moins décalée vers le bas » —
dépendait des données et est retiré.

*Ce lot n'a d'objet que si l'imputation est réintroduite (voir cadrage).*

---

## Phase T — Tests

### Lot T.1 — Estimateurs pondérés
Fonctions pures de `10_estimation_indicateurs.py`, confrontées à des valeurs de référence calculées
indépendamment : moyenne de Hájek, variance descriptive de Kish, quantiles interpolés, Gini de
Lerman-Yitzhaki. Cas dégénérés : poids nuls, observation unique, valeurs identiques.

Distinguer explicitement dans les tests la variance **descriptive** de la variance **d'estimation**.
Documenter la convention exacte des quantiles pondérés — le code utilise une CDF centrée
`(cum_w − w/2)/Σw` là où `docs/methodology.md:202` décrit une CDF à droite.

### Lot T.2 — Invariants de cardinalité
Remplacer la « conservation des lignes aux jointures » de la version 1, trop vague, par une
cardinalité **attendue par étape** : l'étape 05 développe le panel, l'étape 08 produit `M × N`, les
jointures des étapes 06, 07b et 09 conservent la table de gauche. Vérifier aussi l'identité des
séquences entre les deux orchestrateurs et le refus des configurations invalides.

### Lot T.3 — Couverture sur processus générateurs synthétiques
Le protocole doit être **écrit immédiatement après la décision de cadrage**, puis exécuté après la
phase F. Distinguer les cas, la version 1 en confondant plusieurs :

- employeur ayant enchaîné des non-réponses **malgré `p > 0`** — l'estimateur doit récupérer la
  vérité ;
- strate **structurellement jamais répondante** — violation de positivité : aucun IPW ne peut
  récupérer la vérité, le test doit exiger un **échec de positivité**, pas un estimateur non biaisé ;
- modèle de propension faux, modèle de résultat correct ;
- modèle de résultat faux, propension correcte ;
- deux modèles faux — sans exigence de non-biais.

Réplications, tolérances de biais et de couverture, et graines doivent être fixées dans le protocole.

**Critère d'acceptation** : c'est le seul test capable de démontrer que les phases C à F ont
réellement corrigé le biais. Sans lui, les correctifs restent des conjectures.

---

---

## Phase Z — Clôture documentaire *(dernier lot, obligatoire)*

### Lot Z.1 — Répercuter les arbitrages et les correctifs dans la documentation
**Fichiers** : `docs/note_methodologique_traitement.tex` (et son PDF), `docs/methodology.md`,
`README.md`

**Ce lot conditionne la diffusion.** Le pipeline corrigé produira des chiffres justes ; la
documentation, elle, décrira encore la version défectueuse. Or c'est elle qui accompagne les
publications et fonde la confiance des utilisateurs. Aucun indicateur ne doit être diffusé avant
que ce lot ne soit livré.

#### Affirmations aujourd'hui fausses, à corriger

| Où | Ce qui est écrit | Pourquoi c'est faux |
|---|---|---|
| Conclusion de la note | « les deux modèles de correction atteignent une qualité largement supérieure au seuil requis » | Repose sur une AUC calculée **en apprentissage** et gonflée par l'artefact du panel cartésien (constat B2). À refonder sur les métriques hors échantillon du lot D.3 |
| Section « concentration des poids » | « La méthode fonctionne "trop bien" : elle identifie si sûrement les non-déclarants qu'elle produit des poids déséquilibrés » | Diagnostic erroné. Les propensions quasi nulles viennent des lignes artificielles portant `"INCONNU"`, modalité créée par le pipeline et colinéaire à `D_JT = 0` |
| Section « portée et options » | Deux remèdes présentés comme équivalents : resserrer le plafond, ou **retirer l'historique du modèle** | Le second **aggraverait** le problème : priver le modèle de `LAG_D_JT` et `TAUX_DECLARATION_PASSE` lui laisse `"INCONNU"` comme séparateur principal. À retirer. Le premier ne traite que le symptôme. La cause est traitée par C.1 |
| Section imputation | « les intervalles de confiance publiés intègrent le fait qu'une partie du chiffre est reconstituée » | Faux : `U_m` est forcée à zéro (constat B4) et l'imputation est *improper*. À réécrire après la phase F |
| `docs/methodology.md:68` | Effets fixes temporels `γ_t` dans le modèle de déclaration | Absents des covariables (`07:57`). Les implémenter ou retirer l'affirmation |
| `docs/methodology.md` §3.5 | Référence Barnard & Rubin (1999) | Le code implémente Rubin (1987). Aligner (lot F.3) |
| `methodology.md` §3.3, README | Estimation « doublement robuste (AIPW) » | L'AIPW est différé (voir cadrage). Retirer la revendication |
| `methodology.md` §3.6 | Moyenne pondérée présentée comme Horvitz-Thompson | C'est un estimateur ratio de **Hájek**. Renommer |
| `methodology.md` §3.6 | CDF des quantiles décrite à droite `cum_w/Σw` | Le code utilise une CDF centrée `(cum_w − w/2)/Σw`. Documenter la convention réelle |
| README, section ANSTAT | « matching approché sur RAISON_SOCIALE » | Le code fait une égalité après normalisation (lot G.3) |

#### Éléments nouveaux à documenter

- **L'estimand et sa portée** : salarié-employeur-mois ; entreprises ayant déclaré au moins une fois
  dans les K = 12 mois précédents ; **hors champ explicite** — employeurs absents de tout le panel,
  et salariés dont aucune ligne n'existe dans aucune source.
- **Le cadre d'inférence** : population finie avec non-réponse, et ce que mesurent réellement les
  intervalles publiés.
- **L'absence de registre de cessation** et la fenêtre de 12 mois qui en tient lieu, avec les
  résultats de l'analyse de sensibilité sur K ∈ {6, 12, 24, ∞}.
- **La dimension temporelle** : séries mensuelles pour les agrégats robustes, mention « cumul sur la
  période » sur tout tableau agrégé.
- **L'hypothèse de périodicité** : 56,2 % des lignes ont une périodicité non renseignée et sont
  traitées comme mensuelles (lot H.3), avec le volume que cette hypothèse fait exclure par le seuil
  de salaire minimum.
- **Les règles de secret statistique** : seuils d'individus et d'employeurs distincts, dominance sur
  la masse salariale observée, suppression secondaire.
- **La variable de chaque statistique** : Gini et statistiques de queue sur salaire non winsorisé,
  moyenne sur salaire winsorisé, extrêmes étiquetés « observés » (lot A.1).
- **Le retrait de `n_weighted`**, ou son renommage en « nombre d'observations » (lot E.5).
- **La déduplication sur identifiants nuls** et le volume concerné (lot H.2).

**Critère d'acceptation** : une relecture croisée de la note et du code ne relève aucune affirmation
méthodologique non vérifiable dans le code. Le PDF est régénéré à partir du `.tex` corrigé.

---

## Séquencement

```
        DÉCISION DE CADRAGE (estimand, cadre d'inférence, registre)
                              |
        +---------------------+---------------------+
        |                     |                     |
        v                     v                     v
   A.3 config            A.1 variables         A.4 filiation
   A.2 dates                  |                     |
        |                     |                     |
        +----------+----------+---------------------+
                   v
            C.1 panel à risque
                   v
            C.2 covariables as-of
                   |
        +----------+----------+
        v                     v
   D.3 modèles OOF       D.1 historique 07b
   D.4 positivité        D.2 cumul d'emplois
        |                     |
        +----------+ E.2 <----+
                   v
            E.1 jointure poids
                   v
            E.3 facteurs de réponse
            E.4 suppression aug_ratio
            E.5 effectif pondéré
                   v
            F.1 variance   F.2 Rubin   F.3 documentation
                   v
            G.1 secret  →  G.2 export
```

**Phase B** : livrable immédiatement, en parallèle de tout le reste.
**Phases H et I** : sans dépendance, à tout moment.
**Lot T.1** : immédiat. **T.2** : après E. **T.3** : protocole immédiat, exécution après F.
**Lot Z.1** : **dernier lot, et préalable à toute diffusion.** Il ne peut être clos qu'une fois les
phases A à G livrées, puisqu'il documente leur résultat. Alimenter au fil de l'eau une liste des
affirmations rendues caduques par chaque lot livré, plutôt que de tout reconstituer à la fin.

Les lots **G.3** et **G.4** sont indépendants du chemin statistique.

---

## Annexe — ce que la version 1 disait de faux

Consigné pour éviter que ces propositions ne reviennent par inadvertance.

| Lot v1 | Ce qui était proposé | Pourquoi c'était faux |
|---|---|---|
| Cadrage / 2.4 | AIPW au niveau entreprise | L'estimand publié est au niveau salarié ; une moyenne de moyennes d'entreprises ne l'estime pas. Et `m(X)` n'existe pas pour les déclarants |
| 0.1 | Échouer si tous les `W_JT` valent 1 | Sous MCAR à propension constante, des poids unitaires sont **corrects**. Faux positif |
| 0.2 | Supprimer l'IC si les `Q_m` sont identiques | Casserait le lot F.1 : avec une vraie `U_m`, des `Q_m` identiques donnent un IC valide |
| 1.1 | Borner le panel à la dernière apparition | Confond cessation d'activité et non-réponse terminale — supprime le phénomène étudié |
| 1.2 | Propager `CL_AGE_ENTREPRISE` comme invariant | Cette variable dérive d'une durée et varie chaque mois |
| 1.3 | Rendre `AUC ≥ 0,60` bloquant | Une AUC de 0,5 peut refléter un vrai mécanisme MCAR ; une AUC élevée peut masquer un défaut de positivité |
| 2.5 | Un indicateur `SANS_HISTORIQUE` unique | Confond « premier mois » et « historique manquant après non-réponse » |
| 2.6 | Conserver les déclarants à masse nulle | Cas en principe inatteignable ; les conserver masquerait une violation d'invariant |
| 4.1 | Déclarer la variable dans `dimensions.yaml` | `StatDef` n'a pas de champ `variable` : `config.py` doit changer aussi |
| Séquencement | Variables et dates en phase 4 et 5 | Elles changent les entrées des modèles : elles doivent précéder la modélisation |

Trois défauts réels que la version 1 avait manqués sont désormais couverts : **E.3** (absence des
facteurs de réponse dans le poids d'analyse), **E.5** (l'effectif pondéré n'est pas un effectif), et
**D.2** (historique individuel en cas de cumul d'emplois).
