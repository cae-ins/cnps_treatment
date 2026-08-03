# Note de transmission — audit, contrôles de suivi et rapprochement CIAP/comptabilité nationale

**Projet :** Pipeline de traitement des déclarations salariales CNPS

**Version de la note :** 1er août 2026

**Branche auditée :** `fix/audit-phase-b`
**Statut :** finalisation maximale hors VPN ; aucune connexion MinIO et aucun recalcul des données réelles.

## Synthèse décisionnelle

Le chemin de publication est désormais cohérent pour produire des **estimations ponctuelles IPW
à deux étages**, sous réserve de validation sur les données réelles. Le poids appliqué à une ligne
salarié–employeur–mois est :

\[
W_{ijt}=\frac{D_{jt}S_{ijt}}{\hat p_{jt}\hat q_{ijt}}.
\]

Le code ne revendique plus un AIPW ou une double robustesse qu'il ne pouvait pas assurer.
L'ancien mécanisme d'augmentation ad hoc a été retiré du chemin de publication. Les salaires
imputés et les règles de Rubin sont également hors du DAG publié.

La suite hors ligne compte **71 tests réussis**. La compilation Python et les contrôles Ruff
critiques (`F` et `E9`) passent. Cette validation prouve les invariants programmés sur des
données synthétiques ; elle ne valide ni les valeurs CNPS réelles, ni les hypothèses de non-réponse,
ni la concordance avec les comptes nationaux.

### Décision de diffusion

| Niveau | Décision |
|---|---|
| Code hors ligne | **Acceptable**, avec garde-fous renforcés. |
| Estimations ponctuelles réelles | **À finaliser après VPN**, seulement si tous les contrôles bloquants passent. |
| Intervalles de confiance | **Non diffusables** tant que le lot F.1 n'est pas spécifié et validé. |
| AIPW / double robustesse | **Différé** ; ne pas réintroduire l'ancien pseudo-AIPW. |
| Comparaison aux comptes nationaux | **À construire** avec une table CIAP–NAPCN officielle et un pont conceptuel D.11/D.12/D.1. |

## 1. Résultats de l'audit technique et méthodologique

### 1.1 Corrections structurantes réalisées

| Domaine | Correction livrée | Effet |
|---|---|---|
| Champ entreprise | Panel mensuel explicite, borne gauche documentée, absence de cessation non inférée, fenêtre \(K\) strictement passée. | Évite de faire dépendre le champ du mois à prédire. |
| Temporalité | Attributs propagés uniquement par dernière valeur antérieure ; âges recalculés au mois. | Supprime les fuites d'information future. |
| Réponse entreprise | Modèle \(p_{jt}\), diagnostics OOF groupés par employeur, calibration, Brier, équilibre et positivité. | Contrôle la qualité au-delà de l'AUC. |
| Réponse individuelle | Modèle \(q_{ijt}\) limité aux entreprises déclarantes, historique par salarié–employeur, contexte entreprise décalé d'un mois civil. | Corrige la non-réponse salariale sur les lignes existantes sans mélange des deux mécanismes. |
| Poids | Identité \(D\\times S/(p\\times q)\), poids nul hors réponse et hors champ, clipping et trimming tracés, poids brut conservé. | Supprime le double comptage des non-répondants. |
| Jointures | Clés entreprise–mois obligatoires et uniques ; changement de cardinalité bloquant. | Empêche la duplication silencieuse des salariés. |
| Estimation | Variable propre à chaque statistique ; moyenne/variance winsorisées, quantiles/Gini/extrêmes sur salaire non winsorisé. | Aligne configuration, code et libellés. |
| Confidentialité | Seuils sur individus et employeurs distincts, dominance d'un employeur, suppression secondaire par marge. | Réduit les risques de divulgation. |
| Validation | Modèles JSON revalidés, poids finaux et ESS contrôlés, statistiques publiées finies et ordonnées, cellules masquées vérifiées. | L'export échoue plutôt que de diffuser une incohérence. |
| Sécurité | Secrets sortis de la configuration, production obligatoirement TLS, absence de chargement pickle distant. | Réduit l'exposition des identifiants et le risque d'exécution arbitraire. |
| Filiation | UUID de session, empreinte de configuration sans secrets, commit, état Git et versions logicielles. | Permet d'identifier précisément une exécution. |

### 1.2 Robustesse réellement obtenue

La robustesse actuelle est triple :

1. **robustesse d'ingénierie** : invariants, erreurs bloquantes, traçabilité et tests ;
2. **robustesse diagnostique** : les modèles de réponse sont évalués hors échantillon par
   employeur et les violations de positivité/calibration sont bloquantes ;
3. **robustesse de publication** : aucune valeur non finie, aucun intervalle non validé et
   aucune petite cellule non masquée ne doivent sortir.

Elle n'est pas une double robustesse statistique. La validité de l'IPW reste conditionnelle à :

- une définition correcte de la population visée ;
- une probabilité de réponse strictement positive ;
- des modèles \(p\) et \(q\) suffisamment bien spécifiés ;
- une hypothèse de non-réponse explicable par les variables observées ;
- l'existence d'une ligne pour chaque salarié que \(q\) est censé représenter.

## 2. Valeurs historiques à conserver comme points de contrôle

Les valeurs ci-dessous proviennent de l'audit antérieur portant sur 23 fichiers. Elles sont
**historiques, non recalculées dans cette session et non publiables comme résultats courants**.
Elles servent de points de départ pour détecter une rupture lors de la prochaine exécution.

| Indicateur historique | Valeur | Utilisation |
|---|---:|---|
| Salaires manquants situés dans des entreprises ayant déclaré | 65,3 % | Justifie le second étage \(q\). |
| Couples entreprise–mois à déclaration partielle | 17,9 % | Suivi mensuel de la non-réponse partielle. |
| Salariés concentrés dans ces déclarations partielles | 74 % ; 20,1 M sur 26,9 M | Contrôle de concentration par taille d'employeur. |
| Périodicité non renseignée | 56,2 % | Rend obligatoire la sensibilité `monthly` / `daily`. |
| Périodicité mensuelle / journalière / horaire | 40,9 % / 2,3 % / 0,6 % | Contrôle de composition. |
| Lignes horaires avec confusion d'unité suspectée | 65,5 % sur 157 054 lignes | Motive l'exclusion actuelle des horaires. |
| Lignes horaires avec durée incohérente | 69,0 % | Contrôle avant toute réintégration. |
| Part d'effectif représentée par les horaires | 1,4 % | Mesure l'impact de l'exclusion. |
| Durées incohérentes chez les journaliers | 0,12 % | Étaye la conversion ×22,4. |
| Journaliers parmi les salaires à périodicité connue | 5,7 % | Suivi du risque de biais si exclusion. |

Les anciens poids « médiane 2,77 ; moyenne 1 492,73 ; maximum 8 231,55 » décrivaient une
version défectueuse et extrêmement instable. Ils constituent un **signal d'alerte historique**,
pas une cible à reproduire.

## 3. Contrôles nécessaires après rétablissement du VPN

### 3.1 Matrice de contrôles bloquants

| Étape | Mesure à collecter | Règle proposée |
|---|---|---|
| Inventaire | Nombre de fichiers attendus/reçus, mois couverts, feuilles, taille et SHA-256 | Aucun mois attendu manquant sans décision documentée. |
| Schéma | Colonnes nouvelles, absentes, types, taux de parsing | Colonne obligatoire absente : blocage ; échec numérique > 1 % : blocage technique actuel. |
| Déduplication | Doublons à clé complète, clés incomplètes conservées | Doublon complet résiduel : blocage ; clés incomplètes : volume et taux publiés au contrôle. |
| Périodicité | Parts M/J/H/inconnue, lignes exclues par hypothèse | Comparer à l'historique ; exécuter `monthly` et `daily` pour l'inconnue. |
| Salaire | Volumes avant/après seuil, winsorisation, quantiles et extrêmes | Toute rupture doit être expliquée par millésime/source, jamais corrigée automatiquement. |
| Panel | Employeurs, débuts imputés, troncatures gauches, fins censurées | Aucun « décès » d'entreprise inféré de la non-réponse. |
| Champ \(K\) | Effectifs et estimations pour \(K=6,12,24,\\infty\) | Publier l'écart au scénario central ; choix final validé institutionnellement. |
| Réponse | Taux \(D\), taux \(S|D=1\), strates sans répondant | Classe unique ou strate structurelle sans répondant : blocage. |
| Modèles | AUC, Brier, pente, calibration-large, SMD, supports \(p/q\) | AUC descriptive ; calibration, équilibre, recouvrement et valeurs finies bloquants. |
| Poids | Min/médiane/moyenne/max, CV, percentiles, clipping, trimming, ESS | Identité \(D\\times S/(p\\times q)\) exacte ; poids positif uniquement pour les répondants. |
| Estimations | Moyenne, dispersion, quantiles, Gini, masses et effectifs | Valeurs finies, quantiles ordonnés, moyenne dans min–max. |
| Confidentialité | Individus, employeurs, dominance et suppression secondaire | Seuils actuels 30 individus, 3 employeurs, dominance ≤ 85 %, à faire homologuer. |
| Export | Statut d'inférence et rapport de validation | Toute erreur bloque ; aucune colonne d'IC tant que F.1 est en attente. |

Les seuils actuels de calibration, clipping, trimming et confidentialité sont des paramètres
techniques de prépublication. Ils doivent être confirmés par la gouvernance statistique.

### 3.2 Table historique de suivi

Créer une table append-only, une ligne par période et session, contenant au minimum :

- `period`, `source_vintage`, `session_uuid`, `input_sha256`, `config_sha256`,
  `git_commit` et versions des dépendances ;
- nombre de fichiers, lignes, individus, employeurs et couples entreprise–mois ;
- taux de clés incomplètes, doublons complets, erreurs de parsing et périodicités ;
- taux \(D\), \(S|D=1\), supports de \(p\) et \(q\), calibration, Brier et SMD ;
- percentiles des poids, part clippée, part tronquée, ESS et ratio ESS/n ;
- volumes avant/après chaque exclusion ;
- valeurs des indicateurs centraux par mois et branche ;
- statut `PASS/WARNING/FAIL`, justification, décision, responsable et date de visa.

Les alertes historiques doivent s'appuyer sur des séries versionnées : médiane mobile et écart
absolu médian, ou intervalles interquartiles, avec prise en compte des révisions de source. Une
alerte ne doit jamais entraîner automatiquement la suppression d'une observation économique.

## 4. Éléments indispensables pour la finalisation des estimations

### 4.1 Données CNPS à obtenir ou confirmer

1. inventaire officiel des mois et fichiers attendus ;
2. définition juridique et comptable de `SALAIRE_BRUT` : primes, rappels, avantages en nature,
   retenues salarié et assiette de cotisation ;
3. registre de cessation/radiation des employeurs, ou décision explicite de conserver le
   censurage à droite ;
4. si possible, roster indépendant des salariés couverts par employeur et mois afin de représenter
   les lignes entièrement omises ;
5. code d'activité principal de l'employeur, sa nomenclature et sa date de validité ;
6. distinction public/privé, formel/informel, établissement/entreprise et résident/non-résident ;
7. calendrier des corrections et déclarations tardives pour passer d'une logique de paiement à
   une logique d'exercice.

Sans roster salarié, \(q\) ne corrige que les salaires manquants sur des lignes existantes. Cette
limite doit figurer dans toute diffusion.

### 4.2 Décisions institutionnelles

- population cible : salariés CNPS observables ou ensemble des emplois salariés formels ;
- traitement des employeurs sans activité connue ;
- valeur centrale de \(K\) après sensibilité ;
- hypothèse centrale de périodicité inconnue ;
- seuils de confidentialité et règle de dominance ;
- traitement des révisions tardives ;
- niveau de nomenclature diffusé ;
- autorité responsable du visa final.

### 4.3 Inférence F.1

Pour diffuser des erreurs-types, il faut spécifier et faire relire :

- la linéarisation conjointe des deux régressions logistiques L2 ;
- la covariance induite par les employeurs communs ;
- l'effet du clipping et du trimming ;
- les fonctions d'influence de la moyenne, des quantiles et du Gini ;
- la recette de couverture définie dans
  [le protocole DGP](protocole_tests_dgp.md).

Un bootstrap naïf d'employeurs ajouterait une variance de tirage incompatible avec le cadre de
population finie retenu.

## 5. Passerelle CIAP vers la comptabilité nationale

### 5.1 Principe

La CIAP est la nomenclature ivoirienne d'activités et de produits, construite en cohérence avec
la CITI Rev. 4 et les nomenclatures AFRISTAT NAEMA/NOPEMA. L'ANStat indique que la NAPCN
utilisée pour les comptes nationaux dérive de la CIAP et comprend **48 branches, 135
sous-branches et 337 produits**.

Le rapprochement doit utiliser **l'activité principale de l'employeur ou de l'établissement**.
La profession du salarié, la CSP et le produit vendu ne doivent pas être utilisés comme substitut
du code d'activité.

Sources officielles :

- [Classification ivoirienne des activités et des produits — ANStat](https://www.anstat.ci/normes-statistiques-details/9b77e8764a187efaa5d7a98298c933e27f4ab293be6eb70d9d9166bdcef777a6fc545fa52f4c9afd89a14d288334d7bb0134ef2540d841be48a3cf40a44de20ft9qScYIUD-Ftop-DO4xNNTJFnL-XOv4jeoMvbQjM7PQ) ;
- [Comptes nationaux annuels définitifs 2023 — ANStat](https://www.anstat.ci/assets/publications/files/CNA_DEFINITIFS_2023.pdf).

### 5.2 Table de passage à constituer

La table doit être versionnée et comporter :

| Champ | Contenu |
|---|---|
| `source_activity_code` / `source_activity_label` | Code et libellé tels que reçus de la CNPS. |
| `source_nomenclature` / `source_version` | Nomenclature d'origine et millésime. |
| `ciap_activity_code` / `ciap_activity_label` | Activité principale CIAP. |
| `ciap_version` | Version officielle utilisée. |
| `napcn_branch_code` / `napcn_branch_label` | Branche de comptes nationaux. |
| `valid_from` / `valid_to` | Période de validité du pont. |
| `match_method` | `official_exact`, `deterministic` ou `manual`. |
| `match_confidence` | Niveau de confiance documenté. |
| `decision_status` | Validé, ambigu, non apparié. |
| `reviewer` / `review_date` | Visa humain. |
| `source_url` / `source_sha256` | Provenance du référentiel. |

Ordre d'appariement :

1. correspondance de code officielle ;
2. règle déterministe documentée sur libellé normalisé ;
3. arbitrage humain ;
4. catégorie « non apparié/ambigu ».

Aucun fuzzy matching silencieux ne doit affecter une branche de comptes nationaux.

### 5.3 Agrégat comptable comparable

Selon le SCN 2008 :

\[
D.1 = D.11 + D.12,
\]

où D.11 représente les salaires et traitements, en espèces et en nature, et D.12 les
cotisations sociales à la charge des employeurs. La comparaison prioritaire est donc :

1. **masse salariale brute CNPS ↔ D.11**, après vérification du contenu exact de la variable CNPS ;
2. **masse CNPS ajustée + cotisations employeur ↔ D.1**, si D.11 n'est pas disponible ;
3. sinon, ratio clairement étiqueté « non strictement comparable ».

Le [SCN 2008 des Nations Unies](https://unstats.un.org/unsd/nationalaccount/docs/SNA2008.pdf)
précise également que la rémunération est enregistrée en droits constatés et peut inclure des
éléments en nature. Les rappels, primes, avantages, arriérés et dates d'exercice doivent donc être
traités explicitement.

### 5.4 Données à demander à la comptabilité nationale

Pour chaque millésime et branche NAPCN :

- D.11 salaires et traitements ;
- D.12 cotisations sociales employeur ;
- D.1 rémunération des salariés ;
- emploi salarié ou équivalent temps plein, si une moyenne salariale doit être comparée ;
- valeur ajoutée seulement pour calculer une part salariale contextuelle ;
- ventilation formel/informel et public/privé, si disponible ;
- statut du compte : définitif, semi-définitif ou provisoire ;
- année de base, version SCN et date d'extraction.

Au 1er août 2026, les comptes définitifs 2023 et les comptes provisoires 2024 sont disponibles
sur le portail ANStat. Le millésime et le statut de révision doivent être figés dans chaque
comparaison :

- [Comptes définitifs 2023](https://www.anstat.ci/publication-details/c2b1499c71b7f5f06da1e85dd41470ec00c8972592dfab8f1fe63440441c692c587a0b3a71bdf88fe1e272eaa1aaad0be25bf1a01210815f6185d300b43c183fdNA-QJRlpAyfoIO71BT98gt1F4Z08KYa6EE_ln-Pjk4) ;
- [Comptes provisoires 2024](https://www.anstat.ci/publication-details/f70a06341cdd20a58340647a7d315cd52f5778fb052e68bd5c2a19d6c94be0e1011d226d2b8965d6b5941c6152fcdbbda868deab7b7b3d5c53206a2226b4bb07E3Vp9xFeFf9-_psjXZB2Rr3tJGy-YmJO2roFmF7TZqM).

### 5.5 Tableau de rapprochement à produire

| Année | Branche NAPCN | Masse CNPS observée | Masse CNPS IPW | D.11 | D.12 | D.1 | CNPS IPW / D.11 | Pont vers D.1 | Écart | Statut |
|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---|

Ajouter :

- taux d'appariement CIAP pondéré par masse et par effectif ;
- contribution des codes ambigus/non appariés ;
- écarts de niveau et de croissance ;
- comparaison avec et sans secteur public ;
- comparaison avec et sans rappels/déclarations tardives ;
- analyse de sensibilité \(K\), périodicité et trimming.

Les parts officielles de rémunération des salariés dans la valeur ajoutée formelle — 30,4 % en
2022 et 28,9 % en 2023 — sont des repères macroéconomiques contextuels. Elles ne valident pas
directement une moyenne salariale CNPS et ne doivent pas être utilisées comme cible de calage.

## 6. Séquence opérationnelle de finalisation

1. Se connecter au VPN et figer un snapshot en lecture seule des entrées.
2. Calculer et archiver empreintes, inventaire et millésime de chaque source.
3. Exécuter le pipeline complet ; ne pas exporter si une erreur de validation existe.
4. Produire le tableau historique des contrôles et expliquer toute rupture.
5. Rejouer \(K=6,12,24,\\infty\), périodicité inconnue `monthly/daily` et variantes de
   clipping/trimming prévalidées.
6. Faire valider la table CIAP–NAPCN puis agréger les masses annuelles.
7. Réconcilier D.11/D.12/D.1 à concepts et périmètres constants.
8. Faire viser population cible, confidentialité, limites et choix méthodologiques.
9. Versionner immuablement les sorties, le rapport de validation et le manifeste de session.
10. Diffuser uniquement les estimations ponctuelles jusqu'à validation de F.1.

## 7. Points restant ouverts

| Point | Statut | Condition de clôture |
|---|---|---|
| F.1 — variance et intervalles | Non implémenté volontairement | Spécification formelle, revue indépendante et couverture DGP. |
| Lignes salariés totalement omises | Non identifiable avec les données actuelles | Roster indépendant ou analyse de sensibilité institutionnelle. |
| Cessation employeur | Censurée | Registre de radiation/activité. |
| CIAP–NAPCN | À obtenir/valider | Table officielle versionnée et visée. |
| Seuils de confidentialité | Garde-fous techniques | Homologation ANStat/CNPS. |
| Sorties MinIO immuables | Partiel | Préfixes par UUID ou versionnement objet. |
| Valeurs réelles et sensibilités | En attente du VPN | Exécution complète et rapport de contrôle signé. |
| AIPW véritable | Différé | Estimand stabilisé, roster/m(X) individuel complet, cross-fitting et inférence adaptée. |

## 8. Sécurité et passation

- Aucun appel MinIO n'a été effectué pendant cette finalisation.
- Le fichier local de variables d'environnement est ignoré par Git.
- Les identifiants déjà utilisés sur un poste de travail doivent être révoqués ou renouvelés
  avant mise en production, sans jamais être copiés dans un rapport.
- Le mode production doit imposer TLS et des secrets fournis par l'environnement.
- Aucun commit, push ou changement de branche n'a été réalisé dans cette intervention.

## Conclusion

La branche est prête pour une **recette sur données réelles**, pas encore pour une diffusion
définitive. Le principal progrès est d'avoir remplacé une robustesse affichée mais invalide par
une chaîne IPW contrôlable, traçable et honnête sur ses hypothèses. La finalisation des valeurs
nécessite maintenant trois apports externes : les données réelles via VPN, la passerelle officielle
CIAP–NAPCN avec les agrégats D.11/D.12/D.1, et les décisions institutionnelles sur le champ,
la confidentialité et l'inférence.
