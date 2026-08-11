# Rapport de correction — borne gauche du panel entreprise

**Branche** : `fix/audit-phase-b` · **Régression corrigée** : `b54330f` · **Date** : 11 août 2026

---

## 1. Le symptôme

Le run `python run.py run --from NETTOYAGE_DONNEES --to EXPORT_EXCEL` s'interrompt à la sixième
étape, après 462 secondes :

| Étape | Statut | Durée |
|---|---|---|
| Nettoyage des données | OK | 53,6 s |
| Base individus | OK | 56,0 s |
| Base entreprises | OK | 23,5 s |
| Base analytique | OK | 71,8 s |
| Modèle de déclaration | OK | 41,9 s |
| **Modèle de déclaration individuel** | **ERROR** | **212,0 s** |

```
ValueError: Des lignes analytiques n'ont pas de correspondance dans firm_base.
  src/cnps/07b_modele_declaration_indiv.py:243
```

L'erreur est un **symptôme**. Sa cause est trois étapes en amont, à l'étape 05.

---

## 2. La cause

### Ce que fait l'étape 05

Le fichier CNPS contient une ligne par salarié et par mois. L'étape 05 le résume en une ligne
par employeur et par mois — 1 290 497 lignes. Mais ce résumé ne contient que les mois où
l'employeur a **effectivement déclaré**, alors que le modèle sert précisément à corriger la
non-déclaration. Il lui faut donc aussi les mois où l'entreprise existait sans rien déclarer.

L'étape 05 construit donc un **panel** : pour chaque employeur, tous les mois entre sa date de
démarrage et la fin de la période d'étude, sur lesquels viennent se raccrocher les mois
réellement déclarés. D'où 1 455 158 lignes.

### Le défaut

La date de démarrage retenue était la seule `DATE_IMMAT_EMPLOYEUR`. Or certains employeurs
déclarent des salariés **avant** leur date d'immatriculation — l'étape 03 le journalise déjà :

```
73632 dates de DATE_IMMAT_EMPLOYEUR posterieures au mois de reference sont masquees
dans AGE_ENTREPRISE_IMMAT.
```

L'étape 03 ne masquait que la durée dérivée `AGE_ENTREPRISE_IMMAT` ; la colonne brute partait
intacte vers l'étape 05.

Exemple d'un employeur immatriculé en juin 2024 mais présent dans les fichiers dès janvier :

| | jan. 2024 | fév. 2024 | … | juin 2024 |
|---|---|---|---|---|
| Déclarations réelles | ✅ | ✅ | ✅ | ✅ |
| Panel construit | ❌ inexistant | ❌ inexistant | ❌ | ✅ |

Le code (`firm_panel.py`) construisait la grille des mois **puis** y raccrochait les
déclarations en jointure `left` *depuis la grille* :

```python
firms.join(periods, how="cross")
     .filter(pl.col("PERIOD_INDEX") >= pl.col("DEBUT_INDEX"))   # borne posee AVANT
     .join(observed, on=["ID_EMPLOYEUR", "PERIOD"], how="left")  # l'observe se raccroche APRES
```

Les déclarations de janvier à mai n'avaient aucune case où atterrir : elles disparaissaient,
sans erreur ni avertissement. Cas extrême, une immatriculation postérieure à la fin du panel
était ramenée à celle-ci par le `clip`, et l'employeur ne conservait qu'**un seul** mois.

Les lignes salariés correspondantes, elles, survivaient dans `individual_base` puis dans
`analytical_base` — la jointure de l'étape 06 est un `left join` sans contrôle de couverture.
L'étape 07b était la première à les détecter.

### C'est une régression de `b54330f`

Avant ce commit, le panel était un produit cartésien intégral : toute paire observée était
garantie présente. Et l'étape 07b **tolérait** les non-appariements, en attribuant un poids par
défaut de 1,0. Le commit d'audit a changé les deux côtés simultanément : un panel qui peut
perdre des lignes observées, et une validation stricte qui l'interdit.

Il applique le lot C.1 de `plan_correction_v2.md`, dont le critère d'acceptation — « aucune
ligne ne précède l'immatriculation » — a été appliqué au squelette du panel sans prévoir le cas
où **les données observées contredisent la date d'immatriculation**.

Deux autres hypothèses ont été écartées sur le journal du run : aucune clé incomplète
(`[Filtre 3/5] 0 lignes a cle incomplete`), et le run enchaîne les étapes 03 à 07b d'un seul
tenant, excluant un mélange entre bases produites par des exécutions différentes.

---

## 3. Ce qui change

### 3.1 Borne gauche du panel — `firm_panel.py:118`

La borne gauche devient le **plus tôt** entre le mois d'immatriculation et le premier mois
déclaré :

```python
pl.min_horizontal("_IMMAT_INDEX", "PREMIERE_APPARITION_INDEX").alias("_DEBUT_INDEX_BRUT")
```

Justification : une déclaration observée prouve que l'entreprise existait, quelle que soit sa
date d'immatriculation. Aucune donnée réelle n'est perdue, et les mois d'existence sans
déclaration — la non-déclaration que le modèle doit mesurer — restent dans le panel.

Effet de bord voulu : la première apparition étant par construction dans les bornes du panel,
le cas pathologique « immatriculation postérieure à la fin du panel » disparaît.

### 3.2 Traçabilité — `firm_panel.py:123`

Nouvel indicateur `DECLARATION_AVANT_IMMAT`, propagé dans `firm_base` et dénombré au journal :

```
Borne gauche : N entreprises declarent avant leur date d'immatriculation;
le panel demarre a la premiere declaration observee pour celles-ci.
```

### 3.3 Date d'immatriculation lue au-delà du premier mois — `firm_panel.py:101`

`.first()` devient `.drop_nulls().first()`. Auparavant, si le premier mois observé d'un
employeur ne renseignait pas la date alors que les mois suivants le faisaient, la date était
perdue et le début marqué « imputé » à tort.

### 3.4 Garde-fou anti-perte silencieuse — `firm_panel.py:155-160`

La colonne `_LIGNE_SOURCE` était créée, jamais utilisée, puis supprimée : le vestige d'un
garde-fou mort. Elle est réactivée. Toute ligne observée non couverte par le panel interrompt
désormais l'étape 05 :

```
Le panel ne couvre pas toutes les lignes observees: N couples (ID_EMPLOYEUR, PERIOD)
perdus a l'expansion sur M.
```

L'invariant « panel ⊇ observé » casse là où il naît, et non trois étapes et trois minutes plus
loin.

### 3.5 Fenêtre extensible mesurée depuis l'entrée de l'entreprise — `firm_panel.py:195`

```python
extensible = pl.col("PERIOD_INDEX") - pl.col("DEBUT_INDEX") + 1 < k   # etait: panel_start
```

`FENETRE_RISQUE_EXTENSIBLE` signale les mois disposant de moins de `K = 12` mois d'historique.
L'ancienneté était mesurée depuis le début **commun** du panel : un employeur entrant en 2025
dans un panel démarrant en 2024 n'était jamais marqué, malgré un historique tronqué.

C'est une covariable du modèle de déclaration : **cette correction déplace les propensions, donc
les indicateurs**. C'est attendu et voulu — la branche reconstruit des chiffres non publiés.

### 3.6 Contrôle de couverture réparé à l'étape 09 — `09_ponderation_finale.py:50-68`

Le contrôle « lignes analytiques sans poids entreprise » était inopérant précisément sur les
lignes qu'il devait attraper : la portée testée, `DANS_UNIVERS_RISQUE == 1`, vaut `null` sur une
ligne non appariée — puisque la colonne vient de la jointure — donc le filtre les écartait et le
compteur restait à zéro. Plus loin, `fill_null(0)` les classait hors univers et leur attribuait
un poids nul, silencieusement.

Un marqueur `_APPARIE_FIRM` est désormais posé sur la table entreprise avant la jointure, et
tout non-appariement lève une erreur. Sans l'étape 07b en amont, le pipeline aurait produit des
indicateurs amputés sans aucune alerte.

### 3.7 Décompte de colonnes corrigé à l'étape 06 — `06_base_analytique.py:80`

Le journal annonçait « 30 colonnes entreprise ajoutées » alors que la base passe de 47 à 75
colonnes, soit 28 : les deux clés de jointure, déjà présentes côté salariés, étaient comptées à
tort. Correction cosmétique, mais les journaux sont la piste d'audit du pipeline.

### 3.8 Tests de non-régression — `tests/test_data_quality_synthetic.py`

Le test existant `test_risk_window_uses_only_strictly_prior_months_and_asof_attributes` ne
fournit **aucune** colonne `DATE_IMMAT_EMPLOYEUR` : il n'empruntait donc jamais le chemin
fautif. Trois tests sont ajoutés :

| Test | Ce qu'il verrouille |
|---|---|
| `test_panel_keeps_declarations_anterior_to_the_registration_date` | anti-jointure vide entre observé et panel ; `DECLARATION_AVANT_IMMAT` ; absence de mois fictif pour un employeur récent |
| `test_registration_date_is_read_beyond_the_first_observed_month` | date absente le premier mois ⇒ pas de début imputé |
| `test_extensible_window_is_measured_from_the_firm_entry` | `[1, 1, 0]` avec `K = 3` pour un employeur entré deux mois après le début commun |

### 3.9 Documentation

`methodology.md` §3.2 et §3.3, et le critère d'acceptation du lot C.1 de
`plan_correction_v2.md` — source directe de la régression — sont réécrits.

---

## 4. Ce qui ne change pas

Les acquis de l'audit sont conservés. Ce correctif n'est **pas** un retour à l'avant-`b54330f` :

- **pas de mois fictifs** avant l'existence de l'entreprise — un employeur immatriculé en 2025
  ne récupère aucun mois en 2024 ; l'« univers statistique fictif » relevé par l'audit reste
  corrigé ;
- **pas de poids par défaut à 1,0** — la tolérance de l'ancien 07b n'est pas rétablie ; elle est
  au contraire étendue à l'étape 09, où elle était restée inopérante ;
- **aucune cessation inférée** de la dernière déclaration ; chaque employeur reste prolongé
  jusqu'à la fin commune du panel ;
- la portée glissante continue de ne dépendre que du passé **strictement** antérieur.

---

## 5. Effet chiffré

Les tests hors ligne sont passés localement : **74 tests, 0 échec**, `ruff check` propre. Les
trois nouveaux tests échouent bien sur le code d'origine — vérifié en restaurant
temporairement `firm_panel.py` depuis `HEAD` — ce qui confirme qu'ils verrouillent le défaut.

Les chiffres sur données réelles nécessitent un run depuis MinIO. **À compléter après
exécution** de `python run.py run --from BASE_ENTREPRISES --to EXPORT_EXCEL` :

| Indicateur | Avant (run du 11/08) | Après |
|---|---|---|
| Lignes du panel | 1 455 158 | *à mesurer — attendu en hausse* |
| Couples observés orphelins | > 0 (crash 07b) | *attendu : 0* |
| Employeurs `DECLARATION_AVANT_IMMAT` | non mesuré | *à mesurer* |
| Lignes en fenêtre extensible | 667 097 | *à mesurer — attendu en hausse* |
| AUC OOF du modèle 07 | 0,8752 | *à mesurer* |
| Pente de calibration | 1,0004 | *à mesurer* |
| Bornes de propension | [0,0453 ; 0,9939] | *à mesurer* |
| Étape 07b | ERROR à 212 s | *attendu : OK* |

Diagnostic de couverture à rejouer après le run — il doit retourner zéro ligne :

```python
manquants = (
    analytical.select(["ID_EMPLOYEUR", "PERIOD"]).unique()
    .join(firm.select(["ID_EMPLOYEUR", "PERIOD"]), on=["ID_EMPLOYEUR", "PERIOD"], how="anti")
)
```

---

## 6. Reste à arbitrer

**`06_base_analytique.py:56`** — le filtre `c not in indiv_cols` écarte de la base analytique les
attributs entreprise *as-of* calculés par le lot C.2 (`SECTEUR_ACTIVITE`, `COMMUNE`,
`CLASSE_EFFECTIF`…), parce que des colonnes homonymes existent déjà côté salariés. La base
analytique conserve donc les valeurs individuelles brutes, non propagées.

La conséquence est limitée — les mois sans aucune ligne salarié n'existent pas dans cette base —
mais sur une ligne où l'attribut individuel est nul alors que la valeur entreprise est connue,
la ventilation de l'étape 10 classe en « non renseigné » au lieu d'utiliser la valeur
entreprise. Le travail du lot C.2 est ainsi partiellement inutilisé en aval.

Ce n'est pas un défaut mais un choix : quelle valeur doit porter la ventilation publiée. Non
tranché dans ce lot.
