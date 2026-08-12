# Lot C.3 — Propager les attributs entreprise *as-of* jusqu'à la ventilation publiée

> **Statut** : plan validé le 12/08/2026, non implémenté.
> **Branche** : `fix/audit-phase-b` (dernier commit `5836b07`).
> **Suite de** : lot C.2 (`docs/plan_correction_v2.md:324`).

---

## 1. Contexte

L'audit du 01/08/2026 (`docs/rapport_audit_et_decisions.md`) a conclu que les indicateurs du
pipeline CNPS v2 ne sont pas publiables en l'état. La branche `fix/audit-phase-b` en corrige les
défauts par lots ; le commit `5836b07` a réparé la borne gauche du panel entreprise. Un point
était resté explicitement non tranché dans `docs/rapport_correction_panel_entreprise.md` §6 — il
l'est désormais, et fait l'objet du présent lot.

### Le problème

Le lot C.2 a rendu propres, au sens temporel, les attributs décrivant l'employeur :
`SECTEUR_ACTIVITE`, `COMMUNE`, `CLASSE_EFFECTIF`, `CLASSE_EFFECTIF_REDUITE`, et les dérivées
d'âge `CL_AGE_ENTREPRISE`, `AGE_ENTREPRISE_MOIS`, `AGE_ENTREPRISE_IMMAT`. Le panel les propage
par **dernière valeur connue antérieure** (`forward_fill`), sans jamais rétropropager une
information future, et pose un drapeau `JAMAIS_OBSERVE_AVANT_<attr>` quand rien n'est encore
connu — `src/cnps/firm_panel.py:211-226`.

Ce travail n'atteint jamais les tableaux publiés. À `src/cnps/06_base_analytique.py:56` :

```python
firm_value_cols = [c for c in firm.columns if c not in indiv_cols or c in firm_join_cols]
```

La règle « ne joindre que les colonnes absentes de la base individus » existe pour éviter les
collisions de noms (`_right`). Mais ces attributs **existent déjà côté individus** : l'étape 03
les fabrique ligne à ligne (`src/cnps/03_nettoyage_donnees.py:695-710`), et la base individus
reprend toutes les colonnes de `cnps_cleaned` (`src/cnps/04_base_individus.py:54`). Ils sont donc
silencieusement écartés, et la base analytique conserve la valeur individuelle brute, non
propagée.

Conséquence sur une ligne où l'attribut individuel est nul alors que la valeur entreprise est
connue — l'étape 10 ventile en `None` :

| Mois | Ligne salarié (base individus) | Panel entreprise *as-of* | Ventilation actuelle |
|---|---|---|---|
| janvier | Commerce | Commerce | Commerce |
| **février** | *(vide)* | **Commerce** *(reporté de janvier)* | **`None`** |
| mars | Commerce | Commerce | Commerce |

`10_estimation_indicateurs.py:433-438` fait un `group_by` direct et construit le libellé par
`str(group_vals)` : le groupe nul sort avec le libellé littéral `"None"` dans les tableaux
publiés.

### L'arbitrage retenu

**La valeur déclarée sur la ligne fait foi ; elle est complétée, et jamais remplacée, par la
dernière valeur entreprise connue antérieure.** Aucune ligne renseignée n'est modifiée ; seuls
les trous se referment.

Les deux autres options ont été écartées :
- *faire primer l'entreprise partout* — écrase des valeurs individuelles pourtant observées,
  arbitrées par un `last()` non déterministe ;
- *statu quo* — laisse le travail du lot C.2 inutilisé en aval de la modélisation.

### Résultat attendu

Moins de modalités « non renseigné » dans les tableaux de l'étape 10, une ventilation
reproductible d'un run à l'autre, et le lot C.2 enfin utilisé jusqu'à la publication. **Les
chiffres publiés se déplacent — c'est voulu**, et l'écart avant/après doit être chiffré avant
toute diffusion.

---

## 2. Travaux

### 2.1 Jointure par complément — `src/cnps/06_base_analytique.py`

Remplacer le filtre d'exclusion par une jointure suffixée suivie d'un `coalesce`, pour les seuls
attributs *as-of*.

Réutiliser la constante existante `_ASOF_ATTRIBUTES` (`src/cnps/firm_panel.py:14`) plutôt que de
redéclarer la liste — l'exposer publiquement si nécessaire — en y ajoutant les dérivées
`CL_AGE_ENTREPRISE`, `AGE_ENTREPRISE_MOIS`, `AGE_ENTREPRISE_IMMAT` produites par le même bloc
(`firm_panel.py:218-226`).

Mécanique :

- colonne entreprise **absente** côté individus → jointure inchangée ;
- colonne de la liste *as-of* **présente des deux côtés** → jointe avec `suffix="_FIRM"`, puis
  `pl.coalesce([pl.col(c), pl.col(f"{c}_FIRM")]).alias(c)`, puis la colonne `_FIRM` est
  supprimée ;
- toute autre colonne homonyme → exclue comme aujourd'hui (comportement conservé).

Joindre également les drapeaux `JAMAIS_OBSERVE_AVANT_<attr>` (`firm_panel.py:216`) : ils ne
portent pas d'homonyme et tracent ce qui reste inconnu après complément.

Journaliser le décompte des valeurs complétées, **par attribut** — c'est ce chiffre qui alimente
le tableau avant/après du rapport.

**Extraire la logique de jointure en fonction pure** de signature `(indiv, firm) -> DataFrame`,
sur le modèle de `firm_panel.py`, pour la rendre testable sans MinIO. `construire_base_analytique`
conserve la lecture/écriture et les invariants existants, qui restent valables tels quels :
unicité de `firm` sur les clés (`:67-72`) et cardinalité préservée (`:82-86`).

### 2.2 Rendre le report d'attributs déterministe — `src/cnps/05_base_entreprises.py:136-138`

```python
for attr in firm_attrs:
    agg_exprs.append(pl.col(attr).drop_nulls().last().alias(attr))
```

Ce `last()` est évalué après un `group_by` sans tri ni `maintain_order` (`:140`). Quand deux
salariés d'une même entreprise-mois portent des valeurs divergentes, la valeur retenue **n'est
pas reproductible d'un run à l'autre**.

Remplacer par un critère explicite et déterministe : **modalité la plus fréquente** dans
l'entreprise-mois, départage par ordre lexicographique. Journaliser le nombre d'entreprises-mois
où plusieurs modalités coexistent.

Défaut mécanique, pas méthodologique : il entre dans ce lot conformément à la règle de portée
retenue sur cette branche (ne pas différer un défaut connu dans une fonction qu'on réécrit).

### 2.3 Modalité manquante explicite — `src/cnps/10_estimation_indicateurs.py:433-438`

Remplacer le libellé `"None"` produit par `str(group_vals)` par `"Non renseigné"`, au moment de
construire `group_label`. **Ne pas toucher au `group_by` lui-même** : les groupes nuls doivent
rester comptés, pas éliminés.

### 2.4 Tests hors ligne — `tests/test_data_quality_synthetic.py`

Trois cas sur jeu synthétique, dans le style des tests existants du fichier
(`importlib.import_module`, DataFrames construits à la main) :

1. **complément sans écrasement** — sur un mois où l'attribut individuel est renseigné et diffère
   de la valeur entreprise, la valeur individuelle est conservée ; sur un mois où il est nul, la
   valeur entreprise apparaît ;
2. **pas d'information future** — une valeur entreprise observée pour la première fois en mars ne
   remplit aucun trou de janvier ni février ; `JAMAIS_OBSERVE_AVANT_*` vaut 1 sur ces lignes ;
3. **déterminisme du report** — deux entreprises-mois identiques construites dans un ordre de
   lignes inversé produisent le même attribut.

Le test `test_risk_window_uses_only_strictly_prior_months_and_asof_attributes` (`:96`) reste la
garde du lot C.2 en amont — **ne pas le modifier**.

### 2.5 Documentation

- `docs/rapport_correction_panel_entreprise.md` §6 : remplacer « Reste à arbitrer » par la
  décision retenue et sa justification ; ajouter au tableau §5 les lignes de suivi (valeurs
  complétées par attribut, entreprises-mois à modalités divergentes, part de « Non renseigné »
  avant/après par dimension).
- `docs/plan_correction_v2.md` : consigner ce lot à la suite de C.2, avec son critère
  d'acceptation.

---

## 3. Critère d'acceptation

- Aucune valeur individuelle renseignée n'est modifiée par la jointure.
- Aucun attribut d'un mois `t` ne dépend d'une observation postérieure à `t` (vérifié par test
  sur jeu synthétique).
- La cardinalité de la base analytique est inchangée : `analytical.height == indiv.height`.
- Le report d'attributs de l'étape 05 est identique quel que soit l'ordre des lignes en entrée.
- Aucun libellé `"None"` ne subsiste dans les sorties de l'étape 10.
- Les décomptes de complétion et de divergence apparaissent dans les logs.

---

## 4. Vérification

### Hors ligne

```bash
pytest -q
```

Les 74 tests existants doivent rester verts, plus les trois nouveaux. **C'est la seule
vérification possible depuis un poste sans accès MinIO.**

### Sur le run réel — depuis Jupyter (`/home/jovyan`)

```bash
python run.py run --from BASE_ENTREPRISES --to EXPORT_EXCEL
```

À relever dans les logs pour compléter le tableau §5 du rapport :

- nombre de valeurs complétées par attribut (étape 06) ;
- nombre d'entreprises-mois à modalités divergentes (étape 05) ;
- part de « Non renseigné » par dimension, à comparer aux tableaux du run précédent ;
- les repères d'avant, restés à confirmer : panel 1 455 158 lignes, 667 097 lignes en fenêtre
  extensible, AUC 0,8752, pente de calibration 1,0004, propensions [0,0453 ; 0,9939].

Le diagnostic de couverture de `docs/rapport_correction_panel_entreprise.md:229-236` doit
toujours retourner zéro ligne : ce lot ne touche pas à la cardinalité de la jointure, l'invariant
`06:82-86` le garantit.
