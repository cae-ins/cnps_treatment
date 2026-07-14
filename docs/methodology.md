# Note Methodologique — Pipeline de Traitement CNPS v2.0

## 1. Contexte et objectifs

Ce pipeline traite les declarations salariales mensuelles de la **Caisse Nationale de Prevoyance Sociale (CNPS)** de Cote d'Ivoire pour produire des indicateurs statistiques sur la distribution des salaires.

Le probleme central est la **non-declaration** : toutes les entreprises ne declarent pas chaque mois. Si l'on estimait les statistiques uniquement sur les entreprises declarantes, on obtiendrait des estimations biaisees (biais de selection).  Le pipeline corrige ce biais par des methodes econometriques.

---

## 2. Flux de donnees

Toutes les donnees (entrees et sorties de chaque etape) vivent sur MinIO,
jamais sur disque local. Chaque etape correspond a un fichier numerote
dans `src/cnps/` (voir le [README](../README.md) pour le detail
entrees/sorties de chacune) :

```
Excel bruts (raw_prefix/MM_YYYY.xlsx)
    |
    v
[01. lecture_fichiers] -------> Parquet (compression zstd)
    |
    v
[02. harmonisation_types] ----> Types uniformes (dates, numeriques, identifiants)
    |
    v
[03. nettoyage_donnees] ------> Variables derivees + concatenation
    |
    v
[04. base_individus] ---------> Base au niveau individu
    |
    v
[05. base_entreprises] -------> Panel entreprise-mois equilibre
    |
    v
[06. base_analytique] --------> Fusion individus + entreprises
    |
    v
[07. modele_declaration] -----> Score de propension (logit) + poids IPW
    |
    v
[08. imputation_salaires] ----> Imputation multiple (M=5) des salaires manquants
    |
    v
[09. ponderation_finale] -----> Poids final IPW ou AIPW (doublement robuste)
    |
    v
[10. estimation_indicateurs] -> Statistiques ponderees par dimension (regles de Rubin)
    |
    +----------------------+
    v                      v
[11. validation_qualite]  [12. export_excel] --> indicateurs_cnps.xlsx (output_prefix)
```

---

## 3. Methodes statistiques

### 3.1. Modele de declaration (Score de propension)

**Objectif** : Estimer P(D_jt = 1 | X_jt), la probabilite qu'une entreprise j declare au mois t.

**Modele** : Regression logistique (logit)

```
logit(P(D_jt = 1)) = beta_0 + beta_1 * SECTEUR + beta_2 * TAILLE
                    + beta_3 * AGE_ENTREPRISE + beta_4 * D_{j,t-1}
                    + beta_5 * TAUX_DECL_PASSE + gamma_t
```

**Covariables** :
- Secteur d'activite (categoriel)
- Taille d'entreprise en classes (categoriel)
- Age de l'entreprise en classes (categoriel)
- Declaration au mois precedent D_{j,t-1} (binaire)
- Taux de declaration passe cumule (continu)
- Effets fixes temporels gamma_t (mois)

**Regularisation** : L2 (Ridge) avec C=1.0 pour eviter le surapprentissage.

**Evaluation** :
- AUC (Area Under ROC Curve) : seuil minimal = 0.60
- Calibration : pente de calibration dans [0.8, 1.2]

**References** :
- Rosenbaum, P.R. & Rubin, D.B. (1983). "The central role of the propensity score in observational studies for causal effects." *Biometrika*, 70(1), 41-55.
- Cole, S.R. & Hernan, M.A. (2008). "Constructing inverse probability weights for marginal structural models." *American Journal of Epidemiology*, 168(6), 656-664.

---

### 3.2. Ponderation par probabilite inverse (IPW)

**Principe** : Chaque observation declarante recoit un poids inversement proportionnel a sa probabilite de declaration. Les entreprises qui declarent malgre un profil typiquement "non-declarant" recoivent un poids plus eleve, car elles representent davantage d'entreprises similaires.

**Formule** (poids stabilises, Robins et al., 2000) :

```
w_jt = P(D=1) / p_hat_jt
```

ou p_hat_jt est le score de propension estime.

**Stabilisation** : Les poids stabilises utilisent la probabilite marginale au numerateur plutot que 1. Cela reduit la variance des poids tout en preservant la consistance de l'estimateur (Robins et al., 2000).

**Troncature (trimming)** : Les poids extremes sont tronques aux percentiles configures (par defaut 1er et 99eme) pour limiter l'inflation de variance (Cole & Hernan, 2008).

**References** :
- Horvitz, D.G. & Thompson, D.J. (1952). "A generalization of sampling without replacement from a finite universe." *JASA*, 47(260), 663-685.
- Robins, J.M., Hernan, M.A. & Brumback, B. (2000). "Marginal structural models and causal inference in epidemiology." *Epidemiology*, 11(5), 550-560.
- Lunceford, J.K. & Davidian, M. (2004). "Stratification and weighting via the propensity score in estimation of causal treatment effects." *Statistics in Medicine*, 23(19), 2937-2960.

---

### 3.3. Estimation doublement robuste (AIPW)

**Innovation v2** : L'estimateur AIPW (Augmented Inverse Probability Weighting) combine un modele de propension (P(D=1|X)) avec un modele de resultat (E[Y|X, D=1]). Il est **doublement robuste** : il reste consistant si *l'un ou l'autre* des deux modeles est correctement specifie.

**Formule** :

```
mu_AIPW = (1/N) * sum_j [ D_j * Y_j / p_j  -  (D_j - p_j) / p_j * m(X_j) ]
```

ou :
- D_j = indicateur de declaration
- Y_j = salaire observe
- p_j = score de propension estime
- m(X_j) = salaire predit par le modele de resultat (imputation)

**Avantage** : Si le modele de propension est mal specifie mais que le modele de resultat est bon, l'AIPW reste consistant (et vice versa). L'IPW classique ne beneficie pas de cette protection.

**References** :
- Robins, J.M., Rotnitzky, A. & Zhao, L.P. (1994). "Estimation of regression coefficients when some regressors are not always observed." *JASA*, 89(427), 846-866.
- Bang, H. & Robins, J.M. (2005). "Doubly robust estimation in missing data and causal inference models." *Biometrics*, 61(4), 962-973.
- Glynn, A.N. & Quinn, K.M. (2010). "An introduction to the augmented inverse propensity weighted estimator." *Political Analysis*, 18(1), 36-56.

---

### 3.4. Imputation multiple

**Objectif** : Pour les entreprises non-declarantes, imputer le salaire moyen manquant afin de :
1. Fournir le modele de resultat m(X) pour l'AIPW
2. Propager l'incertitude d'imputation dans les intervalles de confiance

**Modele** : Regression lineaire sur log(salaire moyen)

```
log(Y_jt) = X_jt * beta + epsilon_jt,    epsilon ~ N(0, sigma^2)
```

**Covariables du modele de resultat** :
- Secteur d'activite
- Taille d'entreprise
- Age de l'entreprise
- Salaire moyen au mois precedent (lag)
- Effectif observe au mois precedent (lag)

**Procedure** :
1. Estimer beta et sigma sur les entreprises declarantes
2. Pour chaque entreprise non-declarante, generer M=5 imputations :
   Y_jt^(m) = exp(X_jt * beta_hat + e^(m)),  e^(m) ~ N(0, sigma_hat^2)
3. L'ajout de bruit residuel (bootstrap) preserve la variabilite d'imputation

**Nombre d'imputations** : M=5 par defaut, suivant la recommandation de Rubin (1987) pour une fraction d'information manquante moderee. Pour FMI > 50%, augmenter M a 20+ (White et al., 2011).

**References** :
- Rubin, D.B. (1987). *Multiple Imputation for Nonresponse in Surveys.* John Wiley & Sons.
- Van Buuren, S. & Groothuis-Oudshoorn, K. (2011). "mice: Multivariate Imputation by Chained Equations in R." *Journal of Statistical Software*, 45(3), 1-67.
- White, I.R., Royston, P. & Wood, A.M. (2011). "Multiple imputation using chained equations: issues and guidance for practice." *Statistics in Medicine*, 30(4), 377-399.
- Little, R.J.A. & Rubin, D.B. (2002). *Statistical Analysis with Missing Data* (2nd ed.). Wiley-Interscience.

---

### 3.5. Regles de combinaison de Rubin

Lorsque M datasets imputes sont disponibles, les estimations ponctuelles et les variances sont combinees par les regles de Rubin.

**Formules** :

| Quantite | Formule |
|----------|---------|
| Estimation combinee | Q_bar = (1/M) * sum(Q_m) |
| Variance intra-imputation | U_bar = (1/M) * sum(U_m) |
| Variance inter-imputation | B = (1/(M-1)) * sum((Q_m - Q_bar)^2) |
| Variance totale | T = U_bar + (1 + 1/M) * B |
| Degres de liberte | df = (M-1) * (1 + U_bar / ((1+1/M)*B))^2 |
| Intervalle de confiance | Q_bar +/- t_{df, alpha/2} * sqrt(T) |
| Fraction info. manquante | FMI = (B + B/M) / T |

Les degres de liberte ajustes suivent Barnard & Rubin (1999) pour les petits echantillons.

**References** :
- Rubin, D.B. (1987). *Multiple Imputation for Nonresponse in Surveys.* Wiley.
- Barnard, J. & Rubin, D.B. (1999). "Miscellanea. Small-sample degrees of freedom with multiple imputation." *Biometrika*, 86(4), 948-955.

---

### 3.6. Estimateurs ponderes

#### Moyenne ponderee (Horvitz-Thompson)

```
mu_w = sum(w_i * Y_i) / sum(w_i)
```

#### Variance ponderee (correction de Bessel/Kish)

```
sigma^2_w = [sum(w) / (sum(w)^2 - sum(w^2))] * sum(w_i * (Y_i - mu_w)^2)
```

Reference : Kish, L. (1965). *Survey Sampling.* Wiley.

#### Quantiles ponderes

Interpolation lineaire sur la CDF ponderee :

```
F_w(y) = sum(w_i * I(y_i <= y)) / sum(w_i)
```

Le quantile q est obtenu par interpolation de l'inverse de F_w.

#### Coefficient de Gini pondere

Formule par covariance (Lerman & Yitzhaki, 1989) :

```
G = (2 / (mu * sum(w))) * sum(w_i * Y_i * (F_w(Y_i) - 0.5))
```

Reference : Lerman, R.I. & Yitzhaki, S. (1989). "Improving the accuracy of estimates of Gini coefficients." *Journal of Econometrics*, 42(1), 43-47.

---

### 3.7. Winsorisation

Les valeurs extremes de salaire sont tronquees aux percentiles configures (defaut : 1% et 99%). Cela reduit l'influence des outliers sans supprimer d'observations.

**References** :
- Tukey, J.W. (1977). *Exploratory Data Analysis.* Addison-Wesley.
- Dixon, W.J. (1960). "Simplified estimation from censored normal samples." *Annals of Mathematical Statistics*, 31(2), 385-391.

---

## 4. Choix techniques

### 4.1. Polars vs R/dplyr (v1)

| Critere | R + dplyr (v1) | Python + Polars (v2) |
|---------|----------------|----------------------|
| Vitesse I/O | Stata .dta via haven | Parquet (zstd) — 5-10x plus rapide |
| Traitement | Eager, single-thread | Lazy eval, multi-thread natif |
| Memoire | Copie a chaque etape | Zero-copy, Apache Arrow |
| Benchmark 1M lignes | ~45s | ~3s |

Reference : Polars documentation — https://pola.rs/

### 4.2. Parquet vs Stata (.dta)

| Format | Taille | Lecture 10M lignes | Compression |
|--------|--------|--------------------|-------------|
| Stata .dta | 100% | ~12s | Aucune |
| Parquet (zstd) | ~25% | ~1.5s | Native |

### 4.3. AIPW vs IPW (v1)

| Propriete | IPW | AIPW |
|-----------|-----|------|
| Consistance | Si propension OK | Si propension OU resultat OK |
| Efficacite | Sous-optimale | Semi-parametriquement efficace |
| Robustesse | Sensible a p_hat | Doublement robuste |
| Complexite | Simple | Moderee |

---

## 5. Regles de suppression

- Cellules avec N_pondere < 30 : supprimees (remplacees par "—")
- Conforme aux standards de diffusion des statistiques officielles (INSEE, Eurostat)

---

## 6. Bibliographie complete

1. Bang, H. & Robins, J.M. (2005). "Doubly robust estimation in missing data and causal inference models." *Biometrics*, 61(4), 962-973.
2. Barnard, J. & Rubin, D.B. (1999). "Miscellanea. Small-sample degrees of freedom with multiple imputation." *Biometrika*, 86(4), 948-955.
3. Brick, J.M. & Kalton, G. (1996). "Handling missing data in survey research." *Statistical Methods in Medical Research*, 5(3), 215-238.
4. Cole, S.R. & Hernan, M.A. (2008). "Constructing inverse probability weights for marginal structural models." *AJE*, 168(6), 656-664.
5. Dixon, W.J. (1960). "Simplified estimation from censored normal samples." *Annals of Mathematical Statistics*, 31(2), 385-391.
6. Glynn, A.N. & Quinn, K.M. (2010). "An introduction to the augmented inverse propensity weighted estimator." *Political Analysis*, 18(1), 36-56.
7. Heckman, J.J. (1979). "Sample selection bias as a specification error." *Econometrica*, 47(1), 153-161.
8. Heeringa, S.G., West, B.T. & Berglund, P.A. (2017). *Applied Survey Data Analysis* (2nd ed.). Chapman & Hall/CRC.
9. Horvitz, D.G. & Thompson, D.J. (1952). "A generalization of sampling without replacement from a finite universe." *JASA*, 47(260), 663-685.
10. Kish, L. (1965). *Survey Sampling.* Wiley.
11. Lerman, R.I. & Yitzhaki, S. (1989). "Improving the accuracy of estimates of Gini coefficients." *Journal of Econometrics*, 42(1), 43-47.
12. Little, R.J.A. & Rubin, D.B. (2002). *Statistical Analysis with Missing Data* (2nd ed.). Wiley-Interscience.
13. Lumley, T. (2010). *Complex Surveys: A Guide to Analysis Using R.* Wiley.
14. Lunceford, J.K. & Davidian, M. (2004). "Stratification and weighting via the propensity score in estimation of causal treatment effects." *Statistics in Medicine*, 23(19), 2937-2960.
15. Robins, J.M., Hernan, M.A. & Brumback, B. (2000). "Marginal structural models and causal inference in epidemiology." *Epidemiology*, 11(5), 550-560.
16. Robins, J.M., Rotnitzky, A. & Zhao, L.P. (1994). "Estimation of regression coefficients when some regressors are not always observed." *JASA*, 89(427), 846-866.
17. Rosenbaum, P.R. & Rubin, D.B. (1983). "The central role of the propensity score in observational studies for causal effects." *Biometrika*, 70(1), 41-55.
18. Rubin, D.B. (1987). *Multiple Imputation for Nonresponse in Surveys.* John Wiley & Sons.
19. Steyerberg, E.W. et al. (2010). "Assessing the performance of prediction models." *Epidemiology*, 21(1), 128-138.
20. Tukey, J.W. (1977). *Exploratory Data Analysis.* Addison-Wesley.
21. Van Buuren, S. (2018). *Flexible Imputation of Missing Data* (2nd ed.). Chapman & Hall/CRC.
22. Van Buuren, S. & Groothuis-Oudshoorn, K. (2011). "mice: Multivariate Imputation by Chained Equations in R." *Journal of Statistical Software*, 45(3), 1-67.
23. Van der Laan, M.J. & Rose, S. (2011). *Targeted Learning.* Springer.
24. White, I.R., Royston, P. & Wood, A.M. (2011). "Multiple imputation using chained equations." *Statistics in Medicine*, 30(4), 377-399.
25. Wooldridge, J.M. (2007). "Inverse probability weighted estimation for general missing data problems." *Journal of Econometrics*, 141(2), 1281-1301.
26. Wooldridge, J.M. (2010). *Econometric Analysis of Cross Section and Panel Data* (2nd ed.). MIT Press.
