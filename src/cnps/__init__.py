"""
CNPS Treatment Pipeline v2.0
=============================
Pipeline de traitement statistique des declarations salariales
de la Caisse Nationale de Prevoyance Sociale (CNPS) - Cote d'Ivoire.

Methodologie:
- Estimation doublement robuste (AIPW) pour la correction du biais de non-declaration
- Imputation multiple par equations chainees (MICE) avec regles de Rubin
- Traitement parallele via Polars (lazy evaluation) et Joblib

References:
    Robins, Rotnitzky & Zhao (1994). Estimation of regression coefficients
        when some regressors are not always observed. JASA, 89(427), 846-866.
    Rubin (1987). Multiple Imputation for Nonresponse in Surveys. Wiley.
    Bang & Robins (2005). Doubly robust estimation in missing data and
        causal inference models. Biometrics, 61(4), 962-973.
"""

__version__ = "2.0.0"
