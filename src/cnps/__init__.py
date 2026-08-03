"""
CNPS Treatment Pipeline v2.0
=============================
Pipeline de traitement statistique des declarations salariales
de la Caisse Nationale de Prevoyance Sociale (CNPS) - Cote d'Ivoire.

Methodologie:
- Estimation ponctuelle IPW a deux etages pour la non-reponse
- Diagnostics hors echantillon groupes par employeur
- Secret statistique primaire et suppression secondaire
- Traitement parallele de l'ingestion via Polars et Joblib

References:
    Wooldridge (2007). Inverse probability weighted estimation for general
        missing data problems. Journal of Econometrics, 141(2), 1281-1301.
    Lumley (2010). Complex Surveys. Wiley.
"""

__version__ = "2.0.0"
