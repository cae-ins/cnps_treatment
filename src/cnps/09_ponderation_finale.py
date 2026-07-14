"""
Etape 9/12 — Ponderation finale (IPW / AIPW).

Calcule les poids analytiques finaux en combinant les poids entreprise
(issus du modele de declaration, etape 7) et les poids individus,
selon la methode configuree :

1. **IPW** (Inverse Probability Weighting) :
   Ponderation de Horvitz-Thompson standard par les scores de propension.
   Consistante si le modele de propension est correctement specifie.

2. **AIPW** (Augmented IPW / doublement robuste) :
   Combine un modele de resultat (imputation, etape 8) avec le modele
   de propension. Consistante si *au moins un* des deux modeles est
   correctement specifie (Bang & Robins, 2005).

L'AIPW est la methode recommandee par defaut pour ce pipeline, car elle
protege contre une mauvaise specification de l'un ou l'autre modele.

Estimateur AIPW de la moyenne
-------------------------------
mu_AIPW = (1/N) * somme_j [
    D_j * Y_j / p_j  -  (D_j - p_j) / p_j * m(X_j)
]

ou :
- D_j = indicateur de declaration
- Y_j = resultat observe (salaire)
- p_j = score de propension estime
- m(X_j) = resultat estime (salaire impute par le modele d'imputation)

References
----------
Robins, J. M., Rotnitzky, A. & Zhao, L. P. (1994). Estimation of regression
    coefficients when some regressors are not always observed. *JASA*,
    89(427), 846-866.
Bang, H. & Robins, J. M. (2005). Doubly robust estimation in missing data
    and causal inference models. *Biometrics*, 61(4), 962-973.
"""

from __future__ import annotations

import numpy as np
import polars as pl
from loguru import logger

from cnps.config import PipelineConfig
from cnps.storage import object_exists, read_parquet, write_parquet


def _compute_ipw_weights(
    p_hat: np.ndarray,
    d: np.ndarray,
    trim_lower: float,
    trim_upper: float,
) -> np.ndarray:
    """Calcule des poids IPW stabilises et tronques."""
    epsilon = 1e-6
    p_clipped = np.clip(p_hat, epsilon, 1 - epsilon)

    marginal = d.mean()
    w = np.where(d == 1, marginal / p_clipped, 0.0)

    nonzero = w[w > 0]
    if len(nonzero) > 0:
        lo = np.quantile(nonzero, trim_lower)
        hi = np.quantile(nonzero, trim_upper)
        w = np.clip(w, 0, hi)
        w[w > 0] = np.clip(w[w > 0], lo, hi)

    return w


def _compute_aipw_weights(
    y_obs: np.ndarray,
    y_imputed: np.ndarray,
    p_hat: np.ndarray,
    d: np.ndarray,
    trim_lower: float,
    trim_upper: float,
) -> tuple[np.ndarray, float]:
    """
    Calcule les poids AIPW (doublement robustes) et l'estimation ponctuelle AIPW.

    Returns
    -------
    weights : np.ndarray
        Poids ajustes pour chaque observation.
    mu_aipw : float
        Estimation AIPW de la moyenne de population.
    """
    epsilon = 1e-6
    p_clipped = np.clip(p_hat, epsilon, 1 - epsilon)

    ipw_component = d * y_obs / p_clipped
    augmentation = (d - p_clipped) / p_clipped * y_imputed
    mu_aipw = float(np.mean(ipw_component - augmentation))

    w_ipw = _compute_ipw_weights(p_hat, d, trim_lower, trim_upper)

    # Facteur d'augmentation pour les unites observees
    observed_mask = d == 1
    w_aipw = w_ipw.copy()
    if observed_mask.any():
        y_safe = np.where(np.abs(y_obs) > epsilon, y_obs, epsilon)
        aug_ratio = 1.0 - (1.0 - p_clipped) * y_imputed / y_safe
        aug_ratio = np.clip(aug_ratio, 0.5, 2.0)  # stabilisation
        w_aipw[observed_mask] = w_ipw[observed_mask] * aug_ratio[observed_mask]

    nonzero = w_aipw[w_aipw > 0]
    if len(nonzero) > 0:
        lo = np.quantile(nonzero, trim_lower)
        hi = np.quantile(nonzero, trim_upper)
        w_aipw[w_aipw > 0] = np.clip(w_aipw[w_aipw > 0], lo, hi)

    return w_aipw, mu_aipw


def calculer_poids_finaux(cfg: PipelineConfig) -> str:
    """
    Calcule les poids analytiques finaux selon la methode configuree.

    Parameters
    ----------
    cfg : PipelineConfig
        Configuration du pipeline.

    Returns
    -------
    str
        Nom de l'objet de la base analytique mise a jour avec les poids finaux.
    """
    analytical_object = f"{cfg.minio.cleaned_prefix}analytical_base.parquet"

    if not object_exists(cfg.minio, analytical_object):
        raise FileNotFoundError(f"Base analytique introuvable : {analytical_object}")

    df = read_parquet(cfg.minio, analytical_object)
    method = cfg.modeling.estimation_method

    logger.info("Calcul des poids finaux avec la methode : {}", method)

    if method == "aipw" and "P_HAT_JT" in df.columns:
        salary_col = "SALAIRE_BRUT_MENS" if "SALAIRE_BRUT_MENS" in df.columns else "SALAIRE_BRUT"

        if salary_col in df.columns and "P_HAT_JT" in df.columns:
            y_obs = df[salary_col].fill_null(0).to_numpy()
            p_hat = df["P_HAT_JT"].fill_null(0.5).to_numpy()
            d = df["D_JT"].fill_null(1).to_numpy().astype(float) if "D_JT" in df.columns \
                else np.ones(len(df))

            # Utilise la moyenne imputee au niveau entreprise comme prediction du modele de resultat
            y_imputed = df["SALAIRE_MOYEN"].fill_null(
                df[salary_col].mean()
            ).to_numpy() if "SALAIRE_MOYEN" in df.columns else y_obs.copy()

            w_aipw, mu_aipw = _compute_aipw_weights(
                y_obs, y_imputed, p_hat, d,
                cfg.modeling.ipw_trim_lower, cfg.modeling.ipw_trim_upper,
            )

            df = df.with_columns(pl.Series("W_FINAL", w_aipw))
            logger.info("Estimation AIPW du salaire moyen : {:.0f} FCFA", mu_aipw)
        else:
            logger.warning("Colonnes manquantes pour AIPW, repli sur IPW standard")
            df = df.with_columns(
                (pl.col("W_JT") * pl.col("W_INDIV")).alias("W_FINAL")
            )
    else:
        # --- IPW standard ---
        df = df.with_columns(
            (pl.col("W_JT") * pl.col("W_INDIV")).alias("W_FINAL")
        )

    # Normalisation des poids par strate (periode)
    if "PERIOD" in df.columns:
        df = df.with_columns(
            (pl.col("W_FINAL") / pl.col("W_FINAL").mean().over("PERIOD"))
            .alias("W_FINAL")
        )

    write_parquet(cfg.minio, analytical_object, df)
    logger.info("Poids finaux calcules : moyenne={:.3f}, ecart-type={:.3f}",
                df["W_FINAL"].mean(), df["W_FINAL"].std())

    return analytical_object
