"""
Weighting module: IPW, AIPW (doubly robust), and final weight computation.

This module implements three estimation strategies:

1. **IPW** (Inverse Probability Weighting):
   Standard Horvitz-Thompson reweighting by propensity scores.
   Consistent if the propensity model is correctly specified.

2. **AIPW** (Augmented IPW / Doubly Robust):
   Combines an outcome model (imputation) with a propensity model.
   Consistent if *either* model is correctly specified — hence "doubly
   robust" (Bang & Robins, 2005; Robins et al., 1994).

3. **TMLE** (Targeted Maximum Likelihood Estimation):
   Semiparametric efficient estimator that targets the parameter of
   interest (mean salary) directly.  Placeholder for future implementation.

The AIPW estimator is the recommended default for this pipeline, as it
provides protection against model misspecification on either the
propensity or outcome side.

AIPW estimator for the mean
----------------------------
mu_AIPW = (1/N) * sum_j [
    D_j * Y_j / p_j  -  (D_j - p_j) / p_j * m(X_j)
]

where:
- D_j = declaration indicator
- Y_j = observed outcome (salary)
- p_j = estimated propensity score
- m(X_j) = estimated outcome (imputed salary from the outcome model)

References
----------
Robins, J. M., Rotnitzky, A. & Zhao, L. P. (1994). Estimation of regression
    coefficients when some regressors are not always observed. *JASA*,
    89(427), 846-866.
Bang, H. & Robins, J. M. (2005). Doubly robust estimation in missing data
    and causal inference models. *Biometrics*, 61(4), 962-973.
Lunceford, J. K. & Davidian, M. (2004). Stratification and weighting via
    the propensity score in estimation of causal treatment effects.
    *Statistics in Medicine*, 23(19), 2937-2960.
Glynn, A. N. & Quinn, K. M. (2010). An introduction to the augmented
    inverse propensity weighted estimator. *Political Analysis*, 18(1), 36-56.
Van der Laan, M. J. & Rose, S. (2011). *Targeted Learning*. Springer.
"""

from __future__ import annotations

import numpy as np
import polars as pl
from loguru import logger

from cnps.config import PipelineConfig
from cnps.storage import read_parquet, write_parquet
from cnps.storage.minio_client import object_exists


def _compute_ipw_weights(
    p_hat: np.ndarray,
    d: np.ndarray,
    trim_lower: float,
    trim_upper: float,
) -> np.ndarray:
    """
    Compute stabilized, trimmed IPW weights.

    Parameters
    ----------
    p_hat : array
        Estimated propensity scores.
    d : array
        Declaration indicators (1 = observed).
    trim_lower, trim_upper : float
        Percentiles for weight trimming.

    Returns
    -------
    np.ndarray
        IPW weights.
    """
    epsilon = 1e-6
    p_clipped = np.clip(p_hat, epsilon, 1 - epsilon)

    # Stabilized weights
    marginal = d.mean()
    w = np.where(d == 1, marginal / p_clipped, 0.0)

    # Trim
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
    Compute AIPW (doubly robust) weights and the AIPW point estimate.

    The AIPW framework adjusts IPW weights using the outcome model
    predictions, providing double robustness.

    Returns
    -------
    weights : np.ndarray
        Adjusted weights for each observation.
    mu_aipw : float
        AIPW estimate of the population mean.
    """
    epsilon = 1e-6
    p_clipped = np.clip(p_hat, epsilon, 1 - epsilon)
    n = len(d)

    # AIPW estimator components
    # mu = (1/n) * sum[ D*Y/p - (D - p)/p * m(X) ]
    ipw_component = d * y_obs / p_clipped
    augmentation = (d - p_clipped) / p_clipped * y_imputed
    mu_aipw = float(np.mean(ipw_component - augmentation))

    # Effective weights for observed units (for downstream weighted estimation)
    # w_i = 1/p_i - (1 - D_i) * (m(X_i) / (p_i * Y_i))  [simplified]
    # In practice, we use IPW weights adjusted by the augmentation ratio
    w_ipw = _compute_ipw_weights(p_hat, d, trim_lower, trim_upper)

    # Augmentation factor for observed units
    observed_mask = d == 1
    w_aipw = w_ipw.copy()
    if observed_mask.any():
        y_safe = np.where(np.abs(y_obs) > epsilon, y_obs, epsilon)
        aug_ratio = 1.0 - (1.0 - p_clipped) * y_imputed / y_safe
        aug_ratio = np.clip(aug_ratio, 0.5, 2.0)  # Stabilize
        w_aipw[observed_mask] = w_ipw[observed_mask] * aug_ratio[observed_mask]

    # Trim final weights
    nonzero = w_aipw[w_aipw > 0]
    if len(nonzero) > 0:
        lo = np.quantile(nonzero, trim_lower)
        hi = np.quantile(nonzero, trim_upper)
        w_aipw[w_aipw > 0] = np.clip(w_aipw[w_aipw > 0], lo, hi)

    return w_aipw, mu_aipw


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def compute_final_weights(cfg: PipelineConfig) -> str:
    """
    Compute final analytical weights using the configured method.

    Combines firm-level weights (from declaration model) with individual-level
    weights, applying either IPW or AIPW methodology.

    Parameters
    ----------
    cfg : PipelineConfig
        Pipeline configuration.

    Returns
    -------
    str
        Object name of the updated analytical base with final weights.
    """
    analytical_object = f"{cfg.minio.cleaned_prefix}analytical_base.parquet"

    if not object_exists(cfg.minio, analytical_object):
        raise FileNotFoundError(f"Analytical base not found: {analytical_object}")

    df = read_parquet(cfg.minio, analytical_object)
    method = cfg.modeling.estimation_method

    logger.info("Computing final weights using method: {}", method)

    if method == "aipw" and "P_HAT_JT" in df.columns:
        # --- AIPW (Doubly Robust) ---
        salary_col = "SALAIRE_BRUT_MENS" if "SALAIRE_BRUT_MENS" in df.columns else "SALAIRE_BRUT"

        if salary_col in df.columns and "P_HAT_JT" in df.columns:
            y_obs = df[salary_col].fill_null(0).to_numpy()
            p_hat = df["P_HAT_JT"].fill_null(0.5).to_numpy()
            d = df["D_JT"].fill_null(1).to_numpy().astype(float) if "D_JT" in df.columns \
                else np.ones(len(df))

            # Use firm-level imputed mean as outcome model prediction
            y_imputed = df["SALAIRE_MOYEN"].fill_null(
                df[salary_col].mean()
            ).to_numpy() if "SALAIRE_MOYEN" in df.columns else y_obs.copy()

            w_aipw, mu_aipw = _compute_aipw_weights(
                y_obs, y_imputed, p_hat, d,
                cfg.modeling.ipw_trim_lower, cfg.modeling.ipw_trim_upper,
            )

            df = df.with_columns(
                pl.Series("W_FINAL", w_aipw),
            )
            logger.info("AIPW estimate of mean salary: {:.0f} FCFA", mu_aipw)
        else:
            logger.warning("Missing columns for AIPW, falling back to IPW")
            df = df.with_columns(
                (pl.col("W_JT") * pl.col("W_INDIV")).alias("W_FINAL")
            )
    else:
        # --- Standard IPW ---
        df = df.with_columns(
            (pl.col("W_JT") * pl.col("W_INDIV")).alias("W_FINAL")
        )

    # Normalise weights within strata (period)
    if "PERIOD" in df.columns:
        df = df.with_columns(
            (pl.col("W_FINAL") / pl.col("W_FINAL").mean().over("PERIOD"))
            .alias("W_FINAL")
        )

    write_parquet(cfg.minio, analytical_object, df)
    logger.info("Final weights computed: mean={:.3f}, std={:.3f}",
                df["W_FINAL"].mean(), df["W_FINAL"].std())

    return analytical_object
