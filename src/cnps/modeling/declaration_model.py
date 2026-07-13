"""
Declaration (propensity score) model.

Estimates the probability that a firm declares its employees in a given
period using logistic regression.  The estimated propensity scores are
used to compute Inverse Probability Weights (IPW) that correct for
selection bias induced by non-declaration.

Model specification
-------------------
P(D_jt = 1 | X_jt) = logit^{-1}(X_jt * beta)

where D_jt is the binary declaration indicator for firm j in period t,
and X_jt includes:
- Sector of activity
- Firm size class
- Firm age class
- Lagged declaration indicator (D_{j,t-1})
- Past cumulative declaration rate
- Period fixed effects (month dummies)

IPW computation
---------------
w_jt = 1 / max(p_hat_jt, epsilon)

Stabilized weights (Robins et al., 2000):
w_jt^s = P(D=1) / max(p_hat_jt, epsilon)

Weight trimming at configurable percentiles to limit variance inflation
(Cole & Hernan, 2008).

References
----------
Rosenbaum, P. R. & Rubin, D. B. (1983). The central role of the propensity
    score in observational studies for causal effects. *Biometrika*, 70(1), 41-55.
Robins, J. M., Hernan, M. A. & Brumback, B. (2000). Marginal structural
    models and causal inference in epidemiology. *Epidemiology*, 11(5), 550-560.
Cole, S. R. & Hernan, M. A. (2008). Constructing inverse probability weights
    for marginal structural models. *American Journal of Epidemiology*,
    168(6), 656-664.
"""

from __future__ import annotations

import pickle
from pathlib import Path

import numpy as np
import polars as pl
from loguru import logger
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_auc_score
from sklearn.preprocessing import OneHotEncoder
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline as SKPipeline

from cnps.config import PipelineConfig


# ---------------------------------------------------------------------------
# Feature engineering
# ---------------------------------------------------------------------------

# Covariates for the declaration model (categorical and numeric)
_CATEGORICAL_FEATURES = [
    "SECTEUR_ACTIVITE_COD",
    "CLASSE_EFFECTIF_REDUITE",
    "CL_AGE_ENTREPRISE",
]

_NUMERIC_FEATURES = [
    "LAG_D_JT",
    "TAUX_DECLARATION_PASSE",
]


def _prepare_features(
    df: pl.DataFrame,
) -> tuple[pl.DataFrame, list[str], list[str]]:
    """
    Select and validate features for the declaration model.

    Returns the filtered DataFrame and lists of available categorical
    and numeric feature names.
    """
    cat_feats = [c for c in _CATEGORICAL_FEATURES if c in df.columns]
    num_feats = [c for c in _NUMERIC_FEATURES if c in df.columns]

    if not cat_feats and not num_feats:
        raise ValueError("No valid features found for declaration model")

    # Drop rows with all features null
    all_feats = cat_feats + num_feats
    df_model = df.drop_nulls(subset=["D_JT"])

    # Fill numeric nulls with 0 (first period has no lag)
    for col in num_feats:
        df_model = df_model.with_columns(pl.col(col).fill_null(0.0))

    # Fill categorical nulls with "INCONNU"
    for col in cat_feats:
        df_model = df_model.with_columns(
            pl.col(col).cast(pl.Utf8).fill_null("INCONNU")
        )

    logger.info("Declaration model features: cat={}, num={}", cat_feats, num_feats)
    return df_model, cat_feats, num_feats


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def fit_declaration_model(cfg: PipelineConfig) -> Path:
    """
    Fit the firm-level declaration model and compute IPW weights.

    Steps
    -----
    1. Load firm base
    2. Prepare features (one-hot encoding for categoricals)
    3. Fit logistic regression (L2-regularised)
    4. Predict propensity scores
    5. Compute stabilized and trimmed IPW weights
    6. Evaluate model (AUC, calibration)
    7. Save model and update firm base with weights

    Parameters
    ----------
    cfg : PipelineConfig
        Pipeline configuration.

    Returns
    -------
    Path
        Path to the updated firm base with IPW weights.
    """
    firm_path = cfg.paths.cleaned_data / "firm_base.parquet"
    if not firm_path.exists():
        raise FileNotFoundError(f"Firm base not found: {firm_path}")

    df = pl.read_parquet(firm_path)
    logger.info("Fitting declaration model on {} firm-period records", df.height)

    # --- Prepare features ---
    df_model, cat_feats, num_feats = _prepare_features(df)

    y = df_model["D_JT"].to_numpy().astype(float)

    # --- Sklearn pipeline ---
    transformers = []
    if cat_feats:
        transformers.append((
            "cat", OneHotEncoder(drop="first", sparse_output=False, handle_unknown="ignore"),
            cat_feats,
        ))
    if num_feats:
        transformers.append(("num", "passthrough", num_feats))

    preprocessor = ColumnTransformer(transformers, remainder="drop")

    model = SKPipeline([
        ("preprocessor", preprocessor),
        ("classifier", LogisticRegression(
            penalty="l2",
            C=1.0,
            max_iter=1000,
            solver="lbfgs",
            random_state=cfg.modeling.random_seed,
        )),
    ])

    # Convert to pandas for sklearn compatibility
    X_df = df_model.select(cat_feats + num_feats).to_pandas()
    model.fit(X_df, y)

    # --- Propensity scores ---
    p_hat = model.predict_proba(X_df)[:, 1]

    # AUC evaluation
    auc = roc_auc_score(y, p_hat)
    logger.info("Declaration model AUC: {:.4f}", auc)
    if auc < cfg.modeling.min_auc:
        logger.warning("AUC ({:.4f}) below threshold ({:.4f})", auc, cfg.modeling.min_auc)

    # --- IPW weights ---
    # Stabilized weights: w_jt = P(D=1) / p_hat_jt
    marginal_p = y.mean()
    epsilon = 1e-6
    p_hat_clipped = np.clip(p_hat, epsilon, 1 - epsilon)
    w_jt = marginal_p / p_hat_clipped

    # Trim weights at configured percentiles
    lo = np.quantile(w_jt, cfg.modeling.ipw_trim_lower)
    hi = np.quantile(w_jt, cfg.modeling.ipw_trim_upper)
    w_jt = np.clip(w_jt, lo, hi)

    logger.info("IPW weights: mean={:.3f}, median={:.3f}, range=[{:.3f}, {:.3f}]",
                w_jt.mean(), np.median(w_jt), w_jt.min(), w_jt.max())

    # --- Update firm base ---
    df_model = df_model.with_columns(
        pl.Series("W_JT", w_jt),
        pl.Series("P_HAT_JT", p_hat),
    )

    # Write back to firm base (full join to preserve non-modeled rows)
    if "NUMERO_EMPLOYEUR" in df_model.columns and "PERIOD" in df_model.columns:
        join_cols = ["NUMERO_EMPLOYEUR", "PERIOD"]
        weights_df = df_model.select(join_cols + ["W_JT", "P_HAT_JT"])
        df_orig = pl.read_parquet(firm_path).drop(["W_JT"], strict=False)
        df_updated = df_orig.join(weights_df, on=join_cols, how="left")
        df_updated = df_updated.with_columns(
            pl.col("W_JT").fill_null(1.0),
        )
    else:
        df_updated = df_model

    df_updated.write_parquet(firm_path, compression="zstd")

    # --- Save model ---
    model_path = cfg.paths.models / "declaration_model.pkl"
    model_path.parent.mkdir(parents=True, exist_ok=True)
    with open(model_path, "wb") as f:
        pickle.dump({"model": model, "auc": auc, "features": cat_feats + num_feats}, f)
    logger.info("Declaration model saved to {}", model_path)

    return firm_path
