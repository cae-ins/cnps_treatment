"""
Multiple imputation module for missing firm salaries.

When a firm does not declare in a given period (D_jt = 0), its salary
distribution is unobserved.  We impute the missing mean salary using a
log-linear model estimated on declaring firms, then generate M imputed
datasets with residual bootstrap to preserve imputation uncertainty.

Method
------
1. Estimate: log(Y_jt) = X_jt * beta + epsilon_jt  on declaring firms
2. For each non-declaring firm-period, predict:
   Y_hat_jt^(m) = exp(X_jt * beta_hat + e^(m))
   where e^(m) ~ N(0, sigma_hat^2)  (residual bootstrap)
3. Repeat M times to produce M imputed datasets

This follows Rubin's (1987) framework for multiple imputation by
chained equations (MICE), adapted for the specific case of firm-level
non-response in administrative salary data.

References
----------
Rubin, D. B. (1987). *Multiple Imputation for Nonresponse in Surveys*.
    John Wiley & Sons.
Little, R. J. A. & Rubin, D. B. (2002). *Statistical Analysis with Missing
    Data* (2nd ed.). Wiley-Interscience.
Van Buuren, S. & Groothuis-Oudshoorn, K. (2011). mice: Multivariate
    Imputation by Chained Equations in R. *Journal of Statistical Software*,
    45(3), 1-67.
White, I. R., Royston, P. & Wood, A. M. (2011). Multiple imputation using
    chained equations: issues and guidance for practice. *Statistics in
    Medicine*, 30(4), 377-399.
"""

from __future__ import annotations

import numpy as np
import polars as pl
from loguru import logger
from sklearn.compose import ColumnTransformer
from sklearn.linear_model import LinearRegression
from sklearn.pipeline import Pipeline as SKPipeline
from sklearn.preprocessing import OneHotEncoder

from cnps.config import PipelineConfig
from cnps.storage import read_parquet, write_parquet, write_pickle
from cnps.storage.minio_client import object_exists


# ---------------------------------------------------------------------------
# Feature specification
# ---------------------------------------------------------------------------

_CATEGORICAL_FEATURES = [
    "SECTEUR_ACTIVITE_COD",
    "CLASSE_EFFECTIF_REDUITE",
    "CL_AGE_ENTREPRISE",
]

_NUMERIC_FEATURES = [
    "LAG_SALAIRE_MOYEN",
    "LAG_EFFECTIF_OBSERVE",
]


def _prepare_imputation_data(
    df: pl.DataFrame,
) -> tuple[pl.DataFrame, pl.DataFrame, list[str], list[str]]:
    """
    Split into declaring (for model fitting) and non-declaring (for imputation).

    Returns
    -------
    df_declaring : pl.DataFrame
        Rows with D_JT == 1 and non-null salary.
    df_missing : pl.DataFrame
        Rows with D_JT == 0 (salary to impute).
    cat_feats : list[str]
        Available categorical feature names.
    num_feats : list[str]
        Available numeric feature names.
    """
    cat_feats = [c for c in _CATEGORICAL_FEATURES if c in df.columns]
    num_feats = [c for c in _NUMERIC_FEATURES if c in df.columns]

    # Fill nulls
    for col in num_feats:
        df = df.with_columns(pl.col(col).fill_null(0.0))
    for col in cat_feats:
        df = df.with_columns(pl.col(col).cast(pl.Utf8).fill_null("INCONNU"))

    df_declaring = df.filter(
        (pl.col("D_JT") == 1) & pl.col("LOG_SALAIRE_MOYEN").is_not_null()
    )
    df_missing = df.filter(pl.col("D_JT") == 0)

    logger.info("Imputation: {} declaring, {} non-declaring firm-periods",
                df_declaring.height, df_missing.height)

    return df_declaring, df_missing, cat_feats, num_feats


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def impute_firm_salaries(cfg: PipelineConfig) -> str:
    """
    Generate M multiple imputations for non-declaring firm salaries.

    Parameters
    ----------
    cfg : PipelineConfig
        Pipeline configuration.

    Returns
    -------
    str
        Object name of the imputed firm base (long format with ``imputation_id``).
    """
    firm_object = f"{cfg.minio.cleaned_prefix}firm_base.parquet"
    if not object_exists(cfg.minio, firm_object):
        raise FileNotFoundError(f"Firm base not found: {firm_object}")

    df = read_parquet(cfg.minio, firm_object)
    M = cfg.modeling.n_imputations
    rng = np.random.default_rng(cfg.modeling.random_seed)

    df_declaring, df_missing, cat_feats, num_feats = _prepare_imputation_data(df)

    if df_missing.height == 0:
        logger.info("No non-declaring firms to impute.")
        # Write with imputation_id = 0 for compatibility
        df = df.with_columns(pl.lit(0).cast(pl.Int32).alias("IMPUTATION_ID"))
        out_object = f"{cfg.minio.cleaned_prefix}firm_base_imputed.parquet"
        write_parquet(cfg.minio, out_object, df)
        return out_object

    # --- Fit OLS on log(salary) ---
    features = cat_feats + num_feats
    y_train = df_declaring["LOG_SALAIRE_MOYEN"].to_numpy()

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
        ("regressor", LinearRegression()),
    ])

    X_train = df_declaring.select(features).to_pandas()
    model.fit(X_train, y_train)

    # Residual standard deviation
    y_pred_train = model.predict(X_train)
    residuals = y_train - y_pred_train
    sigma_hat = float(np.std(residuals, ddof=len(features)))
    r_squared = 1 - np.var(residuals) / np.var(y_train)
    logger.info("Imputation model: R2={:.4f}, sigma={:.4f}", r_squared, sigma_hat)

    # --- Generate M imputations ---
    X_missing = df_missing.select(features).to_pandas()
    y_hat = model.predict(X_missing)

    imputed_frames: list[pl.DataFrame] = []

    # Imputation 0: original declaring data
    df_declaring_tagged = df_declaring.with_columns(
        pl.lit(0).cast(pl.Int32).alias("IMPUTATION_ID")
    )

    for m in range(M):
        noise = rng.normal(0, sigma_hat, size=len(y_hat))
        imputed_log_salary = y_hat + noise
        imputed_salary = np.exp(imputed_log_salary)

        df_imp = df_missing.with_columns(
            pl.Series("SALAIRE_MOYEN", imputed_salary),
            pl.Series("LOG_SALAIRE_MOYEN", imputed_log_salary),
            pl.lit(m + 1).cast(pl.Int32).alias("IMPUTATION_ID"),
        )
        imputed_frames.append(df_imp)

    # Combine: declaring (tagged 0) + each imputation (tagged 1..M)
    # For each imputation m, the full dataset is: declaring + imputed(m)
    all_frames = []
    for m in range(M):
        full_m = pl.concat([
            df_declaring.with_columns(pl.lit(m + 1).cast(pl.Int32).alias("IMPUTATION_ID")),
            imputed_frames[m],
        ], how="diagonal")
        all_frames.append(full_m)

    result = pl.concat(all_frames, how="diagonal")

    out_object = f"{cfg.minio.cleaned_prefix}firm_base_imputed.parquet"
    write_parquet(cfg.minio, out_object, result)
    logger.info("Imputed firm base: {} rows (M={}) -> {}", result.height, M, out_object)

    # Save model
    model_object = f"{cfg.minio.models_prefix}imputation_model.pkl"
    write_pickle(cfg.minio, model_object, {
        "model": model, "sigma": sigma_hat, "r_squared": r_squared,
        "features": features,
    })

    return out_object
