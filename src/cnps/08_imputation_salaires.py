"""
Etape 8/12 — Imputation multiple des salaires manquants.

Lorsqu'une entreprise ne declare pas sur une periode donnee (D_jt = 0),
sa distribution de salaires est inobservee. On impute le salaire moyen
manquant via un modele log-lineaire estime sur les entreprises
declarantes, puis on genere M jeux de donnees imputes par bootstrap
des residus pour preserver l'incertitude d'imputation.

Methode
-------
1. Estimation : log(Y_jt) = X_jt * beta + epsilon_jt  sur les entreprises declarantes
2. Pour chaque entreprise-periode non-declarante, prediction :
   Y_hat_jt^(m) = exp(X_jt * beta_hat + e^(m))
   avec e^(m) ~ N(0, sigma_hat^2)  (bootstrap des residus)
3. Repetition M fois pour produire M jeux de donnees imputes

Cette approche suit le cadre de Rubin (1987) pour l'imputation multiple
par equations chainees (MICE), adapte au cas specifique de la
non-reponse au niveau entreprise dans les donnees administratives de
salaires.

References
----------
Rubin, D. B. (1987). *Multiple Imputation for Nonresponse in Surveys*.
    John Wiley & Sons.
Little, R. J. A. & Rubin, D. B. (2002). *Statistical Analysis with Missing
    Data* (2nd ed.). Wiley-Interscience.
Van Buuren, S. & Groothuis-Oudshoorn, K. (2011). mice: Multivariate
    Imputation by Chained Equations in R. *Journal of Statistical Software*,
    45(3), 1-67.
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
from cnps.storage import object_exists, read_parquet, write_parquet, write_pickle

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
    Separe les entreprises declarantes (ajustement du modele) des
    non-declarantes (imputation).

    Returns
    -------
    df_declaring : pl.DataFrame
        Lignes avec D_JT == 1 et salaire non nul.
    df_missing : pl.DataFrame
        Lignes avec D_JT == 0 (salaire a imputer).
    cat_feats, num_feats : list[str]
        Covariables disponibles.
    """
    cat_feats = [c for c in _CATEGORICAL_FEATURES if c in df.columns]
    num_feats = [c for c in _NUMERIC_FEATURES if c in df.columns]

    for col in num_feats:
        df = df.with_columns(pl.col(col).fill_null(0.0))
    for col in cat_feats:
        df = df.with_columns(pl.col(col).cast(pl.Utf8).fill_null("INCONNU"))

    df_declaring = df.filter(
        (pl.col("D_JT") == 1) & pl.col("LOG_SALAIRE_MOYEN").is_not_null()
    )
    df_missing = df.filter(pl.col("D_JT") == 0)

    logger.info("Imputation : {} entreprises declarantes, {} non-declarantes",
                df_declaring.height, df_missing.height)

    return df_declaring, df_missing, cat_feats, num_feats


def imputer_salaires(cfg: PipelineConfig) -> str:
    """
    Genere M imputations multiples des salaires des entreprises non-declarantes.

    Parameters
    ----------
    cfg : PipelineConfig
        Configuration du pipeline.

    Returns
    -------
    str
        Nom de l'objet de la base entreprises imputee (format long,
        colonne ``IMPUTATION_ID``).
    """
    firm_object = f"{cfg.minio.cleaned_prefix}firm_base.parquet"
    if not object_exists(cfg.minio, firm_object):
        raise FileNotFoundError(f"Base entreprises introuvable : {firm_object}")

    df = read_parquet(cfg.minio, firm_object)
    M = cfg.modeling.n_imputations
    rng = np.random.default_rng(cfg.modeling.random_seed)

    df_declaring, df_missing, cat_feats, num_feats = _prepare_imputation_data(df)

    if df_missing.height == 0:
        logger.info("Aucune entreprise non-declarante a imputer.")
        df = df.with_columns(pl.lit(0).cast(pl.Int32).alias("IMPUTATION_ID"))
        out_object = f"{cfg.minio.cleaned_prefix}firm_base_imputed.parquet"
        write_parquet(cfg.minio, out_object, df)
        return out_object

    # --- Ajustement OLS sur log(salaire) ---
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

    # Ecart-type des residus
    y_pred_train = model.predict(X_train)
    residuals = y_train - y_pred_train
    sigma_hat = float(np.std(residuals, ddof=len(features)))
    r_squared = 1 - np.var(residuals) / np.var(y_train)
    logger.info("Modele d'imputation : R2={:.4f}, sigma={:.4f}", r_squared, sigma_hat)

    # --- Generation des M imputations ---
    X_missing = df_missing.select(features).to_pandas()
    y_hat = model.predict(X_missing)

    imputed_frames: list[pl.DataFrame] = []
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

    # Combine : pour chaque imputation m, jeu complet = declarantes + imputees(m)
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
    logger.info("Base entreprises imputee : {} lignes (M={}) -> {}", result.height, M, out_object)

    model_object = f"{cfg.minio.models_prefix}imputation_model.pkl"
    write_pickle(cfg.minio, model_object, {
        "model": model, "sigma": sigma_hat, "r_squared": r_squared,
        "features": features,
    })

    return out_object
