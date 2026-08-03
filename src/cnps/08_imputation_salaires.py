"""
Etape 8/12 — Imputation multiple des salaires manquants.

Lorsqu'une entreprise ne declare pas sur une periode donnee (R_JT = 0),
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

from cnps.config import PipelineConfig, load_config
from cnps.storage import object_exists, read_parquet, write_parquet, write_pickle

_CATEGORICAL_FEATURES = [
    "SECTEUR_ACTIVITE",
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
        Lignes avec R_JT == 1 et salaire non nul.
    df_missing : pl.DataFrame
        Lignes avec R_JT == 0 (salaire a imputer).
    cat_feats, num_feats : list[str]
        Covariables disponibles.
    """
    cat_feats = [c for c in _CATEGORICAL_FEATURES if c in df.columns]
    num_feats = [c for c in _NUMERIC_FEATURES if c in df.columns]

    for col in num_feats:
        df = df.with_columns(pl.col(col).fill_null(0.0))
    for col in cat_feats:
        df = df.with_columns(pl.col(col).cast(pl.Utf8).fill_null("INCONNU"))

    # SALAIRE_MOYEN == 0 (entreprise declarante avec masse salariale nulle
    # sur la periode) donne LOG_SALAIRE_MOYEN = log(0) = -inf : ces lignes
    # sont exclues de l'ajustement, sinon LinearRegression echoue
    # ("Input y contains infinity").
    df_declaring = df.filter(
        (pl.col("R_JT") == 1)
        & pl.col("LOG_SALAIRE_MOYEN").is_not_null()
        & pl.col("LOG_SALAIRE_MOYEN").is_finite()
    )
    df_missing = df.filter(pl.col("R_JT") == 0)

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
    cleaned_bucket = cfg.minio.cleaned_bucket
    firm_object = f"{cfg.minio.cleaned_prefix}firm_base.parquet"
    if not object_exists(cfg.minio, cleaned_bucket, firm_object):
        raise FileNotFoundError(f"Base entreprises introuvable : {cleaned_bucket}/{firm_object}")

    df = read_parquet(cfg.minio, cleaned_bucket, firm_object)
    M = cfg.modeling.n_imputations
    logger.info("Nombre d'imputations configure : M={}", M)
    rng = np.random.default_rng(cfg.modeling.random_seed)

    df_declaring, df_missing, cat_feats, num_feats = _prepare_imputation_data(df)

    if df_missing.height == 0:
        logger.info("Aucune entreprise non-declarante a imputer.")
        df = df.with_columns(pl.lit(0).cast(pl.Int32).alias("IMPUTATION_ID"))
        out_object = f"{cfg.minio.cleaned_prefix}firm_base_imputed.parquet"
        write_parquet(cfg.minio, cleaned_bucket, out_object, df)
        return out_object

    # --- Ajustement OLS sur log(salaire) ---
    features = cat_feats + num_feats
    logger.info("Covariables du modele d'imputation : {}", features)
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

    # Categories absentes de l'echantillon d'apprentissage : handle_unknown
    # ="ignore" les encode en zeros, sans erreur. Leur imputation repose alors
    # sur le seul intercept du modele, sans aucun effet propre a leur secteur
    # ou a leur taille. Le fait est acceptable mais doit etre trace : ces
    # entreprises recoivent l'imputation la moins informee du lot.
    for col in cat_feats:
        connues = set(df_declaring[col].drop_nulls().unique().to_list())
        inconnues = df_missing.filter(
            pl.col(col).is_not_null() & ~pl.col(col).is_in(list(connues))
        )
        if inconnues.height:
            logger.warning(
                "Imputation : {} couples entreprise-mois ({:.2f}%) ont une modalite "
                "de '{}' absente des entreprises declarantes -- leur imputation "
                "repose sur le seul intercept du modele",
                inconnues.height, inconnues.height / df_missing.height * 100, col,
            )

    y_hat = model.predict(X_missing)

    # Bornes de plausibilite des salaires imputes.
    # L'imputation porte sur log(salaire) : le bruit gaussien y est symetrique,
    # mais l'exponentielle le rend fortement asymetrique en francs. Avec
    # sigma ~ 0,57, un tirage a +3 ecarts-types multiplie la prediction par
    # ~5,6. Sans borne, l'etape produit des salaires moyens d'entreprise sans
    # rapport avec les donnees observees (jusqu'a plusieurs centaines de
    # millions), qui contaminent ensuite les regles de Rubin.
    # On borne sur la plage effectivement observee chez les declarantes, seule
    # reference empirique disponible a ce stade.
    obs = df_declaring["SALAIRE_MOYEN"].drop_nulls()
    borne_basse = float(obs.min()) if obs.len() else None
    borne_haute = float(obs.max()) if obs.len() else None

    imputed_frames: list[pl.DataFrame] = []
    n_bornees = 0
    for m in range(M):
        noise = rng.normal(0, sigma_hat, size=len(y_hat))
        imputed_log_salary = y_hat + noise
        imputed_salary = np.exp(imputed_log_salary)

        if borne_basse is not None and borne_haute is not None:
            n_bornees += int(
                ((imputed_salary < borne_basse) | (imputed_salary > borne_haute)).sum()
            )
            imputed_salary = np.clip(imputed_salary, borne_basse, borne_haute)
            imputed_log_salary = np.log(imputed_salary)

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

    if n_bornees:
        total_imputes = len(y_hat) * M
        logger.info(
            "Imputations ramenees dans la plage observee [{:.0f}, {:.0f}] : {} sur {} "
            "({:.2f}%) -- l'exponentielle du bruit gaussien produit sinon des valeurs "
            "sans rapport avec les salaires constates",
            borne_basse, borne_haute, n_bornees, total_imputes,
            n_bornees / total_imputes * 100,
        )

    imputed_all = np.concatenate([f["SALAIRE_MOYEN"].to_numpy() for f in imputed_frames])
    logger.info(
        "Salaires imputes (toutes imputations confondues) : moyenne={:.0f}, mediane={:.0f}, "
        "plage=[{:.0f}, {:.0f}] (observe : moyenne={:.0f}, mediane={:.0f})",
        imputed_all.mean(), float(np.median(imputed_all)), imputed_all.min(), imputed_all.max(),
        df_declaring["SALAIRE_MOYEN"].mean(), df_declaring["SALAIRE_MOYEN"].median(),
    )

    out_object = f"{cfg.minio.cleaned_prefix}firm_base_imputed.parquet"
    write_parquet(cfg.minio, cleaned_bucket, out_object, result)
    logger.info("Base entreprises imputee : {} lignes, {} colonnes (M={}) -> {}",
                result.height, result.width, M, out_object)

    model_object = f"{cfg.minio.models_prefix}imputation_model.pkl"
    write_pickle(cfg.minio, cfg.minio.models_bucket, model_object, {
        "model": model, "sigma": sigma_hat, "r_squared": r_squared,
        "features": features,
    })
    logger.info("Modele d'imputation sauvegarde : {}", model_object)

    return out_object


if __name__ == "__main__":
    import argparse
    import sys
    from pathlib import Path

    parser = argparse.ArgumentParser(
        description=__doc__.strip().splitlines()[0] if __doc__ else None
    )
    parser.add_argument("--settings", "-s", type=Path, default=None)
    parser.add_argument("--dimensions", "-d", type=Path, default=None)
    parser.add_argument("--verbose", "-v", action="store_true")
    args = parser.parse_args()

    cfg = load_config(args.settings, args.dimensions)

    logger.remove()
    logger.add(
        sys.stderr,
        level="DEBUG" if args.verbose else "INFO",
        colorize=True,
        format="<green>{time:HH:mm:ss}</green> | <level>{level:<8}</level> | {message}",
    )
    logger.add(
        str(cfg.paths.logs / f"{Path(__file__).stem}.log"),
        level="DEBUG", rotation="10 MB", retention="30 days", encoding="utf-8",
    )

    try:
        imputer_salaires(cfg)
        logger.info("Termine avec succes.")
    except Exception as exc:
        logger.exception("Echec de l'etape: {}", exc)
        sys.exit(1)
