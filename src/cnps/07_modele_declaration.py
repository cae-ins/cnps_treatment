"""
Etape 7/12 — Modele de declaration (score de propension).

Estime la probabilite qu'une entreprise declare ses employes sur une
periode donnee via une regression logistique. Les scores de propension
estimes servent a calculer les poids de ponderation par probabilite
inverse (IPW) qui corrigent le biais de selection induit par la
non-declaration.

Specification du modele
------------------------
P(D_jt = 1 | X_jt) = logit^{-1}(X_jt * beta)

ou D_jt est l'indicateur binaire de declaration de l'entreprise j sur la
periode t, et X_jt comprend :
- Secteur d'activite
- Classe de taille d'entreprise
- Classe d'age d'entreprise
- Indicateur de declaration retarde (D_{j,t-1})
- Taux de declaration cumule passe

Calcul des poids IPW
---------------------
w_jt = 1 / max(p_hat_jt, epsilon)

Poids stabilises (Robins et al., 2000) :
w_jt^s = P(D=1) / max(p_hat_jt, epsilon)

Troncature des poids a des percentiles configurables pour limiter
l'inflation de variance (Cole & Hernan, 2008).

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

import numpy as np
import polars as pl
from loguru import logger
from sklearn.compose import ColumnTransformer
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_auc_score
from sklearn.pipeline import Pipeline as SKPipeline
from sklearn.preprocessing import OneHotEncoder

from cnps.config import PipelineConfig, load_config
from cnps.storage import object_exists, read_parquet, write_parquet, write_pickle

# Covariables du modele de declaration (categorielles et numeriques)
_CATEGORICAL_FEATURES = [
    "SECTEUR_ACTIVITE",
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
    """Selectionne et prepare les covariables du modele de declaration."""
    cat_feats = [c for c in _CATEGORICAL_FEATURES if c in df.columns]
    num_feats = [c for c in _NUMERIC_FEATURES if c in df.columns]

    if not cat_feats and not num_feats:
        raise ValueError("Aucune covariable valide trouvee pour le modele de declaration")

    df_model = df.drop_nulls(subset=["D_JT"])

    # Nulls numeriques -> 0 (la premiere periode n'a pas de valeur retardee)
    for col in num_feats:
        df_model = df_model.with_columns(pl.col(col).fill_null(0.0))

    # Nulls categoriels -> "INCONNU"
    for col in cat_feats:
        df_model = df_model.with_columns(
            pl.col(col).cast(pl.Utf8).fill_null("INCONNU")
        )

    logger.info("Covariables du modele de declaration : cat={}, num={}", cat_feats, num_feats)
    return df_model, cat_feats, num_feats


def ajuster_modele_declaration(cfg: PipelineConfig) -> str:
    """
    Ajuste le modele de declaration au niveau entreprise et calcule les poids IPW.

    Etapes
    ------
    1. Lecture de la base entreprises
    2. Preparation des covariables (encodage one-hot des categorielles)
    3. Ajustement d'une regression logistique (regularisation L2)
    4. Prediction des scores de propension
    5. Calcul des poids IPW stabilises et tronques
    6. Evaluation du modele (AUC)
    7. Sauvegarde du modele et mise a jour de la base entreprises avec les poids

    Parameters
    ----------
    cfg : PipelineConfig
        Configuration du pipeline.

    Returns
    -------
    str
        Nom de l'objet de la base entreprises mise a jour avec les poids IPW.
    """
    cleaned_bucket = cfg.minio.cleaned_bucket
    firm_object = f"{cfg.minio.cleaned_prefix}firm_base.parquet"
    if not object_exists(cfg.minio, cleaned_bucket, firm_object):
        raise FileNotFoundError(f"Base entreprises introuvable : {cleaned_bucket}/{firm_object}")

    df = read_parquet(cfg.minio, cleaned_bucket, firm_object)
    logger.info("Ajustement du modele de declaration sur {} enregistrements entreprise-periode",
                df.height)

    # --- Preparation des covariables ---
    df_model, cat_feats, num_feats = _prepare_features(df)

    y = df_model["D_JT"].to_numpy().astype(float)

    # --- Pipeline sklearn ---
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

    X_df = df_model.select(cat_feats + num_feats).to_pandas()
    model.fit(X_df, y)

    # --- Scores de propension ---
    p_hat = model.predict_proba(X_df)[:, 1]

    auc = roc_auc_score(y, p_hat)
    logger.info("AUC du modele de declaration : {:.4f}", auc)
    if auc < cfg.modeling.min_auc:
        logger.warning("AUC ({:.4f}) sous le seuil ({:.4f})", auc, cfg.modeling.min_auc)

    # --- Poids IPW ---
    # Poids stabilises : w_jt = P(D=1) / p_hat_jt
    marginal_p = y.mean()
    epsilon = 1e-6
    p_hat_clipped = np.clip(p_hat, epsilon, 1 - epsilon)
    w_jt = marginal_p / p_hat_clipped

    lo = np.quantile(w_jt, cfg.modeling.ipw_trim_lower)
    hi = np.quantile(w_jt, cfg.modeling.ipw_trim_upper)
    w_jt = np.clip(w_jt, lo, hi)

    logger.info("Poids IPW : moyenne={:.3f}, mediane={:.3f}, plage=[{:.3f}, {:.3f}]",
                w_jt.mean(), np.median(w_jt), w_jt.min(), w_jt.max())

    # --- Mise a jour de la base entreprises ---
    df_model = df_model.with_columns(
        pl.Series("W_JT", w_jt),
        pl.Series("P_HAT_JT", p_hat),
    )

    # Reintegration dans la base complete (jointure pour preserver les lignes non modelisees)
    if "ID_EMPLOYEUR" in df_model.columns and "PERIOD" in df_model.columns:
        join_cols = ["ID_EMPLOYEUR", "PERIOD"]
        weights_df = df_model.select(join_cols + ["W_JT", "P_HAT_JT"])
        df_orig = read_parquet(cfg.minio, cleaned_bucket, firm_object).drop(["W_JT"], strict=False)
        df_updated = df_orig.join(weights_df, on=join_cols, how="left")
        df_updated = df_updated.with_columns(
            pl.col("W_JT").fill_null(1.0),
        )
    else:
        df_updated = df_model

    write_parquet(cfg.minio, cleaned_bucket, firm_object, df_updated)

    # --- Sauvegarde du modele ---
    model_object = f"{cfg.minio.models_prefix}declaration_model.pkl"
    write_pickle(cfg.minio, cfg.minio.models_bucket, model_object, {
        "model": model, "auc": auc, "features": cat_feats + num_feats,
    })
    logger.info("Modele de declaration sauvegarde : {}", model_object)

    return firm_object


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
        ajuster_modele_declaration(cfg)
        logger.info("Termine avec succes.")
    except Exception as exc:
        logger.exception("Echec de l'etape: {}", exc)
        sys.exit(1)
