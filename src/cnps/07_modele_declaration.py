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

Calcul du facteur IPW
---------------------
w_jt = 1 / max(p_hat_jt, epsilon)

Ce facteur n'est pas stabilise par une probabilite marginale. Le facteur
entreprise n'est pas diffuse seul : l'etape 09 construit le poids final
R_ijt/(p_hat_jt*q_hat_ijt), puis applique la troncature configuree.

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
from sklearn.impute import SimpleImputer
from sklearn.linear_model import LogisticRegression
from sklearn.pipeline import Pipeline as SKPipeline
from sklearn.preprocessing import OneHotEncoder

from cnps.config import PipelineConfig, load_config
from cnps.response_diagnostics import (
    evaluate_oof_predictions,
    grouped_oof_predictions,
    inverse_propensity_weights,
    reject_never_responding_strata,
)
from cnps.storage import object_exists, read_parquet, write_json, write_parquet

# Covariables du modele de declaration (categorielles et numeriques)
_CATEGORICAL_FEATURES = [
    "SECTEUR_ACTIVITE",
    "CLASSE_EFFECTIF_REDUITE",
    "CL_AGE_ENTREPRISE",
]

_NUMERIC_FEATURES = [
    "LAG_D_JT",
    "TAUX_DECLARATION_PASSE",
    "PREMIER_MOIS_RISQUE",
    "FENETRE_RISQUE_EXTENSIBLE",
    "JAMAIS_OBSERVE_AVANT_SECTEUR_ACTIVITE",
    "JAMAIS_OBSERVE_AVANT_CLASSE_EFFECTIF_REDUITE",
]


def _prepare_features(
    df: pl.DataFrame,
) -> tuple[pl.DataFrame, list[str], list[str]]:
    """Selectionne les lignes dans la portee K et prepare leurs covariables."""
    if "DANS_UNIVERS_RISQUE" not in df.columns:
        raise ValueError("DANS_UNIVERS_RISQUE absent: rejouer l'etape 05 apres le lot C.1.")
    df_model = df.filter(pl.col("DANS_UNIVERS_RISQUE") == 1)
    if df_model["D_JT"].null_count():
        raise ValueError("D_JT est nul sur des lignes appartenant a la portee K.")

    cat_feats = [c for c in _CATEGORICAL_FEATURES if c in df_model.columns]
    num_feats = [c for c in _NUMERIC_FEATURES if c in df_model.columns]
    if not cat_feats and not num_feats:
        raise ValueError("Aucune covariable valide trouvee pour le modele de declaration")

    history_features = [
        name for name in ("LAG_D_JT", "TAUX_DECLARATION_PASSE") if name in df_model.columns
    ]
    if history_features:
        df_model = df_model.with_columns(
            pl.any_horizontal([pl.col(name).is_null() for name in history_features])
            .cast(pl.Float64)
            .alias("SANS_HISTORIQUE")
        )
        num_feats = [*num_feats, "SANS_HISTORIQUE"]

    df_model = df_model.with_columns(
        [pl.col(name).cast(pl.Float64).fill_null(0.0) for name in num_feats]
        + [pl.col(name).cast(pl.Utf8) for name in cat_feats]
    )
    logger.info(
        "Portee K modelisee : {} lignes sur {}. Covariables cat={}, num={}.",
        df_model.height,
        df.height,
        cat_feats,
        num_feats,
    )
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
    5. Calcul du facteur IPW non stabilise
    6. Diagnostics hors echantillon groupes par employeur
    7. Sauvegarde d'un resume JSON et mise a jour de la base entreprises

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
    logger.info(
        "Ajustement du modele de declaration sur {} enregistrements entreprise-periode", df.height
    )

    # --- Preparation des covariables ---
    df_model, cat_feats, num_feats = _prepare_features(df)

    y = df_model["D_JT"].to_numpy().astype(float)

    reject_never_responding_strata(
        df_model,
        target="D_JT",
        categorical_features=cat_feats,
        min_size=cfg.modeling.min_structural_stratum_size,
        label="modele entreprise",
    )

    transformers = []
    if cat_feats:
        categorical_pipeline = SKPipeline(
            [
                ("imputer", SimpleImputer(strategy="most_frequent")),
                (
                    "encoder",
                    OneHotEncoder(
                        drop="first",
                        sparse_output=False,
                        handle_unknown="ignore",
                    ),
                ),
            ]
        )
        transformers.append(("cat", categorical_pipeline, cat_feats))
    if num_feats:
        transformers.append(("num", "passthrough", num_feats))

    model = SKPipeline(
        [
            ("preprocessor", ColumnTransformer(transformers, remainder="drop")),
            (
                "classifier",
                LogisticRegression(
                    penalty="l2",
                    C=1.0,
                    max_iter=1000,
                    solver="lbfgs",
                    random_state=cfg.modeling.random_seed,
                ),
            ),
        ]
    )

    X_df = df_model.select(cat_feats + num_feats).to_pandas().replace({None: np.nan})
    if "ID_EMPLOYEUR" not in df_model.columns:
        raise ValueError("ID_EMPLOYEUR absent: validation croisee groupee impossible.")
    groups = df_model["ID_EMPLOYEUR"].to_numpy()
    p_oof, n_splits = grouped_oof_predictions(
        model,
        X_df,
        y,
        groups,
        n_splits=cfg.modeling.n_cv_splits,
        random_seed=cfg.modeling.random_seed,
    )
    diagnostics = evaluate_oof_predictions(
        X_df,
        y,
        p_oof,
        clip=cfg.modeling.propensity_clip,
        calibration_slope_range=cfg.modeling.calibration_slope_range,
        max_calibration_in_large=cfg.modeling.max_calibration_in_large,
        max_abs_smd=cfg.modeling.max_abs_smd,
        n_splits=n_splits,
        label="Modele entreprise",
    )
    if diagnostics.auc < cfg.modeling.min_auc:
        logger.warning(
            "AUC OOF {:.4f} sous le repere descriptif {:.4f}; aucun blocage AUC.",
            diagnostics.auc,
            cfg.modeling.min_auc,
        )

    model.fit(X_df, y)
    p_hat = model.predict_proba(X_df)[:, 1]
    w_jt, _ = inverse_propensity_weights(
        p_hat,
        clip=cfg.modeling.propensity_clip,
        max_clipped_share=cfg.modeling.max_clipped_share,
        label="Poids entreprise",
    )
    df_model = df_model.with_columns(
        pl.Series("W_JT", w_jt),
        pl.Series("P_HAT_JT", p_hat),
    )

    join_cols = ["ID_EMPLOYEUR", "PERIOD"]
    weights_df = df_model.select(join_cols + ["W_JT", "P_HAT_JT"])
    if weights_df.n_unique(subset=join_cols) != weights_df.height:
        raise ValueError("Cle entreprise-periode du modele non unique.")
    df_updated = df.drop(["W_JT", "P_HAT_JT"], strict=False).join(
        weights_df,
        on=join_cols,
        how="left",
    )
    missing_in_scope = df_updated.filter(
        (pl.col("DANS_UNIVERS_RISQUE") == 1)
        & (pl.col("P_HAT_JT").is_null() | pl.col("W_JT").is_null())
    ).height
    if missing_in_scope:
        raise ValueError(f"{missing_in_scope} lignes dans la portee K n'ont pas recu de poids.")
    outside_scope = df_updated.filter(pl.col("DANS_UNIVERS_RISQUE") == 0).height
    logger.info(
        "{} lignes hors portee K conservent explicitement P_HAT_JT et W_JT a null; "
        "aucun poids par defaut ne leur est attribue.",
        outside_scope,
    )

    write_parquet(cfg.minio, cleaned_bucket, firm_object, df_updated)
    logger.info(
        "Base entreprises mise a jour (poids IPW) : {} lignes, {} colonnes -> {}",
        df_updated.height,
        df_updated.width,
        firm_object,
    )

    # Seul un resume JSON non executable est persiste. Les pickles distants ne
    # sont jamais necessaires a la validation et constitueraient un vecteur
    # d'execution de code arbitraire.
    model_object = f"{cfg.minio.models_prefix}declaration_model.json"
    classifier = model.named_steps["classifier"]
    preprocessor = model.named_steps["preprocessor"]
    write_json(
        cfg.minio,
        cfg.minio.models_bucket,
        model_object,
        {
            "schema_version": 1,
            "model_type": "logistic_regression_l2",
            "diagnostics_oof": diagnostics.__dict__,
            "features_raw": cat_feats + num_feats,
            "features_encoded": preprocessor.get_feature_names_out().tolist(),
            "coefficients": classifier.coef_.tolist(),
            "intercept": classifier.intercept_.tolist(),
        },
    )
    logger.info("Resume JSON du modele de declaration sauvegarde : {}", model_object)

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
        level="DEBUG",
        rotation="10 MB",
        retention="30 days",
        encoding="utf-8",
    )

    try:
        ajuster_modele_declaration(cfg)
        logger.info("Termine avec succes.")
    except Exception as exc:
        logger.exception("Echec de l'etape: {}", exc)
        sys.exit(1)
