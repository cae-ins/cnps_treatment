"""
Etape 7b/12 — Modele de declaration individuelle (second etage, annexe 3).

Complete l'etape 07 (modele entreprise ``p_jt``) par le second etage de
l'annexe 3 de la note methodologique : au sein des entreprises qui ont
declare, lesquels de leurs salaries ont effectivement un salaire renseigne ?

    pi_ijt = p_jt x q_ijt

ou :
- ``p_jt``  = P(l'entreprise j declare au mois t)          -> etape 07
- ``q_ijt`` = P(le salarie i est declare | l'entreprise a declare) -> ici

Pourquoi ce second etage
------------------------
Mesure sur les donnees reelles (feuille ``Declaration_Entreprise`` de
l'audit, 23 fichiers) : **65,3% des salaires manquants se trouvent dans des
entreprises qui ont pourtant declare ce mois-la**. Ces declarations
partielles ne representent que 17,9% des couples entreprise-mois, mais
concentrent 74% des salaries (20,1 M sur 26,9 M) -- ce sont les grandes
entreprises, celles qui pesent le plus dans une moyenne ponderee par les
effectifs.

L'annexe 2, qui ne connait que ``R_jt`` valant 0 ou 1, ne peut pas
representer ce cas : elle traiterait ces 10 millions de manquants comme une
absence totale de declaration, alors que 9 999 472 salaires reels sont
observes chez ces memes employeurs, aux memes mois.

Domaine d'estimation
--------------------
Le modele est ajuste **uniquement sur les salaries des entreprises
declarantes** (``D_JT = 1``). Pour les autres, ``q_ijt`` n'est pas defini :
il n'y a rien a observer a l'interieur d'une entreprise qui n'a rien
transmis. Ces lignes gardent la convention neutre ``W_INDIV = 1.0``. Le
facteur de reponse ``R_ijt`` de l'etape 09 leur attribue un poids final nul;
aucune imputation n'entre dans le chemin de publication.

References
----------
Wooldridge, J. M. (2007). Inverse probability weighted estimation for
    general missing data problems. *Journal of Econometrics*, 141(2), 1281-1301.
Little, R. J. A. & Rubin, D. B. (2002). *Statistical Analysis with Missing
    Data* (2nd ed.). Wiley-Interscience.
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

# Caracteristiques propres au salarie (X_ijt de l'annexe 3)
_CATEGORICAL_FEATURES = [
    "SEXE",
    "STATUT_TRAVAILLEUR",
    "CL_AGE_EMPLOYE",
    "NIVEAU_ETUDE",
    # Contexte organisationnel de l'entreprise (Z_jt)
    "SECTEUR_ACTIVITE",
    "CLASSE_EFFECTIF_REDUITE",
]

_NUMERIC_FEATURES = [
    "AGE_EMPLOYE",
    "ANCIENNETE_ENTREPRISE",
    # Historique de declaration de l'individu : generalement le predicteur le
    # plus fort de la non-reponse. C'est la place legitime de l'information de
    # continuite individuelle -- comme COVARIABLE, jamais comme valeur imputee
    # (une imputation backward/forward en amont corromprait la variable que ce
    # modele cherche a expliquer).
    "TAUX_DECLARATION_PASSE_INDIV",
    "S_IJT_LAG",
    # Contexte organisationnel (Z_jt de l'annexe 3) : une entreprise qui declare
    # habituellement 30% de ses salaries n'a pas le meme comportement qu'une
    # autre a 95%. Calcule ci-dessous a partir de EFFECTIF_DECLARE (etape 05).
    "TAUX_COMPLETUDE_ENTREPRISE",
    # Distingue "premiere observation de l'individu" (historique inexistant) de
    # "jamais declare" (historique connu et negatif) : sans elle, les deux
    # situations presentent les memes valeurs nulles remplies a zero.
    "SANS_HISTORIQUE_INDIV",
    "HISTORIQUE_MOIS_PRECEDENT_MANQUANT",
    "PREMIER_MOIS_RISQUE",
]


def _ajouter_historique_individuel(df: pl.DataFrame) -> pl.DataFrame:
    """
    Ajoute l'historique de declaration de chaque salarie.

    - ``S_IJT_LAG`` : le salarie etait-il declare le mois precedent ?
    - ``TAUX_DECLARATION_PASSE_INDIV`` : part de ses mois anterieurs declares
      (moyenne cumulee **excluant le mois courant**, pour ne pas injecter dans
      les covariables l'information que le modele doit predire).
    """
    if not {"ID_INDIV", "ID_EMPLOYEUR", "S_IJT"} <= set(df.columns):
        logger.warning("ID_INDIV, ID_EMPLOYEUR ou S_IJT absent : historique non calcule.")
        return df

    history_keys = ["ID_INDIV", "ID_EMPLOYEUR"]
    period_index = "PERIOD_INDEX"
    temporary_period_index = False
    if period_index not in df.columns:
        if {"ANNEE", "MOIS"} <= set(df.columns):
            df = df.with_columns(
                (pl.col("ANNEE") * 12 + pl.col("MOIS") - 1)
                .cast(pl.Int32)
                .alias("_HIST_PERIOD_INDEX")
            )
            period_index = "_HIST_PERIOD_INDEX"
            temporary_period_index = True
        else:
            logger.warning("Index mensuel absent : historique individuel non calcule.")
            return df

    sort_cols = [*history_keys, period_index]

    df = df.sort(sort_cols)
    # Rang chronologique de l'observation dans la serie de l'individu (0 = 1re).
    # C'est le nombre de mois ANTERIEURS, donc le denominateur du taux passe.
    n_anterieurs = pl.int_range(0, pl.len()).over(history_keys)
    previous_response = pl.col("S_IJT").shift(1).over(history_keys)
    previous_index = pl.col(period_index).shift(1).over(history_keys)
    calendar_gap = pl.col(period_index) - previous_index
    df = df.with_columns(
        pl.when(calendar_gap == 1)
        .then(previous_response)
        .otherwise(0)
        .fill_null(0)
        .cast(pl.Int8)
        .alias("S_IJT_LAG"),
        # cum_sum decale d'un rang / nombre de mois anterieurs : la moyenne
        # porte sur le passe strict, sans jamais inclure le mois courant (ce
        # qui reviendrait a donner au modele la reponse qu'il doit predire).
        # Au premier mois observe, le denominateur vaut 0 : pas d'historique,
        # on pose 0.0 plutot que de diviser (sinon inf).
        pl.when(n_anterieurs > 0)
        .then(
            pl.col("S_IJT").cum_sum().over(history_keys).shift(1).over(history_keys) / n_anterieurs
        )
        .otherwise(0.0)
        .fill_null(0.0)
        .alias("TAUX_DECLARATION_PASSE_INDIV"),
        # Sans cet indicateur, la premiere observation d'un individu serait
        # indiscernable d'un salarie jamais declare : S_IJT_LAG et
        # TAUX_DECLARATION_PASSE_INDIV valent 0 dans les deux cas. Le modele
        # sous-estimerait alors sa probabilite d'etre declare et lui
        # attribuerait un poids W_INDIV excessif.
        (n_anterieurs == 0).cast(pl.Float64).alias("SANS_HISTORIQUE_INDIV"),
        ((n_anterieurs > 0) & (calendar_gap.is_null() | (calendar_gap != 1)))
        .cast(pl.Float64)
        .alias("HISTORIQUE_MOIS_PRECEDENT_MANQUANT"),
    )
    n_sans = int(df.select(pl.col("SANS_HISTORIQUE_INDIV").sum()).item())
    logger.info(
        "Historique individuel calcule : S_IJT_LAG, TAUX_DECLARATION_PASSE_INDIV, "
        "SANS_HISTORIQUE_INDIV ({} premieres observations, {:.2f}%)",
        n_sans,
        n_sans / df.height * 100 if df.height else 0.0,
    )

    if temporary_period_index:
        df = df.drop("_HIST_PERIOD_INDEX")
    return df


def _ajouter_contexte_entreprise(
    df: pl.DataFrame,
    firm_base: pl.DataFrame,
) -> pl.DataFrame:
    """Joint la completude du mois civil precedent calculee sur firm_base."""
    keys = ["ID_EMPLOYEUR", "PERIOD"]
    required = {
        *keys,
        "EFFECTIF_DECLARE",
        "EFFECTIF_OBSERVE",
        "PREMIER_MOIS_RISQUE",
    }
    missing = sorted(required - set(firm_base.columns))
    if missing:
        raise ValueError("Contexte entreprise incomplet dans firm_base: " + ", ".join(missing))
    if firm_base.n_unique(subset=keys) != firm_base.height:
        raise ValueError("Cle (ID_EMPLOYEUR, PERIOD) non unique dans firm_base.")

    sort_cols = ["ID_EMPLOYEUR", "PERIOD"]
    if "PERIOD_INDEX" in firm_base.columns:
        sort_cols = ["ID_EMPLOYEUR", "PERIOD_INDEX"]
    firm = firm_base.sort(sort_cols)

    if "PERIOD_INDEX" in firm.columns:
        gap = pl.col("PERIOD_INDEX") - pl.col("PERIOD_INDEX").shift(1).over("ID_EMPLOYEUR")
        n_gaps = firm.filter(gap.is_not_null() & (gap != 1)).height
        if n_gaps:
            raise ValueError(
                f"firm_base contient {n_gaps} ruptures de mois civil; "
                "rejouer l'etape 05 avant le modele individuel."
            )

    completion = (
        pl.when(pl.col("EFFECTIF_OBSERVE").fill_null(0) > 0)
        .then(
            pl.col("EFFECTIF_DECLARE").fill_null(0).cast(pl.Float64)
            / pl.col("EFFECTIF_OBSERVE").cast(pl.Float64)
        )
        .otherwise(0.0)
    )
    context = (
        firm.with_columns(completion.alias("_COMPLETUDE_COURANTE"))
        .with_columns(
            pl.col("_COMPLETUDE_COURANTE")
            .shift(1)
            .over("ID_EMPLOYEUR")
            .alias("TAUX_COMPLETUDE_ENTREPRISE")
        )
        .select(
            *keys,
            "TAUX_COMPLETUDE_ENTREPRISE",
            "PREMIER_MOIS_RISQUE",
        )
    )

    before = df.height
    result = df.drop(
        ["TAUX_COMPLETUDE_ENTREPRISE", "PREMIER_MOIS_RISQUE"],
        strict=False,
    ).join(context, on=keys, how="left")
    if result.height != before:
        raise ValueError("La jointure du contexte entreprise a change le nombre de lignes.")
    if result["PREMIER_MOIS_RISQUE"].null_count():
        raise ValueError("Des lignes analytiques n'ont pas de correspondance dans firm_base.")
    logger.info(
        "TAUX_COMPLETUDE_ENTREPRISE joint depuis firm_base: meme valeur pour "
        "toutes les lignes d'un couple entreprise-mois, decalee d'un mois civil."
    )
    return result


def _prepare_features(df: pl.DataFrame) -> tuple[pl.DataFrame, list[str], list[str]]:
    """Selectionne les covariables disponibles et ecarte celles sans variance."""
    cat_feats = [c for c in _CATEGORICAL_FEATURES if c in df.columns]
    num_feats = [c for c in _NUMERIC_FEATURES if c in df.columns]

    # Une covariable constante n'apporte rien et fait echouer l'encodage
    cat_feats = [c for c in cat_feats if df[c].n_unique() > 1]
    num_feats = [c for c in num_feats if df[c].n_unique() > 1]

    if not cat_feats and not num_feats:
        raise ValueError("Aucune covariable valide pour le modele de declaration individuelle")

    df_model = df.with_columns(
        [pl.col(c).cast(pl.Utf8) for c in cat_feats]
        + [pl.col(c).cast(pl.Float64).fill_null(0.0) for c in num_feats]
    )
    logger.info("Covariables du modele individuel : cat={}, num={}", cat_feats, num_feats)
    return df_model, cat_feats, num_feats


def ajuster_modele_declaration_indiv(cfg: PipelineConfig) -> str:
    """
    Ajuste le modele q_ijt et calcule les poids individuels W_INDIV.

    Etapes
    ------
    1. Lecture de la base analytique (individus + attributs entreprise, dont D_JT)
    2. Calcul de l'historique de declaration individuelle
    3. Restriction aux entreprises declarantes (D_JT = 1)
    4. Ajustement d'une regression logistique sur S_IJT
    5. Facteur individuel non stabilise W_INDIV = 1 / q_hat
    6. Convention neutre W_INDIV = 1.0 hors du domaine conditionnel
    7. Mise a jour de la base analytique

    Returns
    -------
    str
        Nom de l'objet Parquet de la base analytique mise a jour.
    """
    bucket = cfg.minio.cleaned_bucket
    analytical_object = f"{cfg.minio.cleaned_prefix}analytical_base.parquet"
    if not object_exists(cfg.minio, bucket, analytical_object):
        raise FileNotFoundError(f"Base analytique introuvable : {bucket}/{analytical_object}")

    df = read_parquet(cfg.minio, bucket, analytical_object)
    logger.info("Modele de declaration individuelle sur {} lignes", df.height)

    # D.1: le contexte entreprise vient du panel complet de l'etape 05.
    # Les poids entreprise ne sont volontairement pas joints ici; E.1 les
    # rapatrie dans l'etape 09, seule etape qui les consomme.
    firm_object = f"{cfg.minio.cleaned_prefix}firm_base.parquet"
    if not object_exists(cfg.minio, bucket, firm_object):
        raise FileNotFoundError(f"Base entreprises introuvable : {bucket}/{firm_object}")
    firm = read_parquet(cfg.minio, bucket, firm_object)

    if "S_IJT" not in df.columns:
        raise ValueError("Colonne S_IJT absente : l'etape 04 doit la calculer avant cette etape.")
    if "D_JT" not in df.columns:
        raise ValueError("Colonne D_JT absente : l'etape 05 doit la calculer avant cette etape.")

    df = _ajouter_historique_individuel(df)
    df = _ajouter_contexte_entreprise(df, firm)

    # --- Domaine d'estimation : entreprises declarantes uniquement ---
    # q_ijt est conditionnel a D_JT = 1. Estimer le modele sur l'ensemble des
    # lignes melangerait deux mecanismes distincts (l'entreprise ne declare
    # pas / elle declare mais omet ce salarie) et biaiserait les deux.
    scope = (
        pl.col("DANS_UNIVERS_RISQUE") == 1 if "DANS_UNIVERS_RISQUE" in df.columns else pl.lit(True)
    )
    declarantes = df.filter(scope & (pl.col("D_JT") == 1))
    n_hors_domaine = df.height - declarantes.height
    logger.info(
        "Domaine d'estimation : {} lignes en entreprises declarantes, "
        "{} lignes hors domaine (D_JT = 0, W_INDIV restera a 1.0)",
        declarantes.height,
        n_hors_domaine,
    )

    if declarantes.height == 0:
        raise ValueError("Aucune entreprise declarante pour ajuster q_ijt.")

    taux_declare = float(declarantes["S_IJT"].mean())
    logger.info(
        "Taux de declaration individuelle chez les declarantes : {:.2f}%",
        taux_declare * 100,
    )

    if declarantes["S_IJT"].n_unique() < 2:
        raise ValueError("Classe cible unique pour S_IJT chez les entreprises declarantes.")

    df_model, cat_feats, num_feats = _prepare_features(declarantes)
    y = df_model["S_IJT"].to_numpy().astype(float)

    reject_never_responding_strata(
        df_model,
        target="S_IJT",
        categorical_features=cat_feats,
        min_size=cfg.modeling.min_structural_stratum_size,
        label="modele individuel",
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
    groups = df_model["ID_EMPLOYEUR"].to_numpy()
    q_oof, n_splits = grouped_oof_predictions(
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
        q_oof,
        clip=cfg.modeling.propensity_clip,
        calibration_slope_range=cfg.modeling.calibration_slope_range,
        max_calibration_in_large=cfg.modeling.max_calibration_in_large,
        max_abs_smd=cfg.modeling.max_abs_smd,
        n_splits=n_splits,
        label="Modele individuel",
    )
    if diagnostics.auc < cfg.modeling.min_auc:
        logger.warning(
            "AUC OOF individuelle {:.4f} sous le repere descriptif {:.4f}; aucun blocage AUC.",
            diagnostics.auc,
            cfg.modeling.min_auc,
        )

    model.fit(X_df, y)
    q_hat = model.predict_proba(X_df)[:, 1]
    w_indiv, _ = inverse_propensity_weights(
        q_hat,
        clip=cfg.modeling.propensity_clip,
        max_clipped_share=cfg.modeling.max_clipped_share,
        label="Poids individuels",
    )

    df_model = df_model.with_columns(
        pl.Series("_W_INDIV_NEW", w_indiv),
        pl.Series("_Q_HAT_NEW", q_hat),
    )
    join_key = "OBS_ID" if "OBS_ID" in df.columns and "OBS_ID" in df_model.columns else None
    if join_key is None:
        raise ValueError("OBS_ID absent : impossible de reinjecter les poids individuels.")
    if df_model[join_key].n_unique() != df_model.height:
        raise ValueError("OBS_ID non unique dans le domaine du modele individuel.")

    df = df.drop(["Q_HAT_IJT", "W_INDIV", "W_FINAL"], strict=False).join(
        df_model.select([join_key, "_W_INDIV_NEW", "_Q_HAT_NEW"]),
        on=join_key,
        how="left",
    )
    missing_declaring = df.filter(
        scope
        & (pl.col("D_JT") == 1)
        & (pl.col("_W_INDIV_NEW").is_null() | pl.col("_Q_HAT_NEW").is_null())
    ).height
    if missing_declaring:
        raise ValueError(f"{missing_declaring} lignes declarantes sans poids individuel.")

    # Hors domaine q_ijt n'est pas estime. La convention neutre q=1 est
    # explicite; le facteur D_JT mettra ces lignes a zero a l'etape 09.
    df = df.with_columns(
        pl.when(scope & (pl.col("D_JT") == 1))
        .then(pl.col("_W_INDIV_NEW"))
        .otherwise(1.0)
        .alias("W_INDIV"),
        pl.when(scope & (pl.col("D_JT") == 1))
        .then(pl.col("_Q_HAT_NEW"))
        .otherwise(1.0)
        .alias("Q_HAT_IJT"),
    ).drop(["_W_INDIV_NEW", "_Q_HAT_NEW"])

    write_parquet(cfg.minio, bucket, analytical_object, df)
    logger.info(
        "Base analytique mise a jour (poids individuels) : {} lignes, {} colonnes -> {}",
        df.height,
        df.width,
        analytical_object,
    )

    model_object = f"{cfg.minio.models_prefix}declaration_indiv_model.json"
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
            "taux_declare": taux_declare,
        },
    )
    logger.info("Resume JSON du modele individuel sauvegarde : {}", model_object)

    return analytical_object


if __name__ == "__main__":
    ajuster_modele_declaration_indiv(load_config())
