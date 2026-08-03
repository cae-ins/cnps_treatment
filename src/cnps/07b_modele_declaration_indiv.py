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
declarantes** (``R_JT = 1``). Pour les autres, ``q_ijt`` n'est pas defini :
il n'y a rien a observer a l'interieur d'une entreprise qui n'a rien
transmis. Ces lignes gardent ``W_INDIV = 1.0`` et sont prises en charge par
``W_JT`` (etape 07) et l'imputation au niveau entreprise (etape 08),
exactement comme dans l'annexe 2.

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
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_auc_score
from sklearn.pipeline import Pipeline as SKPipeline
from sklearn.preprocessing import OneHotEncoder

from cnps.config import PipelineConfig, load_config
from cnps.storage import object_exists, read_parquet, write_parquet, write_pickle

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
]


def _ajouter_historique_individuel(df: pl.DataFrame) -> pl.DataFrame:
    """
    Ajoute l'historique de declaration de chaque salarie.

    - ``S_IJT_LAG`` : le salarie etait-il declare le mois precedent ?
    - ``TAUX_DECLARATION_PASSE_INDIV`` : part de ses mois anterieurs declares
      (moyenne cumulee **excluant le mois courant**, pour ne pas injecter dans
      les covariables l'information que le modele doit predire).
    """
    if "ID_INDIV" not in df.columns or "S_IJT" not in df.columns:
        logger.warning(
            "ID_INDIV ou S_IJT absent : historique individuel non calcule."
        )
        return df

    sort_cols = [c for c in ("ID_INDIV", "PERIOD", "ANNEE", "MOIS") if c in df.columns]
    if len(sort_cols) < 2:
        logger.warning("Aucune colonne de periode : historique individuel non calcule.")
        return df

    df = df.sort(sort_cols)
    # Rang chronologique de l'observation dans la serie de l'individu (0 = 1re).
    # C'est le nombre de mois ANTERIEURS, donc le denominateur du taux passe.
    n_anterieurs = pl.int_range(0, pl.len()).over("ID_INDIV")
    df = df.with_columns(
        pl.col("S_IJT").shift(1).over("ID_INDIV").fill_null(0).cast(pl.Int8)
        .alias("S_IJT_LAG"),
        # cum_sum decale d'un rang / nombre de mois anterieurs : la moyenne
        # porte sur le passe strict, sans jamais inclure le mois courant (ce
        # qui reviendrait a donner au modele la reponse qu'il doit predire).
        # Au premier mois observe, le denominateur vaut 0 : pas d'historique,
        # on pose 0.0 plutot que de diviser (sinon inf).
        pl.when(n_anterieurs > 0)
        .then(
            pl.col("S_IJT").cum_sum().over("ID_INDIV").shift(1) / n_anterieurs
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
    )
    n_sans = int(df.select(pl.col("SANS_HISTORIQUE_INDIV").sum()).item())
    logger.info(
        "Historique individuel calcule : S_IJT_LAG, TAUX_DECLARATION_PASSE_INDIV, "
        "SANS_HISTORIQUE_INDIV ({} premieres observations, {:.2f}%)",
        n_sans, n_sans / df.height * 100 if df.height else 0.0,
    )

    # --- Contexte organisationnel : completude habituelle de l'entreprise ---
    # ATTENTION : le taux de completude du mois COURANT contient directement
    # l'information a predire (si l'entreprise declare 100% de ses salaries,
    # alors S_IJT = 1 par construction). On le decale donc d'un mois : c'est le
    # comportement PASSE de l'employeur qui sert de covariable, jamais celui du
    # mois estime.
    if {"EFFECTIF_DECLARE", "EFFECTIF_OBSERVE", "ID_EMPLOYEUR"} <= set(df.columns):
        df = df.sort([c for c in ("ID_EMPLOYEUR", "PERIOD", "ANNEE", "MOIS") if c in df.columns])
        # Completude du mois : part des salaries presents dont le salaire est
        # renseigne. Le panel de l'etape 05 etant equilibre, les mois ou
        # l'entreprise n'apparait pas ont EFFECTIF_* a null : la completude y
        # vaut 0 (rien de declare), et non "inconnue" -- d'ou le fill_null(0.0)
        # AVANT le decalage. L'appliquer apres confondrait "mois precedent sans
        # declaration" et "pas de mois precedent".
        completude = (
            pl.when(pl.col("EFFECTIF_OBSERVE").fill_null(0) > 0)
            .then(
                pl.col("EFFECTIF_DECLARE").fill_null(0).cast(pl.Float64)
                / pl.col("EFFECTIF_OBSERVE").cast(pl.Float64)
            )
            .otherwise(0.0)
        )
        df = df.with_columns(
            completude.shift(1)
            .over("ID_EMPLOYEUR")
            .fill_null(0.0)  # 1er mois observe : pas d'historique
            .alias("TAUX_COMPLETUDE_ENTREPRISE")
        )
        logger.info(
            "Contexte entreprise calcule : TAUX_COMPLETUDE_ENTREPRISE "
            "(completude du mois precedent, decalee pour eviter toute fuite)"
        )
    else:
        logger.info(
            "EFFECTIF_DECLARE/EFFECTIF_OBSERVE absents : TAUX_COMPLETUDE_ENTREPRISE "
            "non calcule (l'etape 05 doit avoir ete rejouee)."
        )

    return df


def _prepare_features(df: pl.DataFrame) -> tuple[pl.DataFrame, list[str], list[str]]:
    """Selectionne les covariables disponibles et ecarte celles sans variance."""
    cat_feats = [c for c in _CATEGORICAL_FEATURES if c in df.columns]
    num_feats = [c for c in _NUMERIC_FEATURES if c in df.columns]

    # Une covariable constante n'apporte rien et fait echouer l'encodage
    cat_feats = [c for c in cat_feats if df[c].n_unique() > 1]
    num_feats = [c for c in num_feats if df[c].n_unique() > 1]

    if not cat_feats and not num_feats:
        raise ValueError(
            "Aucune covariable valide pour le modele de declaration individuelle"
        )

    df_model = df.with_columns(
        [pl.col(c).cast(pl.Utf8).fill_null("INCONNU") for c in cat_feats]
        + [pl.col(c).cast(pl.Float64).fill_null(0.0) for c in num_feats]
    )
    logger.info(
        "Covariables du modele individuel : cat={}, num={}", cat_feats, num_feats
    )
    return df_model, cat_feats, num_feats


def ajuster_modele_declaration_indiv(cfg: PipelineConfig) -> str:
    """
    Ajuste le modele q_ijt et calcule les poids individuels W_INDIV.

    Etapes
    ------
    1. Lecture de la base analytique (individus + attributs entreprise, dont R_JT)
    2. Calcul de l'historique de declaration individuelle
    3. Restriction aux entreprises declarantes (R_JT = 1)
    4. Ajustement d'une regression logistique sur S_IJT
    5. Poids stabilises W_INDIV = q_moyen / q_hat, tronques
    6. W_INDIV = 1.0 pour les salaries des entreprises non declarantes
    7. Mise a jour de la base analytique

    Returns
    -------
    str
        Nom de l'objet Parquet de la base analytique mise a jour.
    """
    bucket = cfg.minio.cleaned_bucket
    analytical_object = f"{cfg.minio.cleaned_prefix}analytical_base.parquet"
    if not object_exists(cfg.minio, bucket, analytical_object):
        raise FileNotFoundError(
            f"Base analytique introuvable : {bucket}/{analytical_object}"
        )

    df = read_parquet(cfg.minio, bucket, analytical_object)
    logger.info("Modele de declaration individuelle sur {} lignes", df.height)

    # --- Rafraichissement des poids entreprise ---
    # La base analytique est construite a l'etape 06, AVANT que l'etape 07
    # n'ajuste le modele entreprise : elle porte donc un W_JT provisoire (1.0)
    # et pas de P_HAT_JT. On les recupere depuis firm_base, mise a jour par
    # l'etape 07, faute de quoi le W_FINAL calcule plus bas serait faux et
    # l'etape 09 travaillerait sur des poids perimes.
    firm_object = f"{cfg.minio.cleaned_prefix}firm_base.parquet"
    if object_exists(cfg.minio, bucket, firm_object):
        firm = read_parquet(cfg.minio, bucket, firm_object)
        poids_cols = [c for c in ("W_JT", "P_HAT_JT") if c in firm.columns]
        join_cols = [
            c for c in ("ID_EMPLOYEUR", "PERIOD")
            if c in df.columns and c in firm.columns
        ]
        if poids_cols and len(join_cols) == 2:
            df = df.drop(poids_cols, strict=False).join(
                firm.select(join_cols + poids_cols), on=join_cols, how="left"
            )
            n_sans_poids = df["W_JT"].null_count() if "W_JT" in df.columns else 0
            if n_sans_poids:
                df = df.with_columns(pl.col("W_JT").fill_null(1.0))
                logger.info(
                    "Poids entreprise par defaut (1.0) applique a {} lignes sans "
                    "correspondance dans firm_base", n_sans_poids,
                )
            logger.info("Poids entreprise rafraichis depuis firm_base : {}", poids_cols)
        else:
            logger.warning(
                "Impossible de rafraichir les poids entreprise (colonnes {} / cles {}) : "
                "W_JT peut etre perime.", poids_cols, join_cols,
            )
    else:
        logger.warning(
            "firm_base introuvable : W_JT reste celui de l'etape 06 (provisoire)."
        )

    if "S_IJT" not in df.columns:
        raise ValueError(
            "Colonne S_IJT absente : l'etape 04 doit la calculer avant cette etape."
        )
    if "R_JT" not in df.columns:
        raise ValueError(
            "Colonne R_JT absente : l'etape 05 doit la calculer avant cette etape."
        )

    df = _ajouter_historique_individuel(df)

    # --- Domaine d'estimation : entreprises declarantes uniquement ---
    # q_ijt est conditionnel a R_JT = 1. Estimer le modele sur l'ensemble des
    # lignes melangerait deux mecanismes distincts (l'entreprise ne declare
    # pas / elle declare mais omet ce salarie) et biaiserait les deux.
    declarantes = df.filter(pl.col("R_JT") == 1)
    n_hors_domaine = df.height - declarantes.height
    logger.info(
        "Domaine d'estimation : {} lignes en entreprises declarantes, "
        "{} lignes hors domaine (R_JT = 0, W_INDIV restera a 1.0)",
        declarantes.height, n_hors_domaine,
    )

    if declarantes.height == 0:
        logger.warning(
            "Aucune entreprise declarante : W_INDIV laisse a 1.0 sur toutes les lignes."
        )
        return analytical_object

    taux_declare = float(declarantes["S_IJT"].mean())
    logger.info(
        "Taux de declaration individuelle chez les declarantes : {:.2f}%",
        taux_declare * 100,
    )

    # Si toutes les entreprises declarantes declarent 100% de leurs salaries,
    # il n'y a pas de non-reponse partielle a corriger : le modele n'a pas
    # d'objet et le second etage se reduit a l'identite.
    if declarantes["S_IJT"].n_unique() < 2:
        logger.warning(
            "S_IJT est constant chez les entreprises declarantes : aucune "
            "declaration partielle a corriger, W_INDIV reste a 1.0."
        )
        df = df.with_columns(pl.lit(1.0).alias("W_INDIV"))
        write_parquet(cfg.minio, bucket, analytical_object, df)
        return analytical_object

    df_model, cat_feats, num_feats = _prepare_features(declarantes)
    y = df_model["S_IJT"].to_numpy().astype(float)

    transformers = []
    if cat_feats:
        transformers.append((
            "cat",
            OneHotEncoder(drop="first", sparse_output=False, handle_unknown="ignore"),
            cat_feats,
        ))
    if num_feats:
        transformers.append(("num", "passthrough", num_feats))

    model = SKPipeline([
        ("preprocessor", ColumnTransformer(transformers, remainder="drop")),
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

    q_hat = model.predict_proba(X_df)[:, 1]

    auc = roc_auc_score(y, q_hat)
    logger.info("AUC du modele de declaration individuelle : {:.4f}", auc)
    if auc < cfg.modeling.min_auc:
        logger.warning(
            "AUC ({:.4f}) sous le seuil configure ({:.4f}) : les poids W_INDIV "
            "corrigeront mal la non-reponse partielle. Enrichir les covariables.",
            auc, cfg.modeling.min_auc,
        )

    # --- Poids stabilises et tronques (meme schema que l'etape 07) ---
    epsilon = 1e-6
    q_hat_clipped = np.clip(q_hat, epsilon, 1 - epsilon)
    w_indiv = taux_declare / q_hat_clipped

    lo = float(np.quantile(w_indiv, cfg.modeling.ipw_trim_lower))
    hi = float(np.quantile(w_indiv, cfg.modeling.ipw_trim_upper))
    logger.info(
        "Troncature des poids individuels aux percentiles [{:.0%}, {:.0%}] : "
        "bornes [{:.3f}, {:.3f}]",
        cfg.modeling.ipw_trim_lower, cfg.modeling.ipw_trim_upper, lo, hi,
    )
    w_indiv = np.clip(w_indiv, lo, hi)
    logger.info(
        "Poids W_INDIV (entreprises declarantes) : moyenne={:.3f}, mediane={:.3f}, "
        "plage=[{:.3f}, {:.3f}]",
        w_indiv.mean(), float(np.median(w_indiv)), w_indiv.min(), w_indiv.max(),
    )

    # --- Reintegration dans la base complete ---
    df_model = df_model.with_columns(
        pl.Series("_W_INDIV_NEW", w_indiv),
        pl.Series("Q_HAT_IJT", q_hat),
    )

    join_key = "OBS_ID" if "OBS_ID" in df.columns and "OBS_ID" in df_model.columns else None
    if join_key is None:
        raise ValueError(
            "OBS_ID absent : impossible de reinjecter les poids individuels sans "
            "cle d'observation unique (voir etape 04)."
        )

    df = df.drop(["Q_HAT_IJT"], strict=False).join(
        df_model.select([join_key, "_W_INDIV_NEW", "Q_HAT_IJT"]),
        on=join_key,
        how="left",
    )

    # Hors domaine (R_JT = 0) : W_INDIV = 1.0, la correction est portee par W_JT
    # et l'imputation entreprise de l'etape 08.
    df = df.with_columns(
        pl.col("_W_INDIV_NEW").fill_null(1.0).alias("W_INDIV")
    ).drop("_W_INDIV_NEW")

    # W_FINAL provisoire, recalcule a l'etape 09 selon la methode retenue
    if "W_JT" in df.columns:
        df = df.with_columns((pl.col("W_JT") * pl.col("W_INDIV")).alias("W_FINAL"))
        logger.info("W_FINAL provisoire recalcule : W_JT x W_INDIV")

    write_parquet(cfg.minio, bucket, analytical_object, df)
    logger.info(
        "Base analytique mise a jour (poids individuels) : {} lignes, {} colonnes -> {}",
        df.height, df.width, analytical_object,
    )

    model_object = f"{cfg.minio.models_prefix}declaration_indiv_model.pkl"
    write_pickle(cfg.minio, cfg.minio.models_bucket, model_object, {
        "model": model,
        "auc": auc,
        "features": cat_feats + num_feats,
        "taux_declare": taux_declare,
    })
    logger.info("Modele de declaration individuelle sauvegarde : {}", model_object)

    return analytical_object


if __name__ == "__main__":
    ajuster_modele_declaration_indiv(load_config())
