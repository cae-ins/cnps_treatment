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

from cnps.config import PipelineConfig, load_config
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
    bucket = cfg.minio.cleaned_bucket
    analytical_object = f"{cfg.minio.cleaned_prefix}analytical_base.parquet"

    if not object_exists(cfg.minio, bucket, analytical_object):
        raise FileNotFoundError(f"Base analytique introuvable : {bucket}/{analytical_object}")

    df = read_parquet(cfg.minio, bucket, analytical_object)
    method = cfg.modeling.estimation_method

    logger.info("Calcul des poids finaux avec la methode : {} ({} lignes)", method, df.height)

    if method == "aipw" and "P_HAT_JT" in df.columns:
        # Meme variable de reference que les etapes 04, 05 et 10 (cf. commentaire
        # de 05_base_entreprises.py) : SALAIRE_BRUT_ESTIME_AU_MOIS traite chaque
        # periodicite avec sa propre conversion et ne depend pas de
        # DUREE_TRAVAILLEE.
        salary_col = next(
            (c for c in ("SALAIRE_BRUT_ESTIME_AU_MOIS", "SALAIRE_BRUT_MENS", "SALAIRE_BRUT")
             if c in df.columns),
            "SALAIRE_BRUT",
        )

        if salary_col in df.columns and "P_HAT_JT" in df.columns:
            y_obs = df[salary_col].fill_null(0).to_numpy()
            p_hat = df["P_HAT_JT"].fill_null(0.5).to_numpy()
            d = df["R_JT"].fill_null(1).to_numpy().astype(float) if "R_JT" in df.columns \
                else np.ones(len(df))

            # Utilise la moyenne imputee au niveau entreprise comme prediction du modele de resultat
            y_imputed = df["SALAIRE_MOYEN"].fill_null(
                df[salary_col].mean()
            ).to_numpy() if "SALAIRE_MOYEN" in df.columns else y_obs.copy()

            w_aipw, mu_aipw = _compute_aipw_weights(
                y_obs, y_imputed, p_hat, d,
                cfg.modeling.ipw_trim_lower, cfg.modeling.ipw_trim_upper,
            )

            # L'AIPW corrige la non-declaration au niveau ENTREPRISE (p_jt) : il
            # remplace W_JT, pas W_INDIV. Le second etage de l'annexe 3 (q_ijt,
            # non-declaration partielle a l'interieur des entreprises
            # declarantes) reste a appliquer par-dessus, sans quoi la correction
            # portant sur 65,3% des salaires manquants serait perdue.
            df = df.with_columns(pl.Series("_W_AIPW", w_aipw))
            if "W_INDIV" in df.columns:
                # W_INDIV n'est renseigne que sur le domaine d'estimation (entreprises
                # declarantes). Hors domaine il vaut null : sans ce fill_null(1.0), la
                # multiplication propagerait le null et retirerait silencieusement les
                # salaries des entreprises non declarantes de toutes les statistiques.
                n_hors_domaine = df["W_INDIV"].null_count()
                df = df.with_columns(
                    (pl.col("_W_AIPW") * pl.col("W_INDIV").fill_null(1.0)).alias("W_FINAL")
                )
                logger.info(
                    "AIPW (etage entreprise) compose avec W_INDIV (etage individuel) : "
                    "W_FINAL = W_AIPW x W_INDIV ({} lignes hors domaine ramenees a "
                    "W_INDIV=1.0)", n_hors_domaine,
                )
            else:
                df = df.with_columns(pl.col("_W_AIPW").alias("W_FINAL"))
                logger.warning(
                    "W_INDIV absent : W_FINAL = W_AIPW seul. Le second etage de "
                    "l'annexe 3 n'est pas applique (etape 07b executee ?)."
                )
            df = df.drop("_W_AIPW")
            logger.info("Estimation AIPW du salaire moyen : {:.0f} FCFA", mu_aipw)
        else:
            logger.warning("Colonnes manquantes pour AIPW, repli sur IPW standard")
            df = df.with_columns(
                (pl.col("W_JT").fill_null(1.0)
                 * pl.col("W_INDIV").fill_null(1.0)).alias("W_FINAL")
            )
    else:
        # --- IPW standard ---
        df = df.with_columns(
            (pl.col("W_JT").fill_null(1.0)
             * pl.col("W_INDIV").fill_null(1.0)).alias("W_FINAL")
        )

    # Normalisation des poids par strate (periode).
    # La moyenne d'une periode peut etre nulle (toutes les lignes du groupe a poids
    # zero) : la division produirait alors des NaN silencieux, qui contaminent
    # ensuite toute somme ponderee en aval. On neutralise ce cas explicitement.
    if "PERIOD" in df.columns:
        n_periodes = df["PERIOD"].n_unique()
        moyenne_periode = pl.col("W_FINAL").mean().over("PERIOD")
        df = df.with_columns(
            pl.when(moyenne_periode > 0)
            .then(pl.col("W_FINAL") / moyenne_periode)
            .otherwise(pl.col("W_FINAL"))
            .alias("W_FINAL")
        )
        logger.info("Poids normalises par periode ({} periodes, moyenne=1.0 sur chacune)", n_periodes)

    # Controle d'integrite : un poids null ou non fini retire silencieusement des
    # lignes de toutes les statistiques ponderees en aval (regles de Rubin,
    # etapes 10 et 12). L'erreur doit etre bloquante, pas un simple avertissement.
    n_null = df["W_FINAL"].null_count()
    n_non_fini = int(df.select(
        (~pl.col("W_FINAL").is_finite() & pl.col("W_FINAL").is_not_null()).sum()
    ).item())
    if n_null or n_non_fini:
        raise ValueError(
            f"W_FINAL contient {n_null} valeurs nulles et {n_non_fini} valeurs non "
            f"finies (NaN/inf) sur {df.height} lignes. Ces lignes seraient exclues "
            f"des statistiques sans avertissement. Verifier que W_JT et W_INDIV sont "
            f"renseignes sur l'ensemble du panel (etapes 07 et 07b) et que la "
            f"normalisation par periode ne divise pas par une moyenne nulle."
        )

    # Un poids nul exclut la ligne des statistiques. C'est le comportement attendu
    # de l'IPW pour les entreprises non declarantes (R_JT = 0) : elles n'ont pas de
    # salaire observe a porter, la correction passe par la reponderation des
    # declarantes. On le journalise pour que le volume reste explicite et verifiable.
    n_zero = int(df.select((pl.col("W_FINAL") == 0).sum()).item())
    if n_zero:
        logger.info(
            "Poids nuls : {} lignes ({:.2f}%) sortent des statistiques ponderees. "
            "Attendu pour les entreprises non declarantes (R_JT = 0), dont la "
            "contribution est portee par la reponderation des declarantes.",
            n_zero, 100.0 * n_zero / df.height,
        )

    write_parquet(cfg.minio, bucket, analytical_object, df)
    logger.info(
        "Poids finaux calcules : {} lignes -> {} | moyenne={:.3f}, ecart-type={:.3f}, plage=[{:.3f}, {:.3f}]",
        df.height, analytical_object,
        df["W_FINAL"].mean(), df["W_FINAL"].std(), df["W_FINAL"].min(), df["W_FINAL"].max(),
    )

    return analytical_object


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
        calculer_poids_finaux(cfg)
        logger.info("Termine avec succes.")
    except Exception as exc:
        logger.exception("Echec de l'etape: {}", exc)
        sys.exit(1)
