"""
Etape 9/12 - Ponderation finale IPW a deux etages.

Le poids d'analyse est construit au niveau salarie-employeur-mois :

    R_ijt = D_jt * S_ijt
    W_FINAL_RAW = R_ijt / (P_HAT_JT * Q_HAT_IJT)

Les propensions proviennent des etapes 07 et 07b. L'etape joint elle-meme
les poids entreprise depuis firm_base, refuse toute cle sans correspondance,
applique le trimming configure aux seuls poids positifs et conserve le poids
brut pour la tracabilite. Les non-repondants ont toujours un poids nul.

Aucune composante d'imputation ou d'augmentation n'entre dans ce calcul.
"""

from __future__ import annotations

import numpy as np
import polars as pl
from loguru import logger

from cnps.config import PipelineConfig, load_config
from cnps.response_diagnostics import trim_positive_weights
from cnps.storage import object_exists, read_parquet, write_parquet


def _join_firm_weights(
    analytical: pl.DataFrame,
    firm_base: pl.DataFrame,
) -> pl.DataFrame:
    """Joint les poids entreprise en plusieurs-vers-un sans valeur par defaut."""
    keys = ["ID_EMPLOYEUR", "PERIOD"]
    required = {*keys, "P_HAT_JT", "W_JT"}
    missing = sorted(required - set(firm_base.columns))
    if missing:
        raise ValueError(
            "Poids entreprise absents de firm_base: "
            + ", ".join(missing)
            + ". Executer l'etape 07."
        )
    if firm_base.n_unique(subset=keys) != firm_base.height:
        raise ValueError("Cle (ID_EMPLOYEUR, PERIOD) non unique dans firm_base.")

    before = analytical.height
    firm_columns = [*keys, "P_HAT_JT", "W_JT"]
    if "DANS_UNIVERS_RISQUE" in firm_base.columns:
        firm_columns.append("DANS_UNIVERS_RISQUE")
    firm_weights = firm_base.select(firm_columns).with_columns(
        pl.lit(1).cast(pl.Int8).alias("_APPARIE_FIRM")
    )
    result = analytical.drop(["P_HAT_JT", "W_JT", "DANS_UNIVERS_RISQUE"], strict=False).join(
        firm_weights,
        on=keys,
        how="left",
    )
    if result.height != before:
        raise ValueError("La jointure des poids entreprise a change la cardinalite.")
    # Un non-appariement n'est pas un hors-univers. Sans ce controle, la portee
    # ci-dessous l'ignore (DANS_UNIVERS_RISQUE y est null, donc filtre) et le
    # poids tombe silencieusement a zero au fill_null de _compute_two_stage_weights.
    unmatched = result.filter(pl.col("_APPARIE_FIRM").is_null()).height
    if unmatched:
        raise ValueError(
            f"{unmatched} lignes analytiques sans correspondance dans firm_base; "
            "rejouer l'etape 05."
        )
    result = result.drop("_APPARIE_FIRM")
    scope = (
        pl.col("DANS_UNIVERS_RISQUE") == 1
        if "DANS_UNIVERS_RISQUE" in result.columns
        else pl.lit(True)
    )
    missing_keys = result.filter(
        scope & (pl.col("P_HAT_JT").is_null() | pl.col("W_JT").is_null())
    ).height
    if missing_keys:
        raise ValueError(
            f"{missing_keys} lignes analytiques sans poids entreprise; "
            "aucun remplissage a 1 n'est autorise."
        )
    return result


def _validate_input_weights(df: pl.DataFrame, cfg: PipelineConfig | None = None) -> None:
    """Controle les poids entreprise puis, avec la config, les deux etages."""
    missing = sorted({"P_HAT_JT", "W_JT"} - set(df.columns))
    if missing:
        raise ValueError(
            "Colonnes requises absentes: "
            + ", ".join(missing)
            + ". Executer 07_modele_declaration.py avant l'etape 09."
        )

    p = df["P_HAT_JT"].cast(pl.Float64, strict=False).fill_null(np.nan).to_numpy()
    w_jt = df["W_JT"].cast(pl.Float64, strict=False).fill_null(np.nan).to_numpy()
    invalid_p = ~np.isfinite(p) | (p <= 0) | (p >= 1)
    invalid_w_jt = ~np.isfinite(w_jt) | (w_jt <= 0)
    if invalid_p.any() or invalid_w_jt.any():
        raise ValueError(
            f"P_HAT_JT ou W_JT invalide: P_HAT_JT={invalid_p.sum()}, W_JT={invalid_w_jt.sum()}."
        )

    if np.unique(w_jt).size == 1:
        logger.warning(
            "W_JT est constant ({:.6g}); situation legitime sous MCAR, non bloquante.",
            w_jt[0],
        )

    if cfg is None:
        return

    missing = sorted({"Q_HAT_IJT", "W_INDIV", "D_JT", "S_IJT"} - set(df.columns))
    if missing:
        raise ValueError(
            "Colonnes du second etage absentes: "
            + ", ".join(missing)
            + ". Executer 07b_modele_declaration_indiv.py avant l'etape 09."
        )

    q = df["Q_HAT_IJT"].cast(pl.Float64, strict=False).fill_null(np.nan).to_numpy()
    w_indiv = df["W_INDIV"].cast(pl.Float64, strict=False).fill_null(np.nan).to_numpy()
    d = df["D_JT"].to_numpy()
    s = df["S_IJT"].to_numpy()
    if not np.isin(d, [0, 1]).all() or not np.isin(s, [0, 1]).all():
        raise ValueError("D_JT et S_IJT doivent etre binaires et non nuls.")
    invalid_q = ~np.isfinite(q) | (q <= 0) | ((d == 1) & (q >= 1))
    invalid_w_indiv = ~np.isfinite(w_indiv) | (w_indiv <= 0)
    if invalid_q.any() or invalid_w_indiv.any():
        raise ValueError(
            "Q_HAT_IJT ou W_INDIV invalide: "
            f"Q_HAT_IJT={invalid_q.sum()}, W_INDIV={invalid_w_indiv.sum()}."
        )

    expected_jt = 1.0 / np.clip(p, cfg.modeling.propensity_clip, 1 - cfg.modeling.propensity_clip)
    expected_indiv = np.where(
        d == 1,
        1.0 / np.clip(q, cfg.modeling.propensity_clip, 1 - cfg.modeling.propensity_clip),
        1.0,
    )
    if not np.allclose(w_jt, expected_jt, rtol=1e-10, atol=1e-12):
        raise ValueError("W_JT ne correspond pas a 1/P_HAT_JT: provenance incoherente.")
    if not np.allclose(w_indiv, expected_indiv, rtol=1e-10, atol=1e-12):
        raise ValueError("W_INDIV ne correspond pas a 1/Q_HAT_IJT.")


def _compute_two_stage_weights(
    df: pl.DataFrame,
    cfg: PipelineConfig,
) -> tuple[np.ndarray, np.ndarray]:
    """Calcule R_ijt/(p_jt*q_ijt), puis applique le trimming configure."""
    scope = (
        df["DANS_UNIVERS_RISQUE"].fill_null(0).to_numpy() == 1
        if "DANS_UNIVERS_RISQUE" in df.columns
        else np.ones(df.height, dtype=bool)
    )
    if not scope.any():
        raise ValueError("Aucune ligne analytique dans l'univers a risque.")

    d = df["D_JT"].to_numpy().astype(float)[scope]
    s = df["S_IJT"].to_numpy().astype(float)[scope]
    p = df["P_HAT_JT"].cast(pl.Float64, strict=False).to_numpy()[scope]
    q = df["Q_HAT_IJT"].cast(pl.Float64, strict=False).to_numpy()[scope]
    p_safe = np.clip(p, cfg.modeling.propensity_clip, 1 - cfg.modeling.propensity_clip)
    q_safe = np.where(
        d == 1,
        np.clip(q, cfg.modeling.propensity_clip, 1 - cfg.modeling.propensity_clip),
        1.0,
    )
    response = d * s
    raw_scope = response / (p_safe * q_safe)
    trimmed_scope, _ = trim_positive_weights(
        raw_scope,
        lower_quantile=cfg.modeling.ipw_trim_lower,
        upper_quantile=cfg.modeling.ipw_trim_upper,
        max_trimmed_share=cfg.modeling.max_trimmed_share,
        label="Poids final a deux etages",
    )
    raw = np.zeros(df.height, dtype=float)
    trimmed = np.zeros(df.height, dtype=float)
    raw[scope] = raw_scope
    trimmed[scope] = trimmed_scope
    return raw, trimmed


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
    firm_object = f"{cfg.minio.cleaned_prefix}firm_base.parquet"

    for object_name, label in (
        (analytical_object, "base analytique"),
        (firm_object, "base entreprises"),
    ):
        if not object_exists(cfg.minio, bucket, object_name):
            raise FileNotFoundError(f"{label} introuvable : {bucket}/{object_name}")

    if cfg.modeling.estimation_method != "ipw":
        raise ValueError("Seule la methode IPW a deux etages est implementee.")

    analytical = read_parquet(cfg.minio, bucket, analytical_object)
    firm = read_parquet(cfg.minio, bucket, firm_object)
    logger.info(
        "Calcul IPW a deux etages sur {} lignes analytiques.",
        analytical.height,
    )
    df = _join_firm_weights(analytical, firm)
    validation_scope = (
        df.filter(pl.col("DANS_UNIVERS_RISQUE") == 1) if "DANS_UNIVERS_RISQUE" in df.columns else df
    )
    _validate_input_weights(validation_scope, cfg)

    raw_weights, final_weights = _compute_two_stage_weights(df, cfg)
    df = df.drop(["W_FINAL_RAW", "W_FINAL"], strict=False).with_columns(
        pl.Series("W_FINAL_RAW", raw_weights),
        pl.Series("W_FINAL", final_weights),
    )

    n_null = df["W_FINAL"].null_count()
    n_non_finite = int(
        df.select((~pl.col("W_FINAL").is_finite() & pl.col("W_FINAL").is_not_null()).sum()).item()
    )
    if n_null or n_non_finite:
        raise ValueError(
            f"W_FINAL contient {n_null} valeurs nulles et {n_non_finite} valeurs non finies."
        )

    scope = (
        pl.col("DANS_UNIVERS_RISQUE") == 1 if "DANS_UNIVERS_RISQUE" in df.columns else pl.lit(True)
    )
    response = scope & (pl.col("D_JT") == 1) & (pl.col("S_IJT") == 1)
    nonresponse_with_weight = df.filter(~response & (pl.col("W_FINAL") != 0)).height
    respondent_without_weight = df.filter(response & (pl.col("W_FINAL") <= 0)).height
    if nonresponse_with_weight or respondent_without_weight:
        raise ValueError(
            "Facteurs de reponse incoherents dans W_FINAL: "
            f"{nonresponse_with_weight} non-repondants avec poids positif, "
            f"{respondent_without_weight} repondants sans poids positif."
        )

    n_zero = int(df.select((pl.col("W_FINAL") == 0).sum()).item())
    logger.info(
        "Facteur R_ijt=D_JT*S_IJT applique : {} poids nuls ({:.2f}%). "
        "Aucune normalisation par periode n'est appliquee.",
        n_zero,
        100.0 * n_zero / df.height if df.height else 0.0,
    )

    write_parquet(cfg.minio, bucket, analytical_object, df)
    logger.info(
        "Poids finaux calcules : {} lignes -> {} | moyenne={:.3f}, ecart-type={:.3f}, plage=[{:.3f}, {:.3f}]",
        df.height,
        analytical_object,
        df["W_FINAL"].mean(),
        df["W_FINAL"].std(),
        df["W_FINAL"].min(),
        df["W_FINAL"].max(),
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
        level="DEBUG",
        rotation="10 MB",
        retention="30 days",
        encoding="utf-8",
    )

    try:
        calculer_poids_finaux(cfg)
        logger.info("Termine avec succes.")
    except Exception as exc:
        logger.exception("Echec de l'etape: {}", exc)
        sys.exit(1)
