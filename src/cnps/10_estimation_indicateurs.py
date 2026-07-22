"""
Etape 10/12 — Estimation des indicateurs.

Calcule toutes les statistiques configurees sur toutes les dimensions
d'analyse, en combinant si necessaire les resultats des imputations
multiples (etape 8) via les regles de combinaison de Rubin.

Ce module regroupe trois responsabilites :
1. Estimateurs ponderes (moyenne, variance, quantile, Gini) — utilises
   pour chaque cellule dimension x groupe.
2. Combinaison de Rubin — fusionne les M estimations issues des jeux
   de donnees imputes en un estimateur unique avec intervalle de confiance.
3. Moteur d'estimation — orchestre le calcul sur toutes les dimensions
   et applique les regles de suppression des petites cellules.

References
----------
Horvitz, D. G. & Thompson, D. J. (1952). A generalization of sampling
    without replacement from a finite universe. *JASA*, 47(260), 663-685.
Kish, L. (1965). *Survey Sampling*. Wiley.
Lerman, R. I. & Yitzhaki, S. (1989). Improving the accuracy of estimates
    of Gini coefficients. *Journal of Econometrics*, 42(1), 43-47.
Rubin, D. B. (1987). *Multiple Imputation for Nonresponse in Surveys*.
    John Wiley & Sons.
Barnard, J. & Rubin, D. B. (1999). Miscellanea. Small-sample degrees of
    freedom with multiple imputation. *Biometrika*, 86(4), 948-955.
Lumley, T. (2010). *Complex Surveys: A Guide to Analysis Using R*. Wiley.
"""

from __future__ import annotations

from collections import defaultdict
from dataclasses import dataclass

import numpy as np
import polars as pl
from loguru import logger
from numpy.typing import NDArray
from scipy import stats

from cnps.config import DimensionDef, PipelineConfig, StatDef, load_config
from cnps.storage import object_exists, read_parquet

# ---------------------------------------------------------------------------
# 1. Estimateurs statistiques ponderes
# ---------------------------------------------------------------------------


def weighted_mean(y: NDArray[np.float64], w: NDArray[np.float64]) -> float:
    """Estimateur de la moyenne ponderee de Horvitz-Thompson."""
    mask = np.isfinite(y) & np.isfinite(w) & (w > 0)
    if not mask.any():
        return np.nan
    return float(np.sum(w[mask] * y[mask]) / np.sum(w[mask]))


def weighted_variance(y: NDArray[np.float64], w: NDArray[np.float64]) -> float:
    """
    Variance ponderee avec correction de Bessel (formule des poids de
    fiabilite, Kish 1965) :
    V = (sum_w / (sum_w^2 - sum_w2)) * sum(w * (y - mu)^2)
    """
    mask = np.isfinite(y) & np.isfinite(w) & (w > 0)
    if mask.sum() < 2:
        return np.nan

    y_m, w_m = y[mask], w[mask]
    mu = np.sum(w_m * y_m) / np.sum(w_m)
    sum_w = np.sum(w_m)
    sum_w2 = np.sum(w_m ** 2)
    denom = sum_w ** 2 - sum_w2

    if denom <= 0:
        return np.nan

    return float(sum_w / denom * np.sum(w_m * (y_m - mu) ** 2))


def weighted_quantile(y: NDArray[np.float64], w: NDArray[np.float64], q: float) -> float:
    """Quantile pondere par interpolation lineaire sur la CDF ponderee."""
    mask = np.isfinite(y) & np.isfinite(w) & (w > 0)
    if not mask.any():
        return np.nan

    y_m, w_m = y[mask], w[mask]
    order = np.argsort(y_m)
    y_sorted = y_m[order]
    w_sorted = w_m[order]

    cum_w = np.cumsum(w_sorted)
    cum_w_norm = (cum_w - 0.5 * w_sorted) / cum_w[-1]

    return float(np.interp(q, cum_w_norm, y_sorted))


def weighted_gini(y: NDArray[np.float64], w: NDArray[np.float64]) -> float:
    """
    Coefficient de Gini pondere via la formule de covariance
    (Lerman & Yitzhaki, 1989) : G = (2 * cov(y, F(y))) / mu
    """
    mask = np.isfinite(y) & np.isfinite(w) & (w > 0) & (y >= 0)
    if mask.sum() < 2:
        return np.nan

    y_m, w_m = y[mask], w[mask]
    order = np.argsort(y_m)
    y_sorted = y_m[order]
    w_sorted = w_m[order]

    cum_w = np.cumsum(w_sorted)
    total_w = cum_w[-1]
    mu = np.sum(w_sorted * y_sorted) / total_w

    if mu <= 0:
        return np.nan

    F = (cum_w - 0.5 * w_sorted) / total_w
    cov_yF = np.sum(w_sorted * (y_sorted - mu) * (F - 0.5)) / total_w
    gini = 2.0 * cov_yF / mu

    return float(np.clip(gini, 0.0, 1.0))


def weighted_count(w: NDArray[np.float64]) -> float:
    """Somme des poids (effectif pondere)."""
    mask = np.isfinite(w) & (w > 0)
    return float(np.sum(w[mask])) if mask.any() else 0.0


def compute_statistic(
    name: str,
    y: NDArray[np.float64],
    w: NDArray[np.float64],
    params: dict | None = None,
) -> float:
    """Repartit vers l'estimateur pondere approprie selon son nom."""
    params = params or {}
    match name:
        case "count":
            return float(np.sum(np.isfinite(y) & (w > 0)))
        case "weighted_count":
            return weighted_count(w)
        case "weighted_mean":
            return weighted_mean(y, w)
        case "weighted_variance":
            return weighted_variance(y, w)
        case "min":
            mask = np.isfinite(y) & (w > 0)
            return float(np.min(y[mask])) if mask.any() else np.nan
        case "max":
            mask = np.isfinite(y) & (w > 0)
            return float(np.max(y[mask])) if mask.any() else np.nan
        case "weighted_quantile":
            return weighted_quantile(y, w, q=params.get("q", 0.5))
        case "gini":
            return weighted_gini(y, w)
        case _:
            raise ValueError(f"Statistique inconnue : {name}")


# ---------------------------------------------------------------------------
# 2. Combinaison de Rubin (imputation multiple)
# ---------------------------------------------------------------------------


@dataclass
class RubinResult:
    """Resultat de la combinaison de Rubin."""
    estimate: float
    std_error: float
    ci_lower: float
    ci_upper: float
    df: float
    within_var: float
    between_var: float
    total_var: float
    n_imputations: int
    fmi: float            # fraction d'information manquante


def combine_rubin(
    estimates: list[float],
    variances: list[float],
    confidence_level: float = 0.95,
) -> RubinResult:
    """
    Applique les regles de combinaison de Rubin a M estimations d'imputation.

    Parameters
    ----------
    estimates : list[float]
        Estimations ponctuelles Q_m de chaque imputation.
    variances : list[float]
        Variances U_m de chaque imputation.
    confidence_level : float
        Niveau de confiance de l'intervalle (0.95 par defaut).
    """
    M = len(estimates)
    if M == 0:
        return RubinResult(
            estimate=np.nan, std_error=np.nan,
            ci_lower=np.nan, ci_upper=np.nan,
            df=np.nan, within_var=np.nan, between_var=np.nan,
            total_var=np.nan, n_imputations=0, fmi=np.nan,
        )

    Q = np.array(estimates)
    U = np.array(variances)

    Q_bar = float(np.mean(Q))
    U_bar = float(np.mean(U))

    if M == 1:
        T = U_bar
        df = np.inf
        B = 0.0
        fmi = 0.0
    else:
        B = float(np.sum((Q - Q_bar) ** 2) / (M - 1))
        T = U_bar + (1 + 1 / M) * B

        if B > 0:
            r = (1 + 1 / M) * B / U_bar if U_bar > 0 else np.inf
            df = (M - 1) * (1 + 1 / r) ** 2
        else:
            df = np.inf

        fmi = float((B + B / M) / T) if T > 0 else 0.0

    se = np.sqrt(max(T, 0))
    alpha = 1 - confidence_level

    if np.isfinite(df) and df > 0:
        t_crit = stats.t.ppf(1 - alpha / 2, df)
    else:
        t_crit = stats.norm.ppf(1 - alpha / 2)

    ci_lower = Q_bar - t_crit * se
    ci_upper = Q_bar + t_crit * se

    return RubinResult(
        estimate=Q_bar, std_error=se, ci_lower=ci_lower, ci_upper=ci_upper,
        df=df, within_var=U_bar, between_var=B, total_var=T,
        n_imputations=M, fmi=fmi,
    )


# ---------------------------------------------------------------------------
# 3. Moteur d'estimation
# ---------------------------------------------------------------------------


def _estimate_group(
    df: pl.DataFrame,
    salary_col: str,
    weight_col: str,
    statistics: list[StatDef],
    min_cell: int,
) -> dict[str, float | None]:
    """Calcule toutes les statistiques pour un groupe (une cellule)."""
    y = df[salary_col].to_numpy().astype(float)
    w = df[weight_col].to_numpy().astype(float)

    n_weighted = float(np.sum(w[np.isfinite(y) & (w > 0)]))

    # Suppression si sous la taille minimale de cellule
    if n_weighted < min_cell:
        return {s.name: None for s in statistics}

    results: dict[str, float | None] = {}
    for stat in statistics:
        try:
            results[stat.name] = compute_statistic(stat.function, y, w, stat.params)
        except Exception:
            results[stat.name] = None

    return results


def _estimate_dimension(
    df: pl.DataFrame,
    dim: DimensionDef,
    salary_col: str,
    weight_col: str,
    statistics: list[StatDef],
    min_cell: int,
) -> list[dict]:
    """Calcule les statistiques pour tous les groupes d'une dimension."""
    rows: list[dict] = []

    if not dim.group_by:
        results = _estimate_group(df, salary_col, weight_col, statistics, min_cell)
        rows.append({"dimension": dim.label, "group": "Total", **results})
        return rows

    group_cols = [c for c in dim.group_by if c in df.columns]
    if not group_cols:
        logger.warning("Dimension '{}': colonnes {} introuvables", dim.name, dim.group_by)
        return rows

    for group_vals, group_df in df.group_by(group_cols):
        group_label = " / ".join(str(v) for v in group_vals) if isinstance(group_vals, tuple) \
            else str(group_vals)
        results = _estimate_group(group_df, salary_col, weight_col, statistics, min_cell)
        rows.append({"dimension": dim.label, "group": group_label, **results})

    return rows


def _estimate_with_imputations(
    cfg: PipelineConfig,
    imputed_object: str,
    salary_col: str,
    weight_col: str,
    min_cell: int,
    statistics: list[StatDef],
    enabled_dims: list[DimensionDef],
) -> pl.DataFrame:
    """Estime avec combinaison de Rubin sur les imputations multiples."""
    imputed = read_parquet(cfg.minio, cfg.minio.cleaned_bucket, imputed_object)

    if "IMPUTATION_ID" not in imputed.columns:
        logger.warning("Aucun IMPUTATION_ID trouve, traite comme imputation unique")
        imputed = imputed.with_columns(pl.lit(1).cast(pl.Int32).alias("IMPUTATION_ID"))

    m_values = sorted(imputed["IMPUTATION_ID"].unique().to_list())
    M = len(m_values)
    logger.info("Combinaison de {} imputations via les regles de Rubin", M)

    imputation_results: list[list[dict]] = []

    for m in m_values:
        df_m = imputed.filter(pl.col("IMPUTATION_ID") == m)

        s_col = salary_col if salary_col in df_m.columns else "SALAIRE_MOYEN"
        if s_col not in df_m.columns:
            continue

        w_col = weight_col if weight_col in df_m.columns else "W_JT"
        if w_col not in df_m.columns:
            df_m = df_m.with_columns(pl.lit(1.0).alias(w_col))

        rows_m: list[dict] = []
        for dim in enabled_dims:
            rows_m.extend(_estimate_dimension(df_m, dim, s_col, w_col, statistics, min_cell))

        imputation_results.append(rows_m)

    if not imputation_results:
        return pl.DataFrame()

    # Regroupe par (dimension, groupe) -> {nom_stat: [valeurs par imputation]}
    combined: dict[tuple[str, str], dict[str, list[float]]] = defaultdict(
        lambda: defaultdict(list)
    )
    for rows_m in imputation_results:
        for row in rows_m:
            key = (row["dimension"], row["group"])
            for stat in statistics:
                val = row.get(stat.name)
                if val is not None:
                    combined[key][stat.name].append(val)

    final_rows: list[dict] = []
    n_rubin = 0
    n_fallback_mean = 0
    for (dim_label, group), stat_values in combined.items():
        row = {"dimension": dim_label, "group": group}
        for stat in statistics:
            vals = stat_values.get(stat.name, [])
            if len(vals) == M:
                # Regles de Rubin (variance intra-imputation simplifiee a 0 : pas de bootstrap intra)
                variances = [0.0] * len(vals)
                rubin = combine_rubin(vals, variances, cfg.estimation.confidence_level)
                row[stat.name] = rubin.estimate
                row[f"{stat.name}_ci_lower"] = rubin.ci_lower
                row[f"{stat.name}_ci_upper"] = rubin.ci_upper
                n_rubin += 1
            elif vals:
                # Certaines imputations n'ont pas produit cette statistique
                # (cellule supprimee pour n_weighted < min_cell dans au moins
                # une imputation) : repli sur une simple moyenne, sans IC.
                row[stat.name] = float(np.mean(vals))
                n_fallback_mean += 1
            else:
                row[stat.name] = None
        final_rows.append(row)

    if n_fallback_mean > 0:
        logger.warning(
            "{} statistiques combinees par moyenne simple (pas de {} imputations completes, "
            "pas d'intervalle de confiance) sur {} au total",
            n_fallback_mean, M, n_rubin + n_fallback_mean,
        )

    result = pl.DataFrame(final_rows)
    logger.info("Estimation par Rubin terminee : {} lignes, {} colonnes ({} groupes distincts)",
                result.height, result.width, len(combined))
    return result


def estimer_indicateurs(cfg: PipelineConfig) -> pl.DataFrame:
    """
    Calcule l'ensemble des statistiques sur toutes les dimensions d'analyse.

    Si des imputations multiples existent (etape 8), les estimations sont
    combinees via les regles de Rubin. Sinon, l'estimation est faite
    directement sur la base analytique.

    Parameters
    ----------
    cfg : PipelineConfig
        Configuration du pipeline.

    Returns
    -------
    pl.DataFrame
        Resultats avec colonnes : dimension, group, + une colonne par statistique.
    """
    bucket = cfg.minio.cleaned_bucket
    analytical_object = f"{cfg.minio.cleaned_prefix}analytical_base.parquet"
    imputed_object = f"{cfg.minio.cleaned_prefix}firm_base_imputed.parquet"

    salary_col = "SALAIRE_BRUT_MENS"
    weight_col = "W_FINAL"
    min_cell = cfg.estimation.min_cell_size
    statistics = cfg.statistics
    enabled_dims = [d for d in cfg.dimensions if d.enabled]

    logger.info("Taille minimale de cellule (secret statistique) : {}", min_cell)

    if object_exists(cfg.minio, bucket, imputed_object):
        logger.info("Imputations multiples detectees, utilisation des regles de Rubin")
        return _estimate_with_imputations(
            cfg, imputed_object, salary_col, weight_col, min_cell, statistics, enabled_dims,
        )

    if not object_exists(cfg.minio, bucket, analytical_object):
        raise FileNotFoundError(f"Base analytique introuvable : {bucket}/{analytical_object}")

    df = read_parquet(cfg.minio, bucket, analytical_object)

    if salary_col not in df.columns:
        fallback = "SALAIRE_BRUT" if "SALAIRE_BRUT" in df.columns else None
        if fallback is None:
            raise ValueError("Aucune colonne de salaire trouvee dans la base analytique")
        logger.info("Colonne '{}' absente, repli sur '{}'", salary_col, fallback)
        salary_col = fallback

    logger.info("Estimation de {} statistiques sur {} dimensions ({} lignes)",
                len(statistics), len(enabled_dims), df.height)

    all_rows: list[dict] = []
    n_suppressed = 0
    for dim in enabled_dims:
        rows = _estimate_dimension(df, dim, salary_col, weight_col, statistics, min_cell)
        all_rows.extend(rows)
        n_suppressed += sum(1 for r in rows if all(r.get(s.name) is None for s in statistics))
        logger.debug("  {} '{}': {} groupes", dim.name, dim.label, len(rows))

    result = pl.DataFrame(all_rows)
    logger.info("Estimation terminee : {} lignes, {} colonnes ({} cellules supprimees pour n < {})",
                result.height, result.width, n_suppressed, min_cell)
    return result


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
        estimer_indicateurs(cfg)
        logger.info("Termine avec succes.")
    except Exception as exc:
        logger.exception("Echec de l'etape: {}", exc)
        sys.exit(1)
