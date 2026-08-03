"""
Etape 10/12 — Estimation des indicateurs.

Calcule toutes les statistiques configurees sur toutes les dimensions
d'analyse a partir des repondants et des poids IPW a deux etages.

Ce module regroupe deux responsabilites de publication :
1. Estimateurs ponderes (moyenne, variance, quantile, Gini) — utilises
   pour chaque cellule dimension x groupe.
2. Moteur d'estimation — orchestre le calcul sur toutes les dimensions
   et applique les regles de suppression des petites cellules.

Les helpers de Rubin restent disponibles pour les tests historiques, mais ne
sont pas appeles par le DAG. Aucune variance ni aucun intervalle n'est diffuse
tant que la linearisation conjointe des deux modeles de reponse (lot F.1)
n'est pas specifiee et validee.

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
    """Estimateur ratio de la moyenne ponderee de Hajek."""
    mask = np.isfinite(y) & np.isfinite(w) & (w > 0)
    if not mask.any():
        return np.nan
    return float(np.sum(w[mask] * y[mask]) / np.sum(w[mask]))


def weighted_variance(y: NDArray[np.float64], w: NDArray[np.float64]) -> float:
    """
    Variance descriptive ponderee des valeurs avec correction de Kish.

    Cette quantite decrit la dispersion des salaires. Elle n'est pas la
    variance de l'estimateur de moyenne, qui doit etre calculee separement.

    Formule des poids de fiabilite (Kish 1965) :
    V = (sum_w / (sum_w^2 - sum_w2)) * sum(w * (y - mu)^2)
    """
    mask = np.isfinite(y) & np.isfinite(w) & (w > 0)
    if mask.sum() < 2:
        return np.nan

    y_m, w_m = y[mask], w[mask]
    mu = np.sum(w_m * y_m) / np.sum(w_m)
    sum_w = np.sum(w_m)
    sum_w2 = np.sum(w_m**2)
    denom = sum_w**2 - sum_w2

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
    fmi: float  # fraction d'information manquante


def _has_valid_rubin_interval(
    result: RubinResult,
    *,
    declared_degenerate: bool = False,
) -> bool:
    """Indique si un intervalle de Rubin peut etre diffuse."""
    numeric_parts = (
        result.total_var,
        result.std_error,
        result.ci_lower,
        result.ci_upper,
    )
    if not all(np.isfinite(value) for value in numeric_parts):
        return False
    if (
        result.total_var < 0
        or result.std_error < 0
        or result.ci_lower < 0
        or result.ci_upper < 0
        or result.ci_upper < result.ci_lower
    ):
        return False

    zero_width = (
        result.total_var == 0 or result.std_error == 0 or result.ci_upper == result.ci_lower
    )
    return declared_degenerate or not zero_width


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
    if len(variances) != M:
        raise ValueError("Les listes d'estimations et de variances doivent avoir la meme longueur.")
    if not 0 < confidence_level < 1:
        raise ValueError("Le niveau de confiance doit etre strictement compris entre 0 et 1.")
    if M == 0:
        return RubinResult(
            estimate=np.nan,
            std_error=np.nan,
            ci_lower=np.nan,
            ci_upper=np.nan,
            df=np.nan,
            within_var=np.nan,
            between_var=np.nan,
            total_var=np.nan,
            n_imputations=0,
            fmi=np.nan,
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

    se = float(np.sqrt(T)) if np.isfinite(T) and T >= 0 else np.nan
    alpha = 1 - confidence_level

    if np.isfinite(df) and df > 0:
        t_crit = stats.t.ppf(1 - alpha / 2, df)
    else:
        t_crit = stats.norm.ppf(1 - alpha / 2)

    ci_lower = Q_bar - t_crit * se
    ci_upper = Q_bar + t_crit * se

    return RubinResult(
        estimate=Q_bar,
        std_error=se,
        ci_lower=ci_lower,
        ci_upper=ci_upper,
        df=df,
        within_var=U_bar,
        between_var=B,
        total_var=T,
        n_imputations=M,
        fmi=fmi,
    )


# ---------------------------------------------------------------------------
# 3. Moteur d'estimation
# ---------------------------------------------------------------------------


def _disclosure_status(
    df: pl.DataFrame,
    cfg: PipelineConfig,
    weight_col: str,
) -> tuple[str, float]:
    """Applique les seuils primaires sur les contributeurs reels."""
    required = {
        "ID_INDIV",
        "ID_EMPLOYEUR",
        "SALAIRE_BRUT_ESTIME_AU_MOIS",
        weight_col,
    }
    missing = sorted(required - set(df.columns))
    if missing:
        raise ValueError("Secret statistique impossible, colonnes absentes: " + ", ".join(missing))

    contributors = df.filter(
        pl.col(weight_col).is_finite()
        & (pl.col(weight_col) > 0)
        & pl.col("SALAIRE_BRUT_ESTIME_AU_MOIS").is_finite()
        & (pl.col("SALAIRE_BRUT_ESTIME_AU_MOIS") > 0)
    )
    individuals = contributors["ID_INDIV"].drop_nulls().n_unique()
    employers = contributors["ID_EMPLOYEUR"].drop_nulls().n_unique()
    wage_mass = (
        contributors.filter(pl.col("ID_EMPLOYEUR").is_not_null())
        .group_by("ID_EMPLOYEUR")
        .agg(pl.col("SALAIRE_BRUT_ESTIME_AU_MOIS").sum().alias("_MASSE"))
    )
    total_mass = float(wage_mass["_MASSE"].sum() or 0.0) if wage_mass.height else 0.0
    top_share = (
        float(wage_mass["_MASSE"].max()) / total_mass
        if total_mass > 0 and wage_mass.height
        else 1.0
    )

    reasons = []
    if individuals < cfg.estimation.min_distinct_individuals:
        reasons.append("individus")
    if employers < cfg.estimation.min_distinct_employers:
        reasons.append("employeurs")
    if top_share > cfg.estimation.max_employer_wage_share:
        reasons.append("dominance")
    status = "primaire:" + ",".join(reasons) if reasons else "publiee"
    return status, total_mass


def _mask_statistics(row: dict, statistics: list[StatDef]) -> None:
    for stat in statistics:
        row[stat.name] = None


def _apply_secondary_suppression(
    rows: list[dict],
    statistics: list[StatDef],
) -> None:
    """Masque une seconde cellule dans chaque marge additive concernee."""
    partitions: dict[str, list[dict]] = {}
    for row in rows:
        key = str(row.get("_secondary_partition", "__all__"))
        partitions.setdefault(key, []).append(row)

    for partition_rows in partitions.values():
        primary = [
            row for row in partition_rows if str(row["suppression_status"]).startswith("primaire:")
        ]
        candidates = [row for row in partition_rows if row["suppression_status"] == "publiee"]
        if len(primary) != 1 or not candidates:
            continue
        secondary = min(
            candidates,
            key=lambda row: (float(row["_wage_mass_observed"]), str(row["group"])),
        )
        _mask_statistics(secondary, statistics)
        secondary["suppression_status"] = "secondaire"


def _estimate_group(
    df: pl.DataFrame,
    weight_col: str,
    statistics: list[StatDef],
    cfg: PipelineConfig,
) -> dict[str, object]:
    """Calcule toutes les statistiques pour un groupe (une cellule)."""
    if weight_col not in df.columns:
        raise ValueError(f"Colonne {weight_col} absente: executer l'etape 09 avant l'estimation.")
    w = df[weight_col].to_numpy().astype(float)
    status, wage_mass = _disclosure_status(df, cfg, weight_col)
    results: dict[str, object] = {
        "suppression_status": status,
        "inference_status": "POINT_ONLY_F1_PENDING",
        "_wage_mass_observed": wage_mass,
    }
    if status != "publiee":
        _mask_statistics(results, statistics)
        return results

    for stat in statistics:
        if stat.variable not in df.columns:
            raise ValueError(
                f"Variable '{stat.variable}' absente pour la statistique '{stat.name}'."
            )
        y = df[stat.variable].to_numpy().astype(float)
        results[stat.name] = compute_statistic(stat.function, y, w, stat.params)

    return results


def _estimate_dimension(
    df: pl.DataFrame,
    dim: DimensionDef,
    weight_col: str,
    statistics: list[StatDef],
    cfg: PipelineConfig,
) -> list[dict]:
    """Calcule les statistiques pour tous les groupes d'une dimension."""
    rows: list[dict] = []

    if not dim.group_by:
        results = _estimate_group(df, weight_col, statistics, cfg)
        rows.append({"dimension": dim.label, "group": "Total", **results})
        return rows

    group_cols = [c for c in dim.group_by if c in df.columns]
    if not group_cols:
        logger.warning("Dimension '{}': colonnes {} introuvables", dim.name, dim.group_by)
        return rows

    for group_vals, group_df in df.group_by(group_cols):
        group_label = (
            " / ".join(str(v) for v in group_vals)
            if isinstance(group_vals, tuple)
            else str(group_vals)
        )
        partition_values = (
            group_vals[:-1] if isinstance(group_vals, tuple) and len(group_cols) > 1 else ()
        )
        partition = " / ".join(str(v) for v in partition_values) or "__all__"
        results = _estimate_group(group_df, weight_col, statistics, cfg)
        rows.append(
            {
                "dimension": dim.label,
                "group": group_label,
                "_secondary_partition": partition,
                **results,
            }
        )

    _apply_secondary_suppression(rows, statistics)
    return rows


def estimer_indicateurs(cfg: PipelineConfig) -> pl.DataFrame:
    """
    Calcule l'ensemble des statistiques sur toutes les dimensions d'analyse.

    Le chemin de publication utilise uniquement les repondants ponderes par
    IPW a deux etages. L'imputation et les regles de Rubin en sont exclues.
    Tant que F.1 n'est pas valide, seules les estimations ponctuelles sortent.

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
    weight_col = "W_FINAL"
    statistics = cfg.statistics
    enabled_dims = [d for d in cfg.dimensions if d.enabled]

    logger.info(
        "Secret statistique: au moins {} individus et {} employeurs; "
        "part salariale maximale d'un employeur {:.1%}.",
        cfg.estimation.min_distinct_individuals,
        cfg.estimation.min_distinct_employers,
        cfg.estimation.max_employer_wage_share,
    )

    if not object_exists(cfg.minio, bucket, analytical_object):
        raise FileNotFoundError(f"Base analytique introuvable : {bucket}/{analytical_object}")

    df = read_parquet(cfg.minio, bucket, analytical_object)
    required_variables = {weight_col, *(stat.variable for stat in statistics)}
    missing = sorted(required_variables - set(df.columns))
    if missing:
        raise ValueError("Colonnes d'estimation absentes: " + ", ".join(missing))

    logger.info(
        "Estimation ponctuelle de {} statistiques sur {} dimensions ({} lignes).",
        len(statistics),
        len(enabled_dims),
        df.height,
    )

    all_rows: list[dict] = []
    n_suppressed = 0
    for dim in enabled_dims:
        rows = _estimate_dimension(df, dim, weight_col, statistics, cfg)
        all_rows.extend(rows)
        n_suppressed += sum(1 for r in rows if all(r.get(s.name) is None for s in statistics))
        logger.debug("  {} '{}': {} groupes", dim.name, dim.label, len(rows))

    for row in all_rows:
        row.pop("_wage_mass_observed", None)
        row.pop("_secondary_partition", None)
    result = pl.DataFrame(all_rows)
    logger.info(
        "Estimation terminee : {} lignes, {} colonnes, {} cellules masquees. "
        "Aucun intervalle n'est diffuse (F.1 en attente).",
        result.height,
        result.width,
        n_suppressed,
    )
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
        level="DEBUG",
        rotation="10 MB",
        retention="30 days",
        encoding="utf-8",
    )

    try:
        estimer_indicateurs(cfg)
        logger.info("Termine avec succes.")
    except Exception as exc:
        logger.exception("Echec de l'etape: {}", exc)
        sys.exit(1)
