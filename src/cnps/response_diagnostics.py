"""Diagnostics communs aux deux modeles de reponse."""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import pandas as pd
import polars as pl
from loguru import logger
from sklearn.base import clone
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import brier_score_loss, roc_auc_score
from sklearn.model_selection import StratifiedGroupKFold


@dataclass(frozen=True)
class ResponseDiagnostics:
    """Metriques hors echantillon d'un modele de reponse."""

    auc: float
    calibration_slope: float
    calibration_in_large: float
    brier: float
    max_abs_smd: float
    propensity_min: float
    propensity_max: float
    n_splits: int


def grouped_oof_predictions(
    model,
    X: pd.DataFrame,
    y: np.ndarray,
    groups: np.ndarray,
    *,
    n_splits: int,
    random_seed: int,
) -> tuple[np.ndarray, int]:
    """Produit des predictions OOF par validation croisee groupee."""
    classes = np.unique(y)
    if classes.size != 2:
        raise ValueError(f"Classe cible unique ({classes.tolist()}): modele de reponse impossible.")
    if not np.isfinite(y).all():
        raise ValueError("La cible du modele de reponse contient des valeurs non finies.")

    unique_groups = np.unique(groups)
    groups_per_class = [np.unique(groups[y == class_value]).size for class_value in classes]
    effective_splits = min(n_splits, unique_groups.size, *groups_per_class)
    if effective_splits < 2:
        raise ValueError(
            "Validation croisee groupee impossible: moins de deux groupes "
            "disponibles dans au moins une classe cible."
        )

    splitter = StratifiedGroupKFold(
        n_splits=effective_splits,
        shuffle=True,
        random_state=random_seed,
    )
    predictions = np.full(y.shape[0], np.nan, dtype=float)
    for train_idx, test_idx in splitter.split(X, y, groups):
        if np.unique(y[train_idx]).size != 2:
            raise ValueError(
                "Un pli d'apprentissage ne contient qu'une classe cible; "
                "reduire n_cv_splits ou enrichir le jeu d'apprentissage."
            )
        fold_model = clone(model)
        fold_model.fit(X.iloc[train_idx], y[train_idx])
        predictions[test_idx] = fold_model.predict_proba(X.iloc[test_idx])[:, 1]

    if not np.isfinite(predictions).all():
        raise ValueError("Predictions OOF non finies ou incompletes.")
    if ((predictions <= 0) | (predictions >= 1)).any():
        raise ValueError("Predictions OOF hors de l'intervalle ouvert ]0, 1[.")
    return predictions, effective_splits


def _calibration_slope(y: np.ndarray, p: np.ndarray, clip: float) -> float:
    """Estime la pente non penalisee de y sur logit(p)."""
    p_safe = np.clip(p, clip, 1 - clip)
    logit_p = np.log(p_safe / (1 - p_safe))
    # Un score quasi constant est le comportement attendu sous MCAR. Sa pente
    # est numeriquement instable et ne doit pas servir de critere bloquant.
    if float(np.std(logit_p)) < 0.10:
        return np.nan
    calibration = LogisticRegression(
        penalty=None,
        solver="lbfgs",
        max_iter=1000,
    )
    calibration.fit(logit_p.reshape(-1, 1), y)
    return float(calibration.coef_[0, 0])


def _maximum_balance_smd(
    X: pd.DataFrame,
    y: np.ndarray,
    p: np.ndarray,
    clip: float,
) -> float:
    """Compare la population complete aux repondants reponderes."""
    encoded = pd.get_dummies(X, dummy_na=True, dtype=float)
    if encoded.shape[1] == 0:
        return 0.0
    matrix = encoded.to_numpy(dtype=float)
    if not np.isfinite(matrix).all():
        raise ValueError("Covariables non finies dans le diagnostic d'equilibre.")

    response = y == 1
    weights = np.where(response, 1.0 / np.clip(p, clip, 1 - clip), 0.0)
    if weights.sum() <= 0:
        raise ValueError("Aucun repondant pondere pour le diagnostic d'equilibre.")

    full_mean = matrix.mean(axis=0)
    weighted_mean = np.average(matrix[response], axis=0, weights=weights[response])
    scale = matrix.std(axis=0, ddof=0)
    informative = scale > 1e-12
    if not informative.any():
        return 0.0
    smd = np.abs((weighted_mean[informative] - full_mean[informative]) / scale[informative])
    return float(np.max(smd))


def evaluate_oof_predictions(
    X: pd.DataFrame,
    y: np.ndarray,
    p_oof: np.ndarray,
    *,
    clip: float,
    calibration_slope_range: tuple[float, float],
    max_calibration_in_large: float,
    max_abs_smd: float,
    n_splits: int,
    label: str,
) -> ResponseDiagnostics:
    """Calcule les diagnostics OOF et bloque les echecs methodologiques."""
    p0 = p_oof[y == 0]
    p1 = p_oof[y == 1]
    no_overlap = (p0.max() < p1.min()) or (p1.max() < p0.min())
    if no_overlap:
        raise ValueError(
            f"Absence de recouvrement des propensions OOF pour {label}: "
            f"support D=0 [{p0.min():.6g}, {p0.max():.6g}], "
            f"support D=1 [{p1.min():.6g}, {p1.max():.6g}]."
        )

    auc = float(roc_auc_score(y, p_oof))
    slope = _calibration_slope(y, p_oof, clip)
    observed_rate = float(np.clip(np.mean(y), clip, 1 - clip))
    predicted_rate = float(np.clip(np.mean(p_oof), clip, 1 - clip))
    calibration_in_large = float(
        np.log(observed_rate / (1 - observed_rate)) - np.log(predicted_rate / (1 - predicted_rate))
    )
    brier = float(brier_score_loss(y, p_oof))
    balance = _maximum_balance_smd(X, y, p_oof, clip)

    if abs(calibration_in_large) > max_calibration_in_large:
        raise ValueError(
            f"Calibration-in-the-large grossierement fausse pour {label}: "
            f"{calibration_in_large:.4f}, tolerance "
            f"{max_calibration_in_large:.4f}."
        )
    slope_min, slope_max = calibration_slope_range
    if np.isfinite(slope) and not slope_min <= slope <= slope_max:
        raise ValueError(
            f"Pente de calibration OOF hors plage pour {label}: "
            f"{slope:.4f}, plage [{slope_min:.4f}, {slope_max:.4f}]."
        )
    if balance > max_abs_smd:
        raise ValueError(
            f"Desequilibre residuel apres ponderation pour {label}: "
            f"SMD max={balance:.4f}, seuil={max_abs_smd:.4f}."
        )

    logger.info(
        "{} OOF : AUC={:.4f} (descriptive), pente={}, calibration-large={:.4f}, "
        "Brier={:.4f}, SMD max={:.4f}, propensions=[{:.6g}, {:.6g}], {} plis.",
        label,
        auc,
        f"{slope:.4f}" if np.isfinite(slope) else "non identifiable (score constant)",
        calibration_in_large,
        brier,
        balance,
        float(p_oof.min()),
        float(p_oof.max()),
        n_splits,
    )
    return ResponseDiagnostics(
        auc=auc,
        calibration_slope=slope,
        calibration_in_large=calibration_in_large,
        brier=brier,
        max_abs_smd=balance,
        propensity_min=float(p_oof.min()),
        propensity_max=float(p_oof.max()),
        n_splits=n_splits,
    )


def inverse_propensity_weights(
    probabilities: np.ndarray,
    *,
    clip: float,
    max_clipped_share: float,
    label: str,
) -> tuple[np.ndarray, float]:
    """Calcule 1/p et rend explicite la part affectee par le clipping."""
    if probabilities.size == 0 or not np.isfinite(probabilities).all():
        raise ValueError(f"Propensions non finies pour {label}.")
    clipped_mask = (probabilities < clip) | (probabilities > 1 - clip)
    clipped_share = float(np.mean(clipped_mask))
    logger.info(
        "{} : propension min={:.6g}, max={:.6g}, part clippee={:.4%}.",
        label,
        float(probabilities.min()),
        float(probabilities.max()),
        clipped_share,
    )
    if clipped_share > max_clipped_share:
        raise ValueError(
            f"Part de propensions clippees trop elevee pour {label}: "
            f"{clipped_share:.4%}, seuil={max_clipped_share:.4%}."
        )
    return 1.0 / np.clip(probabilities, clip, 1 - clip), clipped_share


def trim_positive_weights(
    weights: np.ndarray,
    *,
    lower_quantile: float,
    upper_quantile: float,
    max_trimmed_share: float,
    label: str,
) -> tuple[np.ndarray, dict[str, float]]:
    """Tronque les poids positifs et bloque une troncature trop frequente."""
    positive = weights > 0
    positive_weights = weights[positive]
    if positive_weights.size == 0:
        raise ValueError(f"Aucun poids positif pour {label}.")
    lo = float(np.quantile(positive_weights, lower_quantile))
    hi = float(np.quantile(positive_weights, upper_quantile))
    trimmed_mask = positive & ((weights < lo) | (weights > hi))
    trimmed_share = float(trimmed_mask.sum() / positive_weights.size)
    logger.info(
        "{} : bornes de trimming [{:.6g}, {:.6g}], part tronquee={:.4%}.",
        label,
        lo,
        hi,
        trimmed_share,
    )
    if trimmed_share > max_trimmed_share:
        raise ValueError(
            f"Part de poids tronques trop elevee pour {label}: "
            f"{trimmed_share:.4%}, seuil={max_trimmed_share:.4%}."
        )
    result = weights.copy()
    result[positive] = np.clip(positive_weights, lo, hi)
    return result, {"lower": lo, "upper": hi, "share": trimmed_share}


def reject_never_responding_strata(
    df: pl.DataFrame,
    *,
    target: str,
    categorical_features: list[str],
    min_size: int,
    label: str,
) -> None:
    """Refuse les strates categorielles assez grandes sans aucun repondant."""
    features = [name for name in categorical_features if name in df.columns]
    if not features:
        return
    strata = (
        df.group_by(features)
        .agg(
            pl.len().alias("_N"),
            pl.col(target).sum().alias("_REPONSES"),
        )
        .filter((pl.col("_N") >= min_size) & (pl.col("_REPONSES") == 0))
    )
    if strata.height == 0:
        return
    examples = strata.head(5).to_dicts()
    raise ValueError(
        f"Violation structurelle de positivite pour {label}: "
        f"{strata.height} strate(s) sans repondant, exemples={examples}."
    )
