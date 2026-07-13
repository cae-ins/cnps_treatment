"""
Weighted statistical estimators.

Implements numerically stable, weighted versions of standard descriptive
statistics for use with survey/IPW weights.  All estimators handle missing
values gracefully and are vectorised via NumPy.

Estimators
----------
- **Weighted mean**: Horvitz-Thompson estimator
  mu_w = sum(w_i * y_i) / sum(w_i)

- **Weighted variance**: Bessel-corrected weighted variance
  sigma2_w = [sum(w_i) / (sum(w_i)^2 - sum(w_i^2))] * sum(w_i * (y_i - mu_w)^2)

- **Weighted quantiles**: Linear interpolation on the weighted CDF
  F_w(y) = sum(w_i * I(y_i <= y)) / sum(w_i)

- **Gini coefficient**: Weighted Gini via covariance formula
  G = (2 / (mu * sum(w))) * sum(w_i * y_i * (F_w(y_i) - 0.5))

References
----------
Horvitz, D. G. & Thompson, D. J. (1952). A generalization of sampling
    without replacement from a finite universe. *JASA*, 47(260), 663-685.
Kish, L. (1965). *Survey Sampling*. Wiley.
Lerman, R. I. & Yitzhaki, S. (1989). Improving the accuracy of estimates
    of Gini coefficients. *Journal of Econometrics*, 42(1), 43-47.
Heeringa, S. G., West, B. T. & Berglund, P. A. (2017). *Applied Survey
    Data Analysis* (2nd ed.). Chapman & Hall/CRC.
"""

from __future__ import annotations

import numpy as np
from numpy.typing import NDArray


def weighted_mean(
    y: NDArray[np.float64],
    w: NDArray[np.float64],
) -> float:
    """
    Horvitz-Thompson weighted mean estimator.

    Parameters
    ----------
    y : array
        Outcome values.
    w : array
        Non-negative weights.

    Returns
    -------
    float
        Weighted mean, or NaN if all weights are zero.
    """
    mask = np.isfinite(y) & np.isfinite(w) & (w > 0)
    if not mask.any():
        return np.nan
    return float(np.sum(w[mask] * y[mask]) / np.sum(w[mask]))


def weighted_variance(
    y: NDArray[np.float64],
    w: NDArray[np.float64],
) -> float:
    """
    Bessel-corrected weighted variance.

    Uses the reliability weights formula (Kish, 1965):
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


def weighted_quantile(
    y: NDArray[np.float64],
    w: NDArray[np.float64],
    q: float,
) -> float:
    """
    Weighted quantile via linear interpolation on the weighted CDF.

    Implements the Type 7 quantile definition (same as NumPy default)
    adapted for weighted data.

    Parameters
    ----------
    q : float
        Quantile in [0, 1].
    """
    mask = np.isfinite(y) & np.isfinite(w) & (w > 0)
    if not mask.any():
        return np.nan

    y_m, w_m = y[mask], w[mask]
    order = np.argsort(y_m)
    y_sorted = y_m[order]
    w_sorted = w_m[order]

    # Cumulative weight (normalised to [0, 1])
    cum_w = np.cumsum(w_sorted)
    cum_w_norm = (cum_w - 0.5 * w_sorted) / cum_w[-1]

    # Interpolate
    return float(np.interp(q, cum_w_norm, y_sorted))


def weighted_gini(
    y: NDArray[np.float64],
    w: NDArray[np.float64],
) -> float:
    """
    Weighted Gini coefficient using the covariance formula.

    G = (2 * cov(y, F(y))) / mu

    where F(y) is the weighted cumulative distribution (Lerman & Yitzhaki, 1989).
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

    # Rank (midpoint of cumulative weight)
    F = (cum_w - 0.5 * w_sorted) / total_w

    # Gini = 2 * weighted_cov(y, F) / mu
    cov_yF = np.sum(w_sorted * (y_sorted - mu) * (F - 0.5)) / total_w
    gini = 2.0 * cov_yF / mu

    return float(np.clip(gini, 0.0, 1.0))


def weighted_count(w: NDArray[np.float64]) -> float:
    """Sum of weights (effective sample size)."""
    mask = np.isfinite(w) & (w > 0)
    return float(np.sum(w[mask])) if mask.any() else 0.0


def compute_statistic(
    name: str,
    y: NDArray[np.float64],
    w: NDArray[np.float64],
    params: dict | None = None,
) -> float:
    """
    Dispatch to the appropriate weighted estimator.

    Parameters
    ----------
    name : str
        Estimator function name (matches ``statistics[].function`` in config).
    y : array
        Outcome values.
    w : array
        Weights.
    params : dict, optional
        Additional parameters (e.g., ``{"q": 0.25}`` for quantiles).

    Returns
    -------
    float
        Computed statistic.
    """
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
            raise ValueError(f"Unknown statistic: {name}")
