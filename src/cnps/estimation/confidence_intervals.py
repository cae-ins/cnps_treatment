"""
Confidence intervals via Rubin's combination rules for multiple imputation.

When M imputed datasets are available, point estimates and variances are
combined using Rubin's rules (1987) to produce valid confidence intervals
that account for both within-imputation and between-imputation variability.

Rubin's rules
-------------
Let Q_m be the point estimate from imputation m, and U_m its variance.

1. Combined estimate:      Q_bar = (1/M) * sum(Q_m)
2. Within-imputation var:  U_bar = (1/M) * sum(U_m)
3. Between-imputation var: B = (1/(M-1)) * sum((Q_m - Q_bar)^2)
4. Total variance:         T = U_bar + (1 + 1/M) * B
5. Degrees of freedom:     df = (M-1) * (1 + U_bar / ((1+1/M)*B))^2
6. CI:                     Q_bar +/- t_{df, alpha/2} * sqrt(T)

References
----------
Rubin, D. B. (1987). *Multiple Imputation for Nonresponse in Surveys*.
    John Wiley & Sons.
Barnard, J. & Rubin, D. B. (1999). Miscellanea. Small-sample degrees of
    freedom with multiple imputation. *Biometrika*, 86(4), 948-955.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
from scipy import stats


@dataclass
class RubinResult:
    """Result of Rubin's combination rules."""
    estimate: float
    std_error: float
    ci_lower: float
    ci_upper: float
    df: float
    within_var: float
    between_var: float
    total_var: float
    n_imputations: int
    fmi: float            # fraction of missing information


def combine_rubin(
    estimates: list[float],
    variances: list[float],
    confidence_level: float = 0.95,
) -> RubinResult:
    """
    Apply Rubin's combination rules to M imputation estimates.

    Parameters
    ----------
    estimates : list[float]
        Point estimates Q_m from each imputation.
    variances : list[float]
        Variance estimates U_m from each imputation.
    confidence_level : float
        Confidence level for the interval (default 0.95).

    Returns
    -------
    RubinResult
        Combined estimate with confidence interval.
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

    # Combined estimate
    Q_bar = float(np.mean(Q))

    # Within-imputation variance
    U_bar = float(np.mean(U))

    if M == 1:
        # No between-imputation variance with single imputation
        T = U_bar
        df = np.inf
        B = 0.0
        fmi = 0.0
    else:
        # Between-imputation variance
        B = float(np.sum((Q - Q_bar) ** 2) / (M - 1))

        # Total variance
        T = U_bar + (1 + 1 / M) * B

        # Degrees of freedom (Barnard & Rubin, 1999)
        if B > 0:
            r = (1 + 1 / M) * B / U_bar if U_bar > 0 else np.inf
            df = (M - 1) * (1 + 1 / r) ** 2
        else:
            df = np.inf

        # Fraction of missing information
        fmi = float((B + B / M) / T) if T > 0 else 0.0

    # Confidence interval
    se = np.sqrt(max(T, 0))
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
