"""Tests unitaires des estimateurs ponderes de l'etape 10."""

from __future__ import annotations

import importlib
import math

import numpy as np
import pytest

estimation = importlib.import_module("cnps.10_estimation_indicateurs")

combine_rubin = estimation.combine_rubin
weighted_gini = estimation.weighted_gini
weighted_mean = estimation.weighted_mean
weighted_quantile = estimation.weighted_quantile
weighted_variance = estimation.weighted_variance


def _array(values: list[float]) -> np.ndarray:
    return np.asarray(values, dtype=np.float64)


def test_weighted_mean_hajek_reference_calculee_a_la_main() -> None:
    # (1*10 + 2*20 + 1*40) / (1+2+1) = 90/4 = 22,5.
    result = weighted_mean(_array([10, 20, 40]), _array([1, 2, 1]))
    assert result == pytest.approx(22.5)


def test_weighted_mean_cas_degeneres() -> None:
    assert math.isnan(weighted_mean(_array([10, 20]), _array([0, 0])))
    assert weighted_mean(_array([7]), _array([3])) == pytest.approx(7)
    assert weighted_mean(_array([5, 5, 5]), _array([1, 2, 9])) == pytest.approx(5)
    # Le poids infini est exclu: (10 + 30) / 2 = 20.
    assert weighted_mean(_array([10, 20, 30]), _array([1, np.inf, 1])) == pytest.approx(20)


def test_weighted_variance_kish_reference_calculee_a_la_main() -> None:
    # mu=2,25; somme w*(y-mu)^2=4,75; somme w=4; somme w^2=6.
    # V = 4/(4^2-6) * 4,75 = 4/10 * 4,75 = 1,9.
    result = weighted_variance(_array([1, 2, 4]), _array([1, 2, 1]))
    assert result == pytest.approx(1.9)


def test_weighted_variance_est_descriptive_et_non_variance_estimateur() -> None:
    # Cette fonction mesure la dispersion descriptive des salaires dans la
    # cellule. Elle ne calcule PAS la variance de l'estimateur de moyenne.
    # Deux valeurs identiques ont donc une variance descriptive nulle.
    result = weighted_variance(_array([100, 100]), _array([1, 3]))
    assert result == pytest.approx(0)


def test_weighted_variance_cas_degeneres() -> None:
    assert math.isnan(weighted_variance(_array([1, 4]), _array([0, 0])))
    assert math.isnan(weighted_variance(_array([7]), _array([3])))
    assert weighted_variance(_array([5, 5, 5]), _array([1, 2, 9])) == pytest.approx(0)
    # Le poids infini est exclu. Pour [1,4] a poids egaux, la variance
    # descriptive corrigee de Kish vaut la variance echantillonnale: 4,5.
    result = weighted_variance(_array([1, 2, 4]), _array([1, np.inf, 1]))
    assert result == pytest.approx(4.5)


def test_weighted_quantile_cdf_centree_reference_calculee_a_la_main() -> None:
    # Pour y=[0,10,20], w=[1,2,1], les positions centrees sont
    # [0,125; 0,5; 0,875]. A q=0,25:
    # 0 + (0,25-0,125)/(0,5-0,125) * (10-0) = 10/3.
    result = weighted_quantile(_array([0, 10, 20]), _array([1, 2, 1]), 0.25)
    assert result == pytest.approx(10 / 3)


def test_weighted_quantile_cas_degeneres() -> None:
    assert math.isnan(weighted_quantile(_array([1, 4]), _array([0, 0]), 0.5))
    assert weighted_quantile(_array([7]), _array([3]), 0.1) == pytest.approx(7)
    assert weighted_quantile(_array([5, 5, 5]), _array([1, 2, 9]), 0.75) == pytest.approx(5)
    # Le poids infini du milieu est exclu; interpolation entre 0 et 20.
    result = weighted_quantile(_array([0, 10, 20]), _array([1, np.inf, 1]), 0.5)
    assert result == pytest.approx(10)


def test_weighted_gini_lerman_yitzhaki_reference_calculee_a_la_main() -> None:
    # mu=10 et F=[0,125; 0,5; 0,875].
    # cov_w(y,F) = (3,75 + 0 + 3,75)/4 = 1,875.
    # G = 2*1,875/10 = 0,375.
    result = weighted_gini(_array([0, 10, 20]), _array([1, 2, 1]))
    assert result == pytest.approx(0.375)


def test_weighted_gini_cas_degeneres() -> None:
    assert math.isnan(weighted_gini(_array([1, 4]), _array([0, 0])))
    assert math.isnan(weighted_gini(_array([7]), _array([3])))
    assert weighted_gini(_array([5, 5, 5]), _array([1, 2, 9])) == pytest.approx(0)
    # Le poids infini est exclu; pour [0,20] a poids egaux, G=0,5.
    result = weighted_gini(_array([0, 10, 20]), _array([1, np.inf, 1]))
    assert result == pytest.approx(0.5)


def test_combine_rubin_reference_calculee_a_la_main() -> None:
    # Qbar=12, Ubar=4, B=((10-12)^2+(12-12)^2+(14-12)^2)/2=4.
    # T=4+(1+1/3)*4=28/3; se=sqrt(28/3); r=4/3.
    # nu=(3-1)*(1+1/r)^2=6,125. Avec t_0,975=2,434857823,
    # l'IC vaut [4,561386479; 19,438613521].
    result = combine_rubin([10, 12, 14], [4, 4, 4], 0.95)
    assert result.estimate == pytest.approx(12)
    assert result.within_var == pytest.approx(4)
    assert result.between_var == pytest.approx(4)
    assert result.total_var == pytest.approx(28 / 3)
    assert result.std_error == pytest.approx(math.sqrt(28 / 3))
    assert result.df == pytest.approx(6.125)
    assert result.ci_lower == pytest.approx(4.561386479219918)
    assert result.ci_upper == pytest.approx(19.43861352078008)
    assert result.fmi == pytest.approx(4 / 7)


def test_combine_rubin_imputation_unique() -> None:
    # M=1: T=U=4, se=2, nu=inf et quantile normal 1,9599639845.
    result = combine_rubin([3], [4])
    assert result.estimate == pytest.approx(3)
    assert result.total_var == pytest.approx(4)
    assert result.std_error == pytest.approx(2)
    assert math.isinf(result.df)
    assert result.ci_lower == pytest.approx(-0.919927969080108)
    assert result.ci_upper == pytest.approx(6.919927969080108)


def test_combine_rubin_estimations_identiques_garde_variance_positive() -> None:
    # Des Q_m identiques n'annulent pas un U_m positif: T=4 et l'IC reste valide.
    result = combine_rubin([10, 10, 10], [4, 4, 4])
    assert result.between_var == pytest.approx(0)
    assert result.total_var == pytest.approx(4)
    assert result.std_error == pytest.approx(2)
    assert result.ci_lower < result.estimate < result.ci_upper
    assert estimation._has_valid_rubin_interval(result)


def test_combine_rubin_cas_degeneres() -> None:
    empty = combine_rubin([], [])
    assert empty.n_imputations == 0
    assert math.isnan(empty.estimate)

    non_finite = combine_rubin([10, np.nan], [1, 1])
    assert math.isnan(non_finite.total_var)
    assert math.isnan(non_finite.std_error)
    assert not estimation._has_valid_rubin_interval(non_finite)

    negative_total = combine_rubin([10, 10], [-1, -1])
    assert negative_total.total_var == pytest.approx(-1)
    assert math.isnan(negative_total.std_error)
    assert not estimation._has_valid_rubin_interval(negative_total)

    zero_width = combine_rubin([10, 10], [0, 0])
    assert zero_width.total_var == pytest.approx(0)
    assert not estimation._has_valid_rubin_interval(zero_width)
    assert estimation._has_valid_rubin_interval(
        zero_width,
        declared_degenerate=True,
    )


def test_combine_rubin_refuse_entrees_incoherentes() -> None:
    with pytest.raises(ValueError, match="meme longueur"):
        combine_rubin([1, 2], [1])
    with pytest.raises(ValueError, match="strictement compris"):
        combine_rubin([1], [1], confidence_level=1)
