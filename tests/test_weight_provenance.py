"""Tests des garde-fous de provenance des poids de l'etape 09."""

from __future__ import annotations

import importlib

import numpy as np
import polars as pl
import pytest

weighting = importlib.import_module("cnps.09_ponderation_finale")


def test_weight_columns_are_mandatory_and_name_remediation_stage() -> None:
    with pytest.raises(ValueError, match="07_modele_declaration.py"):
        weighting._validate_input_weights(pl.DataFrame({"W_JT": [1.0]}))


@pytest.mark.parametrize("p_hat", [0.0, 1.0, None, np.nan, np.inf])
def test_propensity_must_be_strictly_between_zero_and_one(
    p_hat: float | None,
) -> None:
    df = pl.DataFrame({"P_HAT_JT": [p_hat], "W_JT": [1.0]})
    with pytest.raises(ValueError, match="P_HAT_JT"):
        weighting._validate_input_weights(df)


@pytest.mark.parametrize("weight", [0.0, -1.0, None, np.nan, np.inf])
def test_weight_must_be_positive_and_finite(weight: float | None) -> None:
    df = pl.DataFrame({"P_HAT_JT": [0.5], "W_JT": [weight]})
    with pytest.raises(ValueError, match="W_JT"):
        weighting._validate_input_weights(df)


def test_constant_unit_weights_are_allowed_for_mcar() -> None:
    df = pl.DataFrame(
        {
            "P_HAT_JT": [0.5, 0.5, 0.5],
            "W_JT": [1.0, 1.0, 1.0],
        }
    )
    weighting._validate_input_weights(df)
