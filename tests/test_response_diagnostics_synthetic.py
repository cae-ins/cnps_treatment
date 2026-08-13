"""Tests synthetiques des garde-fous des modeles de reponse."""

from __future__ import annotations

import numpy as np
import pandas as pd
import polars as pl
import pytest

from cnps.response_diagnostics import evaluate_oof_predictions, reject_never_responding_strata


def test_mcar_constant_score_is_valid_and_auc_is_descriptive() -> None:
    y = np.array([0, 1] * 50)
    p = np.full(y.shape, 0.5)
    diagnostics = evaluate_oof_predictions(
        pd.DataFrame({"constant": np.zeros(y.size)}),
        y,
        p,
        clip=1e-6,
        calibration_slope_range=(0.8, 1.2),
        max_calibration_in_large=0.10,
        max_abs_smd=0.25,
        n_splits=5,
        label="MCAR synthetique",
    )
    assert diagnostics.auc == 0.5
    assert np.isnan(diagnostics.calibration_slope)
    assert diagnostics.calibration_in_large == 0.0


def test_complete_propensity_separation_is_blocking() -> None:
    y = np.array([0, 0, 1, 1])
    p = np.array([0.1, 0.2, 0.8, 0.9])
    with pytest.raises(ValueError, match="Absence de recouvrement"):
        evaluate_oof_predictions(
            pd.DataFrame({"x": [0.0, 0.0, 1.0, 1.0]}),
            y,
            p,
            clip=1e-6,
            calibration_slope_range=(0.8, 1.2),
            max_calibration_in_large=0.10,
            max_abs_smd=0.25,
            n_splits=2,
            label="separation synthetique",
        )


def test_joint_empty_cell_is_diagnostic_when_margins_have_respondents() -> None:
    frame = pl.DataFrame(
        {
            "A": ["x"] * 3 + ["x"] * 3 + ["y"] * 3 + ["y"] * 3,
            "B": ["u"] * 3 + ["v"] * 3 + ["u"] * 3 + ["v"] * 3,
            "R": [0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1],
        }
    )
    diagnostics = reject_never_responding_strata(
        frame,
        target="R",
        categorical_features=["A", "B"],
        min_size=3,
        label="modele additif",
    )
    assert diagnostics["marginal_violations"] == 0
    assert diagnostics["joint_zero_response_cells"] == 1


def test_marginal_level_without_respondent_is_blocking() -> None:
    frame = pl.DataFrame(
        {
            "A": ["x"] * 4 + ["y"] * 4,
            "B": ["u", "v"] * 4,
            "R": [0, 0, 0, 0, 1, 1, 1, 1],
        }
    )
    with pytest.raises(ValueError, match="modalite.*marginale"):
        reject_never_responding_strata(
            frame,
            target="R",
            categorical_features=["A", "B"],
            min_size=3,
            label="modele additif",
        )
