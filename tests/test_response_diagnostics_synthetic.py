"""Tests synthetiques des garde-fous des modeles de reponse."""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from cnps.response_diagnostics import evaluate_oof_predictions


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
