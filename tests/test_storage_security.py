"""Tests des formats de stockage non executables."""

from __future__ import annotations

import json

import cnps.storage as storage


def test_json_writer_masks_nonfinite_values(monkeypatch) -> None:
    captured: dict[str, bytes] = {}

    def capture(_cfg, _bucket, _object, data, **_kwargs):
        captured["data"] = data

    monkeypatch.setattr(storage, "write_bytes", capture)
    storage.write_json(
        None,
        "bucket",
        "model.json",
        {"auc": 0.5, "calibration_slope": float("nan")},
    )
    decoded = json.loads(captured["data"].decode("utf-8"))
    assert decoded == {"auc": 0.5, "calibration_slope": None}


def test_storage_module_exposes_no_pickle_loader() -> None:
    assert not hasattr(storage, "read_pickle")
    assert not hasattr(storage, "write_pickle")
