"""Tests d'acceptation hors ligne pour le nettoyage et la temporalite."""

from __future__ import annotations

import importlib
from dataclasses import replace
from datetime import date
from pathlib import Path

import polars as pl
import pytest

from cnps.config import load_config
from cnps.temporal import add_reference_date, completed_years_expr

ROOT = Path(__file__).resolve().parents[1]
CFG = load_config(ROOT / "config/settings.yaml", ROOT / "config/dimensions.yaml")
harmonisation = importlib.import_module("cnps.02_harmonisation_types")
cleaning = importlib.import_module("cnps.03_nettoyage_donnees")
firm_base = importlib.import_module("cnps.05_base_entreprises")
analytical_base = importlib.import_module("cnps.06_base_analytique")
individual_model = importlib.import_module("cnps.07b_modele_declaration_indiv")
firm_panel = importlib.import_module("cnps.firm_panel")


def test_numeric_parser_accepts_french_and_international_formats() -> None:
    source = pl.DataFrame(
        {
            "SALAIRE_BRUT": [
                "1.234,56",
                "1,234.56",
                "12 345,5",
                "7 500",
                "-1'200",
                "invalide",
            ]
        }
    )
    result = harmonisation._coerce_numeric(source, ["SALAIRE_BRUT"], 1.0)
    assert result["SALAIRE_BRUT"].to_list()[:5] == [
        1234.56,
        1234.56,
        12345.5,
        7500.0,
        -1200.0,
    ]
    assert result["SALAIRE_BRUT"][5] is None

    with pytest.raises(ValueError, match="Echec de parsing numerique"):
        harmonisation._coerce_numeric(source, ["SALAIRE_BRUT"], 0.10)


def test_incomplete_dedup_keys_are_kept_and_flagged(monkeypatch) -> None:
    source = pl.DataFrame(
        {
            "ID_INDIV": ["A", "A", None, None],
            "ID_EMPLOYEUR": ["E", "E", "E", "E"],
            "PERIOD": ["2024-01"] * 4,
            "ANNEE": [2024] * 4,
            "MOIS": [1] * 4,
            "SALAIRE_BRUT": [100_000.0, 120_000.0, 130_000.0, 140_000.0],
            "TYPE_SALARIE": ["M"] * 4,
        }
    )
    captured: dict[str, pl.DataFrame] = {}
    monkeypatch.setattr(cleaning, "list_objects", lambda *_a, **_k: ["x.parquet"])
    monkeypatch.setattr(cleaning, "read_parquet", lambda *_a, **_k: source)
    monkeypatch.setattr(
        cleaning,
        "write_parquet",
        lambda _cfg, _bucket, _object, frame: captured.setdefault("frame", frame),
    )

    cleaning.nettoyer_donnees(CFG)
    result = captured["frame"]
    assert result.height == 3
    assert result.filter(pl.col("ID_INDIV") == "A")["SALAIRE_BRUT"].item() == 120_000
    incomplete = result.filter(pl.col("ID_INDIV").is_null())
    assert incomplete.height == 2
    assert incomplete["CLE_DEDUP_INCOMPLETE"].to_list() == [1, 1]


def test_ages_use_the_declaration_month_not_execution_date() -> None:
    source = pl.DataFrame(
        {
            "ANNEE": [2024, 2024],
            "MOIS": [1, 3],
            "DATE_NAISSANCE": [date(2000, 2, 29), date(2000, 2, 29)],
        }
    )
    result = add_reference_date(source).with_columns(
        completed_years_expr("DATE_NAISSANCE", "DATE_REFERENCE").alias("AGE")
    )
    assert result["AGE"].to_list() == [23, 24]


def test_risk_window_uses_only_strictly_prior_months_and_asof_attributes() -> None:
    cfg = replace(CFG, modeling=replace(CFG.modeling, risk_window_months=2))
    observed = pl.DataFrame(
        {
            "ID_EMPLOYEUR": ["E", "E", "E", "E", "F"],
            "PERIOD": ["2024-01", "2024-02", "2024-03", "2024-04", "2024-05"],
            "ANNEE": [2024] * 5,
            "MOIS": [1, 2, 3, 4, 5],
            "EFFECTIF_DECLARE": [1, 0, 0, 1, 1],
            "SALAIRE_MOYEN": [100_000.0, None, None, 110_000.0, 120_000.0],
            "SECTEUR_ACTIVITE": ["A", None, "B", None, "C"],
        }
    )
    panel = firm_panel.construire_panel_risque(observed, cfg)
    e = panel.filter(pl.col("ID_EMPLOYEUR") == "E").sort("PERIOD")
    assert e["DANS_UNIVERS_RISQUE"].to_list() == [0, 1, 1, 0, 1]
    assert e.filter(pl.col("PERIOD") == "2024-02")["SECTEUR_ACTIVITE"].item() == "A"
    assert e.filter(pl.col("PERIOD") == "2024-03")["SECTEUR_ACTIVITE"].item() == "B"


def test_panel_keeps_declarations_anterior_to_the_registration_date() -> None:
    """Une declaration observee prouve l'existence: elle ne peut pas etre perdue."""
    cfg = replace(CFG, modeling=replace(CFG.modeling, risk_window_months=2))
    observed = pl.DataFrame(
        {
            "ID_EMPLOYEUR": ["E", "E", "E", "F", "F"],
            "PERIOD": ["2024-01", "2024-02", "2024-03", "2024-02", "2024-03"],
            "ANNEE": [2024] * 5,
            "MOIS": [1, 2, 3, 2, 3],
            "EFFECTIF_DECLARE": [1, 1, 1, 1, 1],
            "SALAIRE_MOYEN": [100_000.0] * 5,
            # E est immatriculee apres toutes ses declarations, F est coherente.
            "DATE_IMMAT_EMPLOYEUR": [date(2024, 6, 1)] * 3 + [date(2024, 2, 15)] * 2,
        }
    )
    panel = firm_panel.construire_panel_risque(observed, cfg)

    orphelins = observed.select(["ID_EMPLOYEUR", "PERIOD"]).join(
        panel.select(["ID_EMPLOYEUR", "PERIOD"]),
        on=["ID_EMPLOYEUR", "PERIOD"],
        how="anti",
    )
    assert orphelins.height == 0

    e = panel.filter(pl.col("ID_EMPLOYEUR") == "E")
    f = panel.filter(pl.col("ID_EMPLOYEUR") == "F")
    assert e["DECLARATION_AVANT_IMMAT"].unique().to_list() == [1]
    assert f["DECLARATION_AVANT_IMMAT"].unique().to_list() == [0]
    assert e["PERIOD"].min() == "2024-01"
    # F n'existe qu'a partir de fevrier: aucun mois fictif n'est cree en janvier.
    assert f["PERIOD"].min() == "2024-02"
    assert panel["DEBUT_ACTIVITE_IMPUTE"].sum() == 0
    assert panel["TRONCATURE_GAUCHE"].sum() == 0


def test_registration_date_is_read_beyond_the_first_observed_month() -> None:
    """Une date absente le premier mois ne doit pas faire imputer le debut."""
    cfg = replace(CFG, modeling=replace(CFG.modeling, risk_window_months=2))
    observed = pl.DataFrame(
        {
            "ID_EMPLOYEUR": ["E", "E", "G"],
            "PERIOD": ["2024-02", "2024-03", "2024-01"],
            "ANNEE": [2024] * 3,
            "MOIS": [2, 3, 1],
            "EFFECTIF_DECLARE": [1, 1, 1],
            "SALAIRE_MOYEN": [100_000.0] * 3,
            "DATE_IMMAT_EMPLOYEUR": [None, date(2024, 2, 10), date(2024, 1, 5)],
        }
    )
    panel = firm_panel.construire_panel_risque(observed, cfg)

    e = panel.filter(pl.col("ID_EMPLOYEUR") == "E")
    assert e["DEBUT_ACTIVITE_IMPUTE"].unique().to_list() == [0]
    assert e["DECLARATION_AVANT_IMMAT"].unique().to_list() == [0]
    assert e["PERIOD"].min() == "2024-02"


def test_extensible_window_is_measured_from_the_firm_entry() -> None:
    """Le flag d'amorce se mesure depuis DEBUT_INDEX, pas depuis le debut commun."""
    cfg = replace(CFG, modeling=replace(CFG.modeling, risk_window_months=3))
    observed = pl.DataFrame(
        {
            "ID_EMPLOYEUR": ["A", "H", "H", "H"],
            "PERIOD": ["2024-01", "2024-03", "2024-04", "2024-05"],
            "ANNEE": [2024] * 4,
            "MOIS": [1, 3, 4, 5],
            "EFFECTIF_DECLARE": [1, 1, 1, 1],
            "SALAIRE_MOYEN": [100_000.0] * 4,
            "DATE_IMMAT_EMPLOYEUR": [date(2024, 1, 5)] + [date(2024, 3, 1)] * 3,
        }
    )
    panel = firm_panel.construire_panel_risque(observed, cfg)

    h = panel.filter(pl.col("ID_EMPLOYEUR") == "H").sort("PERIOD")
    assert h["PERIOD"].to_list() == ["2024-03", "2024-04", "2024-05"]
    # H entre deux mois apres le debut commun: ses deux premiers mois restent
    # en fenetre extensible, le troisieme dispose de K=3 mois d'historique.
    assert h["FENETRE_RISQUE_EXTENSIBLE"].to_list() == [1, 1, 0]


def test_individual_history_respects_calendar_gaps_and_employer_pairs() -> None:
    source = pl.DataFrame(
        {
            "ID_INDIV": ["I", "I", "I", "I"],
            "ID_EMPLOYEUR": ["E1", "E1", "E2", "E2"],
            "ANNEE": [2024] * 4,
            "MOIS": [1, 3, 1, 2],
            "S_IJT": [1, 0, 0, 1],
        }
    )
    result = individual_model._ajouter_historique_individuel(source)
    e1_march = result.filter((pl.col("ID_EMPLOYEUR") == "E1") & (pl.col("MOIS") == 3)).row(
        0, named=True
    )
    e2_feb = result.filter((pl.col("ID_EMPLOYEUR") == "E2") & (pl.col("MOIS") == 2)).row(
        0, named=True
    )
    assert e1_march["S_IJT_LAG"] == 0
    assert e1_march["HISTORIQUE_MOIS_PRECEDENT_MANQUANT"] == 1.0
    assert e2_feb["S_IJT_LAG"] == 0
    assert e2_feb["HISTORIQUE_MOIS_PRECEDENT_MANQUANT"] == 0.0


def test_firm_context_is_one_calendar_month_lag_for_every_employee() -> None:
    firm = pl.DataFrame(
        {
            "ID_EMPLOYEUR": ["E", "E"],
            "PERIOD": ["2024-01", "2024-02"],
            "PERIOD_INDEX": [2024 * 12, 2024 * 12 + 1],
            "EFFECTIF_DECLARE": [2, 1],
            "EFFECTIF_OBSERVE": [4, 2],
            "PREMIER_MOIS_RISQUE": [0, 0],
        }
    )
    analytical = pl.DataFrame(
        {
            "ID_EMPLOYEUR": ["E", "E", "E", "E"],
            "ID_INDIV": ["A", "B", "A", "B"],
            "PERIOD": ["2024-01", "2024-01", "2024-02", "2024-02"],
        }
    )
    result = individual_model._ajouter_contexte_entreprise(analytical, firm)
    feb = result.filter(pl.col("PERIOD") == "2024-02")
    assert feb["TAUX_COMPLETUDE_ENTREPRISE"].to_list() == [0.5, 0.5]


def test_analytical_join_rejects_duplicate_firm_month_keys(monkeypatch) -> None:
    individual = pl.DataFrame(
        {
            "ID_INDIV": ["I"],
            "ID_EMPLOYEUR": ["E"],
            "PERIOD": ["2024-01"],
        }
    )
    firm = pl.DataFrame(
        {
            "ID_EMPLOYEUR": ["E", "E"],
            "PERIOD": ["2024-01", "2024-01"],
            "D_JT": [1, 1],
        }
    )
    monkeypatch.setattr(analytical_base, "object_exists", lambda *_a, **_k: True)
    monkeypatch.setattr(
        analytical_base,
        "read_parquet",
        lambda _cfg, _bucket, obj: individual if "individual" in obj else firm,
    )

    with pytest.raises(ValueError, match="cles dupliquees"):
        analytical_base.construire_base_analytique(CFG)


def test_analytical_join_preserves_individual_cardinality(monkeypatch) -> None:
    individual = pl.DataFrame(
        {
            "ID_INDIV": ["I1", "I2"],
            "ID_EMPLOYEUR": ["E", "E"],
            "PERIOD": ["2024-01", "2024-01"],
        }
    )
    firm = pl.DataFrame(
        {
            "ID_EMPLOYEUR": ["E"],
            "PERIOD": ["2024-01"],
            "D_JT": [1],
        }
    )
    captured: dict[str, pl.DataFrame] = {}
    monkeypatch.setattr(analytical_base, "object_exists", lambda *_a, **_k: True)
    monkeypatch.setattr(
        analytical_base,
        "read_parquet",
        lambda _cfg, _bucket, obj: individual if "individual" in obj else firm,
    )
    monkeypatch.setattr(
        analytical_base,
        "write_parquet",
        lambda _cfg, _bucket, _object, frame: captured.setdefault("frame", frame),
    )

    analytical_base.construire_base_analytique(CFG)
    assert captured["frame"].height == individual.height
    assert captured["frame"]["D_JT"].to_list() == [1, 1]


def test_asof_join_completes_nulls_without_overwriting_individual_values() -> None:
    individual = pl.DataFrame(
        {
            "ID_INDIV": ["I", "I"],
            "ID_EMPLOYEUR": ["E", "E"],
            "PERIOD": ["2024-01", "2024-02"],
            "SECTEUR_ACTIVITE": ["Industrie", None],
            "SALAIRE_BRUT": [100_000.0, 110_000.0],
        }
    )
    firm = pl.DataFrame(
        {
            "ID_EMPLOYEUR": ["E", "E"],
            "PERIOD": ["2024-01", "2024-02"],
            "SECTEUR_ACTIVITE": ["Commerce", "Commerce"],
            "SALAIRE_BRUT": [999_000.0, 999_000.0],
            "D_JT": [1, 1],
        }
    )

    result = analytical_base._joindre_attributs_entreprise(individual, firm).sort("PERIOD")

    assert result["SECTEUR_ACTIVITE"].to_list() == ["Industrie", "Commerce"]
    assert result["SALAIRE_BRUT"].to_list() == [100_000.0, 110_000.0]
    assert result["D_JT"].to_list() == [1, 1]
    assert "SECTEUR_ACTIVITE_FIRM" not in result.columns


def test_asof_join_never_uses_future_firm_information() -> None:
    observed = pl.DataFrame(
        {
            "ID_EMPLOYEUR": ["E", "E"],
            "PERIOD": ["2024-01", "2024-03"],
            "ANNEE": [2024, 2024],
            "MOIS": [1, 3],
            "EFFECTIF_DECLARE": [1, 1],
            "SALAIRE_MOYEN": [100_000.0, 110_000.0],
            "SECTEUR_ACTIVITE": [None, "Commerce"],
        }
    )
    firm = firm_panel.construire_panel_risque(observed, CFG)
    individual = pl.DataFrame(
        {
            "ID_INDIV": ["I", "I", "I"],
            "ID_EMPLOYEUR": ["E", "E", "E"],
            "PERIOD": ["2024-01", "2024-02", "2024-03"],
            "SECTEUR_ACTIVITE": [None, None, None],
        }
    )

    result = analytical_base._joindre_attributs_entreprise(individual, firm).sort("PERIOD")

    assert result["SECTEUR_ACTIVITE"].to_list() == [None, None, "Commerce"]
    assert result["JAMAIS_OBSERVE_AVANT_SECTEUR_ACTIVITE"].to_list() == [1, 1, 0]


def test_firm_attribute_aggregation_is_independent_of_row_order(monkeypatch) -> None:
    source = pl.DataFrame(
        {
            "ID_INDIV": ["I1", "I2", "I3", "I4"],
            "ID_EMPLOYEUR": ["E"] * 4,
            "PERIOD": ["2024-01"] * 4,
            "ANNEE": [2024] * 4,
            "MOIS": [1] * 4,
            "SALAIRE_BRUT": [100_000.0] * 4,
            "SECTEUR_ACTIVITE": ["Commerce", "Agriculture", "Commerce", "Agriculture"],
        }
    )
    inputs = iter([source, source.reverse()])
    outputs: list[pl.DataFrame] = []
    monkeypatch.setattr(firm_base, "object_exists", lambda *_a, **_k: True)
    monkeypatch.setattr(firm_base, "read_parquet", lambda *_a, **_k: next(inputs))
    monkeypatch.setattr(
        firm_base,
        "write_parquet",
        lambda _cfg, _bucket, _object, frame: outputs.append(frame),
    )

    firm_base.construire_base_entreprises(CFG)
    firm_base.construire_base_entreprises(CFG)

    assert [frame["SECTEUR_ACTIVITE"].item() for frame in outputs] == [
        "Agriculture",
        "Agriculture",
    ]
