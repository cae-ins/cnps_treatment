"""Construction pure du panel entreprise et des covariables disponibles a date."""

from __future__ import annotations

from calendar import monthrange
from datetime import date

import polars as pl
from loguru import logger

from cnps.config import PipelineConfig
from cnps.temporal import completed_months_expr, completed_years_expr

_ASOF_ATTRIBUTES = (
    "DATE_IMMAT_EMPLOYEUR",
    "SECTEUR_ACTIVITE",
    "COMMUNE",
    "CLASSE_EFFECTIF",
    "CLASSE_EFFECTIF_REDUITE",
)


def _period_index(year: int, month: int) -> int:
    return year * 12 + month - 1


def _complete_period_table(observed: pl.DataFrame) -> pl.DataFrame:
    """Construit tous les mois civils compris entre les bornes observees."""
    required = {"PERIOD", "ANNEE", "MOIS"}
    missing = sorted(required - set(observed.columns))
    if missing:
        raise ValueError("Panel entreprise impossible: colonnes manquantes " + ", ".join(missing))

    periods = observed.select("PERIOD", "ANNEE", "MOIS").unique().sort(["ANNEE", "MOIS"])
    if periods.height == 0:
        raise ValueError("Panel entreprise impossible: aucune periode observee.")

    first = periods.row(0, named=True)
    last = periods.row(-1, named=True)
    start_index = _period_index(int(first["ANNEE"]), int(first["MOIS"]))
    end_index = _period_index(int(last["ANNEE"]), int(last["MOIS"]))

    rows = []
    for index in range(start_index, end_index + 1):
        year, month_zero = divmod(index, 12)
        month = month_zero + 1
        rows.append(
            {
                "PERIOD": f"{year:04d}-{month:02d}",
                "ANNEE": year,
                "MOIS": month,
                "PERIOD_INDEX": index,
                "DATE_REFERENCE": date(year, month, monthrange(year, month)[1]),
            }
        )
    return pl.DataFrame(rows)


def _firm_age_class_expr(cfg: PipelineConfig) -> pl.Expr:
    dimension = next((d for d in cfg.dimensions if d.name == "firm_age"), None)
    if dimension is None or not dimension.classes:
        raise ValueError("Classes firm_age absentes de config/dimensions.yaml.")

    expr = pl.when(pl.col("AGE_ENTREPRISE_IMMAT").is_null()).then(pl.lit(None).cast(pl.Utf8))
    for class_def in dimension.classes:
        expr = expr.when(
            pl.col("AGE_ENTREPRISE_IMMAT").is_between(
                class_def["min"], class_def["max"], closed="both"
            )
        ).then(pl.lit(class_def["label"]))
    return expr.otherwise(pl.lit(None).cast(pl.Utf8))


def construire_panel_risque(
    observed: pl.DataFrame,
    cfg: PipelineConfig,
) -> pl.DataFrame:
    """Etend le panel jusqu'a sa fin et marque la portee glissante de K mois."""
    if "ID_EMPLOYEUR" not in observed.columns:
        raise ValueError("ID_EMPLOYEUR absent de la table entreprise-periode.")
    if observed.select(["ID_EMPLOYEUR", "PERIOD"]).n_unique() != observed.height:
        raise ValueError("La cle (ID_EMPLOYEUR, PERIOD) doit etre unique avant expansion.")

    periods = _complete_period_table(observed)
    panel_start = int(periods["PERIOD_INDEX"].min())
    panel_end = int(periods["PERIOD_INDEX"].max())

    observed = observed.join(
        periods.select("PERIOD", "PERIOD_INDEX", "DATE_REFERENCE"),
        on="PERIOD",
        how="left",
    ).with_columns(pl.lit(1).cast(pl.Int8).alias("_LIGNE_SOURCE"))

    first_rows = (
        observed.sort(["ID_EMPLOYEUR", "PERIOD_INDEX"])
        .group_by("ID_EMPLOYEUR", maintain_order=True)
        .agg(
            pl.col("PERIOD_INDEX").first().alias("PREMIERE_APPARITION_INDEX"),
            # drop_nulls: la date peut manquer sur le premier mois observe et etre
            # renseignee ensuite; la ignorer imputerait un debut d'activite a tort.
            pl.col("DATE_IMMAT_EMPLOYEUR").drop_nulls().first().alias("_IMMAT_PREMIERE")
            if "DATE_IMMAT_EMPLOYEUR" in observed.columns
            else pl.lit(None).cast(pl.Date).alias("_IMMAT_PREMIERE"),
        )
        .with_columns(
            pl.when(pl.col("_IMMAT_PREMIERE").is_not_null())
            .then(
                pl.col("_IMMAT_PREMIERE").dt.year() * 12 + pl.col("_IMMAT_PREMIERE").dt.month() - 1
            )
            .otherwise(pl.col("PREMIERE_APPARITION_INDEX"))
            .cast(pl.Int32)
            .alias("_IMMAT_INDEX"),
            pl.col("_IMMAT_PREMIERE").is_null().cast(pl.Int8).alias("DEBUT_ACTIVITE_IMPUTE"),
        )
        .with_columns(
            # Une declaration observee prouve l'existence de l'entreprise: elle ne
            # peut pas etre effacee par une immatriculation qui la dit posterieure.
            pl.min_horizontal("_IMMAT_INDEX", "PREMIERE_APPARITION_INDEX").alias(
                "_DEBUT_INDEX_BRUT"
            ),
            (pl.col("_IMMAT_INDEX") > pl.col("PREMIERE_APPARITION_INDEX"))
            .cast(pl.Int8)
            .alias("DECLARATION_AVANT_IMMAT"),
        )
        .with_columns(
            pl.col("_DEBUT_INDEX_BRUT").clip(panel_start, panel_end).alias("DEBUT_INDEX"),
            (pl.col("_DEBUT_INDEX_BRUT") < panel_start).cast(pl.Int8).alias("TRONCATURE_GAUCHE"),
        )
    )

    firms = first_rows.select(
        "ID_EMPLOYEUR",
        "DEBUT_INDEX",
        "DEBUT_ACTIVITE_IMPUTE",
        "TRONCATURE_GAUCHE",
        "DECLARATION_AVANT_IMMAT",
    )
    panel = (
        firms.join(periods, how="cross")
        .filter(pl.col("PERIOD_INDEX") >= pl.col("DEBUT_INDEX"))
        .join(
            observed.drop(
                ["ANNEE", "MOIS", "PERIOD_INDEX", "DATE_REFERENCE"],
                strict=False,
            ),
            on=["ID_EMPLOYEUR", "PERIOD"],
            how="left",
        )
        .sort(["ID_EMPLOYEUR", "PERIOD_INDEX"])
    )

    # Le squelette est borne a gauche AVANT d'accrocher l'observe: toute ligne
    # observee anterieure a DEBUT_INDEX serait perdue sans bruit. L'invariant
    # doit casser ici, a l'etape 05, et non trois etapes plus loin.
    n_source = int(panel["_LIGNE_SOURCE"].sum())
    if n_source != observed.height:
        raise ValueError(
            f"Le panel ne couvre pas toutes les lignes observees: "
            f"{observed.height - n_source} couples (ID_EMPLOYEUR, PERIOD) perdus "
            f"a l'expansion sur {observed.height}."
        )

    if "EFFECTIF_DECLARE" not in panel.columns:
        raise ValueError("EFFECTIF_DECLARE absent: D_JT ne peut pas etre defini de facon fiable.")
    panel = panel.with_columns(
        (pl.col("EFFECTIF_DECLARE").fill_null(0) > 0).cast(pl.Int8).alias("D_JT")
    )
    impossible = panel.filter(
        (pl.col("D_JT") == 1)
        & (
            pl.col("SALAIRE_MOYEN").is_null()
            | ~pl.col("SALAIRE_MOYEN").is_finite()
            | (pl.col("SALAIRE_MOYEN") <= 0)
        )
    ).height
    if impossible:
        raise ValueError(
            f"Etat impossible: {impossible} lignes D_JT=1 avec salaire moyen invalide."
        )

    k = cfg.modeling.risk_window_months
    # La portee du mois t depend uniquement des declarations anterieures.
    # Inclure D_JT courant ferait dependre l'univers de la cible a predire.
    past_response = pl.col("D_JT").shift(1).over("ID_EMPLOYEUR").fill_null(0)
    if k is None:
        recent_response = past_response.cum_sum().over("ID_EMPLOYEUR") > 0
        extensible = pl.lit(0)
        k_label = "inf"
    else:
        recent_response = (
            past_response.rolling_sum(window_size=k, min_samples=1).over("ID_EMPLOYEUR") > 0
        )
        # Mesure depuis l'entree de l'entreprise dans le panel, pas depuis le
        # debut commun: une firme entrant tard a bien un historique tronque.
        extensible = pl.col("PERIOD_INDEX") - pl.col("DEBUT_INDEX") + 1 < k
        k_label = str(k)

    panel = panel.with_columns(
        recent_response.cast(pl.Int8).alias("DANS_UNIVERS_RISQUE"),
        extensible.cast(pl.Int8).alias("FENETRE_RISQUE_EXTENSIBLE"),
    )

    panel = panel.with_columns(
        (
            (pl.col("DANS_UNIVERS_RISQUE") == 1)
            & (pl.col("DANS_UNIVERS_RISQUE").cum_sum().over("ID_EMPLOYEUR") == 1)
        )
        .cast(pl.Int8)
        .alias("PREMIER_MOIS_RISQUE")
    )
    for attr in _ASOF_ATTRIBUTES:
        if attr not in panel.columns:
            continue
        panel = panel.with_columns(
            pl.col(attr).forward_fill().over("ID_EMPLOYEUR").alias(attr)
        ).with_columns(pl.col(attr).is_null().cast(pl.Int8).alias(f"JAMAIS_OBSERVE_AVANT_{attr}"))

    if "DATE_IMMAT_EMPLOYEUR" in panel.columns:
        panel = panel.with_columns(
            completed_months_expr("DATE_IMMAT_EMPLOYEUR", "DATE_REFERENCE").alias(
                "AGE_ENTREPRISE_MOIS"
            ),
            completed_years_expr("DATE_IMMAT_EMPLOYEUR", "DATE_REFERENCE").alias(
                "AGE_ENTREPRISE_IMMAT"
            ),
        ).with_columns(_firm_age_class_expr(cfg).alias("CL_AGE_ENTREPRISE"))

    n_left = int(first_rows["TRONCATURE_GAUCHE"].sum())
    n_imputed = int(first_rows["DEBUT_ACTIVITE_IMPUTE"].sum())
    n_outside = int(panel.filter(pl.col("DANS_UNIVERS_RISQUE") == 0).height)
    logger.info(
        "Panel a risque K={} : {} entreprises, {} lignes jusqu'a la fin commune, "
        "{} lignes hors portee glissante.",
        k_label,
        firms.height,
        panel.height,
        n_outside,
    )
    n_avant_immat = int(first_rows["DECLARATION_AVANT_IMMAT"].sum())
    logger.info(
        "Bornes du panel : {} troncatures gauches, {} debuts imputes sur la "
        "premiere apparition, 0 cessations observees, {} fins censurees.",
        n_left,
        n_imputed,
        firms.height,
    )
    logger.info(
        "Borne gauche : {} entreprises declarent avant leur date d'immatriculation; "
        "le panel demarre a la premiere declaration observee pour celles-ci.",
        n_avant_immat,
    )
    if k is not None:
        n_extensible = int(panel["FENETRE_RISQUE_EXTENSIBLE"].sum())
        logger.info(
            "Amorce du panel : {} lignes utilisent une fenetre extensible (< {} mois).",
            n_extensible,
            k,
        )

    return panel.drop(["_LIGNE_SOURCE", "_IMMAT_PREMIERE"], strict=False)
