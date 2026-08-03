"""Expressions temporelles partagees par les etapes de nettoyage et de panel."""

from __future__ import annotations

import polars as pl


def month_end_expr(year_col: str = "ANNEE", month_col: str = "MOIS") -> pl.Expr:
    """Construit le dernier jour civil du mois porte par deux colonnes."""
    valid = pl.col(year_col).is_between(1, 9999, closed="both") & pl.col(month_col).is_between(
        1, 12, closed="both"
    )
    return (
        pl.when(valid)
        .then(
            pl.date(
                pl.col(year_col).cast(pl.Int32),
                pl.col(month_col).cast(pl.Int32),
                pl.lit(1),
            ).dt.month_end()
        )
        .otherwise(pl.lit(None).cast(pl.Date))
    )


def completed_years_expr(date_col: str, reference_col: str) -> pl.Expr:
    """Calcule un nombre d'annees revolues, sans approximation en jours."""
    source = pl.col(date_col).cast(pl.Date, strict=False)
    reference = pl.col(reference_col).cast(pl.Date, strict=False)
    anniversary_not_reached = (
        (reference.dt.month() < source.dt.month())
        | ((reference.dt.month() == source.dt.month()) & (reference.dt.day() < source.dt.day()))
    ).cast(pl.Int32)
    years = reference.dt.year() - source.dt.year() - anniversary_not_reached
    return (
        pl.when(source.is_not_null() & reference.is_not_null() & (source <= reference))
        .then(years.cast(pl.Int32))
        .otherwise(pl.lit(None).cast(pl.Int32))
    )


def completed_months_expr(date_col: str, reference_col: str) -> pl.Expr:
    """Calcule un nombre de mois revolus entre deux dates."""
    source = pl.col(date_col).cast(pl.Date, strict=False)
    reference = pl.col(reference_col).cast(pl.Date, strict=False)
    day_not_reached = (reference.dt.day() < source.dt.day()).cast(pl.Int32)
    months = (
        (reference.dt.year() - source.dt.year()) * 12
        + reference.dt.month()
        - source.dt.month()
        - day_not_reached
    )
    return (
        pl.when(source.is_not_null() & reference.is_not_null() & (source <= reference))
        .then(months.cast(pl.Int32))
        .otherwise(pl.lit(None).cast(pl.Int32))
    )


def add_reference_date(df: pl.DataFrame) -> pl.DataFrame:
    """Ajoute DATE_REFERENCE a partir du couple ANNEE-MOIS."""
    required = {"ANNEE", "MOIS"}
    missing = sorted(required - set(df.columns))
    if missing:
        raise ValueError("Date de reference impossible: colonnes manquantes " + ", ".join(missing))
    return df.with_columns(month_end_expr().alias("DATE_REFERENCE"))
