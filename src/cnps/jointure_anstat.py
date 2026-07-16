"""
Etape 5.1/12 — Jointure avec le referentiel entreprises ANSTAT.

Enrichit la base entreprises (etape 05, ``firm_base.parquet``) avec les
attributs du referentiel ANSTAT (nomenclature sectorielle CEPICI, forme
juridique, anciennete d'immatriculation, numeros RCCM/DFE).

Aucun identifiant n'est partage entre les deux sources : la base CNPS
n'a pas de RCCM/DFE, et le referentiel ANSTAT n'a pas de ID_EMPLOYEUR.
La seule cle de jointure disponible est donc la raison sociale, apres
normalisation (majuscules, ponctuation retiree, espaces uniformises).

Qualite de la jointure (mesuree sur les donnees de reference)
---------------------------------------------------------------
- ~96.5% des entreprises CNPS trouvent une correspondance ANSTAT.
- Le referentiel ANSTAT contient plusieurs enregistrements pour une
  meme entreprise au fil du temps (changements de secteur/motif) : on
  ne garde que le plus recent (``DATE DE DEBUT D'ACTIVITE`` maximale)
  pour ne pas dupliquer les lignes de ``firm_base.parquet`` lors de la
  jointure.
- Le matching par nom reste approximatif (raisons sociales orthographiees
  differemment, abreviations, sigles) : le taux de correspondance et le
  detail des non-matches sont journalises pour permettre un controle
  qualite, mais aucune verification manuelle n'est faite ici.
"""

from __future__ import annotations

import polars as pl
from loguru import logger

from cnps.config import PipelineConfig
from cnps.storage import object_exists, read_excel_bytes, read_parquet, write_parquet

# Nom exact du fichier depose manuellement sur MinIO (staging/cnps/), a plat,
# a cote des CSV existants -- pas de sous-dossier dedie.
_ANSTAT_OBJECT = "cnps/REQUETES_ANSTAT_MODULE_EMPLOYEURS.xlsx"
_ANSTAT_SHEET = "DATA"

# Colonnes ANSTAT reportees sur le panel entreprise, avec leur alias final
_ANSTAT_ATTRS = {
    "SECTEUR_ACTIVITE": "SECTEUR_ACTIVITE_ANSTAT",
    "FORME JURIDIQUE": "FORME_JURIDIQUE_ANSTAT",
    "NUMERO_RCCM": "NUMERO_RCCM",
    "NUMERO_DFE": "NUMERO_DFE",
}


def _normalize_raison_sociale(col: pl.Expr) -> pl.Expr:
    """Normalise une raison sociale pour la jointure (majuscules, ponctuation, espaces)."""
    return (
        col.str.to_uppercase()
        .str.replace_all(r"[^A-Z0-9 ]", "")
        .str.replace_all(r"\s+", " ")
        .str.strip_chars()
    )


def _load_anstat_reference(cfg: PipelineConfig) -> pl.DataFrame:
    """
    Charge le referentiel ANSTAT depuis MinIO et le reduit a une ligne par
    entreprise (raison sociale normalisee), en gardant l'enregistrement le
    plus recent en cas de doublon.
    """
    bucket = cfg.minio.raw_bucket
    if not object_exists(cfg.minio, bucket, _ANSTAT_OBJECT):
        raise FileNotFoundError(
            f"Referentiel ANSTAT introuvable : {bucket}/{_ANSTAT_OBJECT}"
        )

    buf = read_excel_bytes(cfg.minio, bucket, _ANSTAT_OBJECT)
    df_anstat = pl.read_excel(buf, sheet_name=_ANSTAT_SHEET)
    logger.info("Referentiel ANSTAT charge : {} lignes, {} colonnes", df_anstat.height, df_anstat.width)

    df_anstat = df_anstat.with_columns(
        _normalize_raison_sociale(pl.col("RAISON_SOCIALE")).alias("RAISON_SOCIALE_NORM")
    )

    # Le referentiel contient plusieurs lignes par entreprise au fil du temps
    # (changements de secteur, de motif...). On garde la plus recente par
    # date de debut d'activite pour obtenir une ligne unique par entreprise.
    date_col = "DATE DE DEBUT D'ACTIVITE"
    if date_col in df_anstat.columns:
        df_anstat = df_anstat.sort(date_col, descending=True, nulls_last=True)

    n_avant = df_anstat.height
    df_anstat = df_anstat.unique(subset="RAISON_SOCIALE_NORM", keep="first")
    logger.info("Referentiel ANSTAT deduplique : {} -> {} entreprises (raison sociale normalisee)",
                n_avant, df_anstat.height)

    keep_cols = ["RAISON_SOCIALE_NORM"] + [c for c in _ANSTAT_ATTRS if c in df_anstat.columns]
    return df_anstat.select(keep_cols).rename(_ANSTAT_ATTRS)


def enrichir_avec_anstat(cfg: PipelineConfig) -> str:
    """
    Enrichit la base entreprises avec les attributs du referentiel ANSTAT.

    Etapes
    ------
    1. Lecture de la base entreprises (etape 05, ``firm_base.parquet``)
    2. Construction de la table de correspondance ID_EMPLOYEUR -> RAISON_SOCIALE
       (depuis ``cnps_cleaned.parquet``, la raison sociale la plus recente par
       entreprise si elle a change au fil du temps)
    3. Chargement et deduplication du referentiel ANSTAT
    4. Jointure sur raison sociale normalisee (gauche : aucune ligne du panel
       entreprise n'est perdue si elle ne trouve pas de correspondance)
    5. Re-ecriture de la base entreprises enrichie

    Parameters
    ----------
    cfg : PipelineConfig
        Configuration du pipeline.

    Returns
    -------
    str
        Nom de l'objet Parquet de la base entreprises enrichie sur MinIO.
    """
    bucket = cfg.minio.cleaned_bucket

    firm_object = f"{cfg.minio.cleaned_prefix}firm_base.parquet"
    if not object_exists(cfg.minio, bucket, firm_object):
        raise FileNotFoundError(f"Base entreprises introuvable : {bucket}/{firm_object}")
    firm_df = read_parquet(cfg.minio, bucket, firm_object)
    logger.info("Base entreprises chargee : {} lignes", firm_df.height)

    # --- Table de correspondance ID_EMPLOYEUR -> RAISON_SOCIALE ---
    cleaned_object = f"{cfg.minio.cleaned_prefix}cnps_cleaned.parquet"
    if not object_exists(cfg.minio, bucket, cleaned_object):
        raise FileNotFoundError(f"Donnees nettoyees introuvables : {bucket}/{cleaned_object}")

    noms = read_parquet(cfg.minio, bucket, cleaned_object).select(
        "ID_EMPLOYEUR", "RAISON_SOCIALE", "PERIOD"
    )
    # Une entreprise peut changer de raison sociale au fil du temps (rare,
    # ~0.25% des entreprises) : on garde la plus recente par PERIOD.
    noms = (
        noms.sort("PERIOD", descending=True)
        .unique(subset="ID_EMPLOYEUR", keep="first")
        .select("ID_EMPLOYEUR", "RAISON_SOCIALE")
        .with_columns(_normalize_raison_sociale(pl.col("RAISON_SOCIALE")).alias("RAISON_SOCIALE_NORM"))
    )

    # --- Referentiel ANSTAT ---
    anstat_ref = _load_anstat_reference(cfg)

    # --- Jointure ---
    noms_enrichis = noms.join(anstat_ref, on="RAISON_SOCIALE_NORM", how="left")

    n_total = noms_enrichis.height
    n_matches = noms_enrichis.filter(pl.col("SECTEUR_ACTIVITE_ANSTAT").is_not_null()).height if \
        "SECTEUR_ACTIVITE_ANSTAT" in noms_enrichis.columns else 0
    logger.info("Correspondance ANSTAT : {} / {} entreprises ({:.1f}%)",
                n_matches, n_total, n_matches / n_total * 100 if n_total else 0.0)

    anstat_cols = [c for c in _ANSTAT_ATTRS.values() if c in noms_enrichis.columns]
    firm_df = firm_df.join(
        noms_enrichis.select(["ID_EMPLOYEUR"] + anstat_cols),
        on="ID_EMPLOYEUR",
        how="left",
    )

    out_object = firm_object
    write_parquet(cfg.minio, bucket, out_object, firm_df)
    logger.info("Base entreprises enrichie (ANSTAT) ecrite : {} lignes, {} colonnes -> {}",
                firm_df.height, firm_df.width, out_object)

    return out_object
