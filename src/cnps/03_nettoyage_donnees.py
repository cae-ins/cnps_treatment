"""
Etape 3/12 — Nettoyage et enrichissement des donnees.

Concatene tous les Parquets mensuels harmonises (etape 2) en un seul
jeu de donnees, applique les regles metier (salaire minimum, types
d'employes exclus), calcule les variables derivees (ages, anciennete,
classes de taille d'entreprise) et winsorise les valeurs extremes de
salaire.

Variables derivees
------------------
- ``AGE_EMPLOYE`` : age en annees depuis ``DATE_NAISSANCE``
- ``ANCIENNETE_ENTREPRISE`` : anciennete en annees depuis ``DATE_EMBAUCHE``
- ``ANCIENNETE_IMMAT`` : anciennete d'immatriculation de l'employé 
- ``AGE_ENTREPRISE_IMMAT`` : age de l'entreprise depuis ``DATE_IMMAT_EMPLOYEUR``
- ``SALAIRE_BRUT_MENS`` : salaire mensuel = SALAIRE_BRUT / max(DUREE_TRAVAILLEE, 1) * 12
- Variables de classe (age, anciennete, taille d'entreprise)

References
----------
Tukey, J. W. (1977). *Exploratory Data Analysis*. Addison-Wesley.
    — Winsorisation comme traitement robuste des valeurs extremes.
Dixon, W. J. (1960). Simplified estimation from censored normal samples.
    *Annals of Mathematical Statistics*, 31(2), 385-391.
"""

from __future__ import annotations

import re
from datetime import date

import polars as pl
from loguru import logger

from cnps.config import PipelineConfig, load_config
from cnps.storage import list_objects, read_parquet, write_parquet


# ---------------------------------------------------------------------------
# Helpers de classification
# ---------------------------------------------------------------------------

def _classify(value_col: str, breaks: list[tuple[float, float, str]]) -> pl.Expr:
    """
    Construit une chaine Polars ``when/then/otherwise`` pour classifier une colonne.

    Parameters
    ----------
    value_col : str
        Colonne a classifier.
    breaks : list de (min, max, label)
        Bornes inferieure incluse, superieure exclue, et etiquette.
    """
    expr = pl.when(pl.col(value_col).is_null()).then(pl.lit(None).cast(pl.Utf8))
    for lo, hi, label in breaks:
        expr = expr.when(
            (pl.col(value_col) >= lo) & (pl.col(value_col) < hi)
        ).then(pl.lit(label))
    return expr.otherwise(pl.lit(None).cast(pl.Utf8))


_AGE_CLASSES = [
    (0, 25, "Moins de 25 ans"),
    (25, 35, "25-34 ans"),
    (35, 50, "35-49 ans"),
    (50, 999, "50 ans et plus"),
]

_SENIORITY_CLASSES = [
    (0, 2, "Moins de 2 ans"),
    (2, 5, "2-4 ans"),
    (5, 10, "5-9 ans"),
    (10, 999, "10 ans et plus"),
]

_FIRM_SIZE_DETAILED = [
    (1, 2, "1 salarie"),
    (2, 6, "2-5 salaries"),
    (6, 11, "6-10 salaries"),
    (11, 21, "11-20 salaries"),
    (21, 51, "21-50 salaries"),
    (51, 101, "51-100 salaries"),
    (101, 201, "101-200 salaries"),
    (201, 501, "201-500 salaries"),
    (501, 1001, "501-1000 salaries"),
    (1001, 1_000_000, "Plus de 1000 salaries"),
]

_FIRM_SIZE_REDUCED = [
    (1, 11, "Micro (1-10)"),
    (11, 51, "Petite (11-50)"),
    (51, 201, "Moyenne (51-200)"),
    (201, 1_000_000, "Grande (201+)"),
]

# Niveaux de la colonne TAG retires au filtre 2/5. Les niveaux 1, 2 et 4 sont
# conserves : ils signalent une correspondance avec une autre ligne sans que
# celle-ci constitue necessairement un double compte (cumul d'emplois chez des
# employeurs distincts, reprise de contrat en cours de mois). Seul le niveau 3
# est exclu. La deduplication sur ID_INDIV+ID_EMPLOYEUR+periode (filtre 3/5)
# prend le relais contre les doubles comptes chez un meme employeur.
_TAGS_EXCLUS = frozenset({"doublon_niv_3"})

# Conventions de conversion journalier/horaire -> mensuel pour
# SALAIRE_BRUT_ESTIME_AU_MOIS (memes hypotheses que audit.py::_check_analyse_salaire
# et le notebook analyse_incoherences_salaires.ipynb, a garder alignees si modifiees).
# 22,4 jours ouvres : moyenne annuelle hors dimanches et jours feries, retenue de
# preference a 26 qui correspond a un mois travaille six jours sur sept.
_JOURS_OUVRES_PAR_MOIS = 22.4
_HEURES_PAR_MOIS = 179.2  # 22,4 jours x 8h


# ---------------------------------------------------------------------------
# API publique
# ---------------------------------------------------------------------------

def nettoyer_donnees(cfg: PipelineConfig, *, include_hj_estimated: bool = False) -> str:
    """
    Nettoie et enrichit le jeu de donnees concatene.

    Etapes
    ------
    1. Concatenation de tous les Parquets mensuels
    2. Regles metier : doublons (lignes strictement identiques + colonne TAG),
       doublons ID_INDIV+ID_EMPLOYEUR+mois (meme employeur, salaire le plus
       eleve conserve ; les cumuls d'emplois chez des employeurs differents
       ne sont pas touches), types d'employes exclus OU estimation mensuelle
       (cf. ``include_hj_estimated``), salaire minimum (negatifs/nuls/sous-seuil,
       cf. commentaire au point d'exclusion)
    3. Calcul des variables derivees (ages, anciennete, classes,
       SALAIRE_BRUT_ESTIME_AU_MOIS)
    4. Winsorisation des valeurs extremes de salaire
    5. Ecriture du Parquet nettoye

    Parameters
    ----------
    cfg : PipelineConfig
        Configuration du pipeline.
    include_hj_estimated : bool, optional
        Si False (defaut, **recommande**) : les types listes dans
        ``cfg.cleaning.exclude_employee_types`` sont retires. Cette liste vaut
        desormais ``["H"]`` : seuls les HORAIRES sont exclus, les journaliers
        sont conserves et convertis via ``SALAIRE_BRUT_ESTIME_AU_MOIS``
        (cf. section 3).

        Si True : **aucun** type n'est exclu, horaires compris. A n'utiliser
        que pour une analyse de sensibilite explicite. L'audit du 28/07/2026
        (feuille ``Analyse_Salaire``) montre que 69% des lignes horaires ont
        une ``DUREE_TRAVAILLEE`` incoherente et 65,5% une confusion d'unite
        suspectee : les reintegrer propage ces erreurs, amplifiees par la
        conversion x208.

        Le parametre YAML ``cleaning.exclude_employee_types`` n'est jamais
        modifie par ce flag : c'est un choix ponctuel pour l'execution en
        cours, pas un changement de configuration persistant.

    Returns
    -------
    str
        Nom de l'objet Parquet nettoye sur MinIO.
    """
    if include_hj_estimated:
        logger.warning(
            "Mode periodicites : AUCUN type exclu (include_hj_estimated=True), "
            "les HORAIRES sont reintegres alors que l'audit les signale comme "
            "majoritairement ininterpretables (69% de DUREE_TRAVAILLEE "
            "incoherente). Mode d'analyse de sensibilite uniquement -- ne pas "
            "utiliser pour une production d'indicateurs."
        )
    else:
        logger.info(
            "Mode periodicites : types {} exclus (config), les autres sont "
            "conserves et ramenes au mois via SALAIRE_BRUT_ESTIME_AU_MOIS.",
            cfg.cleaning.exclude_employee_types,
        )

    processed_bucket = cfg.minio.processed_bucket
    all_objects = list_objects(cfg.minio, processed_bucket, cfg.minio.processed_prefix, recursive=False)
    files = sorted(obj for obj in all_objects if re.search(r"\.parquet$", obj))

    if not files:
        raise FileNotFoundError(
            f"Aucun fichier Parquet sous {processed_bucket}/{cfg.minio.processed_prefix}"
        )

    # --- 1. Concatenation ---
    logger.info("[1. Concatenation] Lecture de {} fichiers mensuels depuis {}/{}",
                len(files), processed_bucket, cfg.minio.processed_prefix)
    frames = [read_parquet(cfg.minio, processed_bucket, f) for f in files]

    # Aligne les schemas (union des colonnes)
    all_cols = dict.fromkeys(col for f in frames for col in f.columns)
    aligned = []
    for f in frames:
        for col in all_cols:
            if col not in f.columns:
                f = f.with_columns(pl.lit(None).alias(col))
        aligned.append(f.select(list(all_cols.keys())))

    df = pl.concat(aligned, how="vertical")
    logger.info("[1. Concatenation] Termine : {} lignes, {} colonnes ({} colonnes uniques sur l'ensemble des fichiers)",
                df.height, df.width, len(all_cols))

    # --- Suivi du volume cumule ---
    # n_brut est le denominateur fixe (volume brut concatene, avant tout
    # filtre) utilise pour TOUS les "% du brut conserve" ci-dessous -- a ne
    # pas confondre avec le "% retire" de chaque log de filtre, qui lui est
    # relatif au volume ENTRANT dans ce filtre precis (cf. TRAITEMENT_MANOUAN.md,
    # point 8, pour la distinction). _log_etape() logue les deux a chaque etape.
    n_brut = df.height

    def _log_etape(nom: str, hauteur: int) -> None:
        pct_brut = hauteur / n_brut * 100 if n_brut else 0.0
        logger.info(
            "[VOLUME CUMULE] Apres {} : {} lignes restantes, soit {:.2f}% du volume brut "
            "initial ({} lignes)",
            nom, hauteur, pct_brut, n_brut,
        )

    _log_etape("concatenation (etat initial)", df.height)

    # --- 1bis. Regles metier (filtres) ---
    if cfg.cleaning.remove_duplicates:
        n_avant = df.height
        df = df.unique()
        n_retire = n_avant - df.height
        logger.info(
            "[Filtre 1/5 - Doublons stricts] Lignes strictement identiques (toutes colonnes) : "
            "{} -> {} lignes ({} retirees, {:.2f}% du volume entrant)",
            n_avant, df.height, n_retire, n_retire / n_avant * 100 if n_avant else 0.0,
        )
    else:
        logger.info("[Filtre 1/5 - Doublons stricts] Desactive (cleaning.remove_duplicates=false), aucune ligne retiree.")
    _log_etape("Filtre 1/5 - Doublons stricts", df.height)

    # TAG (deja calcule cote source) classe chaque ligne selon un niveau de
    # doublon detecte en amont ("unique", "doublon_niv_1".."doublon_niv_4").
    # Seuls les niveaux listes dans _TAGS_EXCLUS sont retires : les autres
    # niveaux sont conserves car ils correspondent a des situations legitimes
    # (cumul d'emplois, reprise de contrat) et non a des doubles comptes.
    # Le filtre 3/5, qui deduplique sur ID_INDIV+ID_EMPLOYEUR+periode, reste
    # le garde-fou contre les doubles comptes chez un meme employeur.
    if "TAG" in df.columns:
        n_avant = df.height
        tag_counts = df["TAG"].value_counts().sort("count", descending=True)
        df = df.filter(
            pl.col("TAG").is_null() | ~pl.col("TAG").is_in(_TAGS_EXCLUS)
        )
        n_retire = n_avant - df.height
        logger.info(
            "[Filtre 2/5 - Colonne TAG] Repartition avant filtre : {}",
            {row["TAG"]: row["count"] for row in tag_counts.iter_rows(named=True)},
        )
        logger.info(
            "[Filtre 2/5 - Colonne TAG] Niveaux exclus {} (les autres niveaux sont "
            "CONSERVES) : {} -> {} lignes ({} retirees, {:.2f}% du volume entrant)",
            sorted(_TAGS_EXCLUS), n_avant, df.height, n_retire,
            n_retire / n_avant * 100 if n_avant else 0.0,
        )
    else:
        logger.info("[Filtre 2/5 - Colonne TAG] Colonne absente, filtre ignore.")
    _log_etape("Filtre 2/5 - Colonne TAG", df.height)

    # Doublons d'ID_INDIV au sein d'un meme mois, chez le MEME employeur :
    # un individu ne devrait avoir qu'une seule declaration par employeur et
    # par mois (contrairement a un cumul d'emplois legitime chez PLUSIEURS
    # employeurs differents, jamais touche ici). En cas de doublon reel
    # (meme individu, meme employeur, meme mois), on ne garde que la ligne
    # au salaire le plus eleve : les lignes en trop sont le plus souvent des
    # sous-declarations partielles ou des saisies incompletes, le montant le
    # plus eleve etant la meilleure approximation disponible du salaire
    # effectivement verse ce mois-la.
    _cols_periode = [c for c in ("PERIOD", "ANNEE", "MOIS") if c in df.columns]
    if "ID_INDIV" in df.columns and "ID_EMPLOYEUR" in df.columns and _cols_periode and "SALAIRE_BRUT" in df.columns:
        n_avant = df.height
        cle_dedup = ["ID_INDIV", "ID_EMPLOYEUR", *_cols_periode]
        df = (
            df.sort("SALAIRE_BRUT", descending=True, nulls_last=True)
            .unique(subset=cle_dedup, keep="first", maintain_order=True)
        )
        n_retire = n_avant - df.height
        logger.info(
            "[Filtre 3/5 - Doublons ID_INDIV+ID_EMPLOYEUR] Cle : {} (meme employeur, salaire le "
            "plus eleve conserve ; cumuls d'emplois chez des employeurs differents non touches) : "
            "{} -> {} lignes ({} retirees, {:.2f}% du volume entrant)",
            cle_dedup, n_avant, df.height, n_retire, n_retire / n_avant * 100 if n_avant else 0.0,
        )
    else:
        logger.info(
            "[Filtre 3/5 - Doublons ID_INDIV+ID_EMPLOYEUR] Colonnes requises absentes "
            "(ID_INDIV, ID_EMPLOYEUR, SALAIRE_BRUT, PERIOD/ANNEE+MOIS), filtre ignore."
        )
    _log_etape("Filtre 3/5 - Doublons ID_INDIV+ID_EMPLOYEUR", df.height)

    if "TYPE_SALARIE" in df.columns and cfg.cleaning.exclude_employee_types and not include_hj_estimated:
        n_avant = df.height
        type_counts_avant = df["TYPE_SALARIE"].value_counts().sort("count", descending=True)
        # IMPORTANT : is_in() sur une valeur TYPE_SALARIE null renvoie null (pas
        # False) en Polars, et ~null est traite comme faux par .filter() -- donc
        # sans le is_null() explicite ci-dessous, ce filtre exclurait AUSSI,
        # silencieusement, toutes les lignes a TYPE_SALARIE non renseigne (qui
        # se trouvent avoir systematiquement SALAIRE_BRUT egalement manquant,
        # cf. TRAITEMENT_MANOUAN.md). Ce n'etait pas l'intention du filtre :
        # seuls les types listes en config doivent etre exclus, les lignes non
        # renseignees doivent rester disponibles pour l'imputation en aval
        # (etapes 07/07b/08/09).
        df = df.filter(
            pl.col("TYPE_SALARIE").is_null()
            | ~pl.col("TYPE_SALARIE").is_in(cfg.cleaning.exclude_employee_types)
        )
        n_retire = n_avant - df.height
        logger.info(
            "[Filtre 4/5 - Types d'employes exclus] Repartition TYPE_SALARIE avant filtre : {}",
            {row["TYPE_SALARIE"]: row["count"] for row in type_counts_avant.iter_rows(named=True)},
        )
        logger.info(
            "[Filtre 4/5 - Types d'employes exclus] Types {} exclus, TYPE_SALARIE non renseigne "
            "CONSERVE : {} -> {} lignes ({} retirees, {:.2f}% du volume entrant). "
            "Attendu : ~1,4% du volume (horaires uniquement, cf. audit) -- un pourcentage "
            "nettement superieur signale que la config exclut plus que les seuls horaires.",
            cfg.cleaning.exclude_employee_types, n_avant, df.height, n_retire,
            n_retire / n_avant * 100 if n_avant else 0.0,
        )
    elif "TYPE_SALARIE" in df.columns and cfg.cleaning.exclude_employee_types and include_hj_estimated:
        logger.warning(
            "[Filtre 4/5 - Types d'employes exclus] IGNORE (include_hj_estimated=True) : "
            "les types {} sont CONSERVES, horaires compris. Analyse de sensibilite "
            "uniquement.",
            cfg.cleaning.exclude_employee_types,
        )
    else:
        logger.info("[Filtre 4/5 - Types d'employes exclus] TYPE_SALARIE absent ou exclude_employee_types vide, filtre ignore.")
    _log_etape("Filtre 4/5 - Types d'employes exclus", df.height)

    if "SALAIRE_BRUT" in df.columns:
        # Seuil de plausibilite du salaire, ventile par periodicite
        # (TYPE_SALARIE) des qu'un type non mensuel subsiste dans les donnees :
        # un taux journalier de 3 000 FCFA est parfaitement plausible
        # (~2 885 FCFA/jour attendu) mais serait exclu a tort par une
        # comparaison directe au seuil mensuel (75 000).
        #
        # La condition ne peut PAS dependre du seul include_hj_estimated :
        # depuis que exclude_employee_types vaut ["H"] (les journaliers sont
        # conserves par defaut, cf. settings.yaml), des lignes "J" survivent au
        # filtre 4/5 meme en mode par defaut. Les comparer a 75 000 FCFA/jour
        # les eliminerait presque toutes. On ventile donc des qu'un type J ou H
        # est effectivement present apres le filtre precedent.
        _types_restants = (
            set(df["TYPE_SALARIE"].drop_nulls().unique().to_list())
            if "TYPE_SALARIE" in df.columns else set()
        )
        if _types_restants & {"J", "H"}:
            seuil_journalier = cfg.cleaning.min_salary / _JOURS_OUVRES_PAR_MOIS
            seuil_horaire = cfg.cleaning.min_salary / _HEURES_PAR_MOIS
            seuil_expr = (
                pl.when(pl.col("TYPE_SALARIE") == "J").then(pl.lit(seuil_journalier))
                .when(pl.col("TYPE_SALARIE") == "H").then(pl.lit(seuil_horaire))
                .otherwise(pl.lit(cfg.cleaning.min_salary))
            )
            logger.info(
                "[Filtre 5/5 - Salaire minimum] Seuils par periodicite (types non mensuels "
                "presents : {}) : Mensuel/autre={:.0f} FCFA, Journalier={:.0f} FCFA/jour, "
                "Horaire={:.0f} FCFA/h",
                sorted(_types_restants & {"J", "H"}),
                cfg.cleaning.min_salary, seuil_journalier, seuil_horaire,
            )
        else:
            seuil_expr = pl.lit(cfg.cleaning.min_salary)

        # Ce filtre unique (SALAIRE_BRUT >= seuil) exclut en une seule
        # passe TROIS categories distinctes d'incoherence, toutes < seuil :
        #   - les salaires negatifs (impossibles en toute circonstance) ;
        #   - les salaires nuls / a zero ;
        #   - les salaires positifs mais sous le seuil (SMIG mensuel, 75 000
        #     FCFA par defaut, cf. cleaning.min_salary dans settings.yaml,
        #     ou seuil ventile par periodicite calcule ci-dessus).
        # Les valeurs SALAIRE_BRUT.is_null() sont explicitement exemptees
        # (conservees telles quelles, une absence de salaire n'est pas la
        # meme incoherence qu'un montant errone -- elle reste disponible pour
        # l'imputation en aval, etapes 07/08/09).
        # Le detail par categorie (nb negatifs, nb nuls, nb sous-seuil) est
        # logue separement ci-dessous a titre informatif uniquement : il ne
        # s'agit PAS de trois filtres distincts, seulement d'un decompte pour
        # la tracabilite -- la ligne est bien retiree en une seule fois par
        # le filtre ci-dessous, quelle que soit sa categorie.
        n_negatifs = df.filter(pl.col("SALAIRE_BRUT") < 0).height
        n_nuls = df.filter(pl.col("SALAIRE_BRUT") == 0).height
        n_sous_seuil = df.filter(
            (pl.col("SALAIRE_BRUT") > 0) & (pl.col("SALAIRE_BRUT") < seuil_expr)
        ).height
        logger.info(
            "[Filtre 5/5 - Salaire minimum] Detail des salaires sous seuil a exclure : "
            "{} negatifs + {} nuls (zero) + {} positifs sous le seuil = {} lignes au total",
            n_negatifs, n_nuls, n_sous_seuil,
            n_negatifs + n_nuls + n_sous_seuil,
        )

        n_avant = df.height
        df = df.filter(
            pl.col("SALAIRE_BRUT").is_null() | (pl.col("SALAIRE_BRUT") >= seuil_expr)
        )
        n_retire = n_avant - df.height
        logger.info(
            "[Filtre 5/5 - Salaire minimum] Salaires sous seuil exclus (negatifs+nuls+sous-seuil, "
            "salaires manquants CONSERVES) : {} -> {} lignes ({} retirees, {:.2f}% du volume entrant)",
            n_avant, df.height, n_retire, n_retire / n_avant * 100 if n_avant else 0.0,
        )
    else:
        logger.info("[Filtre 5/5 - Salaire minimum] SALAIRE_BRUT absent, filtre ignore.")
    _log_etape("Filtre 5/5 - Salaire minimum (fin des filtres de nettoyage)", df.height)

    # --- 2. Variables derivees ---
    ref_date = date.today()

    # SALAIRE_BRUT_MENS : variable HISTORIQUE, conservee pour compatibilite.
    # Elle n'est plus la reference des etapes aval (voir SALAIRE_BRUT_ESTIME_AU_MOIS
    # ci-dessous) et n'est plus winsorisee : la winsorisation a ete deplacee
    # apres la conversion de periodicite, ou elle a un sens (cf. plus bas).
    # Rappel de sa limite : elle suppose DUREE_TRAVAILLEE exprimee en mois, ce
    # que l'audit infirme pour les horaires (69% de durees incoherentes).
    if "SALAIRE_BRUT" in df.columns and "DUREE_TRAVAILLEE" in df.columns:
        df = df.with_columns(
            (pl.col("SALAIRE_BRUT") / pl.col("DUREE_TRAVAILLEE").clip(1, cfg.cleaning.max_duration) * 12)
            .alias("SALAIRE_BRUT_MENS")
        )

    # --- SALAIRE_BRUT_ESTIME_AU_MOIS ---
    # Contrairement a SALAIRE_BRUT_MENS (qui suppose DUREE_TRAVAILLEE exprimee
    # en mois, borne a max_duration=12 -- coherent uniquement pour TYPE_SALARIE
    # == "M"), cette variable estime un equivalent MENSUEL du salaire pour
    # CHAQUE periodicite declaree, en appliquant le taux journalier/horaire
    # aux jours/heures ouvres standard d'un mois complet (26 jours, 208h) :
    #   - Mensuel (M)    : SALAIRE_BRUT tel quel (deja un montant mensuel).
    #   - Journalier (J) : SALAIRE_BRUT (taux/jour) x 26 jours ouvres/mois.
    #   - Horaire (H)    : SALAIRE_BRUT (taux/heure) x 208h/mois (26j x 8h).
    #   - Autre/absent   : SALAIRE_BRUT tel quel (aucune conversion connue).
    # Memes conventions que audit.py::_check_analyse_salaire (SEUIL_PLAUSIBLE)
    # et le notebook analyse_incoherences_salaires.ipynb (SEUIL_PLAUSIBLE) --
    # a garder alignees si l'une des trois est modifiee.
    if "SALAIRE_BRUT" in df.columns:
        if "TYPE_SALARIE" in df.columns:
            df = df.with_columns(
                pl.when(pl.col("TYPE_SALARIE") == "J")
                .then(pl.col("SALAIRE_BRUT") * _JOURS_OUVRES_PAR_MOIS)
                .when(pl.col("TYPE_SALARIE") == "H")
                .then(pl.col("SALAIRE_BRUT") * _HEURES_PAR_MOIS)
                .otherwise(pl.col("SALAIRE_BRUT"))
                .alias("SALAIRE_BRUT_ESTIME_AU_MOIS")
            )
            n_j = df.filter(pl.col("TYPE_SALARIE") == "J").height
            n_h = df.filter(pl.col("TYPE_SALARIE") == "H").height
            logger.info(
                "SALAIRE_BRUT_ESTIME_AU_MOIS calcule : Mensuel/autre inchange, "
                "Journalier x{} ({} lignes), Horaire x{} ({} lignes)",
                _JOURS_OUVRES_PAR_MOIS, n_j, _HEURES_PAR_MOIS, n_h,
            )
        else:
            df = df.with_columns(pl.col("SALAIRE_BRUT").alias("SALAIRE_BRUT_ESTIME_AU_MOIS"))
            logger.info(
                "SALAIRE_BRUT_ESTIME_AU_MOIS = SALAIRE_BRUT (TYPE_SALARIE absent, "
                "aucune conversion de periodicite possible)."
            )

        # --- Winsorisation, APRES conversion de periodicite (Tukey, 1977) ---
        #
        # L'ordre est essentiel. Winsoriser AVANT conversion (comportement
        # precedent, sur SALAIRE_BRUT_MENS) revient a calculer des percentiles
        # sur une distribution melangeant trois echelles incomparables : un taux
        # horaire (~500 F), un taux journalier (~3 000 F) et un salaire mensuel
        # (~200 000 F). Le p1 global tombe alors dans la masse des taux
        # horaires : presque tous les H/J passent sous cette borne et sont
        # ecretes VERS LE HAUT -- un horaire a 500 F/h devenait artificiellement
        # le p1 mensuel. On ne supprimait pas des valeurs aberrantes, on en
        # fabriquait.
        #
        # Sur SALAIRE_BRUT_ESTIME_AU_MOIS, toutes les lignes sont ramenees a la
        # meme echelle mensuelle : les percentiles ont un sens, et l'ecretage
        # ne touche que de vraies valeurs extremes.
        #
        # Effet de bord assume : les deux extremites de la distribution sont
        # ecrasees. Les statistiques d'inegalite (Gini, ratios inter-deciles,
        # part du dernier centile) ne doivent PAS etre calculees sur cette
        # variable -- utiliser SALAIRE_BRUT ou une version non winsorisee.
        bornes = df.select(
            pl.col("SALAIRE_BRUT_ESTIME_AU_MOIS").quantile(cfg.cleaning.winsor_lower).alias("lo"),
            pl.col("SALAIRE_BRUT_ESTIME_AU_MOIS").quantile(cfg.cleaning.winsor_upper).alias("hi"),
        ).row(0)
        lo, hi = bornes
        if lo is not None and hi is not None:
            n_bas = df.filter(pl.col("SALAIRE_BRUT_ESTIME_AU_MOIS") < lo).height
            n_haut = df.filter(pl.col("SALAIRE_BRUT_ESTIME_AU_MOIS") > hi).height
            df = df.with_columns(
                pl.col("SALAIRE_BRUT_ESTIME_AU_MOIS").clip(lo, hi)
                .alias("SALAIRE_BRUT_ESTIME_AU_MOIS")
            )
            logger.info(
                "Winsorisation SALAIRE_BRUT_ESTIME_AU_MOIS (apres conversion de "
                "periodicite) : bornes [{:.0f}, {:.0f}] (p{:.0f}/p{:.0f}), "
                "{} valeurs ecretees en bas, {} en haut",
                lo, hi, cfg.cleaning.winsor_lower * 100, cfg.cleaning.winsor_upper * 100,
                n_bas, n_haut,
            )
        else:
            logger.warning(
                "Winsorisation impossible : aucune valeur exploitable dans "
                "SALAIRE_BRUT_ESTIME_AU_MOIS."
            )

    # --- Pas d'imputation par continuite individuelle (retire volontairement) ---
    #
    # Une imputation backward/forward au niveau ID_INDIV
    # (``SALAIRE_BRUT_IMPUTE_INDIV`` / ``FLAG_IMPUTE_INDIV``) a existe ici puis
    # a ete retiree, pour rester conforme a la note methodologique de reference
    # (« Estimation du salaire moyen national a partir de donnees incompletes »,
    # annexe 2 « Declaration des salaires par l'employeur » et annexe 3
    # « Declaration employeur partielle »).
    #
    # Raison : dans les donnees administratives CNPS, l'unite declarante est
    # l'ENTREPRISE, pas le salarie. Le manquant est donc structurellement situe
    # au niveau entreprise-mois. Combler un trou individuel par report d'un
    # autre mois du meme individu revient a mal specifier le mecanisme de
    # non-reponse.
    #
    # Consequence technique, et c'est le point bloquant : la correction prevue
    # repose sur un IPW qui repondere les DECLARANTS par l'inverse de leur
    # probabilite de declarer (cf. 07_modele_declaration.py). Pre-remplir des
    # manquants transforme des non-declarants en declarants aux yeux de ce
    # modele : la probabilite p_jt est alors estimee sur une population
    # artificiellement gonflee, les poids 1/p sont sous-estimes, et la
    # correction du biais echoue silencieusement. S'y ajoute une double
    # comptabilisation, l'entreprise concernee etant de toute facon imputee a
    # l'etape 08.
    #
    # Constat empirique allant dans le meme sens (feuille Changement_Employeur
    # de l'audit) : la variation mediane du salaire est de ~62% lors d'un
    # changement d'employeur contre ~0,7% a employeur constant. Un report
    # individuel traverse ces ruptures et transporte le salaire de l'ancien
    # poste sur le nouveau.
    #
    # L'information de continuite individuelle reste exploitable, mais comme
    # VARIABLE EXPLICATIVE du modele de declaration (historique et regularite
    # de declaration de l'individu, cf. annexe 3, probabilite q_ijt), jamais
    # comme valeur imputee en amont de l'IPW.

    if "DATE_NAISSANCE" in df.columns:
        df = df.with_columns(
            ((pl.lit(ref_date) - pl.col("DATE_NAISSANCE")).dt.total_days() / 365.25)
            .floor()
            .cast(pl.Int32, strict=False)
            .alias("AGE_EMPLOYE")
        )

    if "DATE_EMBAUCHE" in df.columns:
        df = df.with_columns(
            ((pl.lit(ref_date) - pl.col("DATE_EMBAUCHE")).dt.total_days() / 365.25)
            .floor()
            .cast(pl.Int32, strict=False)
            .alias("ANCIENNETE_ENTREPRISE")
        )

    if "DATE_IMMATRICULATION" in df.columns:
        df = df.with_columns(
            ((pl.lit(ref_date) - pl.col("DATE_IMMATRICULATION")).dt.total_days() / 365.25)
            .floor()
            .cast(pl.Int32, strict=False)
            .alias("ANCIENNETE_IMMAT")
        )

    if "DATE_IMMAT_EMPLOYEUR" in df.columns:
        df = df.with_columns(
            ((pl.lit(ref_date) - pl.col("DATE_IMMAT_EMPLOYEUR")).dt.total_days() / 365.25)
            .floor()
            .cast(pl.Int32, strict=False)
            .alias("AGE_ENTREPRISE_IMMAT")
        )

    if "MOIS" in df.columns:
        df = df.with_columns(
            ((pl.col("MOIS") - 1) // 3 + 1).cast(pl.Int32).alias("TRIMESTRE"),
            ((pl.col("MOIS") - 1) // 6 + 1).cast(pl.Int32).alias("SEMESTRE"),
        )

    # --- Variables de classification ---
    if "AGE_EMPLOYE" in df.columns:
        df = df.with_columns(
            _classify("AGE_EMPLOYE", _AGE_CLASSES).alias("CL_AGE_EMPLOYE")
        )

    if "ANCIENNETE_ENTREPRISE" in df.columns:
        df = df.with_columns(
            _classify("ANCIENNETE_ENTREPRISE", _SENIORITY_CLASSES)
            .alias("CL_ANCIENNETE_ENTREPRISE")
        )

    if "ANCIENNETE_IMMAT" in df.columns:
        df = df.with_columns(
            _classify("ANCIENNETE_IMMAT", _SENIORITY_CLASSES)
            .alias("CL_ANCIENNETE_IMMAT")
        )

    if "AGE_ENTREPRISE_IMMAT" in df.columns:
        df = df.with_columns(
            _classify("AGE_ENTREPRISE_IMMAT", _SENIORITY_CLASSES)
            .alias("CL_AGE_ENTREPRISE")
        )

    if "EFFECTIF_SALARIES" in df.columns:
        df = df.with_columns(
            _classify("EFFECTIF_SALARIES", _FIRM_SIZE_DETAILED).alias("CLASSE_EFFECTIF"),
            _classify("EFFECTIF_SALARIES", _FIRM_SIZE_REDUCED).alias("CLASSE_EFFECTIF_REDUITE"),
        )

    # --- 3. Ecriture ---
    out_object = f"{cfg.minio.cleaned_prefix}cnps_cleaned.parquet"
    write_parquet(cfg.minio, cfg.minio.cleaned_bucket, out_object, df)
    logger.info("Donnees nettoyees ecrites : {} ({} lignes, {} colonnes)",
                out_object, df.height, df.width)
    logger.info(
        "[VOLUME CUMULE - RECAPITULATIF FINAL] {} lignes brutes -> {} lignes en sortie, "
        "soit {:.2f}% du volume brut conserve au total ({:.2f}% retire cumule, tous filtres "
        "confondus).",
        n_brut, df.height, df.height / n_brut * 100 if n_brut else 0.0,
        (n_brut - df.height) / n_brut * 100 if n_brut else 0.0,
    )

    return out_object


if __name__ == "__main__":
    import argparse
    import sys
    from pathlib import Path

    parser = argparse.ArgumentParser(
        description=__doc__.strip().splitlines()[0] if __doc__ else None
    )
    parser.add_argument("--settings", "-s", type=Path, default=None)
    parser.add_argument("--dimensions", "-d", type=Path, default=None)
    parser.add_argument(
        "--include-hj-estimated", action="store_true",
        help="ANALYSE DE SENSIBILITE UNIQUEMENT : n'exclut AUCUN type d'employe, "
             "horaires compris, en leur calculant SALAIRE_BRUT_ESTIME_AU_MOIS. "
             "Par defaut (sans ce flag), seuls les types listes dans "
             "cleaning.exclude_employee_types sont retires -- soit les horaires, "
             "dont 69%% des lignes ont une DUREE_TRAVAILLEE incoherente. Les "
             "journaliers sont conserves dans les deux cas. Ne modifie jamais "
             "settings.yaml : ce choix ne vaut que pour cette execution.",
    )
    parser.add_argument("--verbose", "-v", action="store_true")
    args = parser.parse_args()

    cfg = load_config(args.settings, args.dimensions)

    logger.remove()
    logger.add(
        sys.stderr,
        level="DEBUG" if args.verbose else "INFO",
        colorize=True,
        format="<green>{time:HH:mm:ss}</green> | <level>{level:<8}</level> | {message}",
    )
    logger.add(
        str(cfg.paths.logs / f"{Path(__file__).stem}.log"),
        level="DEBUG", rotation="10 MB", retention="30 days", encoding="utf-8",
    )

    try:
        nettoyer_donnees(cfg, include_hj_estimated=args.include_hj_estimated)
        logger.info("Termine avec succes.")
    except Exception as exc:
        logger.exception("Echec de l'etape: {}", exc)
        sys.exit(1)
