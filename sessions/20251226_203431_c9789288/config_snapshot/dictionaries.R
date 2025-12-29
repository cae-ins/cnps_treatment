# ============================================================
# DICTIONARIES.R
# Dictionnaires de variables, types et mappings
# ============================================================

# ============================================================
# 1. TYPES DE COLONNES
# ============================================================

COLUMN_TYPES <- list(
  
  # === IDENTIFIANTS ===
  ID_INDIV = "character",
  ID_EMPLOI = "character",
  NUMERO_EMPLOYEUR = "double",
  NUMERO_EMPLOYEUR_ALT = "character",
  
  # === INFORMATIONS PERSONNELLES ===
  SEXE = "character",
  DATE_NAISSANCE = "datetime",
  SITUATION_MATRIMONIALE = "character",
  NIVEAU_ETUDE = "character",
  
  # === INFORMATIONS PROFESSIONNELLES ===
  PROFESSION = "character",
  STATUT_TRAVAILLEUR = "character",
  TYPE_SALARIE = "character",
  DATE_EMBAUCHE = "datetime",
  DATE_IMMATRICULATION = "datetime",
  CSP_CODE = "character",
  PROF_CODE = "character",
  
  # === INFORMATIONS ENTREPRISE ===
  RAISON_SOCIALE = "character",
  SECTEUR_ACTIVITE = "character",
  SECTEUR_ACTIVITE_COD = "character",
  SECTOR_CODE = "character",
  CATEGORIE_ENTREPRISE = "double",
  DATE_DEBUT_ACT_EMPLOY = "datetime",
  DATE_IMMAT_EMPLOYEUR = "datetime",
  EFFECTIF_SALARIES = "double",
  
  # === LOCALISATION ===
  LIBELLE_AGENCE = "character",
  COMMUNE = "character",
  
  # === DONNÉES SALARIALES ===
  SALAIRE_BRUT = "double",
  DUREE_TRAVAILLEE = "double",
  SALAIRE_BRUT_MENS = "double",
  
  # === PÉRIODE ===
  MOIS = "double",
  ANNEE = "double",
  PERIOD = "character",
  TRIMESTRE = "double",
  SEMESTRE = "double",
  
  # === ÂGES ET ANCIENNETÉS (calculés) ===
  AGE_EMPLOYE = "double",
  ANCIENNETE_ENTREPRISE = "double",
  ANCIENNETE_IMMAT = "double",
  AGE_ENTREPRISE_IMMAT = "double",
  
  # === CLASSES D'ÂGE DÉTAILLÉES (17 classes) ===
  CL_AGE_EMPLOYE = "character",
  CL_ANCIENNETE_ENTREPRISE = "character",
  CL_ANCIENNETE_IMMAT = "character",
  CL_AGE_ENTREPRISE_IMMAT = "character",
  
  # === CLASSES D'ÂGE RÉDUITES (4 classes) ===
  CL_RED_AGE_EMPLOYE = "character",
  CL_RED_ANCIENNETE_ENTREPRISE = "character",
  CL_RED_ANCIENNETE_IMMAT = "character",
  CL_RED_AGE_ENTREPRISE_IMMAT = "character",
  
  # === CLASSES D'EFFECTIF ===
  CLASSE_EFFECTIF = "character",
  CLASSE_EFFECTIF_REDUITE = "character"
)

# ============================================================
# 2. VARIABLES REQUISES PAR CATÉGORIE
# ============================================================

REQUIRED_VARS <- list(
  
  # Variables numériques obligatoires
  numeric = c(
    "SALAIRE_BRUT",
    "DUREE_TRAVAILLEE"
  ),
  
  # Variables caractères obligatoires
  character = c(
    "TYPE_SALARIE"
  ),
  
  # Variables d'identification
  id = c(
    "ID_INDIV"
  ),
  
  # Variables de période
  period = c(
    "MOIS",
    "ANNEE"
  ),
  
  # Variables entreprise
  firm = c(
    "NUMERO_EMPLOYEUR"
  ),
  
  # Variables pour les âges/anciennetés
  dates = c(
    "DATE_NAISSANCE",
    "DATE_EMBAUCHE",
    "DATE_IMMATRICULATION",
    "DATE_IMMAT_EMPLOYEUR"
  )
)

# ============================================================
# 3. LABELS DE SEXE
# ============================================================

SEXE_LABELS <- list(
  "M" = "Masculin",
  "F" = "Féminin",
  "1" = "Masculin",
  "2" = "Féminin"
)

# ============================================================
# 4. LABELS DE TYPE DE SALARIÉ
# ============================================================

TYPE_SALARIE_LABELS <- list(
  "M" = "Mensuel",
  "H" = "Horaire",
  "J" = "Journalier"
)

# ============================================================
# 5. LABELS DE SITUATION MATRIMONIALE
# ============================================================

SITUATION_MATRIMONIALE_LABELS <- list(
  "C" = "Célibataire",
  "M" = "Marié(e)",
  "D" = "Divorcé(e)",
  "V" = "Veuf/Veuve",
  "1" = "Célibataire",
  "2" = "Marié(e)",
  "3" = "Divorcé(e)",
  "4" = "Veuf/Veuve"
)

# ============================================================
# 6. CLASSES D'ÂGE DÉTAILLÉES (17 classes)
# ============================================================

CLASSE_AGE_BREAKS <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, Inf)

CLASSE_AGE_LABELS <- c(
  "< 1 an",
  "1 - 4 ans",
  "5 - 9 ans",
  "10 - 14 ans",
  "15 - 19 ans",
  "20 - 24 ans",
  "25 - 29 ans",
  "30 - 34 ans",
  "35 - 39 ans",
  "40 - 44 ans",
  "45 - 49 ans",
  "50 - 54 ans",
  "55 - 59 ans",
  "60 - 64 ans",
  "65 - 69 ans",
  "70 - 74 ans",
  "75 ans et plus"
)

CLASSE_AGE_LABELS_LIST <- list(
  "1"  = "< 1 an",
  "2"  = "1 - 4 ans",
  "3"  = "5 - 9 ans",
  "4"  = "10 - 14 ans",
  "5"  = "15 - 19 ans",
  "6"  = "20 - 24 ans",
  "7"  = "25 - 29 ans",
  "8"  = "30 - 34 ans",
  "9"  = "35 - 39 ans",
  "10" = "40 - 44 ans",
  "11" = "45 - 49 ans",
  "12" = "50 - 54 ans",
  "13" = "55 - 59 ans",
  "14" = "60 - 64 ans",
  "15" = "65 - 69 ans",
  "16" = "70 - 74 ans",
  "17" = "75 ans et plus"
)

# ============================================================
# 7. CLASSES D'ÂGE RÉDUITES (4 classes)
# ============================================================

CLASSE_AGE_REDUITE_BREAKS <- c(0, 5, 10, 20, Inf)

CLASSE_AGE_REDUITE_LABELS <- c(
  "< 5 ans",
  "5 - 9 ans",
  "10 - 19 ans",
  "20 ans et plus"
)

CLASSE_AGE_REDUITE_LABELS_LIST <- list(
  "1" = "< 5 ans",
  "2" = "5 - 9 ans",
  "3" = "10 - 19 ans",
  "4" = "20 ans et plus"
)

# ============================================================
# 8. CLASSES D'EFFECTIF DÉTAILLÉES (10 classes)
# ============================================================

CLASSE_EFFECTIF_BREAKS <- c(0, 6, 21, 51, 101, 201, 501, 1001, 5001, 10001, Inf)

CLASSE_EFFECTIF_LABELS <- c(
  "1 à 5",
  "6 à 20",
  "21 à 50",
  "51 à 100",
  "101 à 200",
  "201 à 500",
  "501 à 1000",
  "1001 à 5000",
  "5001 à 10000",
  "Plus de 10000"
)

CLASSE_EFFECTIF_LABELS_LIST <- list(
  "1"  = "1 à 5",
  "2"  = "6 à 20",
  "3"  = "21 à 50",
  "4"  = "51 à 100",
  "5"  = "101 à 200",
  "6"  = "201 à 500",
  "7"  = "501 à 1000",
  "8"  = "1001 à 5000",
  "9"  = "5001 à 10000",
  "10" = "Plus de 10000"
)

# ============================================================
# 9. CLASSES D'EFFECTIF RÉDUITES (4 classes)
# ============================================================

CLASSE_EFFECTIF_REDUITE_BREAKS <- c(0, 21, 101, 501, Inf)

CLASSE_EFFECTIF_REDUITE_LABELS <- c(
  "1 à 20",
  "21 à 100",
  "101 à 500",
  "Plus de 500"
)

CLASSE_EFFECTIF_REDUITE_LABELS_LIST <- list(
  "1" = "1 à 20",
  "2" = "21 à 100",
  "3" = "101 à 500",
  "4" = "Plus de 500"
)

# Mapping ancien format (pour compatibilité)
CLASSE_EFFECTIF_REDUITE_MAPPING <- list(
  "TPE" = c("1", "2"),
  "PE"  = c("3", "4"),
  "ME"  = c("5", "6"),
  "GE"  = c("7", "8", "9", "10")
)

# ============================================================
# 10. ANCIENNETÉ (ancien format - compatibilité)
# ============================================================

ANCIENNETE_BREAKS <- c(0, 1, 3, 5, 10, 20, Inf)
ANCIENNETE_LABELS <- c(
  "Moins de 1 an",
  "1-3 ans",
  "3-5 ans",
  "5-10 ans",
  "10-20 ans",
  "Plus de 20 ans"
)

# ============================================================
# 11. ÂGE ENTREPRISE (ancien format - compatibilité)
# ============================================================

AGE_ENTREPRISE_BREAKS <- c(0, 2, 5, 10, 20, 50, Inf)
AGE_ENTREPRISE_LABELS <- c(
  "Moins de 2 ans",
  "2-5 ans",
  "5-10 ans",
  "10-20 ans",
  "20-50 ans",
  "Plus de 50 ans"
)

# ============================================================
# 12. GROUPES POUR CALCUL DES STATISTIQUES
# ============================================================

STAT_GROUPS <- list(
  
  # Âge employé (réduit)
  age_employe = list(
    var   = "CL_RED_AGE_EMPLOYE",
    title = "Salaire mensuel brut – Par tranche d'âge des employés",
    sheet = "Age_Employe"
  ),
  
  # Âge employé (détaillé)
  age_employe_det = list(
    var   = "CL_AGE_EMPLOYE",
    title = "Salaire mensuel brut – Par tranche d'âge des employés (détaillé)",
    sheet = "Age_Employe_Det"
  ),
  
  # Ancienneté entreprise (réduit)
  anciennete_entreprise = list(
    var   = "CL_RED_ANCIENNETE_ENTREPRISE",
    title = "Salaire mensuel brut – Par ancienneté dans l'entreprise",
    sheet = "Anciennete_Entreprise"
  ),
  
  # Ancienneté entreprise (détaillé)
  anciennete_entreprise_det = list(
    var   = "CL_ANCIENNETE_ENTREPRISE",
    title = "Salaire mensuel brut – Par ancienneté dans l'entreprise (détaillé)",
    sheet = "Anciennete_Entrep_Det"
  ),
  
  # Ancienneté immatriculation (réduit)
  anciennete_immat = list(
    var   = "CL_RED_ANCIENNETE_IMMAT",
    title = "Salaire mensuel brut – Par ancienneté immatriculation",
    sheet = "Anciennete_Immat"
  ),
  
  # Âge entreprise (réduit)
  age_entreprise = list(
    var   = "CL_RED_AGE_ENTREPRISE_IMMAT",
    title = "Salaire mensuel brut – Par âge de l'entreprise",
    sheet = "Age_Entreprise"
  ),
  
  # Âge entreprise (détaillé)
  age_entreprise_det = list(
    var   = "CL_AGE_ENTREPRISE_IMMAT",
    title = "Salaire mensuel brut – Par âge de l'entreprise (détaillé)",
    sheet = "Age_Entreprise_Det"
  ),
  
  # Secteur d'activité
  secteur_activite = list(
    var   = "SECTEUR_ACTIVITE_COD",
    title = "Salaire mensuel brut – Par secteur d'activité",
    sheet = "Secteur_Activite"
  ),
  
  # Taille entreprise (détaillée)
  taille_entreprise = list(
    var   = "CLASSE_EFFECTIF",
    title = "Salaire mensuel brut – Par taille d'entreprise",
    sheet = "Taille_Entreprise"
  ),
  
  # Taille entreprise (réduite)
  taille_entreprise_red = list(
    var   = "CLASSE_EFFECTIF_REDUITE",
    title = "Salaire mensuel brut – Par taille d'entreprise (réduite)",
    sheet = "Taille_Entrep_Red"
  ),
  
  # Sexe
  sexe = list(
    var   = "SEXE",
    title = "Salaire mensuel brut – Par sexe",
    sheet = "Sexe"
  ),
  
  # Commune
  commune = list(
    var   = "COMMUNE",
    title = "Salaire mensuel brut – Par commune",
    sheet = "Commune"
  )
)

# ============================================================
# 13. COLONNES À EXCLURE DES EXPORTS
# ============================================================

EXCLUDE_FROM_EXPORT <- c(
  "merge_status",
  "temp_id",
  "row_number",
  "w_jt_raw",
  "w_ijt_raw"
)

# ============================================================
# 14. FORMATS DE DATE RECONNUS
# ============================================================

DATE_FORMATS <- c(
  "DD/MM/YYYY"          = "^\\d{2}/\\d{2}/\\d{4}$",
  "DD/MM/YYYY HH:MM:SS" = "^\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}$",
  "YYYY-MM-DD"          = "^\\d{4}-\\d{2}-\\d{2}$",
  "YYYY-MM-DD HH:MM:SS" = "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$",
  "YYYYMMDD"            = "^\\d{8}$",
  "EXCEL_NUMERIC"       = "^\\d+(\\.\\d+)?$"
)

# ============================================================
# 15. FONCTIONS UTILITAIRES
# ============================================================

#' Obtenir le type attendu d'une colonne
#' @param col_name Nom de la colonne
#' @return Type attendu ou NULL si non défini
get_column_type <- function(col_name) {
  if (col_name %in% names(COLUMN_TYPES)) {
    return(COLUMN_TYPES[[col_name]])
  }
  return(NULL)
}

#' Obtenir le label d'un code
#' @param code Code à traduire
#' @param mapping Liste de mapping
#' @return Label correspondant ou le code original
get_label <- function(code, mapping) {
  code_str <- as.character(code)
  if (code_str %in% names(mapping)) {
    return(mapping[[code_str]])
  }
  return(code_str)
}

#' Appliquer les labels à un vecteur
#' @param x Vecteur de codes
#' @param mapping Liste de mapping
#' @return Vecteur de labels
apply_labels <- function(x, mapping) {
  sapply(x, function(code) get_label(code, mapping))
}

#' Créer les classes d'âge détaillées (17 classes)
#' @param x Vecteur numérique (années)
#' @return Facteur avec les classes
create_classe_age <- function(x) {
  
  classe <- case_when(
    is.na(x) ~ NA_integer_,
    x < 1 ~ 1L,
    x >= 1 & x <= 4 ~ 2L,
    x >= 5 & x <= 9 ~ 3L,
    x >= 10 & x <= 14 ~ 4L,
    x >= 15 & x <= 19 ~ 5L,
    x >= 20 & x <= 24 ~ 6L,
    x >= 25 & x <= 29 ~ 7L,
    x >= 30 & x <= 34 ~ 8L,
    x >= 35 & x <= 39 ~ 9L,
    x >= 40 & x <= 44 ~ 10L,
    x >= 45 & x <= 49 ~ 11L,
    x >= 50 & x <= 54 ~ 12L,
    x >= 55 & x <= 59 ~ 13L,
    x >= 60 & x <= 64 ~ 14L,
    x >= 65 & x <= 69 ~ 15L,
    x >= 70 & x <= 74 ~ 16L,
    x >= 75 ~ 17L
  )
  
  factor(classe, levels = 1:17, labels = CLASSE_AGE_LABELS)
}

#' Créer les classes d'âge réduites (4 classes)
#' @param x Vecteur numérique (années)
#' @return Facteur avec les classes
create_classe_age_reduite <- function(x) {
  
  classe <- case_when(
    is.na(x) ~ NA_integer_,
    x < 5 ~ 1L,
    x >= 5 & x <= 9 ~ 2L,
    x >= 10 & x <= 19 ~ 3L,
    x >= 20 ~ 4L
  )
  
  factor(classe, levels = 1:4, labels = CLASSE_AGE_REDUITE_LABELS)
}

#' Créer les classes d'effectif détaillées (10 classes)
#' @param x Vecteur numérique (effectif)
#' @return Facteur avec les classes
create_classe_effectif <- function(x) {
  
  classe <- case_when(
    is.na(x) ~ NA_integer_,
    x >= 1 & x <= 5 ~ 1L,
    x >= 6 & x <= 20 ~ 2L,
    x >= 21 & x <= 50 ~ 3L,
    x >= 51 & x <= 100 ~ 4L,
    x >= 101 & x <= 200 ~ 5L,
    x >= 201 & x <= 500 ~ 6L,
    x >= 501 & x <= 1000 ~ 7L,
    x >= 1001 & x <= 5000 ~ 8L,
    x >= 5001 & x <= 10000 ~ 9L,
    x > 10000 ~ 10L
  )
  
  factor(classe, levels = 1:10, labels = CLASSE_EFFECTIF_LABELS)
}

#' Créer les classes d'effectif réduites (4 classes)
#' @param x Vecteur numérique (effectif)
#' @return Facteur avec les classes
create_classe_effectif_reduite <- function(x) {
  
  classe <- case_when(
    is.na(x) ~ NA_integer_,
    x >= 1 & x <= 20 ~ 1L,
    x >= 21 & x <= 100 ~ 2L,
    x >= 101 & x <= 500 ~ 3L,
    x > 500 ~ 4L
  )
  
  factor(classe, levels = 1:4, labels = CLASSE_EFFECTIF_REDUITE_LABELS)
}

#' Créer des classes d'ancienneté (ancien format)
#' @param x Vecteur d'années d'ancienneté
#' @return Facteur avec les classes
create_anciennete_classes <- function(x) {
  cut(x, 
      breaks = ANCIENNETE_BREAKS, 
      labels = ANCIENNETE_LABELS,
      right = FALSE,
      include.lowest = TRUE)
}

#' Créer des classes d'âge entreprise (ancien format)
#' @param x Vecteur d'années d'âge
#' @return Facteur avec les classes
create_age_entreprise_classes <- function(x) {
  cut(x, 
      breaks = AGE_ENTREPRISE_BREAKS, 
      labels = AGE_ENTREPRISE_LABELS,
      right = FALSE,
      include.lowest = TRUE)
}

#' Vérifier si toutes les variables requises sont présentes
#' @param df Data frame à vérifier
#' @param category Catégorie de variables
#' @return Liste avec status et variables manquantes
check_required_vars <- function(df, category = NULL) {
  
  if (is.null(category)) {
    all_required <- unlist(REQUIRED_VARS)
  } else {
    all_required <- REQUIRED_VARS[[category]]
  }
  
  missing <- setdiff(all_required, names(df))
  
  return(list(
    valid   = length(missing) == 0,
    missing = missing
  ))
}

#' Obtenir la configuration d'un groupe de statistiques
#' @param group_name Nom du groupe
#' @return Configuration du groupe ou NULL
get_stat_group_config <- function(group_name) {
  if (group_name %in% names(STAT_GROUPS)) {
    return(STAT_GROUPS[[group_name]])
  }
  return(NULL)
}

#' Lister toutes les variables de classe disponibles
#' @return Vecteur des noms de variables de classe
get_class_variables <- function() {
  c(
    # Classes d'âge détaillées
    "CL_AGE_EMPLOYE",
    "CL_ANCIENNETE_ENTREPRISE",
    "CL_ANCIENNETE_IMMAT",
    "CL_AGE_ENTREPRISE_IMMAT",
    # Classes d'âge réduites
    "CL_RED_AGE_EMPLOYE",
    "CL_RED_ANCIENNETE_ENTREPRISE",
    "CL_RED_ANCIENNETE_IMMAT",
    "CL_RED_AGE_ENTREPRISE_IMMAT",
    # Classes d'effectif
    "CLASSE_EFFECTIF",
    "CLASSE_EFFECTIF_REDUITE"
  )
}

#' Obtenir les labels pour une variable de classe
#' @param var_name Nom de la variable
#' @return Liste des labels ou NULL
get_class_labels <- function(var_name) {
  
  if (grepl("^CL_RED_", var_name)) {
    return(CLASSE_AGE_REDUITE_LABELS_LIST)
  }
  
  if (grepl("^CL_", var_name)) {
    return(CLASSE_AGE_LABELS_LIST)
  }
  
  if (var_name == "CLASSE_EFFECTIF") {
    return(CLASSE_EFFECTIF_LABELS_LIST)
  }
  
  if (var_name == "CLASSE_EFFECTIF_REDUITE") {
    return(CLASSE_EFFECTIF_REDUITE_LABELS_LIST)
  }
  
  return(NULL)
}

# ============================================================
# MESSAGE DE CONFIRMATION
# ============================================================

message("Dictionnaires chargés depuis dictionaries.R")
message("  - ", length(COLUMN_TYPES), " types de colonnes définis")
message("  - ", length(STAT_GROUPS), " groupes statistiques définis")
message("  - ", length(CLASSE_AGE_LABELS), " classes d'âge détaillées")
message("  - ", length(CLASSE_AGE_REDUITE_LABELS), " classes d'âge réduites")
message("  - ", length(CLASSE_EFFECTIF_LABELS), " classes d'effectif détaillées")
message("  - ", length(CLASSE_EFFECTIF_REDUITE_LABELS), " classes d'effectif réduites")