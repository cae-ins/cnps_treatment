# ============================================================
# DICTIONARIES.R
# Dictionnaires de variables, types et mappings
# ============================================================

# ------------------------------------------------------------
# DICTIONNAIRE DES TYPES DE COLONNES
# ------------------------------------------------------------
# Types possibles: "character", "double", "integer", "datetime", "logical"

COLUMN_TYPES <- list(
  
  # === IDENTIFIANTS ===
  ID_INDIV                  = "character",
  ID_EMPLOI                 = "character",
  NUMERO_EMPLOYEUR          = "double",
  NUMERO_EMPLOYEUR_ALT      = "character",
  
  # === INFORMATIONS PERSONNELLES ===
  SEXE                      = "character",
  DATE_NAISSANCE            = "datetime",
  SITUATION_MATRIMONIALE    = "character",
  NIVEAU_ETUDE              = "character",
  NIVEAU_ETUDE_ALT          = "character",
  
  # === INFORMATIONS PROFESSIONNELLES ===
  PROFESSION                = "character",
  STATUT_TRAVAILLEUR        = "character",
  TYPE_SALARIE              = "character",
  DATE_EMBAUCHE             = "datetime",
  DATE_IMMATRICULATION      = "datetime",
  
  # === INFORMATIONS ENTREPRISE ===
  RAISON_SOCIALE            = "character",
  SECTEUR_ACTIVITE          = "character",
  CATEGORIE_ENTREPRISE      = "double",
  DATE_DEBUT_ACT_EMPLOY     = "datetime",
  DATE_IMMAT_EMPLOYEUR      = "datetime",
  EFFECTIF_SALARIES         = "double",
  
  # === LOCALISATION ===
  LIBELLE_AGENCE            = "character",
  COMMUNE                   = "character",
  
  # === DONNÉES SALARIALES ===
  SALAIRE_BRUT              = "double",
  DUREE_TRAVAILLEE          = "double",
  
  # === PÉRIODE ===
  MOIS                      = "double",
  ANNEE                     = "double"
)

# ------------------------------------------------------------
# VARIABLES REQUISES PAR CATÉGORIE
# ------------------------------------------------------------
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
  )
)

# ------------------------------------------------------------
# MAPPING DES CODES SEXE
# ------------------------------------------------------------
SEXE_LABELS <- list(
  "M" = "Masculin",
  "F" = "Féminin",
  "1" = "Masculin",
  "2" = "Féminin"
)

# ------------------------------------------------------------
# MAPPING DES TYPES DE SALARIÉS
# ------------------------------------------------------------
TYPE_SALARIE_LABELS <- list(
  "M" = "Mensuel",
  "H" = "Horaire",
  "J" = "Journalier"
)

# ------------------------------------------------------------
# MAPPING DES SITUATIONS MATRIMONIALES
# ------------------------------------------------------------
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

# ------------------------------------------------------------
# CLASSES D'EFFECTIFS (TAILLE ENTREPRISE)
# ------------------------------------------------------------
CLASSE_EFFECTIF_LABELS <- list(
  "1"  = "1-5 salariés",
  "2"  = "6-10 salariés",
  "3"  = "11-20 salariés",
  "4"  = "21-50 salariés",
  "5"  = "51-100 salariés",
  "6"  = "101-200 salariés",
  "7"  = "201-500 salariés",
  "8"  = "501-1000 salariés",
  "9"  = "Plus de 1000 salariés"
)

# Classes réduites pour les statistiques
CLASSE_EFFECTIF_REDUITE <- list(
  "TPE"  = c("1", "2"),           # Très Petite Entreprise (1-10)
  "PE"   = c("3", "4"),           # Petite Entreprise (11-50)
  "ME"   = c("5", "6"),           # Moyenne Entreprise (51-200)
  "GE"   = c("7", "8", "9")       # Grande Entreprise (200+)
)

# ------------------------------------------------------------
# CLASSES D'ANCIENNETÉ
# ------------------------------------------------------------
ANCIENNETE_BREAKS <- c(0, 1, 3, 5, 10, 20, Inf)
ANCIENNETE_LABELS <- c(
  "Moins de 1 an",
  "1-3 ans",
  "3-5 ans",
  "5-10 ans",
  "10-20 ans",
  "Plus de 20 ans"
)

# ------------------------------------------------------------
# CLASSES D'ÂGE ENTREPRISE
# ------------------------------------------------------------
AGE_ENTREPRISE_BREAKS <- c(0, 2, 5, 10, 20, 50, Inf)
AGE_ENTREPRISE_LABELS <- c(
  "Moins de 2 ans",
  "2-5 ans",
  "5-10 ans",
  "10-20 ans",
  "20-50 ans",
  "Plus de 50 ans"
)

# ------------------------------------------------------------
# GROUPES POUR CALCUL DES STATISTIQUES
# ------------------------------------------------------------
STAT_GROUPS <- list(
  
  anciennete_employe = list(
    var   = "CL_RED_ANCIENNETE_IMMAT",
    title = "Salaire mensuel brut – Par ancienneté des employés",
    sheet = "Anciennete_Employe"
  ),
  
  age_entreprise = list(
    var   = "CL_RED_AGE_ENTREPRISE_IMMAT",
    title = "Salaire mensuel brut – Par âge de l'entreprise",
    sheet = "Age_Entreprise"
  ),
  
  secteur_activite = list(
    var   = "SECTEUR_ACTIVITE_COD",
    title = "Salaire mensuel brut – Par secteur d'activité",
    sheet = "Secteur_Activite"
  ),
  
  taille_entreprise = list(
    var   = "CLASSE_EFFECTIF_REDUITE",
    title = "Salaire mensuel brut – Par taille d'entreprise",
    sheet = "Taille_Entreprise"
  ),
  
  sexe = list(
    var   = "SEXE",
    title = "Salaire mensuel brut – Par sexe",
    sheet = "Sexe"
  ),
  
  commune = list(
    var   = "COMMUNE",
    title = "Salaire mensuel brut – Par commune",
    sheet = "Commune"
  )
)

# ------------------------------------------------------------
# FONCTIONS UTILITAIRES POUR LES DICTIONNAIRES
# ------------------------------------------------------------

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
#' @param mapping Liste de mapping (ex: SEXE_LABELS)
#' @return Label correspondant ou le code original si non trouvé
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

#' Créer des classes d'ancienneté
#' @param x Vecteur d'années d'ancienneté
#' @return Facteur avec les classes
create_anciennete_classes <- function(x) {
  cut(x, 
      breaks = ANCIENNETE_BREAKS, 
      labels = ANCIENNETE_LABELS,
      right = FALSE,
      include.lowest = TRUE)
}

#' Créer des classes d'âge entreprise
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
#' @param category Catégorie de variables ("numeric", "character", "id", "period")
#' @return Liste avec status et variables manquantes
check_required_vars <- function(df, category = NULL) {
  
  if (is.null(category)) {
    # Vérifier toutes les catégories
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

# ------------------------------------------------------------
# LISTE DES COLONNES À EXCLURE DES EXPORTS
# ------------------------------------------------------------
EXCLUDE_FROM_EXPORT <- c(
  "merge_status",
  "temp_id",
  "row_number"
)

# ------------------------------------------------------------
# FORMATS DE DATE RECONNUS
# ------------------------------------------------------------
DATE_FORMATS <- c(
  "DD/MM/YYYY"         = "^\\d{2}/\\d{2}/\\d{4}$",
  "DD/MM/YYYY HH:MM:SS" = "^\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}$",
  "YYYY-MM-DD"         = "^\\d{4}-\\d{2}-\\d{2}$",
  "YYYY-MM-DD HH:MM:SS" = "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$",
  "YYYYMMDD"           = "^\\d{8}$",
  "EXCEL_NUMERIC"      = "^\\d+(\\.\\d+)?$"
)

# ------------------------------------------------------------
# MESSAGE DE CONFIRMATION
# ------------------------------------------------------------
message("Dictionnaires chargés depuis dictionaries.R")