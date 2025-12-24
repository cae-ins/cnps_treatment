# ============================================================
# STAT_DIMENSIONS.R
# Définition déclarative des catégories analytiques
# ============================================================

STAT_DIMENSIONS <- list(
  
  global = list(
    enabled = TRUE,
    label = "National",
    min_cell_size = 1
  ),
  
  sector = list(
    enabled = TRUE,
    variable = "SECTEUR_ACTIVITE_COD",
    label = "Secteur d'activité",
    min_cell_size = 30,
    include_in_ipw = TRUE,
    include_in_imputation = TRUE
  ),
  
  # Âge de l'employé
  age_employe = list(
    enabled = TRUE,
    variable = "CL_RED_AGE_EMPLOYE",
    label = "Tranche d'âge employé",
    min_cell_size = 30,
    include_in_ipw = FALSE,
    include_in_imputation = TRUE
  ),
  
  # Ancienneté dans l'entreprise
  anciennete_entreprise = list(
    enabled = TRUE,
    variable = "CL_RED_ANCIENNETE_ENTREPRISE",
    label = "Ancienneté entreprise",
    min_cell_size = 30,
    include_in_ipw = TRUE,
    include_in_imputation = TRUE
  ),
  
  # Ancienneté immatriculation
  anciennete_immat = list(
    enabled = TRUE,
    variable = "CL_RED_ANCIENNETE_IMMAT",
    label = "Ancienneté immatriculation",
    min_cell_size = 30,
    include_in_ipw = TRUE,
    include_in_imputation = TRUE
  ),
  
  # Âge de l'entreprise
  age_entreprise = list(
    enabled = TRUE,
    variable = "CL_RED_AGE_ENTREPRISE_IMMAT",
    label = "Âge entreprise",
    min_cell_size = 30,
    include_in_ipw = TRUE,
    include_in_imputation = TRUE
  ),
  
  # Taille entreprise (détaillée)
  taille_entreprise = list(
    enabled = TRUE,
    variable = "CLASSE_EFFECTIF",
    label = "Taille entreprise (détaillée)",
    min_cell_size = 30,
    include_in_ipw = TRUE,
    include_in_imputation = TRUE
  ),
  
  # Taille entreprise (réduite)
  taille_entreprise_red = list(
    enabled = TRUE,
    variable = "CLASSE_EFFECTIF_REDUITE",
    label = "Taille entreprise (réduite)",
    min_cell_size = 30,
    include_in_ipw = TRUE,
    include_in_imputation = TRUE
  ),
  
  # Sexe
  sex = list(
    enabled = TRUE,
    variable = "SEXE",
    label = "Sexe",
    min_cell_size = 30,
    include_in_ipw = FALSE,
    include_in_imputation = TRUE
  ),
  
  # Commune
  commune = list(
    enabled = TRUE,
    variable = "COMMUNE",
    label = "Commune",
    min_cell_size = 50,
    include_in_ipw = FALSE,
    include_in_imputation = FALSE
  ),
  
  # CSP
  csp = list(
    enabled = TRUE,
    variable = "CSP_CODE",
    label = "Catégorie socio-professionnelle",
    min_cell_size = 30,
    include_in_ipw = TRUE,
    include_in_imputation = TRUE
  ),
  
  # Profession
  profession = list(
    enabled = TRUE,
    variable = "PROF_CODE",
    label = "Profession",
    min_cell_size = 50,
    include_in_ipw = FALSE,
    include_in_imputation = TRUE
  )
)

# ============================================================
# FONCTIONS UTILITAIRES
# ============================================================

get_enabled_dimensions <- function() {
  dims <- names(STAT_DIMENSIONS)[sapply(STAT_DIMENSIONS, function(x) x$enabled)]
  return(dims)
}

get_dimension_variable <- function(dim_name) {
  if (dim_name %in% names(STAT_DIMENSIONS)) {
    return(STAT_DIMENSIONS[[dim_name]]$variable)
  }
  return(NULL)
}

get_ipw_dimensions <- function() {
  dims <- names(STAT_DIMENSIONS)[sapply(STAT_DIMENSIONS, function(x) {
    isTRUE(x$include_in_ipw)
  })]
  return(dims)
}

get_imputation_dimensions <- function() {
  dims <- names(STAT_DIMENSIONS)[sapply(STAT_DIMENSIONS, function(x) {
    isTRUE(x$include_in_imputation)
  })]
  return(dims)
}

# Obtenir toutes les variables de dimension
get_all_dimension_variables <- function() {
  vars <- sapply(STAT_DIMENSIONS, function(x) x$variable)
  vars <- vars[!sapply(vars, is.null)]
  return(unique(unname(vars)))
}

message("Dimensions statistiques chargées depuis stat_dimensions.R")