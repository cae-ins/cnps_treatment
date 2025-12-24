# ============================================================
# CONFIG.R
# CONFIGURATION CENTRALE DU PIPELINE CNPS
# ============================================================

# Déterminer le chemin du dossier config
if (exists("PIPELINE_ROOT")) {
  CONFIG_DIR <- file.path(PIPELINE_ROOT, "config")
} else {
  CONFIG_DIR <- "C:/Users/e_koffie/Documents/Salaires/CNPS_TREATMENT/config"
}

# Charger les sous-fichiers de configuration
source(file.path(CONFIG_DIR, "paths.R"))
source(file.path(CONFIG_DIR, "dictionaries.R"))
source(file.path(CONFIG_DIR, "merge_configs.R"))
source(file.path(CONFIG_DIR, "stat_dimensions.R"))
source(file.path(CONFIG_DIR, "stat_definitions.R"))
source(file.path(CONFIG_DIR, "model_specs.R"))
source(file.path(CONFIG_DIR, "estimators.R"))
source(file.path(CONFIG_DIR, "validation_rules.R"))

# ------------------------------------------------------------
# PARAMÈTRES STATA
# ------------------------------------------------------------
STATA_VERSION <- 15

# ------------------------------------------------------------
# PARAMÈTRES DE TRAITEMENT
# ------------------------------------------------------------
PROCESSING <- list(
  remove_duplicates = TRUE,
  duplicate_scope = "file",
  apply_winsor = TRUE,
  winsor_percentile = 0.01,
  min_salaire_mensuel = 75000,
  exclude_type_salarie = c("H", "J"),
  archive_replaced_files = TRUE,
  verbose = TRUE,
  n_imputations = 5,
  ipw_trim_quantile = c(0.01, 0.99),
  stabilize_weights = TRUE
)

# ------------------------------------------------------------
# PARAMÈTRES D'AUDIT
# ------------------------------------------------------------
AUDIT <- list(
  outlier_vars = c("SALAIRE_BRUT"),
  iqr_multiplier = 1.5,
  max_pct_missing = 20,
  max_pct_duplicates = 5
)

# ------------------------------------------------------------
# FONCTIONS UTILITAIRES GLOBALES
# ------------------------------------------------------------

get_timestamp <- function(format = "%Y%m%d_%H%M%S") {
  format(Sys.time(), format)
}

extract_period_from_filename <- function(filename) {
  name <- tools::file_path_sans_ext(basename(filename))
  if (nchar(name) >= 7 && substr(name, 3, 3) == "_") {
    mois <- as.integer(substr(name, 1, 2))
    annee <- as.integer(substr(name, 4, 7))
    if (!is.na(mois) && mois >= 1 && mois <= 12 &&
        !is.na(annee) && annee >= 1900 && annee <= 2100) {
      return(list(mois = mois, annee = annee, valid = TRUE))
    }
  }
  return(list(mois = NA, annee = NA, valid = FALSE))
}

format_period <- function(mois, annee) {
  sprintf("%02d_%04d", mois, annee)
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  dir.exists(path)
}

archive_file <- function(filepath) {
  if (!file.exists(filepath)) return(NULL)
  if (!PROCESSING$archive_replaced_files) return(NULL)
  
  filename <- basename(filepath)
  timestamp <- get_timestamp()
  archive_name <- paste0(
    tools::file_path_sans_ext(filename),
    "_archived_", timestamp, ".",
    tools::file_ext(filename)
  )
  archive_path <- file.path(PATHS$archive, archive_name)
  ensure_dir(PATHS$archive)
  file.copy(filepath, archive_path)
  if (PROCESSING$verbose) message("  Archivé: ", archive_path)
  return(archive_path)
}

message("Configuration CNPS Pipeline chargée")