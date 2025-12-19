# ============================================================
# CONFIG.R
# CONFIGURATION CENTRALE DU PIPELINE CNPS
# ============================================================

# ------------------------------------------------------------
# CHARGER LES SOUS-FICHIERS DE CONFIGURATION
# ------------------------------------------------------------

# Déterminer le chemin du dossier config
if (exists("PIPELINE_ROOT")) {
  CONFIG_DIR <- file.path(PIPELINE_ROOT, "config")
} else {
  # Si appelé directement, utiliser le chemin relatif
  CONFIG_DIR <- dirname(sys.frame(1)$ofile)
  if (is.null(CONFIG_DIR) || CONFIG_DIR == "") {
    CONFIG_DIR <- "C:/Users/e_koffie/Documents/Salaires/CNPS_TREATMENT/config"
  }
}

# Charger les chemins
source(file.path(CONFIG_DIR, "paths.R"))

# Charger les dictionnaires
source(file.path(CONFIG_DIR, "dictionaries.R"))

# ------------------------------------------------------------
# PARAMÈTRES STATA
# ------------------------------------------------------------
STATA_VERSION <-  15 # Stata 14+

# ------------------------------------------------------------
# PARAMÈTRES DE TRAITEMENT
# ------------------------------------------------------------
PROCESSING <- list(
  
  # === DÉDUPLICATION ===
  remove_duplicates = TRUE,
  duplicate_scope   = "file",    # "file" ou "global"
  
  # === WINSORISATION ===
  apply_winsor      = TRUE,
  winsor_percentile = 0.01,      # 1%
  
  # === SEUILS DE NETTOYAGE ===
  min_salaire_mensuel = 75000,   # SMIG Côte d'Ivoire
  
  # === TYPES DE SALARIÉS À EXCLURE ===
  exclude_type_salarie = c("H", "J"),  # Horaire, Journalier
  
  # === OPTIONS DE TRAITEMENT ===
  archive_replaced_files = TRUE,  # Archiver les fichiers remplacés
  verbose               = TRUE    # Afficher les messages détaillés
)

# ------------------------------------------------------------
# PARAMÈTRES D'AUDIT
# ------------------------------------------------------------
AUDIT <- list(
  
  # Variables pour détection outliers
  outlier_vars = c("SALAIRE_BRUT"),
  
  # Méthode IQR
  iqr_multiplier = 1.5,
  
  # Seuils de warning
  max_pct_missing = 20,     # Warning si > 20% manquants
 max_pct_duplicates = 5   # Warning si > 5% doublons
)

# ------------------------------------------------------------
# FONCTIONS UTILITAIRES GLOBALES
# ------------------------------------------------------------

#' Obtenir un timestamp formaté
#' @param format Format de date/heure
#' @return String formaté
get_timestamp <- function(format = "%Y%m%d_%H%M%S") {
  format(Sys.time(), format)
}

#' Extraire la période depuis un nom de fichier (MM_YYYY)
#' @param filename Nom du fichier
#' @return Liste avec mois, annee, valid
extract_period_from_filename <- function(filename) {
  name <- tools::file_path_sans_ext(basename(filename))
  
  if (nchar(name) >= 7 && substr(name, 3, 3) == "_") {
    mois  <- as.integer(substr(name, 1, 2))
    annee <- as.integer(substr(name, 4, 7))
    
    if (!is.na(mois) && mois >= 1 && mois <= 12 &&
        !is.na(annee) && annee >= 1900 && annee <= 2100) {
      return(list(mois = mois, annee = annee, valid = TRUE))
    }
  }
  
  return(list(mois = NA, annee = NA, valid = FALSE))
}

#' Formater une période en string
#' @param mois Mois (numérique)
#' @param annee Année (numérique)
#' @return String formaté "MM_YYYY"
format_period <- function(mois, annee) {
  sprintf("%02d_%04d", mois, annee)
}

#' Vérifier et créer un dossier si nécessaire
#' @param path Chemin du dossier
#' @return TRUE si existe ou créé, FALSE sinon
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  dir.exists(path)
}

#' Archiver un fichier avant remplacement
#' @param filepath Chemin du fichier à archiver
#' @return Chemin du fichier archivé ou NULL
archive_file <- function(filepath) {
  if (!file.exists(filepath)) return(NULL)
  
  if (!PROCESSING$archive_replaced_files) return(NULL)
  
  # Créer le nom du fichier archivé
  filename <- basename(filepath)
  timestamp <- get_timestamp()
  archive_name <- paste0(
    tools::file_path_sans_ext(filename),
    "_archived_",
    timestamp,
    ".",
    tools::file_ext(filename)
  )
  
  archive_path <- file.path(PATHS$archive, archive_name)
  
  # Copier le fichier
  ensure_dir(PATHS$archive)
  file.copy(filepath, archive_path)
  
  if (PROCESSING$verbose) {
    message("  Archivé: ", archive_path)
  }
  
  return(archive_path)
}

#' Charger et valider la configuration
#' @return TRUE si tout est OK
load_config <- function() {
  
  # Vérifier les chemins critiques
  critical_paths <- c("raw_excel", "processed_dta", "cleaned_final", "results")
  
  for (path_name in critical_paths) {
    path <- PATHS[[path_name]]
    if (!dir.exists(path)) {
      warning("Chemin critique manquant: ", path_name, " (", path, ")")
    }
  }
  
  # Vérifier les fichiers externes
  for (ext_name in names(EXTERNAL_FILES)) {
    ext_file <- EXTERNAL_FILES[[ext_name]]$path
    if (!file.exists(ext_file)) {
      warning("Fichier externe manquant: ", ext_name, " (", ext_file, ")")
    }
  }
  
  message("Configuration chargée avec succès")
  return(invisible(TRUE))
}

# ------------------------------------------------------------
# AFFICHER RÉSUMÉ DE LA CONFIGURATION
# ------------------------------------------------------------
print_config_summary <- function() {
  cat("\n")
  cat(strrep("=", 60), "\n")
  cat("  CONFIGURATION DU PIPELINE CNPS\n")
  cat(strrep("=", 60), "\n")
  
  cat("\nChemins principaux:\n")
  cat("  Projet:    ", PROJECT_ROOT, "\n")
  cat("  Excel:     ", PATHS$raw_excel, "\n")
  cat("  Processed: ", PATHS$processed_dta, "\n")
  cat("  Final:     ", PATHS$cleaned_final, "\n")
  cat("  Résultats: ", PATHS$results, "\n")
  
  cat("\nParamètres de traitement:\n")
  cat("  Déduplication:     ", PROCESSING$remove_duplicates, 
      " (", PROCESSING$duplicate_scope, ")\n", sep = "")
  cat("  Winsorisation:     ", PROCESSING$apply_winsor,
      " (", PROCESSING$winsor_percentile * 100, "%)\n", sep = "")
  cat("  Salaire min:       ", format(PROCESSING$min_salaire_mensuel, big.mark = " "), "\n")
  cat("  Types exclus:      ", paste(PROCESSING$exclude_type_salarie, collapse = ", "), "\n")
  
  cat("\nGroupes statistiques:\n")
  for (grp in names(STAT_GROUPS)) {
    cat("  - ", STAT_GROUPS[[grp]]$sheet, " (", STAT_GROUPS[[grp]]$var, ")\n", sep = "")
  }
  
  cat("\n")
}

# ------------------------------------------------------------
# MESSAGE DE CONFIRMATION
# ------------------------------------------------------------
message("Configuration CNPS Pipeline chargée")