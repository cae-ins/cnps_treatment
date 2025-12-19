# ============================================================
# 03_ADD_MOIS_ANNEE.R
# Ajout des colonnes MOIS et ANNEE depuis le nom du fichier
# ============================================================

# ------------------------------------------------------------
# CHARGER LA CONFIGURATION
# ------------------------------------------------------------
if (exists("PIPELINE_ROOT")) {
  config_path <- file.path(PIPELINE_ROOT, "config/config.R")
} else {
  config_path <- file.path(dirname(sys.frame(1)$ofile), "../config/config.R")
}
source(config_path)
source(file.path(PATHS$scripts, "00_init_project.R"))

# ------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------
library(haven)
library(stringr)
library(progress)

# ------------------------------------------------------------
# FONCTION PRINCIPALE
# ------------------------------------------------------------
add_mois_annee <- function(files_to_process = NULL,
                            input_folder = NULL,
                            replace_original = TRUE) {
  
  message("\n", strrep("=", 60))
  message("ÉTAPE 3: AJOUT DES COLONNES MOIS/ANNEE")
  message(strrep("=", 60))
  
  # Paramètres par défaut
  if (is.null(input_folder)) input_folder <- PATHS$processed_dta
  
  # Obtenir les fichiers à traiter
  if (is.null(files_to_process)) {
    files_to_process <- list.files(
      input_folder, 
      pattern = "\\.dta$", 
      full.names = TRUE,
      ignore.case = TRUE
    )
  }
  
  if (length(files_to_process) == 0) {
    message("Aucun fichier à traiter")
    return(invisible(NULL))
  }
  
  message("Fichiers à traiter: ", length(files_to_process))
  
  # Résultats
  results <- list(success = character(), skipped = character(), errors = character())
  
  # Barre de progression
  pb <- progress_bar$new(
    format = "  Traitement [:bar] :percent (:current/:total)",
    total = length(files_to_process),
    width = 60
  )
  
  # Traitement
  for (filepath in files_to_process) {
    pb$tick()
    
    filename <- basename(filepath)
    
    # Extraire période depuis le nom
    period <- extract_period_from_filename(filename)
    
    if (!period$valid) {
      message("\n  ⚠ Format invalide: ", filename, " – ignoré")
      results$skipped <- c(results$skipped, filename)
      next
    }
    
    tryCatch({
      # Charger les données
      data <- read_dta(filepath)
      
      # Ajouter MOIS et ANNEE
      data$MOIS  <- period$mois
      data$ANNEE <- period$annee
      
      # Sauvegarder
      if (replace_original) {
        save_path <- filepath
      } else {
        save_path <- file.path(
          dirname(filepath),
          "with_period",
          filename
        )
        dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
      }
      
      write_dta(data, save_path, version = STATA_VERSION)
      
      # Mettre à jour le registry
      source_file <- paste0(tools::file_path_sans_ext(filename), ".xlsx")
      update_registry(
        filename       = filename,
        source_file    = source_file,
        step_completed = "03_add_mois_annee",
        status         = "in_progress",
        n_rows         = nrow(data),
        n_cols         = ncol(data)
      )
      
      results$success <- c(results$success, filename)
      
    }, error = function(e) {
      message("\n  ✖ ERREUR ", filename, ": ", e$message)
      results$errors <- c(results$errors, filename)
    })
  }
  
  # Résumé
  message("\n", strrep("-", 40))
  message("Résumé:")
  message("  Succès:  ", length(results$success))
  message("  Ignorés: ", length(results$skipped))
  message("  Erreurs: ", length(results$errors))
  
  return(results)
}

# ------------------------------------------------------------
# EXÉCUTION SI APPELÉ DIRECTEMENT
# ------------------------------------------------------------
if (sys.nframe() == 0) {
  add_mois_annee()
}