# ============================================================
# 02_ADD_MOIS_ANNEE.R
# Ajout des colonnes MOIS et ANNEE depuis le nom du fichier
# ============================================================

library(haven)
library(stringr)

# Fonction principale
add_mois_annee <- function(files_to_process = NULL,
                           input_folder = NULL,
                           replace_original = TRUE) {
  
  message("\n", strrep("=", 60))
  message("AJOUT DES COLONNES MOIS/ANNEE")
  message(strrep("=", 60))
  
  if (is.null(input_folder)) input_folder <- PATHS$processed_dta
  
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
  
  results <- list(success = character(), skipped = character(), errors = character())
  
  for (filepath in files_to_process) {
    filename <- basename(filepath)
    period <- extract_period_from_filename(filename)
    
    if (!period$valid) {
      message("  ⚠ Format invalide: ", filename, " – ignoré")
      results$skipped <- c(results$skipped, filename)
      next
    }
    
    tryCatch({
      data <- read_dta(filepath)
      
      data$MOIS <- period$mois
      data$ANNEE <- period$annee
      
      if (replace_original) {
        save_path <- filepath
      } else {
        save_path <- file.path(dirname(filepath), "with_period", filename)
        dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
      }
      
      write_dta(data, save_path, version = STATA_VERSION)
      
      message("  ✓ ", filename, " (", period$mois, "/", period$annee, ")")
      results$success <- c(results$success, filename)
      
    }, error = function(e) {
      message("  ✖ ERREUR ", filename, ": ", e$message)
      results$errors <- c(results$errors, filename)
    })
  }
  
  message("\n", strrep("-", 40))
  message("Résumé:")
  message("  Succès:  ", length(results$success))
  message("  Ignorés: ", length(results$skipped))
  message("  Erreurs: ", length(results$errors))
  
  return(results)
}

message("Module d'ajout mois/année chargé")