# ============================================================
# 04_CONCAT_DATABASES.R
# Concaténation des bases de données mensuelles
# ============================================================

library(haven)
library(dplyr)
library(purrr)

# Fonction principale
concat_databases <- function(input_folder = NULL,
                             output_folder = NULL,
                             incremental = TRUE) {
  
  message("\n", strrep("=", 60))
  message("CONCATÉNATION DES BASES DE DONNÉES")
  message(strrep("=", 60))
  
  if (is.null(input_folder)) input_folder <- PATHS$processed_dta
  if (is.null(output_folder)) output_folder <- PATHS$cleaned_monthly
  
  # ----------------------------------------------------------
  # 1. LISTE DES FICHIERS
  # ----------------------------------------------------------
  files <- list.files(input_folder, pattern = "\\.dta$", full.names = TRUE)
  
  if (length(files) == 0) {
    message("Aucun fichier .dta trouvé dans: ", input_folder)
    return(invisible(NULL))
  }
  
  message("Fichiers à concaténer: ", length(files))
  
  # ----------------------------------------------------------
  # 2. EXTRACTION DES PÉRIODES
  # ----------------------------------------------------------
  periods <- map_dfr(files, function(f) {
    period <- extract_period_from_filename(f)
    if (period$valid) {
      tibble(mois = period$mois, annee = period$annee, file = f)
    } else {
      NULL
    }
  })
  
  if (nrow(periods) == 0) {
    stop("Impossible d'extraire les périodes des fichiers")
  }
  
  # Période couverte
  min_period <- periods %>% arrange(annee, mois) %>% slice(1)
  max_period <- periods %>% arrange(desc(annee), desc(mois)) %>% slice(1)
  
  period_str <- sprintf("%02d_%04d-%02d_%04d",
                        min_period$mois, min_period$annee,
                        max_period$mois, max_period$annee)
  
  message("Période couverte: ", period_str)
  
  # ----------------------------------------------------------
  # 3. CHARGEMENT ET CONCATÉNATION
  # ----------------------------------------------------------
  message("\nChargement des fichiers...")
  
  data_list <- map(files, function(f) {
    message("  ", basename(f))
    read_dta(f)
  })
  
  # Déduplication par fichier si configuré
  if (isTRUE(PROCESSING$remove_duplicates) && 
      PROCESSING$duplicate_scope == "file") {
    data_list <- map(data_list, distinct)
  }
  
  # Concaténer
  all_data <- bind_rows(data_list)
  
  message("\nObservations totales: ", format(nrow(all_data), big.mark = " "))
  
  # ----------------------------------------------------------
  # 4. DÉDUPLICATION GLOBALE
  # ----------------------------------------------------------
  if (isTRUE(PROCESSING$remove_duplicates) && 
      PROCESSING$duplicate_scope == "global") {
    n_before <- nrow(all_data)
    all_data <- distinct(all_data)
    n_removed <- n_before - nrow(all_data)
    if (n_removed > 0) {
      message("Doublons supprimés (global): ", format(n_removed, big.mark = " "))
    }
  }
  
  # ----------------------------------------------------------
  # 5. SAUVEGARDE
  # ----------------------------------------------------------
  output_file <- file.path(output_folder, paste0("data_cnps_", period_str, ".dta"))
  
  ensure_dir(output_folder)
  write_dta(all_data, output_file, version = STATA_VERSION)
  
  message("\n", strrep("-", 40))
  message("Fichier concaténé: ", output_file)
  message("Observations: ", format(nrow(all_data), big.mark = " "))
  message("Variables: ", ncol(all_data))
  
  return(list(
    output_file = output_file,
    period = period_str,
    n_rows = nrow(all_data),
    n_cols = ncol(all_data),
    n_files = length(files)
  ))
}

message("Module de concaténation chargé")