# ============================================================
# 05_MERGE_SECTOR_CODES.R
# Fusion avec les codes secteurs
# ============================================================

library(dplyr)
library(haven)
library(readxl)
library(stringr)

# Fonction principale
merge_sector_codes <- function(input_file = NULL, output_file = NULL) {
  
  message("\n", strrep("=", 60))
  message("FUSION AVEC LES CODES SECTEURS")
  message(strrep("=", 60))
  
  if (is.null(input_file)) {
    cleaned_files <- list.files(PATHS$cleaned_final, 
                                pattern = "_cleaned\\.dta$",
                                full.names = TRUE)
    if (length(cleaned_files) == 0) {
      stop("Aucun fichier nettoyé trouvé dans ", PATHS$cleaned_final)
    }
    input_file <- cleaned_files[which.max(file.mtime(cleaned_files))]
  }
  
  message("Fichier master: ", input_file)
  
  # Import fichier externe
  external_config <- EXTERNAL_FILES$sector_codes
  
  if (!file.exists(external_config$path)) {
    stop("Fichier externe non trouvé: ", external_config$path)
  }
  
  message("Fichier externe: ", external_config$path)
  
  df_excel <- read_excel(external_config$path, sheet = external_config$sheet)
  
  if (!is.null(external_config$drop_cols)) {
    df_excel <- df_excel %>%
      select(-any_of(external_config$drop_cols))
  }
  
  merge_key <- external_config$key
  
  if (merge_key %in% names(df_excel)) {
    df_excel <- df_excel %>%
      mutate(!!merge_key := str_trim(as.character(.data[[merge_key]])))
  }
  
  df_excel <- df_excel %>%
    distinct(!!sym(merge_key), .keep_all = TRUE)
  
  message("  Observations externes: ", nrow(df_excel))
  
  # Chargement fichier master
  df_master <- read_dta(input_file)
  
  if (merge_key %in% names(df_master)) {
    df_master <- df_master %>%
      mutate(!!merge_key := str_trim(as.character(.data[[merge_key]])))
  }
  
  message("  Observations master: ", format(nrow(df_master), big.mark = " "))
  
  # Fusion
  df_merged <- df_master %>%
    left_join(df_excel, by = merge_key, suffix = c("", "_ext"))
  
  # Diagnostics
  n_matched <- sum(!is.na(df_merged$SECTEUR_ACTIVITE_COD))
  n_unmatched <- sum(is.na(df_merged$SECTEUR_ACTIVITE_COD))
  
  message("\n  Résultats de la fusion:")
  message("    Matched:     ", format(n_matched, big.mark = " "))
  message("    Non matched: ", format(n_unmatched, big.mark = " "))
  
  # Créer SECTOR_CODE si absent
  if (!"SECTOR_CODE" %in% names(df_merged) && "SECTEUR_ACTIVITE_COD" %in% names(df_merged)) {
    df_merged <- df_merged %>%
      mutate(SECTOR_CODE = SECTEUR_ACTIVITE_COD)
  }
  
  # Sauvegarde
  if (is.null(output_file)) {
    period_match <- regmatches(basename(input_file), 
                               regexpr("\\d{2}_\\d{4}-\\d{2}_\\d{4}", basename(input_file)))
    
    if (length(period_match) == 0) period_match <- "unknown"
    
    output_file <- file.path(
      PATHS$cleaned_final,
      paste0("data_cnps_final_", period_match, "_", "_merged",".dta")
    )
  }
  
  write_dta(df_merged, output_file, version = STATA_VERSION)
  
  message("\n", strrep("-", 40))
  message("Fichier final: ", output_file)
  
  return(list(
    output_file = output_file,
    n_matched = n_matched,
    n_unmatched = n_unmatched
  ))
}

message("Module de fusion codes secteurs chargé")