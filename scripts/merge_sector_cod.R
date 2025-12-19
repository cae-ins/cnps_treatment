# ============================================================
# MERGE_SECTOR_COD.R
# Fusion avec les codes secteurs
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

# ------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------
library(dplyr)
library(haven)
library(readxl)
library(stringr)

# ------------------------------------------------------------
# FONCTION PRINCIPALE
# ------------------------------------------------------------
merge_sector_codes <- function(input_file = NULL, output_file = NULL) {
  
  message("\n", strrep("=", 60))
  message("FUSION AVEC LES CODES SECTEURS")
  message(strrep("=", 60))
  
  # Paramètres par défaut
  if (is.null(input_file)) {
    # Trouver le fichier nettoyé le plus récent
    cleaned_files <- list.files(PATHS$cleaned_final, 
                                 pattern = "_cleaned\\.dta$",
                                 full.names = TRUE)
    if (length(cleaned_files) == 0) {
      stop("Aucun fichier nettoyé trouvé dans ", PATHS$cleaned_final)
    }
    input_file <- cleaned_files[which.max(file.mtime(cleaned_files))]
  }
  
  message("Fichier master: ", input_file)
  
  # ------------------------------------------------------------
  # IMPORT FICHIER EXTERNE (CODES SECTEURS)
  # ------------------------------------------------------------
  external_config <- EXTERNAL_FILES$sector_codes
  
  if (!file.exists(external_config$path)) {
    stop("Fichier externe non trouvé: ", external_config$path)
  }
  
  message("Fichier externe: ", external_config$path)
  
  df_excel <- read_excel(
    external_config$path, 
    sheet = external_config$sheet
  )
  
  # Supprimer les colonnes non désirées
  if (!is.null(external_config$drop_cols)) {
    df_excel <- df_excel %>%
      select(-any_of(external_config$drop_cols))
  }
  
  # Nettoyer la clé de fusion
  merge_key <- external_config$key
  
  if (merge_key %in% names(df_excel)) {
    df_excel <- df_excel %>%
      mutate(!!merge_key := str_trim(as.character(.data[[merge_key]])))
  }
  
  # Assurer l'unicité
  df_excel <- df_excel %>%
    distinct(!!sym(merge_key), .keep_all = TRUE)
  
  message("  Observations externes: ", nrow(df_excel))
  
  # ------------------------------------------------------------
  # CHARGEMENT FICHIER MASTER
  # ------------------------------------------------------------
  df_master <- read_dta(input_file)
  
  # Nettoyer la clé de fusion
  if (merge_key %in% names(df_master)) {
    df_master <- df_master %>%
      mutate(!!merge_key := str_trim(as.character(.data[[merge_key]])))
  }
  
  message("  Observations master: ", format(nrow(df_master), big.mark = " "))
  
  # ------------------------------------------------------------
  # FUSION
  # ------------------------------------------------------------
  df_merged <- df_master %>%
    left_join(df_excel, by = merge_key, suffix = c("", "_ext"))
  
  # Diagnostics
  df_merged <- df_merged %>%
    mutate(
      merge_status = case_when(
        !is.na(.data[[merge_key]]) & !is.na(SECTEUR_ACTIVITE_COD) ~ "matched",
        !is.na(.data[[merge_key]]) & is.na(SECTEUR_ACTIVITE_COD)  ~ "master_only",
        TRUE ~ "other"
      )
    )
  
  merge_stats <- table(df_merged$merge_status)
  
  message("\n  Résultats de la fusion:")
  message("    Matched:     ", format(merge_stats["matched"], big.mark = " "))
  message("    Master only: ", format(merge_stats["master_only"], big.mark = " "))
  
  # ------------------------------------------------------------
  # SAUVEGARDE
  # ------------------------------------------------------------
  if (is.null(output_file)) {
    # Extraire la période du nom du fichier
    period_match <- regmatches(basename(input_file), 
                                regexpr("\\d{2}_\\d{4}-\\d{2}_\\d{4}", basename(input_file)))
    
    if (length(period_match) == 0) period_match <- "unknown"
    
    timestamp <- get_timestamp("%d_%m_%Y")
    
    output_file <- file.path(
      PATHS$cleaned_final,
      paste0("data_cnps_final_", period_match, "_", timestamp, ".dta")
    )
  }
  
  # Supprimer la colonne de diagnostic avant sauvegarde
  df_merged <- df_merged %>% select(-merge_status)
  
  write_dta(df_merged, output_file, version = STATA_VERSION)
  
  message("\n", strrep("-", 40))
  message("Fichier final: ", output_file)
  
  return(list(
    output_file = output_file,
    n_matched = merge_stats["matched"],
    n_master_only = merge_stats["master_only"]
  ))
}

# ------------------------------------------------------------
# EXÉCUTION SI APPELÉ DIRECTEMENT
# ------------------------------------------------------------
if (sys.nframe() == 0) {
  merge_sector_codes()
}