# ============================================================
# 02_COLUMN_TYPES_MATCHING.R
# Harmonisation des types de colonnes dans les fichiers .dta
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
library(dplyr)
library(stringr)
library(lubridate)
library(readr)
library(tools)

# ------------------------------------------------------------
# FONCTION DE CONVERSION DATETIME
# ------------------------------------------------------------
convert_to_datetime <- function(x) {
  
  # Si déjà POSIXct, retourner tel quel
  if (inherits(x, "POSIXct")) {
    return(x)
  }
  
  # Si déjà Date, convertir en POSIXct
  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = "UTC"))
  }
  
  max_excel_date <- as.Date("2100-12-31")
  y <- rep(as.POSIXct(NA, tz = "UTC"), length(x))
  
  # Convert factor -> character
  if (is.factor(x)) x <- as.character(x)
  
  # Si numérique, traiter comme date Excel
  if (is.numeric(x)) {
    valid_idx <- which(!is.na(x) & x > 0 & x < 100000)
    if (length(valid_idx) > 0) {
      candidate <- as.Date(as.integer(x[valid_idx]), origin = "1899-12-30")
      valid <- candidate <= max_excel_date
      y[valid_idx[valid]] <- as.POSIXct(candidate[valid], tz = "UTC")
    }
    return(y)
  }
  
  # Traitement des chaînes
  x <- str_trim(as.character(x))
  x[x == ""] <- NA
  
  # Essayer de convertir les valeurs numériques (dates Excel)
  suppressWarnings({
    nums <- as.numeric(x)
  })
  
  idx_num <- which(!is.na(nums))
  if (length(idx_num) > 0) {
    vals <- as.integer(nums[idx_num])
    valid_vals <- vals > 0 & vals < 100000
    candidate <- as.Date(vals[valid_vals], origin = "1899-12-30")
    valid <- candidate <= max_excel_date
    y[idx_num[valid_vals][valid]] <- as.POSIXct(candidate[valid], tz = "UTC")
  }
  
  # Traiter les chaînes de date
  idx_str <- which(is.na(nums) & !is.na(x))
  if (length(idx_str) > 0) {
    vals <- x[idx_str]
    
    # DD/MM/YYYY
    idx <- which(str_detect(vals, "^\\d{2}/\\d{2}/\\d{4}$"))
    if (length(idx) > 0) y[idx_str[idx]] <- dmy(vals[idx], tz = "UTC")
    
    # DD/MM/YYYY HH:MM:SS
    idx <- which(str_detect(vals, "^\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}$"))
    if (length(idx) > 0) y[idx_str[idx]] <- dmy_hms(vals[idx], tz = "UTC")
    
    # YYYY-MM-DD
    idx <- which(str_detect(vals, "^\\d{4}-\\d{2}-\\d{2}$"))
    if (length(idx) > 0) y[idx_str[idx]] <- ymd(vals[idx], tz = "UTC")
    
    # YYYY-MM-DD HH:MM:SS
    idx <- which(str_detect(vals, "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$"))
    if (length(idx) > 0) y[idx_str[idx]] <- ymd_hms(vals[idx], tz = "UTC")
    
    # YYYYMMDD
    idx <- which(str_detect(vals, "^\\d{8}$"))
    if (length(idx) > 0) y[idx_str[idx]] <- ymd(vals[idx], tz = "UTC")
  }
  
  return(y)
}

# ------------------------------------------------------------
# FONCTION PRINCIPALE
# ------------------------------------------------------------
match_column_types <- function(input_folder = NULL, replace_original = TRUE) {
  
  message("\n", strrep("=", 60))
  message("ÉTAPE 2: HARMONISATION DES TYPES DE COLONNES")
  message(strrep("=", 60))
  
  # Paramètres par défaut - UTILISER processed_dta, PAS registry
  if (is.null(input_folder)) {
    input_folder <- PATHS$processed_dta
  }
  
  # Lister les fichiers .dta dans le dossier processed_dta
  files_to_process <- list.files(
    input_folder, 
    pattern = "\\.dta$", 
    full.names = TRUE
  )
  
  if (length(files_to_process) == 0) {
    message("Aucun fichier .dta trouvé dans: ", input_folder)
    return(invisible(NULL))
  }
  
  message("Fichiers à traiter: ", length(files_to_process))
  
  # Log des erreurs
  error_log <- tibble(fichier = character(), erreur = character())
  results <- list(success = character(), errors = character())
  
  # Utiliser le dictionnaire de la config
  dict <- COLUMN_TYPES
  
  # Traitement de chaque fichier
  for (filepath in files_to_process) {
    
    fn <- tools::file_path_sans_ext(basename(filepath))
    message("\n→ Traitement: ", fn)
    
    tryCatch({
      # Lire le fichier .dta
      df <- read_dta(filepath)
      
      message("  Lignes: ", format(nrow(df), big.mark = " "), 
              " | Colonnes: ", ncol(df))
      
      # Convertir les colonnes selon le dictionnaire
      cols_converted <- 0
      
      for (v in intersect(names(dict), names(df))) {
        target <- dict[[v]]
        
        tryCatch({
          if (target == "character") {
            df[[v]] <- as.character(df[[v]])
            # Nettoyer les espaces et les chaînes vides
            df[[v]] <- str_trim(df[[v]])
            df[[v]][df[[v]] == ""] <- NA
          }
          
          if (target == "double") {
            df[[v]] <- suppressWarnings(as.numeric(as.character(df[[v]])))
          }
          
          if (target == "datetime") {
            df[[v]] <- convert_to_datetime(df[[v]])
          }
          
          cols_converted <- cols_converted + 1
          
        }, error = function(e) {
          message("    Warning: Conversion échouée pour ", v, ": ", e$message)
        })
      }
      
      message("  Colonnes converties: ", cols_converted)
      
      # Sauvegarder
      if (replace_original) {
        save_path <- filepath
      } else {
        output_dir <- file.path(dirname(filepath), "typed")
        dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
        save_path <- file.path(output_dir, paste0(fn, ".dta"))
      }
      
      write_dta(df, save_path, version = STATA_VERSION)
      
      # Mettre à jour le registry
      source_file <- paste0(fn, ".xlsx")
      update_registry(
        filename       = basename(save_path),
        source_file    = source_file,
        step_completed = "02_types_matching",
        status         = "in_progress",
        n_rows         = nrow(df),
        n_cols         = ncol(df)
      )
      
      message("  ✓ Types harmonisés")
      results$success <- c(results$success, fn)
      
    }, error = function(e) {
      message("  ✖ ERREUR: ", e$message)
      error_log <<- bind_rows(error_log, tibble(fichier = fn, erreur = e$message))
      results$errors <- c(results$errors, fn)
    })
  }
  
  # Export log erreurs si nécessaire
  if (nrow(error_log) > 0) {
    log_file <- file.path(
      PATHS$inconsistencies, 
      paste0("error_log_types_", get_timestamp(), ".csv")
    )
    write_csv(error_log, log_file)
    message("\nLog des erreurs: ", log_file)
  }
  
  # Résumé
  message("\n", strrep("-", 40))
  message("Résumé: ", length(results$success), " succès, ", 
          length(results$errors), " erreurs")
  
  return(results)
}

# ------------------------------------------------------------
# EXÉCUTION SI APPELÉ DIRECTEMENT
# ------------------------------------------------------------
if (sys.nframe() == 0) {
  match_column_types()
}