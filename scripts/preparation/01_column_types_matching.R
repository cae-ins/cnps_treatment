# ============================================================
# 01_COLUMN_TYPES_MATCHING.R
# Harmonisation des types de colonnes dans les fichiers .dta
# ============================================================

library(haven)
library(dplyr)
library(stringr)
library(lubridate)

# Fonction de conversion datetime
convert_to_datetime <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  if (inherits(x, "Date")) return(as.POSIXct(x, tz = "UTC"))
  
  max_excel_date <- as.Date("2100-12-31")
  y <- rep(as.POSIXct(NA, tz = "UTC"), length(x))
  
  if (is.factor(x)) x <- as.character(x)
  
  if (is.numeric(x)) {
    valid_idx <- which(!is.na(x) & x > 0 & x < 100000)
    if (length(valid_idx) > 0) {
      candidate <- as.Date(as.integer(x[valid_idx]), origin = "1899-12-30")
      valid <- candidate <= max_excel_date
      y[valid_idx[valid]] <- as.POSIXct(candidate[valid], tz = "UTC")
    }
    return(y)
  }
  
  x <- str_trim(as.character(x))
  x[x == ""] <- NA
  
  suppressWarnings({ nums <- as.numeric(x) })
  
  idx_num <- which(!is.na(nums))
  if (length(idx_num) > 0) {
    vals <- as.integer(nums[idx_num])
    valid_vals <- vals > 0 & vals < 100000
    candidate <- as.Date(vals[valid_vals], origin = "1899-12-30")
    valid <- candidate <= max_excel_date
    y[idx_num[valid_vals][valid]] <- as.POSIXct(candidate[valid], tz = "UTC")
  }
  
  idx_str <- which(is.na(nums) & !is.na(x))
  if (length(idx_str) > 0) {
    vals <- x[idx_str]
    
    idx <- which(str_detect(vals, "^\\d{2}/\\d{2}/\\d{4}$"))
    if (length(idx) > 0) y[idx_str[idx]] <- dmy(vals[idx], tz = "UTC")
    
    idx <- which(str_detect(vals, "^\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}$"))
    if (length(idx) > 0) y[idx_str[idx]] <- dmy_hms(vals[idx], tz = "UTC")
    
    idx <- which(str_detect(vals, "^\\d{4}-\\d{2}-\\d{2}$"))
    if (length(idx) > 0) y[idx_str[idx]] <- ymd(vals[idx], tz = "UTC")
    
    idx <- which(str_detect(vals, "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$"))
    if (length(idx) > 0) y[idx_str[idx]] <- ymd_hms(vals[idx], tz = "UTC")
    
    idx <- which(str_detect(vals, "^\\d{8}$"))
    if (length(idx) > 0) y[idx_str[idx]] <- ymd(vals[idx], tz = "UTC")
  }
  
  return(y)
}

# Fonction principale
match_column_types <- function(input_folder = NULL, replace_original = TRUE) {
  
  message("\n", strrep("=", 60))
  message("HARMONISATION DES TYPES DE COLONNES")
  message(strrep("=", 60))
  
  if (is.null(input_folder)) {
    input_folder <- PATHS$processed_dta
  }
  
  files_to_process <- list.files(input_folder, pattern = "\\.dta$", full.names = TRUE)
  
  if (length(files_to_process) == 0) {
    message("Aucun fichier .dta trouvé dans: ", input_folder)
    return(invisible(NULL))
  }
  
  message("Fichiers à traiter: ", length(files_to_process))
  
  results <- list(success = character(), errors = character())
  dict <- COLUMN_TYPES
  
  for (filepath in files_to_process) {
    fn <- tools::file_path_sans_ext(basename(filepath))
    message("\n→ Traitement: ", fn)
    
    tryCatch({
      df <- read_dta(filepath)
      message("  Lignes: ", format(nrow(df), big.mark = " "), 
              " | Colonnes: ", ncol(df))
      
      cols_converted <- 0
      
      for (v in intersect(names(dict), names(df))) {
        target <- dict[[v]]
        
        tryCatch({
          if (target == "character") {
            df[[v]] <- as.character(df[[v]])
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
      
      if (replace_original) {
        save_path <- filepath
      } else {
        output_dir <- file.path(dirname(filepath), "typed")
        dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
        save_path <- file.path(output_dir, paste0(fn, ".dta"))
      }
      
      write_dta(df, save_path, version = STATA_VERSION)
      message("  ✓ Types harmonisés")
      results$success <- c(results$success, fn)
      
    }, error = function(e) {
      message("  ✖ ERREUR: ", e$message)
      results$errors <- c(results$errors, fn)
    })
  }
  
  message("\n", strrep("-", 40))
  message("Résumé: ", length(results$success), " succès, ", 
          length(results$errors), " erreurs")
  
  return(results)
}

message("Module de matching des types chargé")