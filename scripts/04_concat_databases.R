# ============================================================
# 04_CONCAT_DATABASES.R
# Concaténation des bases de données avec gestion incrémentale
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
library(tools)
library(purrr)

# ------------------------------------------------------------
# FONCTION PRINCIPALE
# ------------------------------------------------------------
concat_databases <- function(input_folder = NULL,
                              output_folder = NULL,
                              incremental = TRUE) {
  
  message("\n", strrep("=", 60))
  message("ÉTAPE 4: CONCATÉNATION DES BASES DE DONNÉES")
  message(strrep("=", 60))
  
  # Paramètres par défaut
  if (is.null(input_folder))  input_folder  <- PATHS$processed_dta
  if (is.null(output_folder)) output_folder <- PATHS$cleaned_monthly
  
  # Lister les fichiers
  files <- list.files(input_folder, pattern = "\\.dta$", full.names = TRUE)
  
  if (length(files) == 0) {
    message("Aucun fichier .dta trouvé")
    return(invisible(NULL))
  }
  
  message("Fichiers à concaténer: ", length(files))
  
  # Extraire les périodes pour le nom du fichier de sortie
  periods <- map_dfr(files, function(f) {
    period <- extract_period_from_filename(f)
    if (period$valid) {
      tibble(mois = period$mois, annee = period$annee)
    } else {
      NULL
    }
  })
  
  if (nrow(periods) == 0) {
    stop("Impossible d'extraire les périodes des fichiers")
  }
  
  # Déterminer la plage de dates
  min_period <- periods %>% 
    arrange(annee, mois) %>% 
    slice(1)
  max_period <- periods %>% 
    arrange(desc(annee), desc(mois)) %>% 
    slice(1)
  
  period_str <- sprintf("%02d_%04d-%02d_%04d",
                        min_period$mois, min_period$annee,
                        max_period$mois, max_period$annee)
  
  # Nom du fichier de sortie
  output_file <- file.path(output_folder, paste0("data_cnps_", period_str, ".dta"))
  
  # ------------------------------------------------------------
  # MODE INCRÉMENTAL
  # ------------------------------------------------------------
  if (incremental && file.exists(output_file)) {
    message("\nMode incrémental activé")
    
    # Charger la base existante
    existing_data <- read_dta(output_file)
    existing_periods <- existing_data %>%
      distinct(ANNEE, MOIS) %>%
      mutate(key = paste(ANNEE, MOIS, sep = "_"))
    
    # Identifier les nouveaux fichiers
    new_files <- files[map_lgl(files, function(f) {
      period <- extract_period_from_filename(f)
      if (!period$valid) return(FALSE)
      key <- paste(period$annee, period$mois, sep = "_")
      !(key %in% existing_periods$key)
    })]
    
    # Identifier les fichiers modifiés (via registry)
    registry <- read_registry()
    modified_files <- files[map_lgl(files, function(f) {
      fn <- basename(f)
      check <- needs_processing(file.path(PATHS$raw_excel, 
                                          paste0(file_path_sans_ext(fn), ".xlsx")),
                                registry)
      check$needs_processing && check$reason == "modified"
    })]
    
    files_to_add <- unique(c(new_files, modified_files))
    
    if (length(files_to_add) == 0) {
      message("Aucune mise à jour nécessaire")
      return(list(output_file = output_file, mode = "no_update"))
    }
    
    message("  Nouveaux fichiers: ", length(new_files))
    message("  Fichiers modifiés: ", length(modified_files))
    
    # Supprimer les données des périodes modifiées
    if (length(modified_files) > 0) {
      modified_periods <- map_dfr(modified_files, function(f) {
        period <- extract_period_from_filename(f)
        tibble(annee = period$annee, mois = period$mois)
      })
      
      existing_data <- existing_data %>%
        anti_join(modified_periods, by = c("ANNEE" = "annee", "MOIS" = "mois"))
      
      message("  Périodes supprimées pour mise à jour: ", nrow(modified_periods))
    }
    
    # Charger les nouveaux fichiers
    new_data_list <- map(files_to_add, function(f) {
      message("  Chargement: ", basename(f))
      read_dta(f)
    })
    
    # Déduplication si configurée
    if (PROCESSING$remove_duplicates && PROCESSING$duplicate_scope == "file") {
      new_data_list <- map(new_data_list, distinct)
    }
    
    # Combiner
    all_data <- bind_rows(existing_data, bind_rows(new_data_list))
    
    mode <- "incremental"
    
  } else {
    # ------------------------------------------------------------
    # MODE COMPLET
    # ------------------------------------------------------------
    message("\nMode complet")
    
    # Charger tous les fichiers
    data_list <- map(files, function(f) {
      message("  Chargement: ", basename(f))
      read_dta(f)
    })
    
    # Déduplication si configurée
    if (PROCESSING$remove_duplicates) {
      if (PROCESSING$duplicate_scope == "file") {
        data_list <- map(data_list, distinct)
      }
    }
    
    # Concaténer
    all_data <- bind_rows(data_list)
    
    # Déduplication globale si configurée
    if (PROCESSING$remove_duplicates && PROCESSING$duplicate_scope == "global") {
      n_before <- nrow(all_data)
      all_data <- distinct(all_data)
      message("  Doublons supprimés (global): ", n_before - nrow(all_data))
    }
    
    mode <- "full"
  }
  
  # ------------------------------------------------------------
  # SAUVEGARDE
  # ------------------------------------------------------------
  message("\nSauvegarde...")
  write_dta(all_data, output_file, version = STATA_VERSION)
  
  # Résumé
  message("\n", strrep("-", 40))
  message("Mode: ", mode)
  message("Observations: ", format(nrow(all_data), big.mark = " "))
  message("Variables: ", ncol(all_data))
  message("Période: ", period_str)
  message("Fichier: ", output_file)
  
  return(list(
    output_file = output_file,
    mode = mode,
    period = period_str,
    n_rows = nrow(all_data),
    n_cols = ncol(all_data)
  ))
}

# ------------------------------------------------------------
# EXÉCUTION SI APPELÉ DIRECTEMENT
# ------------------------------------------------------------
if (sys.nframe() == 0) {
  concat_databases()
}