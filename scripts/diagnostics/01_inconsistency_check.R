# ============================================================
# 01_INCONSISTENCY_CHECK.R
# Audit complet des fichiers CNPS
# ============================================================

library(haven)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(openxlsx)

# Fonction principale
run_inconsistency_check <- function(input_folder = NULL, 
                                    output_folder = NULL,
                                    suffix = NULL) {
  
  message("\n", strrep("=", 60))
  message("CONTRÔLE D'INCOHÉRENCES")
  message(strrep("=", 60))
  
  if (is.null(input_folder)) input_folder <- PATHS$processed_dta
  if (is.null(output_folder)) output_folder <- PATHS$inconsistencies
  if (is.null(suffix)) suffix <- get_timestamp()
  
  output_file <- file.path(output_folder, paste0("audit_fichiers_cnps_", suffix, ".xlsx"))
  
  files <- list.files(input_folder, pattern = "\\.dta$", full.names = TRUE)
  
  if (length(files) == 0) {
    message("Aucun fichier .dta trouvé dans: ", input_folder)
    return(invisible(NULL))
  }
  
  message("Fichiers à auditer: ", length(files))
  file_names <- basename(files)
  
  # Extraction mois/année
  file_info <- tibble(
    fichier = file_names,
    MOIS = as.integer(str_sub(file_names, 1, 2)),
    ANNEE = as.integer(str_sub(file_names, 4, 7))
  ) %>% arrange(ANNEE, MOIS)
  
  files <- file.path(input_folder, file_info$fichier)
  
  # Import des données
  message("\nChargement des données...")
  data_raw <- map(files, read_dta)
  names(data_raw) <- file_info$fichier
  
  # 1. Check doublons lignes
  message("Vérification des doublons...")
  doublons_lignes <- map2_dfr(data_raw, file_info$fichier, function(df, f) {
    n_tot <- nrow(df)
    n_dup <- sum(duplicated(df))
    tibble(
      fichier = f,
      ANNEE = file_info$ANNEE[file_info$fichier == f],
      MOIS = file_info$MOIS[file_info$fichier == f],
      total_obs = n_tot,
      nb_lignes_dupliquees = n_dup,
      pct_lignes_dupliquees = round(n_dup / n_tot * 100, 2)
    )
  }) %>% arrange(ANNEE, MOIS)
  
  # Suppression doublons pour analyses
  data_clean <- map(data_raw, ~ .x[!duplicated(.x), ])
  
  # 2. Contrôle des colonnes
  message("Vérification des colonnes...")
  cols_list <- map(data_clean, names)
  ref_cols <- cols_list[[1]]
  
  controle_colonnes <- map_dfr(file_info$fichier, function(f) {
    tibble(
      fichier = f,
      ANNEE = file_info$ANNEE[file_info$fichier == f],
      MOIS = file_info$MOIS[file_info$fichier == f],
      colonnes_manquantes = paste(setdiff(ref_cols, cols_list[[f]]), collapse = ", "),
      colonnes_en_plus = paste(setdiff(cols_list[[f]], ref_cols), collapse = ", ")
    )
  }) %>% arrange(ANNEE, MOIS)
  
  # 3. Contrôle des types
  message("Vérification des types...")
  types_list <- map(data_clean, ~ sapply(.x, typeof))
  ref_types <- types_list[[1]]
  
  controle_types <- map_dfr(file_info$fichier, function(f) {
    vars <- intersect(names(types_list[[f]]), names(ref_types))
    diff <- vars[types_list[[f]][vars] != ref_types[vars]]
    
    if (length(diff) == 0) {
      return(tibble(
        fichier = f,
        ANNEE = file_info$ANNEE[file_info$fichier == f],
        MOIS = file_info$MOIS[file_info$fichier == f],
        variable = NA_character_,
        type_fichier = NA_character_,
        type_reference = NA_character_
      ))
    }
    
    tibble(
      fichier = f,
      ANNEE = file_info$ANNEE[file_info$fichier == f],
      MOIS = file_info$MOIS[file_info$fichier == f],
      variable = diff,
      type_fichier = types_list[[f]][diff],
      type_reference = ref_types[diff]
    )
  }) %>% filter(!is.na(variable)) %>% arrange(ANNEE, MOIS)
  
  # 4. Valeurs manquantes
  message("Vérification des valeurs manquantes...")
  valeurs_manquantes <- map2_dfr(data_clean, file_info$fichier, function(df, f) {
    df %>%
      summarise(across(everything(), ~ sum(is.na(.)))) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "nb_na") %>%
      mutate(
        fichier = f,
        ANNEE = file_info$ANNEE[file_info$fichier == f],
        MOIS = file_info$MOIS[file_info$fichier == f],
        total_obs = nrow(df),
        pct_na = round(nb_na / total_obs * 100, 2)
      )
  }) %>% arrange(ANNEE, MOIS)
  
  # 5. Outliers IQR - SALAIRE_BRUT
  message("Détection des outliers...")
  outliers <- map2_dfr(data_clean, file_info$fichier, function(df, f) {
    if (!"SALAIRE_BRUT" %in% names(df)) return(NULL)
    
    x <- df$SALAIRE_BRUT
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQRv <- Q3 - Q1
    
    tibble(
      fichier = f,
      ANNEE = file_info$ANNEE[file_info$fichier == f],
      MOIS = file_info$MOIS[file_info$fichier == f],
      variable = "SALAIRE_BRUT",
      nb_outliers = sum(x < Q1 - 1.5*IQRv | x > Q3 + 1.5*IQRv, na.rm = TRUE),
      pct_outliers = round(nb_outliers / length(x) * 100, 2)
    )
  }) %>% arrange(ANNEE, MOIS)
  
  # 6. Unicité ID_INDIV
  message("Vérification de l'unicité des ID...")
  id_var <- REQUIRED_VARS$id[1]
  
  unicite_id <- map2_dfr(data_clean, file_info$fichier, function(df, f) {
    if (!id_var %in% names(df)) {
      return(tibble(
        fichier = f,
        ANNEE = file_info$ANNEE[file_info$fichier == f],
        MOIS = file_info$MOIS[file_info$fichier == f],
        erreur = paste(id_var, "absent")
      ))
    }
    
    tibble(
      fichier = f,
      ANNEE = file_info$ANNEE[file_info$fichier == f],
      MOIS = file_info$MOIS[file_info$fichier == f],
      total_obs = nrow(df),
      nb_unique = n_distinct(df[[id_var]], na.rm = TRUE),
      nb_doublons = sum(duplicated(df[[id_var]]))
    )
  }) %>% arrange(ANNEE, MOIS)
  
  # Export Excel
  message("\nExport Excel...")
  ensure_dir(output_folder)
  
  wb <- createWorkbook()
  
  addWorksheet(wb, "Doublons_lignes")
  addWorksheet(wb, "Colonnes")
  addWorksheet(wb, "Types_variables")
  addWorksheet(wb, "Valeurs_manquantes")
  addWorksheet(wb, "Outliers_Salaire")
  addWorksheet(wb, "Unicite_ID")
  
  writeData(wb, "Doublons_lignes", doublons_lignes)
  writeData(wb, "Colonnes", controle_colonnes)
  writeData(wb, "Types_variables", controle_types)
  writeData(wb, "Valeurs_manquantes", valeurs_manquantes)
  writeData(wb, "Outliers_Salaire", outliers)
  writeData(wb, "Unicite_ID", unicite_id)
  
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  message("\n✓ Fichier d'audit: ", output_file)
  
  return(list(
    output_file = output_file,
    doublons = doublons_lignes,
    colonnes = controle_colonnes,
    types = controle_types
  ))
}

message("Module de contrôle d'incohérences chargé")