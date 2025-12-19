# ============================================================
# DATA_CLEANING.R
# Nettoyage des données SALAIRE_BRUT & DUREE_TRAVAILLEE
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
library(haven)
library(dplyr)

# ------------------------------------------------------------
# FONCTION PRINCIPALE
# ------------------------------------------------------------
clean_data <- function(input_file = NULL, output_file = NULL) {
  
  message("\n", strrep("=", 60))
  message("NETTOYAGE DES DONNÉES")
  message(strrep("=", 60))
  
  # Paramètres par défaut
  if (is.null(input_file)) {
    # Trouver le fichier concaténé le plus récent
    concat_files <- list.files(PATHS$cleaned_monthly, 
                                pattern = "^data_cnps_.*\\.dta$",
                                full.names = TRUE)
    if (length(concat_files) == 0) {
      stop("Aucun fichier concaténé trouvé dans ", PATHS$cleaned_monthly)
    }
    input_file <- concat_files[which.max(file.mtime(concat_files))]
  }
  
  if (is.null(output_file)) {
    # Générer le nom de sortie
    output_file <- file.path(
      PATHS$cleaned_final,
      gsub("\\.dta$", "_cleaned.dta", basename(input_file))
    )
  }
  
  message("Fichier source: ", input_file)
  
  # ------------------------------------------------------------
  # CHARGEMENT
  # ------------------------------------------------------------
  df <- read_dta(input_file)
  n_initial <- nrow(df)
  message("Observations initiales: ", format(n_initial, big.mark = " "))
  
  # ------------------------------------------------------------
  # VÉRIFICATIONS
  # ------------------------------------------------------------
  required_numeric   <- REQUIRED_VARS$numeric
  required_character <- REQUIRED_VARS$character
  
  missing_num <- setdiff(required_numeric, names(df))
  missing_chr <- setdiff(required_character, names(df))
  
  if (length(missing_num) > 0) {
    stop("Variables numériques manquantes: ", paste(missing_num, collapse = ", "))
  }
  if (length(missing_chr) > 0) {
    stop("Variables caractères manquantes: ", paste(missing_chr, collapse = ", "))
  }
  
  # ------------------------------------------------------------
  # NETTOYAGE
  # ------------------------------------------------------------
  
  # 1. Supprimer SALAIRE_BRUT manquant
  df <- df %>% filter(!is.na(SALAIRE_BRUT))
  message("  Après suppression NA SALAIRE_BRUT: ", format(nrow(df), big.mark = " "))
  
  # 2. Supprimer SALAIRE_BRUT == 0
  df <- df %>% filter(SALAIRE_BRUT != 0)
  message("  Après suppression SALAIRE_BRUT == 0: ", format(nrow(df), big.mark = " "))
  
  # 3. Exclure les types de salariés configurés (H, J)
  df <- df %>% filter(!(TYPE_SALARIE %in% PROCESSING$exclude_type_salarie))
  message("  Après exclusion types ", paste(PROCESSING$exclude_type_salarie, collapse = "/"), 
          ": ", format(nrow(df), big.mark = " "))
  
  # 4. Traitement spécifique TYPE_SALARIE == "M"
  min_salaire <- PROCESSING$min_salaire_mensuel
  
  # 4.1 DUREE_TRAVAILLEE == 0 & TYPE_SALARIE == "M" → 1
  df <- df %>%
    mutate(
      DUREE_TRAVAILLEE = if_else(
        DUREE_TRAVAILLEE == 0 & TYPE_SALARIE == "M",
        1,
        DUREE_TRAVAILLEE
      )
    )
  
  # 4.2 DUREE_TRAVAILLEE > 1 & TYPE_SALARIE == "M" → 1
  df <- df %>%
    mutate(
      DUREE_TRAVAILLEE = if_else(
        DUREE_TRAVAILLEE > 1 & TYPE_SALARIE == "M",
        1,
        DUREE_TRAVAILLEE
      )
    )
  
  # 4.3 SALAIRE_BRUT < min_salaire & TYPE_SALARIE == "M" → suppression
  df <- df %>%
    filter(!(SALAIRE_BRUT < min_salaire & TYPE_SALARIE == "M"))
  message("  Après seuil minimum salaire (", format(min_salaire, big.mark = " "), 
          "): ", format(nrow(df), big.mark = " "))
  
  # ------------------------------------------------------------
  # CALCUL SALAIRE MENSUEL BRUT
  # ------------------------------------------------------------
  df <- df %>%
    mutate(
      SALAIRE_BRUT_MENS = case_when(
        DUREE_TRAVAILLEE > 0 ~ SALAIRE_BRUT / DUREE_TRAVAILLEE,
        TRUE ~ SALAIRE_BRUT
      )
    )
  
  # ------------------------------------------------------------
  # SAUVEGARDE
  # ------------------------------------------------------------
  write_dta(df, output_file, version = STATA_VERSION)
  
  # Résumé
  n_final <- nrow(df)
  n_supprime <- n_initial - n_final
  pct_supprime <- round(n_supprime / n_initial * 100, 1)
  
  message("\n", strrep("-", 40))
  message("Observations finales: ", format(n_final, big.mark = " "))
  message("Observations supprimées: ", format(n_supprime, big.mark = " "), 
          " (", pct_supprime, "%)")
  message("Fichier: ", output_file)
  
  return(list(
    output_file = output_file,
    n_initial = n_initial,
    n_final = n_final,
    n_supprime = n_supprime
  ))
}

# ------------------------------------------------------------
# EXÉCUTION SI APPELÉ DIRECTEMENT
# ------------------------------------------------------------
if (sys.nframe() == 0) {
  clean_data()
}