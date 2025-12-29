# ============================================================
# 03_CREATE_ANALYTICAL_BASE.R
# Création de la base analytique avec toutes les dimensions
# Utilise les variables créées dans 03_data_cleaning.R
# ============================================================

library(haven)
library(dplyr)

# Fonction principale
create_analytical_base <- function(df_individual = NULL, 
                                   df_firm_time = NULL,
                                   output_file = NULL) {
  
  message("\n", strrep("=", 60))
  message("CRÉATION DE LA BASE ANALYTIQUE")
  message(strrep("=", 60))
  
  # ----------------------------------------------------------
  # 1. CHARGEMENT DES DONNÉES
  # ----------------------------------------------------------
  if (is.null(df_individual)) {
    ind_files <- list.files(
      PATHS$cleaned_individual, 
      pattern = "^base_individuelle_.*\\.dta$",
      full.names = TRUE
    )
    if (length(ind_files) > 0) {
      df_individual <- read_dta(ind_files[which.max(file.mtime(ind_files))])
      message("Base individuelle: ", format(nrow(df_individual), big.mark = " "), " obs")
    } else {
      stop("Base individuelle non trouvée")
    }
  }
  
  if (is.null(df_firm_time)) {
    firm_files <- list.files(
      PATHS$cleaned_firm_time, 
      pattern = "^base_entreprise_temps_.*\\.dta$",
      full.names = TRUE
    )
    if (length(firm_files) > 0) {
      df_firm_time <- read_dta(firm_files[which.max(file.mtime(firm_files))])
      message("Base entreprise-temps: ", format(nrow(df_firm_time), big.mark = " "), " obs")
    }
  }
  
  # ----------------------------------------------------------
  # 2. JOINTURE DES POIDS ENTREPRISE-TEMPS
  # ----------------------------------------------------------
  message("\n  Jointure avec la base entreprise-temps...")
  
  if (!is.null(df_firm_time)) {
    # Sélectionner les colonnes à joindre
    cols_to_join <- c("NUMERO_EMPLOYEUR", "ANNEE", "MOIS")
    
    # Ajouter les poids si présents
    if ("w_jt" %in% names(df_firm_time)) {
      cols_to_join <- c(cols_to_join, "w_jt")
    }
    if ("pi_jt" %in% names(df_firm_time)) {
      cols_to_join <- c(cols_to_join, "pi_jt")
    }
    if ("D_jt" %in% names(df_firm_time)) {
      cols_to_join <- c(cols_to_join, "D_jt")
    }
    
    # Joindre
    df_analytical <- df_individual %>%
      left_join(
        df_firm_time %>% select(all_of(cols_to_join)),
        by = c("NUMERO_EMPLOYEUR", "ANNEE", "MOIS"),
        suffix = c("", "_firm")
      )
    
    # Mettre à jour w_jt si présent
    if ("w_jt_firm" %in% names(df_analytical)) {
      df_analytical <- df_analytical %>%
        mutate(w_jt = coalesce(w_jt_firm, w_jt)) %>%
        select(-w_jt_firm)
    }
    
    message("    Jointure effectuée")
  } else {
    df_analytical <- df_individual
    message("    Pas de base entreprise-temps, utilisation de la base individuelle seule")
  }
  
  # ----------------------------------------------------------
  # 3. VÉRIFICATION DES DIMENSIONS ANALYTIQUES
  # ----------------------------------------------------------
  message("\n  Vérification des dimensions analytiques:")
  
  dimensions_status <- list()
  
  for (dim_name in names(STAT_DIMENSIONS)) {
    if (dim_name == "global") next
    
    dim_config <- STAT_DIMENSIONS[[dim_name]]
    if (!dim_config$enabled) next
    
    var_name <- dim_config$variable
    
    if (var_name %in% names(df_analytical)) {
      n_values <- n_distinct(df_analytical[[var_name]], na.rm = TRUE)
      n_missing <- sum(is.na(df_analytical[[var_name]]))
      pct_missing <- round(n_missing / nrow(df_analytical) * 100, 1)
      
      dimensions_status[[dim_name]] <- list(
        present = TRUE,
        n_values = n_values,
        pct_missing = pct_missing
      )
      
      status_icon <- ifelse(pct_missing < 10, "✓", "⚠")
      message("    ", status_icon, " ", dim_config$label, " (", var_name, "): ",
              n_values, " valeurs, ", pct_missing, "% manquants")
    } else {
      dimensions_status[[dim_name]] <- list(
        present = FALSE,
        n_values = 0,
        pct_missing = 100
      )
      message("    ✖ ", dim_config$label, " (", var_name, "): ABSENT")
    }
  }
  
  # ----------------------------------------------------------
  # 4. CALCUL DU POIDS FINAL
  # ----------------------------------------------------------
  message("\n  Calcul du poids final...")
  
  df_analytical <- df_analytical %>%
    mutate(
      w_jt = coalesce(w_jt, 1),
      w_ijt = coalesce(w_ijt, 1),
      w_final = w_jt * w_ijt
    )
  
  message("    Poids moyen: ", round(mean(df_analytical$w_final, na.rm = TRUE), 3))
  message("    Poids min: ", round(min(df_analytical$w_final, na.rm = TRUE), 3))
  message("    Poids max: ", round(max(df_analytical$w_final, na.rm = TRUE), 3))
  
  # ----------------------------------------------------------
  # 5. RÉSUMÉ DES VARIABLES DISPONIBLES
  # ----------------------------------------------------------
  message("\n  Variables disponibles pour l'analyse:")
  
  var_groups <- list(
    "Salaire" = c("SALAIRE_BRUT", "SALAIRE_BRUT_MENS"),
    "Âge employé" = c("AGE_EMPLOYE", "CL_AGE_EMPLOYE", "CL_RED_AGE_EMPLOYE"),
    "Ancienneté" = c("ANCIENNETE_ENTREPRISE", "CL_ANCIENNETE_ENTREPRISE", "CL_RED_ANCIENNETE_ENTREPRISE"),
    "Taille entreprise" = c("CLASSE_EFFECTIF", "CLASSE_EFFECTIF_REDUITE"),
    "Secteur" = c("SECTEUR_ACTIVITE_COD"#, "SECTOR_CODE"
    ),
    "Géographie" = c("COMMUNE", "REGION_CODE"),
    "Démographie" = c("SEXE"),
    "Poids" = c("w_jt", "w_ijt", "w_final")
  )
  
  for (group_name in names(var_groups)) {
    vars_present <- intersect(var_groups[[group_name]], names(df_analytical))
    if (length(vars_present) > 0) {
      message("    ", group_name, ": ", paste(vars_present, collapse = ", "))
    }
  }
  
  # ----------------------------------------------------------
  # 6. SAUVEGARDE
  # ----------------------------------------------------------
  if (is.null(output_file)) {
    output_file <- file.path(
      PATHS$cleaned_analytical,
      paste0("base_analytique_", get_timestamp(), ".dta")
    )
  }
  
  ensure_dir(dirname(output_file))
  write_dta(df_analytical, output_file, version = STATA_VERSION)
  
  message("\n", strrep("-", 40))
  message("Base analytique créée: ", output_file)
  message("Observations: ", format(nrow(df_analytical), big.mark = " "))
  message("Variables: ", ncol(df_analytical))
  
  return(list(
    output_file = output_file,
    data = df_analytical,
    n_obs = nrow(df_analytical),
    n_vars = ncol(df_analytical),
    dimensions_status = dimensions_status
  ))
}

message("Module de création base analytique chargé")