# ============================================================
# 01_CREATE_INDIVIDUAL_BASE.R
# Création de la base individuelle nettoyée
# Utilise les variables créées dans 03_data_cleaning.R
# ============================================================

library(haven)
library(dplyr)

# Fonction principale
create_individual_base <- function(input_file = NULL, output_file = NULL) {
  
  message("\n", strrep("=", 60))
  message("CRÉATION DE LA BASE INDIVIDUELLE")
  message(strrep("=", 60))
  
  # ----------------------------------------------------------
  # 1. DÉTERMINATION DU FICHIER D'ENTRÉE
  # ----------------------------------------------------------
  if (is.null(input_file)) {
    # Chercher le fichier nettoyé le plus récent
    cleaned_files <- list.files(
      PATHS$cleaned_final, 
      pattern = "_merged\\.dta$",
      full.names = TRUE
    )
    if (length(cleaned_files) == 0) {
      stop("Aucun fichier nettoyé trouvé dans ", PATHS$cleaned_final)
    }
    input_file <- cleaned_files[which.max(file.mtime(cleaned_files))]
  }
  
  message("Fichier source: ", input_file)
  
  # ----------------------------------------------------------
  # 2. CHARGEMENT DES DONNÉES
  # ----------------------------------------------------------
  df <- read_dta(input_file)
  n_initial <- nrow(df)
  
  message("Observations: ", format(n_initial, big.mark = " "))
  message("Variables: ", ncol(df))
  
  # ----------------------------------------------------------
  # 3. VÉRIFICATION DES VARIABLES REQUISES
  # ----------------------------------------------------------
  message("\n  Vérification des variables créées par 03_data_cleaning.R...")
  
  # Variables créées par 03_data_cleaning.R
  expected_vars <- c(
    # Âges et anciennetés
    "AGE_EMPLOYE",
    "ANCIENNETE_ENTREPRISE",
    "ANCIENNETE_IMMAT",
    "AGE_ENTREPRISE_IMMAT",
    # Classes d'âge détaillées
    "CL_AGE_EMPLOYE",
    "CL_ANCIENNETE_ENTREPRISE",
    "CL_ANCIENNETE_IMMAT",
    "CL_AGE_ENTREPRISE_IMMAT",
    # Classes d'âge réduites
    "CL_RED_AGE_EMPLOYE",
    "CL_RED_ANCIENNETE_ENTREPRISE",
    "CL_RED_ANCIENNETE_IMMAT",
    "CL_RED_AGE_ENTREPRISE_IMMAT",
    # Classes d'effectif
    "CLASSE_EFFECTIF",
    "CLASSE_EFFECTIF_REDUITE",
    # Période et salaire
    "PERIOD",
    "TRIMESTRE",
    "SEMESTRE",
    "SALAIRE_BRUT_MENS"
  )
  
  present_vars <- intersect(expected_vars, names(df))
  missing_vars <- setdiff(expected_vars, names(df))
  
  message("    Variables présentes: ", length(present_vars), "/", length(expected_vars))
  
  if (length(missing_vars) > 0) {
    message("    ⚠ Variables manquantes:")
    for (v in missing_vars) {
      message("      - ", v)
    }
  }
  
  # ----------------------------------------------------------
  # 4. CRÉATION DES VARIABLES COMPLÉMENTAIRES
  # ----------------------------------------------------------
  message("\n  Création des variables complémentaires...")
  
 # Identifiant unique d'observation
  if (!"OBS_ID" %in% names(df)) {
    df <- df %>%
      mutate(
        OBS_ID = paste(ID_INDIV, NUMERO_EMPLOYEUR, PERIOD, sep = "_")
      )
    message("    - OBS_ID créé")
  }
  
  # Initialiser les poids à 1 (seront mis à jour par les modèles)
  df <- df %>%
    mutate(
      w_jt = 1,      # Poids IPW entreprise-temps
      w_ijt = 1,     # Poids IPW individuel conditionnel
      w_final = 1    # Poids final = w_jt * w_ijt
    )
  message("    - Poids initialisés à 1")
  
  # ----------------------------------------------------------
  # 5. VÉRIFICATION DE LA COHÉRENCE
  # ----------------------------------------------------------
  message("\n  Vérification de la cohérence des données...")
  
  # Vérifier les identifiants
  n_obs_id_unique <- n_distinct(df$OBS_ID)
  n_doublons_obs <- n_initial - n_obs_id_unique
  
  if (n_doublons_obs > 0) {
    message("    ⚠ ", n_doublons_obs, " doublons sur OBS_ID détectés")
    
    # Supprimer les doublons si configuré
    if (isTRUE(PROCESSING$remove_duplicates)) {
      df <- df %>% distinct(OBS_ID, .keep_all = TRUE)
      message("    - Doublons supprimés, observations restantes: ", nrow(df))
    }
  } else {
    message("    ✓ Pas de doublons sur OBS_ID")
  }
  
  # Vérifier SALAIRE_BRUT_MENS
  if ("SALAIRE_BRUT_MENS" %in% names(df)) {
    n_missing_sal <- sum(is.na(df$SALAIRE_BRUT_MENS))
    n_zero_sal <- sum(df$SALAIRE_BRUT_MENS == 0, na.rm = TRUE)
    n_negative_sal <- sum(df$SALAIRE_BRUT_MENS < 0, na.rm = TRUE)
    
    if (n_missing_sal > 0) {
      message("    ⚠ SALAIRE_BRUT_MENS: ", n_missing_sal, " valeurs manquantes")
    }
    if (n_zero_sal > 0) {
      message("    ⚠ SALAIRE_BRUT_MENS: ", n_zero_sal, " valeurs nulles")
    }
    if (n_negative_sal > 0) {
      message("    ⚠ SALAIRE_BRUT_MENS: ", n_negative_sal, " valeurs négatives")
    }
    if (n_missing_sal == 0 && n_zero_sal == 0 && n_negative_sal == 0) {
      message("    ✓ SALAIRE_BRUT_MENS valide")
    }
  }
  
  # ----------------------------------------------------------
  # 6. RÉSUMÉ DES VARIABLES ANALYTIQUES
  # ----------------------------------------------------------
  message("\n  Résumé des variables analytiques:")
  
  # Taux de remplissage des variables clés
  key_vars <- c(
    "SALAIRE_BRUT_MENS",
    "AGE_EMPLOYE",
    "ANCIENNETE_ENTREPRISE",
    "CL_RED_AGE_EMPLOYE",
    "CL_RED_ANCIENNETE_ENTREPRISE",
    "CLASSE_EFFECTIF_REDUITE",
    "SEXE",
    "SECTEUR_ACTIVITE_COD"
  )
  
  for (v in key_vars) {
    if (v %in% names(df)) {
      n_valid <- sum(!is.na(df[[v]]))
      pct_valid <- round(n_valid / nrow(df) * 100, 1)
      message("    - ", v, ": ", pct_valid, "% rempli")
    }
  }
  
  # ----------------------------------------------------------
  # 7. SAUVEGARDE
  # ----------------------------------------------------------
  if (is.null(output_file)) {
    output_file <- file.path(
      PATHS$cleaned_individual,
      paste0("base_individuelle_", get_timestamp(), ".dta")
    )
  }
  
  ensure_dir(dirname(output_file))
  write_dta(df, output_file, version = STATA_VERSION)
  
  # ----------------------------------------------------------
  # 8. RÉSUMÉ FINAL
  # ----------------------------------------------------------
  message("\n", strrep("-", 40))
  message("BASE INDIVIDUELLE CRÉÉE")
  message(strrep("-", 40))
  message("Fichier: ", output_file)
  message("Observations: ", format(nrow(df), big.mark = " "))
  message("Variables: ", ncol(df))
  
  # Liste des variables de la base
  var_categories <- list(
    identifiants = c("ID_INDIV", "NUMERO_EMPLOYEUR", "OBS_ID"),
    periode = c("MOIS", "ANNEE", "PERIOD", "TRIMESTRE", "SEMESTRE"),
    salaire = c("SALAIRE_BRUT", "SALAIRE_BRUT_MENS", "DUREE_TRAVAILLEE"),
    age_anciennete = c("AGE_EMPLOYE", "ANCIENNETE_ENTREPRISE", 
                       "ANCIENNETE_IMMAT", "AGE_ENTREPRISE_IMMAT"),
    classes_age = c("CL_AGE_EMPLOYE", "CL_ANCIENNETE_ENTREPRISE",
                    "CL_RED_AGE_EMPLOYE", "CL_RED_ANCIENNETE_ENTREPRISE"),
    classes_effectif = c("CLASSE_EFFECTIF", "CLASSE_EFFECTIF_REDUITE"),
    poids = c("w_jt", "w_ijt", "w_final")
  )
  
  message("\nCatégories de variables:")
  for (cat_name in names(var_categories)) {
    vars_present <- intersect(var_categories[[cat_name]], names(df))
    message("  ", cat_name, ": ", length(vars_present), " variables")
  }
  
  return(list(
    output_file = output_file,
    data = df,
    n_obs = nrow(df),
    n_vars = ncol(df),
    variables_present = present_vars,
    variables_missing = missing_vars
  ))
}

# ============================================================
# FONCTION: AFFICHER LE RÉSUMÉ STATISTIQUE DE LA BASE
# ============================================================

summarize_individual_base <- function(df) {
  
  message("\n=== RÉSUMÉ DE LA BASE INDIVIDUELLE ===\n")
  
  # Dimensions
  message("DIMENSIONS:")
  message("  Observations: ", format(nrow(df), big.mark = " "))
  message("  Variables: ", ncol(df))
  message("  Individus uniques: ", n_distinct(df$ID_INDIV))
  message("  Employeurs uniques: ", n_distinct(df$NUMERO_EMPLOYEUR))
  message("  Périodes: ", n_distinct(df$PERIOD))
  
  # Salaires
  if ("SALAIRE_BRUT_MENS" %in% names(df)) {
    message("\nSALAIRE MENSUEL BRUT:")
    sal_stats <- summary(df$SALAIRE_BRUT_MENS)
    message("  Min:     ", format(sal_stats["Min."], big.mark = " "))
    message("  Q1:      ", format(sal_stats["1st Qu."], big.mark = " "))
    message("  Médiane: ", format(sal_stats["Median"], big.mark = " "))
    message("  Moyenne: ", format(round(sal_stats["Mean"]), big.mark = " "))
    message("  Q3:      ", format(sal_stats["3rd Qu."], big.mark = " "))
    message("  Max:     ", format(sal_stats["Max."], big.mark = " "))
  }
  
  # Distribution par sexe
  if ("SEXE" %in% names(df)) {
    message("\nDISTRIBUTION PAR SEXE:")
    tab_sexe <- df %>%
      group_by(SEXE) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(pct = round(n / sum(n) * 100, 1))
    for (i in 1:nrow(tab_sexe)) {
      message("  ", tab_sexe$SEXE[i], ": ", 
              format(tab_sexe$n[i], big.mark = " "), 
              " (", tab_sexe$pct[i], "%)")
    }
  }
  
  # Distribution par classe d'âge réduite
  if ("CL_RED_AGE_EMPLOYE" %in% names(df)) {
    message("\nDISTRIBUTION PAR TRANCHE D'ÂGE:")
    tab_age <- df %>%
      filter(!is.na(CL_RED_AGE_EMPLOYE)) %>%
      group_by(CL_RED_AGE_EMPLOYE) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(pct = round(n / sum(n) * 100, 1))
    for (i in 1:nrow(tab_age)) {
      message("  ", tab_age$CL_RED_AGE_EMPLOYE[i], ": ", 
              format(tab_age$n[i], big.mark = " "), 
              " (", tab_age$pct[i], "%)")
    }
  }
  
  # Distribution par taille d'entreprise
  if ("CLASSE_EFFECTIF_REDUITE" %in% names(df)) {
    message("\nDISTRIBUTION PAR TAILLE D'ENTREPRISE:")
    tab_eff <- df %>%
      filter(!is.na(CLASSE_EFFECTIF_REDUITE)) %>%
      group_by(CLASSE_EFFECTIF_REDUITE) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(pct = round(n / sum(n) * 100, 1))
    for (i in 1:nrow(tab_eff)) {
      message("  ", tab_eff$CLASSE_EFFECTIF_REDUITE[i], ": ", 
              format(tab_eff$n[i], big.mark = " "), 
              " (", tab_eff$pct[i], "%)")
    }
  }
  
  # Distribution par période
  message("\nDISTRIBUTION PAR PÉRIODE:")
  tab_period <- df %>%
    group_by(ANNEE, MOIS) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(ANNEE, MOIS)
  
  message("  Première période: ", min(tab_period$ANNEE), "-", 
          sprintf("%02d", min(tab_period$MOIS[tab_period$ANNEE == min(tab_period$ANNEE)])))
  message("  Dernière période: ", max(tab_period$ANNEE), "-",
          sprintf("%02d", max(tab_period$MOIS[tab_period$ANNEE == max(tab_period$ANNEE)])))
  message("  Nombre de périodes: ", nrow(tab_period))
  
  return(invisible(NULL))
}

message("Module de création base individuelle chargé")