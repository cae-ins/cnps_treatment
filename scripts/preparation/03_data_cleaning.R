# ============================================================
# 03_DATA_CLEANING.R
# Nettoyage des données et création des variables dérivées
# ============================================================

library(haven)
library(dplyr)
library(lubridate)

# ============================================================
# FONCTIONS DE CRÉATION DES CLASSES
# ============================================================

#' Créer les classes d'âge détaillées (17 classes)
#' @param x Vecteur numérique (années)
#' @return Facteur avec les classes
create_classe_age <- function(x) {
  
  labels <- c(
    "< 1 an",
    "1 - 4 ans",
    "5 - 9 ans",
    "10 - 14 ans",
    "15 - 19 ans",
    "20 - 24 ans",
    "25 - 29 ans",
    "30 - 34 ans",
    "35 - 39 ans",
    "40 - 44 ans",
    "45 - 49 ans",
    "50 - 54 ans",
    "55 - 59 ans",
    "60 - 64 ans",
    "65 - 69 ans",
    "70 - 74 ans",
    "75 ans et plus"
  )
  
  classe <- case_when(
    is.na(x) ~ NA_integer_,
    x < 1 ~ 1L,
    x >= 1 & x <= 4 ~ 2L,
    x >= 5 & x <= 9 ~ 3L,
    x >= 10 & x <= 14 ~ 4L,
    x >= 15 & x <= 19 ~ 5L,
    x >= 20 & x <= 24 ~ 6L,
    x >= 25 & x <= 29 ~ 7L,
    x >= 30 & x <= 34 ~ 8L,
    x >= 35 & x <= 39 ~ 9L,
    x >= 40 & x <= 44 ~ 10L,
    x >= 45 & x <= 49 ~ 11L,
    x >= 50 & x <= 54 ~ 12L,
    x >= 55 & x <= 59 ~ 13L,
    x >= 60 & x <= 64 ~ 14L,
    x >= 65 & x <= 69 ~ 15L,
    x >= 70 & x <= 74 ~ 16L,
    x >= 75 ~ 17L
  )
  
  factor(classe, levels = 1:17, labels = labels)
}

#' Créer les classes d'âge réduites (4 classes)
#' @param x Vecteur numérique (années)
#' @return Facteur avec les classes
create_classe_age_reduite <- function(x) {
  
  labels <- c(
    "< 5 ans",
    "5 - 9 ans",
    "10 - 19 ans",
    "20 ans et plus"
  )
  
  classe <- case_when(
    is.na(x) ~ NA_integer_,
    x < 5 ~ 1L,
    x >= 5 & x <= 9 ~ 2L,
    x >= 10 & x <= 19 ~ 3L,
    x >= 20 ~ 4L
  )
  
  factor(classe, levels = 1:4, labels = labels)
}

#' Créer les classes d'effectif détaillées (10 classes)
#' @param x Vecteur numérique (effectif)
#' @return Facteur avec les classes
create_classe_effectif <- function(x) {
  
  labels <- c(
    "1 à 5",
    "6 à 20",
    "21 à 50",
    "51 à 100",
    "101 à 200",
    "201 à 500",
    "501 à 1000",
    "1001 à 5000",
    "5001 à 10000",
    "Plus de 10000"
  )
  
  classe <- case_when(
    is.na(x) ~ NA_integer_,
    x >= 1 & x <= 5 ~ 1L,
    x >= 6 & x <= 20 ~ 2L,
    x >= 21 & x <= 50 ~ 3L,
    x >= 51 & x <= 100 ~ 4L,
    x >= 101 & x <= 200 ~ 5L,
    x >= 201 & x <= 500 ~ 6L,
    x >= 501 & x <= 1000 ~ 7L,
    x >= 1001 & x <= 5000 ~ 8L,
    x >= 5001 & x <= 10000 ~ 9L,
    x > 10000 ~ 10L
  )
  
  factor(classe, levels = 1:10, labels = labels)
}

#' Créer les classes d'effectif réduites (4 classes)
#' @param x Vecteur numérique (effectif)
#' @return Facteur avec les classes
create_classe_effectif_reduite <- function(x) {
  
  labels <- c(
    "1 à 20",
    "21 à 100",
    "101 à 500",
    "Plus de 500"
  )
  
  classe <- case_when(
    is.na(x) ~ NA_integer_,
    x >= 1 & x <= 20 ~ 1L,
    x >= 21 & x <= 100 ~ 2L,
    x >= 101 & x <= 500 ~ 3L,
    x > 500 ~ 4L
  )
  
  factor(classe, levels = 1:4, labels = labels)
}

# ============================================================
# FONCTION DE CRÉATION DES VARIABLES DÉRIVÉES
# ============================================================

create_derived_variables <- function(df) {
  
  message("\n  Création des variables dérivées...")
  
  # ----------------------------------------------------------
  # 1. CALCUL DES ÂGES ET ANCIENNETÉS
  # ----------------------------------------------------------
  message("    - Âges et anciennetés...")
  
  df <- df %>%
    mutate(
      # Âge de l'employé
      AGE_EMPLOYE = if_else(
        !is.na(DATE_NAISSANCE),
        as.numeric(ANNEE) - year(DATE_NAISSANCE),
        NA_real_
      ),
      
      # Ancienneté dans l'entreprise (depuis embauche)
      ANCIENNETE_ENTREPRISE = if_else(
        !is.na(DATE_EMBAUCHE),
        as.numeric(ANNEE) - year(DATE_EMBAUCHE),
        NA_real_
      ),
      
      # Ancienneté depuis immatriculation salarié
      ANCIENNETE_IMMAT = if_else(
        !is.na(DATE_IMMATRICULATION),
        as.numeric(ANNEE) - year(DATE_IMMATRICULATION),
        NA_real_
      ),
      
      # Âge de l'entreprise (depuis immatriculation employeur)
      AGE_ENTREPRISE_IMMAT = if_else(
        !is.na(DATE_IMMAT_EMPLOYEUR),
        as.numeric(ANNEE) - year(DATE_IMMAT_EMPLOYEUR),
        NA_real_
      )
    )
  
  # ----------------------------------------------------------
  # 2. CLASSES D'ÂGE - FORMES LONGUES (17 classes)
  # ----------------------------------------------------------
  message("    - Classes d'âge détaillées...")
  
  df <- df %>%
    mutate(
      CL_AGE_EMPLOYE = create_classe_age(AGE_EMPLOYE),
      CL_ANCIENNETE_ENTREPRISE = create_classe_age(ANCIENNETE_ENTREPRISE),
      CL_ANCIENNETE_IMMAT = create_classe_age(ANCIENNETE_IMMAT),
      CL_AGE_ENTREPRISE_IMMAT = create_classe_age(AGE_ENTREPRISE_IMMAT)
    )
  
  # ----------------------------------------------------------
  # 3. CLASSES D'ÂGE - FORMES RÉDUITES (4 classes)
  # ----------------------------------------------------------
  message("    - Classes d'âge réduites...")
  
  df <- df %>%
    mutate(
      CL_RED_AGE_EMPLOYE = create_classe_age_reduite(AGE_EMPLOYE),
      CL_RED_ANCIENNETE_ENTREPRISE = create_classe_age_reduite(ANCIENNETE_ENTREPRISE),
      CL_RED_ANCIENNETE_IMMAT = create_classe_age_reduite(ANCIENNETE_IMMAT),
      CL_RED_AGE_ENTREPRISE_IMMAT = create_classe_age_reduite(AGE_ENTREPRISE_IMMAT)
    )
  
  # ----------------------------------------------------------
  # 4. CLASSES D'EFFECTIF
  # ----------------------------------------------------------
  message("    - Classes d'effectif...")
  
  # Vérifier si EFFECTIF_SALARIES existe
  if ("EFFECTIF_SALARIES" %in% names(df)) {
    df <- df %>%
      mutate(
        # Classe détaillée (10 classes)
        CLASSE_EFFECTIF = create_classe_effectif(EFFECTIF_SALARIES),
        
        # Classe réduite (4 classes)
        CLASSE_EFFECTIF_REDUITE = create_classe_effectif_reduite(EFFECTIF_SALARIES)
      )
  } else {
    message("      ⚠ Variable EFFECTIF_SALARIES absente")
  }
  
  # ----------------------------------------------------------
  # 5. PÉRIODE
  # ----------------------------------------------------------
  message("    - Variables de période...")
  
  df <- df %>%
    mutate(
      PERIOD = paste(ANNEE, sprintf("%02d", MOIS), sep = "_"),
      TRIMESTRE = ceiling(MOIS / 3),
      SEMESTRE = ceiling(MOIS / 6)
    )
  
  # ----------------------------------------------------------
  # 6. SALAIRE MENSUEL BRUT
  # ----------------------------------------------------------
  message("    - Salaire mensuel brut...")
  
  if (all(c("SALAIRE_BRUT", "DUREE_TRAVAILLEE") %in% names(df))) {
    df <- df %>%
      mutate(
        SALAIRE_BRUT_MENS = case_when(
          !is.na(DUREE_TRAVAILLEE) & DUREE_TRAVAILLEE > 0 ~ SALAIRE_BRUT / DUREE_TRAVAILLEE,
          TRUE ~ SALAIRE_BRUT
        )
      )
  }
  
  return(df)
}

# ============================================================
# FONCTION DE NETTOYAGE DES DONNÉES
# ============================================================

apply_data_cleaning <- function(df, apply_filters = TRUE) {
  
  if (!apply_filters) {
    message("  Filtres de nettoyage désactivés")
    return(df)
  }
  
  message("\n  Application des filtres de nettoyage...")
  n_initial <- nrow(df)
  
  # ----------------------------------------------------------
  # 1. Suppression des SALAIRE_BRUT manquants
  # ----------------------------------------------------------
  if ("SALAIRE_BRUT" %in% names(df)) {
    df <- df %>% filter(!is.na(SALAIRE_BRUT))
    message("    - Après suppression NA SALAIRE_BRUT: ", 
            format(nrow(df), big.mark = " "))
  }
  
  # ----------------------------------------------------------
  # 2. Suppression des SALAIRE_BRUT == 0
  # ----------------------------------------------------------
  if ("SALAIRE_BRUT" %in% names(df)) {
    df <- df %>% filter(SALAIRE_BRUT != 0)
    message("    - Après suppression SALAIRE_BRUT == 0: ", 
            format(nrow(df), big.mark = " "))
  }
  
  # ----------------------------------------------------------
  # 3. Exclusion des types de salariés (H, J)
  # ----------------------------------------------------------
  if ("TYPE_SALARIE" %in% names(df)) {
    exclude_types <- PROCESSING$exclude_type_salarie
    df <- df %>% filter(!(TYPE_SALARIE %in% exclude_types))
    message("    - Après exclusion types ", 
            paste(exclude_types, collapse = "/"), ": ",
            format(nrow(df), big.mark = " "))
  }
  
  # ----------------------------------------------------------
  # 4. Correction DUREE_TRAVAILLEE pour TYPE_SALARIE == "M"
  # ----------------------------------------------------------
  if (all(c("DUREE_TRAVAILLEE", "TYPE_SALARIE") %in% names(df))) {
    df <- df %>%
      mutate(
        # Si DUREE_TRAVAILLEE == 0 et mensuel -> 1
        DUREE_TRAVAILLEE = if_else(
          DUREE_TRAVAILLEE == 0 & TYPE_SALARIE == "M",
          1,
          DUREE_TRAVAILLEE
        ),
        # Si DUREE_TRAVAILLEE > 1 et mensuel -> 1
        DUREE_TRAVAILLEE = if_else(
          DUREE_TRAVAILLEE > 1 & TYPE_SALARIE == "M",
          1,
          DUREE_TRAVAILLEE
        )
      )
    message("    - Correction DUREE_TRAVAILLEE pour mensuels")
  }
  
  # ----------------------------------------------------------
  # 5. Seuil minimum salaire (SMIG)
  # ----------------------------------------------------------
  if (all(c("SALAIRE_BRUT", "TYPE_SALARIE") %in% names(df))) {
    min_salaire <- PROCESSING$min_salaire_mensuel
    df <- df %>%
      filter(!(SALAIRE_BRUT < min_salaire & TYPE_SALARIE == "M"))
    message("    - Après seuil minimum salaire (", 
            format(min_salaire, big.mark = " "), "): ",
            format(nrow(df), big.mark = " "))
  }
  
  # ----------------------------------------------------------
  # 6. Recalcul SALAIRE_BRUT_MENS après nettoyage
  # ----------------------------------------------------------
  if (all(c("SALAIRE_BRUT", "DUREE_TRAVAILLEE") %in% names(df))) {
    df <- df %>%
      mutate(
        SALAIRE_BRUT_MENS = case_when(
          !is.na(DUREE_TRAVAILLEE) & DUREE_TRAVAILLEE > 0 ~ SALAIRE_BRUT / DUREE_TRAVAILLEE,
          TRUE ~ SALAIRE_BRUT
        )
      )
  }
  
  # Résumé
  n_final <- nrow(df)
  n_supprime <- n_initial - n_final
  pct_supprime <- round(n_supprime / n_initial * 100, 1)
  
  message("    - Total supprimé: ", format(n_supprime, big.mark = " "),
          " (", pct_supprime, "%)")
  
  return(df)
}

# ============================================================
# FONCTION DE VALIDATION DES VALEURS ABERRANTES
# ============================================================

validate_derived_variables <- function(df) {
  
  message("\n  Validation des variables dérivées...")
  
  issues <- list()
  
  # ----------------------------------------------------------
  # Vérification des âges négatifs
  # ----------------------------------------------------------
  age_vars <- c("AGE_EMPLOYE", "ANCIENNETE_ENTREPRISE", 
                "ANCIENNETE_IMMAT", "AGE_ENTREPRISE_IMMAT")
  
  for (var in age_vars) {
    if (var %in% names(df)) {
      n_negative <- sum(df[[var]] < 0, na.rm = TRUE)
      if (n_negative > 0) {
        issues[[var]] <- list(
          type = "negative_values",
          count = n_negative,
          message = paste(var, ": ", n_negative, " valeurs négatives")
        )
        message("    ⚠ ", var, ": ", n_negative, " valeurs négatives détectées")
      }
    }
  }
  
  # ----------------------------------------------------------
  # Vérification des âges improbables (> 100 ans)
  # ----------------------------------------------------------
  if ("AGE_EMPLOYE" %in% names(df)) {
    n_improbable <- sum(df$AGE_EMPLOYE > 100, na.rm = TRUE)
    if (n_improbable > 0) {
      issues$age_improbable <- list(
        type = "improbable_age",
        count = n_improbable,
        message = paste("AGE_EMPLOYE > 100 ans: ", n_improbable, " observations")
      )
      message("    ⚠ AGE_EMPLOYE > 100 ans: ", n_improbable, " observations")
    }
  }
  
  # ----------------------------------------------------------
  # Vérification des anciennetés > âge
  # ----------------------------------------------------------
  if (all(c("AGE_EMPLOYE", "ANCIENNETE_ENTREPRISE") %in% names(df))) {
    n_incoherent <- sum(
      df$ANCIENNETE_ENTREPRISE > df$AGE_EMPLOYE, 
      na.rm = TRUE
    )
    if (n_incoherent > 0) {
      issues$anciennete_age <- list(
        type = "incoherent",
        count = n_incoherent,
        message = paste("Ancienneté > Âge: ", n_incoherent, " observations")
      )
      message("    ⚠ Ancienneté entreprise > Âge employé: ", n_incoherent, " observations")
    }
  }
  
  # ----------------------------------------------------------
  # Résumé des taux de remplissage
  # ----------------------------------------------------------
  message("\n  Taux de remplissage des variables créées:")
  
  vars_to_check <- c(
    "AGE_EMPLOYE", "ANCIENNETE_ENTREPRISE", "ANCIENNETE_IMMAT",
    "AGE_ENTREPRISE_IMMAT", "CLASSE_EFFECTIF", "CLASSE_EFFECTIF_REDUITE",
    "SALAIRE_BRUT_MENS"
  )
  
  for (var in vars_to_check) {
    if (var %in% names(df)) {
      n_valid <- sum(!is.na(df[[var]]))
      pct_valid <- round(n_valid / nrow(df) * 100, 1)
      message("    - ", var, ": ", pct_valid, "%")
    }
  }
  
  return(list(
    valid = length(issues) == 0,
    issues = issues
  ))
}

# ============================================================
# FONCTION PRINCIPALE
# ============================================================

clean_data <- function(input_file = NULL, 
                       output_file = NULL,
                       apply_filters = TRUE,
                       create_variables = TRUE) {
  
  message("\n", strrep("=", 60))
  message("NETTOYAGE DES DONNÉES ET CRÉATION DES VARIABLES")
  message(strrep("=", 60))
  
  # ----------------------------------------------------------
  # Détermination du fichier d'entrée
  # ----------------------------------------------------------
  if (is.null(input_file)) {
    concat_files <- list.files(
      PATHS$cleaned_monthly, 
      pattern = "^data_cnps_.*\\.dta$",
      full.names = TRUE
    )
    if (length(concat_files) == 0) {
      stop("Aucun fichier concaténé trouvé dans ", PATHS$cleaned_monthly)
    }
    input_file <- concat_files[which.max(file.mtime(concat_files))]
  }
  
  # ----------------------------------------------------------
  # Détermination du fichier de sortie
  # ----------------------------------------------------------
  if (is.null(output_file)) {
    output_file <- file.path(
      PATHS$cleaned_final,
      gsub("\\.dta$", "_cleaned.dta", basename(input_file))
    )
  }
  
  message("Fichier source: ", input_file)
  
  # ----------------------------------------------------------
  # Chargement des données
  # ----------------------------------------------------------
  df <- read_dta(input_file)
  n_initial <- nrow(df)
  message("Observations initiales: ", format(n_initial, big.mark = " "))
  message("Variables initiales: ", ncol(df))
  
  # ----------------------------------------------------------
  # Vérification des variables requises
  # ----------------------------------------------------------
  message("\n  Vérification des variables requises...")
  
  required_numeric <- REQUIRED_VARS$numeric
  required_character <- REQUIRED_VARS$character
  
  missing_num <- setdiff(required_numeric, names(df))
  missing_chr <- setdiff(required_character, names(df))
  
  if (length(missing_num) > 0) {
    warning("Variables numériques manquantes: ", paste(missing_num, collapse = ", "))
  }
  if (length(missing_chr) > 0) {
    warning("Variables caractères manquantes: ", paste(missing_chr, collapse = ", "))
  }
  
  # ----------------------------------------------------------
  # Création des variables dérivées
  # ----------------------------------------------------------
  if (create_variables) {
    df <- create_derived_variables(df)
  }
  
  # ----------------------------------------------------------
  # Application des filtres de nettoyage
  # ----------------------------------------------------------
  df <- apply_data_cleaning(df, apply_filters = apply_filters)
  
  # ----------------------------------------------------------
  # Validation des variables créées
  # ----------------------------------------------------------
  validation <- validate_derived_variables(df)
  
  # ----------------------------------------------------------
  # Sauvegarde
  # ----------------------------------------------------------
  message("\n  Sauvegarde...")
  ensure_dir(dirname(output_file))
  write_dta(df, output_file, version = STATA_VERSION)
  
  # ----------------------------------------------------------
  # Résumé final
  # ----------------------------------------------------------
  n_final <- nrow(df)
  n_supprime <- n_initial - n_final
  pct_supprime <- round(n_supprime / n_initial * 100, 1)
  
  message("\n", strrep("-", 40))
  message("RÉSUMÉ")
  message(strrep("-", 40))
  message("Observations initiales: ", format(n_initial, big.mark = " "))
  message("Observations finales:   ", format(n_final, big.mark = " "))
  message("Observations supprimées: ", format(n_supprime, big.mark = " "), 
          " (", pct_supprime, "%)")
  message("Variables finales: ", ncol(df))
  message("Fichier: ", output_file)
  
  # Liste des nouvelles variables créées
  new_vars <- c(
    "AGE_EMPLOYE", "ANCIENNETE_ENTREPRISE", "ANCIENNETE_IMMAT", 
    "AGE_ENTREPRISE_IMMAT",
    "CL_AGE_EMPLOYE", "CL_ANCIENNETE_ENTREPRISE", 
    "CL_ANCIENNETE_IMMAT", "CL_AGE_ENTREPRISE_IMMAT",
    "CL_RED_AGE_EMPLOYE", "CL_RED_ANCIENNETE_ENTREPRISE",
    "CL_RED_ANCIENNETE_IMMAT", "CL_RED_AGE_ENTREPRISE_IMMAT",
    "CLASSE_EFFECTIF", "CLASSE_EFFECTIF_REDUITE",
    "PERIOD", "TRIMESTRE", "SEMESTRE", "SALAIRE_BRUT_MENS"
  )
  
  created_vars <- intersect(new_vars, names(df))
  message("\nVariables créées (", length(created_vars), "):")
  for (v in created_vars) {
    message("  - ", v)
  }
  
  return(list(
    output_file = output_file,
    n_initial = n_initial,
    n_final = n_final,
    n_supprime = n_supprime,
    n_vars = ncol(df),
    validation = validation,
    created_variables = created_vars
  ))
}

# ============================================================
# FONCTION UTILITAIRE: Afficher la distribution des classes
# ============================================================

show_class_distribution <- function(df, var_name) {
  
  if (!var_name %in% names(df)) {
    message("Variable ", var_name, " non trouvée")
    return(NULL)
  }
  
  tab <- df %>%
    group_by(!!sym(var_name)) %>%
    summarise(
      n = n(),
      pct = round(n() / nrow(df) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(!!sym(var_name))
  
  message("\nDistribution de ", var_name, ":")
  print(tab, n = Inf)
  
  return(tab)
}

message("Module de nettoyage et création des variables chargé")