# ============================================================
# 01_DECLARATION_MODEL.R
# Modèle de probabilité de déclaration entreprise-mois
# Utilise les variables créées dans les étapes précédentes
# ============================================================

library(dplyr)
library(pROC)

# Préparer les données pour le modèle de déclaration
prepare_declaration_data <- function(data_firm_time) {
  
  message("Préparation des données pour le modèle de déclaration...")
  
  # Vérifier les variables requises
  required_vars <- c("NUMERO_EMPLOYEUR", "ANNEE", "MOIS", "D_jt")
  missing <- setdiff(required_vars, names(data_firm_time))
  
  if (length(missing) > 0) {
    stop("Variables manquantes: ", paste(missing, collapse = ", "))
  }
  
  df <- data_firm_time
  
  # Vérifier et utiliser les variables existantes
  message("  Variables utilisées:")
  
  # Variables de lag (créées dans 02_create_firm_time_base.R)
  if ("D_jt_lag1" %in% names(df)) {
    message("    - D_jt_lag1 (existante)")
  }
  if ("D_jt_lag2" %in% names(df)) {
    message("    - D_jt_lag2 (existante)")
  }
  if ("PCT_DECLARATIONS_PASSEES" %in% names(df)) {
    message("    - PCT_DECLARATIONS_PASSEES (existante)")
  }
  if ("log_Y_bar_jt_lag1" %in% names(df)) {
    message("    - log_Y_bar_jt_lag1 (existante)")
  }
  
  # Classes d'effectif (créées dans 03_data_cleaning.R)
  if ("CLASSE_EFFECTIF_REDUITE" %in% names(df)) {
    message("    - CLASSE_EFFECTIF_REDUITE (existante)")
  }
  
  # Âge entreprise (créé dans 03_data_cleaning.R)
  if ("CL_RED_AGE_ENTREPRISE" %in% names(df)) {
    message("    - CL_RED_AGE_ENTREPRISE (existante)")
  }
  
  # Secteur
  if ("SECTEUR_ACTIVITE_COD" %in% names(df)) {
    message("    - SECTEUR_ACTIVITE_COD (existante)")
  }
  
  # Créer la variable de période numérique si nécessaire
  if (!"PERIOD_NUM" %in% names(df)) {
    df <- df %>%
      mutate(PERIOD_NUM = ANNEE * 12 + MOIS)
  }
  
  # Statistiques
  message("\n  Statistiques:")
  message("    Observations: ", format(nrow(df), big.mark = " "))
  message("    Entreprises: ", n_distinct(df$NUMERO_EMPLOYEUR))
  message("    Taux déclaration: ", round(mean(df$D_jt, na.rm = TRUE) * 100, 1), "%")
  
  return(df)
}

# Estimer le modèle de déclaration
estimate_declaration_model <- function(df, spec = NULL) {
  
  if (is.null(spec)) {
    spec <- MODEL_SPECS$declaration_firm
  }
  
  message("\nEstimation du modèle de déclaration...")
  
  # Construire la liste des covariables disponibles
  all_covariates <- unlist(spec$covariates)
  available_covariates <- intersect(all_covariates, names(df))
  
  if (length(available_covariates) == 0) {
    warning("Aucune covariable disponible, utilisation d'un modèle minimal")
    available_covariates <- "1"  # Intercept seulement
  }
  
  message("  Covariables utilisées: ", paste(available_covariates, collapse = ", "))
  
  # Filtrer les observations complètes
  df_model <- df %>%
    filter(!is.na(D_jt)) %>%
    select(D_jt, all_of(available_covariates)) %>%
    na.omit()
  
  message("  Observations pour estimation: ", nrow(df_model))
  
  # Formule
  formula_str <- paste("D_jt ~", paste(available_covariates, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Estimation
  model <- glm(
    formula_obj,
    data = df_model,
    family = binomial(link = spec$estimation$link)
  )
  
  message("  Convergence: ", ifelse(model$converged, "OK", "NON"))
  message("  AIC: ", round(AIC(model), 2))
  
  return(model)
}

# Calculer les diagnostics du modèle
compute_declaration_diagnostics <- function(model, df) {
  
  message("\nCalcul des diagnostics...")
  
  # Prédictions
  df$pi_jt_pred <- predict(model, newdata = df, type = "response")
  
  # Filtrer les observations utilisées
  df_valid <- df %>% filter(!is.na(D_jt) & !is.na(pi_jt_pred))
  
  # ROC et AUC
  roc_obj <- roc(df_valid$D_jt, df_valid$pi_jt_pred, quiet = TRUE)
  auc_value <- as.numeric(auc(roc_obj))
  
  # Calibration
  n_bins <- 10
  df_valid$prob_bin <- cut(df_valid$pi_jt_pred, 
                           breaks = quantile(df_valid$pi_jt_pred, 
                                             probs = seq(0, 1, 1/n_bins),
                                             na.rm = TRUE),
                           include.lowest = TRUE)
  
  calibration <- df_valid %>%
    group_by(prob_bin) %>%
    summarise(
      n = n(),
      mean_pred = mean(pi_jt_pred, na.rm = TRUE),
      mean_obs = mean(D_jt, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Slope de calibration
  calib_model <- lm(mean_obs ~ mean_pred, data = calibration)
  calibration_slope <- coef(calib_model)[2]
  
  diagnostics <- list(
    auc = auc_value,
    calibration_slope = calibration_slope,
    n_obs = nrow(df_valid),
    n_events = sum(df_valid$D_jt),
    event_rate = mean(df_valid$D_jt)
  )
  
  message("  AUC: ", round(auc_value, 3))
  message("  Calibration slope: ", round(calibration_slope, 3))
  
  return(diagnostics)
}

# Calculer les poids IPW
compute_ipw_weights <- function(model, df, spec = NULL) {
  
  if (is.null(spec)) {
    spec <- MODEL_SPECS$declaration_firm$ipw
  }
  
  message("\nCalcul des poids IPW...")
  
  # Probabilités prédites
  df$pi_jt <- predict(model, newdata = df, type = "response")
  
  # Remplacer NA par probabilité marginale
  marginal_prob <- mean(df$pi_jt, na.rm = TRUE)
  df$pi_jt[is.na(df$pi_jt)] <- marginal_prob
  
  # Appliquer le minimum
  df$pi_jt <- pmax(df$pi_jt, 0.01)
  
  # Calculer les poids
  df$w_jt_raw <- 1 / df$pi_jt
  
  # Stabilisation
  if (isTRUE(spec$stabilize)) {
    df$w_jt_raw <- marginal_prob / df$pi_jt
    message("  Poids stabilisés")
  }
  
  # Troncature
  trim_q <- spec$trim_quantiles
  bounds <- quantile(df$w_jt_raw, probs = trim_q, na.rm = TRUE)
  df$w_jt <- pmax(pmin(df$w_jt_raw, bounds[2]), bounds[1])
  
  message("  Poids moyen: ", round(mean(df$w_jt, na.rm = TRUE), 3))
  message("  Poids min: ", round(min(df$w_jt, na.rm = TRUE), 3))
  message("  Poids max: ", round(max(df$w_jt, na.rm = TRUE), 3))
  
  return(df)
}

# Fonction principale
run_declaration_modeling <- function(data_firm_time) {
  
  message("\n", strrep("=", 60))
  message("MODÉLISATION DÉCLARATION ENTREPRISE")
  message(strrep("=", 60))
  
  # Préparer les données
  df <- prepare_declaration_data(data_firm_time)
  
  # Estimer le modèle
  model <- estimate_declaration_model(df)
  
  # Diagnostics
  diagnostics <- compute_declaration_diagnostics(model, df)
  
  # Calculer les poids
  df <- compute_ipw_weights(model, df)
  
  # Enregistrer le modèle
  model_id <- register_model(model, "declaration_firm", diagnostics)
  
  return(list(
    model = model,
    model_id = model_id,
    data = df,
    diagnostics = diagnostics
  ))
}

message("Module de modélisation déclaration chargé")