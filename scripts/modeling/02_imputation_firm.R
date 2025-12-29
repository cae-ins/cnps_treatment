# ============================================================
# 02_IMPUTATION_FIRM.R
# Imputation du salaire moyen pour les entreprises non-déclarantes
# Utilise les variables créées dans les étapes précédentes
# ============================================================

library(dplyr)

# Préparer les données pour l'imputation
prepare_imputation_data <- function(data_firm_time) {
  
  message("Préparation des données pour l'imputation...")
  
  # Filtrer les entreprises déclarantes (D_jt = 1)
  df <- data_firm_time %>%
    filter(D_jt == 1)
  
  message("  Entreprises-mois déclarantes: ", format(nrow(df), big.mark = " "))
  
  # Vérifier les variables existantes
  message("  Variables utilisées:")
  
  # log_Y_bar_jt (créé dans 02_create_firm_time_base.R)
  if ("log_Y_bar_jt" %in% names(df)) {
    message("    - log_Y_bar_jt (existante)")
  } else {
    df <- df %>%
      mutate(log_Y_bar_jt = log(SALAIRE_MOYEN + 1))
    message("    - log_Y_bar_jt (calculée)")
  }
  
  # Variables de lag
  if ("log_Y_bar_jt_lag1" %in% names(df)) {
    message("    - log_Y_bar_jt_lag1 (existante)")
  }
  if ("log_Y_bar_jt_lag2" %in% names(df)) {
    message("    - log_Y_bar_jt_lag2 (existante)")
  }
  
  # Caractéristiques entreprise
  if ("CLASSE_EFFECTIF_REDUITE" %in% names(df)) {
    message("    - CLASSE_EFFECTIF_REDUITE (existante)")
  }
  if ("CL_RED_AGE_ENTREPRISE" %in% names(df)) {
    message("    - CL_RED_AGE_ENTREPRISE (existante)")
  }
  if ("SECTEUR_ACTIVITE_COD" %in% names(df)) {
    message("    - SECTEUR_ACTIVITE_COD (existante)")
  }
  
  return(df)
}

# Estimer le modèle d'imputation
estimate_imputation_model <- function(df, spec = NULL) {
  
  if (is.null(spec)) {
    spec <- MODEL_SPECS$imputation_firm_salary
  }
  
  message("\nEstimation du modèle d'imputation...")
  
  # Covariables disponibles
  all_covariates <- unlist(spec$covariates)
  available_covariates <- intersect(all_covariates, names(df))
  
  message("  Covariables: ", paste(available_covariates, collapse = ", "))
  
  # Filtrer les observations complètes
  df_model <- df %>%
    filter(!is.na(log_Y_bar_jt)) %>%
    select(log_Y_bar_jt, all_of(available_covariates)) %>%
    na.omit()
  
  message("  Observations: ", nrow(df_model))
  
  # Formule
  if (length(available_covariates) > 0) {
    formula_str <- paste("log_Y_bar_jt ~", paste(available_covariates, collapse = " + "))
  } else {
    formula_str <- "log_Y_bar_jt ~ 1"
  }
  
  formula_obj <- as.formula(formula_str)
  
  # Estimation
  model <- lm(formula_obj, data = df_model)
  
  message("  R²: ", round(summary(model)$r.squared, 3))
  message("  RMSE: ", round(sqrt(mean(model$residuals^2)), 3))
  
  return(model)
}

# Générer les imputations multiples
generate_firm_imputations <- function(model, data_to_impute, n_imputations = 5) {
  
  message("\nGénération de ", n_imputations, " imputations...")
  
  # Prédiction moyenne
  pred_mean <- predict(model, newdata = data_to_impute)
  residual_sd <- sigma(model)
  
  # Générer les imputations
  imputations <- list()
  
  for (m in 1:n_imputations) {
    # Ajouter du bruit résiduel
    noise <- rnorm(length(pred_mean), mean = 0, sd = residual_sd)
    log_Y_imputed <- pred_mean + noise
    Y_imputed <- exp(log_Y_imputed) - 1
    
    imputations[[m]] <- data_to_impute %>%
      mutate(
        imputation_id = m,
        log_Y_bar_jt_imputed = log_Y_imputed,
        Y_bar_jt_imputed = Y_imputed
      )
  }
  
  df_imputed <- bind_rows(imputations)
  
  message("  Imputations générées: ", n_imputations)
  message("  Observations imputées: ", nrow(data_to_impute))
  
  return(df_imputed)
}

# Calculer les diagnostics
compute_imputation_diagnostics <- function(model, df_imputed) {
  
  diagnostics <- list(
    r_squared = summary(model)$r.squared,
    adj_r_squared = summary(model)$adj.r.squared,
    rmse = sqrt(mean(model$residuals^2)),
    n_obs_model = nobs(model),
    n_imputed = nrow(df_imputed) / length(unique(df_imputed$imputation_id)),
    n_imputations = length(unique(df_imputed$imputation_id)),
    aic = AIC(model),
    bic = BIC(model)
  )
  
  return(diagnostics)
}

# Fonction principale
run_firm_imputation <- function(data_firm_time) {
  
  message("\n", strrep("=", 60))
  message("IMPUTATION SALAIRE MOYEN ENTREPRISE")
  message(strrep("=", 60))
  
  # Préparer les données d'estimation
  df_declarants <- prepare_imputation_data(data_firm_time)
  
  # Estimer le modèle
  model <- estimate_imputation_model(df_declarants)
  
  # Identifier les entreprises à imputer (non-déclarantes)
  df_non_declarants <- data_firm_time %>%
    filter(D_jt == 0)
  
  message("\nEntreprises-mois à imputer: ", format(nrow(df_non_declarants), big.mark = " "))
  
  if (nrow(df_non_declarants) == 0) {
    message("Aucune imputation nécessaire")
    return(list(
      model = model,
      data = data_firm_time,
      diagnostics = compute_imputation_diagnostics(model, data.frame())
    ))
  }
  
  # Générer les imputations
  n_imp <- PROCESSING$n_imputations
  df_imputed <- generate_firm_imputations(model, df_non_declarants, n_imp)
  
  # Diagnostics
  diagnostics <- compute_imputation_diagnostics(model, df_imputed)
  
  # Enregistrer le modèle
  model_id <- register_model(model, "imputation_firm_salary", diagnostics)
  
  return(list(
    model = model,
    model_id = model_id,
    data_imputed = df_imputed,
    diagnostics = diagnostics
  ))
}

message("Module d'imputation entreprise chargé")