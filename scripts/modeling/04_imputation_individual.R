# ============================================================
# 04_IMPUTATION_INDIVIDUAL.R
# Imputation des salaires individuels manquants
# Utilise les variables créées dans les étapes précédentes
# ============================================================

library(dplyr)

# Préparer les données
prepare_individual_imputation_data <- function(df_individual) {
  
  message("Préparation des données pour l'imputation individuelle...")
  
  df <- df_individual %>%
    filter(!is.na(SALAIRE_BRUT_MENS))
  
  # Créer log si nécessaire
  if (!"log_Y_ijt" %in% names(df)) {
    df <- df %>%
      mutate(log_Y_ijt = log(SALAIRE_BRUT_MENS + 1))
  }
  
  # Vérifier les variables existantes
  message("  Variables utilisées:")
  
  if ("CL_RED_AGE_EMPLOYE" %in% names(df)) {
    message("    - CL_RED_AGE_EMPLOYE (existante)")
  }
  if ("CL_RED_ANCIENNETE_ENTREPRISE" %in% names(df)) {
    message("    - CL_RED_ANCIENNETE_ENTREPRISE (existante)")
  }
  if ("SEXE" %in% names(df)) {
    message("    - SEXE (existante)")
  }
  if ("CLASSE_EFFECTIF_REDUITE" %in% names(df)) {
    message("    - CLASSE_EFFECTIF_REDUITE (existante)")
  }
  
  message("  Observations avec salaire: ", format(nrow(df), big.mark = " "))
  
  return(df)
}

# Estimer le modèle
estimate_individual_imputation_model <- function(df, spec = NULL) {
  
  if (is.null(spec)) {
    spec <- MODEL_SPECS$imputation_individual_salary
  }
  
  message("\nEstimation du modèle d'imputation individuelle...")
  
  covariates <- unlist(spec$covariates)
  available_covariates <- intersect(covariates, names(df))
  
  message("  Covariables: ", paste(available_covariates, collapse = ", "))
  
  if (length(available_covariates) == 0) {
    formula_str <- "log_Y_ijt ~ 1"
  } else {
    formula_str <- paste("log_Y_ijt ~", paste(available_covariates, collapse = " + "))
  }
  
  formula_obj <- as.formula(formula_str)
  
  model <- lm(formula_obj, data = df)
  
  message("  R²: ", round(summary(model)$r.squared, 3))
  
  return(model)
}

# Générer les imputations
generate_individual_imputations <- function(model, data_to_impute, n_imputations = 5) {
  
  message("\nGénération de ", n_imputations, " imputations...")
  
  pred_mean <- predict(model, newdata = data_to_impute)
  residual_sd <- sigma(model)
  
  imputations <- list()
  
  for (m in 1:n_imputations) {
    noise <- rnorm(length(pred_mean), mean = 0, sd = residual_sd)
    log_Y_imputed <- pred_mean + noise
    Y_imputed <- exp(log_Y_imputed) - 1
    
    imputations[[m]] <- data_to_impute %>%
      mutate(
        imputation_id = m,
        log_Y_ijt_imputed = log_Y_imputed,
        Y_ijt_imputed = Y_imputed
      )
  }
  
  df_imputed <- bind_rows(imputations)
  
  return(df_imputed)
}

# Fonction principale
run_individual_imputation <- function(df_individual) {
  
  message("\n", strrep("=", 60))
  message("IMPUTATION SALAIRE INDIVIDUEL")
  message(strrep("=", 60))
  
  # Préparer les données
  df <- prepare_individual_imputation_data(df_individual)
  
  # Estimer le modèle
  model <- estimate_individual_imputation_model(df)
  
  # Identifier les individus à imputer
  data_to_impute <- df_individual %>%
    filter(is.na(SALAIRE_BRUT_MENS))
  
  message("\nIndividus à imputer: ", format(nrow(data_to_impute), big.mark = " "))
  
  if (nrow(data_to_impute) == 0) {
    message("Aucune imputation individuelle nécessaire")
    return(list(model = model, data = df_individual))
  }
  
  # Générer les imputations
  n_imp <- PROCESSING$n_imputations
  df_imputed <- generate_individual_imputations(model, data_to_impute, n_imp)
  
  # Diagnostics
  diagnostics <- list(
    r_squared = summary(model)$r.squared,
    rmse = sqrt(mean(model$residuals^2)),
    n_imputed = nrow(data_to_impute)
  )
  
  # Enregistrer le modèle
  model_id <- register_model(model, "imputation_individual_salary", diagnostics)
  
  return(list(
    model = model,
    model_id = model_id,
    data_imputed = df_imputed,
    diagnostics = diagnostics
  ))
}

message("Module d'imputation individuelle chargé")