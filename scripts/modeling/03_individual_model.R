# ============================================================
# 03_INDIVIDUAL_MODEL.R
# Modèle de déclaration/observation individuelle conditionnelle
# Utilise les variables créées dans les étapes précédentes
# ============================================================

library(dplyr)
library(pROC)

# Préparer les données
prepare_individual_model_data <- function(df_individual, df_firm_time) {
  
  message("Préparation des données pour le modèle individuel...")
  
  # Joindre les informations entreprise-temps
  df <- df_individual %>%
    left_join(
      df_firm_time %>%
        select(NUMERO_EMPLOYEUR, ANNEE, MOIS, D_jt, N_SALARIES),
      by = c("NUMERO_EMPLOYEUR", "ANNEE", "MOIS")
    )
  
  # Indicateur d'observation
  df <- df %>%
    mutate(S_ijt = 1)
  
  # Vérifier les variables existantes
  message("  Variables utilisées:")
  
  # Variables individuelles de 03_data_cleaning.R
  if ("AGE_EMPLOYE" %in% names(df)) {
    message("    - AGE_EMPLOYE (existante)")
  }
  if ("CL_RED_AGE_EMPLOYE" %in% names(df)) {
    message("    - CL_RED_AGE_EMPLOYE (existante)")
  }
  if ("ANCIENNETE_ENTREPRISE" %in% names(df)) {
    message("    - ANCIENNETE_ENTREPRISE (existante)")
  }
  if ("CL_RED_ANCIENNETE_ENTREPRISE" %in% names(df)) {
    message("    - CL_RED_ANCIENNETE_ENTREPRISE (existante)")
  }
  if ("SEXE" %in% names(df)) {
    message("    - SEXE (existante)")
  }
  
  message("  Observations: ", format(nrow(df), big.mark = " "))
  
  return(df)
}

# Estimer le modèle
estimate_individual_model <- function(df, spec = NULL) {
  
  if (is.null(spec)) {
    spec <- MODEL_SPECS$declaration_individual
  }
  
  message("\nEstimation du modèle individuel...")
  
  # Covariables disponibles
  covariates <- unlist(spec$covariates)
  available_covariates <- intersect(covariates, names(df))
  
  message("  Covariables: ", paste(available_covariates, collapse = ", "))
  
  if (length(available_covariates) == 0) {
    available_covariates <- "1"
  }
  
  # Formule
  formula_str <- paste("S_ijt ~", paste(available_covariates, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Note: modèle trivial car S_ijt = 1 pour tous
  # En pratique, on aurait besoin de non-observés
  model <- glm(
    formula_obj,
    data = df,
    family = binomial(link = "logit")
  )
  
  message("  Convergence: ", ifelse(model$converged, "OK", "NON"))
  
  return(model)
}

# Calculer les poids individuels
compute_individual_weights <- function(model, df, spec = NULL) {
  
  if (is.null(spec)) {
    spec <- MODEL_SPECS$declaration_individual$ipw
  }
  
  message("\nCalcul des poids individuels...")
  
  # Probabilités prédites
  df$rho_ijt <- predict(model, newdata = df, type = "response")
  
  # Appliquer le minimum
  df$rho_ijt <- pmax(df$rho_ijt, 0.01)
  
  # Calculer les poids
  df$w_ijt_raw <- 1 / df$rho_ijt
  
  # Stabilisation
  if (isTRUE(spec$stabilize)) {
    marginal_prob <- mean(df$rho_ijt, na.rm = TRUE)
    df$w_ijt_raw <- marginal_prob / df$rho_ijt
  }
  
  # Troncature
  bounds <- quantile(df$w_ijt_raw, probs = spec$trim_quantiles, na.rm = TRUE)
  df$w_ijt <- pmax(pmin(df$w_ijt_raw, bounds[2]), bounds[1])
  
  message("  Poids moyen: ", round(mean(df$w_ijt), 3))
  
  return(df)
}

# Fonction principale
run_individual_modeling <- function(df_individual, df_firm_time) {
  
  message("\n", strrep("=", 60))
  message("MODÉLISATION INDIVIDUELLE CONDITIONNELLE")
  message(strrep("=", 60))
  
  # Préparer les données
  df <- prepare_individual_model_data(df_individual, df_firm_time)
  
  # Estimer le modèle
  model <- estimate_individual_model(df)
  
  # Calculer les poids
  df <- compute_individual_weights(model, df)
  
  # Diagnostics simplifiés (pas d'AUC car S_ijt = 1 partout)
  diagnostics <- list(
    n_obs = nrow(df),
    mean_weight = mean(df$w_ijt)
  )
  
  # Enregistrer le modèle
  model_id <- register_model(model, "declaration_individual", diagnostics)
  
  return(list(
    model = model,
    model_id = model_id,
    data = df,
    diagnostics = diagnostics
  ))
}

message("Module de modélisation individuelle chargé")