# ============================================================
# 02_MODEL_DIAGNOSTICS.R
# Diagnostics des modèles statistiques
# ============================================================

library(dplyr)
library(ggplot2)
library(pROC)

# Diagnostics pour modèle de classification (logit)
compute_classification_diagnostics <- function(model, data, outcome_var) {
  
  message("Calcul des diagnostics de classification...")
  
  # Prédictions
  prob_pred <- predict(model, type = "response")
  outcome <- data[[outcome_var]]
  
  # ROC et AUC
  roc_obj <- roc(outcome, prob_pred, quiet = TRUE)
  auc_value <- as.numeric(auc(roc_obj))
  
  # Matrice de confusion à différents seuils
  thresholds <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  confusion_matrices <- lapply(thresholds, function(t) {
    pred_class <- ifelse(prob_pred >= t, 1, 0)
    table(Predicted = pred_class, Actual = outcome)
  })
  names(confusion_matrices) <- paste0("threshold_", thresholds)
  
  # Calibration
  n_bins <- 10
  data$prob_pred <- prob_pred
  data$prob_bin <- cut(prob_pred, 
                       breaks = quantile(prob_pred, probs = seq(0, 1, 1/n_bins)),
                       include.lowest = TRUE, labels = 1:n_bins)
  
  calibration <- data %>%
    group_by(prob_bin) %>%
    summarise(
      n = n(),
      mean_pred = mean(prob_pred),
      mean_obs = mean(.data[[outcome_var]]),
      .groups = "drop"
    )
  
  # Slope de calibration
  calib_model <- lm(mean_obs ~ mean_pred, data = calibration)
  calibration_slope <- coef(calib_model)[2]
  calibration_intercept <- coef(calib_model)[1]
  
  # Brier score
  brier_score <- mean((prob_pred - outcome)^2)
  
  diagnostics <- list(
    auc = auc_value,
    roc_curve = roc_obj,
    confusion_matrices = confusion_matrices,
    calibration = calibration,
    calibration_slope = calibration_slope,
    calibration_intercept = calibration_intercept,
    brier_score = brier_score,
    n_obs = length(outcome),
    n_events = sum(outcome),
    event_rate = mean(outcome)
  )
  
  message("  AUC: ", round(auc_value, 3))
  message("  Brier Score: ", round(brier_score, 4))
  message("  Calibration slope: ", round(calibration_slope, 3))
  
  return(diagnostics)
}

# Diagnostics pour modèle de régression
compute_regression_diagnostics <- function(model, data = NULL) {
  
  message("Calcul des diagnostics de régression...")
  
  # Résumé du modèle
  model_summary <- summary(model)
  
  # R²
  r_squared <- model_summary$r.squared
  adj_r_squared <- model_summary$adj.r.squared
  
  # RMSE
  residuals <- model$residuals
  rmse <- sqrt(mean(residuals^2))
  
  # MAE
  mae <- mean(abs(residuals))
  
  # Test de normalité des résidus
  shapiro_test <- NULL
  if (length(residuals) <= 5000) {
    shapiro_test <- shapiro.test(residuals)
  }
  
  # Statistiques des résidus
  residual_stats <- list(
    mean = mean(residuals),
    sd = sd(residuals),
    min = min(residuals),
    max = max(residuals),
    q25 = quantile(residuals, 0.25),
    q75 = quantile(residuals, 0.75)
  )
  
  diagnostics <- list(
    r_squared = r_squared,
    adj_r_squared = adj_r_squared,
    rmse = rmse,
    mae = mae,
    residual_stats = residual_stats,
    shapiro_test = shapiro_test,
    n_obs = nobs(model),
    n_params = length(coef(model)),
    aic = AIC(model),
    bic = BIC(model)
  )
  
  message("  R²: ", round(r_squared, 3))
  message("  RMSE: ", round(rmse, 3))
  message("  AIC: ", round(AIC(model), 1))
  
  return(diagnostics)
}

# Générer un rapport de diagnostics
generate_diagnostics_report <- function(diagnostics, model_name, output_dir = NULL) {
  
  if (is.null(output_dir)) {
    output_dir <- PATHS$models_declaration_diagnostics
  }
  
  ensure_dir(output_dir)
  
  # Sauvegarder les diagnostics
  diag_file <- file.path(output_dir, paste0(model_name, "_diagnostics.rds"))
  saveRDS(diagnostics, diag_file)
  
  # Créer un résumé textuel
  summary_file <- file.path(output_dir, paste0(model_name, "_summary.txt"))
  
  sink(summary_file)
  cat("=== DIAGNOSTICS DU MODÈLE ===\n")
  cat("Modèle: ", model_name, "\n")
  cat("Date: ", as.character(Sys.time()), "\n\n")
  
  if (!is.null(diagnostics$auc)) {
    cat("--- Métriques de classification ---\n")
    cat("AUC: ", round(diagnostics$auc, 4), "\n")
    cat("Brier Score: ", round(diagnostics$brier_score, 4), "\n")
    cat("Calibration slope: ", round(diagnostics$calibration_slope, 4), "\n")
    cat("N observations: ", diagnostics$n_obs, "\n")
    cat("N événements: ", diagnostics$n_events, "\n")
    cat("Taux d'événements: ", round(diagnostics$event_rate * 100, 2), "%\n")
  }
  
  if (!is.null(diagnostics$r_squared)) {
    cat("--- Métriques de régression ---\n")
    cat("R²: ", round(diagnostics$r_squared, 4), "\n")
    cat("R² ajusté: ", round(diagnostics$adj_r_squared, 4), "\n")
    cat("RMSE: ", round(diagnostics$rmse, 4), "\n")
    cat("MAE: ", round(diagnostics$mae, 4), "\n")
    cat("AIC: ", round(diagnostics$aic, 1), "\n")
    cat("BIC: ", round(diagnostics$bic, 1), "\n")
  }
  
  sink()
  
  message("Rapport de diagnostics: ", summary_file)
  
  return(list(
    diagnostics_file = diag_file,
    summary_file = summary_file
  ))
}

# Valider les diagnostics contre les règles
validate_model_diagnostics <- function(diagnostics) {
  
  rules <- VALIDATION_RULES$model_diagnostics
  issues <- list()
  
  # AUC minimum
  if (!is.null(diagnostics$auc) && !is.null(rules$auc_minimum)) {
    if (diagnostics$auc < 0.6) {
      issues$auc <- list(
        metric = "AUC",
        value = diagnostics$auc,
        threshold = 0.6,
        message = "AUC trop faible"
      )
    }
  }
  
  # Calibration slope
  if (!is.null(diagnostics$calibration_slope) && !is.null(rules$calibration_slope)) {
    if (diagnostics$calibration_slope < 0.8 || diagnostics$calibration_slope > 1.2) {
      issues$calibration <- list(
        metric = "calibration_slope",
        value = diagnostics$calibration_slope,
        threshold = c(0.8, 1.2),
        message = "Mauvaise calibration"
      )
    }
  }
  
  # N observations minimum
  if (!is.null(diagnostics$n_obs) && diagnostics$n_obs < 100) {
    issues$n_obs <- list(
      metric = "n_obs",
      value = diagnostics$n_obs,
      threshold = 100,
      message = "Trop peu d'observations"
    )
  }
  
  return(list(
    valid = length(issues) == 0,
    issues = issues
  ))
}

message("Module de diagnostics des modèles chargé")