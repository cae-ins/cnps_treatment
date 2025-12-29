# ============================================================
# VALIDATION_RULES.R
# Règles de validation des données et des modèles
# ============================================================

VALIDATION_RULES <- list(
  
  # Règles sur les données individuelles
  individual_data = list(
    salaire_brut_positive = list(
      variable = "SALAIRE_BRUT",
      rule = "x > 0",
      message = "Salaire brut doit être positif",
      action = "exclude"
    ),
    salaire_brut_min = list(
      variable = "SALAIRE_BRUT_MENS",
      rule = "x >= 75000",
      message = "Salaire mensuel sous le SMIG",
      action = "flag"
    ),
    duree_travaillee_valid = list(
      variable = "DUREE_TRAVAILLEE",
      rule = "x >= 0 & x <= 12",
      message = "Durée travaillée invalide",
      action = "flag"
    ),
    id_indiv_not_missing = list(
      variable = "ID_INDIV",
      rule = "!is.na(x) & x != ''",
      message = "ID individu manquant",
      action = "exclude"
    )
  ),
  
  # Règles sur les données entreprise-mois
  firm_data = list(
    numero_employeur_valid = list(
      variable = "NUMERO_EMPLOYEUR",
      rule = "!is.na(x)",
      message = "Numéro employeur manquant",
      action = "exclude"
    ),
    effectif_coherent = list(
      variables = c("EFFECTIF_DECLARE", "EFFECTIF_OBSERVE"),
      rule = "EFFECTIF_OBSERVE <= EFFECTIF_DECLARE * 1.1",
      message = "Effectif observé incohérent avec déclaré",
      action = "flag"
    )
  ),
  
  # Règles sur les modèles
  model_diagnostics = list(
    auc_minimum = list(
      metric = "AUC",
      rule = "x >= 0.6",
      message = "AUC trop faible, modèle discriminant mal",
      action = "warn"
    ),
    calibration_slope = list(
      metric = "calibration_slope",
      rule = "x >= 0.8 & x <= 1.2",
      message = "Mauvaise calibration du modèle",
      action = "warn"
    ),
    n_obs_minimum = list(
      metric = "n_obs",
      rule = "x >= 100",
      message = "Trop peu d'observations pour le modèle",
      action = "error"
    )
  ),
  
  # Règles sur les estimations
  estimation_output = list(
    cell_size_minimum = list(
      metric = "n_weighted",
      rule = "x >= 30",
      message = "Effectif pondéré insuffisant",
      action = "suppress"
    ),
    variance_positive = list(
      metric = "variance",
      rule = "x >= 0",
      message = "Variance négative",
      action = "error"
    ),
    mean_in_range = list(
      metric = "mean",
      rule = "x >= 75000 & x <= 50000000",
      message = "Moyenne hors plage plausible",
      action = "flag"
    )
  )
)

validate_data <- function(data, rules_category) {
  rules <- VALIDATION_RULES[[rules_category]]
  if (is.null(rules)) return(list(valid = TRUE, issues = NULL))
  
  issues <- list()
  
  for (rule_name in names(rules)) {
    rule <- rules[[rule_name]]
    
    if (!is.null(rule$variable) && rule$variable %in% names(data)) {
      x <- data[[rule$variable]]
      valid <- eval(parse(text = rule$rule))
      n_invalid <- sum(!valid, na.rm = TRUE)
      
      if (n_invalid > 0) {
        issues[[rule_name]] <- list(
          rule = rule_name,
          variable = rule$variable,
          n_invalid = n_invalid,
          pct_invalid = round(n_invalid / length(x) * 100, 2),
          message = rule$message,
          action = rule$action
        )
      }
    }
  }
  
  return(list(
    valid = length(issues) == 0,
    issues = issues
  ))
}

apply_validation_actions <- function(data, validation_result) {
  if (validation_result$valid) return(data)
  
  for (issue in validation_result$issues) {
    if (issue$action == "exclude") {
      x <- data[[issue$variable]]
      rule_expr <- VALIDATION_RULES$individual_data[[issue$rule]]$rule
      valid <- eval(parse(text = rule_expr))
      data <- data[valid, ]
    }
  }
  
  return(data)
}

message("Règles de validation chargées depuis validation_rules.R")