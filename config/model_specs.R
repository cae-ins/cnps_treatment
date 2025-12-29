# ============================================================
# MODEL_SPECS.R
# Spécifications des modèles statistiques
# ============================================================

MODEL_SPECS <- list(
  
  # Modèle de déclaration entreprise-mois
  declaration_firm = list(
    name = "declaration_firm",
    type = "logit",
    description = "Modèle de probabilité de déclaration entreprise-mois",
    outcome = "D_jt",
    outcome_label = "Déclaration (1=oui, 0=non)",
    
    # Variables explicatives
    covariates = list(
      firm_characteristics = c(
        "SECTOR_CODE",
        "CLASSE_EFFECTIF_REDUITE",
        "AGE_ENTREPRISE"
      ),
      history = c(
        "D_jt_lag1",
        "D_jt_lag2",
        "N_DECLARATIONS_PASSEES",
        "PCT_DECLARATIONS_PASSEES"
      ),
      time = c(
        "MOIS",
        "ANNEE"
      )
    ),
    
    # Options d'estimation
    estimation = list(
      method = "glm",
      family = "binomial",
      link = "logit"
    ),
    
    # Options IPW
    ipw = list(
      stabilize = TRUE,
      trim_quantiles = c(0.01, 0.99),
      min_probability = 0.01
    )
  ),
  
  # Modèle d'imputation du salaire moyen entreprise
  imputation_firm_salary = list(
    name = "imputation_firm_salary",
    type = "linear",
    description = "Modèle d'imputation log(salaire moyen) entreprise-mois",
    outcome = "log_Y_bar_jt",
    
    covariates = list(
      firm_characteristics = c(
        "SECTOR_CODE",
        "CLASSE_EFFECTIF_REDUITE",
        "AGE_ENTREPRISE"
      ),
      history = c(
        "log_Y_bar_jt_lag1",
        "log_Y_bar_jt_lag2",
        "EFFECTIF_MOYEN_PASSE"
      ),
      time = c(
        "factor(MOIS)",
        "ANNEE"
      )
    ),
    
    estimation = list(
      method = "lm",
      n_imputations = 5,
      residual_bootstrap = TRUE
    )
  ),
  
  # Modèle de déclaration individuelle conditionnelle
  declaration_individual = list(
    name = "declaration_individual",
    type = "logit",
    description = "Modèle de probabilité d'observation salarié conditionnel",
    outcome = "S_ijt",
    outcome_label = "Salarié observé (1=oui, 0=non)",
    
    covariates = list(
      individual = c(
        "SEXE",
        "ANCIENNETE_TRANCHE",
        "CSP_CODE"
      ),
      firm = c(
        "SECTOR_CODE",
        "CLASSE_EFFECTIF_REDUITE"
      ),
      time = c(
        "MOIS"
      )
    ),
    
    estimation = list(
      method = "glm",
      family = "binomial",
      link = "logit"
    ),
    
    ipw = list(
      stabilize = TRUE,
      trim_quantiles = c(0.01, 0.99)
    )
  ),
  
  # Modèle d'imputation salaire individuel
  imputation_individual_salary = list(
    name = "imputation_individual_salary",
    type = "linear_mixed",
    description = "Modèle d'imputation log(salaire) individuel",
    outcome = "log_Y_ijt",
    
    covariates = list(
      individual = c(
        "SEXE",
        "ANCIENNETE_TRANCHE",
        "CSP_CODE",
        "PROF_CODE"
      ),
      firm = c(
        "SECTOR_CODE",
        "CLASSE_EFFECTIF_REDUITE"
      )
    ),
    
    random_effects = list(
      firm = "(1|NUMERO_EMPLOYEUR)",
      time = "(1|PERIOD)"
    ),
    
    estimation = list(
      method = "lmer",
      n_imputations = 5
    )
  )
)

get_model_spec <- function(model_name) {
  if (!model_name %in% names(MODEL_SPECS)) {
    stop("Modèle inconnu: ", model_name)
  }
  return(MODEL_SPECS[[model_name]])
}

get_model_covariates <- function(model_name) {
  spec <- get_model_spec(model_name)
  return(unlist(spec$covariates, use.names = FALSE))
}

message("Spécifications des modèles chargées depuis model_specs.R")