# ============================================================
# STAT_DEFINITIONS.R
# Définitions des statistiques salariales à produire
# ============================================================

STATISTICS <- list(
  
  n_obs = list(
    type = "count",
    label = "Effectif observé",
    description = "Nombre d'observations"
  ),
  
  n_weighted = list(
    type = "count_weighted",
    label = "Effectif pondéré",
    description = "Somme des poids w_i",
    formula = "N_w = sum(w_i)"
  ),
  
  mean = list(
    type = "mean_weighted",
    label = "Moyenne pondérée",
    description = "Moyenne des salaires pondérée",
    formula = "Y_bar_w = sum(w_i * Y_i) / sum(w_i)",
    combine_imputations = TRUE
  ),
  
  variance = list(
    type = "variance_weighted",
    label = "Variance pondérée",
    description = "Variance des salaires pondérée",
    combine_imputations = TRUE
  ),
  
  min = list(
    type = "min",
    label = "Minimum",
    description = "Valeur minimale observée"
  ),
  
  max = list(
    type = "max",
    label = "Maximum",
    description = "Valeur maximale observée"
  ),
  
  q1 = list(
    type = "quantile_weighted",
    p = 0.25,
    label = "Q1 (25%)",
    description = "Premier quartile pondéré",
    combine_imputations = TRUE
  ),
  
  median = list(
    type = "quantile_weighted",
    p = 0.50,
    label = "Médiane",
    description = "Médiane pondérée",
    combine_imputations = TRUE
  ),
  
  q3 = list(
    type = "quantile_weighted",
    p = 0.75,
    label = "Q3 (75%)",
    description = "Troisième quartile pondéré",
    combine_imputations = TRUE
  ),
  
  p10 = list(
    type = "quantile_weighted",
    p = 0.10,
    label = "P10",
    description = "10ème percentile pondéré",
    combine_imputations = TRUE
  ),
  
  p90 = list(
    type = "quantile_weighted",
    p = 0.90,
    label = "P90",
    description = "90ème percentile pondéré",
    combine_imputations = TRUE
  ),
  
  gini = list(
    type = "gini_weighted",
    label = "Coefficient de Gini",
    description = "Mesure d'inégalité pondérée",
    combine_imputations = TRUE
  )
)

get_statistics_to_compute <- function() {
  return(names(STATISTICS))
}

get_statistics_requiring_imputation_combine <- function() {
  stats <- names(STATISTICS)[sapply(STATISTICS, function(x) {
    isTRUE(x$combine_imputations)
  })]
  return(stats)
}

message("Définitions statistiques chargées depuis stat_definitions.R")