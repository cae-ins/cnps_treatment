# ============================================================
# ESTIMATORS.R
# Fonctions d'estimation statistique
# ============================================================

# Effectif pondéré
compute_n_weighted <- function(weights) {
  sum(weights, na.rm = TRUE)
}

# Moyenne pondérée
compute_mean_weighted <- function(y, weights) {
  sum(weights * y, na.rm = TRUE) / sum(weights, na.rm = TRUE)
}

# Variance pondérée
compute_variance_weighted <- function(y, weights) {
  mu <- compute_mean_weighted(y, weights)
  sum(weights * (y - mu)^2, na.rm = TRUE) / sum(weights, na.rm = TRUE)
}

# Quantile pondéré
compute_quantile_weighted <- function(y, weights, p) {
  if (length(y) == 0 || all(is.na(y))) return(NA_real_)
  
  ord <- order(y)
  y_sorted <- y[ord]
  w_sorted <- weights[ord]
  
  cum_w <- cumsum(w_sorted) / sum(w_sorted)
  
  idx <- which(cum_w >= p)[1]
  if (is.na(idx)) return(y_sorted[length(y_sorted)])
  
  return(y_sorted[idx])
}

# Coefficient de Gini pondéré
compute_gini_weighted <- function(y, weights) {
  if (length(y) < 2) return(NA_real_)
  
  ord <- order(y)
  y_sorted <- y[ord]
  w_sorted <- weights[ord]
  
  n <- length(y)
  cum_w <- cumsum(w_sorted)
  total_w <- sum(w_sorted)
  
  cum_yw <- cumsum(w_sorted * y_sorted)
  total_yw <- sum(w_sorted * y_sorted)
  
  B <- sum(w_sorted * cum_yw) / (total_w * total_yw)
  
  gini <- 1 - 2 * B
  return(gini)
}

# Combinaison de Rubin pour imputations multiples
rubin_combine <- function(estimates, variances = NULL) {
  M <- length(estimates)
  
  Q_bar <- mean(estimates, na.rm = TRUE)
  
  if (!is.null(variances)) {
    U_bar <- mean(variances, na.rm = TRUE)
    B <- var(estimates, na.rm = TRUE)
    T_total <- U_bar + (1 + 1/M) * B
    
    return(list(
      estimate = Q_bar,
      variance = T_total,
      se = sqrt(T_total),
      between_var = B,
      within_var = U_bar
    ))
  }
  
  return(list(
    estimate = Q_bar,
    variance = var(estimates, na.rm = TRUE),
    se = sd(estimates, na.rm = TRUE)
  ))
}

# Calcul du poids IPW stabilisé et tronqué
compute_ipw_weight <- function(prob, stabilize = TRUE, 
                                trim_quantiles = c(0.01, 0.99),
                                min_prob = 0.01) {
  prob <- pmax(prob, min_prob)
  
  w <- 1 / prob
  
  if (stabilize) {
    marginal_prob <- mean(prob, na.rm = TRUE)
    w <- marginal_prob / prob
  }
  
  bounds <- quantile(w, probs = trim_quantiles, na.rm = TRUE)
  w <- pmax(pmin(w, bounds[2]), bounds[1])
  
  return(w)
}

# Calcul de toutes les statistiques pour un groupe
compute_all_statistics <- function(y, weights, stats_to_compute = NULL) {
  
  if (is.null(stats_to_compute)) {
    stats_to_compute <- names(STATISTICS)
  }
  
  results <- list()
  
  for (stat_name in stats_to_compute) {
    spec <- STATISTICS[[stat_name]]
    
    value <- switch(spec$type,
      "count" = length(y[!is.na(y)]),
      "count_weighted" = compute_n_weighted(weights),
      "mean_weighted" = compute_mean_weighted(y, weights),
      "variance_weighted" = compute_variance_weighted(y, weights),
      "min" = min(y, na.rm = TRUE),
      "max" = max(y, na.rm = TRUE),
      "quantile_weighted" = compute_quantile_weighted(y, weights, spec$p),
      "gini_weighted" = compute_gini_weighted(y, weights),
      NA_real_
    )
    
    results[[stat_name]] <- value
  }
  
  return(results)
}

message("Estimateurs chargés depuis estimators.R")