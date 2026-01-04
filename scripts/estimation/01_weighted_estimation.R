# ============================================================
# 01_WEIGHTED_ESTIMATION.R
# Estimation des statistiques salariales pondérées
# ============================================================

library(dplyr)
library(tidyr)

# Calculer les poids finaux
compute_final_weights <- function(df_individual) {
  
  message("Calcul des poids finaux...")
  
  df <- df_individual %>%
    mutate(
      w_jt = ifelse(is.na(w_jt), 1, w_jt),
      w_ijt = ifelse(is.na(w_ijt), 1, w_ijt),
      w_final = w_jt * w_ijt
    )
  
  # Normaliser les poids
  df <- df %>%
    group_by(ANNEE, MOIS) %>%
    mutate(
      w_normalized = w_final / sum(w_final) * n()
    ) %>%
    ungroup()
  
  message("  Poids moyen: ", round(mean(df$w_final, na.rm = TRUE), 3))
  message("  Coefficient de variation: ", 
          round(sd(df$w_final, na.rm = TRUE) / mean(df$w_final, na.rm = TRUE), 3))
  
  return(df)
}

# Calculer les statistiques pour un groupe
compute_group_statistics <- function(df, group_vars = NULL, 
                                      salary_var = "SALAIRE_BRUT_MENS",
                                      weight_var = "w_final") {
  
  if (is.null(group_vars)) {
    df <- df %>% mutate(.group = "Total")
    group_vars <- ".group"
  }
  
  stats <- df %>%
    group_by(across(all_of(c(group_vars, "ANNEE", "MOIS")))) %>%
    summarise(
      n_obs = n(),
      n_weighted = compute_n_weighted(.data[[weight_var]]),
      mean = compute_mean_weighted(.data[[salary_var]], .data[[weight_var]]),
      variance = compute_variance_weighted(.data[[salary_var]], .data[[weight_var]]),
      min = min(.data[[salary_var]], na.rm = TRUE),
      max = max(.data[[salary_var]], na.rm = TRUE),
      q1 = compute_quantile_weighted(.data[[salary_var]], .data[[weight_var]], 0.25),
      median = compute_quantile_weighted(.data[[salary_var]], .data[[weight_var]], 0.50),
      q3 = compute_quantile_weighted(.data[[salary_var]], .data[[weight_var]], 0.75),
      p10 = compute_quantile_weighted(.data[[salary_var]], .data[[weight_var]], 0.10),
      p90 = compute_quantile_weighted(.data[[salary_var]], .data[[weight_var]], 0.90),
      gini = compute_gini_weighted(.data[[salary_var]], .data[[weight_var]]),
      .groups = "drop"
    )
  
  return(stats)
}

# Combiner les résultats des imputations multiples
combine_imputation_results <- function(results_by_imputation) {
  
  message("Combinaison des résultats d'imputation (règles de Rubin)...")
  
  n_imp <- length(results_by_imputation)
  
  # Identifier les colonnes de statistiques
  first_result <- results_by_imputation[[1]]
  group_cols <- intersect(names(first_result), c("ANNEE", "MOIS", get_enabled_dimensions()))
  stat_cols <- setdiff(names(first_result), group_cols)
  stat_cols <- intersect(stat_cols, get_statistics_to_compute())
  
  # Combiner par groupe
  combined <- first_result %>%
    select(all_of(group_cols))
  
  for (stat in stat_cols) {
    # Extraire les estimations pour cette statistique
    estimates <- sapply(results_by_imputation, function(r) r[[stat]])
    
    if (is.matrix(estimates)) {
      combined_stat <- apply(estimates, 1, function(x) {
        rubin_combine(x)$estimate
      })
      combined_se <- apply(estimates, 1, function(x) {
        rubin_combine(x)$se
      })
    } else {
      combined_stat <- rubin_combine(estimates)$estimate
      combined_se <- rubin_combine(estimates)$se
    }
    
    combined[[stat]] <- combined_stat
    combined[[paste0(stat, "_se")]] <- combined_se
  }
  
  return(combined)
}

# Calculer toutes les statistiques par dimension
compute_all_dimensions <- function(df, imputation_id = NULL) {
  
  message("\nCalcul des statistiques par dimension...")
  
  results <- list()
  
  # Global
  if (STAT_DIMENSIONS$global$enabled) {
    message("  Dimension: Global")
    results$global <- compute_group_statistics(df, group_vars = NULL)
  }
  
  # Par dimension
  for (dim_name in names(STAT_DIMENSIONS)) {
    if (dim_name == "global") next
    
    dim_config <- STAT_DIMENSIONS[[dim_name]]
    if (!dim_config$enabled) next
    
    var_name <- dim_config$variable
    if (!var_name %in% names(df)) {
      message("  Dimension ", dim_name, ": variable ", var_name, " absente")
      next
    }
    
    message("  Dimension: ", dim_config$label)
    
    stats <- compute_group_statistics(df, group_vars = var_name)
    
    # Appliquer le seuil de cellule minimum
    stats <- stats %>%
      filter(n_weighted >= dim_config$min_cell_size)
    
    results[[dim_name]] <- stats
  }
  
  return(results)
}

# Fonction principale d'estimation
run_weighted_estimation <- function(df_individual, imputations = NULL) {
  
  message("\n", strrep("=", 60))
  message("ESTIMATION DES STATISTIQUES SALARIALES PONDÉRÉES")
  message(strrep("=", 60))
  
  # Calculer les poids finaux
  df <- compute_final_weights(df_individual)
  
  # Si pas d'imputations multiples
  if (is.null(imputations) || length(imputations) == 0) {
    results <- compute_all_dimensions(df)
    return(list(
      results = results,
      combined = FALSE
    ))
  }
  
  # Avec imputations multiples
  results_by_imp <- list()
  
  for (m in seq_along(imputations)) {
    message("\n  Imputation ", m, "/", length(imputations))
    
    # Fusionner les données imputées
    df_m <- df %>%
      left_join(
        imputations[[m]] %>% 
          select(NUMERO_EMPLOYEUR, ANNEE, MOIS, Y_bar_jt_imputed),
        by = c("NUMERO_EMPLOYEUR", "ANNEE", "MOIS")
      ) %>%
      mutate(
        SALAIRE_BRUT_MENS = coalesce(SALAIRE_BRUT_MENS, Y_bar_jt_imputed)
      )
    
    results_by_imp[[m]] <- compute_all_dimensions(df_m)
  }
  
  # Combiner les résultats
  combined_results <- list()
  
  for (dim_name in names(results_by_imp[[1]])) {
    dim_results <- lapply(results_by_imp, function(r) r[[dim_name]])
    combined_results[[dim_name]] <- combine_imputation_results(dim_results)
  }
  
  return(list(
    results = combined_results,
    results_by_imputation = results_by_imp,
    combined = TRUE
  ))
}

message("Module d'estimation pondérée chargé")