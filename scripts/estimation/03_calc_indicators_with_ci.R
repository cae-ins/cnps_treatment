# ============================================================
# 03_CALC_INDICATORS_WITH_CI.R
# Calcul des indicateurs avec intervalles de confiance et SE
# ============================================================

library(dplyr)
library(tidyr)
library(openxlsx)
library(haven)

# ------------------------------------------------------------
# FONCTIONS DE CALCUL AVEC VARIANCE
# ------------------------------------------------------------

# Moyenne pondérée avec variance
compute_mean_weighted_with_var <- function(y, weights) {
  n <- length(y)
  sum_w <- sum(weights, na.rm = TRUE)
  
  # Moyenne
  mean_val <- sum(weights * y, na.rm = TRUE) / sum_w
  

  # Variance de la moyenne pondérée (estimateur de Horvitz-Thompson)
  # Var(mean) = (1/sum_w^2) * sum(w_i^2 * (y_i - mean)^2)
  var_mean <- sum(weights^2 * (y - mean_val)^2, na.rm = TRUE) / (sum_w^2)
  
  # Correction pour échantillon fini
  # Facteur de correction: n / (n - 1)
  if (n > 1) {
    var_mean <- var_mean * n / (n - 1)
  }
  
  return(list(
    estimate = mean_val,
    variance = var_mean,
    se = sqrt(var_mean),
    n = n,
    sum_weights = sum_w
  ))
}

# Quantile pondéré avec variance (méthode bootstrap)
compute_quantile_weighted_with_var <- function(y, weights, p, n_boot = 200) {
  
  n <- length(y)
  if (n < 10) {
    # Pas assez d'observations pour bootstrap
    q_val <- compute_quantile_weighted(y, weights, p)
    return(list(
      estimate = q_val,
      variance = NA_real_,
      se = NA_real_,
      n = n
    ))
  }
  
  # Estimation ponctuelle
  q_val <- compute_quantile_weighted(y, weights, p)
  
  # Bootstrap pour la variance
  boot_estimates <- numeric(n_boot)
  
  for (b in 1:n_boot) {
    # Rééchantillonnage avec remplacement
    idx <- sample(1:n, n, replace = TRUE)
    y_boot <- y[idx]
    w_boot <- weights[idx]
    
    boot_estimates[b] <- compute_quantile_weighted(y_boot, w_boot, p)
  }
  
  # Variance bootstrap
  var_q <- var(boot_estimates, na.rm = TRUE)
  
  return(list(
    estimate = q_val,
    variance = var_q,
    se = sqrt(var_q),
    n = n,
    boot_estimates = boot_estimates
  ))
}

# Min/Max avec "variance" (range des valeurs proches)
compute_minmax_with_range <- function(y, weights, type = "min", k = 5) {
  
  n <- length(y)
  
  if (type == "min") {
    val <- min(y, na.rm = TRUE)
    # Prendre les k plus petites valeurs pour estimer l'incertitude
    sorted_y <- sort(y)[1:min(k, n)]
  } else {
    val <- max(y, na.rm = TRUE)
    # Prendre les k plus grandes valeurs
    sorted_y <- sort(y, decreasing = TRUE)[1:min(k, n)]
  }
  
  # "Variance" basée sur l'écart entre les valeurs extrêmes
  range_val <- diff(range(sorted_y))
  
  return(list(
    estimate = val,
    variance = NA_real_,  # Pas de variance formelle pour min/max
    se = NA_real_,
    range = range_val,
    n = n
  ))
}

# ------------------------------------------------------------
# CALCUL COMPLET AVEC IC
# ------------------------------------------------------------

compute_statistics_with_ci <- function(y, weights, alpha = 0.05, n_boot = 200) {
  
  n <- length(y[!is.na(y)])
  
  if (n == 0) {
    return(NULL)
  }
  
  # Filtre NA
  valid <- !is.na(y) & !is.na(weights)
  y <- y[valid]
  weights <- weights[valid]
  
  # Z pour l'intervalle de confiance
  z <- qnorm(1 - alpha/2)
  
  results <- list()
  
  # Effectif
  results$n_obs <- list(
    estimate = n,
    se = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_
  )
  
  # Effectif pondéré
  n_w <- sum(weights)
  results$n_weighted <- list(
    estimate = n_w,
    se = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_
  )
  
  # Moyenne
  mean_result <- compute_mean_weighted_with_var(y, weights)
  results$mean <- list(
    estimate = mean_result$estimate,
    se = mean_result$se,
    ci_lower = mean_result$estimate - z * mean_result$se,
    ci_upper = mean_result$estimate + z * mean_result$se
  )
  
  # Min
  min_result <- compute_minmax_with_range(y, weights, "min")
  results$min <- list(
    estimate = min_result$estimate,
    se = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_
  )
  
  # Max
 max_result <- compute_minmax_with_range(y, weights, "max")
  results$max <- list(
    estimate = max_result$estimate,
    se = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_
  )
  
  # Q1
  q1_result <- compute_quantile_weighted_with_var(y, weights, 0.25, n_boot)
  results$q1 <- list(
    estimate = q1_result$estimate,
    se = q1_result$se,
    ci_lower = if (!is.na(q1_result$se)) q1_result$estimate - z * q1_result$se else NA_real_,
    ci_upper = if (!is.na(q1_result$se)) q1_result$estimate + z * q1_result$se else NA_real_
  )
  
  # Médiane
  med_result <- compute_quantile_weighted_with_var(y, weights, 0.50, n_boot)
  results$median <- list(
    estimate = med_result$estimate,
    se = med_result$se,
    ci_lower = if (!is.na(med_result$se)) med_result$estimate - z * med_result$se else NA_real_,
    ci_upper = if (!is.na(med_result$se)) med_result$estimate + z * med_result$se else NA_real_
  )
  
  # Q3
  q3_result <- compute_quantile_weighted_with_var(y, weights, 0.75, n_boot)
  results$q3 <- list(
    estimate = q3_result$estimate,
    se = q3_result$se,
    ci_lower = if (!is.na(q3_result$se)) q3_result$estimate - z * q3_result$se else NA_real_,
    ci_upper = if (!is.na(q3_result$se)) q3_result$estimate + z * q3_result$se else NA_real_
  )
  
  # P10
  p10_result <- compute_quantile_weighted_with_var(y, weights, 0.10, n_boot)
  results$p10 <- list(
    estimate = p10_result$estimate,
    se = p10_result$se,
    ci_lower = if (!is.na(p10_result$se)) p10_result$estimate - z * p10_result$se else NA_real_,
    ci_upper = if (!is.na(p10_result$se)) p10_result$estimate + z * p10_result$se else NA_real_
  )
  
  # P90
  p90_result <- compute_quantile_weighted_with_var(y, weights, 0.90, n_boot)
  results$p90 <- list(
    estimate = p90_result$estimate,
    se = p90_result$se,
    ci_lower = if (!is.na(p90_result$se)) p90_result$estimate - z * p90_result$se else NA_real_,
    ci_upper = if (!is.na(p90_result$se)) p90_result$estimate + z * p90_result$se else NA_real_
  )
  
  return(results)
}

# ------------------------------------------------------------
# COMBINAISON RUBIN ÉTENDUE
# ------------------------------------------------------------

rubin_combine_extended <- function(estimates_list) {
  # estimates_list : liste de M listes, chacune contenant estimate, se, etc.
  
  M <- length(estimates_list)
  
  # Extraire les estimations
  Q_m <- sapply(estimates_list, function(x) x$estimate)
  
  # Moyenne des estimations
  Q_bar <- mean(Q_m, na.rm = TRUE)
  
  # Variance inter-imputation
  B <- var(Q_m, na.rm = TRUE)
  
  # Variance intra-imputation (moyenne des variances)
  U_m <- sapply(estimates_list, function(x) {
    if (!is.null(x$se) && !is.na(x$se)) x$se^2 else NA_real_
  })
  U_bar <- mean(U_m, na.rm = TRUE)
  
  # Variance totale (règle de Rubin)
  if (!is.na(U_bar) && !is.na(B)) {
    T_var <- U_bar + (1 + 1/M) * B
  } else if (!is.na(B)) {
    T_var <- (1 + 1/M) * B
  } else {
    T_var <- NA_real_
  }
  
  # Erreur standard
  T_se <- if (!is.na(T_var)) sqrt(T_var) else NA_real_
  
  # Degrés de liberté (formule de Barnard-Rubin)
  if (!is.na(U_bar) && !is.na(B) && B > 0) {
    lambda <- (1 + 1/M) * B / T_var
    df_old <- (M - 1) / lambda^2
    # Ajustement pour petits échantillons si n disponible
    df <- df_old
  } else {
    df <- Inf
  }
  
  # Intervalle de confiance
  alpha <- 0.05
  if (is.finite(df) && df > 0) {
    t_val <- qt(1 - alpha/2, df)
  } else {
    t_val <- qnorm(1 - alpha/2)
  }
  
  ci_lower <- Q_bar - t_val * T_se
  ci_upper <- Q_bar + t_val * T_se
  
  return(list(
    estimate = Q_bar,
    se = T_se,
    variance = T_var,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    between_var = B,
    within_var = U_bar,
    df = df,
    m = M
  ))
}

# ------------------------------------------------------------
# CALCUL PAR GROUPE AVEC IC
# ------------------------------------------------------------

compute_group_statistics_with_ci <- function(df, group_vars = NULL,
                                              salary_var = "SALAIRE_BRUT_MENS",
                                              weight_var = "w_final",
                                              alpha = 0.05,
                                              n_boot = 200) {
  
  if (is.null(group_vars)) {
    df <- df %>% mutate(.group = "Total")
    group_vars <- ".group"
  }
  
  # Grouper et calculer
  results <- df %>%
    filter(!is.na(.data[[salary_var]]), !is.na(.data[[weight_var]])) %>%
    group_by(across(all_of(c(group_vars, "ANNEE", "MOIS")))) %>%
    group_modify(~ {
      y <- .x[[salary_var]]
      w <- .x[[weight_var]]
      
      stats <- compute_statistics_with_ci(y, w, alpha, n_boot)
      
      # Convertir en tibble
      tibble(
        n_obs = stats$n_obs$estimate,
        n_weighted = stats$n_weighted$estimate,
        
        mean = stats$mean$estimate,
        mean_se = stats$mean$se,
        mean_ci_lower = stats$mean$ci_lower,
        mean_ci_upper = stats$mean$ci_upper,
        
        min = stats$min$estimate,
        max = stats$max$estimate,
        
        q1 = stats$q1$estimate,
        q1_se = stats$q1$se,
        q1_ci_lower = stats$q1$ci_lower,
        q1_ci_upper = stats$q1$ci_upper,
        
        median = stats$median$estimate,
        median_se = stats$median$se,
        median_ci_lower = stats$median$ci_lower,
        median_ci_upper = stats$median$ci_upper,
        
        q3 = stats$q3$estimate,
        q3_se = stats$q3$se,
        q3_ci_lower = stats$q3$ci_lower,
        q3_ci_upper = stats$q3$ci_upper,
        
        p10 = stats$p10$estimate,
        p10_se = stats$p10$se,
        p10_ci_lower = stats$p10$ci_lower,
        p10_ci_upper = stats$p10$ci_upper,
        
        p90 = stats$p90$estimate,
        p90_se = stats$p90$se,
        p90_ci_lower = stats$p90$ci_lower,
        p90_ci_upper = stats$p90$ci_upper
      )
    }) %>%
    ungroup()
  
  return(results)
}

# ------------------------------------------------------------
# COMBINAISON DES IMPUTATIONS AVEC IC
# ------------------------------------------------------------

combine_imputation_results_with_ci <- function(results_by_imputation) {
  
  message("Combinaison des résultats d'imputation avec IC (règles de Rubin)...")
  
  M <- length(results_by_imputation)
  first_result <- results_by_imputation[[1]]
  
  # Colonnes de groupement
  group_cols <- c("ANNEE", "MOIS")
  for (dim_name in names(STAT_DIMENSIONS)) {
    if (dim_name == "global") next
    var_name <- STAT_DIMENSIONS[[dim_name]]$variable
    if (var_name %in% names(first_result)) {
      group_cols <- c(group_cols, var_name)
    }
  }
  group_cols <- intersect(group_cols, names(first_result))
  
  # Statistiques à combiner
  stats_to_combine <- c("mean", "q1", "median", "q3", "p10", "p90")
  
  # Créer le résultat combiné
  combined <- first_result %>% select(all_of(group_cols))
  
  # Colonnes simples (pas de combinaison)
  combined$n_obs <- first_result$n_obs
  combined$n_weighted <- first_result$n_weighted
  combined$min <- first_result$min
  combined$max <- first_result$max
  
  # Combiner chaque statistique
  for (stat in stats_to_combine) {
    
    # Extraire les résultats de chaque imputation
    stat_estimates <- lapply(1:M, function(m) {
      list(
        estimate = results_by_imputation[[m]][[stat]],
        se = results_by_imputation[[m]][[paste0(stat, "_se")]]
      )
    })
    
    # Appliquer la combinaison ligne par ligne
    n_rows <- nrow(first_result)
    
    combined_stat <- numeric(n_rows)
    combined_se <- numeric(n_rows)
    combined_ci_lower <- numeric(n_rows)
    combined_ci_upper <- numeric(n_rows)
    
    for (i in 1:n_rows) {
      row_estimates <- lapply(stat_estimates, function(x) {
        list(
          estimate = x$estimate[i],
          se = x$se[i]
        )
      })
      
      rubin_result <- rubin_combine_extended(row_estimates)
      
      combined_stat[i] <- rubin_result$estimate
      combined_se[i] <- rubin_result$se
      combined_ci_lower[i] <- rubin_result$ci_lower
      combined_ci_upper[i] <- rubin_result$ci_upper
    }
    
    combined[[stat]] <- combined_stat
    combined[[paste0(stat, "_se")]] <- combined_se
    combined[[paste0(stat, "_ci_lower")]] <- combined_ci_lower
    combined[[paste0(stat, "_ci_upper")]] <- combined_ci_upper
  }
  
  return(combined)
}

# ------------------------------------------------------------
# FONCTION PRINCIPALE
# ------------------------------------------------------------

calculate_indicators_with_ci <- function(estimation_results = NULL,
                                          input_file = NULL,
                                          output_folder = NULL,
                                          alpha = 0.05,
                                          n_boot = 200) {
  
  message("\n", strrep("=", 60))
  message("CALCUL DES INDICATEURS AVEC INTERVALLES DE CONFIANCE")
  message(strrep("=", 60))
  
  if (is.null(output_folder)) {
    output_folder <- PATHS$indicators
  }
  
  ensure_dir(output_folder)
  
  # Déterminer la source des données
  if (!is.null(estimation_results)) {
    # Utiliser les résultats d'estimation existants
    use_estimation_results <- TRUE
    
    if (estimation_results$combined) {
      results_with_ci <- estimation_results$results
    } else {
      # Recalculer avec IC si pas encore fait
      # ...
      results_with_ci <- estimation_results$results
    }
    
  } else if (!is.null(input_file)) {
    # Charger les données et calculer
    df <- read_dta(input_file)
    use_estimation_results <- FALSE
  } else {
    # Trouver le fichier le plus récent
    final_files <- list.files(PATHS$cleaned_analytical, 
                              pattern = "^base_analytique_.*\\.dta$",
                              full.names = TRUE)
    if (length(final_files) == 0) {
      final_files <- list.files(PATHS$cleaned_final, 
                                pattern = "^data_cnps_final_.*\\.dta$",
                                full.names = TRUE)
    }
    if (length(final_files) == 0) {
      stop("Aucun fichier de données trouvé")
    }
    
    input_file <- final_files[which.max(file.mtime(final_files))]
    df <- read_dta(input_file)
    use_estimation_results <- FALSE
  }
  
  # Si on doit calculer depuis les données brutes
  if (!use_estimation_results) {
    message("Calcul des statistiques avec IC depuis les données...")
    
    # S'assurer que w_final existe
    if (!"w_final" %in% names(df)) {
      df$w_final <- 1
    }
    
    results_with_ci <- list()
    
    # Global
    if (STAT_DIMENSIONS$global$enabled) {
      message("  Dimension: Global")
      results_with_ci$global <- compute_group_statistics_with_ci(
        df, group_vars = NULL, alpha = alpha, n_boot = n_boot
      )
    }
    
    # Par dimension
    for (dim_name in names(STAT_DIMENSIONS)) {
      if (dim_name == "global") next
      
      dim_config <- STAT_DIMENSIONS[[dim_name]]
      if (!dim_config$enabled) next
      
      var_name <- dim_config$variable
      if (!var_name %in% names(df)) {
        message("  Dimension ", dim_name, ": variable absente")
        next
      }
      
      message("  Dimension: ", dim_config$label)
      
      stats <- compute_group_statistics_with_ci(
        df, group_vars = var_name, alpha = alpha, n_boot = n_boot
      )
      
      # Filtrer les petites cellules
      stats <- stats %>%
        filter(n_weighted >= dim_config$min_cell_size)
      
      results_with_ci[[dim_name]] <- stats
    }
  }
  
  # Extraire la période pour le nom du fichier
  if (exists("df")) {
    periods <- df %>% distinct(ANNEE, MOIS) %>% arrange(ANNEE, MOIS)
    min_p <- periods %>% slice(1)
    max_p <- periods %>% slice(n())
    period_str <- sprintf("%02d_%04d-%02d_%04d",
                          min_p$MOIS, min_p$ANNEE,
                          max_p$MOIS, max_p$ANNEE)
  } else {
    period_str <- "unknown"
  }
  
  # Créer le workbook Excel
  wb <- createWorkbook()
  
  # Styles
  style_title <- createStyle(fontSize = 14, textDecoration = "bold", halign = "center")
  style_header <- createStyle(textDecoration = "bold", border = "Bottom", halign = "center")
  style_estimate <- createStyle(numFmt = "#,##0", halign = "right")
  style_se <- createStyle(numFmt = "#,##0.00", halign = "right", fontColour = "#666666")
  style_ci <- createStyle(numFmt = "#,##0", halign = "right", fontColour = "#336699")
  
  # Créer une feuille par dimension
  for (dim_name in names(results_with_ci)) {
    
    dim_results <- results_with_ci[[dim_name]]
    
    if (is.null(dim_results) || nrow(dim_results) == 0) next
    
    # Nom de la feuille
    if (dim_name == "global") {
      sheet_name <- "National"
      sheet_title <- "Statistiques salariales nationales"
    } else {
      dim_config <- STAT_DIMENSIONS[[dim_name]]
      sheet_name <- gsub(" ", "_", substr(dim_config$label, 1, 30))
      sheet_title <- paste("Salaire mensuel brut -", dim_config$label)
    }
    
    addWorksheet(wb, sheet_name)
    
    # Titre
    writeData(wb, sheet_name, sheet_title, startRow = 1, startCol = 1)
    mergeCells(wb, sheet_name, cols = 1:ncol(dim_results), rows = 1)
    addStyle(wb, sheet_name, style_title, rows = 1, cols = 1)
    
    # Données
    writeData(wb, sheet_name, dim_results, startRow = 3, headerStyle = style_header)
    
    # Appliquer les styles
    n_rows <- nrow(dim_results) + 3
    
    # Colonnes d'estimation
    est_cols <- which(names(dim_results) %in% c("mean", "min", "max", "q1", "median", "q3", "p10", "p90", "n_obs", "n_weighted"))
    if (length(est_cols) > 0) {
      addStyle(wb, sheet_name, style_estimate, rows = 4:n_rows, cols = est_cols, gridExpand = TRUE)
    }
    
    # Colonnes SE
    se_cols <- which(grepl("_se$", names(dim_results)))
    if (length(se_cols) > 0) {
      addStyle(wb, sheet_name, style_se, rows = 4:n_rows, cols = se_cols, gridExpand = TRUE)
    }
    
    # Colonnes CI
    ci_cols <- which(grepl("_ci_", names(dim_results)))
    if (length(ci_cols) > 0) {
      addStyle(wb, sheet_name, style_ci, rows = 4:n_rows, cols = ci_cols, gridExpand = TRUE)
    }
    
    # Freeze panes
    freezePane(wb, sheet_name, firstActiveRow = 4, firstActiveCol = 3)
    
    # Largeur auto
    setColWidths(wb, sheet_name, cols = 1:ncol(dim_results), widths = "auto")
  }
  
  # Feuille métadonnées
  addWorksheet(wb, "Metadata")
  
  metadata <- tibble(
    Parametre = c(
      "Date création",
      "Période couverte",
      "Niveau de confiance",
      "Nombre bootstrap",
      "Winsorisation",
      "Seuil cellule minimum"
    ),
    Valeur = c(
      as.character(Sys.time()),
      period_str,
      paste0((1 - alpha) * 100, "%"),
      as.character(n_boot),
      ifelse(PROCESSING$apply_winsor, "Oui", "Non"),
      "30"
    )
  )
  
  writeData(wb, "Metadata", metadata)
  
  # Feuille légende
  addWorksheet(wb, "Legende")
  
  legend <- tibble(
    Colonne = c(
      "mean", "mean_se", "mean_ci_lower", "mean_ci_upper",
      "q1", "q1_se", "q1_ci_lower", "q1_ci_upper",
      "median", "median_se", "median_ci_lower", "median_ci_upper",
      "q3", "q3_se", "q3_ci_lower", "q3_ci_upper",
      "min", "max", "n_obs", "n_weighted"
    ),
    Description = c(
      "Moyenne pondérée", "Erreur standard de la moyenne", "Borne inférieure IC 95%", "Borne supérieure IC 95%",
      "Premier quartile (25%)", "Erreur standard Q1", "Borne inférieure IC 95%", "Borne supérieure IC 95%",
      "Médiane (50%)", "Erreur standard médiane", "Borne inférieure IC 95%", "Borne supérieure IC 95%",
      "Troisième quartile (75%)", "Erreur standard Q3", "Borne inférieure IC 95%", "Borne supérieure IC 95%",
      "Valeur minimale", "Valeur maximale", "Effectif observé", "Effectif pondéré"
    )
  )
  
  writeData(wb, "Legende", legend)
  
  # Sauvegarder
  output_file <- file.path(
    output_folder,
    paste0("stats_salaires_cnps_avec_IC_", period_str, "_", get_timestamp(), ".xlsx")
  )
  
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  message("\n", strrep("-", 40))
  message("Fichier de résultats: ", output_file)
  
  return(list(
    output_file = output_file,
    results = results_with_ci,
    period = period_str
  ))
}

message("Module de calcul des indicateurs avec IC chargé")