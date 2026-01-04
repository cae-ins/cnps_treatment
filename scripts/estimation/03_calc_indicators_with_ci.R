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
  var_mean <- sum(weights^2 * (y - mean_val)^2, na.rm = TRUE) / (sum_w^2)
  
  # Correction pour échantillon fini
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

# Quantile pondéré simple (sans bootstrap pour éviter erreurs)
compute_quantile_weighted <- function(y, weights, p) {
  if (length(y) == 0 || all(is.na(y))) return(NA_real_)
  
  ord <- order(y)
  y_sorted <- y[ord]
  w_sorted <- weights[ord]
  
  cum_w <- cumsum(w_sorted) / sum(w_sorted, na.rm = TRUE)
  
  idx <- which(cum_w >= p)[1]
  if (is.na(idx)) return(NA_real_)
  
  return(y_sorted[idx])
}

# Quantile pondéré avec variance (méthode bootstrap)
compute_quantile_weighted_with_var <- function(y, weights, p, n_boot = 200) {
  
  n <- length(y)
  if (n < 10) {
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
    idx <- sample(1:n, n, replace = TRUE)
    y_boot <- y[idx]
    w_boot <- weights[idx]
    boot_estimates[b] <- compute_quantile_weighted(y_boot, w_boot, p)
  }
  
  var_q <- var(boot_estimates, na.rm = TRUE)
  
  return(list(
    estimate = q_val,
    variance = var_q,
    se = sqrt(var_q),
    n = n,
    boot_estimates = boot_estimates
  ))
}

# Min/Max avec range
compute_minmax_with_range <- function(y, weights, type = "min", k = 5) {
  
  n <- length(y)
  
  if (type == "min") {
    val <- min(y, na.rm = TRUE)
    sorted_y <- sort(y)[1:min(k, n)]
  } else {
    val <- max(y, na.rm = TRUE)
    sorted_y <- sort(y, decreasing = TRUE)[1:min(k, n)]
  }
  
  range_val <- diff(range(sorted_y))
  
  return(list(
    estimate = val,
    variance = NA_real_,
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
  
  M <- length(estimates_list)
  
  Q_m <- sapply(estimates_list, function(x) x$estimate)
  Q_bar <- mean(Q_m, na.rm = TRUE)
  B <- var(Q_m, na.rm = TRUE)
  
  U_m <- sapply(estimates_list, function(x) {
    if (!is.null(x$se) && !is.na(x$se)) x$se^2 else NA_real_
  })
  U_bar <- mean(U_m, na.rm = TRUE)
  
  if (!is.na(U_bar) && !is.na(B)) {
    T_var <- U_bar + (1 + 1/M) * B
  } else if (!is.na(B)) {
    T_var <- (1 + 1/M) * B
  } else {
    T_var <- NA_real_
  }
  
  T_se <- if (!is.na(T_var)) sqrt(T_var) else NA_real_
  
  if (!is.na(U_bar) && !is.na(B) && B > 0) {
    lambda <- (1 + 1/M) * B / T_var
    df <- (M - 1) / lambda^2
  } else {
    df <- Inf
  }
  
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
  
  # Vérifier les variables
  if (!salary_var %in% names(df)) {
    if ("SALAIRE_BRUT" %in% names(df)) {
      salary_var <- "SALAIRE_BRUT"
    } else {
      stop("Variable de salaire non trouvée")
    }
  }
  
  if (!weight_var %in% names(df)) {
    df[[weight_var]] <- 1
  }
  
  # Variables de période si disponibles
  period_vars <- intersect(c("ANNEE", "MOIS"), names(df))
  all_group_vars <- unique(c(group_vars, period_vars))
  
  # Grouper et calculer
  results <- df %>%
    filter(!is.na(.data[[salary_var]]), !is.na(.data[[weight_var]])) %>%
    group_by(across(all_of(all_group_vars))) %>%
    group_modify(~ {
      y <- .x[[salary_var]]
      w <- .x[[weight_var]]
      
      stats <- compute_statistics_with_ci(y, w, alpha, n_boot)
      
      if (is.null(stats)) {
        return(tibble(n_obs = 0))
      }
      
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
  
  # Supprimer la colonne .group si créée
  if (".group" %in% names(results)) {
    results <- results %>% select(-.group)
  }
  
  return(results)
}

# ------------------------------------------------------------
# FONCTION PRINCIPALE (CORRIGÉE)
# ------------------------------------------------------------

calculate_indicators_with_ci <- function(estimation_results = NULL,
                                          input_file = NULL,
                                          output_folder = NULL,
                                          alpha = 0.05,
                                          n_boot = 200) {
  
  message("\n", strrep("=", 60))
  message("CALCUL DES INDICATEURS AVEC INTERVALLES DE CONFIANCE")
  message(strrep("=", 60))
  
  # Déterminer le dossier de sortie
  if (is.null(output_folder)) {
    if (exists("PATHS") && !is.null(PATHS$indicators)) {
      output_folder <- PATHS$indicators
    } else {
      output_folder <- "output/indicators"
    }
  }
  
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  # ----------------------------------------------------------
  # 1. DÉTERMINER LA SOURCE DES DONNÉES
  # ----------------------------------------------------------
  
  results_with_ci <- NULL
  df <- NULL
  use_precalculated <- FALSE
  
  if (!is.null(estimation_results)) {
    
    # Vérifier que ce n'est pas une fonction
    if (is.function(estimation_results)) {
      message("  ⚠ estimation_results est une fonction, ignoré")
      estimation_results <- NULL
      
    } else if (is.list(estimation_results)) {
      
      # Cas 1: Structure avec $results contenant des tibbles par dimension
      if (!is.null(estimation_results$results) && is.list(estimation_results$results)) {
        
        # Vérifier que c'est bien une liste de data.frames
        first_elem_name <- names(estimation_results$results)[1]
        first_elem <- estimation_results$results[[first_elem_name]]
        
        if (is.data.frame(first_elem)) {
          message("  Source: résultats pré-calculés")
          message("  Dimensions disponibles: ", paste(names(estimation_results$results), collapse = ", "))
          
          results_with_ci <- estimation_results$results
          use_precalculated <- TRUE
        }
      }
      
      # Cas 2: Chercher un data.frame brut
      if (!use_precalculated) {
        for (loc in c("data", "df_analytical", "df")) {
          if (!is.null(estimation_results[[loc]]) && is.data.frame(estimation_results[[loc]])) {
            df <- estimation_results[[loc]]
            message("  Source: données brutes depuis $", loc)
            break
          }
        }
      }
    } else if (is.data.frame(estimation_results)) {
      df <- estimation_results
      message("  Source: data.frame direct")
    }
  }
  
  # Si toujours pas de données, charger depuis fichier
  if (!use_precalculated && is.null(df)) {
    
    if (is.null(input_file)) {
      # Chercher le fichier le plus récent
      search_paths <- c()
      if (exists("PATHS")) {
        if (!is.null(PATHS$cleaned_analytical)) search_paths <- c(search_paths, PATHS$cleaned_analytical)
        if (!is.null(PATHS$cleaned_final)) search_paths <- c(search_paths, PATHS$cleaned_final)
      }
      
      for (path in search_paths) {
        if (dir.exists(path)) {
          files <- list.files(path, pattern = "\\.dta$", full.names = TRUE)
          if (length(files) > 0) {
            input_file <- files[which.max(file.mtime(files))]
            break
          }
        }
      }
    }
    
    if (!is.null(input_file) && file.exists(input_file)) {
      message("  Fichier source: ", basename(input_file))
      df <- read_dta(input_file)
    } else {
      stop("Aucune donnée disponible")
    }
  }
  
  # ----------------------------------------------------------
  # 2A. TRAITEMENT DES RÉSULTATS PRÉ-CALCULÉS
  # ----------------------------------------------------------
  
  if (use_precalculated && !is.null(results_with_ci)) {
    
    message("\n  Utilisation des résultats pré-calculés...")
    
    # Afficher un résumé
    for (dim_name in names(results_with_ci)) {
      dim_data <- results_with_ci[[dim_name]]
      if (is.data.frame(dim_data)) {
        message("    ✓ ", dim_name, ": ", nrow(dim_data), " lignes × ", ncol(dim_data), " colonnes")
      }
    }
    
    # Extraire la période depuis les données
    period_str <- extract_period_string(results_with_ci)
    
    # Créer le fichier Excel
    output_file <- create_excel_output(results_with_ci, output_folder, period_str)
    
    message("\n", strrep("-", 40))
    message("Fichier de résultats: ", output_file)
    
    return(list(
      output_file = output_file,
      results = results_with_ci,
      period = period_str,
      source = "pre_calculated"
    ))
  }
  
  # ----------------------------------------------------------
  # 2B. CALCUL DEPUIS LES DONNÉES BRUTES
  # ----------------------------------------------------------
  
  message("\n  Calcul des statistiques depuis les données brutes...")
  message("  Observations: ", format(nrow(df), big.mark = " "))
  
  # S'assurer que w_final existe
  if (!"w_final" %in% names(df)) {
    if ("w_jt" %in% names(df)) {
      df$w_final <- df$w_jt
    } else {
      df$w_final <- 1
    }
    message("  Poids: ", ifelse("w_jt" %in% names(df), "w_jt utilisé", "initialisés à 1"))
  }
  
  results_with_ci <- list()
  
  # Global
  message("  Dimension: Global")
  results_with_ci$global <- compute_group_statistics_with_ci(
    df, group_vars = NULL, alpha = alpha, n_boot = n_boot
  )
  
  # Par dimension (si STAT_DIMENSIONS existe)
  if (exists("STAT_DIMENSIONS")) {
    for (dim_name in names(STAT_DIMENSIONS)) {
      if (dim_name == "global") next
      
      dim_config <- STAT_DIMENSIONS[[dim_name]]
      if (!isTRUE(dim_config$enabled)) next
      
      var_name <- dim_config$variable
      if (is.null(var_name) || !var_name %in% names(df)) {
        message("  Dimension ", dim_name, ": variable absente (", var_name, ")")
        next
      }
      
      n_modalities <- n_distinct(df[[var_name]], na.rm = TRUE)
      if (n_modalities > 100) {
        message("  Dimension ", dim_name, ": trop de modalités (", n_modalities, ")")
        next
      }
      
      message("  Dimension: ", dim_config$label, " (", n_modalities, " modalités)")
      
      stats <- compute_group_statistics_with_ci(
        df, group_vars = var_name, alpha = alpha, n_boot = n_boot
      )
      
      # Filtrer les petites cellules
      min_cell <- ifelse(!is.null(dim_config$min_cell_size), dim_config$min_cell_size, 30)
      stats <- stats %>% filter(n_obs >= min_cell)
      
      results_with_ci[[dim_name]] <- stats
    }
  }
  
  # Extraire la période
  period_str <- "unknown"
  if ("ANNEE" %in% names(df) && "MOIS" %in% names(df)) {
    periods <- df %>% 
      dplyr::distinct(ANNEE, MOIS) %>% 
      dplyr::arrange(ANNEE, MOIS)
    
    if (nrow(periods) > 0) {
      min_p <- periods[1, ]
      max_p <- periods[nrow(periods), ]
      period_str <- sprintf("%02d_%04d-%02d_%04d",
                            min_p$MOIS, min_p$ANNEE,
                            max_p$MOIS, max_p$ANNEE)
    }
  }
  
  # Créer le fichier Excel
  output_file <- create_excel_output(results_with_ci, output_folder, period_str)
  
  message("\n", strrep("-", 40))
  message("Fichier de résultats: ", output_file)
  
  return(list(
    output_file = output_file,
    results = results_with_ci,
    period = period_str,
    source = "calculated"
  ))
}

# ------------------------------------------------------------
# FONCTIONS UTILITAIRES
# ------------------------------------------------------------

# Extraire la chaîne de période depuis les résultats
extract_period_string <- function(results_list) {
  
  # Chercher ANNEE et MOIS dans le premier data.frame
  for (dim_name in names(results_list)) {
    dim_data <- results_list[[dim_name]]
    
    if (is.data.frame(dim_data) && "ANNEE" %in% names(dim_data) && "MOIS" %in% names(dim_data)) {
      periods <- dim_data %>% 
        dplyr::distinct(ANNEE, MOIS) %>% 
        dplyr::arrange(ANNEE, MOIS)
      
      if (nrow(periods) > 0) {
        min_p <- periods[1, ]
        max_p <- periods[nrow(periods), ]
        return(sprintf("%02d_%04d-%02d_%04d",
                       min_p$MOIS, min_p$ANNEE,
                       max_p$MOIS, max_p$ANNEE))
      }
    }
  }
  
  return(format(Sys.Date(), "%Y%m%d"))
}

# Créer le fichier Excel de sortie
create_excel_output <- function(results_with_ci, output_folder, period_str) {
  
  message("\n  Création du fichier Excel...")
  
  # Créer le workbook
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
    
    if (is.null(dim_results) || !is.data.frame(dim_results) || nrow(dim_results) == 0) next
    
    # Nom de la feuille (max 31 caractères)
    sheet_name <- substr(gsub("[^a-zA-Z0-9_]", "", dim_name), 1, 31)
    if (sheet_name == "") sheet_name <- "Data"
    
    # Éviter les doublons
    existing_sheets <- names(wb)
    if (sheet_name %in% existing_sheets) {
      sheet_name <- paste0(sheet_name, "_", length(existing_sheets))
    }
    
    # Titre
    if (dim_name == "global") {
      sheet_title <- "Statistiques salariales nationales"
    } else if (exists("STAT_DIMENSIONS") && !is.null(STAT_DIMENSIONS[[dim_name]])) {
      sheet_title <- paste("Salaire mensuel brut -", STAT_DIMENSIONS[[dim_name]]$label)
    } else {
      sheet_title <- paste("Statistiques -", dim_name)
    }
    
    addWorksheet(wb, sheet_name)
    
    # Titre
    writeData(wb, sheet_name, sheet_title, startRow = 1, startCol = 1)
    mergeCells(wb, sheet_name, cols = 1:min(ncol(dim_results), 10), rows = 1)
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
    
    message("    ✓ Feuille: ", sheet_name)
  }
  
  # Feuille métadonnées
  addWorksheet(wb, "Metadata")
  
  metadata <- data.frame(
    Parametre = c(
      "Date création",
      "Période couverte",
      "Niveau de confiance",
      "Dimensions"
    ),
    Valeur = c(
      as.character(Sys.time()),
      period_str,
      "95%",
      paste(names(results_with_ci), collapse = ", ")
    ),
    stringsAsFactors = FALSE
  )
  
  writeData(wb, "Metadata", metadata)
  
  # Feuille légende
  addWorksheet(wb, "Legende")
  
  legend <- data.frame(
    Colonne = c(
      "mean", "mean_se", "mean_ci_lower", "mean_ci_upper",
      "median", "median_se", "median_ci_lower", "median_ci_upper",
      "min", "max", "n_obs", "n_weighted"
    ),
    Description = c(
      "Moyenne pondérée", "Erreur standard", "Borne inférieure IC 95%", "Borne supérieure IC 95%",
      "Médiane (50%)", "Erreur standard", "Borne inférieure IC 95%", "Borne supérieure IC 95%",
      "Valeur minimale", "Valeur maximale", "Effectif observé", "Effectif pondéré"
    ),
    stringsAsFactors = FALSE
  )
  
  writeData(wb, "Legende", legend)
  
  # Sauvegarder
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- file.path(
    output_folder,
    paste0("stats_salaires_cnps_avec_IC_", period_str, "_", timestamp, ".xlsx")
  )
  
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  return(output_file)
}

message("Module de calcul des indicateurs avec IC chargé")