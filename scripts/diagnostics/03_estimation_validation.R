# ============================================================
# 03_ESTIMATION_VALIDATION.R
# Validation des estimations produites
# ============================================================

library(dplyr)

# Valider les estimations par dimension
validate_estimations <- function(results, dimension_name = NULL) {
  
  message("Validation des estimations...")
  
  rules <- VALIDATION_RULES$estimation_output
  all_issues <- list()
  
  for (dim_name in names(results)) {
    dim_results <- results[[dim_name]]
    dim_issues <- list()
    
    # Vérifier la taille des cellules
    if ("n_weighted" %in% names(dim_results)) {
      min_cell <- STAT_DIMENSIONS[[dim_name]]$min_cell_size
      if (!is.null(min_cell)) {
        small_cells <- dim_results %>%
          filter(n_weighted < min_cell)
        
        if (nrow(small_cells) > 0) {
          dim_issues$small_cells <- list(
            n_cells = nrow(small_cells),
            threshold = min_cell,
            message = paste(nrow(small_cells), "cellules sous le seuil minimum")
          )
        }
      }
    }
    
    # Vérifier la plausibilité des moyennes
    if ("mean" %in% names(dim_results)) {
      implausible <- dim_results %>%
        filter(mean < 50000 | mean > 100000000)
      
      if (nrow(implausible) > 0) {
        dim_issues$implausible_means <- list(
          n_cells = nrow(implausible),
          message = paste(nrow(implausible), "moyennes hors plage plausible")
        )
      }
    }
    
    # Vérifier la cohérence des quantiles
    if (all(c("q1", "median", "q3") %in% names(dim_results))) {
      incoherent <- dim_results %>%
        filter(q1 > median | median > q3)
      
      if (nrow(incoherent) > 0) {
        dim_issues$incoherent_quantiles <- list(
          n_cells = nrow(incoherent),
          message = paste(nrow(incoherent), "cellules avec quantiles incohérents")
        )
      }
    }
    
    if (length(dim_issues) > 0) {
      all_issues[[dim_name]] <- dim_issues
    }
  }
  
  # Résumé
  n_issues <- sum(sapply(all_issues, length))
  
  if (n_issues > 0) {
    message("  ⚠ ", n_issues, " problèmes détectés")
    for (dim_name in names(all_issues)) {
      for (issue_name in names(all_issues[[dim_name]])) {
        issue <- all_issues[[dim_name]][[issue_name]]
        message("    - ", dim_name, ": ", issue$message)
      }
    }
  } else {
    message("  ✓ Toutes les estimations sont valides")
  }
  
  return(list(
    valid = n_issues == 0,
    issues = all_issues
  ))
}

# Supprimer les cellules non publiables
suppress_small_cells <- function(results, min_cell_size = 30) {
  
  message("Suppression des cellules trop petites (seuil: ", min_cell_size, ")...")
  
  n_suppressed <- 0
  
  for (dim_name in names(results)) {
    if (!"n_weighted" %in% names(results[[dim_name]])) next
    
    n_before <- nrow(results[[dim_name]])
    
    results[[dim_name]] <- results[[dim_name]] %>%
      filter(n_weighted >= min_cell_size)
    
    n_after <- nrow(results[[dim_name]])
    n_suppressed <- n_suppressed + (n_before - n_after)
  }
  
  message("  Cellules supprimées: ", n_suppressed)
  
  return(results)
}

# Comparer les résultats entre deux sessions
compare_results <- function(results_new, results_old, tolerance = 0.05) {
  
  message("Comparaison avec les résultats précédents...")
  
  comparisons <- list()
  
  for (dim_name in intersect(names(results_new), names(results_old))) {
    new <- results_new[[dim_name]]
    old <- results_old[[dim_name]]
    
    # Identifier les colonnes communes
    common_cols <- intersect(names(new), names(old))
    stat_cols <- intersect(common_cols, names(STATISTICS))
    
    if (length(stat_cols) == 0) next
    
    # Joindre les données
    comparison <- new %>%
      inner_join(old, by = intersect(names(new), c("ANNEE", "MOIS", 
                                                    get_dimension_variable(dim_name))),
                 suffix = c("_new", "_old"))
    
    # Calculer les différences
    for (stat in stat_cols) {
      col_new <- paste0(stat, "_new")
      col_old <- paste0(stat, "_old")
      
      if (all(c(col_new, col_old) %in% names(comparison))) {
        comparison[[paste0(stat, "_diff")]] <- 
          comparison[[col_new]] - comparison[[col_old]]
        comparison[[paste0(stat, "_pct_diff")]] <- 
          (comparison[[col_new]] - comparison[[col_old]]) / comparison[[col_old]] * 100
      }
    }
    
    comparisons[[dim_name]] <- comparison
  }
  
  return(comparisons)
}

# Générer un rapport de validation
generate_validation_report <- function(validation_result, output_file = NULL) {
  
  if (is.null(output_file)) {
    output_file <- file.path(
      PATHS$analytical_reports,
      paste0("validation_report_", get_timestamp(), ".txt")
    )
  }
  
  ensure_dir(dirname(output_file))
  
  sink(output_file)
  cat("=== RAPPORT DE VALIDATION DES ESTIMATIONS ===\n")
  cat("Date: ", as.character(Sys.time()), "\n\n")
  
  if (validation_result$valid) {
    cat("STATUT: VALIDE\n")
    cat("Toutes les estimations respectent les critères de qualité.\n")
  } else {
    cat("STATUT: PROBLÈMES DÉTECTÉS\n\n")
    
    for (dim_name in names(validation_result$issues)) {
      cat("--- Dimension: ", dim_name, " ---\n")
      
      for (issue_name in names(validation_result$issues[[dim_name]])) {
        issue <- validation_result$issues[[dim_name]][[issue_name]]
        cat("  * ", issue$message, "\n")
      }
      cat("\n")
    }
  }
  
  sink()
  
  message("Rapport de validation: ", output_file)
  
  return(output_file)
}

message("Module de validation des estimations chargé")