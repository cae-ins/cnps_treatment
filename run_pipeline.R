# ============================================================
# RUN_PIPELINE.R
# Script principal d'orchestration du pipeline CNPS
# ============================================================

# Définir le chemin racine
PIPELINE_ROOT <- "C:/Users/e_koffie/Documents/Salaires/CNPS_TREATMENT_PROJECT"

# Charger la configuration
source(file.path(PIPELINE_ROOT, "config/config.R"))

# Charger les modules
source(file.path(PATHS$scripts_pipeline, "session_manager.R"))
source(file.path(PATHS$scripts_pipeline, "model_registry.R"))

# Charger les scripts d'ingestion
source(file.path(PATHS$scripts_ingestion, "01_from_excel_to_dta.R"))

# Charger les scripts de préparation
source(file.path(PATHS$scripts_preparation, "01_column_types_matching.R"))
source(file.path(PATHS$scripts_preparation, "02_add_mois_annee.R"))
source(file.path(PATHS$scripts_preparation, "03_data_cleaning.R"))

# Charger les scripts de structuration
source(file.path(PATHS$scripts_structuring, "01_create_individual_base.R"))
source(file.path(PATHS$scripts_structuring, "02_create_firm_time_base.R"))
source(file.path(PATHS$scripts_structuring, "03_create_analytical_base.R"))
source(file.path(PATHS$scripts_structuring, "04_concat_databases.R"))
source(file.path(PATHS$scripts_structuring, "05_merge_references.R"))

# Charger les scripts de diagnostics
source(file.path(PATHS$scripts_diagnostics, "01_inconsistency_check.R"))
source(file.path(PATHS$scripts_diagnostics, "02_model_diagnostics.R"))
source(file.path(PATHS$scripts_diagnostics, "03_estimation_validation.R"))

# Charger les scripts de modélisation
source(file.path(PATHS$scripts_modeling, "01_declaration_model.R"))
source(file.path(PATHS$scripts_modeling, "02_imputation_firm.R"))
source(file.path(PATHS$scripts_modeling, "03_individual_model.R"))
source(file.path(PATHS$scripts_modeling, "04_imputation_individual.R"))

# Charger les scripts d'estimation
source(file.path(PATHS$scripts_estimation, "01_weighted_estimation.R"))
source(file.path(PATHS$scripts_estimation, "02_calc_indicators.R"))

# Pipeline principal
run_pipeline <- function(steps = "all", force_reprocess = FALSE, 
                         run_checks = TRUE, description = "") {
  
  start_time <- Sys.time()
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("  PIPELINE CNPS - TRAITEMENT DES DONNÉES SALARIALES\n")
  cat("  Démarré: ", as.character(start_time), "\n")
  cat(strrep("=", 70), "\n")
  
  # Créer une session
  session_id <- create_session(description)
  
  tryCatch({
    
    all_steps <- c(
      "init",
      "ingestion",
      "check_pre",
      "preparation",
      "check_post",
      "concat",
      "cleaning",
      "merge_references",
      "structuring",
      "modeling_declaration",
      "modeling_imputation",
      "individual_model",
      "weighting",
      "estimation",
      "validation",
      "reporting"
    )
    
    if (steps == "all") {
      steps_to_run <- all_steps
    } else {
      steps_to_run <- intersect(steps, all_steps)
    }
    
    results <- list()
    
    # ============================================================
    # ÉTAPE 1: INITIALISATION
    # ============================================================
    if ("init" %in% steps_to_run) {
      message("\n[1/16] INITIALISATION")
      init_model_registry()
    }
    
    # ============================================================
    # ÉTAPE 2: INGESTION
    # ============================================================
    if ("ingestion" %in% steps_to_run) {
      message("\n[2/16] INGESTION EXCEL → STATA")
      results$ingestion <- convert_excel_to_dta(force = force_reprocess)
    }
    
    # ============================================================
    # ÉTAPE 3: CONTRÔLE PRÉ-TYPAGE
    # ============================================================
    if ("check_pre" %in% steps_to_run && run_checks) {
      message("\n[3/16] CONTRÔLE D'INCOHÉRENCES (PRÉ-TYPAGE)")
      results$check_pre <- run_inconsistency_check(
        input_folder = PATHS$processed_dta,
        output_folder = PATHS$inconsistencies,
        suffix = paste0("pre_typing_", get_timestamp())
      )
    }
    
    # ============================================================
    # ÉTAPE 4: PRÉPARATION
    # ============================================================
    if ("preparation" %in% steps_to_run) {
      message("\n[4/16] PRÉPARATION DES DONNÉES")
      
      # 4.1 Harmonisation des types
      message("  4.1 Harmonisation des types...")
      results$types <- match_column_types()
      
      # 4.2 Ajout mois/année
      message("  4.2 Ajout MOIS/ANNEE...")
      results$period <- add_mois_annee()
    }
    
    # ============================================================
    # ÉTAPE 5: CONTRÔLE POST-TYPAGE
    # ============================================================
    if ("check_post" %in% steps_to_run && run_checks) {
      message("\n[5/16] CONTRÔLE D'INCOHÉRENCES (POST-TYPAGE)")
      results$check_post <- run_inconsistency_check(
        input_folder = PATHS$processed_dta,
        output_folder = PATHS$inconsistencies,
        suffix = paste0("post_typing_", get_timestamp())
      )
    }
    
    # ============================================================
    # ÉTAPE 6: CONCATÉNATION
    # ============================================================
    if ("concat" %in% steps_to_run) {
      message("\n[6/16] CONCATÉNATION DES BASES")
      results$concat <- concat_databases(incremental = !force_reprocess)
    }
    
    # ============================================================
    # ÉTAPE 7: NETTOYAGE
    # ============================================================
    if ("cleaning" %in% steps_to_run) {
      message("\n[7/16] NETTOYAGE DES DONNÉES")
      results$cleaning <- clean_data(apply_filters=FALSE)  ## Ne pas nettoyer la base de donnée pour l'instant
    }
    
    # ============================================================
    # ÉTAPE 8: FUSION AVEC LES RÉFÉRENCES
    # ============================================================
    if ("merge_references" %in% steps_to_run) {
      message("\n[8/16] FUSION AVEC LES BASES DE RÉFÉRENCE")
      
      # Afficher les merges disponibles
      message("  Merges configurés:")
      merge_list <- list_available_merges()
      print(merge_list)
      
      # Exécuter tous les merges activés
      results$merge <- merge_all_references()
      
      # Générer le rapport de fusion
      if (!is.null(results$merge)) {
        generate_merge_report(
          results$merge,
          file.path(get_session_dir(), "outputs", "merge_report.txt")
        )
      }
    }
    # ============================================================
    # ÉTAPE 9: STRUCTURATION
    # ============================================================
    if ("structuring" %in% steps_to_run) {
      message("\n[9/16] STRUCTURATION ANALYTIQUE")
      
      # 9.1 Base individuelle
      message("  9.1 Création base individuelle...")
      results$individual <- create_individual_base()
      
      # 9.2 Base entreprise-temps
      message("  9.2 Création base entreprise-temps...")
      results$firm_time <- create_firm_time_base(results$individual$data)
      
      # 9.3 Base analytique
      message("  9.3 Création base analytique...")
      results$analytical <- create_analytical_base(
        results$individual$data,
        results$firm_time$data
      )
    }
    
    # ============================================================
    # ÉTAPE 10: MODÉLISATION DÉCLARATION
    # ============================================================
    if ("modeling_declaration" %in% steps_to_run) {
      message("\n[10/16] MODÉLISATION DÉCLARATION ENTREPRISE")
      
      if (!is.null(results$firm_time$data)) {
        results$declaration <- run_declaration_modeling(results$firm_time$data)
      } else {
        message("  ⚠ Base entreprise-temps non disponible, étape ignorée")
      }
    }
    
    # ============================================================
    # ÉTAPE 11: IMPUTATION ENTREPRISE
    # ============================================================
    if ("modeling_imputation" %in% steps_to_run) {
      message("\n[11/16] IMPUTATION ENTREPRISE")
      
      if (!is.null(results$declaration$data)) {
        results$imputation_firm <- run_firm_imputation(results$declaration$data)
      } else {
        message("  ⚠ Données de déclaration non disponibles, étape ignorée")
      }
    }
    
    # ============================================================
    # ÉTAPE 12: MODÈLE INDIVIDUEL
    # ============================================================
    if ("individual_model" %in% steps_to_run) {
      message("\n[12/16] MODÉLISATION INDIVIDUELLE")
      
      if (!is.null(results$individual$data) && !is.null(results$firm_time$data)) {
        results$individual_model <- run_individual_modeling(
          results$individual$data,
          results$firm_time$data
        )
      } else {
        message("  ⚠ Données non disponibles, étape ignorée")
      }
    }
    
    # ============================================================
    # ÉTAPE 13: PONDÉRATIONS
    # ============================================================
    if ("weighting" %in% steps_to_run) {
      message("\n[13/16] CALCUL DES PONDÉRATIONS FINALES")
      
      # Fusionner les poids dans la base analytique
      if (!is.null(results$analytical$data)) {
        df_weighted <- results$analytical$data
        
        # Ajouter w_jt depuis le modèle de déclaration
        if (!is.null(results$declaration$data)) {
          df_weighted <- df_weighted %>%
            left_join(
              results$declaration$data %>%
                select(NUMERO_EMPLOYEUR, ANNEE, MOIS, w_jt, pi_jt),
              by = c("NUMERO_EMPLOYEUR", "ANNEE", "MOIS")
            ) %>%
            mutate(w_jt = coalesce(w_jt.y, w_jt.x, 1)) %>%
            select(-w_jt.x, -w_jt.y)
        }
        
        # Ajouter w_ijt depuis le modèle individuel
        if (!is.null(results$individual_model$data)) {
          df_weighted <- df_weighted %>%
            left_join(
              results$individual_model$data %>%
                select(ID_INDIV, NUMERO_EMPLOYEUR, ANNEE, MOIS, w_ijt, rho_ijt),
              by = c("ID_INDIV", "NUMERO_EMPLOYEUR", "ANNEE", "MOIS")
            ) %>%
            mutate(w_ijt = coalesce(w_ijt.y, w_ijt.x, 1)) %>%
            select(-w_ijt.x, -w_ijt.y)
        }
        
        # Calculer le poids final
        df_weighted <- df_weighted %>%
          mutate(
            w_jt = coalesce(w_jt, 1),
            w_ijt = coalesce(w_ijt, 1),
            w_final = w_jt * w_ijt
          )
        
        results$weighted_data <- df_weighted
        
        message("  Poids calculés pour ", format(nrow(df_weighted), big.mark = " "), " observations")
        message("  Poids moyen: ", round(mean(df_weighted$w_final, na.rm = TRUE), 3))
      }
    }
    
    # ============================================================
    # ÉTAPE 14: ESTIMATION
    # ============================================================
    if ("estimation" %in% steps_to_run) {
      message("\n[14/16] ESTIMATION DES STATISTIQUES PONDÉRÉES")
      
      # Préparer les imputations si disponibles
      imputations <- NULL
      if (!is.null(results$imputation_firm$data_imputed)) {
        # Séparer par imputation_id
        imp_data <- results$imputation_firm$data_imputed
        imp_ids <- unique(imp_data$imputation_id)
        imputations <- lapply(imp_ids, function(m) {
          imp_data %>% filter(imputation_id == m)
        })
      }
      
      # Exécuter l'estimation
      if (!is.null(results$weighted_data)) {
        results$estimation <- run_weighted_estimation(
          results$weighted_data,
          imputations
        )
      } else if (!is.null(results$analytical$data)) {
        results$estimation <- run_weighted_estimation(
          results$analytical$data,
          imputations
        )
      }
    }
    
    # ============================================================
    # ÉTAPE 15: VALIDATION
    # ============================================================
    if ("validation" %in% steps_to_run) {
      message("\n[15/16] VALIDATION DES ESTIMATIONS")
      
      if (!is.null(results$estimation$results)) {
        results$validation <- validate_estimations(results$estimation$results)
        
        # Générer le rapport de validation
        generate_validation_report(
          results$validation,
          file.path(
            get_session_dir(),
            "outputs",
            paste0("validation_report_", get_timestamp(), ".txt")
          )
        )
        
        # Supprimer les petites cellules si nécessaire
        if (!results$validation$valid) {
          results$estimation$results <- suppress_small_cells(
            results$estimation$results
          )
        }
      }
    }
    
    # ============================================================
    # ÉTAPE 16: REPORTING
    # ============================================================
    if ("reporting" %in% steps_to_run) {
      message("\n[16/16] GÉNÉRATION DES RAPPORTS")
      
      # Calculer les indicateurs avec IC et SE
      if (!is.null(results$estimation)) {
        results$indicators <- calculate_indicators_with_ci(
          results$estimation,
          output_folder = PATHS$indicators
        )
      } else {
        # Fallback sur le calcul simple
        results$indicators <- calculate_indicators()
      }
      
      # Copier les outputs dans la session
      session_output_dir <- file.path(get_session_dir(), "outputs")
      if (!is.null(results$indicators$output_file)) {
        file.copy(
          results$indicators$output_file,
          session_output_dir,
          overwrite = TRUE
        )
      }
    }
    
    # Fermer la session avec succès
    close_session(status = "completed", summary = results)
    
  }, error = function(e) {
    message("\n✖ ERREUR: ", e$message)
    close_session(status = "failed", summary = list(error = e$message))
    stop(e)
  })
  
  # Résumé final
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "mins")
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("  PIPELINE TERMINÉ\n")
  cat("  Session: ", session_id, "\n")
  cat("  Durée: ", round(duration, 2), " minutes\n")
  cat(strrep("=", 70), "\n")
  
  # Afficher les fichiers de sortie
  if (!is.null(results$indicators$output_file)) {
    cat("\nFichiers de sortie:\n")
    cat("  Indicateurs: ", results$indicators$output_file, "\n")
  }
  if (!is.null(results$check_pre$output_file)) {
    cat("  Audit pré-typage: ", results$check_pre$output_file, "\n")
  }
  if (!is.null(results$check_post$output_file)) {
    cat("  Audit post-typage: ", results$check_post$output_file, "\n")
  }
  
  return(invisible(results))
}

# Fonctions utilitaires
run_incremental <- function() {
  run_pipeline(force_reprocess = FALSE, run_checks = FALSE,
               description = "Mise à jour incrémentale")
}

run_full <- function() {
  run_pipeline(force_reprocess = TRUE, run_checks = TRUE,
               description = "Traitement complet")
}

# Afficher l'aide
if (interactive()) {
  cat("\n")
  cat("Fonctions disponibles:\n")
  cat("  run_pipeline()      - Pipeline complet\n")
  cat("  run_incremental()   - Mise à jour incrémentale\n")
  cat("  run_full()          - Retraitement complet\n")
  cat("  list_sessions()     - Voir les sessions\n")
  cat("  list_models()       - Voir les modèles\n")
  cat("\n")
}