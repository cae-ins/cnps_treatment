# ============================================================
# RUN_PIPELINE.R
# Script principal d'orchestration du pipeline CNPS
# ============================================================

# ------------------------------------------------------------
# DÉFINIR LE CHEMIN RACINE DU PROJET
# ------------------------------------------------------------
PIPELINE_ROOT <- "C:/Users/e_koffie/Documents/Salaires/CNPS_TREATMENT"

# ------------------------------------------------------------
# CHARGER LA CONFIGURATION
# ------------------------------------------------------------
source(file.path(PIPELINE_ROOT, "config/config.R"))

# ------------------------------------------------------------
# CHARGER TOUS LES SCRIPTS
# ------------------------------------------------------------
source(file.path(PATHS$scripts, "00_init_project.R"))
source(file.path(PATHS$scripts, "01_from_excel_to_dta.R"))
source(file.path(PATHS$scripts, "inconsistency_check.R"))
source(file.path(PATHS$scripts, "02_column_types_matching.R"))
source(file.path(PATHS$scripts, "03_add_mois_annee.R"))
source(file.path(PATHS$scripts, "04_concat_databases.R"))
source(file.path(PATHS$scripts, "data_cleaning.R"))
source(file.path(PATHS$scripts, "merge_sector_cod.R"))
source(file.path(PATHS$scripts, "05_calc_indicators.R"))

# ------------------------------------------------------------
# FONCTION PRINCIPALE DU PIPELINE
# ------------------------------------------------------------
run_pipeline <- function(steps = "all",
                         force_reprocess = FALSE,
                         run_checks = TRUE) {
  start_time <- Sys.time()

  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("  PIPELINE CNPS - TRAITEMENT DES DONNÉES SALARIALES\n")
  cat("  Démarré: ", as.character(start_time), "\n")
  cat(strrep("=", 70), "\n")

  # Définir les étapes
  all_steps <- c(
    "init",
    "excel_to_dta",
    "check_1",
    "types_matching",
    "check_2",
    "add_mois_annee",
    "concat",
    "cleaning",
    "merge_sectors",
    "indicators"
  )

  if (steps == "all") {
    steps_to_run <- all_steps
  } else {
    steps_to_run <- intersect(steps, all_steps)
  }

  results <- list()

  # ------------------------------------------------------------
  # ÉTAPE 0: INITIALISATION
  # ------------------------------------------------------------
  if ("init" %in% steps_to_run) {
    cat("\n[0/9] INITIALISATION\n")
    init_directories()
    init_registry(force_reset = force_reprocess)
  }

  # ------------------------------------------------------------
  # ÉTAPE 1: EXCEL → DTA
  # ------------------------------------------------------------
  if ("excel_to_dta" %in% steps_to_run) {
    cat("\n[1/9] CONVERSION EXCEL → STATA\n")
    results$excel_to_dta <- convert_excel_to_dta(force = force_reprocess)
  }

  # ------------------------------------------------------------
  # ÉTAPE 2: CONTRÔLE D'INCOHÉRENCES (PRÉ-TYPAGE)
  # ------------------------------------------------------------
  if ("check_1" %in% steps_to_run && run_checks) {
    cat("\n[2/9] CONTRÔLE D'INCOHÉRENCES (PRÉ-TYPAGE)\n")
    results$check_1 <- run_inconsistency_check(
      suffix = paste0("pre_typing_", get_timestamp())
    )
  }

  # ------------------------------------------------------------
  # ÉTAPE 3: HARMONISATION DES TYPES
  # ------------------------------------------------------------
  if ("types_matching" %in% steps_to_run) {
    cat("\n[3/9] HARMONISATION DES TYPES DE COLONNES\n")
    results$types_matching <- match_column_types()
  }

  # ------------------------------------------------------------
  # ÉTAPE 4: CONTRÔLE D'INCOHÉRENCES (POST-TYPAGE)
  # ------------------------------------------------------------
  if ("check_2" %in% steps_to_run && run_checks) {
    cat("\n[4/9] CONTRÔLE D'INCOHÉRENCES (POST-TYPAGE)\n")
    results$check_2 <- run_inconsistency_check(
      suffix = paste0("post_typing_", get_timestamp())
    )
  }

  # ------------------------------------------------------------
  # ÉTAPE 5: AJOUT MOIS/ANNEE
  # ------------------------------------------------------------
  if ("add_mois_annee" %in% steps_to_run) {
    cat("\n[5/9] AJOUT DES COLONNES MOIS/ANNEE\n")
    results$add_mois_annee <- add_mois_annee()
  }

  # ------------------------------------------------------------
  # ÉTAPE 6: CONCATÉNATION
  # ------------------------------------------------------------
  if ("concat" %in% steps_to_run) {
    cat("\n[6/9] CONCATÉNATION DES BASES\n")
    results$concat <- concat_databases(incremental = !force_reprocess)
  }

  # ------------------------------------------------------------
  # ÉTAPE 7: NETTOYAGE
  # ------------------------------------------------------------
  if ("cleaning" %in% steps_to_run) {
    cat("\n[7/9] NETTOYAGE DES DONNÉES\n")
    results$cleaning <- clean_data()
  }

  # ------------------------------------------------------------
  # ÉTAPE 8: FUSION CODES SECTEURS
  # ------------------------------------------------------------
  if ("merge_sectors" %in% steps_to_run) {
    cat("\n[8/9] FUSION AVEC LES CODES SECTEURS\n")
    results$merge_sectors <- merge_sector_codes()
  }

  # ------------------------------------------------------------
  # ÉTAPE 9: CALCUL DES INDICATEURS
  # ------------------------------------------------------------
  if ("indicators" %in% steps_to_run) {
    cat("\n[9/9] CALCUL DES INDICATEURS\n")
    results$indicators <- calculate_indicators()
  }

  # ------------------------------------------------------------
  # RÉSUMÉ FINAL
  # ------------------------------------------------------------
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "mins")

  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("  PIPELINE TERMINÉ\n")
  cat("  Durée: ", round(duration, 2), " minutes\n")
  cat("  Terminé: ", as.character(end_time), "\n")
  cat(strrep("=", 70), "\n")

  # Afficher les fichiers de sortie
  if (!is.null(results$indicators$output_file)) {
    cat("\nFichiers de sortie:\n")
    cat("  Résultats: ", results$indicators$output_file, "\n")
  }
  if (!is.null(results$merge_sectors$output_file)) {
    cat("  Base finale: ", results$merge_sectors$output_file, "\n")
  }

  return(invisible(results))
}

# ------------------------------------------------------------
# FONCTION POUR TRAITEMENT INCRÉMENTAL
# ------------------------------------------------------------
run_incremental <- function() {
  cat("\n=== MODE INCRÉMENTAL ===\n")

  # Vérifier les nouveaux fichiers
  files_status <- get_files_to_process()

  cat("Nouveaux fichiers: ", length(files_status$new), "\n")
  cat("Fichiers modifiés: ", length(files_status$modified), "\n")
  cat("Fichiers à jour:   ", length(files_status$up_to_date), "\n")

  if (length(files_status$new) + length(files_status$modified) == 0) {
    cat("\nAucune mise à jour nécessaire.\n")
    return(invisible(NULL))
  }

  # Lancer le pipeline
  run_pipeline(force_reprocess = FALSE, run_checks = FALSE)
}

# ------------------------------------------------------------
# EXÉCUTION
# ------------------------------------------------------------
# Exécution par défaut si appelé directement
if (interactive()) {
  cat("\n")
  cat("Fonctions disponibles:\n")
  cat("  run_pipeline()        - Pipeline complet\n")
  cat("  run_pipeline(force_reprocess = TRUE) - Retraiter tout\n")
  cat("  run_incremental()     - Mise à jour incrémentale\n")
  cat("  get_files_to_process() - Voir le statut des fichiers\n")
  cat("\n")
}

run_pipeline()
