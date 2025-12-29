# ============================================================
# 01_FROM_EXCEL_TO_DTA.R
# Wrapper R qui appelle le script Python
# ============================================================

convert_excel_to_dta <- function(force = FALSE) {
  
  python_script <- file.path(PATHS$scripts_ingestion, "01_excel_to_dta.py")
  
  if (!file.exists(python_script)) {
    stop("Script Python non trouvé: ", python_script)
  }
  
  args <- c(
    shQuote(python_script),
    shQuote(PROJECT_ROOT)
  )
  
  if (force) {
    args <- c(args, "--force")
  }
  
  message("\nAppel du script Python...")
  message("Commande: python ", paste(args, collapse = " "), "\n")
  
  exit_code <- system2(
    command = "python",
    args = args,
    stdout = "",
    stderr = ""
  )
  
  if (exit_code != 0) {
    warning("Le script Python a retourné des erreurs (code: ", exit_code, ")")
  } else {
    message("\n✓ Conversion terminée avec succès")
  }
  
  return(invisible(exit_code))
}

message("Wrapper d'ingestion R chargé")