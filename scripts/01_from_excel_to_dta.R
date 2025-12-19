# ============================================================
# 01_FROM_EXCEL_TO_DTA.R
# Wrapper R qui appelle le script Python
# ============================================================

# ------------------------------------------------------------
# CHARGER LA CONFIGURATION
# ------------------------------------------------------------
if (exists("PIPELINE_ROOT")) {
  config_path <- file.path(PIPELINE_ROOT, "config/config.R")
} else {
  config_path <- file.path(dirname(sys.frame(1)$ofile), "../config/config.R")
}
source(config_path)

# ------------------------------------------------------------
# FONCTION PRINCIPALE
# ------------------------------------------------------------
convert_excel_to_dta <- function(force = FALSE) {
  
  # Chemin du script Python
  python_script <- file.path(PATHS$scripts, "01_excel_to_dta.py")
  
  if (!file.exists(python_script)) {
    stop("Script Python non trouvé: ", python_script)
  }
  
  # Construire les arguments
  args <- c(
    shQuote(python_script),
    shQuote(PROJECT_ROOT)
  )
  
  if (force) {
    args <- c(args, "--force")
  }
  
  # Exécuter Python et afficher la sortie en temps réel
  message("\nAppel du script Python...")
  message("Commande: python ", paste(args, collapse = " "), "\n")
  
  # Utiliser system() pour voir la sortie en temps réel
  exit_code <- system2(
    command = "python",
    args = args,
    stdout = "",  # Afficher directement dans la console
    stderr = ""   # Afficher directement dans la console
  )
  
  # Vérifier le résultat
  if (exit_code != 0) {
    warning("Le script Python a retourné des erreurs (code: ", exit_code, ")")
  } else {
    message("\n✓ Conversion terminée avec succès")
  }
  
  return(invisible(exit_code))
}

# ------------------------------------------------------------
# EXÉCUTION SI APPELÉ DIRECTEMENT
# ------------------------------------------------------------
if (sys.nframe() == 0) {
  convert_excel_to_dta()
}