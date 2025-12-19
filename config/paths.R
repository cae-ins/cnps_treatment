# ============================================================
# PATHS.R
# Définition centralisée de tous les chemins du projet
# ============================================================

# ------------------------------------------------------------
# CHEMIN RACINE DU PROJET
# ------------------------------------------------------------
# Modifier ce chemin selon votre installation
PROJECT_ROOT <- "C:/Users/e_koffie/Documents/Salaires/CNPS_TREATMENT"

# ------------------------------------------------------------
# STRUCTURE DES DOSSIERS
# ------------------------------------------------------------
PATHS <- list(
  

  # === DONNÉES BRUTES ===
  raw_excel       = file.path(PROJECT_ROOT, "data/raw/excel"),
  
  # === DONNÉES TRAITÉES ===
  processed_dta   = file.path(PROJECT_ROOT, "data/processed/dta"),
  registry        = file.path(PROJECT_ROOT, "data/processed/registry"),
  registry_file   = file.path(PROJECT_ROOT, "data/processed/registry/processed_index.csv"),
  
  # === DONNÉES NETTOYÉES ===
  cleaned_monthly = file.path(PROJECT_ROOT, "data/cleaned/monthly"),
  cleaned_final   = file.path(PROJECT_ROOT, "data/cleaned/final"),
  
  # === ARCHIVES ===
  archive         = file.path(PROJECT_ROOT, "data/archive"),
  
  # === SORTIES ===
  inconsistencies = file.path(PROJECT_ROOT, "output/inconsistencies"),
  results         = file.path(PROJECT_ROOT, "output/results"),
  
  # === SCRIPTS ET CONFIG ===
  scripts         = file.path(PROJECT_ROOT, "scripts"),
  config          = file.path(PROJECT_ROOT, "config")
)

# ------------------------------------------------------------
# FICHIERS EXTERNES (POUR MERGE)
# ------------------------------------------------------------
EXTERNAL_FILES <- list(
  
  sector_codes = list(
    path      = file.path(PROJECT_ROOT, "data/raw/CNPS_M_2024.xlsx"),
    sheet     = 1,
    key       = "NUMERO_EMPLOYEUR",
    drop_cols = c("CLASSE_EFFECTIF")
  ),
  
  communes = list(
    path  = file.path(PROJECT_ROOT, "data/raw/excel/communes_ci.xlsx"),
    sheet = 1,
    key   = "CODE_COMMUNE"
  )
)
  

# ------------------------------------------------------------
# FONCTION: VÉRIFIER L'EXISTENCE DES CHEMINS
# ------------------------------------------------------------
check_paths <- function(create_missing = FALSE) {
  

  missing_dirs <- character()
  existing_dirs <- character()
  
  for (name in names(PATHS)) {
    path <- PATHS[[name]]
    
    # Ignorer les fichiers (contiennent une extension)
    if (grepl("\\.[a-zA-Z]+$", basename(path))) next
    
    if (dir.exists(path)) {
      existing_dirs <- c(existing_dirs, path)
    } else {
      missing_dirs <- c(missing_dirs, path)
      
      if (create_missing) {
        dir.create(path, recursive = TRUE, showWarnings = FALSE)
        message("  Créé: ", path)
      }
    }
  }
  
  return(list(
    existing = existing_dirs,
    missing  = missing_dirs
  ))
}

# ------------------------------------------------------------
# FONCTION: OBTENIR UN CHEMIN PAR NOM
# ------------------------------------------------------------
get_path <- function(name) {
  if (!name %in% names(PATHS)) {
    stop("Chemin inconnu: ", name, 
         "\nChemins disponibles: ", paste(names(PATHS), collapse = ", "))
  }
  return(PATHS[[name]])
}

# ------------------------------------------------------------
# FONCTION: LISTER LES FICHIERS DANS UN DOSSIER
# ------------------------------------------------------------
list_files_in <- function(path_name, pattern = NULL, full_names = TRUE) {
  path <- get_path(path_name)
  
  if (!dir.exists(path)) {
    warning("Le dossier n'existe pas: ", path)
    return(character())
  }
  
  list.files(path, pattern = pattern, full.names = full_names)
}

# ------------------------------------------------------------
# MESSAGE DE CONFIRMATION
# ------------------------------------------------------------
message("Chemins chargés depuis paths.R")