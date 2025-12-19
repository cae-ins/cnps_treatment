# ============================================================
# 00_INIT_PROJECT.R
# Initialise la structure du projet et le registry
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
# LIBRARIES
# ------------------------------------------------------------
library(dplyr)
library(readr)

# ------------------------------------------------------------
# CRÉER LES DOSSIERS MANQUANTS
# ------------------------------------------------------------
init_directories <- function() {
  message("=== Initialisation des dossiers ===")
  
  for (name in names(PATHS)) {
    path <- PATHS[[name]]
    
    # Ignorer les fichiers (contiennent un point dans le basename)
    if (grepl("\\.", basename(path))) next
    
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
      message("  Créé: ", path)
    } else {
      message("  Existe: ", path)
    }
  }
}

# ------------------------------------------------------------
# INITIALISER LE REGISTRY
# ------------------------------------------------------------
init_registry <- function(force_reset = FALSE) {
  message("\n=== Initialisation du Registry ===")
  
  registry_file <- PATHS$registry_file
  
  if (file.exists(registry_file) && !force_reset) {
    message("  Registry existant trouvé: ", registry_file)
    registry <- read_registry()
    message("  ", nrow(registry), " fichiers déjà enregistrés")
    return(registry)
  }
  
  # Créer un registry vide
  registry <- tibble(
    filename       = character(),
    source_file    = character(),
    hash_md5       = character(),
    processed_at   = character(),
    step_completed = character(),
    status         = character(),
    mois           = character(),
    annee          = character(),
    n_rows         = character(),
    n_cols         = character()
  )
  
  # Créer le dossier si nécessaire
  dir.create(dirname(registry_file), showWarnings = FALSE, recursive = TRUE)
  
  # Sauvegarder
  write_csv(registry, registry_file)
  message("  Nouveau registry créé: ", registry_file)
  
  return(registry)
}

# ------------------------------------------------------------
# FONCTIONS UTILITAIRES POUR LE REGISTRY
# ------------------------------------------------------------

# Calculer le hash MD5 d'un fichier
get_file_hash <- function(filepath) {
  if (!file.exists(filepath)) return(NA_character_)
  tools::md5sum(filepath)[[1]]
}

# Lire le registry (TOUT EN CHARACTER pour éviter les conflits de type)
read_registry <- function() {
  registry_file <- PATHS$registry_file
  
  if (!file.exists(registry_file)) {
    return(init_registry())
  }
  
  # Lire TOUTES les colonnes comme character
  registry <- read_csv(
    registry_file, 
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  )
  
  return(registry)
}

# Sauvegarder le registry (TOUT EN CHARACTER)
save_registry <- function(registry) {
  # S'assurer que toutes les colonnes sont character
  registry <- registry %>%
    mutate(across(everything(), as.character))
  
  write_csv(registry, PATHS$registry_file)
}

# Vérifier si un fichier doit être traité
needs_processing <- function(source_file, registry = NULL) {
  if (is.null(registry)) {
    registry <- read_registry()
  }
  
  filename <- basename(source_file)
  current_hash <- get_file_hash(source_file)
  
  # Chercher dans le registry
  existing <- registry %>% 
    filter(source_file == !!filename)
  
  if (nrow(existing) == 0) {
    # Nouveau fichier
    return(list(
      needs_processing = TRUE,
      reason = "new_file"
    ))
  }
  
  if (existing$hash_md5[1] != current_hash) {
    # Fichier modifié
    return(list(
      needs_processing = TRUE,
      reason = "modified"
    ))
  }
  
  if (existing$status[1] != "completed") {
    # Traitement incomplet
    return(list(
      needs_processing = TRUE,
      reason = "incomplete"
    ))
  }
  
  # Pas besoin de traitement
  return(list(
    needs_processing = FALSE,
    reason = "up_to_date"
  ))
}

# Mettre à jour le registry pour un fichier
update_registry <- function(filename, source_file, step_completed, 
                            status, n_rows = NA, n_cols = NA) {
  
  registry <- read_registry()
  
  # Extraire période
  period <- extract_period_from_filename(filename)
  
  # Calculer hash
  source_path <- file.path(PATHS$raw_excel, source_file)
  hash <- get_file_hash(source_path)
  
  # Nouvelle entrée - TOUT EN CHARACTER
  new_entry <- tibble(
    filename       = as.character(filename),
    source_file    = as.character(source_file),
    hash_md5       = as.character(hash),
    processed_at   = as.character(Sys.time()),
    step_completed = as.character(step_completed),
    status         = as.character(status),
    mois           = as.character(period$mois),
    annee          = as.character(period$annee),
    n_rows         = as.character(n_rows),
    n_cols         = as.character(n_cols)
  )
  
  # Supprimer l'ancienne entrée si elle existe
  registry <- registry %>%
    filter(filename != !!filename)
  
  # S'assurer que le registry est aussi tout en character
  registry <- registry %>%
    mutate(across(everything(), as.character))
  
  # Ajouter la nouvelle entrée
  registry <- bind_rows(registry, new_entry)
  
  # Sauvegarder
  save_registry(registry)
  
  return(invisible(registry))
}

# Obtenir les fichiers à traiter
get_files_to_process <- function() {
  registry <- read_registry()
  
  # Lister les fichiers Excel
  excel_files <- list.files(
    PATHS$raw_excel,
    pattern = "\\.xlsx$",
    full.names = TRUE
  )
  
  # Exclure les fichiers temporaires Excel
  excel_files <- excel_files[!grepl("^~\\$", basename(excel_files))]
  
  if (length(excel_files) == 0) {
    message("Aucun fichier Excel trouvé")
    return(list(new = character(), modified = character(), up_to_date = character()))
  }
  
  # Classifier les fichiers
  result <- list(new = character(), modified = character(), up_to_date = character())
  
  for (f in excel_files) {
    check <- needs_processing(f, registry)
    
    if (check$needs_processing) {
      if (check$reason == "new_file") {
        result$new <- c(result$new, f)
      } else {
        result$modified <- c(result$modified, f)
      }
    } else {
      result$up_to_date <- c(result$up_to_date, f)
    }
  }
  
  return(result)
}

# ------------------------------------------------------------
# EXÉCUTION PRINCIPALE
# ------------------------------------------------------------
if (sys.nframe() == 0 || interactive()) {
  init_directories()
  init_registry()
  
  files_status <- get_files_to_process()
  
  message("\n=== Statut des fichiers ===")
  message("  Nouveaux: ", length(files_status$new))
  message("  Modifiés: ", length(files_status$modified))
  message("  À jour:   ", length(files_status$up_to_date))
}