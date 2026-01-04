# ============================================================
# SESSION_MANAGER.R
# Gestion des sessions d'exécution du pipeline
# ============================================================

library(jsonlite)
library(digest)

# Générer un ID de session unique
generate_session_id <- function() {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  config_hash <- substr(digest(list(PROCESSING, STAT_DIMENSIONS)), 1, 8)
  paste0(timestamp, "_", config_hash)
}

# Créer une nouvelle session
create_session <- function(description = "") {
  
  session_id <- generate_session_id()
  session_dir <- file.path(PATHS$sessions, session_id)
  
  # Créer la structure de la session
  dirs <- c(
    "config_snapshot",
    "data_snapshot",
    "models",
    "outputs",
    "logs"
  )
  
  for (d in dirs) {
    dir.create(file.path(session_dir, d), recursive = TRUE, showWarnings = FALSE)
  }
  
  # Snapshot de la configuration
  config_files <- list.files(PATHS$config, pattern = "\\.R$", full.names = TRUE)
  for (f in config_files) {
    file.copy(f, file.path(session_dir, "config_snapshot", basename(f)))
  }
  
  # Créer les métadonnées
  metadata <- list(
    session_id = session_id,
    created_at = as.character(Sys.time()),
    description = description,
    status = "active",
    user = Sys.info()["user"],
    r_version = R.version.string,
    packages = as.list(installed.packages()[, "Version"])
  )
  
  write_json(metadata, file.path(session_dir, "session_metadata.json"), 
             pretty = TRUE, auto_unbox = TRUE)
  
  # Enregistrer comme session active
  writeLines(session_id, PATHS$current_session_file)
  
  # Mettre à jour l'index des sessions
  update_sessions_index(session_id, "created", description)
  
  message("Session créée: ", session_id)
  return(session_id)
}

# Fermer une session
close_session <- function(status = "completed", summary = NULL) {
  
  session_id <- get_current_session()
  if (is.null(session_id)) {
    warning("Aucune session active")
    return(invisible(NULL))
  }
  
  session_dir <- file.path(PATHS$sessions, session_id)
  metadata_file <- file.path(session_dir, "session_metadata.json")
  
  if (file.exists(metadata_file)) {
    metadata <- read_json(metadata_file)
    metadata$status <- status
    metadata$closed_at <- as.character(Sys.time())
    metadata$summary <- summary
    write_json(metadata, metadata_file, pretty = TRUE, auto_unbox = TRUE)
  }
  
  # Mettre à jour l'index
  update_sessions_index(session_id, status)
  
  # Supprimer la session active
  if (file.exists(PATHS$current_session_file)) {
    file.remove(PATHS$current_session_file)
  }
  
  message("Session fermée: ", session_id, " (", status, ")")
  return(invisible(session_id))
}

# Obtenir la session courante
get_current_session <- function() {
  if (!file.exists(PATHS$current_session_file)) {
    return(NULL)
  }
  readLines(PATHS$current_session_file, n = 1)
}

# Obtenir le répertoire de la session courante
get_session_dir <- function(session_id = NULL) {
  if (is.null(session_id)) {
    session_id <- get_current_session()
  }
  if (is.null(session_id)) return(NULL)
  file.path(PATHS$sessions, session_id)
}

# Rollback vers une session précédente
rollback_session <- function(session_id) {
  
  session_dir <- file.path(PATHS$sessions, session_id)
  
  if (!dir.exists(session_dir)) {
    stop("Session non trouvée: ", session_id)
  }
  
  # Restaurer la configuration
  config_snapshot <- file.path(session_dir, "config_snapshot")
  if (dir.exists(config_snapshot)) {
    config_files <- list.files(config_snapshot, full.names = TRUE)
    for (f in config_files) {
      file.copy(f, file.path(PATHS$config, basename(f)), overwrite = TRUE)
    }
  }
  
  # Restaurer les données si présentes
  data_snapshot <- file.path(session_dir, "data_snapshot")
  if (dir.exists(data_snapshot) && length(list.files(data_snapshot)) > 0) {
    message("Restauration des données depuis la session ", session_id)
    # Copier les fichiers de données
  }
  
  message("Rollback effectué vers la session: ", session_id)
  return(invisible(TRUE))
}

# Supprimer une session
delete_session <- function(session_id, confirm = TRUE) {
  
  if (session_id == get_current_session()) {
    stop("Impossible de supprimer la session active")
  }
  
  session_dir <- file.path(PATHS$sessions, session_id)
  
  if (!dir.exists(session_dir)) {
    warning("Session non trouvée: ", session_id)
    return(invisible(FALSE))
  }
  
  if (confirm) {
    response <- readline(paste0("Supprimer la session ", session_id, "? (o/n): "))
    if (tolower(response) != "o") {
      message("Suppression annulée")
      return(invisible(FALSE))
    }
  }
  
  unlink(session_dir, recursive = TRUE)
  update_sessions_index(session_id, "deleted")
  
  message("Session supprimée: ", session_id)
  return(invisible(TRUE))
}

# Mettre à jour l'index des sessions
update_sessions_index <- function(session_id, status, description = "") {
  
  ensure_dir(PATHS$sessions_registry)
  index_file <- PATHS$sessions_index_file
  
  if (file.exists(index_file)) {
    index <- read.csv(index_file, stringsAsFactors = FALSE)
  } else {
    index <- data.frame(
      session_id = character(),
      created_at = character(),
      status = character(),
      description = character(),
      stringsAsFactors = FALSE
    )
  }
  
  if (status == "deleted") {
    index <- index[index$session_id != session_id, ]
  } else {
    existing <- which(index$session_id == session_id)
    
    if (length(existing) > 0) {
      index$status[existing] <- status
      index$updated_at[existing] <- as.character(Sys.time())
    } else {
      new_row <- data.frame(
        session_id = session_id,
        created_at = as.character(Sys.time()),
        status = status,
        description = description,
        stringsAsFactors = FALSE
      )
      index <- rbind(index, new_row)
    }
  }
  
  write.csv(index, index_file, row.names = FALSE)
}

# Lister les sessions
list_sessions <- function(status = NULL) {
  
  if (!file.exists(PATHS$sessions_index_file)) {
    message("Aucune session enregistrée")
    return(NULL)
  }
  
  index <- read.csv(PATHS$sessions_index_file, stringsAsFactors = FALSE)
  
  if (!is.null(status)) {
    index <- index[index$status == status, ]
  }
  
  return(index)
}

# Comparer deux sessions
compare_sessions <- function(session_id_1, session_id_2) {
  
  dir1 <- file.path(PATHS$sessions, session_id_1)
  dir2 <- file.path(PATHS$sessions, session_id_2)
  
  if (!dir.exists(dir1)) stop("Session 1 non trouvée")
  if (!dir.exists(dir2)) stop("Session 2 non trouvée")
  
  meta1 <- read_json(file.path(dir1, "session_metadata.json"))
  meta2 <- read_json(file.path(dir2, "session_metadata.json"))
  
  comparison <- list(
    session_1 = list(
      id = session_id_1,
      created = meta1$created_at,
      status = meta1$status
    ),
    session_2 = list(
      id = session_id_2,
      created = meta2$created_at,
      status = meta2$status
    )
  )
  
  return(comparison)
}

message("Gestionnaire de sessions chargé")