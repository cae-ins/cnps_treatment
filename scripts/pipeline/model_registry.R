# ============================================================
# MODEL_REGISTRY.R
# Gestion du registre des modèles estimés
# ============================================================

library(digest)

# Initialiser le registre des modèles
init_model_registry <- function(force_reset = FALSE) {
  
  ensure_dir(PATHS$models_registry)
  registry_file <- PATHS$model_registry_file
  
  if (file.exists(registry_file) && !force_reset) {
    return(read.csv(registry_file, stringsAsFactors = FALSE))
  }
  
  registry <- data.frame(
    model_id = character(),
    model_name = character(),
    model_type = character(),
    session_id = character(),
    estimated_at = character(),
    n_obs = integer(),
    n_params = integer(),
    aic = numeric(),
    bic = numeric(),
    auc = numeric(),
    file_path = character(),
    diagnostics_path = character(),
    status = character(),
    stringsAsFactors = FALSE
  )
  
  write.csv(registry, registry_file, row.names = FALSE)
  return(registry)
}

# Générer un ID de modèle
generate_model_id <- function(model_name) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  hash <- substr(digest(Sys.time()), 1, 6)
  paste0(model_name, "_", timestamp, "_", hash)
}

# Enregistrer un modèle
register_model <- function(model, model_name, diagnostics = NULL) {
  
  model_id <- generate_model_id(model_name)
  session_id <- get_current_session()
  
  # Déterminer les chemins de sauvegarde
  if (!is.null(session_id)) {
    base_path <- file.path(get_session_dir(), "models")
  } else {
    model_spec <- MODEL_SPECS[[model_name]]
    if (grepl("declaration", model_name)) {
      base_path <- PATHS$models_declaration_fitted
    } else {
      base_path <- PATHS$models_imputation_fitted
    }
  }
  
  ensure_dir(base_path)
  
  # Sauvegarder le modèle
  model_file <- file.path(base_path, paste0(model_id, ".rds"))
  saveRDS(model, model_file)
  
  # Sauvegarder les diagnostics
  diagnostics_file <- NA
  if (!is.null(diagnostics)) {
    diag_path <- gsub("/fitted", "/diagnostics", base_path)
    ensure_dir(diag_path)
    diagnostics_file <- file.path(diag_path, paste0(model_id, "_diagnostics.rds"))
    saveRDS(diagnostics, diagnostics_file)
  }
  
  # Extraire les métriques du modèle
  n_obs <- tryCatch(nobs(model), error = function(e) NA)
  n_params <- tryCatch(length(coef(model)), error = function(e) NA)
  aic_val <- tryCatch(AIC(model), error = function(e) NA)
  bic_val <- tryCatch(BIC(model), error = function(e) NA)
  auc_val <- if (!is.null(diagnostics)) diagnostics$auc else NA
  
  # Mettre à jour le registre
  registry <- init_model_registry()
  
  new_entry <- data.frame(
    model_id = model_id,
    model_name = model_name,
    model_type = class(model)[1],
    session_id = ifelse(is.null(session_id), "", session_id),
    estimated_at = as.character(Sys.time()),
    n_obs = n_obs,
    n_params = n_params,
    aic = aic_val,
    bic = bic_val,
    auc = auc_val,
    file_path = model_file,
    diagnostics_path = ifelse(is.na(diagnostics_file), "", diagnostics_file),
    status = "active",
    stringsAsFactors = FALSE
  )
  
  registry <- rbind(registry, new_entry)
  write.csv(registry, PATHS$model_registry_file, row.names = FALSE)
  
  message("Modèle enregistré: ", model_id)
  return(model_id)
}

# Charger un modèle depuis le registre
load_model <- function(model_id) {
  
  registry <- init_model_registry()
  entry <- registry[registry$model_id == model_id, ]
  
  if (nrow(entry) == 0) {
    stop("Modèle non trouvé: ", model_id)
  }
  
  if (!file.exists(entry$file_path)) {
    stop("Fichier de modèle manquant: ", entry$file_path)
  }
  
  readRDS(entry$file_path)
}

# Charger les diagnostics d'un modèle
load_model_diagnostics <- function(model_id) {
  
  registry <- init_model_registry()
  entry <- registry[registry$model_id == model_id, ]
  
  if (nrow(entry) == 0) {
    stop("Modèle non trouvé: ", model_id)
  }
  
  if (entry$diagnostics_path == "" || !file.exists(entry$diagnostics_path)) {
    return(NULL)
  }
  
  readRDS(entry$diagnostics_path)
}

# Obtenir le dernier modèle d'un type
get_latest_model <- function(model_name) {
  
  registry <- init_model_registry()
  entries <- registry[registry$model_name == model_name & registry$status == "active", ]
  
  if (nrow(entries) == 0) {
    return(NULL)
  }
  
  entries <- entries[order(entries$estimated_at, decreasing = TRUE), ]
  return(entries$model_id[1])
}

# Lister les modèles
list_models <- function(model_name = NULL, session_id = NULL) {
  
  registry <- init_model_registry()
  
  if (!is.null(model_name)) {
    registry <- registry[registry$model_name == model_name, ]
  }
  
  if (!is.null(session_id)) {
    registry <- registry[registry$session_id == session_id, ]
  }
  
  return(registry)
}

message("Registre des modèles chargé")