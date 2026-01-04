# ============================================================
# MODEL_REGISTRY.R
# Gestion du registre des modèles estimés
# Chemin: scripts/pipeline/model_registry.R
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

# ============================================================
# FONCTION UTILITAIRE: EXTRACTION SÉCURISÉE
# Retourne une valeur scalaire ou NA, jamais NULL ou vecteur vide
# ============================================================

safe_extract <- function(expr, default = NA_real_) {
  result <- tryCatch(expr, error = function(e) NULL)
  
  # Vérifier que le résultat est valide

if (is.null(result) || length(result) == 0) {
    return(default)
  }
  
  # Si c'est un vecteur, prendre le premier élément
  if (length(result) > 1) {
    result <- result[1]
  }
  
  # Convertir au type approprié
  if (is.numeric(default)) {
    return(as.numeric(result))
  } else if (is.integer(default)) {
    return(as.integer(result))
  } else if (is.character(default)) {
    return(as.character(result))
  }
  
  return(result)
}

# Enregistrer un modèle
register_model <- function(model, model_name, diagnostics = NULL) {
  
  model_id <- generate_model_id(model_name)
  session_id <- tryCatch(get_current_session(), error = function(e) NULL)
  
  # Déterminer les chemins de sauvegarde
  if (!is.null(session_id) && session_id != "") {
    base_path <- file.path(get_session_dir(), "models")
  } else {
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
  diagnostics_file <- ""
  if (!is.null(diagnostics)) {
    diag_path <- gsub("/fitted", "/diagnostics", base_path)
    ensure_dir(diag_path)
    diagnostics_file <- file.path(diag_path, paste0(model_id, "_diagnostics.rds"))
    saveRDS(diagnostics, diagnostics_file)
  }
  
  # Extraire les métriques du modèle de manière sécurisée
  n_obs_val <- safe_extract(nobs(model), default = NA_integer_)
  
  # Pour n_params, gérer le cas où coef() retourne NULL ou vecteur vide
  n_params_val <- safe_extract({
    coefs <- coef(model)
    if (is.null(coefs) || length(coefs) == 0) NA_integer_ else length(coefs)
  }, default = NA_integer_)
  
  aic_val <- safe_extract(AIC(model), default = NA_real_)
  bic_val <- safe_extract(BIC(model), default = NA_real_)
  
  # Extraire AUC depuis diagnostics si disponible
  auc_val <- NA_real_
  if (!is.null(diagnostics) && !is.null(diagnostics$auc)) {
    auc_val <- safe_extract(diagnostics$auc, default = NA_real_)
  }
  
  # Mettre à jour le registre
  registry <- init_model_registry()
  
  # Créer new_entry avec des valeurs garanties scalaires
  new_entry <- data.frame(
    model_id = as.character(model_id),
    model_name = as.character(model_name),
    model_type = as.character(class(model)[1]),
    session_id = as.character(ifelse(is.null(session_id), "", session_id)),
    estimated_at = as.character(Sys.time()),
    n_obs = as.integer(n_obs_val),
    n_params = as.integer(n_params_val),
    aic = as.numeric(aic_val),
    bic = as.numeric(bic_val),
    auc = as.numeric(auc_val),
    file_path = as.character(model_file),
    diagnostics_path = as.character(diagnostics_file),
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
  
  if (is.na(entry$diagnostics_path) || entry$diagnostics_path == "" || !file.exists(entry$diagnostics_path)) {
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

# ============================================================
# FONCTION: PRINT_MODEL_SUMMARY
# Affiche le résumé d'un modèle enregistré
# ============================================================

print_model_summary <- function(model_id) {
  
  registry <- init_model_registry()
  entry <- registry[registry$model_id == model_id, ]
  
  if (nrow(entry) == 0) {
    message("Modèle non trouvé: ", model_id)
    return(invisible(NULL))
  }
  
  message("\n", strrep("=", 50))
  message("MODÈLE: ", entry$model_id)
  message(strrep("=", 50))
  message("Nom: ", entry$model_name)
  message("Type: ", entry$model_type)
  message("Session: ", entry$session_id)
  message("Estimé le: ", entry$estimated_at)
  message("Observations: ", entry$n_obs)
  message("Paramètres: ", entry$n_params)
  message("AIC: ", round(entry$aic, 2))
  message("BIC: ", round(entry$bic, 2))
  if (!is.na(entry$auc)) {
    message("AUC: ", round(entry$auc, 3))
  }
  message("Fichier: ", entry$file_path)
  message("Status: ", entry$status)
  
  return(invisible(entry))
}

message("Registre des modèles chargé")