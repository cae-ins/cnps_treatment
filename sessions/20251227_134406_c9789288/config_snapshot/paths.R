# ============================================================
# PATHS.R
# Définition centralisée de tous les chemins du projet
# ============================================================

PROJECT_ROOT <- "/home/migone/CNPS_TREATMENT_PROJECT"

PATHS <- list(
  # Données brutes
  raw = file.path(PROJECT_ROOT, "data/raw"),
  raw_excel = file.path(PROJECT_ROOT, "data/raw/excel"),
  
  # Données traitées
  processed = file.path(PROJECT_ROOT, "data/processed"),
  processed_dta = file.path(PROJECT_ROOT, "data/processed/dta"),
  registry = file.path(PROJECT_ROOT, "data/processed/registry"),
  registry_file = file.path(PROJECT_ROOT, "data/processed/registry/processed_index.csv"),
  
  # Données nettoyées
  cleaned = file.path(PROJECT_ROOT, "data/cleaned"),
  cleaned_individual = file.path(PROJECT_ROOT, "data/cleaned/individual"),
  cleaned_firm_time = file.path(PROJECT_ROOT, "data/cleaned/firm_time"),
  cleaned_analytical = file.path(PROJECT_ROOT, "data/cleaned/analytical"),
  cleaned_imputations = file.path(PROJECT_ROOT, "data/cleaned/imputations"),
  cleaned_monthly = file.path(PROJECT_ROOT, "data/cleaned/monthly"),
  cleaned_final = file.path(PROJECT_ROOT, "data/cleaned/final"),
  
  # Archives
  archive = file.path(PROJECT_ROOT, "data/archive"),
  
  # Modèles
  models = file.path(PROJECT_ROOT, "models"),
  models_declaration = file.path(PROJECT_ROOT, "models/declaration"),
  models_declaration_fitted = file.path(PROJECT_ROOT, "models/declaration/fitted"),
  models_declaration_diagnostics = file.path(PROJECT_ROOT, "models/declaration/diagnostics"),
  models_declaration_summaries = file.path(PROJECT_ROOT, "models/declaration/summaries"),
  models_imputation = file.path(PROJECT_ROOT, "models/imputation"),
  models_imputation_fitted = file.path(PROJECT_ROOT, "models/imputation/fitted"),
  models_imputation_diagnostics = file.path(PROJECT_ROOT, "models/imputation/diagnostics"),
  models_imputation_summaries = file.path(PROJECT_ROOT, "models/imputation/summaries"),
  models_registry = file.path(PROJECT_ROOT, "models/registry"),
  model_registry_file = file.path(PROJECT_ROOT, "models/registry/model_registry.csv"),
  
  # Sessions
  sessions = file.path(PROJECT_ROOT, "sessions"),
  sessions_registry = file.path(PROJECT_ROOT, "sessions/registry"),
  sessions_index_file = file.path(PROJECT_ROOT, "sessions/registry/sessions_index.csv"),
  sessions_active = file.path(PROJECT_ROOT, "sessions/active"),
  current_session_file = file.path(PROJECT_ROOT, "sessions/active/current_session.txt"),
  
  # Sorties
  output = file.path(PROJECT_ROOT, "output"),
  indicators = file.path(PROJECT_ROOT, "output/indicators"),
  analytical_reports = file.path(PROJECT_ROOT, "output/analytical_reports"),
  inconsistencies = file.path(PROJECT_ROOT, "output/inconsistencies"),
  
  # Scripts
  scripts = file.path(PROJECT_ROOT, "scripts"),
  scripts_ingestion = file.path(PROJECT_ROOT, "scripts/ingestion"),
  scripts_preparation = file.path(PROJECT_ROOT, "scripts/preparation"),
  scripts_structuring = file.path(PROJECT_ROOT, "scripts/structuring"),
  scripts_modeling = file.path(PROJECT_ROOT, "scripts/modeling"),
  scripts_diagnostics = file.path(PROJECT_ROOT, "scripts/diagnostics"),
  scripts_estimation = file.path(PROJECT_ROOT, "scripts/estimation"),
  scripts_pipeline = file.path(PROJECT_ROOT, "scripts/pipeline"),
  
  # Référentiels (bases pour les merges)
  referentiels = file.path(PROJECT_ROOT, "data/referentiels"),
  
  # Config
  config = file.path(PROJECT_ROOT, "config")
)

EXTERNAL_FILES <- list(
  sector_codes = list(
    path = file.path(PROJECT_ROOT, "data/raw/CNPS_M_2024.xlsx"),
    sheet = 1,
    key = "NUMERO_EMPLOYEUR",
    drop_cols = c("CLASSE_EFFECTIF")
  )
)

get_path <- function(name) {
  if (!name %in% names(PATHS)) {
    stop("Chemin inconnu: ", name)
  }
  return(PATHS[[name]])
}

message("Chemins chargés depuis paths.R")