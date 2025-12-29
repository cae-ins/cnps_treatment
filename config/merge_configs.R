# ============================================================
# MERGE_CONFIGS.R
# Configuration des fusions avec les bases de référence
# ============================================================

# ============================================================
# CONFIGURATION DES FICHIERS DE RÉFÉRENCE POUR LES MERGES
# ============================================================

MERGE_CONFIGS <- list(
  
  # ----------------------------------------------------------
  # 1. SECTEURS D'ACTIVITÉ (fusion par clé exacte)
  # ----------------------------------------------------------
  secteurs = list(
    enabled = TRUE,
    name = "Secteurs d'activité",
    description = "Codes et libellés des secteurs d'activité économique",
    
    # Fichier source
    file = list(
      path = file.path(PROJECT_ROOT, "data/referentiels/CNPS_M_2024.xlsx"),
      sheet = 1,
      skip = 0
    ),
    
    # Type de fusion: "exact" ou "fuzzy"
    match_type = "exact",
    
    # Configuration de la fusion exacte
    merge = list(
      key_master = "NUMERO_EMPLOYEUR",
      key_ref = "NUMERO_EMPLOYEUR",
      type = "left"
    ),
    
    # Colonnes à conserver du fichier de référence
    keep_cols = c(
      "SECTEUR_ACTIVITE_COD"
    ),
    
    # Colonnes à supprimer (si présentes)
    drop_cols = c("CLASSE_EFFECTIF"),
    
    # Colonnes à renommer
    rename_cols = list(),
    
    # Variable de vérification
    check_var = "SECTEUR_ACTIVITE_COD",
    
    # Ordre d'exécution
    order = 1
  )
#   ,
  # ----------------------------------------------------------
  # 2. PROFESSIONS (fusion par similarité)
  # ----------------------------------------------------------
#   professions = list(
#     enabled = TRUE,
#     name = "Professions",
#     description = "Nomenclature des professions et CSP (matching fuzzy)",
    
#     file = list(
#       path = file.path(PROJECT_ROOT, "data/referentiels/nomenclature_professions.xlsx"),
#       sheet = 1,
#       skip = 0
#     ),
    
#     # Type de fusion: fuzzy (similarité de chaînes)
#     match_type = "fuzzy",
    
#     # Configuration de la fusion fuzzy
#     fuzzy = list(
#       # Colonnes à comparer
#       col_master = "PROFESSION",
#       col_ref = "PROFESSION_LIB",
      
#       # Méthode de similarité: "jw" (Jaro-Winkler), "lv" (Levenshtein), 
#       # "cosine", "jaccard", "osa", "lcs"
#       method = "jw",
      
#       # Seuil de similarité minimum (0-1)
#       threshold = 0.85,
      
#       # Nombre maximum de candidats à évaluer par observation
#       max_candidates = 5,
      
#       # Prétraitement des chaînes
#       preprocessing = list(
#         lowercase = TRUE,
#         remove_accents = TRUE,
#         remove_punctuation = TRUE,
#         trim_whitespace = TRUE,
#         remove_stopwords = FALSE
#       ),
      
#       # Conserver le score de similarité
#       keep_similarity_score = TRUE,
      
#       # Nom de la colonne du score
#       similarity_col = "PROFESSION_SIMILARITY"
#     ),
    
#     keep_cols = c(
#       "PROF_CODE",
#       "PROF_LIB",
#       "CSP_CODE",
#       "CSP_LIB",
#       "GRANDE_CSP_CODE",
#       "GRANDE_CSP_LIB"
#     ),
    
#     drop_cols = NULL,
#     rename_cols = list(),
#     check_var = "PROF_CODE",
#     order = 2
#   ),
  
  # ----------------------------------------------------------
  # 3. COMMUNES (fusion par similarité)
  # ----------------------------------------------------------
#   communes = list(
#     enabled = TRUE,
#     name = "Communes",
#     description = "Référentiel géographique des communes (matching fuzzy)",
    
#     file = list(
#       path = file.path(PROJECT_ROOT, "data/referentiels/communes_ci.xlsx"),
#       sheet = 1,
#       skip = 0
#     ),
    
#     match_type = "fuzzy",
    
#     fuzzy = list(
#       col_master = "COMMUNE",
#       col_ref = "COMMUNE_LIB",
#       method = "jw",
#       threshold = 0.80,
#       max_candidates = 10,
#       preprocessing = list(
#         lowercase = TRUE,
#         remove_accents = TRUE,
#         remove_punctuation = TRUE,
#         trim_whitespace = TRUE,
#         remove_stopwords = FALSE
#       ),
#       keep_similarity_score = TRUE,
#       similarity_col = "COMMUNE_SIMILARITY"
#     ),
    
#     keep_cols = c(
#       "COMMUNE_CODE",
#       "COMMUNE_LIB_NORM",
#       "DEPARTEMENT_CODE",
#       "DEPARTEMENT_LIB",
#       "REGION_CODE",
#       "REGION_LIB",
#       "DISTRICT_CODE",
#       "DISTRICT_LIB"
#     ),
    
#     drop_cols = NULL,
#     rename_cols = list(),
#     check_var = "REGION_CODE",
#     order = 3
#   ),
  
  # ----------------------------------------------------------
  # 4. RAISON SOCIALE → SECTEUR (fusion par similarité)
  # ----------------------------------------------------------
#   raison_sociale_secteur = list(
#     enabled = FALSE,
#     name = "Raison sociale → Secteur",
#     description = "Déduction du secteur depuis la raison sociale",
    
#     file = list(
#       path = file.path(PROJECT_ROOT, "data/referentiels/mots_cles_secteurs.xlsx"),
#       sheet = 1,
#       skip = 0
#     ),
    
#     match_type = "fuzzy",
    
#     fuzzy = list(
#       col_master = "RAISON_SOCIALE",
#       col_ref = "MOTS_CLES",
#       method = "cosine",  # Cosine similarity pour les mots-clés
#       threshold = 0.70,
#       max_candidates = 3,
#       preprocessing = list(
#         lowercase = TRUE,
#         remove_accents = TRUE,
#         remove_punctuation = TRUE,
#         trim_whitespace = TRUE,
#         remove_stopwords = TRUE  # Supprimer "SARL", "SA", etc.
#       ),
#       keep_similarity_score = TRUE,
#       similarity_col = "RAISON_SECTEUR_SIMILARITY"
#     ),
    
#     keep_cols = c(
#       "SECTEUR_DEDUIT_CODE",
#       "SECTEUR_DEDUIT_LIB"
#     ),
    
#     drop_cols = NULL,
#     rename_cols = list(),
#     check_var = "SECTEUR_DEDUIT_CODE",
#     order = 4
#   ),
  
  # ----------------------------------------------------------
  # 5. AGENCES CNPS (fusion exacte)
  # ----------------------------------------------------------
#   agences = list(
#     enabled = FALSE,
#     name = "Agences CNPS",
#     description = "Référentiel des agences CNPS",
    
#     file = list(
#       path = file.path(PROJECT_ROOT, "data/referentiels/agences_cnps.xlsx"),
#       sheet = 1,
#       skip = 0
#     ),
    
#     match_type = "exact",
    
#     merge = list(
#       key_master = "LIBELLE_AGENCE",
#       key_ref = "CODE_AGENCE",
#       type = "left"
#     ),
    
#     keep_cols = c(
#       "AGENCE_LIB",
#       "DIRECTION_REGIONALE",
#       "ZONE_GEOGRAPHIQUE"
#     ),
    
#     drop_cols = NULL,
#     rename_cols = list(),
#     check_var = "DIRECTION_REGIONALE",
#     order = 5
#   ),
  
  # ----------------------------------------------------------
  # 6. CONVENTIONS COLLECTIVES (fusion exacte)
  # ----------------------------------------------------------
#   conventions = list(
#     enabled = FALSE,
#     name = "Conventions collectives",
#     description = "Référentiel des conventions collectives",
    
#     file = list(
#       path = file.path(PROJECT_ROOT, "data/referentiels/conventions_collectives.xlsx"),
#       sheet = 1,
#       skip = 0
#     ),
    
#     match_type = "exact",
    
#     merge = list(
#       key_master = "CODE_CONVENTION",
#       key_ref = "CODE_CONVENTION",
#       type = "left"
#     ),
    
#     keep_cols = c(
#       "CONVENTION_LIB",
#       "SECTEUR_CONVENTION",
#       "DATE_SIGNATURE"
#     ),
    
#     drop_cols = NULL,
#     rename_cols = list(),
#     check_var = "CONVENTION_LIB",
#     order = 6
#   ),
  
  # ----------------------------------------------------------
  # 7. NIVEAUX D'ÉTUDE (fusion exacte ou fuzzy)
  # ----------------------------------------------------------
#   niveaux_etude = list(
#     enabled = FALSE,
#     name = "Niveaux d'étude",
#     description = "Référentiel des niveaux d'étude",
    
#     file = list(
#       path = file.path(PROJECT_ROOT, "data/referentiels/niveaux_etude.xlsx"),
#       sheet = 1,
#       skip = 0
#     ),
    
#     match_type = "fuzzy",
    
#     fuzzy = list(
#       col_master = "NIVEAU_ETUDE",
#       col_ref = "NIVEAU_ETUDE_LIB",
#       method = "jw",
#       threshold = 0.75,
#       max_candidates = 5,
#       preprocessing = list(
#         lowercase = TRUE,
#         remove_accents = TRUE,
#         remove_punctuation = TRUE,
#         trim_whitespace = TRUE,
#         remove_stopwords = FALSE
#       ),
#       keep_similarity_score = TRUE,
#       similarity_col = "NIVEAU_ETUDE_SIMILARITY"
#     ),
    
#     keep_cols = c(
#       "NIVEAU_ETUDE_CODE",
#       "NIVEAU_ETUDE_LIB_NORM",
#       "NIVEAU_AGREGAT",
#       "ANNEES_ETUDE"
#     ),
    
#     drop_cols = NULL,
#     rename_cols = list(),
#     check_var = "NIVEAU_ETUDE_CODE",
#     order = 7
#   )
 )

# ============================================================
# CONFIGURATION GLOBALE DU FUZZY MATCHING
# ============================================================

FUZZY_MATCHING_CONFIG <- list(
  
  # Méthodes disponibles
  available_methods = c("jw", "lv", "osa", "lcs", "cosine", "jaccard"),
  
  # Seuils par défaut
  default_threshold = 0.80,
  
  # Mots vides à supprimer (pour le prétraitement)
  stopwords_fr = c(
    # "le", "la", "les", "de", "du", "des", "et", "en", "un", "une",
    # "sa", "sarl", "sas", "eurl", "sci", "scp", "gie", "earl",
    # "ets", "etablissement", "etablissements", "societe", "entreprise",
    # "cie", "compagnie", "groupe", "holding"
  ),
  
  # Seuils de qualité du matching
  quality_thresholds = list(
    excellent = 0.95,
    good = 0.85,
    acceptable = 0.75,
    poor = 0.60
  ),
  
  # Limites de performance
  max_comparisons_per_batch = 10000,
  use_blocking = TRUE,  # Utiliser le blocking pour accélérer
  blocking_threshold = 0.3  # Seuil pour le blocking initial
)

# ============================================================
# FONCTIONS UTILITAIRES
# ============================================================

#' Obtenir les merges activés triés par ordre
get_enabled_merges <- function() {
  enabled <- MERGE_CONFIGS[sapply(MERGE_CONFIGS, function(x) isTRUE(x$enabled))]
  orders <- sapply(enabled, function(x) x$order)
  enabled <- enabled[order(orders)]
  return(enabled)
}

#' Obtenir la configuration d'un merge spécifique
get_merge_config <- function(merge_name) {
  if (merge_name %in% names(MERGE_CONFIGS)) {
    return(MERGE_CONFIGS[[merge_name]])
  }
  return(NULL)
}

#' Vérifier si tous les fichiers de référence existent
check_reference_files <- function() {
  enabled <- get_enabled_merges()
  missing <- character()
  existing <- character()
  
  for (merge_name in names(enabled)) {
    config <- enabled[[merge_name]]
    file_path <- config$file$path
    
    if (file.exists(file_path)) {
      existing <- c(existing, file_path)
    } else {
      missing <- c(missing, file_path)
    }
  }
  
  return(list(
    valid = length(missing) == 0,
    existing = existing,
    missing = missing
  ))
}

#' Activer ou désactiver un merge
enable_merge <- function(merge_name, enabled = TRUE) {
  if (merge_name %in% names(MERGE_CONFIGS)) {
    MERGE_CONFIGS[[merge_name]]$enabled <<- enabled
    message("Merge '", merge_name, "' ", 
            ifelse(enabled, "activé", "désactivé"))
  } else {
    warning("Merge inconnu: ", merge_name)
  }
}

#' Lister les merges disponibles
list_available_merges <- function() {
  df <- data.frame(
    nom = names(MERGE_CONFIGS),
    description = sapply(MERGE_CONFIGS, function(x) x$name),
    type = sapply(MERGE_CONFIGS, function(x) x$match_type),
    enabled = sapply(MERGE_CONFIGS, function(x) isTRUE(x$enabled)),
    ordre = sapply(MERGE_CONFIGS, function(x) x$order),
    fichier_existe = sapply(MERGE_CONFIGS, function(x) file.exists(x$file$path)),
    stringsAsFactors = FALSE
  )
  
  df <- df[order(df$ordre), ]
  rownames(df) <- NULL
  
  return(df)
}

#' Obtenir les merges par type
get_merges_by_type <- function(type = c("exact", "fuzzy")) {
  type <- match.arg(type)
  filtered <- MERGE_CONFIGS[sapply(MERGE_CONFIGS, function(x) x$match_type == type)]
  return(filtered)
}

message("Configuration des merges chargée depuis merge_configs.R")
message("  - ", length(MERGE_CONFIGS), " merges configurés")
message("  - ", sum(sapply(MERGE_CONFIGS, function(x) x$match_type == "exact")), " merges exacts")
message("  - ", sum(sapply(MERGE_CONFIGS, function(x) x$match_type == "fuzzy")), " merges fuzzy")
message("  - ", sum(sapply(MERGE_CONFIGS, function(x) isTRUE(x$enabled))), " merges activés")