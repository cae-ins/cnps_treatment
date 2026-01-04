# ============================================================
# 05_MERGE_REFERENCES.R
# Fusion avec les bases de référence (exacte et fuzzy)
# ============================================================

library(dplyr)
library(haven)
library(readxl)
library(stringr)
library(stringi)  # Pour la manipulation de chaînes avancée

# Installer stringdist si nécessaire
if (!requireNamespace("stringdist", quietly = TRUE)) {
  install.packages("stringdist")
}
library(stringdist)

# ============================================================
# FONCTIONS DE PRÉTRAITEMENT DES CHAÎNES
# ============================================================

#' Supprimer les accents d'une chaîne
#' @param x Vecteur de chaînes
#' @return Vecteur sans accents
remove_accents <- function(x) {
  stri_trans_general(x, "Latin-ASCII")
}

#' Prétraiter une chaîne pour le matching fuzzy
#' @param x Vecteur de chaînes
#' @param config Configuration de prétraitement
#' @return Vecteur prétraité
preprocess_string <- function(x, config = NULL) {
  
  if (is.null(config)) {
    config <- list(
      lowercase = TRUE,
      remove_accents = TRUE,
      remove_punctuation = TRUE,
      trim_whitespace = TRUE,
      remove_stopwords = FALSE
    )
  }
  
  # Convertir en caractère
  x <- as.character(x)
  
  # Remplacer NA par chaîne vide
  x[is.na(x)] <- ""
  
  # Minuscules
  if (isTRUE(config$lowercase)) {
    x <- tolower(x)
  }
  
  # Supprimer les accents
  if (isTRUE(config$remove_accents)) {
    x <- remove_accents(x)
  }
  
  # Supprimer la ponctuation
  if (isTRUE(config$remove_punctuation)) {
    x <- str_replace_all(x, "[[:punct:]]", " ")
  }
  
  # Supprimer les mots vides
  if (isTRUE(config$remove_stopwords)) {
    stopwords <- FUZZY_MATCHING_CONFIG$stopwords_fr
    for (sw in stopwords) {
      x <- str_replace_all(x, paste0("\\b", sw, "\\b"), " ")
    }
  }
  
  # Nettoyer les espaces
  if (isTRUE(config$trim_whitespace)) {
    x <- str_squish(x)
  }
  
  return(x)
}

# ============================================================
# FONCTIONS DE CALCUL DE SIMILARITÉ
# ============================================================

#' Calculer la similarité entre deux vecteurs de chaînes
#' @param x Vecteur de chaînes source
#' @param y Vecteur de chaînes cible
#' @param method Méthode de similarité
#' @return Matrice de similarité
calculate_similarity_matrix <- function(x, y, method = "jw") {
  
  # Méthodes basées sur stringdist (retourne distance, pas similarité)
  distance_methods <- c("jw", "lv", "osa", "lcs", "dl", "hamming", "qgram")
  
  if (method %in% distance_methods) {
    # Calculer la matrice de distance
    dist_matrix <- stringdistmatrix(x, y, method = method)
    
    # Convertir en similarité
    if (method == "jw") {
      # Jaro-Winkler: distance déjà entre 0 et 1
      sim_matrix <- 1 - dist_matrix
    } else {
      # Normaliser par la longueur maximale
      max_lens <- outer(nchar(x), nchar(y), pmax)
      max_lens[max_lens == 0] <- 1  # Éviter division par zéro
      sim_matrix <- 1 - (dist_matrix / max_lens)
    }
    
  } else if (method == "cosine") {
    # Similarité cosinus basée sur les caractères (q-grams)
    sim_matrix <- 1 - stringdistmatrix(x, y, method = "cosine", q = 2)
    
  } else if (method == "jaccard") {
    # Similarité de Jaccard basée sur les q-grams
    sim_matrix <- 1 - stringdistmatrix(x, y, method = "jaccard", q = 2)
    
  } else {
    stop("Méthode de similarité non supportée: ", method)
  }
  
  # S'assurer que les valeurs sont entre 0 et 1
  sim_matrix[sim_matrix < 0] <- 0
  sim_matrix[sim_matrix > 1] <- 1
  
  return(sim_matrix)
}

#' Trouver le meilleur match pour chaque élément
#' @param x Vecteur source
#' @param y Vecteur cible
#' @param method Méthode de similarité
#' @param threshold Seuil minimum
#' @param max_candidates Nombre max de candidats
#' @return Data frame avec les meilleurs matches
find_best_matches <- function(x, y, method = "jw", threshold = 0.80, 
                               max_candidates = 5, preprocessing = NULL) {
  
  # Prétraiter les chaînes
  x_clean <- preprocess_string(x, preprocessing)
  y_clean <- preprocess_string(y, preprocessing)
  
  # Valeurs uniques pour optimisation
  unique_x <- unique(x_clean)
  unique_y <- unique(y_clean)
  
  message("    Calcul de similarité: ", length(unique_x), " x ", length(unique_y))
  
  # Calculer la matrice de similarité pour les valeurs uniques
  sim_matrix <- calculate_similarity_matrix(unique_x, unique_y, method)
  rownames(sim_matrix) <- unique_x
  colnames(sim_matrix) <- unique_y
  
  # Trouver le meilleur match pour chaque valeur unique de x
  results <- lapply(seq_along(unique_x), function(i) {
    
    x_val <- unique_x[i]
    similarities <- sim_matrix[i, ]
    
    # Trier par similarité décroissante
    sorted_idx <- order(similarities, decreasing = TRUE)
    top_idx <- sorted_idx[1:min(max_candidates, length(sorted_idx))]
    
    # Meilleur match
    best_idx <- top_idx[1]
    best_sim <- similarities[best_idx]
    best_match <- unique_y[best_idx]
    
    # Vérifier le seuil
    if (best_sim >= threshold) {
      data.frame(
        x_clean = x_val,
        y_match = best_match,
        similarity = best_sim,
        match_rank = 1,
        above_threshold = TRUE,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        x_clean = x_val,
        y_match = NA_character_,
        similarity = best_sim,
        match_rank = NA_integer_,
        above_threshold = FALSE,
        stringsAsFactors = FALSE
      )
    }
  })
  
  match_df <- bind_rows(results)
  
  # Mapper vers les valeurs originales de x
  x_to_clean <- data.frame(
    x_original = x,
    x_clean = x_clean,
    stringsAsFactors = FALSE
  )
  
  x_to_clean <- x_to_clean %>%
    left_join(match_df, by = "x_clean")
  
  return(x_to_clean)
}

# ============================================================
# FONCTION DE LECTURE D'UN FICHIER DE RÉFÉRENCE
# ============================================================

read_reference_file <- function(config) {
  
  file_path <- config$file$path
  
  if (!file.exists(file_path)) {
    stop("Fichier de référence non trouvé: ", file_path)
  }
  
  ext <- tolower(tools::file_ext(file_path))
  
  if (ext %in% c("xlsx", "xls")) {
    df <- read_excel(
      file_path, 
      sheet = config$file$sheet,
      skip = config$file$skip
    )
  } else if (ext == "csv") {
    df <- read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  } else if (ext == "dta") {
    df <- read_dta(file_path)
  } else {
    stop("Format de fichier non supporté: ", ext)
  }
  
  return(df)
}

# ============================================================
# FONCTION DE PRÉPARATION D'UN FICHIER DE RÉFÉRENCE
# ============================================================

prepare_reference_data <- function(df, config) {
  
  # Supprimer les colonnes non désirées
  if (!is.null(config$drop_cols)) {
    df <- df %>% select(-any_of(config$drop_cols))
  }
  
  # Garder uniquement les colonnes spécifiées
  if (!is.null(config$keep_cols)) {
    if (config$match_type == "exact") {
      key_col <- config$merge$key_ref
    } else {
      key_col <- config$fuzzy$col_ref
    }
    cols_to_keep <- unique(c(key_col, config$keep_cols))
    cols_available <- intersect(cols_to_keep, names(df))
    df <- df %>% select(all_of(cols_available))
  }
  
  # Renommer les colonnes
  if (!is.null(config$rename_cols) && length(config$rename_cols) > 0) {
    for (old_name in names(config$rename_cols)) {
      new_name <- config$rename_cols[[old_name]]
      if (old_name %in% names(df)) {
        df <- df %>% rename(!!new_name := !!old_name)
      }
    }
  }
  
  return(df)
}

# ============================================================
# FUSION EXACTE (PAR CLÉ)
# ============================================================

perform_exact_merge <- function(df_master, config, verbose = TRUE) {
  
  merge_name <- config$name
  
  if (verbose) message("\n  → Fusion exacte: ", merge_name)
  
  # Lire et préparer le fichier de référence
  df_ref <- read_reference_file(config)
  df_ref <- prepare_reference_data(df_ref, config)
  
  if (verbose) message("    Référence: ", nrow(df_ref), " lignes")
  
  # Configuration de la fusion
  key_master <- config$merge$key_master
  key_ref <- config$merge$key_ref
  
  if (!key_master %in% names(df_master)) {
    warning("    ⚠ Clé master '", key_master, "' absente")
    return(list(
      data = df_master,
      stats = list(merge_name = merge_name, status = "skipped", reason = "key_missing")
    ))
  }
  
  # Nettoyer les clés
  df_master <- df_master %>%
    mutate(!!key_master := str_trim(as.character(.data[[key_master]])))
  
  if (key_ref %in% names(df_ref)) {
    df_ref <- df_ref %>%
      mutate(!!key_ref := str_trim(as.character(.data[[key_ref]]))) %>%
      distinct(!!sym(key_ref), .keep_all = TRUE)
  }
  
  # Renommer la clé si différente
  if (key_master != key_ref) {
    df_ref <- df_ref %>% rename(!!key_master := !!key_ref)
  }
  
  # Effectuer la fusion
  n_before <- nrow(df_master)
  
  merge_type <- config$merge$type
  if (merge_type == "left") {
    df_merged <- df_master %>%
      left_join(df_ref, by = key_master, suffix = c("", "_ref"))
  } else if (merge_type == "inner") {
    df_merged <- df_master %>%
      inner_join(df_ref, by = key_master, suffix = c("", "_ref"))
  } else {
    df_merged <- df_master %>%
      left_join(df_ref, by = key_master, suffix = c("", "_ref"))
  }
  
  # Statistiques
  check_var <- config$check_var
  if (!is.null(check_var) && check_var %in% names(df_merged)) {
    n_matched <- sum(!is.na(df_merged[[check_var]]))
    n_unmatched <- sum(is.na(df_merged[[check_var]]))
    pct_matched <- round(n_matched / nrow(df_merged) * 100, 1)
  } else {
    n_matched <- NA
    n_unmatched <- NA
    pct_matched <- NA
  }
  
  if (verbose) {
    message("    Matched: ", format(n_matched, big.mark = " "), " (", pct_matched, "%)")
  }
  
  stats <- list(
    merge_name = merge_name,
    match_type = "exact",
    status = "completed",
    n_matched = n_matched,
    n_unmatched = n_unmatched,
    pct_matched = pct_matched
  )
  
  return(list(data = df_merged, stats = stats))
}

# ============================================================
# FUSION PAR SIMILARITÉ (FUZZY)
# ============================================================

perform_fuzzy_merge <- function(df_master, config, verbose = TRUE) {
  
  merge_name <- config$name
  
  if (verbose) message("\n  → Fusion fuzzy: ", merge_name)
  
  # Lire et préparer le fichier de référence
  df_ref <- read_reference_file(config)
  df_ref <- prepare_reference_data(df_ref, config)
  
  if (verbose) message("    Référence: ", nrow(df_ref), " lignes")
  
  # Configuration fuzzy
  fuzzy_config <- config$fuzzy
  col_master <- fuzzy_config$col_master
  col_ref <- fuzzy_config$col_ref
  
  if (!col_master %in% names(df_master)) {
    warning("    ⚠ Colonne master '", col_master, "' absente")
    return(list(
      data = df_master,
      stats = list(merge_name = merge_name, status = "skipped", reason = "column_missing")
    ))
  }
  
  if (!col_ref %in% names(df_ref)) {
    warning("    ⚠ Colonne référence '", col_ref, "' absente")
    return(list(
      data = df_master,
      stats = list(merge_name = merge_name, status = "skipped", reason = "column_missing")
    ))
  }
  
  # Valeurs à matcher
  x_values <- df_master[[col_master]]
  y_values <- df_ref[[col_ref]]
  
  if (verbose) {
    message("    Valeurs uniques master: ", n_distinct(x_values, na.rm = TRUE))
    message("    Valeurs uniques référence: ", n_distinct(y_values, na.rm = TRUE))
    message("    Méthode: ", fuzzy_config$method, ", seuil: ", fuzzy_config$threshold)
  }
  
  # Trouver les meilleurs matches
  match_results <- find_best_matches(
    x = x_values,
    y = y_values,
    method = fuzzy_config$method,
    threshold = fuzzy_config$threshold,
    max_candidates = fuzzy_config$max_candidates,
    preprocessing = fuzzy_config$preprocessing
  )
  
  # Statistiques de matching
  n_matched <- sum(match_results$above_threshold, na.rm = TRUE)
  n_unmatched <- sum(!match_results$above_threshold, na.rm = TRUE)
  pct_matched <- round(n_matched / nrow(match_results) * 100, 1)
  
  # Distribution des scores
  score_stats <- summary(match_results$similarity[match_results$above_threshold])
  
  if (verbose) {
    message("    Matched: ", format(n_matched, big.mark = " "), " (", pct_matched, "%)")
    message("    Score moyen: ", round(mean(match_results$similarity[match_results$above_threshold], na.rm = TRUE), 3))
    message("    Score min: ", round(min(match_results$similarity[match_results$above_threshold], na.rm = TRUE), 3))
  }
  
  # Préparer la table de correspondance pour la fusion
  # Créer une colonne de jointure prétraitée dans df_ref
  df_ref$._join_key <- preprocess_string(df_ref[[col_ref]], fuzzy_config$preprocessing)
  
  # Ajouter les résultats du matching au master
  df_master$._row_id <- seq_len(nrow(df_master))
  
  match_lookup <- match_results %>%
    select(x_original, y_match, similarity, above_threshold) %>%
    rename(._match_value = y_match, ._similarity = similarity, ._matched = above_threshold)
  
  df_master <- df_master %>%
    mutate(._x_original = .data[[col_master]]) %>%
    left_join(match_lookup, by = c("._x_original" = "x_original"))
  
  # Fusionner avec le référentiel via la valeur matchée
  df_ref_unique <- df_ref %>%
    distinct(._join_key, .keep_all = TRUE)
  
  df_merged <- df_master %>%
    left_join(
      df_ref_unique %>% select(-!!col_ref),
      by = c("._match_value" = "._join_key"),
      suffix = c("", "_ref")
    )
  
  # Ajouter la colonne de score de similarité
  if (isTRUE(fuzzy_config$keep_similarity_score)) {
    sim_col_name <- fuzzy_config$similarity_col
    df_merged[[sim_col_name]] <- df_merged$._similarity
  }
  
  # Nettoyer les colonnes temporaires
  df_merged <- df_merged %>%
    select(-starts_with("._"))
  
  # Statistiques finales
  check_var <- config$check_var
  if (!is.null(check_var) && check_var %in% names(df_merged)) {
    n_matched_final <- sum(!is.na(df_merged[[check_var]]))
    pct_matched_final <- round(n_matched_final / nrow(df_merged) * 100, 1)
  } else {
    n_matched_final <- n_matched
    pct_matched_final <- pct_matched
  }
  
  stats <- list(
    merge_name = merge_name,
    match_type = "fuzzy",
    status = "completed",
    method = fuzzy_config$method,
    threshold = fuzzy_config$threshold,
    n_matched = n_matched,
    n_unmatched = n_unmatched,
    pct_matched = pct_matched,
    similarity_mean = round(mean(match_results$similarity[match_results$above_threshold], na.rm = TRUE), 3),
    similarity_min = round(min(match_results$similarity[match_results$above_threshold], na.rm = TRUE), 3),
    similarity_max = round(max(match_results$similarity[match_results$above_threshold], na.rm = TRUE), 3)
  )
  
  return(list(data = df_merged, stats = stats))
}

# ============================================================
# FONCTION DE FUSION GÉNÉRIQUE
# ============================================================

perform_single_merge <- function(df_master, config, verbose = TRUE) {
  
  if (config$match_type == "exact") {
    return(perform_exact_merge(df_master, config, verbose))
  } else if (config$match_type == "fuzzy") {
    return(perform_fuzzy_merge(df_master, config, verbose))
  } else {
    stop("Type de fusion non supporté: ", config$match_type)
  }
}

# ============================================================
# FONCTION PRINCIPALE: EXÉCUTER TOUS LES MERGES
# ============================================================

merge_all_references <- function(input_file = NULL, 
                                  output_file = NULL,
                                  merges_to_run = NULL,
                                  verbose = TRUE) {
  
  message("\n", strrep("=", 60))
  message("FUSION AVEC LES BASES DE RÉFÉRENCE")
  message(strrep("=", 60))
  
  # Déterminer le fichier d'entrée
  if (is.null(input_file)) {
    cleaned_files <- list.files(
      PATHS$cleaned_final, 
      pattern = "_cleaned\\.dta$",
      full.names = TRUE
    )
    if (length(cleaned_files) == 0) {
      stop("Aucun fichier nettoyé trouvé dans ", PATHS$cleaned_final)
    }
    input_file <- cleaned_files[which.max(file.mtime(cleaned_files))]
  }
  
  message("\nFichier source: ", input_file)
  
  # Charger les données
  df_master <- read_dta(input_file)
  n_initial <- nrow(df_master)
  n_vars_initial <- ncol(df_master)
  
  message("Observations: ", format(n_initial, big.mark = " "))
  message("Variables initiales: ", n_vars_initial)
  
  # Obtenir les merges à exécuter
  if (is.null(merges_to_run)) {
    merges_to_run <- get_enabled_merges()
  } else if (is.character(merges_to_run)) {
    merges_to_run <- MERGE_CONFIGS[merges_to_run]
    merges_to_run <- merges_to_run[!sapply(merges_to_run, is.null)]
  }
  
  if (length(merges_to_run) == 0) {
    message("\nAucun merge à exécuter")
    return(list(data = df_master, output_file = input_file, stats = list()))
  }
  
  message("\nMerges à exécuter: ", length(merges_to_run))
  
  # Afficher le résumé
  for (merge_name in names(merges_to_run)) {
    config <- merges_to_run[[merge_name]]
    type_label <- ifelse(config$match_type == "exact", "[EXACT]", "[FUZZY]")
    file_exists <- ifelse(file.exists(config$file$path), "✓", "✖")
    message("  ", file_exists, " ", type_label, " ", config$name)
  }
  
  # Exécuter les fusions
  message("\nExécution des fusions...")
  
  all_stats <- list()
  
  for (merge_name in names(merges_to_run)) {
    config <- merges_to_run[[merge_name]]
    
    if (!file.exists(config$file$path)) {
      all_stats[[merge_name]] <- list(
        merge_name = config$name,
        status = "skipped",
        reason = "file_missing"
      )
      next
    }
    
    tryCatch({
      result <- perform_single_merge(df_master, config, verbose)
      df_master <- result$data
      all_stats[[merge_name]] <- result$stats
      
    }, error = function(e) {
      message("    ✖ ERREUR: ", e$message)
      all_stats[[merge_name]] <<- list(
        merge_name = config$name,
        status = "error",
        error = e$message
      )
    })
  }
  
  # Résumé
  message("\n", strrep("-", 40))
  message("RÉSUMÉ DES FUSIONS")
  message(strrep("-", 40))
  
  n_completed <- sum(sapply(all_stats, function(x) x$status == "completed"))
  n_skipped <- sum(sapply(all_stats, function(x) x$status == "skipped"))
  n_error <- sum(sapply(all_stats, function(x) x$status == "error"))
  
  message("  Complétées: ", n_completed)
  message("  Ignorées:   ", n_skipped)
  message("  Erreurs:    ", n_error)
  message("  Variables finales: ", ncol(df_master))
  
  # Sauvegarder les noms des colonnes originales avant le merge
  original_cols <- names(read_dta(input_file))
  new_vars <- setdiff(names(df_master), original_cols)
  
  if (length(new_vars) > 0) {
    message("\nNouvelles variables ajoutées (", length(new_vars), "):")
    for (v in head(new_vars, 20)) {
      n_valid <- sum(!is.na(df_master[[v]]))
      pct_valid <- round(n_valid / nrow(df_master) * 100, 1)
      message("  - ", v, " (", pct_valid, "% rempli)")
    }
    if (length(new_vars) > 20) {
      message("  ... et ", length(new_vars) - 20, " autres")
    }
  }
  
  # Sauvegarde
  if (is.null(output_file)) {
    period_match <- regmatches(
      basename(input_file), 
      regexpr("\\d{2}_\\d{4}-\\d{2}_\\d{4}", basename(input_file))
    )
    if (length(period_match) == 0) period_match <- "unknown"
    
    output_file <- file.path(
      PATHS$cleaned_final,
      paste0("data_cnps_final_", period_match, "_", "merged", ".dta")
    )
  }
  
  message("\nSauvegarde...")
  ensure_dir(dirname(output_file))
  write_dta(df_master, output_file, version = STATA_VERSION)
  
  message("Fichier final: ", output_file)
  
  return(list(
    data = df_master,
    output_file = output_file,
    stats = all_stats,
    n_initial = n_initial,
    n_final = nrow(df_master),
    n_vars_initial = n_vars_initial,
    n_vars_final = ncol(df_master),
    new_variables = new_vars
  ))
}

# ============================================================
# FONCTION: EXÉCUTER UN MERGE SPÉCIFIQUE
# ============================================================

merge_single_reference <- function(input_file = NULL, 
                                    merge_name,
                                    output_file = NULL,
                                    verbose = TRUE) {
  
  config <- get_merge_config(merge_name)
  
  if (is.null(config)) {
    stop("Configuration de merge non trouvée: ", merge_name)
  }
  
  return(merge_all_references(
    input_file = input_file,
    output_file = output_file,
    merges_to_run = setNames(list(config), merge_name),
    verbose = verbose
  ))
}

# ============================================================
# FONCTION: ANALYSER LA QUALITÉ DU MATCHING FUZZY
# ============================================================

analyze_fuzzy_match_quality <- function(df, similarity_col, group_col = NULL) {
  
  if (!similarity_col %in% names(df)) {
    stop("Colonne de similarité non trouvée: ", similarity_col)
  }
  
  message("\n=== ANALYSE DE QUALITÉ DU MATCHING ===")
  message("Colonne: ", similarity_col)
  
  scores <- df[[similarity_col]]
  scores <- scores[!is.na(scores)]
  
  message("\nDistribution des scores:")
  message("  N: ", length(scores))
  message("  Moyenne: ", round(mean(scores), 3))
  message("  Médiane: ", round(median(scores), 3))
  message("  Min: ", round(min(scores), 3))
  message("  Max: ", round(max(scores), 3))
  message("  Écart-type: ", round(sd(scores), 3))
  
  # Catégories de qualité
  thresholds <- FUZZY_MATCHING_CONFIG$quality_thresholds
  
  n_excellent <- sum(scores >= thresholds$excellent)
  n_good <- sum(scores >= thresholds$good & scores < thresholds$excellent)
  n_acceptable <- sum(scores >= thresholds$acceptable & scores < thresholds$good)
  n_poor <- sum(scores >= thresholds$poor & scores < thresholds$acceptable)
  n_very_poor <- sum(scores < thresholds$poor)
  
  message("\nDistribution par catégorie:")
  message("  Excellent (≥", thresholds$excellent, "): ", n_excellent, 
          " (", round(n_excellent/length(scores)*100, 1), "%)")
  message("  Bon (≥", thresholds$good, "): ", n_good,
          " (", round(n_good/length(scores)*100, 1), "%)")
  message("  Acceptable (≥", thresholds$acceptable, "): ", n_acceptable,
          " (", round(n_acceptable/length(scores)*100, 1), "%)")
  message("  Faible (≥", thresholds$poor, "): ", n_poor,
          " (", round(n_poor/length(scores)*100, 1), "%)")
  message("  Très faible (<", thresholds$poor, "): ", n_very_poor,
          " (", round(n_very_poor/length(scores)*100, 1), "%)")
  
  # Par groupe si spécifié
  if (!is.null(group_col) && group_col %in% names(df)) {
    message("\nPar ", group_col, ":")
    
    by_group <- df %>%
      filter(!is.na(.data[[similarity_col]])) %>%
      group_by(.data[[group_col]]) %>%
      summarise(
        n = n(),
        mean_score = round(mean(.data[[similarity_col]], na.rm = TRUE), 3),
        min_score = round(min(.data[[similarity_col]], na.rm = TRUE), 3),
        .groups = "drop"
      ) %>%
      arrange(desc(n))
    
    print(head(by_group, 15))
  }
  
  return(list(
    n = length(scores),
    mean = mean(scores),
    median = median(scores),
    sd = sd(scores),
    min = min(scores),
    max = max(scores),
    by_category = list(
      excellent = n_excellent,
      good = n_good,
      acceptable = n_acceptable,
      poor = n_poor,
      very_poor = n_very_poor
    )
  ))
}

# ============================================================
# FONCTION: GÉNÉRER UN RAPPORT DE FUSION
# ============================================================

generate_merge_report <- function(merge_results, output_file = NULL) {
  
  if (is.null(output_file)) {
    output_file <- file.path(
      PATHS$analytical_reports,
      paste0("merge_report_", get_timestamp(), ".txt")
    )
  }
  
  ensure_dir(dirname(output_file))
  
  sink(output_file)
  
  cat("=== RAPPORT DE FUSION DES BASES DE RÉFÉRENCE ===\n")
  cat("Date: ", as.character(Sys.time()), "\n\n")
  
  cat("FICHIER SOURCE:\n")
  cat("  Observations: ", merge_results$n_initial, "\n")
  cat("  Variables initiales: ", merge_results$n_vars_initial, "\n\n")
  
  cat("FICHIER FINAL:\n")
  cat("  Observations: ", merge_results$n_final, "\n")
  cat("  Variables finales: ", merge_results$n_vars_final, "\n")
  cat("  Fichier: ", merge_results$output_file, "\n\n")
  
  cat("DÉTAIL DES FUSIONS:\n")
  cat(strrep("-", 60), "\n")
  
  for (merge_name in names(merge_results$stats)) {
    stats <- merge_results$stats[[merge_name]]
    
    cat("\n", stats$merge_name, " [", toupper(stats$match_type), "]:\n", sep = "")
    cat("  Status: ", stats$status, "\n")
    
    if (stats$status == "completed") {
      cat("  Matched: ", stats$n_matched, " (", stats$pct_matched, "%)\n")
      cat("  Non matched: ", stats$n_unmatched, "\n")
      
      if (stats$match_type == "fuzzy") {
        cat("  Méthode: ", stats$method, "\n")
        cat("  Seuil: ", stats$threshold, "\n")
        cat("  Similarité moyenne: ", stats$similarity_mean, "\n")
        cat("  Similarité min: ", stats$similarity_min, "\n")
        cat("  Similarité max: ", stats$similarity_max, "\n")
      }
    } else if (stats$status == "skipped") {
      cat("  Raison: ", stats$reason, "\n")
    } else if (stats$status == "error") {
      cat("  Erreur: ", stats$error, "\n")
    }
  }
  
  cat("\n\nNOUVELLES VARIABLES AJOUTÉES (", length(merge_results$new_variables), "):\n")
  for (v in merge_results$new_variables) {
    cat("  - ", v, "\n")
  }
  
  sink()
  
  message("Rapport de fusion: ", output_file)
  
  return(output_file)
}

# ============================================================
# FONCTION: EXPORTER LES NON-MATCHÉS POUR RÉVISION
# ============================================================

export_unmatched_for_review <- function(df, config, output_file = NULL) {
  
  check_var <- config$check_var
  
  if (is.null(check_var) || !check_var %in% names(df)) {
    stop("Variable de vérification non trouvée")
  }
  
  # Filtrer les non-matchés
  unmatched <- df %>%
    filter(is.na(.data[[check_var]]))
  
  message("Observations non matchées: ", nrow(unmatched))
  
  # Déterminer la colonne source
  if (config$match_type == "exact") {
    source_col <- config$merge$key_master
  } else {
    source_col <- config$fuzzy$col_master
  }
  
  # Extraire les valeurs uniques non matchées
  unmatched_values <- unmatched %>%
    group_by(.data[[source_col]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(desc(n))
  
  if (is.null(output_file)) {
    output_file <- file.path(
      PATHS$inconsistencies,
      paste0("unmatched_", gsub(" ", "_", config$name), "_", get_timestamp(), ".xlsx")
    )
  }
  
  ensure_dir(dirname(output_file))
  
  library(openxlsx)
  wb <- createWorkbook()
  addWorksheet(wb, "Valeurs_Non_Matchees")
  writeData(wb, "Valeurs_Non_Matchees", unmatched_values)
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  message("Export: ", output_file)
  
  return(list(
    output_file = output_file,
    n_unmatched_rows = nrow(unmatched),
    n_unique_values = nrow(unmatched_values)
  ))
}

message("Module de fusion des références chargé (exact + fuzzy)")