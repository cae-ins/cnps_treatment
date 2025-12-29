# ==============================================================================
# TRANSFORMATION PANEL LONG VERS LARGE - Version PARALLÉLISÉE AMÉLIORÉE
# Pour fichiers volumineux (18+ GB)
# ==============================================================================

library(dplyr)
library(tidyr)
library(haven)
library(data.table)
library(future)
library(future.apply)
library(parallel)
library(dplyr)
library(stringr)


# Configuration du multiprocessing
setup_parallel <- function(n_cores = NULL) {
  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores() - 1
  }
  
  if (n_cores > 125) {
    message("⚠️  Demande de ", n_cores, " cores mais R limite à 125 connexions PSOCK")
    message("   Solution: utilisation de 'multicore' (fork) sur Linux")
    
    if (.Platform$OS.type == "unix") {
      plan(multicore, workers = n_cores)
      message("✅ Configuration 'multicore' : ", n_cores, " cores")
    } else {
      n_cores <- 120
      plan(multisession, workers = n_cores)
      message("⚠️  Windows détecté, limitation à ", n_cores, " cores")
    }
  } else {
    message("Configuration parallèle : ", n_cores, " cores utilisés")
    plan(multisession, workers = n_cores)
  }
  
  return(n_cores)
}

# Fonction pour lire le fichier Stata par chunks
read_stata_chunked <- function(input_file, chunk_size = 500000) {
  
  message("\n[1] LECTURE DU FICHIER STATA PAR CHUNKS")
  message("   Taille du chunk : ", format(chunk_size, big.mark = ","), " lignes")
  
  message("   Lecture de l'en-tête...")
  header_data <- read_dta(input_file, n_max = 1000)
  
  total_vars <- ncol(header_data)
  var_names <- names(header_data)
  
  message("   Variables détectées : ", total_vars)
  
  file_size_gb <- file.info(input_file)$size / (1024^3)
  message("   Taille du fichier : ", round(file_size_gb, 2), " GB")
  
  return(list(
    var_names = var_names,
    header = header_data,
    file_size = file_size_gb
  ))
}

# Fonction pour identifier les variables clés
identify_key_variables <- function(var_names) {
  
  message("\n[2] IDENTIFICATION DES VARIABLES CLÉS")
  message(strrep("-", 60))
  
  id_candidates <- var_names[grepl("id|mat|ident|individu|person|num|nni|cnps", 
                                    var_names, ignore.case = TRUE)]
  
  time_candidates <- var_names[grepl("annee|year|periode|date|trimestre|mois|time|an", 
                                      var_names, ignore.case = TRUE)]
  
  salary_vars <- var_names[grepl("sal|remun|wage|revenu|income|montant|masse", 
                                  var_names, ignore.case = TRUE)]
  
  status_vars <- var_names[grepl("statut|status|categorie|type|class|poste|fonction", 
                                  var_names, ignore.case = TRUE)]
  
  firm_vars <- var_names[grepl("entreprise|firm|employ|societe|company|etab|raison", 
                                var_names, ignore.case = TRUE)]
  
  message("\n   ID candidats : ", paste(head(id_candidates, 5), collapse=", "))
  message("   Temps candidats : ", paste(head(time_candidates, 5), collapse=", "))
  message("   Salaire vars : ", paste(head(salary_vars, 5), collapse=", "))
  message("   Statut vars : ", paste(head(status_vars, 5), collapse=", "))
  message("   Entreprise vars : ", paste(head(firm_vars, 5), collapse=", "))
  
  return(list(
    id = id_candidates,
    time = time_candidates,
    salary = salary_vars,
    status = status_vars,
    firm = firm_vars
  ))
}

# Fonction principale avec parallélisation
transform_to_wide_panel_parallel <- function(
    input_file, 
    output_file = NULL,
    id_var = NULL,
    time_var = NULL,
    vars_to_pivot = NULL,
    time_invariant_vars = NULL,
    chunk_size = 500000,
    n_cores = NULL
) {
  
  message("\n", strrep("=", 70))
  message("TRANSFORMATION PANEL LONG -> LARGE (VERSION PARALLÉLISÉE)")
  message(strrep("=", 70))
  
  # Configuration parallèle
  n_cores <- setup_parallel(n_cores)
  
  # Lecture de l'en-tête et identification
  file_info <- read_stata_chunked(input_file, chunk_size)
  key_vars <- identify_key_variables(file_info$var_names)
  
  # Sélection interactive ou automatique des variables
  if (is.null(id_var)) {
    message("\n[3] SÉLECTION DE LA VARIABLE ID")
    message("   Candidats : ", paste(head(key_vars$id, 10), collapse=", "))
    id_var <- readline(prompt = "   Entrez le nom de la variable ID : ")
    if (id_var == "") id_var <- key_vars$id[1]
  }
  
  if (is.null(time_var)) {
    message("\n[4] SÉLECTION DE LA VARIABLE TEMPS")
    message("   Candidats : ", paste(head(key_vars$time, 10), collapse=", "))
    time_var <- readline(prompt = "   Entrez le nom de la variable TEMPS : ")
    if (time_var == "") time_var <- key_vars$time[1]
  }
  
  if (is.null(vars_to_pivot)) {
    message("\n[5] SÉLECTION DES VARIABLES À PIVOTER")
    message("   Salaire : ", paste(head(key_vars$salary, 5), collapse=", "))
    message("   Statut : ", paste(head(key_vars$status, 5), collapse=", "))
    message("   Entreprise : ", paste(head(key_vars$firm, 5), collapse=", "))
    
    vars_input <- readline(prompt = "   Entrez les variables (séparées par des virgules) : ")
    if (vars_input == "") {
      vars_to_pivot <- c(key_vars$salary[1], key_vars$status[1], key_vars$firm[1])
      vars_to_pivot <- vars_to_pivot[!is.na(vars_to_pivot)]
    } else {
      vars_to_pivot <- trimws(strsplit(vars_input, ",")[[1]])
    }
  }
  
  message("\n   Variable ID : ", id_var)
  message("   Variable TEMPS : ", time_var)
  message("   Variables à pivoter : ", paste(vars_to_pivot, collapse=", "))
  if (!is.null(time_invariant_vars)) {
    message("   Variables invariantes : ", paste(time_invariant_vars, collapse=", "))
  }
  
  # MÉTHODE OPTIMISÉE : Utilisation de data.table pour performance
  message("\n[6] CHARGEMENT AVEC DATA.TABLE (optimisé mémoire)...")
  message("   Lecture en cours... Cela peut prendre plusieurs minutes...")
  
  # Liste de toutes les colonnes à charger
  cols_to_load <- unique(c(id_var, time_var, vars_to_pivot, time_invariant_vars))
  
  data_dt <- setDT(read_dta(input_file, col_select = cols_to_load))
  
  message("   Dimensions : ", format(nrow(data_dt), big.mark=","), " lignes × ", 
          ncol(data_dt), " colonnes")
  message("   Mémoire utilisée : ", format(object.size(data_dt), units="GB"))
  
  # Extraction des variables invariantes dans le temps (SEXE, DATE_NAISSANCE)
  if (!is.null(time_invariant_vars)) {
    message("\n[6b] EXTRACTION DES VARIABLES INVARIANTES DANS LE TEMPS...")
    
    time_inv_data <- data_dt[, c(id_var, time_invariant_vars), with = FALSE]
    # Garder une seule valeur par individu (première observation)
    time_inv_data <- unique(time_inv_data, by = id_var)
    
    message("   Variables invariantes extraites : ", paste(time_invariant_vars, collapse=", "))
  }
  
  # Nettoyage mémoire
  gc()
  
  # TRANSFORMATION EN WIDE avec data.table
  message("\n[7] TRANSFORMATION LONG -> WIDE avec data.table...")
  
  # Vérification des duplications
  message("   Vérification des duplications...")
  duplicates <- data_dt[, .N, by = c(id_var, time_var)][N > 1]
  
  if (nrow(duplicates) > 0) {
    message("   ⚠️  ATTENTION: ", format(nrow(duplicates), big.mark=","), 
            " combinaisons (ID, PERIOD) ont des doublons!")
    message("   Stratégie: on garde la dernière observation par combinaison")
    
    data_dt <- data_dt[, .SD[.N], by = c(id_var, time_var)]
    message("   ✅ Doublons résolus")
  }
  
  # Formule pour dcast
  formula_str <- paste0(id_var, " ~ ", time_var)
  
  data_wide <- dcast(
    data_dt, 
    formula = as.formula(formula_str),
    value.var = vars_to_pivot,
    sep = "_t"
  )
  
  message("   Nouvelles dimensions : ", format(nrow(data_wide), big.mark=","), 
          " lignes × ", ncol(data_wide), " colonnes")
  
  # FUSION avec les variables invariantes
  if (!is.null(time_invariant_vars)) {
    message("\n[7b] FUSION DES VARIABLES INVARIANTES...")
    data_wide <- merge(data_wide, time_inv_data, by = id_var, all.x = TRUE)
    message("   ✅ Variables invariantes ajoutées")
  }
  
  # Libération mémoire
  rm(data_dt)
  gc()
  
  # CALCULS DES DYNAMIQUES
  message("\n[8] CALCUL DES DYNAMIQUES...")
  
  if (length(vars_to_pivot) > 0) {
    
    for (var_base in vars_to_pivot) {
      # Cherche toutes les colonnes de cette variable avec pattern _t suivi de chiffres
      cols <- grep(paste0("^", var_base, "_t\\d+$"), names(data_wide), value = TRUE)
      
      if (length(cols) >= 2) {
        message("   Calcul dynamiques pour: ", var_base)
        
        cols_sorted <- sort(cols)
        first_col <- cols_sorted[1]
        last_col <- cols_sorted[length(cols_sorted)]
        
        is_numeric <- is.numeric(data_wide[[first_col]])
        
        if (is_numeric) {
          message("      → Type: numérique")
          
          data_wide[, paste0(var_base, "_change") := 
                      get(last_col) - get(first_col)]
          
          data_wide[, paste0(var_base, "_growth_pct") := 
                      (get(last_col) - get(first_col)) / get(first_col) * 100]
          
          data_wide[, paste0(var_base, "_mean") := 
                      rowMeans(.SD, na.rm = TRUE), .SDcols = cols_sorted]
          
          data_wide[, paste0(var_base, "_max") := 
                      do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = cols_sorted]
          
          data_wide[, paste0(var_base, "_min") := 
                      do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = cols_sorted]
          
        } else {
          message("      → Type: catégoriel")
          
          data_wide[, paste0(var_base, "_first") := get(first_col)]
          data_wide[, paste0(var_base, "_last") := get(last_col)]
          
          data_wide[, paste0(var_base, "_changed") := 
                      get(first_col) != get(last_col)]
          
          data_wide[, paste0(var_base, "_mode") := {
            mode_val <- apply(.SD, 1, function(x) {
              x <- x[!is.na(x)]
              if (length(x) == 0) return(NA)
              tbl <- table(x)
              names(tbl)[which.max(tbl)]
            })
            mode_val
          }, .SDcols = cols_sorted]
        }
        
        data_wide[, paste0(var_base, "_n_changes") := {
          rowSums(sapply(2:length(cols_sorted), function(i) {
            get(cols_sorted[i]) != get(cols_sorted[i-1])
          }), na.rm = TRUE)
        }]
        
        data_wide[, paste0(var_base, "_n_obs") := 
                    rowSums(!is.na(.SD)), .SDcols = cols_sorted]
      }
    }
  }
  
  # RÉORGANISATION DES COLONNES
  message("\n[9] RÉORGANISATION DES COLONNES PAR PÉRIODE...")
  
  # Ordre souhaité des variables dans chaque période
  var_order <- c("NIVEAU_ETUDE", "PROFESSION", "STATUT_TRAVAILLEUR", 
                 "SALAIRE_BRUT_MENS", "NUMERO_EMPLOYEUR", 
                 "RAISON_SOCIALE", "SECTEUR_ACTIVITE")
  
  # Extraire toutes les périodes uniques
  all_cols <- names(data_wide)
  
  # Chercher toutes les colonnes avec pattern _t suivi de chiffres
  time_cols <- grep("_t\\d+", all_cols, value = TRUE)
  
  if (length(time_cols) > 0) {
    # Extraire les périodes de toutes les colonnes temporelles
    periods <- unique(gsub(".*_t(\\d+).*$", "\\1", time_cols))
    periods <- sort(periods)
    
    message("   Périodes détectées : ", paste(periods, collapse=", "))
    
    # Construire l'ordre final des colonnes
    final_order <- c(id_var)  # Commence avec ID_INDIV
    
    # Ajoute les variables invariantes
    if (!is.null(time_invariant_vars)) {
      final_order <- c(final_order, time_invariant_vars)
    }
    
    # Pour chaque période, ajoute les variables dans l'ordre souhaité
    for (period in periods) {
      for (var in var_order) {
        col_name <- paste0(var, "_t", period)
        if (col_name %in% all_cols) {
          final_order <- c(final_order, col_name)
        }
      }
    }
    
    # Ajoute les variables de dynamiques à la fin
    dynamic_vars <- grep("_(change|growth_pct|mean|max|min|first|last|changed|mode|n_changes|n_obs)$", 
                         all_cols, value = TRUE)
    # Enlever les variables déjà dans final_order
    dynamic_vars <- setdiff(dynamic_vars, final_order)
    final_order <- c(final_order, dynamic_vars)
    
    # Ajouter les colonnes manquantes (au cas où)
    missing_cols <- setdiff(all_cols, final_order)
    final_order <- c(final_order, missing_cols)
    
    # Réorganiser le dataset
    data_wide <- data_wide[, ..final_order]
    
    message("   ✅ Colonnes réorganisées")
  } else {
    message("   ⚠️  Aucune colonne temporelle détectée!")
    message("   Les colonnes actuelles sont : ", paste(head(all_cols, 10), collapse=", "), "...")
  }
  
  # STATISTIQUES DESCRIPTIVES
  message("\n[10] STATISTIQUES DESCRIPTIVES")
  message(strrep("-", 60))
  message("   Nombre d'individus uniques : ", format(nrow(data_wide), big.mark=","))
  message("   Nombre total de variables : ", ncol(data_wide))
  message("   Nombre de périodes : ", length(periods))
  message("   Taille en mémoire : ", format(object.size(data_wide), units="GB"))
  
  # EXPORT
  if (!is.null(output_file)) {
    message("\n[11] EXPORT DU FICHIER...")
    
    output_ext <- tools::file_ext(output_file)
    
    if (output_ext == "dta") {
      message("   Export en Stata (.dta)...")
      write_dta(as.data.frame(data_wide), output_file)
    } else if (output_ext == "rds") {
      message("   Export en RDS (compressé)...")
      saveRDS(data_wide, output_file, compress = "xz")
    } else if (output_ext == "csv") {
      message("   Export en CSV...")
      fwrite(data_wide, output_file)
    } else if (output_ext == "parquet") {
      message("   Export en Parquet (recommandé pour gros fichiers)...")
      library(arrow)
      write_parquet(data_wide, output_file)
    } else {
      message("   Export en CSV par défaut...")
      fwrite(data_wide, paste0(output_file, ".csv"))
    }
    
    message("   Fichier sauvegardé : ", output_file)
    message("   Taille du fichier : ", 
            round(file.info(output_file)$size / (1024^3), 2), " GB")
  }
  
  message("\n", strrep("=", 70))
  message("TRANSFORMATION TERMINÉE !")
  message("Temps de calcul avec ", n_cores, " cores")
  message(strrep("=", 70), "\n")
  
  # Fermeture du plan parallèle
  plan(sequential)
  
  return(data_wide)
}

# ==============================================================================
# UTILISATION AVEC TOUTES LES NOUVELLES FONCTIONNALITÉS
# ==============================================================================

# data_wide <- transform_to_wide_panel_parallel(
#   input_file = "data/panel_long.dta",
#   output_file = "data/panel_wide.parquet",
#   id_var = "ID_INDIV",
#   time_var = "PERIOD",
#   vars_to_pivot = c(
#     "NIVEAU_ETUDE", 
#     "PROFESSION", 
#     "STATUT_TRAVAILLEUR", 
#     "SALAIRE_BRUT_MENS",
#     "NUMERO_EMPLOYEUR", 
#     "RAISON_SOCIALE", 
#     "SECTEUR_ACTIVITE"
#   ),
#   time_invariant_vars = c("SEXE", "DATE_NAISSANCE"),
#   n_cores = 250
# )
# Configure data.table pour utiliser tous les threads
setDTthreads(threads = 124)

# Configure multicore (fork) pour Linux
plan(multicore, workers = 124)

# ==============================================================================
# TRANSFORMATION AVEC TES VARIABLES
# ==============================================================================

data_wide <- transform_to_wide_panel_parallel(
  input_file = "data/processed/data_cnps_merged_01_2024-11_2025.dta",
  output_file = "data/processed/panel_wide.parquet",
  id_var = "ID_INDIV",
  time_var = "PERIOD",
  vars_to_pivot = c(
    "PROFESSION", 
    "STATUT_TRAVAILLEUR", 
    "SALAIRE_BRUT_MENS",
    "NUMERO_EMPLOYEUR", 
    "RAISON_SOCIALE", 
    "SECTEUR_ACTIVITE"
  ),
  time_invariant_vars = c("SEXE", "DATE_NAISSANCE"),
  n_cores = 124
)

# ==============================================================================
# ORDONNER LES VARIABLES TEMPORELLES PAR PÉRIODE
# ==============================================================================
message("\n[12] RÉORDONNANCEMENT DES VARIABLES TEMPORELLES PAR PÉRIODE...")
var_order <- c(
  "PROFESSION",
  "STATUT_TRAVAILLEUR",
  "SALAIRE_BRUT_MENS",
  "NUMERO_EMPLOYEUR",
  "RAISON_SOCIALE",
  "SECTEUR_ACTIVITE"
)

cols <- colnames(data_wide)

# Colonnes avec période YYYY_MM
cols_time <- cols[str_detect(cols, "_t\\d{4}_\\d{2}$")]

# Colonnes fixes (sans période)
cols_static <- setdiff(cols, cols_time)

# Tableau pour ordonner
order_tbl <- tibble(col = cols_time) %>%
  mutate(
    period = str_extract(col, "\\d{4}_\\d{2}$"),
    var    = str_remove(col, "_t\\d{4}_\\d{2}$"),
    var = factor(var, levels = var_order)
  ) %>%
  arrange(period, var)

new_order <- c(
  cols_static,   # variables fixes
  order_tbl$col  # variables temporelles dans l'ordre défini
)

data_wide <- data_wide %>%
  select(all_of(new_order))

# Aperçu du résultat
head(data_wide, 10)
dim(data_wide)
names(data_wide)
glimpse(data_wide)
write_dta(data_wide, "data/processed/panel_wide.dta")
