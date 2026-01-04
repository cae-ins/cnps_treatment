# =============================================================================
# Fusion optimisée par similarité de chaînes entre Excel et Stata
# =============================================================================

# Installation des packages si nécessaire
packages <- c("readxl", "haven", "stringdist", "data.table", "parallel", "stringi")
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
invisible(lapply(packages, install_if_missing))

# Chargement des packages
library(readxl)      # Lecture Excel
library(haven)       # Lecture .dta (Stata)
library(stringdist)  # Calcul de similarité
library(data.table)  # Manipulation rapide des données
library(parallel)    # Parallélisation
library(stringi)     # Manipulation de chaînes optimisée

# =============================================================================
# PARAMÈTRES À MODIFIER
# =============================================================================

# Chemins des fichiers (À MODIFIER)
EXCEL_PATH <- "C:/Users/e_koffie/Documents/Salaires/CNPS_TREATMENT_PROJECT/data/referentiels/CITP_E_2025.xlsx"
DTA_PATH <- "C:/Users/e_koffie/Documents/Salaires/CNPS_TREATMENT_PROJECT/output/data_cnps_merged_01_2024-11_2025.dta"
OUTPUT_PATH <- "C:/Users/e_koffie/Documents/Salaires/CNPS_TREATMENT_PROJECT/output/data_cnps_01_2024-11_2025_merged_pro.dta"  # ou .csv, .xlsx

# Seuil de similarité minimum (70% = 0.70)
SEUIL_SIMILARITE <- 0.70

# Nombre de coeurs pour le calcul parallèle (NULL = auto-détection)
N_CORES <- NULL

# =============================================================================
# FONCTIONS OPTIMISÉES
# =============================================================================

#' Normalise les chaînes pour améliorer la correspondance
normaliser_chaine <- function(x) {
  x <- tolower(x)
  x <- stri_trans_general(x, "Latin-ASCII")  # Supprime accents
  x <- gsub("[^a-z0-9 ]", " ", x)            # Garde lettres/chiffres/espaces
  x <- gsub("\\s+", " ", x)                  # Espaces multiples -> simple
  x <- trimws(x)                             # Supprime espaces début/fin
  return(x)
}

#' Calcule la similarité Jaro-Winkler (optimisée pour les noms)
#' Retourne une valeur entre 0 et 1
calculer_similarite <- function(s1, s2) {
  1 - stringdist(s1, s2, method = "jw", p = 0.1)
}

#' Trouve la meilleure correspondance pour une profession
#' Utilise une approche en deux étapes pour optimiser
trouver_meilleure_correspondance <- function(profession_norm, 
                                             libelles_norm, 
                                             libelles_orig,
                                             codes_citp,
                                             libelles_citp,
                                             seuil) {
  
  # Calcul vectorisé des similarités
  similarites <- 1 - stringdist(profession_norm, libelles_norm, method = "jw", p = 0.1)
  
  # Trouver le maximum
  idx_max <- which.max(similarites)
  score_max <- similarites[idx_max]
  
  # Retourner les résultats
  if (score_max >= seuil) {
    return(list(
      libelle = libelles_orig[idx_max],
      Codes_CITP_final = codes_citp[idx_max],
      libelle_CITP_final = libelles_citp[idx_max],
      score_similarite = round(score_max * 100, 2)
    ))
  } else {
    return(list(
      libelle = NA_character_,
      Codes_CITP_final = NA_character_,
      libelle_CITP_final = NA_character_,
      score_similarite = round(score_max * 100, 2)
    ))
  }
}

# =============================================================================
# SCRIPT PRINCIPAL
# =============================================================================

cat("=== Démarrage de la fusion par similarité ===\n\n")

# --- 1. Chargement des données ---
cat("[1/5] Chargement des données...\n")

# Charger Excel
tryCatch({
  dt_excel <- as.data.table(read_excel(EXCEL_PATH))
  cat(sprintf("    Excel: %d lignes chargées\n", nrow(dt_excel)))
}, error = function(e) {
  stop(paste("Erreur lecture Excel:", e$message))
})

# Charger Stata
tryCatch({
  dt_stata <- as.data.table(read_dta(DTA_PATH))
  cat(sprintf("    Stata: %d lignes chargées\n", nrow(dt_stata)))
}, error = function(e) {
  stop(paste("Erreur lecture Stata:", e$message))
})

# --- 2. Préparation des données ---
cat("\n[2/5] Préparation et normalisation des données...\n")

# Vérifier les colonnes
colonnes_excel_requises <- c("libelle", "Codes_CITP_final", "libelle_CITP_final")
if (!all(colonnes_excel_requises %in% names(dt_excel))) {
  stop(paste("Colonnes manquantes dans Excel:", 
             paste(setdiff(colonnes_excel_requises, names(dt_excel)), collapse = ", ")))
}

if (!"PROFESSION" %in% names(dt_stata)) {
  stop("Colonne 'PROFESSION' manquante dans le fichier Stata")
}

# Normaliser les chaînes (Excel)
dt_excel[, libelle_norm := normaliser_chaine(libelle)]

# Supprimer les doublons dans Excel (garder unique pour la référence)
dt_excel_unique <- unique(dt_excel, by = "libelle_norm")
cat(sprintf("    Libellés uniques dans Excel: %d\n", nrow(dt_excel_unique)))

# Normaliser les professions (Stata)
dt_stata[, PROFESSION_norm := normaliser_chaine(PROFESSION)]

# Identifier les professions uniques pour optimiser
professions_uniques <- unique(dt_stata$PROFESSION_norm)
cat(sprintf("    Professions uniques dans Stata: %d\n", length(professions_uniques)))

# --- 3. Calcul des correspondances (parallélisé) ---
cat("\n[3/5] Calcul des correspondances (cela peut prendre du temps)...\n")

# Configuration parallèle
if (is.null(N_CORES)) {
  N_CORES <- max(1, detectCores() - 1)
}
cat(sprintf("    Utilisation de %d coeur(s)\n", N_CORES))

# Préparer les vecteurs pour le calcul
libelles_norm_vec <- dt_excel_unique$libelle_norm
libelles_orig_vec <- dt_excel_unique$libelle
codes_citp_vec <- as.character(dt_excel_unique$Codes_CITP_final)
libelles_citp_vec <- as.character(dt_excel_unique$libelle_CITP_final)

# Fonction pour traiter un lot de professions
traiter_lot <- function(professions_lot) {
  resultats <- lapply(professions_lot, function(prof) {
    trouver_meilleure_correspondance(
      prof, 
      libelles_norm_vec, 
      libelles_orig_vec,
      codes_citp_vec,
      libelles_citp_vec,
      SEUIL_SIMILARITE
    )
  })
  return(resultats)
}

# Calcul parallèle ou séquentiel selon la taille
temps_debut <- Sys.time()

if (length(professions_uniques) > 100 && N_CORES > 1) {
  # Diviser en lots pour le traitement parallèle
  n_lots <- N_CORES * 4
  lots <- split(professions_uniques, cut(seq_along(professions_uniques), n_lots, labels = FALSE))
  
  # Créer le cluster
  cl <- makeCluster(N_CORES)
  clusterExport(cl, c("libelles_norm_vec", "libelles_orig_vec", "codes_citp_vec", 
                      "libelles_citp_vec", "SEUIL_SIMILARITE", 
                      "trouver_meilleure_correspondance", "stringdist"))
  
  # Traitement parallèle avec barre de progression
  cat("    Traitement en cours")
  resultats_lots <- parLapply(cl, lots, traiter_lot)
  stopCluster(cl)
  
  # Aplatir les résultats
  resultats_list <- unlist(resultats_lots, recursive = FALSE)
} else {
  # Traitement séquentiel avec barre de progression
  resultats_list <- vector("list", length(professions_uniques))
  n_total <- length(professions_uniques)
  
  for (i in seq_along(professions_uniques)) {
    resultats_list[[i]] <- trouver_meilleure_correspondance(
      professions_uniques[i],
      libelles_norm_vec,
      libelles_orig_vec,
      codes_citp_vec,
      libelles_citp_vec,
      SEUIL_SIMILARITE
    )
    
    # Afficher progression tous les 5%
    if (i %% max(1, n_total %/% 20) == 0) {
      cat(sprintf("\r    Progression: %d%%", round(i / n_total * 100)))
    }
  }
}

temps_fin <- Sys.time()
cat(sprintf("\n    Temps de calcul: %.1f secondes\n", 
            as.numeric(difftime(temps_fin, temps_debut, units = "secs"))))

# --- 4. Construction de la table de correspondance ---
cat("\n[4/5] Construction de la table de correspondance...\n")

# Créer le data.table de correspondance
dt_correspondance <- data.table(
  PROFESSION_norm = professions_uniques,
  libelle_match = sapply(resultats_list, function(x) x$libelle),
  Codes_CITP_final = sapply(resultats_list, function(x) x$Codes_CITP_final),
  libelle_CITP_final = sapply(resultats_list, function(x) x$libelle_CITP_final),
  score_similarite = sapply(resultats_list, function(x) x$score_similarite)
)

# Statistiques
n_matches <- sum(!is.na(dt_correspondance$libelle_match))
cat(sprintf("    Correspondances trouvées (>= %d%%): %d / %d (%.1f%%)\n",
            SEUIL_SIMILARITE * 100, n_matches, nrow(dt_correspondance),
            n_matches / nrow(dt_correspondance) * 100))

# --- 5. Fusion avec la base Stata ---
cat("\n[5/5] Fusion avec la base Stata...\n")

# Jointure par la clé normalisée
dt_resultat <- merge(
  dt_stata,
  dt_correspondance,
  by = "PROFESSION_norm",
  all.x = TRUE
)

# Supprimer la colonne normalisée temporaire
dt_resultat[, PROFESSION_norm := NULL]

# Réorganiser les colonnes (PROFESSION d'abord, puis les nouvelles colonnes, puis le reste)
nouvelles_cols <- c("libelle_match", "Codes_CITP_final", "libelle_CITP_final", "score_similarite")
autres_cols <- setdiff(names(dt_resultat), c("PROFESSION", nouvelles_cols))
setcolorder(dt_resultat, c("PROFESSION", nouvelles_cols, autres_cols))

# --- 6. Sauvegarde ---
cat("\n[6/5] Sauvegarde du résultat...\n")

# Déterminer le format de sortie
extension <- tools::file_ext(OUTPUT_PATH)

if (extension == "dta") {
  write_dta(dt_resultat, OUTPUT_PATH)
} else if (extension == "csv") {
  fwrite(dt_resultat, OUTPUT_PATH, bom = TRUE)  # BOM pour Excel
} else if (extension == "xlsx") {
  if (!requireNamespace("writexl", quietly = TRUE)) {
    install.packages("writexl", repos = "https://cloud.r-project.org")
  }
  writexl::write_xlsx(dt_resultat, OUTPUT_PATH)
} else {
  # Par défaut, sauvegarder en CSV
  OUTPUT_PATH <- paste0(tools::file_path_sans_ext(OUTPUT_PATH), ".csv")
  fwrite(dt_resultat, OUTPUT_PATH, bom = TRUE)
}

cat(sprintf("    Fichier sauvegardé: %s\n", OUTPUT_PATH))
cat(sprintf("    Nombre de lignes: %d\n", nrow(dt_resultat)))

# --- Résumé final ---
cat("\n=== Résumé ===\n")
cat(sprintf("Total lignes dans le résultat: %d\n", nrow(dt_resultat)))
cat(sprintf("Lignes avec correspondance (>= %d%%): %d\n", 
            SEUIL_SIMILARITE * 100, sum(!is.na(dt_resultat$libelle_match))))
cat(sprintf("Lignes sans correspondance: %d\n", sum(is.na(dt_resultat$libelle_match))))

# Distribution des scores
cat("\nDistribution des scores de similarité:\n")
scores <- dt_resultat$score_similarite[!is.na(dt_resultat$score_similarite)]
if (length(scores) > 0) {
  cat(sprintf("  Min: %.1f%%\n", min(scores)))
  cat(sprintf("  Médiane: %.1f%%\n", median(scores)))
  cat(sprintf("  Moyenne: %.1f%%\n", mean(scores)))
  cat(sprintf("  Max: %.1f%%\n", max(scores)))
}

cat("\n=== Terminé ===\n")