# =============================================================================
# Analyse des flux d'individus entre périodes (MOIS/ANNEE)
# =============================================================================

library(data.table)

# S'assurer que c'est un data.table
if (!is.data.table(dt_resultat)) {
  setDT(dt_resultat)
}

# Créer la clé ANNEE_MOIS et trier
dt_resultat[, ANNEE_MOIS := paste(ANNEE, sprintf("%02d", MOIS), sep = "_")]

# =============================================================================
# EXTRAIRE LES INDIVIDUS UNIQUES PAR PÉRIODE
# =============================================================================

cat("=== Extraction des individus par période ===\n")

# Liste des individus uniques par période (optimisé)
individus_par_periode <- dt_resultat[, .(ID_INDIV = unique(ID_INDIV)), keyby = ANNEE_MOIS]

# Obtenir les périodes triées
periodes <- sort(unique(dt_resultat$ANNEE_MOIS))
n_periodes <- length(periodes)

cat(sprintf("Périodes: %s à %s (%d mois)\n\n", 
            periodes[1], periodes[n_periodes], n_periodes))

# =============================================================================
# CALCUL DES FLUX ENTRE PÉRIODES CONSÉCUTIVES
# =============================================================================

cat("=== Calcul des flux entre périodes consécutives ===\n\n")

# Pré-allouer le résultat
flux_results <- vector("list", n_periodes - 1)

for (i in 1:(n_periodes - 1)) {
  
  periode_avant <- periodes[i]
  periode_apres <- periodes[i + 1]
  
  # Extraire les ID de chaque période
  id_avant <- individus_par_periode[ANNEE_MOIS == periode_avant, ID_INDIV]
  id_apres <- individus_par_periode[ANNEE_MOIS == periode_apres, ID_INDIV]
  
  # Calculs des flux
  n_avant <- length(id_avant)
  n_apres <- length(id_apres)
  
  # Utiliser data.table pour les intersections (plus rapide)
  id_avant_dt <- data.table(ID_INDIV = id_avant, key = "ID_INDIV")
  id_apres_dt <- data.table(ID_INDIV = id_apres, key = "ID_INDIV")
  
  # Communs (dans les deux)
  n_communs <- nrow(fintersect(id_avant_dt, id_apres_dt))
  
  # Sortants (dans avant mais pas dans après)
  n_sortants <- n_avant - n_communs
  
  # Entrants (dans après mais pas dans avant)
  n_entrants <- n_apres - n_communs
  
  flux_results[[i]] <- data.table(
    periode_avant = periode_avant,
    periode_apres = periode_apres,
    n_avant = n_avant,
    n_apres = n_apres,
    entrants = n_entrants,
    sortants = n_sortants,
    communs = n_communs,
    pct_entrants = round(n_entrants / n_apres * 100, 2),
    pct_sortants = round(n_sortants / n_avant * 100, 2),
    pct_communs_avant = round(n_communs / n_avant * 100, 2),
    pct_communs_apres = round(n_communs / n_apres * 100, 2)
  )
}

# Combiner les résultats
base_flux <- rbindlist(flux_results)

# Afficher
print(base_flux)

# =============================================================================
# RÉSUMÉ GLOBAL
# =============================================================================

cat("\n=== Résumé global des flux ===\n")
cat(sprintf("Moyenne entrants par mois: %s (%.1f%%)\n", 
            format(round(mean(base_flux$entrants)), big.mark = " "),
            mean(base_flux$pct_entrants)))
cat(sprintf("Moyenne sortants par mois: %s (%.1f%%)\n", 
            format(round(mean(base_flux$sortants)), big.mark = " "),
            mean(base_flux$pct_sortants)))
cat(sprintf("Moyenne communs par mois: %s\n", 
            format(round(mean(base_flux$communs)), big.mark = " ")))

# =============================================================================
# MATRICE DE TRANSITION (optionnel - toutes les paires de périodes)
# =============================================================================

cat("\n=== Voulez-vous la matrice complète ? (décommenter si besoin) ===\n")

# # Matrice complète de tous les flux (attention: peut être long)
# matrice_flux <- CJ(periode_1 = periodes, periode_2 = periodes)[periode_1 < periode_2]
# 
# matrice_flux[, c("communs", "only_p1", "only_p2") := {
#   id_p1 <- individus_par_periode[ANNEE_MOIS == periode_1, ID_INDIV]
#   id_p2 <- individus_par_periode[ANNEE_MOIS == periode_2, ID_INDIV]
#   
#   id_p1_dt <- data.table(ID_INDIV = id_p1, key = "ID_INDIV")
#   id_p2_dt <- data.table(ID_INDIV = id_p2, key = "ID_INDIV")
#   
#   n_comm <- nrow(fintersect(id_p1_dt, id_p2_dt))
#   
#   list(n_comm, length(id_p1) - n_comm, length(id_p2) - n_comm)
# }, by = .(periode_1, periode_2)]

# Nettoyer
dt_resultat[, ANNEE_MOIS := NULL]

# =============================================================================
# EXPORT (décommenter si besoin)
# =============================================================================

# fwrite(base_flux, "flux_individus_entre_periodes.csv")

cat("\n=== Terminé ===\n")