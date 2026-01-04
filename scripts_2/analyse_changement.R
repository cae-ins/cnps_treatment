# =============================================================================
# Analyse des changements (optimisé mémoire)
# =============================================================================

library(data.table)

# Récupérer les ID des individus concernés
ids_manquants <- individus_manquants$ID_INDIV

# Forcer le garbage collector
gc()

# Approche directe sans copie intermédiaire
analyse_changements <- dt_unique[ID_INDIV %in% ids_manquants, .(
  nb_periodes = .N,
  nb_employeurs = uniqueN(NUMERO_EMPLOYEUR, na.rm = TRUE),
  nb_professions = uniqueN(libelle_CITP_final, na.rm = TRUE),
  a_change_employeur = uniqueN(NUMERO_EMPLOYEUR, na.rm = TRUE) > 1,
  a_change_profession = uniqueN(libelle_CITP_final, na.rm = TRUE) > 1
), keyby = ID_INDIV]

gc()

# Joindre avec le nombre de mois manquants
analyse_changements <- merge(
  analyse_changements, 
  individus_manquants[, .(ID_INDIV, nb_mois_manquants)],
  by = "ID_INDIV"
)

# 1. Changement des deux (employeur ET profession)
change_deux <- analyse_changements[a_change_employeur == TRUE & a_change_profession == TRUE]

# 2. Aucun changement
change_aucun <- analyse_changements[a_change_employeur == FALSE & a_change_profession == FALSE]

# 3. Changement employeur uniquement
change_employeur_seul <- analyse_changements[a_change_employeur == TRUE & a_change_profession == FALSE]

# 4. Changement profession uniquement
change_profession_seul <- analyse_changements[a_change_employeur == FALSE & a_change_profession == TRUE]


# =============================================================================
# Résumé
# =============================================================================

cat("=== Résumé des changements ===\n\n")

n_total <- nrow(analyse_changements)

cat(sprintf("Ont changé d'employeur: %s (%.1f%%)\n",
            format(sum(analyse_changements$a_change_employeur), big.mark = " "),
            mean(analyse_changements$a_change_employeur) * 100))

cat(sprintf("Ont changé de profession: %s (%.1f%%)\n",
            format(sum(analyse_changements$a_change_profession), big.mark = " "),
            mean(analyse_changements$a_change_profession) * 100))

cat(sprintf("Ont changé les deux: %s (%.1f%%)\n",
            format(sum(analyse_changements$a_change_employeur & analyse_changements$a_change_profession), big.mark = " "),
            mean(analyse_changements$a_change_employeur & analyse_changements$a_change_profession) * 100))

cat(sprintf("Aucun changement: %s (%.1f%%)\n",
            format(sum(!analyse_changements$a_change_employeur & !analyse_changements$a_change_profession), big.mark = " "),
            mean(!analyse_changements$a_change_employeur & !analyse_changements$a_change_profession) * 100))

# Distribution
cat("\n=== Nb employeurs ===\n")
print(analyse_changements[, .N, keyby = nb_employeurs])

cat("\n=== Nb professions ===\n")
print(analyse_changements[, .N, keyby = nb_professions])

# Aperçu
cat("\n=== Aperçu ===\n")
print(head(analyse_changements, 20))