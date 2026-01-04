# =============================================================================
# Supprimer les doublons par ID_INDIV puis compter les manquants
# =============================================================================

library(data.table)

if (!is.data.table(dt_resultat)) {
  setDT(dt_resultat)
}

# Supprimer les doublons par ID_INDIV, MOIS, ANNEE (garder la première occurrence)
dt_unique <- unique(dt_resultat, by = c("ID_INDIV", "MOIS", "ANNEE"))

cat(sprintf("Avant dédoublonnage: %s lignes\n", format(nrow(dt_resultat), big.mark = " ")))
cat(sprintf("Après dédoublonnage: %s lignes\n", format(nrow(dt_unique), big.mark = " ")))
cat(sprintf("Doublons supprimés: %s\n\n", format(nrow(dt_resultat) - nrow(dt_unique), big.mark = " ")))

# Individus avec >= 2 mois manquants (périodes triées)
individus_manquants <- dt_unique[is.na(SALAIRE_BRUT)][order(ID_INDIV, ANNEE, MOIS), .(
  nb_mois_manquants = .N,
  periodes_manquantes = paste(paste(ANNEE, sprintf("%02d", MOIS), sep = "_"), collapse = ", ")
), by = ID_INDIV][nb_mois_manquants >= 2]

cat(sprintf("Individus avec >= 2 mois manquants: %s\n\n", 
            format(nrow(individus_manquants), big.mark = " ")))

# Aperçu
print(head(individus_manquants, 20))

# Distribution
cat("\n=== Distribution ===\n")
print(individus_manquants[, .N, keyby = nb_mois_manquants])