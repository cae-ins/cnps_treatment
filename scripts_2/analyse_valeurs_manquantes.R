# =============================================================================
# Analyse optimisée des valeurs manquantes - 27M lignes
# =============================================================================

library(data.table)

# S'assurer que c'est un data.table (pas de copie si déjà le cas)
if (!is.data.table(dt_resultat)) {
  setDT(dt_resultat)
}

# Travailler uniquement sur les colonnes nécessaires (évite de charger les 49 vars)
cols_needed <- c("SALAIRE_BRUT", "MOIS", "ANNEE")

# --- 1. Par ANNEE ---
cat("=== Par ANNEE ===\n")
print(dt_resultat[, .(
  Total = .N, 
  Manquants = sum(is.na(SALAIRE_BRUT)), 
  Pct = round(sum(is.na(SALAIRE_BRUT))/.N*100, 2)
), keyby = ANNEE])

# --- 2. Par MOIS ---
cat("\n=== Par MOIS ===\n")
print(dt_resultat[, .(
  Total = .N, 
  Manquants = sum(is.na(SALAIRE_BRUT)), 
  Pct = round(sum(is.na(SALAIRE_BRUT))/.N*100, 2)
), keyby = MOIS])

# --- 3. Croisé ANNEE x MOIS ---
cat("\n=== Croisé ANNEE x MOIS ===\n")
tab_croise <- dt_resultat[, .(
  Total = .N, 
  Manquants = sum(is.na(SALAIRE_BRUT)), 
  Pct = round(sum(is.na(SALAIRE_BRUT))/.N*100, 2)
), keyby = .(ANNEE, MOIS)]
print(tab_croise)

# --- 4. Tableau pivot (% manquants) ---
cat("\n=== Pivot: % manquants (ANNEE x MOIS) ===\n")
print(dcast(tab_croise, ANNEE ~ MOIS, value.var = "Pct", fill = NA))

# --- 5. Résumé global ---
n_miss <- dt_resultat[, sum(is.na(SALAIRE_BRUT))]
n_tot <- nrow(dt_resultat)
cat(sprintf("\nTotal: %s | Manquants: %s (%.2f%%)\n", 
            format(n_tot, big.mark = " "), 
            format(n_miss, big.mark = " "), 
            n_miss/n_tot*100))