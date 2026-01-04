# =============================================================================
# Employeurs n'ayant pas déclaré les salaires >= 2 fois pour AUCUN salarié
# =============================================================================

library(data.table)

# Compter les manquants par employeur et individu
manquants_par_emp_indiv <- dt_unique[, .(
  nb_manquants = sum(is.na(SALAIRE_BRUT))
), keyby = .(NUMERO_EMPLOYEUR, ID_INDIV)]

# Pour chaque employeur, vérifier si AU MOINS UN salarié a >= 2 manquants
employeurs_avec_recidive <- manquants_par_emp_indiv[nb_manquants >= 2, unique(NUMERO_EMPLOYEUR)]

# Employeurs où AUCUN salarié n'a >= 2 manquants
employeurs_sans_recidive <- setdiff(
  unique(dt_unique$NUMERO_EMPLOYEUR),
  employeurs_avec_recidive
)

cat(sprintf("Employeurs totaux: %s\n", 
            format(uniqueN(dt_unique$NUMERO_EMPLOYEUR), big.mark = " ")))
cat(sprintf("Employeurs avec au moins 1 salarié non déclaré >= 2 fois: %s\n", 
            format(length(employeurs_avec_recidive), big.mark = " ")))
cat(sprintf("Employeurs sans récidive (aucun salarié non déclaré >= 2 fois): %s\n", 
            format(length(employeurs_sans_recidive), big.mark = " ")))

# =============================================================================
# Créer la base des employeurs "récidivistes" (>= 2 non-déclarations)
# =============================================================================

gc()

base_employeurs_recidive <- dt_unique[NUMERO_EMPLOYEUR %in% employeurs_avec_recidive, .(
  nb_salaries = uniqueN(ID_INDIV),
  nb_salaries_manq_2plus = uniqueN(ID_INDIV[is.na(SALAIRE_BRUT)][duplicated(ID_INDIV[is.na(SALAIRE_BRUT)]) | 
                                                                   duplicated(ID_INDIV[is.na(SALAIRE_BRUT)], fromLast = TRUE)]),
  nb_periodes = uniqueN(paste(ANNEE, MOIS)),
  total_manquants = sum(is.na(SALAIRE_BRUT)),
  pct_manquants = round(sum(is.na(SALAIRE_BRUT)) / .N * 100, 2)
), keyby = NUMERO_EMPLOYEUR]

# Correction du calcul nb_salaries_manq_2plus (plus fiable)
temp <- manquants_par_emp_indiv[nb_manquants >= 2, .(
  nb_salaries_manq_2plus = .N
), keyby = NUMERO_EMPLOYEUR]

base_employeurs_recidive <- merge(
  base_employeurs_recidive[, -"nb_salaries_manq_2plus"],
  temp,
  by = "NUMERO_EMPLOYEUR"
)

# =============================================================================
# Résumé
# =============================================================================

cat("\n=== Aperçu des employeurs récidivistes ===\n")
print(head(base_employeurs_recidive[order(-nb_salaries_manq_2plus)], 20))

cat("\n=== Distribution du nb de salariés non déclarés >= 2 fois ===\n")
print(base_employeurs_recidive[, .N, keyby = .(nb_salaries_manq_2plus = pmin(nb_salaries_manq_2plus, 10))][order(nb_salaries_manq_2plus)])

# =============================================================================
# Export (décommenter si besoin)
# =============================================================================

# fwrite(base_employeurs_recidive, "employeurs_recidivistes.csv")