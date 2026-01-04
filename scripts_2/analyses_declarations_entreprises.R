# =============================================================================
# Analyse des déclarations par entreprise et par individu
# =============================================================================

library(data.table)

# S'assurer que c'est un data.table
if (!is.data.table(dt_resultat)) {
  setDT(dt_resultat)
}

# Créer la clé ANNEE_MOIS une seule fois
dt_resultat[, ANNEE_MOIS := paste(ANNEE, sprintf("%02d", MOIS), sep = "_")]

# =============================================================================
# 1. DÉCLARATIONS PAR ENTREPRISE PAR MOIS
# =============================================================================

cat("=== Construction de la base par ENTREPRISE ===\n")

# Agréger par entreprise et période
agg_entreprise <- dt_resultat[, .(
  nb_decla = sum(!is.na(SALAIRE_BRUT)),
  nb_manq = sum(is.na(SALAIRE_BRUT))
), keyby = .(NUMERO_EMPLOYEUR, ANNEE_MOIS)]

# Pivoter pour avoir les colonnes decla et manq par période
base_entreprise <- dcast(
  agg_entreprise,
  NUMERO_EMPLOYEUR ~ ANNEE_MOIS,
  value.var = c("nb_decla", "nb_manq"),
  fill = 0,
  sep = "_"
)

# Renommer les colonnes pour plus de clarté (nb_decla_2023_01 -> 2023_01_decla)
old_names <- names(base_entreprise)[-1]
new_names <- gsub("^nb_decla_(.*)$", "\\1_decla", old_names)
new_names <- gsub("^nb_manq_(.*)$", "\\1_manq", new_names)
setnames(base_entreprise, old_names, new_names)

# Réordonner les colonnes (alterner decla/manq par période)
periodes <- sort(unique(dt_resultat$ANNEE_MOIS))
cols_ordered <- c("NUMERO_EMPLOYEUR", 
                  as.vector(rbind(paste0(periodes, "_decla"), 
                                  paste0(periodes, "_manq"))))
cols_ordered <- cols_ordered[cols_ordered %in% names(base_entreprise)]
setcolorder(base_entreprise, cols_ordered)

cat(sprintf("Base entreprise: %s lignes x %d colonnes\n", 
            format(nrow(base_entreprise), big.mark = " "), ncol(base_entreprise)))

# Aperçu
print(head(base_entreprise))

# =============================================================================
# 2. INDICATEUR DE DÉCLARATION PAR INDIVIDU PAR MOIS
# =============================================================================

cat("\n=== Construction de la base par INDIVIDU ===\n")

# Créer l'indicateur de déclaration (1 = déclaré, 0 = non déclaré/manquant)
agg_individu <- dt_resultat[, .(
  a_declare = as.integer(!is.na(SALAIRE_BRUT))
), keyby = .(ID_INDIV, ANNEE_MOIS)]

# Pivoter pour avoir une colonne par période
base_individu <- dcast(
  agg_individu,
  ID_INDIV ~ ANNEE_MOIS,
  value.var = "a_declare",
  fill = 0  # Si pas de ligne pour cette période = non déclaré
)

cat(sprintf("Base individu: %s lignes x %d colonnes\n", 
            format(nrow(base_individu), big.mark = " "), ncol(base_individu)))

# Aperçu
print(head(base_individu))

# =============================================================================
# RÉSUMÉ
# =============================================================================

cat("\n=== Résumé ===\n")
cat(sprintf("Périodes couvertes: %s à %s (%d mois)\n", 
            min(periodes), max(periodes), length(periodes)))
cat(sprintf("Nombre d'entreprises: %s\n", format(nrow(base_entreprise), big.mark = " ")))
cat(sprintf("Nombre d'individus: %s\n", format(nrow(base_individu), big.mark = " ")))

# Nettoyer la colonne temporaire
dt_resultat[, ANNEE_MOIS := NULL]

# =============================================================================
# EXPORT (décommenter si besoin)
# =============================================================================

# fwrite(base_entreprise, "declarations_par_entreprise.csv")
# fwrite(base_individu, "declarations_par_individu.csv")