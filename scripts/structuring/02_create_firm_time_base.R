# ============================================================
# 02_CREATE_FIRM_TIME_BASE.R
# Création de la base entreprise-temps (B_jt)
# Utilise les variables créées dans 03_data_cleaning.R
# ============================================================

library(haven)
library(dplyr)
library(tidyr)

# Fonction principale
create_firm_time_base <- function(df_individual = NULL, output_file = NULL) {
  
  message("\n", strrep("=", 60))
  message("CRÉATION DE LA BASE ENTREPRISE-TEMPS")
  message(strrep("=", 60))
  
  # ----------------------------------------------------------
  # 1. CHARGEMENT DES DONNÉES
  # ----------------------------------------------------------
  if (is.null(df_individual)) {
    ind_files <- list.files(
      PATHS$cleaned_individual, 
      pattern = "^base_individuelle_.*\\.dta$",
      full.names = TRUE
    )
    if (length(ind_files) == 0) {
      stop("Aucune base individuelle trouvée")
    }
    df_individual <- read_dta(ind_files[which.max(file.mtime(ind_files))])
  }
  
  message("Observations individuelles: ", format(nrow(df_individual), big.mark = " "))
  
  # ----------------------------------------------------------
  # 2. AGRÉGATION AU NIVEAU ENTREPRISE-MOIS
  # ----------------------------------------------------------
  message("\n  Agrégation au niveau entreprise-mois...")
  
  df_firm_time <- df_individual %>%
    group_by(NUMERO_EMPLOYEUR, ANNEE, MOIS) %>%
    summarise(
      # Effectifs
      N_SALARIES = n(),
      N_SALARIES_DISTINCTS = n_distinct(ID_INDIV),
      
      # Salaires (utilise SALAIRE_BRUT_MENS de 03_data_cleaning.R)
      SALAIRE_MOYEN = mean(SALAIRE_BRUT_MENS, na.rm = TRUE),
      SALAIRE_MEDIAN = median(SALAIRE_BRUT_MENS, na.rm = TRUE),
      SALAIRE_TOTAL = sum(SALAIRE_BRUT_MENS, na.rm = TRUE),
      SALAIRE_MIN = min(SALAIRE_BRUT_MENS, na.rm = TRUE),
      SALAIRE_MAX = max(SALAIRE_BRUT_MENS, na.rm = TRUE),
      SALAIRE_SD = sd(SALAIRE_BRUT_MENS, na.rm = TRUE),
      
      # Caractéristiques entreprise (première valeur non NA)
      # SECTOR_CODE = first(na.omit(SECTOR_CODE)),
      SECTEUR_ACTIVITE_COD = first(na.omit(SECTEUR_ACTIVITE_COD)),
      RAISON_SOCIALE = first(na.omit(RAISON_SOCIALE)),
      COMMUNE = first(na.omit(COMMUNE)),
      DATE_IMMAT_EMPLOYEUR = first(na.omit(DATE_IMMAT_EMPLOYEUR)),
      EFFECTIF_SALARIES = first(na.omit(EFFECTIF_SALARIES)),
      
      # Classes d'effectif (utilise celles de 03_data_cleaning.R)
      CLASSE_EFFECTIF = first(na.omit(CLASSE_EFFECTIF)),
      CLASSE_EFFECTIF_REDUITE = first(na.omit(CLASSE_EFFECTIF_REDUITE)),
      
      # Âge entreprise (utilise AGE_ENTREPRISE_IMMAT de 03_data_cleaning.R)
      AGE_ENTREPRISE = first(na.omit(AGE_ENTREPRISE_IMMAT)),
      CL_AGE_ENTREPRISE = first(na.omit(CL_AGE_ENTREPRISE_IMMAT)),
      CL_RED_AGE_ENTREPRISE = first(na.omit(CL_RED_AGE_ENTREPRISE_IMMAT)),
      
      # Composition
      PCT_FEMMES = mean(SEXE == "F", na.rm = TRUE),
      ANCIENNETE_MOYENNE = mean(ANCIENNETE_ENTREPRISE, na.rm = TRUE),
      AGE_MOYEN_EMPLOYES = mean(AGE_EMPLOYE, na.rm = TRUE),
      
      .groups = "drop"
    )
  
  message("    Entreprises-mois: ", format(nrow(df_firm_time), big.mark = " "))
  
  # ----------------------------------------------------------
  # 3. AJOUT DES VARIABLES DÉRIVÉES
  # ----------------------------------------------------------
  message("\n  Ajout des variables dérivées...")
  
  df_firm_time <- df_firm_time %>%
    mutate(
      # Période (utilise le même format que 03_data_cleaning.R)
      PERIOD = paste(ANNEE, sprintf("%02d", MOIS), sep = "_"),
      
      # Log du salaire moyen (pour les modèles)
      log_Y_bar_jt = log(SALAIRE_MOYEN + 1),
      
      # Indicateur de déclaration (1 = déclarant)
      D_jt = 1
    )
  
  # ----------------------------------------------------------
  # 4. CRÉATION DU PANEL COMPLET
  # ----------------------------------------------------------
  message("\n  Création du panel complet...")
  
  # Toutes les entreprises observées
  all_firms <- unique(df_firm_time$NUMERO_EMPLOYEUR)
  
  # Toutes les périodes observées
  all_periods <- df_firm_time %>%
    distinct(ANNEE, MOIS) %>%
    arrange(ANNEE, MOIS)
  
  # Panel complet
  panel_complet <- expand_grid(
    NUMERO_EMPLOYEUR = all_firms,
    all_periods
  )
  
  # Joindre avec les observations
  df_firm_time_full <- panel_complet %>%
    left_join(df_firm_time, by = c("NUMERO_EMPLOYEUR", "ANNEE", "MOIS")) %>%
    mutate(
      D_jt = ifelse(is.na(D_jt), 0, D_jt),
      PERIOD = paste(ANNEE, sprintf("%02d", MOIS), sep = "_")
    )
  
  # ----------------------------------------------------------
  # 5. PROPAGATION DES CARACTÉRISTIQUES FIXES
  # ----------------------------------------------------------
  message("  Propagation des caractéristiques fixes...")
  
  df_firm_time_full <- df_firm_time_full %>%
    group_by(NUMERO_EMPLOYEUR) %>%
    fill(
      # SECTOR_CODE, 
      SECTEUR_ACTIVITE_COD, RAISON_SOCIALE, COMMUNE,
      DATE_IMMAT_EMPLOYEUR, EFFECTIF_SALARIES,
      CLASSE_EFFECTIF, CLASSE_EFFECTIF_REDUITE,
      AGE_ENTREPRISE, CL_AGE_ENTREPRISE, CL_RED_AGE_ENTREPRISE,
      .direction = "downup"
    ) %>%
    ungroup()
  
  # ----------------------------------------------------------
  # 6. CALCUL DES LAGS (HISTORIQUE)
  # ----------------------------------------------------------
  message("  Calcul de l'historique de déclaration...")
  
  df_firm_time_full <- df_firm_time_full %>%
    arrange(NUMERO_EMPLOYEUR, ANNEE, MOIS) %>%
    group_by(NUMERO_EMPLOYEUR) %>%
    mutate(
      # Lags de déclaration
      D_jt_lag1 = lag(D_jt, 1),
      D_jt_lag2 = lag(D_jt, 2),
      
      # Nombre de déclarations passées
      N_DECLARATIONS_PASSEES = cumsum(lag(D_jt, default = 0)),
      
      # Pourcentage de déclarations passées
      N_PERIODES_PASSEES = row_number() - 1,
      PCT_DECLARATIONS_PASSEES = ifelse(
        N_PERIODES_PASSEES > 0,
        N_DECLARATIONS_PASSEES / N_PERIODES_PASSEES,
        NA_real_
      ),
      
      # Lags du salaire moyen
      log_Y_bar_jt_lag1 = lag(log_Y_bar_jt, 1),
      log_Y_bar_jt_lag2 = lag(log_Y_bar_jt, 2),
      
      # Effectif moyen passé
      EFFECTIF_MOYEN_PASSE = lag(cummean(N_SALARIES), 1)
    ) %>%
    ungroup()
  
  # ----------------------------------------------------------
  # 7. STATISTIQUES
  # ----------------------------------------------------------
  n_entreprises <- n_distinct(df_firm_time_full$NUMERO_EMPLOYEUR)
  n_periodes <- n_distinct(df_firm_time_full$PERIOD)
  taux_declaration <- mean(df_firm_time_full$D_jt) * 100
  
  message("\n  Résumé du panel:")
  message("    Entreprises: ", format(n_entreprises, big.mark = " "))
  message("    Périodes: ", n_periodes)
  message("    Observations: ", format(nrow(df_firm_time_full), big.mark = " "))
  message("    Taux de déclaration: ", round(taux_declaration, 1), "%")
  
  # ----------------------------------------------------------
  # 8. SAUVEGARDE
  # ----------------------------------------------------------
  if (is.null(output_file)) {
    output_file <- file.path(
      PATHS$cleaned_firm_time,
      paste0("base_entreprise_temps_", get_timestamp(), ".dta")
    )
  }
  
  ensure_dir(dirname(output_file))
  write_dta(df_firm_time_full, output_file, version = STATA_VERSION)
  
  message("\n", strrep("-", 40))
  message("Base entreprise-temps créée: ", output_file)
  
  return(list(
    output_file = output_file,
    data = df_firm_time_full,
    n_firms = n_entreprises,
    n_periods = n_periodes,
    n_obs = nrow(df_firm_time_full),
    declaration_rate = taux_declaration / 100
  ))
}

message("Module de création base entreprise-temps chargé")