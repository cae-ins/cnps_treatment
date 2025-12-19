# ============================================================
# 05_CALC_INDICATORS.R
# Calcul des indicateurs statistiques
# ============================================================

# ------------------------------------------------------------
# CHARGER LA CONFIGURATION
# ------------------------------------------------------------
if (exists("PIPELINE_ROOT")) {
  config_path <- file.path(PIPELINE_ROOT, "config/config.R")
} else {
  config_path <- file.path(dirname(sys.frame(1)$ofile), "../config/config.R")
}
source(config_path)

# ------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------
library(tidyverse)
library(haven)
library(openxlsx)

# ------------------------------------------------------------
# STYLES EXCEL
# ------------------------------------------------------------
style_title <- createStyle(
  fontSize = 14, textDecoration = "bold", halign = "center"
)

style_header <- createStyle(
  textDecoration = "bold",
  border = "Bottom",
  halign = "center",
  valign = "center"
)

style_stat <- createStyle(
  textDecoration = "bold",
  halign = "left"
)

style_cell <- createStyle(
  numFmt = "#,##0.00",
  halign = "right"
)

# ------------------------------------------------------------
# FONCTION DE WINSORISATION
# ------------------------------------------------------------
winsorize_vector <- function(x, p = 0.01) {
  q <- quantile(x, probs = c(p, 1 - p), na.rm = TRUE)
  pmax(pmin(x, q[2]), q[1])
}

# ------------------------------------------------------------
# FONCTION DE CREATION DES TABLEAUX
# ------------------------------------------------------------
create_salary_sheet <- function(data, group_var, sheet_name, sheet_title, wb,
                                 apply_winsor = TRUE, winsor_perc = 0.01) {
  
  group_var_sym <- sym(group_var)
  
  stats <- data %>%
    filter(
      !is.na(SALAIRE_BRUT_MENS),
      !is.na(!!group_var_sym),
      !is.na(ANNEE),
      !is.na(MOIS)
    ) %>%
    mutate(
      GROUPE = as_factor(!!group_var_sym)
    ) %>%
    group_by(GROUPE, ANNEE, MOIS) %>%
    mutate(
      SALAIRE_BRUT_W = if (apply_winsor) {
        winsorize_vector(SALAIRE_BRUT_MENS, p = winsor_perc)
      } else {
        SALAIRE_BRUT_MENS
      }
    ) %>%
    summarise(
      Min     = min(SALAIRE_BRUT_W, na.rm = TRUE),
      Q1      = quantile(SALAIRE_BRUT_W, 0.25, na.rm = TRUE),
      Mediane = median(SALAIRE_BRUT_W, na.rm = TRUE),
      Q3      = quantile(SALAIRE_BRUT_W, 0.75, na.rm = TRUE),
      Max     = max(SALAIRE_BRUT_W, na.rm = TRUE),
      Moyenne = mean(SALAIRE_BRUT_W, na.rm = TRUE),
      N       = n(),
      .groups = "drop"
    )
  
  stats_long <- stats %>%
    pivot_longer(
      cols = Min:N,
      names_to = "Statistique",
      values_to = "Valeur"
    )
  
  stats_wide <- stats_long %>%
    unite("Annee_Mois", ANNEE, MOIS, sep = "_") %>%
    pivot_wider(
      names_from  = Annee_Mois,
      values_from = Valeur
    ) %>%
    arrange(GROUPE, Statistique)
  
  addWorksheet(wb, sheet_name)
  
  writeData(wb, sheet_name, sheet_title, startRow = 1, startCol = 1)
  mergeCells(wb, sheet_name, cols = 1:ncol(stats_wide), rows = 1)
  addStyle(wb, sheet_name, style_title, rows = 1, cols = 1, gridExpand = TRUE)
  
  writeData(wb, sheet_name, stats_wide, startRow = 3, headerStyle = style_header)
  
  addStyle(
    wb, sheet_name, style_stat,
    rows = 4:(nrow(stats_wide) + 3),
    cols = 1:2,
    gridExpand = TRUE
  )
  
  addStyle(
    wb, sheet_name, style_cell,
    rows = 4:(nrow(stats_wide) + 3),
    cols = 3:ncol(stats_wide),
    gridExpand = TRUE,
    stack = TRUE
  )
  
  freezePane(wb, sheet_name, firstActiveRow = 4, firstActiveCol = 3)
  setColWidths(wb, sheet_name, cols = 1:ncol(stats_wide), widths = "auto")
  
  return(stats_wide)
}

# ------------------------------------------------------------
# FONCTION PRINCIPALE
# ------------------------------------------------------------
calculate_indicators <- function(input_file = NULL, output_file = NULL) {
  
  message("\n", strrep("=", 60))
  message("CALCUL DES INDICATEURS STATISTIQUES")
  message(strrep("=", 60))
  
  # Paramètres par défaut
  if (is.null(input_file)) {
    # Trouver le fichier final le plus récent
    final_files <- list.files(PATHS$cleaned_final, 
                               pattern = "^data_cnps_final_.*\\.dta$",
                               full.names = TRUE)
    if (length(final_files) == 0) {
      stop("Aucun fichier final trouvé dans ", PATHS$cleaned_final)
    }
    input_file <- final_files[which.max(file.mtime(final_files))]
  }
  
  message("Fichier source: ", input_file)
  
  # ------------------------------------------------------------
  # CHARGEMENT
  # ------------------------------------------------------------
  df <- read_dta(input_file)
  message("Observations: ", format(nrow(df), big.mark = " "))
  
  # Extraire la période
  periods <- df %>%
    distinct(ANNEE, MOIS) %>%
    arrange(ANNEE, MOIS)
  
  min_p <- periods %>% slice(1)
  max_p <- periods %>% slice(n())
  
  period_str <- sprintf("%02d_%04d-%02d_%04d",
                        min_p$MOIS, min_p$ANNEE,
                        max_p$MOIS, max_p$ANNEE)
  
  # ------------------------------------------------------------
  # NOM DU FICHIER DE SORTIE
  # ------------------------------------------------------------
  if (is.null(output_file)) {
    timestamp <- get_timestamp("%d_%m_%Y")
    output_file <- file.path(
      PATHS$results,
      paste0("stats_salaires_cnps_", period_str, "_", timestamp, ".xlsx")
    )
  }
  
  # ------------------------------------------------------------
  # CRÉATION DU WORKBOOK
  # ------------------------------------------------------------
  wb <- createWorkbook()
  
  # Paramètres de winsorisation
  apply_winsor <- PROCESSING$apply_winsor
  winsor_perc  <- PROCESSING$winsor_percentile
  
  message("\nWinsorisation: ", ifelse(apply_winsor, "Oui", "Non"))
  if (apply_winsor) message("  Percentile: ", winsor_perc * 100, "%")
  
  # ------------------------------------------------------------
  # CRÉATION DES FEUILLES
  # ------------------------------------------------------------
  results <- list()
  
  for (group_name in names(STAT_GROUPS)) {
    group_config <- STAT_GROUPS[[group_name]]
    
    message("\n→ ", group_config$title)
    
    # Vérifier que la variable existe
    if (!group_config$var %in% names(df)) {
      message("  ⚠ Variable absente: ", group_config$var)
      next
    }
    
    tryCatch({
      results[[group_name]] <- create_salary_sheet(
        data        = df,
        group_var   = group_config$var,
        sheet_name  = group_config$sheet,
        sheet_title = group_config$title,
        wb          = wb,
        apply_winsor = apply_winsor,
        winsor_perc  = winsor_perc
      )
      message("  ✓ Créée")
    }, error = function(e) {
      message("  ✖ Erreur: ", e$message)
    })
  }
  
  # ------------------------------------------------------------
  # FEUILLE MÉTADONNÉES
  # ------------------------------------------------------------
  addWorksheet(wb, "Metadata")
  
  metadata <- tibble(
    Parametre = c("Fichier source", "Date création", "Période couverte",
                  "Observations", "Winsorisation", "Percentile winsor"),
    Valeur = c(basename(input_file), as.character(Sys.time()), period_str,
               format(nrow(df), big.mark = " "),
               ifelse(apply_winsor, "Oui", "Non"),
               paste0(winsor_perc * 100, "%"))
  )
  
  writeData(wb, "Metadata", metadata)
  
  # ------------------------------------------------------------
  # SAUVEGARDE
  # ------------------------------------------------------------
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  message("\n", strrep("-", 40))
  message("Fichier de résultats: ", output_file)
  
  return(list(
    output_file = output_file,
    period = period_str,
    sheets_created = length(results)
  ))
}

# ------------------------------------------------------------
# EXECUTION SI APPELÉ DIRECTEMENT
# ------------------------------------------------------------
if (sys.nframe() == 0) {
  calculate_indicators()
}