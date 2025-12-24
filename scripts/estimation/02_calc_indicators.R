# ============================================================
# 02_CALC_INDICATORS.R
# Calcul des indicateurs statistiques finaux
# ============================================================

library(dplyr)
library(tidyr)
library(openxlsx)
library(haven)

# Styles Excel
create_excel_styles <- function() {
  list(
    title = createStyle(fontSize = 14, textDecoration = "bold", halign = "center"),
    header = createStyle(textDecoration = "bold", border = "Bottom", halign = "center"),
    stat = createStyle(textDecoration = "bold", halign = "left"),
    cell = createStyle(numFmt = "#,##0.00", halign = "right")
  )
}

# Fonction de winsorisation
winsorize_vector <- function(x, p = 0.01) {
  q <- quantile(x, probs = c(p, 1 - p), na.rm = TRUE)
  pmax(pmin(x, q[2]), q[1])
}

# Créer une feuille de statistiques
create_salary_sheet <- function(data, group_var, sheet_name, sheet_title, wb,
                                apply_winsor = TRUE, winsor_perc = 0.01) {
  
  group_var_sym <- sym(group_var)
  styles <- create_excel_styles()
  
  stats <- data %>%
    filter(
      !is.na(SALAIRE_BRUT_MENS),
      !is.na(!!group_var_sym),
      !is.na(ANNEE), !is.na(MOIS)
    ) %>%
    mutate(GROUPE = as_factor(!!group_var_sym)) %>%
    group_by(GROUPE, ANNEE, MOIS) %>%
    mutate(
      SALAIRE_W = if (apply_winsor) {
        winsorize_vector(SALAIRE_BRUT_MENS, p = winsor_perc)
      } else {
        SALAIRE_BRUT_MENS
      },
      WEIGHT = w_final
    ) %>%
    summarise(
      N = n(),
      N_weighted = sum(WEIGHT, na.rm = TRUE),
      Min = min(SALAIRE_W, na.rm = TRUE),
      Q1 = compute_quantile_weighted(SALAIRE_W, WEIGHT, 0.25),
      Mediane = compute_quantile_weighted(SALAIRE_W, WEIGHT, 0.50),
      Moyenne = compute_mean_weighted(SALAIRE_W, WEIGHT),
      Q3 = compute_quantile_weighted(SALAIRE_W, WEIGHT, 0.75),
      Max = max(SALAIRE_W, na.rm = TRUE),
      .groups = "drop"
    )
  
  stats_long <- stats %>%
    pivot_longer(
      cols = N:Max,
      names_to = "Statistique",
      values_to = "Valeur"
    )
  
  stats_wide <- stats_long %>%
    unite("Annee_Mois", ANNEE, MOIS, sep = "_") %>%
    pivot_wider(
      names_from = Annee_Mois,
      values_from = Valeur
    ) %>%
    arrange(GROUPE, Statistique)
  
  addWorksheet(wb, sheet_name)
  
  writeData(wb, sheet_name, sheet_title, startRow = 1, startCol = 1)
  mergeCells(wb, sheet_name, cols = 1:ncol(stats_wide), rows = 1)
  addStyle(wb, sheet_name, styles$title, rows = 1, cols = 1)
  
  writeData(wb, sheet_name, stats_wide, startRow = 3, headerStyle = styles$header)
  
  addStyle(wb, sheet_name, styles$stat,
           rows = 4:(nrow(stats_wide) + 3), cols = 1:2, gridExpand = TRUE)
  addStyle(wb, sheet_name, styles$cell,
           rows = 4:(nrow(stats_wide) + 3), cols = 3:ncol(stats_wide),
           gridExpand = TRUE, stack = TRUE)
  
  freezePane(wb, sheet_name, firstActiveRow = 4, firstActiveCol = 3)
  setColWidths(wb, sheet_name, cols = 1:ncol(stats_wide), widths = "auto")
  
  return(stats_wide)
}

# Fonction principale
calculate_indicators <- function(input_file = NULL, output_file = NULL) {
  
  message("\n", strrep("=", 60))
  message("CALCUL DES INDICATEURS STATISTIQUES")
  message(strrep("=", 60))
  
  if (is.null(input_file)) {
    final_files <- list.files(PATHS$cleaned_final, 
                              pattern = "^data_cnps_final_.*\\.dta$",
                              full.names = TRUE)
    if (length(final_files) == 0) {
      # Essayer la base analytique
      final_files <- list.files(PATHS$cleaned_analytical, 
                                pattern = "^base_analytique_.*\\.dta$",
                                full.names = TRUE)
    }
    if (length(final_files) == 0) {
      stop("Aucun fichier final trouvé")
    }
    input_file <- final_files[which.max(file.mtime(final_files))]
  }
  
  message("Fichier source: ", input_file)
  
  df <- read_dta(input_file)
  message("Observations: ", format(nrow(df), big.mark = " "))
  
  # Extraire la période
  periods <- df %>% distinct(ANNEE, MOIS) %>% arrange(ANNEE, MOIS)
  min_p <- periods %>% slice(1)
  max_p <- periods %>% slice(n())
  
  period_str <- sprintf("%02d_%04d-%02d_%04d",
                        min_p$MOIS, min_p$ANNEE,
                        max_p$MOIS, max_p$ANNEE)
  
  if (is.null(output_file)) {
    output_file <- file.path(
      PATHS$indicators,
      paste0("stats_salaires_cnps_", period_str, "_", get_timestamp(), ".xlsx")
    )
  }
  
  # Créer le workbook
  wb <- createWorkbook()
  
  apply_winsor <- PROCESSING$apply_winsor
  winsor_perc <- PROCESSING$winsor_percentile
  
  message("\nWinsorisation: ", ifelse(apply_winsor, "Oui", "Non"))
  
  results <- list()
  
  # Créer les feuilles pour chaque dimension
  for (dim_name in names(STAT_DIMENSIONS)) {
    if (dim_name == "global") next
    
    dim_config <- STAT_DIMENSIONS[[dim_name]]
    if (!dim_config$enabled) next
    
    var_name <- dim_config$variable
    
    if (!var_name %in% names(df)) {
      message("  ⚠ Variable absente: ", var_name)
      next
    }
    
    message("\n→ ", dim_config$label)
    
    tryCatch({
      results[[dim_name]] <- create_salary_sheet(
        data = df,
        group_var = var_name,
        sheet_name = gsub(" ", "_", dim_config$label),
        sheet_title = paste("Salaire mensuel brut –", dim_config$label),
        wb = wb,
        apply_winsor = apply_winsor,
        winsor_perc = winsor_perc
      )
      message("  ✓ Créée")
    }, error = function(e) {
      message("  ✖ Erreur: ", e$message)
    })
  }
  
  # Feuille métadonnées
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
  
  # Sauvegarder
  ensure_dir(dirname(output_file))
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  message("\n", strrep("-", 40))
  message("Fichier de résultats: ", output_file)
  
  return(list(
    output_file = output_file,
    period = period_str,
    sheets_created = length(results)
  ))
}

message("Module de calcul des indicateurs chargé")