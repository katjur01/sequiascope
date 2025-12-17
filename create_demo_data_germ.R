files <- c("input_files/MOII_e117/117_WES_germline/per_sample_final_var_tabs/tsv_formated/DZ1601krev.variants.tsv",
          "input_files/MOII_e117/117_WES_germline/per_sample_final_var_tabs/tsv_formated/MR1507krev.variants.tsv",
          "input_files/MOII_e117/117_WES_germline/per_sample_final_var_tabs/tsv_formated/VH0452krev.variants.tsv",
          "input_files/reanalysed_data/germline/per_sample_final_var_tabs/tsv_formated/FZ0711krev.variants.tsv",
          "input_files/reanalysed_data/germline/per_sample_final_var_tabs/tsv_formated/JM1629krev.variants.tsv",
          "input_files/reanalysed_data/germline/per_sample_final_var_tabs/tsv_formated/MP1556krev.variants.tsv",
          "input_files/reanalysed_data/germline/per_sample_final_var_tabs/tsv_formated/MP2102krev.variants.tsv",
          "input_files/reanalysed_data/germline/per_sample_final_var_tabs/tsv_formated/NT1162krev.variants.tsv")

# Funkce pro stratifikovaný výběr podle clinvar_sig
stratified_sample <- function(dt, n = 15, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # Definuj priority kategorií (od nejzajímavějších)
  priority_order <- c(
    "Pathogenic",
    "Likely pathogenic", 
    "Pathogenic/Likely pathogenic",
    "Uncertain significance",
    "Likely benign",
    "Benign",
    "Benign/Likely benign",
    NA,  # prázdné ClinVar
    ""
  )
  
  # Počty pro každou kategorii (celkem 15)
  target_counts <- list(
    "Pathogenic" = 1,
    "Likely pathogenic" = 1,
    "Pathogenic/Likely pathogenic" = 1,
    "Uncertain significance" = 3,
    "Likely benign" = 2,
    "Benign" = 2,
    "rest" = 5  # zbytek (NA nebo jiné)
  )
  
  sampled <- data.table()
  
  # Vyber z každé kategorie
  for (category in names(target_counts)) {
    if (category == "rest") {
      # Vyber z prázdných nebo ostatních
      subset <- dt[is.na(clinvar_sig) | clinvar_sig == "" | 
                     !clinvar_sig %in% names(target_counts)]
    } else {
      subset <- dt[clinvar_sig == category]
    }
    
    n_to_sample <- min(target_counts[[category]], nrow(subset))
    
    if (n_to_sample > 0) {
      sampled <- rbind(sampled, subset[sample(.N, n_to_sample)])
    }
  }
  
  # Pokud nemáme dost, doplň náhodně
  if (nrow(sampled) < n) {
    remaining <- dt[!sampled, on = names(dt)]
    n_more <- min(n - nrow(sampled), nrow(remaining))
    if (n_more > 0) {
      sampled <- rbind(sampled, remaining[sample(.N, n_more)])
    }
  }
  
  return(sampled)
}

# Vytvoření demo kolekce
demo_collection <- list()

for (i in seq_along(files)) {
  cat("Zpracovávám:", files[i], "\n")
  
  patient_data <- fread(files[i])
  
  # Stratifikovaný výběr
  demo_collection[[i]] <- stratified_sample(patient_data, n = 15, seed = i)
  demo_collection[[i]][, patient_id := paste0("DEMO_", i)]
  
  # Zobraz distribuci pro kontrolu
  cat("  ClinVar distribuce:\n")
  print(demo_collection[[i]][, .N, by = clinvar_sig])
}

# Spojit všechny
demo_data <- rbindlist(demo_collection, fill = TRUE)

fwrite(demo_data, "input_files/demo_data/DZ1601/germline/DZ1601.krev.variants.tsv",sep="\t")

