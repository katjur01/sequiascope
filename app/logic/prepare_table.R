# app/logic/prepare_table.R

box::use(
  data.table[fread,tstrsplit,setcolorder,setnames,uniqueN,dcast,fifelse,is.data.table,as.data.table,setorderv],
  shiny[observe],
  openxlsx[read.xlsx],
  scales[scientific_format],
  stats[setNames]
)
box::use(
  app/logic/load_data[get_inputs,load_data],
  app/logic/prepare_arriba_pictures[pdf2png],
  stringi[stri_trans_totitle]
)

#' @export
get_tissue_list <- function(){
  # tissue_list <- c("Liver","Lung")
  input_files <- get_inputs("per_sample_file")
  tissue_list <- sort(unique(gsub(".*/([^/]+?)_(all_genes|genes_of_interest).*", "\\1", input_files$expression.files)))
  return(tissue_list)
}

capitalize_first_letter <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  )
}

#' @export
replace_underscore_with_space <- function(dt, cols_to_clean) {
  if (!is.data.table(dt)) setDT(dt)
  
  existing_cols <- cols_to_clean[cols_to_clean %in% names(dt)]
  
  if (length(existing_cols) > 0) {
    # Nahrazení "_" za " "
    dt[, (existing_cols) := lapply(.SD, function(x) {
      if (is.character(x)) gsub("_", " ", x) else x
    }), .SDcols = existing_cols]
    
    # # Capitalize pouze první písmeno celého stringu
    # dt[, (existing_cols) := lapply(.SD, function(x) {
    #   if (is.character(x)) capitalize_first_letter(x) else x
    # }), .SDcols = existing_cols]
  }
  
  return(dt)
}


replace_dot_with_na <- function(dt) {
  dt[, (names(dt)) := lapply(.SD, function(x) fifelse(x == ".", NA, x))]
  return(dt)
}

clean_consequence <- function(x) {
  if (is.na(x) || trimws(x) == "") {
    return("missing value")
  } else {
    parts <- unlist(strsplit(x, ","))
    cleaned_parts <- trimws(parts)
    cleaned_parts <- cleaned_parts[cleaned_parts != ""]
    return(cleaned_parts)
  }
}
clean_clinvar_sig <- function(x) {
  if (is.na(x) || trimws(x) == "" || is.null(x)) {
    return("missing value")
  } else {
    parts <- unlist(strsplit(x, ","))
    all_parts <- unlist(strsplit(parts, "[|/]"))
    cleaned_parts <- trimws(gsub("^_+", "", all_parts))
    cleaned_parts <- cleaned_parts[cleaned_parts != ""]
    return(cleaned_parts)
  }
}
fast_lookup_column <- function(dt, input_col, output_col, clean_fun) {
  input_vector <- dt[[input_col]]
  input_vector_nafix <- ifelse(is.na(input_vector), "__NA__", input_vector)
  unique_vals <- unique(input_vector_nafix)
  
  lookup <- setNames(
    lapply(unique_vals, function(x) {
      if (x == "__NA__") {
        clean_fun(NA)
      } else {
        clean_fun(x)
      }
    }),
    unique_vals
  )
  
  dt[, (output_col) := lookup[input_vector_nafix]]
}


#' @export
prepare_fusion_genes_table <- function(sample, data, manifest_dt,all_colNames){
  
  normalize_keys <- function(dt) {
    dt <- as.data.table(dt)
    if ("chrom1" %in% names(dt)) setnames(dt, c("chrom1","chrom2"), c("chr1","chr2"))
    if ("pos1" %in% names(dt) && !is.numeric(dt$pos1)) dt[, pos1 := as.numeric(pos1)]
    if ("pos2" %in% names(dt) && !is.numeric(dt$pos2)) dt[, pos2 := as.numeric(pos2)]
    dt
  }
  
  data <- normalize_keys(data)
  
  if (is.null(manifest_dt)) {
    message(sprintf("[fusion] Manifest not found: www/manifests/fusion/%s.tsv", sample))
    data[, position1 := paste0(chr1, ":", pos1)]
    data[, position2 := paste0(chr2, ":", pos2)]
    data[,c("chr1","pos1","chr2","pos2") := NULL]
    data[, `:=`( svg_path = NA_character_, png_path = NA_character_,
                 has_svg = FALSE, has_png = FALSE)]
    return(data)
    
  } else {
    message(sprintf("[fusion] Merging base table with manifest for sample '%s'", sample))
    
    manifest_dt <- normalize_keys(manifest_dt)

    strip_dot_slash <- function(x) sub("^\\./", "", x)  # cesty nech relativní k www/ (bez "./")
    manifest_dt[, png_path := strip_dot_slash(png_path)]
    manifest_dt[, svg_path := strip_dot_slash(svg_path)]
    
    merge_dt <- merge(data, manifest_dt, by = c("gene1","gene2","chr1","pos1","chr2","pos2"), all.x = TRUE)
    
    merge_dt[, position1 := paste0(chr1, ":", pos1)]
    merge_dt[, position2 := paste0(chr2, ":", pos2)]
    merge_dt[,c("chr1","pos1","chr2","pos2") := NULL]
    
    merge_dt[, `:=`( has_svg = !is.na(svg_path) & nzchar(svg_path),
                     has_png = !is.na(png_path) & nzchar(png_path))]

    default_columns <- colFilter("fusion",all_colNames)$default_columns
    merge_dt[, `:=`(Visual_Check = "", Notes = "")]
    setcolorder(merge_dt, default_columns)
    
    message(paste0("Fusion genes, pacient ", unique(merge_dt$sample), " (prepare_table script)"))
    return(merge_dt)
  }
  
}


#' @export
prepare_somatic_table <- function(dt,all_colNames){
  dt <- replace_dot_with_na(dt)
  dt <- replace_underscore_with_space(dt, c("gene_region", "clinvar_sig", "Consequence", "clinvar_DBN"))
  cols_to_numeric <- c("gnomAD_NFE", "tumor_variant_freq")
  dt[, (cols_to_numeric) := lapply(.SD, as.numeric), .SDcols = cols_to_numeric]
  
  fast_lookup_column(dt, "Consequence", "consequence_trimws", clean_consequence)
  
  default_columns <- colFilter("somatic",all_colNames)$default_columns
  dt <- setcolorder(dt,default_columns)
  
  message(paste0("Somatic varcall, pacient ",unique(dt$sample)," (prepare_table script)"))
  return(dt)
}


#' @export
prepare_germline_table <- function(dt){
  dt <- replace_dot_with_na(dt)
  dt <- replace_underscore_with_space(dt, c("gene_region", "clinvar_sig", "Consequence", "clinvar_DBN"))
  cols_to_numeric <- c("gnomAD_NFE", "variant_freq")
  dt[, (cols_to_numeric) := lapply(.SD, as.numeric), .SDcols = cols_to_numeric]

  fast_lookup_column(dt, "Consequence", "consequence_trimws", clean_consequence)
  fast_lookup_column(dt, "clinvar_sig", "clinvar_trimws", clean_clinvar_sig)
  
  default_selection <- c("var_name","variant_freq","in_library","Gene_symbol","coverage_depth","gene_region",
                         "gnomAD_NFE","clinvar_sig","snpDB","CGC_Germline","trusight_genes","fOne","Consequence","HGVSc", "HGVSp","all_full_annot_name")
  setcolorder(dt, default_selection)

  message(paste0("Germline varcall, pacient ",unique(dt$sample)," (prepare_table script)"))
  return(dt)
}

#' @export
prepare_expression_table <- function(combined_dt) {
  # vyhoď "none", pokud by se tam náhodou připletlo
  combined_dt <- combined_dt[tissue != "none"]
  
  tissues <- unique(combined_dt$tissue)
  base_cols <- colnames(combined_dt)
  
  # sloupce pro UI (all/default) odvodíme JEŠTĚ na longu (base_cols)
  cols <- colFilter("expression", base_cols, tissues)
  
  # wide
  wide_dt <- dcast(
    combined_dt,
    sample + feature_name + geneid + refseq_id + type + all_kegg_gene_names +
      gene_definition + pathway + num_of_paths ~ tissue,
    value.var = c("log2FC","p_value","p_adj")
  )
  
  # pomocné metriky
  wide_dt[, mean_log2FC := rowMeans(.SD, na.rm = TRUE), .SDcols = patterns("^log2FC_")]
  
  # cílové pořadí sloupců – vezmeme defaulty + zbývající „log2FC_/p_value_/p_adj_“ dle tissues
  log2FC_cols <- paste0("log2FC_", tissues)
  p_value_cols <- paste0("p_value_", tissues)
  p_adj_cols   <- paste0("p_adj_", tissues)
  column_order <- c(cols$default_columns, as.vector(rbind(log2FC_cols, p_value_cols, p_adj_cols)))
  column_order <- intersect(column_order, names(wide_dt))  # jistota
  
  wide_dt <- wide_dt[, ..column_order]
  
  # formátování (ponechá NA)
  if (length(intersect(log2FC_cols, names(wide_dt)))) {
    wide_dt[, (intersect(log2FC_cols, names(wide_dt))) :=
              lapply(.SD, function(x) formatC(x, format = "fg")), .SDcols = intersect(log2FC_cols, names(wide_dt))]
  }
  if (length(intersect(p_value_cols, names(wide_dt)))) {
    wide_dt[, (intersect(p_value_cols, names(wide_dt))) :=
              lapply(.SD, function(x) formatC(x, format = "e", digits = 3)), .SDcols = intersect(p_value_cols, names(wide_dt))]
  }
  if (length(intersect(p_adj_cols, names(wide_dt)))) {
    wide_dt[, (intersect(p_adj_cols, names(wide_dt))) :=
              lapply(.SD, function(x) formatC(x, format = "e", digits = 3)), .SDcols = intersect(p_adj_cols, names(wide_dt))]
  }
  
  message(paste0("Expression profile, pacient ", paste(unique(wide_dt$sample), collapse = ", "), " (prepare_table)"))
  
  # vrátíme balíček: tabulka + sloupce + tissues
  list(
    dt = wide_dt,
    columns = cols,       # list(all_columns=..., default_columns=...)
    tissues = tissues
  )
}


#' @export
colFilter <- function(flag, all_column_var,tissues = NULL){
  if (flag == "somatic"){
    all_column_names  <- setdiff(all_column_var, c("sample"))  # dont show/add sample column in table
    default_selection <- c("var_name","in_library","Gene_symbol","tumor_variant_freq","tumor_depth","gene_region","gnomAD_NFE","clinvar_sig",
                           "clinvar_DBN","snpDB","CGC_Somatic","fOne","COSMIC","HGMD","Consequence","HGVSc", "HGVSp","all_full_annot_name")
  } else if (flag == "germline"){
    all_column_names  <- setdiff(all_column_var, c("sample"))  # dont show/add sample column in table
    default_selection <- c("var_name","variant_freq","in_library","Gene_symbol","coverage_depth","gene_region",
                           "gnomAD_NFE","clinvar_sig","snpDB","CGC_Germline","trusight_genes","fOne","Consequence","HGVSc", "HGVSp","all_full_annot_name")
    
  } else if (flag == "fusion"){
    filtered_all_column_var <- setdiff(all_column_var, c("chr1", "chr2", "pos1", "pos2"))
    
    all_column_names <- c(filtered_all_column_var,"Visual_Check","Notes","position1","position2")
    default_selection <- c("gene1","gene2","arriba.called","starfus.called","arriba.confidence","overall_support","Visual_Check","Notes","position1","strand1","position2","strand2",
                           "arriba.site1","arriba.site2","starfus.splice_type","DB_count","DB_list")

  } else if (flag == "expression"){
    filtered_all_column_var <- setdiff(all_column_var, c("log2FC","p_value","p_adj","sample","all_kegg_paths_name","counts_tpm_round","fc","size","mu","lower_than_p","higher_than_p"))
    
    # Generování dynamických sloupců pro každou tkáň
    log2FC_cols <- paste0("log2FC_", tissues)
    p_value_cols <- paste0("p_value_", tissues)
    p_adj_cols <- paste0("p_adj_", tissues)
    
    # # Kombinace do finálního pořadí sloupců
    all_column_names <- c(filtered_all_column_var, as.vector(rbind(log2FC_cols, p_value_cols, p_adj_cols)))
    default_selection <- c("sample", "feature_name", "geneid", "pathway", "mean_log2FC", as.vector(rbind(log2FC_cols, p_value_cols, p_adj_cols)))
    
  } else {
    print("NOT germline, expression or fusion")
  }

  ordered_columns <- factor(all_column_names, levels = default_selection)
  all_column_names_sorted <- all_column_names[order(ordered_columns)]

  return(list(all_columns = all_column_names_sorted, default_columns = default_selection))
}



#' @export
set_pathway_colors <- function(){
  pathway_colors <- c(
    "Metabolic Signaling" = "darkturquoise",
    "Chromatin Remodeling/DNA Methylation" = "orchid1",
    "PI3K/AKT1/MTOR Signaling" = "green4",
    "Mitogen Activated Protein (MAP) Kinase Signaling" = "maroon",
    "Receptor Tyrosine Kinase/Growth Factor Signaling" = "palegreen2",
    "Kinase Fusions" = "#D2691E",
    "apoptosis" = "#FF7F00",
    "WNTsignaling" = "#6495ED",
    "Hormone Signaling" = "#DC143C",
    "Cellular architecture and microenvironment" = "#6A5ACD", 
    "DNA damage/repair" = "skyblue2",
    "Cell cycle control" = "#FB9A99",
    "non-WNT/non-SHH medulloblastoma-related markers" = "khaki2",
    "Immune Checkpoints" = "#A9A9A9",
    "TGF-B Signaling" = "gold1",
    "Janus Kinase (JAK)/ (STAT) Signaling" = "#BDB76B",
    "Hedgehog Signaling" = "#8B008B"
  )
  return(pathway_colors)
}

# dt <- fread("./input_files/MOII_e117/117_WES_germline/final_variant_table.tsv")

#### fusion testing ####
#
# library("data.table")
# library("openxlsx")
# library <- "MOII_e117"
# fusion_genes_project <- "117_fusions"
# input_files <- list.files(paste("input_files", library, fusion_genes_project, "results", sep = "/"), pattern = "*.xlsx", full.names = TRUE)
# input_var <- lapply(input_files,read.xlsx)
# names(input_var) <- gsub(".*results\\/(.*)\\_fusions.xlsx","\\1",input_files)
# data <- rbindlist(input_var, id = "sample", use.names = TRUE)
