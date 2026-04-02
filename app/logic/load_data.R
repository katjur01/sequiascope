# app/logic/load_data.R

box::use(
  data.table[fread,as.data.table,rbindlist,tstrsplit,setcolorder,setnames,fwrite],
  tools[file_ext],
  openxlsx[getSheetNames],
  readxl[read_xls, read_xlsx],
  # VariantAnnotation[readVcf],
  matrixStats[rowRanges]
)

box::use(
  app/logic/session_utils[add_in_library_from_session]
)

# Ordered list of known population-frequency columns (case-insensitive after tolower()).
# First match found in data wins. Used across multiple modules.
#' @export
MAF_COLUMN_CANDIDATES <- c(
  # gnomAD subpopulations
  "gnomad_nfe",   # Non-Finnish European
  "gnomad_afr",   # African
  "gnomad_amr",   # Latino/Admixed American
  "gnomad_eas",   # East Asian
  "gnomad_sas",   # South Asian
  "gnomad_fin",   # Finnish
  "gnomad_asj",   # Ashkenazi Jewish
  "gnomad_mid",   # Middle Eastern
  "gnomad_af",    # gnomAD overall AF
  # 1000 Genomes subpopulations
  "1000g_eur_af", # European
  "1000g_amr_af", # American
  "1000g_afr_af", # African
  "1000g_eas_af", # East Asian
  "1000g_sas_af", # South Asian
  "1000g_af"      # overall 1000G AF
)

#' @export
MAF_COLUMN_LABELS <- c(
  gnomad_nfe   = "gnomAD NFE",
  gnomad_afr   = "gnomAD AFR",
  gnomad_amr   = "gnomAD AMR",
  gnomad_eas   = "gnomAD EAS",
  gnomad_sas   = "gnomAD SAS",
  gnomad_fin   = "gnomAD FIN",
  gnomad_asj   = "gnomAD ASJ",
  gnomad_mid   = "gnomAD MID",
  gnomad_af    = "gnomAD AF",
  `1000g_eur_af` = "1000G EUR AF",
  `1000g_amr_af` = "1000G AMR AF",
  `1000g_afr_af` = "1000G AFR AF",
  `1000g_eas_af` = "1000G EAS AF",
  `1000g_sas_af` = "1000G SAS AF",
  `1000g_af`     = "1000G AF"
)

# Helper function to read HEADER of files based on extension
#' @export
read_file_header <- function(file_path) {
  tryCatch({
    if (!file.exists(file_path)) return(NULL)
    
    file_name <- basename(file_path)
    
    # ---- VCF / VCF.GZ ----
    if (grepl("\\.vcf(\\.gz)?$", file_name, ignore.case = TRUE)) {
      is_gz <- grepl("\\.gz$", file_name, ignore.case = TRUE)
      con <- if (is_gz) gzfile(file_path, "rt") else file(file_path, "r")
      on.exit(close(con), add = TRUE)
      
      header <- NULL
      repeat {
        line <- readLines(con, n = 1, warn = FALSE)
        if (length(line) == 0) break
        if (startsWith(line, "#CHROM")) {
          header <- strsplit(sub("^#", "", line), "\t")[[1]]
          break
        }
      }
      
      if (is.null(header)) {
        warning("File does not contain #CHROM row – not a valid VCF: ", file_path)
        return(NULL)
      }
      return(header)
    }
    
    # ---- XLS / XLSX ----
    if (grepl("\\.xls$", file_name, ignore.case = TRUE)) {
      return(names(read_xls(file_path, n_max = 0)))
    }
    # ---- XLS / XLSX ----
    if (grepl("\\.xlsx$", file_name, ignore.case = TRUE)) {
      return(names(read_xlsx(file_path, n_max = 0)))
    }
    # ---- CSV / TSV ----
    return(names(fread(file_path, nrows = 0, showProgress = FALSE)))
    
  }, error = function(e) {
    message("Error reading header from ", file_path, ": ", e$message)
    return(NULL)
  })
}

# Helper function to read file based on extension
#' @export
read_by_extension <- function(file_path) {
  ext <- tolower(file_ext(file_path))
  
  if (ext %in% c("tsv", "txt")) {
    return(fread(file_path))
  } else if (ext == "xlsx") {
    return(as.data.table(read_xlsx(file_path)))
  } else if (ext == "xls") {
    return(as.data.table(read_xls(file_path)))
    # vcf <- readVcf(file_path)
    # return(as.data.table(rowRanges(vcf)))
  } else {
    stop(paste("Unsupported format:", ext))
  }
}

# Required columns are defined here
#' @export
get_required_columns <- function(dataset_type) {
  required_cols <- list(
    somatic = c("var_name", "gene_symbol", "tumor_variant_freq", "tumor_depth", "gene_region", "gnomAD_NFE",
                "consequence", "HGVSc", "HGVSp", "variant_type", "all_full_annot_name"),
    germline = c("var_name", "gene_symbol", "variant_freq", "coverage_depth", "gene_region",
                 "clinvar_sig", "consequence", "HGVSc", "HGVSp", "variant_type", "all_full_annot_name"),
    # For fusion: chr1/chr2 are required (chrom1/chrom2 will be accepted and renamed during validation)
    fusion = c("gene1","gene2","chr1","chr2","pos1","pos2","strand1","strand2","arriba.called","starfus.called",
               "arriba.confidence","overall_support","arriba.site1","arriba.site2"),
    expression = c("feature_name", "geneid","all_kegg_gene_names", "log2FC", "p_value", "p_adj")
  )
  
  return(required_cols[[dataset_type]])
}





# sample <- "DZ1601"
# input_files <- fusion_genes_filenames
#' @export
load_data <- function(input_files, flag, sample = NULL, session_dir = NULL) { 
  
  # ============ SOMATIC ============
  if (flag == "somatic") {
    input_var <- input_files[grepl(sample, input_files)]
    if (length(input_var) == 0) stop("File not found for sample ", sample)
    
    dt <- read_by_extension(input_var)
    # Normalize column names to lowercase
    setnames(dt, tolower(names(dt)))
    
    # Rename existing in_library column if present
    if ("in_library" %in% names(dt)) {
      setnames(dt, "in_library", "in_library_original")
      message("⚠️ Column 'in_library' already exists in data, renamed to 'in_library_original'")
    }
    
    dt[, sample := sample]
    
    # # Add in_library if cache exists
    if (!is.null(session_dir) && dir.exists(session_dir)) {
      dt <- add_in_library_from_session(dt, session_dir, "somatic")
    }
    
    return(dt)
    
    # ============ GERMLINE ============
  } else if (flag == "germline") {
    input_var <- input_files[grepl(sample, input_files)]
    if (length(input_var) == 0) stop("File not found for sample ", sample)
    
    dt <- read_by_extension(input_var)
    # Normalize column names to lowercase
    setnames(dt, tolower(names(dt)))
    
    # Rename existing in_library column if present
    if ("in_library" %in% names(dt)) {
      setnames(dt, "in_library", "in_library_original")
      message("⚠️ Column 'in_library' already exists in data, renamed to 'in_library_original'")
    }
    
    dt[, sample := sample]
    
    # Add in_library if cache exists
    if (!is.null(session_dir) && dir.exists(session_dir)) {
      dt <- add_in_library_from_session(dt, session_dir, "germline")

    }
    
    return(dt)
    # ============ FUSION ============
  } else if (flag == "fusion") {
    input_var <- input_files[grepl(sample, input_files)]
    if (length(input_var) == 0) stop("File not found for sample ", sample)
    
    dt <- read_by_extension(input_var)
    # Normalize column names to lowercase
    setnames(dt, tolower(names(dt)))
    
    # Rename chrom1/chrom2 to chr1/chr2 if present
    if ("chrom1" %in% names(dt)) setnames(dt, c("chrom1", "chrom2"), c("chr1", "chr2"))
    
    # Check if arriba.reading_frame column exists, if not create empty one
    if (!"arriba.reading_frame" %in% names(dt)) {
      dt[, arriba.reading_frame := ""]
    }
    
    dt[, sample := sample]
    return(dt)
    
    # ============ EXPRESSION ============
  } else if (flag == "expression") {
    # Assumes structure: input_files = list(files = list(expression = ...), tissues = ...)
    expr_files <- input_files$files$expression
    input_var <- expr_files[grepl(sample, expr_files)]
    
    if (length(input_var) == 0) stop("Expression file not found for sample ", sample)
    
    # Single file without tissues
    if (length(unique(input_files$tissues)) == 1 && unique(input_files$tissues) == "none") {
      combined_dt <- read_by_extension(input_var[[1]])
      # Normalize column names to lowercase
      setnames(combined_dt, tolower(names(combined_dt)))
      combined_dt[, c("tissue", "sample") := .("none", sample)]
      
      # Multiple files with tissues
    } else {
      dt_list <- lapply(seq_along(input_var), function(i) {
        dt <- read_by_extension(input_var[[i]])
        # Normalize column names to lowercase
        setnames(dt, tolower(names(dt)))
        dt[, c("tissue", "sample") := .(input_files$tissues[i], sample)]
        return(dt)
      })
      combined_dt <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
    }
    
    combined_dt$sample <- NULL
    combined_dt[, sample := sample]
    setnames(combined_dt, "all_kegg_paths_name", "pathway", skip_absent = TRUE)

    return(combined_dt)
    
    # ============ GOI (Genes of Interest) ============
  } else if (flag == "GOI") {
    dt <- read_by_extension(input_files)
    # Normalize column names to lowercase
    setnames(dt, tolower(names(dt)))
    
    if (ncol(dt) < 1L) stop("File contains no columns.")
    
    # Add specific GOI logic here if needed
    # e.g. filter by sample if needed
    if (!is.null(sample)) {
      dt[, sample := sample]
    }
    return(dt)
    
    # ============ TMB ============
  } else if (flag == "TMB") { 
    dt <- read_by_extension(input_files)
    # Normalize column names to lowercase
    setnames(dt, tolower(names(dt)))
    
    if (ncol(dt) < 2L) stop("TMB file must contain at least 2 columns.")
    
    dt <- dt[, 1:2]
    setnames(dt, c("patient", "TMB"))
    dt[, TMB := as.numeric(gsub(",", ".", TMB))]
    
    if (!is.null(sample)) {
      return(dt[patient == sample, ])
    }
    return(dt)
    
    # ============ UNKNOWN FLAG ============
  } else {
    stop("Unknown flag: ", flag, ". Supported: varcall, germline, fusion, expression, GOI, TMB")
  }
}

# test app
# library(data.table)
# library(openxlsx)