# app/logic/load_data.R

box::use(
  data.table[fread,as.data.table,rbindlist,tstrsplit,setcolorder,setnames,fwrite],
  tools[file_ext],
  openxlsx[read.xlsx,getSheetNames],
  readxl[read_xls],
  VariantAnnotation[readVcf],
  matrixStats[rowRanges]
)

box::use(
  app/logic/session_utils[add_in_library_from_session]
)

# Helper function to read file based on extension
read_by_extension <- function(file_path) {
  ext <- tolower(file_ext(file_path))
  
  if (ext %in% c("tsv", "txt")) {
    return(fread(file_path))
  } else if (ext == "xlsx") {
    return(as.data.table(read.xlsx(file_path)))
  } else if (ext == "xls") {
    return(as.data.table(read_xls(file_path)))
    vcf <- readVcf(file_path)
    return(as.data.table(rowRanges(vcf)))
  } else {
    stop(paste("Nepodporovaný formát:", ext))
  }
}
# sample <- "DZ1601"
# input_files <- fusion_genes_filenames
#' @export
load_data <- function(input_files, flag, sample = NULL, session_dir = NULL) { 
  
  # ============ SOMATIC ============
  if (flag == "somatic") {
    input_var <- input_files[grepl(sample, input_files)]
    if (length(input_var) == 0) stop("Soubor pro vzorek ", sample, " nenalezen")
    
    dt <- read_by_extension(input_var)
    dt[, sample := sample]
    
    # # Add in_library if cache exists
    if (!is.null(session_dir) && dir.exists(session_dir)) {
      dt <- add_in_library_from_session(dt, session_dir, "somatic")
    }
    
    return(dt)
    
    # ============ GERMLINE ============
  } else if (flag == "germline") {
    input_var <- input_files[grepl(sample, input_files)]
    if (length(input_var) == 0) stop("Soubor pro vzorek ", sample, " nenalezen")
    
    dt <- read_by_extension(input_var)
    dt[, sample := sample]
    
    # Add in_library if cache exists
    if (!is.null(session_dir) && dir.exists(session_dir)) {
      dt <- add_in_library_from_session(dt, session_dir, "germline")

    }
    
    return(dt)
    # ============ FUSION ============
  } else if (flag == "fusion") {
    input_var <- input_files[grepl(sample, input_files)]
    if (length(input_var) == 0) stop("Soubor pro vzorek ", sample, " nenalezen")
    
    dt <- read_by_extension(input_var)
    dt[, sample := sample]
    return(dt)
    
    # ============ EXPRESSION ============
  } else if (flag == "expression") {
    # Předpokládám strukturu: input_files = list(files = list(expression = ...), tissues = ...)
    expr_files <- input_files$files$expression
    input_var <- expr_files[grepl(sample, expr_files)]
    
    if (length(input_var) == 0) stop("Expression soubor pro vzorek ", sample, " nenalezen")
    
    # Jeden soubor bez tkání
    if (length(unique(input_files$tissues)) == 1 && unique(input_files$tissues) == "none") {
      combined_dt <- read_by_extension(input_var[[1]])
      combined_dt[, c("tissue", "sample") := .("none", sample)]
      
      # Více souborů s tkáněmi
    } else {
      dt_list <- lapply(seq_along(input_var), function(i) {
        dt <- read_by_extension(input_var[[i]])
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
    
    if (ncol(dt) < 1L) stop("Soubor neobsahuje žádné sloupce.")
    
    # Zde můžeš přidat specifickou logiku pro GOI
    # např. filtrování podle sample pokud je potřeba
    if (!is.null(sample)) {
      dt[, sample := sample]
    }
    return(dt)
    
    # ============ TMB ============
  } else if (flag == "TMB") { 
    dt <- read_by_extension(input_files)
    
    if (ncol(dt) < 2L) stop("TMB soubor musí obsahovat alespoň 2 sloupce.")
    
    dt <- dt[, 1:2]
    setnames(dt, c("patient", "TMB"))
    dt[, TMB := as.numeric(gsub(",", ".", TMB))]
    
    if (!is.null(sample)) {
      return(dt[patient == sample, ])
    }
    return(dt)
    
    # ============ UNKNOWN FLAG ============
  } else {
    stop("Neznámý flag: ", flag, ". Podporované: varcall, germline, fusion, expression, GOI, TMB")
  }
}
# load_data <- function(input_files, flag, sample = NULL,expr_flag = NULL){ 
#   if (flag == "varcall"){
#     input_var <- input_files[grepl(sample, input_files)]
#     dt <- fread(input_var)
#     dt[,sample := sample]
#     
#     if("in_library" %in% colnames(dt)){
#       return(dt)
#       
#     } else {
#       ## adding information about how unique variants are in samples
#       ## this will run only first time when in_library column is not present 
#       message("Creating in_library column.")
#       
#       filenames_all <- get_inputs("all_sample_file")
#       input_files_all <- filenames_all$var_call.germline
#       input_var_all <- lapply(input_files_all,fread)
#       dt_all <- rbindlist(input_var_all,fill=TRUE)
#       
#       sample_list <- c()
#       
#       for (path in input_files) {
#         if (grepl("somatic", path)) {
#           sample <- gsub(".*\\/(.*)\\.variants.tsv", "\\1", path)
#           sample_list <- c(sample_list, paste0(sample, "krev"))
#         } else if (grepl("germline", path)) {
#           sample <- gsub(".*\\/(.*)\\.variants.tsv", "\\1", path)
#           sample_list <- c(sample_list, paste0(sample, "krev"))
#         }
#       }
#       
#       
#       dt_all[, in_library := rowSums(.SD), .SDcols = sample_list]
#       dt_all[, in_library := paste0(in_library, "/", length(sample_list))]
#       merged_dt <- merge(dt, unique(dt_all[, .(var_name, in_library)]), by = "var_name", all.x = TRUE)
#       fwrite(merged_dt,input_var)
#       rm(dt_all,input_var_all,filenames_all,input_files_all,dt)
#       
#       return(merged_dt)
#     }
#     
#   } else if (flag == "fusion") {
#     input_var <- input_files[grepl(sample, input_files)]
#     dt <- as.data.table(read.xlsx(input_var))
#     dt[, sample := sample]
#     return(dt)
#     
#   } else if (flag == "expression") {
#     
#     expr_files <- input_files$files$expression
#     input_var  <- expr_files[grepl(sample, expr_files)]
# 
#     if (length(unique(input_files$tissues)) == 1 &&  unique(input_files$tissues) == "none") {
#       combined_dt <- fread(unlist(input_var))
#       combined_dt[, c("tissue", "sample") := .("none", sample)]
#       
#     } else {
#       dt_list <- lapply(seq_along(input_var), function(i) {
#         dt <- fread(input_var[[i]])
#         dt[, c("tissue", "sample") := .(input_files$tissues[i], sample)]
#         return(dt)
#       })
#       combined_dt <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
#     }
#     
#     combined_dt$sample <- NULL
#     combined_dt[, sample := sample]
#     setnames(combined_dt, "all_kegg_paths_name", "pathway", skip_absent = TRUE)
#     return(combined_dt)
#     
#   } else if (flag == "TMB") { 
#     ext <- tolower(file_ext(input_files))
#     
#     if (ext == "xlsx") {
#       dt <- as.data.table(read.xlsx(input_files))
#     } else if (ext == "tsv") {
#       dt <- fread(input_files)
#     } else if (ext == "txt") {
#       dt <- fread(input_files)
#     } else {
#       stop("Podporovány jsou pouze soubory .xlsx, .tsv nebo .txt")
#     }
# 
#     if (ncol(dt) < 1L) stop("Soubor neobsahuje žádné sloupce.")
#     
#     dt <- dt[,1:2]
#     setnames(dt,c("patient","TMB"))
#     dt[,TMB := as.numeric(gsub(",", ".", TMB))]
#     return(dt[patient == sample,])
#     
#   } else {
#     return(print("not varcall nor fusion nor expression"))
#   }
# }



#' @export
get_inputs <- function(flag){
  
  library <- "MOII_e117"
  
  if (flag == "per_sample_file"){
    
    path = paste0("./input_files/", library)
    
    ## somatic var call path
    somatic_variant_calling_project <- "117_WES_somatic"
    somatic_variant_calling_filenames <- list.files(paste(path, somatic_variant_calling_project,"per_sample_final_var_tabs/tsv_formated",sep = "/"), pattern = "*.tsv", full.names = TRUE)
    somatic_varcall_mutation_load <- list.files(paste(path, somatic_variant_calling_project,sep = "/"), pattern = "*_loads.xlsx", full.names = TRUE)
    ## gemline var call path
    germline_variant_calling_project <- "117_WES_germline"
    germline_variant_calling_filenames <- list.files(paste(path, germline_variant_calling_project,"per_sample_final_var_tabs/tsv_formated", sep = "/"), pattern = "*.tsv", full.names = TRUE)
    
    ## structural var call path
    structural_variant_calling_project <- "117_WES_structural"
    structural_variant_calling_filenames <- list.files(paste(path, structural_variant_calling_project, sep = "/"), pattern = "*.tsv", full.names = TRUE)
    
    ## fusion var call path
    fusion_genes_project <- "117_fusions"
    fusion_genes_filenames <- list.files(paste(path, fusion_genes_project, "results", sep = "/"), pattern = "*.xlsx", full.names = TRUE)
    
    ## arriba folder path
    arriba_res_folder <- paste(path, fusion_genes_project, "results", sep = "/")
    
    ## expression profile results path
    expression_profile_project <- "RNAseq21_NEW"
    expression_profile_dirnames <- list.files(paste(path, expression_profile_project,sep = "/"), pattern = "*", full.names = TRUE)
    expression_profile_filenames <- list.files(expression_profile_dirnames, pattern = "*.tsv", full.names = TRUE)
    # expression_profile_project <- "RNAseq21"
    # expression_profile_filenames <- list.files(paste(path, expression_profile_project, sep = "/"), pattern = "*_report.xlsx", full.names = TRUE)
    #
    return(list(var_call.somatic = somatic_variant_calling_filenames,
                var_call.somatic.mut_load = somatic_varcall_mutation_load,
                var_call.germline = germline_variant_calling_filenames,
                var_call.structural = structural_variant_calling_filenames,
                fusions = fusion_genes_filenames,
                arriba_res = arriba_res_folder,
                expression.files = expression_profile_filenames,
                expression.project = expression_profile_project
    ))
    
  } else if (flag == "all_sample_file"){
    
    path = paste0("./input_files/", library)
    
    ## somatic var call path
    somatic_variant_calling_project <- "117_WES_somatic"
    somatic_variant_calling_filenames <- list.files(paste(path, somatic_variant_calling_project,sep = "/"), pattern = "*.tsv", full.names = TRUE)
    
    ## gemline var call path
    germline_variant_calling_project <- "117_WES_germline"
    germline_variant_calling_filenames <- list.files(paste(path, germline_variant_calling_project, sep = "/"), pattern = "*.tsv", full.names = TRUE)
    
    ## structural var call path
    structural_variant_calling_project <- "117_WES_structural"
    structural_variant_calling_filenames <- list.files(paste(path, structural_variant_calling_project, sep = "/"), pattern = "*.tsv", full.names = TRUE)
    
    
    return(list(var_call.somatic = somatic_variant_calling_filenames,
                var_call.germline = germline_variant_calling_filenames
    ))
    
  } else if (flag == "bam_file"){
    
    path = paste0("input_files/", library, "/primary_analysis/")
    
    
    somatic_variant_calling_project <- "230426_MOII_e117_tkane"
    DNA_tumor_bam <- list.files(paste(path, somatic_variant_calling_project,"mapped",sep = "/"), pattern = "*.bam", full.names = TRUE)
    
    germline_variant_calling_project <- "230426_MOII_e117_krve"
    DNA_normal_bam <- list.files(paste(path, germline_variant_calling_project,"mapped",sep = "/"), pattern = "*.bam", full.names = TRUE)
    
    fusion_genes_project <- "230426_MOII_e117_fuze"
    RNA_tumor_bam <- list.files(paste(path, fusion_genes_project,"mapped",sep = "/"), pattern = "*.bam", full.names = TRUE)
    RNA_chimeric_bam <- list.files(file.path(path, fusion_genes_project), pattern   = "Chimeric\\.out\\.bam$", recursive = TRUE, full.names = TRUE)
    return(list(dna.tumor_bam = DNA_tumor_bam,
                dna.normal_bam = DNA_normal_bam,
                rna.tumor_bam = RNA_tumor_bam,
                rna.chimeric_bam = RNA_chimeric_bam,
                path_to_folder = path
    ))
  } else if (flag == "goi_file"){ # genes_of_interest
    
    path = paste0("./input_files/", library)
    
    ## somatic var call path
    somatic_project <- "117_WES_somatic"
    somatic_filenames <- list.files(paste(path, somatic_project,sep = "/"), pattern = "*genes_of_interest.tsv", full.names = TRUE)
    
    ## gemline var call path
    germline_project <- "117_WES_germline"
    germline_filenames <- list.files(paste(path, germline_project, sep = "/"), pattern = "*genes_of_interest.tsv", full.names = TRUE)
    
    ## expression profile results path
    expression_project <- "RNAseq21_NEW"
    expression_dirnames <- list.files(paste(path, expression_project,sep = "/"), pattern = "*", full.names = TRUE)
    expression_filenames <- list.files(paste(expression_dirnames,sep = "/"), pattern = "genes_of_interest.tsv", full.names = TRUE)
    
    return(list(goi.somatic = somatic_filenames,
                goi.germline = germline_filenames,
                goi.expression = expression_filenames
    ))
    
  } else {
    stop("Invalid tag. Use 'per_sample_file' or 'all_sample_file'.")
  }
}


#' #' @export
#' get_inputs <- function(flag){
#'   
#'   library <- "reanalysed_data"
#'   path = paste0("./input_files/", library)
#'   
#'   if (flag == "per_sample_file"){
#'     
#'     ## somatic var call path
#'     somatic_variant_calling_project <- "somatic"
#'     somatic_variant_calling_filenames <- list.files(paste(path, somatic_variant_calling_project,"per_sample_final_var_tabs/tsv_formated",sep = "/"), pattern = "*.tsv", full.names = TRUE)
#'     
#'     ## gemline var call path
#'     germline_variant_calling_project <- "germline"
#'     germline_variant_calling_filenames <- list.files(paste(path, germline_variant_calling_project,"per_sample_final_var_tabs/tsv_formated", sep = "/"), pattern = "*.tsv", full.names = TRUE)
#'     
#'     ## structural var call path
#'     structural_variant_calling_project <- "structural"
#'     structural_variant_calling_filenames <- list.files(paste(path, structural_variant_calling_project, sep = "/"), pattern = "*.tsv", full.names = TRUE)
#'     
#'     ## fusion var call path
#'     fusion_genes_project <- "fusions"
#'     fusion_genes_filenames <- list.files(paste(path, fusion_genes_project, "results", sep = "/"), pattern = "*.xlsx", full.names = TRUE)
#'     
#'     ## arriba folder path
#'     arriba_res_folder <- paste(path, fusion_genes_project, "results", sep = "/")
#'     
#'     ## expression profile results path
#'     expression_profile_project <- "rnaseq"
#'     expression_profile_dirnames <- list.files(paste(path, expression_profile_project,sep = "/"), pattern = "*", full.names = TRUE)
#'     expression_profile_filenames <- list.files(expression_profile_dirnames, pattern = "*.tsv", full.names = TRUE)
#'     # expression_profile_project <- "RNAseq21"
#'     # expression_profile_filenames <- list.files(paste(path, expression_profile_project, sep = "/"), pattern = "*_report.xlsx", full.names = TRUE)
#'     # 
#'     return(list(var_call.somatic = somatic_variant_calling_filenames,
#'                 var_call.germline = germline_variant_calling_filenames,
#'                 var_call.structural = structural_variant_calling_filenames,
#'                 fusions = fusion_genes_filenames,
#'                 arriba_res = arriba_res_folder,
#'                 expression.files = expression_profile_filenames,
#'                 expression.project = expression_profile_project
#'     ))
#'     
#'   } else if (flag == "all_sample_file"){
#'     
#'     ## somatic var call path
#'     somatic_variant_calling_project <- "somatic"
#'     somatic_variant_calling_filenames <- list.files(paste(path, somatic_variant_calling_project,sep = "/"), pattern = "*.tsv", full.names = TRUE)
#'     
#'     ## gemline var call path
#'     germline_variant_calling_project <- "germline"
#'     germline_variant_calling_filenames <- list.files(paste(path, germline_variant_calling_project, sep = "/"), pattern = "*.tsv", full.names = TRUE)
#'     
#'     ## structural var call path
#'     structural_variant_calling_project <- "structural"
#'     structural_variant_calling_filenames <- list.files(paste(path, structural_variant_calling_project, sep = "/"), pattern = "*.tsv", full.names = TRUE)
#'     
#'     
#'     return(list(var_call.somatic = somatic_variant_calling_filenames,
#'                 var_call.germline = germline_variant_calling_filenames,
#'                 var_call.structural = structural_variant_calling_filenames
#'     ))
#'     
#'   } else {
#'     stop("Invalid tag. Use 'per_sample_file' or 'all_sample_file'.")
#'   }
#' }






# test app
# library(data.table)
# library(openxlsx)