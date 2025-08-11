# app/logic/load_data.R

box::use(
  data.table[fread,as.data.table,rbindlist,tstrsplit,setcolorder,setnames,fwrite],
  openxlsx[read.xlsx,getSheetNames]
)
# sample <- "DZ1601"
# input_files <- fusion_genes_filenames
#' @export
load_data <- function(input_files, flag, sample = NULL,expr_flag = NULL){ 
  if (flag == "varcall"){
    input_var <- input_files[grepl(sample, input_files)]
    dt <- fread(input_var)
    dt[,sample := sample]
    
    if("in_library" %in% colnames(dt)){
      return(dt)
      
    } else {
      ## adding information about how unique variants are in samples
      ## this will run only first time when in_library column is not present 
      message("Creating in_library column.")
      
      filenames_all <- get_inputs("all_sample_file")
      input_files_all <- filenames_all$var_call.germline
      input_var_all <- lapply(input_files_all,fread)
      dt_all <- rbindlist(input_var_all,fill=TRUE)
      
      sample_list <- c()
      
      for (path in input_files) {
        if (grepl("somatic", path)) {
          sample <- gsub(".*\\/(.*)\\.variants.tsv", "\\1", path)
          sample_list <- c(sample_list, paste0(sample, "krev"))
        } else if (grepl("germline", path)) {
          sample <- gsub(".*\\/(.*)\\.variants.tsv", "\\1", path)
          sample_list <- c(sample_list, paste0(sample, "krev"))
        }
      }
      
      
      dt_all[, in_library := rowSums(.SD), .SDcols = sample_list]
      dt_all[, in_library := paste0(in_library, "/", length(sample_list))]
      merged_dt <- merge(dt, unique(dt_all[, .(var_name, in_library)]), by = "var_name", all.x = TRUE)
      fwrite(merged_dt,input_var)
      rm(dt_all,input_var_all,filenames_all,input_files_all,dt)
      
      return(merged_dt)
    }
    
  } else if (flag == "fusion") {
    input_var <- input_files[grepl(sample, input_files)]
    dt <- as.data.table(read.xlsx(input_var))
    dt[, sample := sample]
    return(dt)
    
  } else if (flag == "expression") { 
    # input_files <- expression_profile_filenames
    # input_files_var <- get_inputs("per_sample_file")
    patient_files <- input_files[grep(sample, input_files)] # sample = "MR1507"
    
    if(expr_flag == "genes_of_interest"){
      patient_files <- patient_files[grep(expr_flag, patient_files)] # sample = "DZ1601"
      
      dt_list <- lapply(patient_files, function(file) {
        dt <- fread(file)
        tissue <- gsub("^.*/|_genes_of_interest\\.tsv$","",file)
        dt[, c("tissue", "sample") := .(tissue, sample)]
        return(dt)
      })
      combined_dt <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
      
    } else if (expr_flag == "all_genes"){
      patient_files <- patient_files[grep(expr_flag, patient_files)] # sample = "DZ1601"
      patient_files <- patient_files[grep("multiRow", patient_files)]
      dt_list <- lapply(patient_files, function(file) {
        dt <- fread(file)
        tissue <- gsub("^.*/|_all_genes_multiRow\\.tsv$","",file)
        dt[, c("tissue", "sample") := .(tissue, sample)]
        return(dt)
      })
      combined_dt <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
      combined_dt$sample <- NULL
      combined_dt[, sample := sample]
      setnames(combined_dt, "all_kegg_paths_name", "pathway")
    } else{
      stop("Your data table has wrong name. Must contain string gene_of_interest or all_genes.")
    }
    
    return(combined_dt)
    
    
    # patient_data <- lapply(patient_files, function(file) { #file <- patient_files[1]
    #   sheet_names <- getSheetNames(file)
    #   input_var <- lapply(sheet_names, function(sheet) read.xlsx(file, sheet = sheet))
    #   names(input_var) <- sheet_names
    #   info <- gsub(paste0(".*", input_files_var$expression.project, "\\/(.*)\\_[0-9]*\\_(.*)\\_report.xlsx"), "\\1_\\2", file)
    #   info <- gsub("[ -]", "", info)
    # 
    #   dt <- as.data.table(rbindlist(input_var, fill = TRUE))
    #   dt[, c("Sample", "Tissue") := tstrsplit(info, "_")]
    #   setcolorder(dt, c("Sample", "Tissue", "Gene", "Pathway", "Scale", "FC"))
    #   tissue <- unique(dt$Tissue)
    #   setnames(dt, c("Sample", paste0("Tissue_",tissue), "Gene", "Pathway", paste0("Scale_",tissue), paste0("FC_",tissue)))
    # 
    #   return(dt)
    # })
    # 
    #     combined_data <- merge(patient_data[[1]], patient_data[[2]], by = c("Sample", "Gene", "Pathway"), all = TRUE)
    #     return(combined_data)
    
  } else {
    return(print("not varcall nor fusion nor expression"))
  }
}



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