# app/logic/prerun_fusion.R

box::use(
  shiny[reactiveVal],
  data.table[fread, fwrite, as.data.table, setnames, tstrsplit, uniqueN],
  openxlsx[read.xlsx],
  stringr[str_replace_all, str_remove],
  future[future, value, resolved],
  promises[then, catch]
)

box::use(
  app/logic/prepare_arriba_pictures[pdf2png],
  app/logic/load_data[get_inputs],
  app/logic/helper_main[get_patients,get_files_fo_all_patients],
)

#' Create IGV batch file for taking snapshots
#' @param bam_file Path to BAM file
#' @param chimeric_file Path to chimeric BAM file
#' @param chrom_break_pos Position string like "chr1:100-200 chr2:300-400"
#' @param snapshot_name Name for the snapshot file
#' @param batch_file_name Name for the batch file
#' @param output_dir Output directory for snapshots
#' @param genome_build Genome build (default: hg38)
createIGVBatchFile <- function(bam_file, chimeric_file, chrom_break_pos, snapshot_name, batch_file_name, output_dir, genome_build = "hg38") {
  header <- paste("new",
                  paste0("genome ", genome_build),
                  paste0("load ", bam_file),
                  paste0("load ", chimeric_file),
                  paste0("snapshotDirectory ", output_dir),
                  paste0("goto ", chrom_break_pos),
                  "viewaspairs",
                  "maxPanelHeight 10000",
                  "preference SAM.SHOW_SOFT_CLIPPED true",
                  "preference IGV.Bounds 94,0,1280,1024",
                  "preference SAM.SHOW_JUNCTION_TRACK true",
                  "preference SAM.COLOR_BY NONE",
                  paste0("snapshot ", snapshot_name, ".png"),
                  "exit",
                  sep = "\n")

  cat(header, file = batch_file_name)
  return(batch_file_name)
}

#' Create fusion table from Excel/TSV file for IGV snapshots
#' @param fusion_file Path to fusion file
#' @param sample_name Sample name
#' @param zoom Zoom level around breakpoint (default: 251)
createFusionTableFromFile <- function(fusion_file, sample_name, zoom = 251) {
  if (!file.exists(fusion_file)) {
    message("Fusion file doesn't exist: ", fusion_file)
    return(NULL)
  }

  # Handle both Excel and TSV files
  if (grepl("\\.xlsx?$", fusion_file)) {
    input_table <- as.data.table(read.xlsx(fusion_file))
  } else if (grepl("\\.tsv$", fusion_file)) {
    input_table <- fread(fusion_file)
  } else {
    message("Unsupported file format: ", fusion_file)
    return(NULL)
  }

  # Standardize column names if needed
  if ("chrom1" %in% names(input_table)) setnames(input_table, c("chrom1", "chrom2"), c("chr1", "chr2"))

  # Extract required columns
  required_cols <- c("gene1", "gene2", "chr1", "pos1", "chr2", "pos2")
  missing_cols <- setdiff(required_cols, names(input_table))
  if (length(missing_cols) > 0) {
    message("Missing required columns in fusion file: ", paste(missing_cols, collapse = ", "))
    return(NULL)
  }

  fusions_tab <- input_table[, ..required_cols]
  fusions_tab <- fusions_tab[, .(gene1 = paste(unique(gene1), collapse = ","),
                                 gene2 = paste(unique(gene2), collapse = ",")),
                             by = .(chr1, pos1, chr2, pos2)]
  fusions_tab[, id := seq_len(.N)]
  fusions_tab[, chrom_break_pos := paste0(chr1, ":", pos1 - zoom, "-", pos1 + zoom,
                                          " ", chr2, ":", pos2 - zoom, "-", pos2 + zoom)]
  fusions_tab[, fusion_genes := paste0(gene1, "__", gene2)]
  fusions_tab$fusion_genes <- str_replace_all(fusions_tab$fusion_genes, "[.,()-]", "_")
  fusions_tab <- unique(fusions_tab)
  fusions_tab[, png_path := sprintf(paste0("./igv_snapshots/", sample_name, "/", sample_name, "_%03d.png"), .I)]

  return(fusions_tab)
}

#' Run IGV snapshot using batch file
#' @param IGV_batch_file Path to IGV batch file
runIGVSnapshot <- function(IGV_batch_file) {
  igv_executive <- "igv"
  igv_command <- paste0("xvfb-run --auto-servernum --server-args='-screen 0 1280x1024x24 -ac' ",
                        igv_executive, " -b ", IGV_batch_file)

  result <- tryCatch({
    system(igv_command, wait = TRUE)

  }, error = function(e) {
    message("Error running IGV command: ", e$message)
    return(1)
  })

  # Clean up batch file
  if (file.exists(IGV_batch_file)) {
    file.remove(IGV_batch_file)
  }

  return(result)
}

#' Process IGV snapshots for a single patient
#' @param patient_id Patient identifier
#' @param file_list List of files for this patient (fusion, tumor, chimeric)
#' @param output_base_dir Base directory for output (usually "./www")
process_patient_igv <- function(patient_id, file_list, output_base_dir = "./www") {
  message("Processing IGV snapshots for patient: ", patient_id)

  # Get required files
  fusion_file <- file_list$fusion
  tumor_files <- file_list$tumor
  chimeric_files <- file_list$chimeric
  
  if (length(fusion_file) == 0) {
    message("No fusion file found for patient: ", patient_id)
    return(FALSE)
  }

  # Create fusion table
  fusions_tab <- createFusionTableFromFile(fusion_file, patient_id)
  if (is.null(fusions_tab) || nrow(fusions_tab) == 0) {
    message("No fusion data found for patient: ", patient_id)
    return(FALSE)
  }

  # Create output directory
  output_dir <- file.path(output_base_dir, "igv_snapshots", patient_id)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Find BAM files (optional for IGV)
  tumor_bam <- if (length(tumor_files) > 0) {
    tumor_files[grepl("\\.bam$", tumor_files)][1]
  } else NULL

  chimeric_bam <- if (length(chimeric_files) > 0) {
    chimeric_files[grepl("\\.bam$", chimeric_files)][1]
  } else NULL

  if (is.null(tumor_bam) || is.null(chimeric_bam)) {
    message("Warning: Missing BAM files for patient ", patient_id,
            ". IGV snapshots may not be complete.")
  }

  # Process each fusion
  success_count <- 0
  # for (i in seq_len(nrow(fusions_tab))) {
  #   tryCatch({
  #     if (!is.null(tumor_bam) && !is.null(chimeric_bam)) {
  #       batch_file <- createIGVBatchFile(
  #         bam_file = tumor_bam,
  #         chimeric_file = chimeric_bam,
  #         chrom_break_pos = fusions_tab$chrom_break_pos[i],
  #         snapshot_name = sprintf("%s_%03d", patient_id, i),
  #         output_dir = output_dir,
  #         batch_file_name = paste0("www/igv_snapshots/",fusions_tab$fusion_genes[i], "_temp_batch.txt")
  #       )
  # 
  #       result <- runIGVSnapshot(batch_file)
  #       if (result == 0) {
  #         success_count <- success_count + 1
  #       }
  #     }
  #   }, error = function(e) {
  #     message("Error processing fusion ", i, " for patient ", patient_id, ": ", e$message)
  #   })
  # }

  message("IGV snapshots completed for patient ", patient_id, ": ",
          success_count, "/", nrow(fusions_tab), " successful")

  return(success_count > 0)
}

#' Process Arriba images for a single patient
#' @param sample Sample identifier
#' @param arriba_pdf List of files for this patient
#' @param output_base_dir Base directory for output
process_arriba_pdf <- function(sample, arriba, output_base_dir = "./www") {
  message("Processing Arriba images for patient: ", sample)
  success_count <- 0
  
  arriba_tsv <- fread(arriba[grepl("\\.tsv$", arriba)])
  arriba_pdf <- arriba[grepl("\\.pdf$", arriba)]
  
  output_dir <- file.path(output_base_dir, "arriba_reports", sample)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  

  pages <- NA_integer_  # kolik stran má PDF (pdftocairo pak pojedeme po jedné stránce)
  pdfinfo_out <- tryCatch(system2("pdfinfo", args = shQuote(arriba_pdf), stdout = TRUE), error = function(e) character())
  if (length(pdfinfo_out)) {
    p_line <- pdfinfo_out[grepl("^Pages:\\s+\\d+", pdfinfo_out)]
    if (length(p_line)) pages <- as.integer(sub("^Pages:\\s+", "", p_line[1]))
  }
  if (is.na(pages)) stop("Nepodařilo se zjistit počet stran PDF pro sample ", sample)
  
  if (length(list.files(output_dir, pattern = "\\.svg$", ignore.case = TRUE)) == 0) {
    for (i in seq_len(pages)) {
      output_svg <- file.path(output_dir, sprintf("%s_%03d.svg", sample, i))
      system2("pdftocairo", args = c("-svg", "-f", as.character(i), "-l", as.character(i), shQuote(arriba_pdf), shQuote(output_svg)))
      if (file.exists(output_svg)) success_count <- success_count + 1
    }
    message("Creating arriba pictures... (", success_count, "/", pages, ")")
  } else {
    print("Arriba svg picture are already prepared. Nothing to do here.")
    success_count <- length(list.files(output_dir, pattern = "\\.svg$", ignore.case = TRUE))
  }
  
  message("Arriba PDF to SVG completed for patient ", sample, ": ",
          success_count, "/", nrow(arriba_tsv), " successful")
  return(success_count > 0)
}



#' Create manifest file for fusion images
#' @param patient_id Patient identifier
#' @param fusion_file Path to fusion file
#' @param output_base_dir Base directory for output
create_fusion_manifest <- function(patient_id, fusion_file, arriba, output_base_dir = "./www") {
  
  arriba_tsv <- arriba[grepl("\\.tsv$", arriba)]

  if (file.exists(arriba_tsv)){ # prepare_arriba_image_paths
    arriba_dt <- fread(arriba_tsv)
    arriba_dt[, path := sprintf("arriba_reports/%s/%s_%03d.svg", patient_id, patient_id, .I)]
    arriba_dt[, gene1 := gsub("\\(.*?\\)", "", `#gene1`)]
    arriba_dt[, gene2 := gsub("\\(.*?\\)", "", gene2)]
    arriba_dt[, c("chr1", "pos1") := tstrsplit(breakpoint1, ":", fixed = TRUE)]
    arriba_dt[, c("chr2", "pos2") := tstrsplit(breakpoint2, ":", fixed = TRUE)]
    arriba_dt[, chr1 := paste0("chr", chr1)]
    arriba_dt[, chr2 := paste0("chr", chr2)]
    arriba_dt[, pos1 := as.numeric(pos1)]
    arriba_dt[, pos2 := as.numeric(pos2)]
    tsv_dt <- split_genes(arriba_dt)
    setnames(tsv_dt, "path", "svg_path")
  } else return(NULL)
  if (file.exists(fusion_file)){ # prepare_igv_snapshot_paths 
    if (grepl("\\.xlsx?$", fusion_file)) {
      dt <- as.data.table(read.xlsx(fusion_file))
    } else {
      dt <- fread(fusion_file)
    }
    if ("chrom1" %in% names(dt)) setnames(dt, c("chrom1","chrom2"), c("chr1","chr2"))
    if ("pos1" %in% names(dt) && !is.numeric(dt$pos1)) dt[, pos1 := as.numeric(pos1)]
    if ("pos2" %in% names(dt) && !is.numeric(dt$pos2)) dt[, pos2 := as.numeric(pos2)]
    
    dt[, png_path := sprintf("igv_snapshots/%s/%s_%03d.png",   patient_id, patient_id, .I)]
  } else return(NULL)
  manifest_dt <- merge(tsv_dt[,.(gene1,gene2,chr1,pos1,chr2,pos2,svg_path)], dt[,.(gene1,gene2,chr1,pos1,chr2,pos2,png_path)], all = TRUE, suffixes = c("_arriba", "_igv"))
  
  manifest_dir <- file.path(output_base_dir, "manifests", "fusion")
  if (!dir.exists(manifest_dir)) dir.create(manifest_dir, recursive = TRUE)
  manifest_file <- file.path(manifest_dir, paste0(patient_id, ".tsv"))
  fwrite(manifest_dt, manifest_file, sep = "\t")
  
  message("Created fusion manifest for patient ", patient_id, ": ", manifest_file)
  manifest_file
}


#' Main prerun function for fusion data
#' @param confirmed_paths Data frame with paths from upload_data
#' @param shared_data Reactive values object to update status
#' @export
# prerun_fusion_data <- function(confirmed_paths, shared_data) {
#   print("Starting fusion data prerun...")
# 
#   # Update status
#   if (!is.null(shared_data)) {
#     shared_data$fusion_prerun_status <- reactiveVal("running")
#     shared_data$fusion_prerun_progress <- reactiveVal(0)
#   }
#   
#   # Get fusion patients and their files
#   fusion_patients <- get_patients(confirmed_paths, "fusion")
#   if (length(fusion_patients) == 0) {
#     print("No fusion patients found")
#     if (!is.null(shared_data)) {
#       shared_data$fusion_prerun_status <- reactiveVal("completed")
#     }
#     return()
#   }
#   
#   fusion_files <- get_files_fo_all_patients(confirmed_paths, "fusion")
#   message("Found ", length(fusion_patients), " fusion patients: ", paste(fusion_patients, collapse = ", "))
#   
#   # Create base directories
#   base_dirs <- c("./www/igv_snapshots", "./www/arriba_reports", "./www/manifests/fusion")
#   for (dir in base_dirs) {
#     if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
#   }
#   
#   # Process each patient
#   total_patients <- length(fusion_patients)
#   prog_file <- file.path(tempdir(), paste0("fusion_", Sys.getpid(), ".progress"))
#   
#   for (i in seq_along(fusion_patients)) {
#     sample <- fusion_patients[i]
#     file_list <- fusion_files[[sample]]
#     
#     if (is.null(file_list)) {
#       message("No files found for patient: ", sample)
#       next
#     }
#     
#     message("Processing patient ", i, "/", total_patients, ": ", sample)
#     
#     tryCatch({
#       igv_success <- process_patient_igv(sample, file_list) # Process IGV snapshots
#       arriba_success <- process_arriba_pdf(sample, file_list$arriba)  # Process Arriba images
# 
#       # Create manifest
#       fusion_file <- file_list$fusion
#       if (length(fusion_file) > 0) {
#         create_fusion_manifest(sample, fusion_file[1], file_list$arriba)
#       }
#       # Update progress
#       if (!is.null(shared_data)) {
#         progress <- round((i / total_patients) * 100)
#         shared_data$fusion_prerun_progress <- reactiveVal(progress)
#         writeLines(as.character(progress), prog_file)
#       }
# 
#     }, error = function(e) {  # Continue with next patient instead of stopping
#       message("Error processing patient ", sample, ": ", e$message)
# 
#     })
#   }
#   
#   # Mark as completed
#   if (!is.null(shared_data)) {
#     shared_data$fusion_prerun_status <- reactiveVal("completed")
#     shared_data$fusion_prerun_progress <- reactiveVal(100)
#   }
#   
#   message("Fusion data prerun completed!")
# }
prerun_fusion_data <- function(confirmed_paths, shared_data, prog_file = NULL) {
  message("Starting fusion data prerun...")
  
  # 1) Inicializace stavu (setter, ne nahrazování reactiveVal)
  if (!is.null(shared_data)) {
    if (is.function(shared_data$fusion_prerun_status))   shared_data$fusion_prerun_status("running")
    if (is.function(shared_data$fusion_prerun_progress)) shared_data$fusion_prerun_progress(0)
  }
  if (!is.null(prog_file) && nzchar(prog_file)) writeLines("0", prog_file)
  
  # 2) Pacienti a jejich soubory
  fusion_patients <- get_patients(confirmed_paths, "fusion")
  if (length(fusion_patients) == 0) {
    message("No fusion patients found")
    if (!is.null(shared_data) && is.function(shared_data$fusion_prerun_status)) {
      shared_data$fusion_prerun_status("completed")
    }
    if (!is.null(prog_file) && nzchar(prog_file)) writeLines("100", prog_file)
    return(TRUE)
  }
  
  fusion_files <- get_files_fo_all_patients(confirmed_paths, "fusion")
  message("Found ", length(fusion_patients), " fusion patients: ", paste(fusion_patients, collapse = ", "))
  
  # 3) Základní adresáře (idempotentně)
  base_dirs <- c("./www/igv_snapshots", "./www/arriba_reports", "./www/manifests/fusion")
  for (dir in base_dirs) {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  }
  
  # 4) Smyčka přes pacienty + průběžný progress
  total_patients <- length(fusion_patients)
  for (i in seq_along(fusion_patients)) {
    sample    <- fusion_patients[i]
    file_list <- fusion_files[[sample]]
    
    if (is.null(file_list)) {
      message("No files found for patient: ", sample)
      next
    }
    
    message("Processing patient ", i, "/", total_patients, ": ", sample)
    
    tryCatch({
      igv_success    <- process_patient_igv(sample, file_list)                # IGV snapshots
      arriba_success <- process_arriba_pdf(sample, file_list$arriba)          # Arriba SVG
      
      # Manifest
      fusion_file <- file_list$fusion
      if (length(fusion_file) > 0) {
        create_fusion_manifest(sample, fusion_file[1], file_list$arriba)
      }
      
      # Progress (reaktiv + volitelně do souboru)
      progress <- round((i / total_patients) * 100)
      if (!is.null(shared_data) && is.function(shared_data$fusion_prerun_progress)) {
        shared_data$fusion_prerun_progress(progress)
      }
      if (!is.null(prog_file) && nzchar(prog_file)) {
        writeLines(as.character(progress), prog_file)
      }
      
    }, error = function(e) {
      message("Error processing patient ", sample, ": ", e$message)
      # pokračuj na dalšího pacienta
    })
  }
  
  # 5) Dokončení
  if (!is.null(shared_data)) {
    if (is.function(shared_data$fusion_prerun_status))   shared_data$fusion_prerun_status("completed")
    if (is.function(shared_data$fusion_prerun_progress)) shared_data$fusion_prerun_progress(100)
  }
  if (!is.null(prog_file) && nzchar(prog_file)) writeLines("100", prog_file)
  
  message("Fusion data prerun completed!")
  TRUE
}


#' Return only fusion patients that still miss required prerun assets
#' @export
fusion_patients_to_prerun <- function(fusion_patients, www_root = "www") {
  if (length(fusion_patients) == 0) return(character(0))
  
  manifest_dir <- file.path(www_root, "manifests", "fusion")
  need <- logical(length(fusion_patients))
  
  for (i in seq_along(fusion_patients)) {
    sample <- fusion_patients[i]
    
    has_manifest <- dir.exists(manifest_dir) &&
      length(list.files(
        manifest_dir,
        pattern = paste0("^", sample, "\\.(tsv|csv)$"),
        ignore.case = TRUE
      )) > 0
    
    png_dir  <- file.path(www_root, "igv_snapshots", sample)
    has_pngs <- dir.exists(png_dir) &&
      length(list.files(png_dir, pattern = "\\.png$", ignore.case = TRUE)) > 0
    
    svg_dir  <- file.path(www_root, "arriba_reports", sample)
    has_svgs <- dir.exists(svg_dir) &&
      length(list.files(svg_dir, pattern = "\\.svg$", ignore.case = TRUE)) > 0
    
    need[i] <- !(has_manifest && has_pngs && has_svgs)
  }
  
  fusion_patients[need]
}



# Helper function to get prerun status
#' @export
get_fusion_prerun_status <- function(shared_data) {
  if (is.null(shared_data$fusion_prerun_status)) {
    return("not_started")
  }
  return(shared_data$fusion_prerun_status())
}


split_genes <- function(dt) {
  dt_gene1 <- dt[, .(gene1 = unlist(strsplit(gene1, ",", fixed = TRUE)),
                     gene2, chr1, pos1, chr2, pos2, path), by = .(.I)]
  dt_gene2 <- dt_gene1[, .(gene1, gene2 = unlist(strsplit(gene2, ",", fixed = TRUE)),
                           chr1, pos1, chr2, pos2, path), by = .(.I)]
  dt_gene2[,I := NULL]
  
  return(dt_gene2)
}
