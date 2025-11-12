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
  app/logic/helper_main[get_patients,get_files_by_patient],
)

#' Create IGV batch file for taking snapshots
#' @param bam_file Path to BAM file
#' @param chimeric_file Path to chimeric BAM file
#' @param chrom_break_pos Position string like "chr1:100-200 chr2:300-400"
#' @param snapshot_name Name for the snapshot file
#' @param batch_file_name Name for the batch file
#' @param output_dir Output directory for snapshots
#' @param genome_build Genome build (default: hg38)
createIGVBatchFile <- function(tumor_bam, chimeric_bam, fusions_tab, batch_file, output_dir, genome_build) {
  header <- paste("new",
                  paste0("genome ", genome_build),
                  paste0("snapshotDirectory ", output_dir),
                  paste0("load ", tumor_bam),
                  paste0("load ", chimeric_bam),
                  "viewaspairs",
                  "maxPanelHeight 10000",
                  "preference SAM.SHOW_SOFT_CLIPPED TRUE",
                  "preference SAM.SHOW_JUNCTION_TRACK TRUE",
                  "preference SAM.COLOR_BY NONE",
                  "preference IGV.Bounds 94,0,1280,1024",
                   sep = "\n")
  writeLines(header, con = batch_file)
  
  # pro každý řádek: goto + snapshot (jen basename, když máš snapshotDirectory)
  for (i in seq_len(nrow(fusions_tab))) {
    goto_line     <- paste0("goto ", fusions_tab$chrom_break_pos[i])
    snapshot_name <- basename(fusions_tab$png_path[i])
    snap_line     <- paste0("snapshot ", snapshot_name)
    write(goto_line, file = batch_file, append = TRUE)
    write(snap_line, file = batch_file, append = TRUE)
  }
  
  write("exit", file = batch_file, append = TRUE)
  return(batch_file)
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
  # ═══════════════════════════════════════════════════════════════════════════
  # COMMENTED OUT FOR TESTING - Replaced with Sys.sleep() to simulate processing
  # ═══════════════════════════════════════════════════════════════════════════
  # igv_executive <- "igv"
  # igv_command <- paste0("xvfb-run --auto-servernum --server-args='-screen 0 1280x1024x24 -ac' ",igv_executive," -b ",IGV_batch_file)
  # 
  # result <- tryCatch({
  #   system(igv_command, wait = FALSE)
  # 
  # }, error = function(e) {
  #   message("Error running IGV command: ", e$message)
  #   return(1)
  # })
  # 
  # # Clean up batch file
  # if (file.exists(IGV_batch_file)) {
  #   file.remove(IGV_batch_file)
  # }
  # return(result)
  
  # SIMULATION: Sleep to mimic IGV processing time
  message("  [SIMULATION] Running IGV snapshot (sleeping 5 seconds)...")
  Sys.sleep(5)
  
  # Clean up batch file
  if (file.exists(IGV_batch_file)) {
    file.remove(IGV_batch_file)
  }
  
  return(0)  # Success

}

#' Process IGV snapshots for a single patient
#' @param patient_id Patient identifier
#' @param file_list List of files for this patient (fusion, tumor, chimeric)
#' @param output_base_dir Base directory for output (usually "./www")
process_patient_igv <- function(sample, file_list, output_base_dir = "./www") {
  message("Processing IGV snapshots for patient: ", sample)

  # Get required files
  fusion_file <- file_list$fusion
  tumor_files <- file_list$tumor
  chimeric_files <- file_list$chimeric
  
  if (is.null(fusion_file) || length(fusion_file) == 0) { 
    message("No fusion file: ", sample); 
    return(FALSE) 
  }
  
  # Create fusion table
  fusions_tab <- createFusionTableFromFile(fusion_file, sample)
  if (is.null(fusions_tab) || nrow(fusions_tab) == 0) { 
    message("No data: ", sample); 
    return(FALSE) 
  }
  

  # Create output directory
  output_dir <- file.path(output_base_dir, "igv_snapshots", sample)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Find BAM files (optional for IGV) - handle NULL safely
  tumor_bam <- NULL
  if (!is.null(tumor_files) && length(tumor_files) > 0) {
    bam_matches <- tumor_files[grepl("\\.bam$", tumor_files)]
    if (length(bam_matches) > 0) tumor_bam <- bam_matches[1]
  }
  
  chimeric_bam <- NULL
  if (!is.null(chimeric_files) && length(chimeric_files) > 0) {
    bam_matches <- chimeric_files[grepl("\\.bam$", chimeric_files)]
    if (length(bam_matches) > 0) chimeric_bam <- bam_matches[1]
  }

  if (is.null(tumor_bam) || is.null(chimeric_bam)) {
    message("Warning: Missing BAM files for patient ", sample, ". IGV snapshots may not be complete.")
  }

  batch_file = file.path(output_dir, paste0(sample,"_batch.txt"))

  tryCatch({
    if (!is.null(tumor_bam) && !is.null(chimeric_bam)) {
      batch_file <- createIGVBatchFile(
        tumor_bam = tumor_bam,
        chimeric_bam = chimeric_bam,
        fusions_tab = fusions_tab,
        batch_file = batch_file,
        output_dir = output_dir,
        genome_build = "hg38")
      
      # runIGVSnapshot(batch_file)
      message("IGV batch file created: ", batch_file)
      return(TRUE)
    } else {
      message("Skipping IGV snapshot generation - missing BAM files")
      return(TRUE)  # Still return TRUE - this is not a fatal error
    }
  }, error = function(e) {
    message("Error processing fusions for patient ", sample, ": ", e$message)
    return(FALSE)
  })

}

#' Process Arriba images for a single patient
#' @param sample Sample identifier
#' @param arriba_pdf List of files for this patient
#' @param output_base_dir Base directory for output
process_arriba_pdf <- function(sample, arriba, output_base_dir = "./www") {
  message("[PDF] Processing Arriba images for patient: ", sample)
  message("[PDF] Received arriba parameter: is.null=", is.null(arriba), ", length=", if(!is.null(arriba)) length(arriba) else "NULL")
  if (!is.null(arriba) && length(arriba) > 0) {
    message("[PDF] Arriba files: ", paste(arriba, collapse = ", "))
  }
  
  success_count <- 0
  
  # Validate arriba files exist
  if (is.null(arriba) || length(arriba) == 0) {
    message("[PDF] ❌ No arriba files provided for patient: ", sample)
    return(FALSE)
  }
  
  # Find TSV and PDF files
  message("[PDF] Searching for TSV and PDF files...")
  tsv_files <- arriba[grepl("\\.tsv$", arriba)]
  pdf_files <- arriba[grepl("\\.pdf$", arriba)]
  message("[PDF] Found ", length(tsv_files), " TSV files and ", length(pdf_files), " PDF files")
  
  if (length(tsv_files) == 0) {
    message("[PDF] ❌ No Arriba TSV file found for patient: ", sample)
    return(FALSE)
  }
  
  if (length(pdf_files) == 0) {
    message("[PDF] ❌ No Arriba PDF file found for patient: ", sample)
    return(FALSE)
  }
  
  message("[PDF] Using TSV: ", tsv_files[1])
  message("[PDF] Using PDF: ", pdf_files[1])
  message("[PDF] Reading TSV file with fread()...")
  arriba_tsv <- fread(tsv_files[1])
  message("[PDF] TSV read successfully, rows: ", nrow(arriba_tsv))
  arriba_pdf <- pdf_files[1]
  
  output_dir <- file.path(output_base_dir, "arriba_reports", sample)
  message("[PDF] Output directory: ", output_dir)
  if (!dir.exists(output_dir)) {
    message("[PDF] Creating output directory...")
    dir.create(output_dir, recursive = TRUE)
  }
  
  message("[PDF] Getting PDF page count with pdfinfo...")
  pages <- NA_integer_  # kolik stran má PDF (pdftocairo pak pojedeme po jedné stránce)
  pdfinfo_out <- tryCatch(
    system2("pdfinfo", args = shQuote(arriba_pdf), stdout = TRUE, wait = FALSE),
    error = function(e) {
      message("[PDF] pdfinfo error: ", e$message)
      character()
    }
  )
  message("[PDF] pdfinfo output length: ", length(pdfinfo_out))
  if (length(pdfinfo_out)) {
    p_line <- pdfinfo_out[grepl("^Pages:\\s+\\d+", pdfinfo_out)]
    if (length(p_line)) {
      pages <- as.integer(sub("^Pages:\\s+", "", p_line[1]))
      message("[PDF] ✅ PDF has ", pages, " pages")
    }
  }
  if (is.na(pages)) {
    message("[PDF] ❌ Failed to determine page count")
    stop("Nepodařilo se zjistit počet stran PDF pro sample ", sample)
  }
  
  if (length(list.files(output_dir, pattern = "\\.svg$", ignore.case = TRUE)) == 0) {
    # Convert PDF pages to SVG using pdftocairo
    message("[PDF] 🔄 SVG files don't exist - splitting PDF to ", pages, " SVG files using pdftocairo...")
    for (i in seq_len(pages)) {
      output_svg <- file.path(output_dir, sprintf("%s_%03d.svg", sample, i))
      system2("pdftocairo", args = c("-svg", "-f", as.character(i), "-l", as.character(i), shQuote(arriba_pdf), shQuote(output_svg)), wait = TRUE)
      if (file.exists(output_svg)) success_count <- success_count + 1
    }
    message("[PDF] ✅ Created ", success_count, "/", pages, " SVG files")
  } else {
    existing_svg_count <- length(list.files(output_dir, pattern = "\\.svg$", ignore.case = TRUE))
    message("[PDF] ⏭️  SVG files already exist (", existing_svg_count, " files) - skipping PDF conversion")
    success_count <- existing_svg_count
  }
  
  message("[PDF] Arriba PDF processing completed for patient ", sample, ": ",
          success_count, "/", nrow(arriba_tsv), " SVG files available")
  return(success_count > 0)
}



#' Create manifest file for fusion images
#' @param patient_id Patient identifier
#' @param fusion_file Path to fusion file
#' @param arriba Vector of arriba file paths (TSV and PDF)
#' @param output_base_dir Base directory for output
create_fusion_manifest <- function(patient_id, fusion_file, arriba, output_base_dir = "./www") {
  
  message("[MANIFEST] Creating manifest for patient: ", patient_id)
  message("[MANIFEST] arriba parameter: is.null=", is.null(arriba), ", length=", if(!is.null(arriba)) length(arriba) else "NULL")
  
  if (is.null(arriba) || length(arriba) == 0) {
    message("[MANIFEST] ❌ No arriba files provided")
    return(FALSE)
  }
  
  # Find TSV file
  tsv_files <- arriba[grepl("\\.tsv$", arriba)]
  message("[MANIFEST] Found ", length(tsv_files), " TSV file(s)")
  
  if (length(tsv_files) == 0) {
    message("[MANIFEST] ❌ No TSV file found in arriba list")
    return(FALSE)
  }
  
  arriba_tsv <- tsv_files[1]
  message("[MANIFEST] Using TSV: ", arriba_tsv)

  tryCatch({
    if (file.exists(arriba_tsv)){ # prepare_arriba_image_paths
      message("[MANIFEST] Reading TSV file...")
      arriba_dt <- fread(arriba_tsv)
      message("[MANIFEST] TSV loaded, rows: ", nrow(arriba_dt))
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
    } else {
      message("[MANIFEST] ❌ TSV file does not exist: ", arriba_tsv)
      return(FALSE)
    }
  }, error = function(e) {
    message("[MANIFEST ERROR] Failed to process arriba TSV: ", e$message)
    message("[MANIFEST ERROR] Traceback: ", paste(deparse(sys.calls()), collapse = "\n"))
    return(FALSE)
  })
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
prerun_fusion_data <- function(confirmed_paths, shared_data, prog_file = NULL) {
  message("Starting fusion data prerun...")
  
  # 1) Inicializace stavu
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
    return(list(success = TRUE, errors = character()))
  }
  
  fusion_files <- get_files_by_patient(confirmed_paths, "fusion")
  message("Found ", length(fusion_patients), " fusion patients: ", paste(fusion_patients, collapse = ", "))
  
  # 3) Základní adresáře (idempotentně)
  base_dirs <- c("./www/igv_snapshots", "./www/arriba_reports", "./www/manifests/fusion")
  for (dir in base_dirs) {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  }
  
  # 4) Count total fusions for granular progress
  total_fusions <- 0
  for (sample in fusion_patients) {
    file_list <- fusion_files[[sample]]
    if (!is.null(file_list$fusion) && length(file_list$fusion) > 0) {
      fusion_dt <- fread(file_list$fusion[1])
      total_fusions <- total_fusions + nrow(fusion_dt)
    }
  }
  message("Total fusions to process: ", total_fusions)
  
  # Progress tracking
  processed_fusions <- 0
  errors <- character()
  
  # 5) Process patients (sequentially within the async future block from main.R)
  # Note: Using furrr here causes serialization issues with large data.table objects.
  # The async benefit comes from the entire prerun running in a future() in main.R.
  message("Processing ", length(fusion_patients), " patients...")
  
  total_patients <- length(fusion_patients)
  
  for (i in seq_along(fusion_patients)) {
    sample <- fusion_patients[i]
    file_list <- fusion_files[[sample]]
    
    if (is.null(file_list)) {
      errors <- c(errors, paste0(sample, ": No files found"))
      next
    }
    
    message("Processing patient ", i, "/", total_patients, ": ", sample)
    
    patient_result <- tryCatch({
      # Count fusions for this patient
      patient_fusions <- 0
      if (!is.null(file_list$fusion) && length(file_list$fusion) > 0) {
        fusion_dt <- fread(file_list$fusion[1])
        patient_fusions <- nrow(fusion_dt)
      }
      
      # Process IGV snapshots
      igv_success <- process_patient_igv(sample, file_list)
      
      # Process Arriba PDF
      arriba_success <- process_arriba_pdf(sample, file_list$arriba, output_base_dir = "./www")
      
      # Create manifest
      fusion_file <- file_list$fusion
      if (length(fusion_file) > 0) {
        create_fusion_manifest(sample, fusion_file[1], file_list$arriba, output_base_dir = "./www")
      }
      
      list(success = TRUE, fusions = patient_fusions)
      
    }, error = function(e) {
      message("Error processing patient ", sample, ": ", e$message)
      errors <<- c(errors, paste0(sample, ": ", e$message))
      list(success = FALSE, fusions = 0)
    })
    
    # Update progress
    current_progress <- round((i / total_patients) * 100)
    if (!is.null(prog_file) && nzchar(prog_file)) {
      writeLines(as.character(current_progress), prog_file)
    }
  }
  
  # 6) Dokončení
  if (!is.null(shared_data)) {
    if (is.function(shared_data$fusion_prerun_status)) {
      shared_data$fusion_prerun_status(if (length(errors) == 0) "completed" else "failed")
    }
    if (is.function(shared_data$fusion_prerun_progress)) {
      shared_data$fusion_prerun_progress(100)
    }
  }
  if (!is.null(prog_file) && nzchar(prog_file)) writeLines("100", prog_file)
  
  if (length(errors) > 0) {
    message("Fusion data prerun completed with ", length(errors), " error(s)")
  } else {
    message("Fusion data prerun completed successfully!")
  }
  
  list(success = length(errors) == 0, errors = errors)
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


#' Process fusion data for a SINGLE patient (for parallel execution)
#' @param patient_id Patient identifier
#' @param file_list List of files for this patient (fusion, tumor, chimeric, arriba)
#' @param prog_file Path to progress file for cross-process communication
#' @param output_base_dir Base directory for output (usually "./www")
#' @export
prerun_fusion_patient <- function(patient_id, file_list, prog_file = NULL, output_base_dir = "./www") {
  message(sprintf("=== FUSION PRERUN STARTED for %s ===", patient_id))
  message("[PRERUN] Patient: ", patient_id)
  message("[PRERUN] file_list structure:")
  message("[PRERUN]   - fusion: ", if(!is.null(file_list$fusion)) paste(file_list$fusion, collapse=", ") else "NULL")
  message("[PRERUN]   - tumor: ", if(!is.null(file_list$tumor)) paste(file_list$tumor, collapse=", ") else "NULL")
  message("[PRERUN]   - chimeric: ", if(!is.null(file_list$chimeric)) paste(file_list$chimeric, collapse=", ") else "NULL")
  message("[PRERUN]   - arriba: ", if(!is.null(file_list$arriba)) paste(file_list$arriba, collapse=", ") else "NULL")
  
  # Initialize progress
  if (!is.null(prog_file) && nzchar(prog_file)) writeLines("0", prog_file)
  
  errors <- character()
  warnings <- character()
  messages <- character()
  
  tryCatch({
    # Create base directories if needed
    base_dirs <- c(
      file.path(output_base_dir, "igv_snapshots"),
      file.path(output_base_dir, "arriba_reports"),
      file.path(output_base_dir, "manifests", "fusion")
    )
    for (dir in base_dirs) {
      if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    }
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Step 1: Process IGV snapshots (0-40%) - COMMENTED OUT FOR NOW
    # ═══════════════════════════════════════════════════════════════════════════
    # message(sprintf("[%s] Step 1/3: Processing IGV snapshots...", patient_id))
    # if (!is.null(prog_file)) writeLines("10", prog_file)
    # 
    # igv_success <- process_patient_igv(patient_id, file_list, output_base_dir)
    # 
    # if (!is.null(prog_file)) writeLines("40", prog_file)
    # message(sprintf("[%s] IGV snapshots: %s", patient_id, if(igv_success) "SUCCESS" else "SKIPPED"))
    
    # Skip IGV for now - start at 40%
    if (!is.null(prog_file)) writeLines("40", prog_file)
    message(sprintf("[%s] Skipping IGV snapshots (not implemented yet)", patient_id))
    
    # Step 2: Process Arriba PDF (40-80%)
    message(sprintf("[%s] Step 1/2: Converting Arriba PDF to SVG...", patient_id))
    message(sprintf("[PRERUN] Checking arriba files for %s...", patient_id))
    message(sprintf("[PRERUN] file_list$arriba is.null: %s", is.null(file_list$arriba)))
    if (!is.null(file_list$arriba)) {
      message(sprintf("[PRERUN] file_list$arriba length: %s", length(file_list$arriba)))
      message(sprintf("[PRERUN] file_list$arriba content: %s", paste(file_list$arriba, collapse=", ")))
    }
    
    if (!is.null(prog_file)) writeLines("50", prog_file)
    
    arriba_success <- FALSE
    if (!is.null(file_list$arriba) && length(file_list$arriba) > 0) {
      message(sprintf("[PRERUN] Calling process_arriba_pdf for %s...", patient_id))
      arriba_success <- process_arriba_pdf(patient_id, file_list$arriba, output_base_dir)
      message(sprintf("[PRERUN] process_arriba_pdf returned: %s", arriba_success))
    } else {
      message(sprintf("[%s] ⏭️ No Arriba files found, skipping PDF conversion", patient_id))
    }
    
    if (!is.null(prog_file)) writeLines("80", prog_file)
    message(sprintf("[%s] Arriba PDF: %s", patient_id, if(arriba_success) "SUCCESS" else "SKIPPED"))
    
    # Step 2: Create manifest (80-100%)
    message(sprintf("[%s] Step 2/2: Creating manifest...", patient_id))
    if (!is.null(prog_file)) writeLines("90", prog_file)
    
    fusion_file <- file_list$fusion
    if (length(fusion_file) > 0 && !is.null(file_list$arriba)) {
      create_fusion_manifest(patient_id, fusion_file[1], file_list$arriba, output_base_dir)
      message(sprintf("[%s] Manifest created successfully", patient_id))
    } else {
      warning(sprintf("[%s] Cannot create manifest - missing files", patient_id))
      errors <- c(errors, paste0(patient_id, ": Missing files for manifest creation"))
    }
    
    if (!is.null(prog_file)) writeLines("100", prog_file)
    
  }, error = function(e) {
    err_msg <- sprintf("[ERROR] %s: %s", patient_id, e$message)
    message(err_msg)
    
    # Capture traceback
    tb <- sys.calls()
    if (length(tb) > 0) {
      message("[ERROR] Traceback:")
      for (i in seq_along(tb)) {
        message(sprintf("  %d: %s", i, deparse(tb[[i]])[1]))
      }
    }
    
    errors <<- c(errors, paste0(patient_id, ": ", e$message))
  })
  
  message(sprintf("=== FUSION PRERUN COMPLETED for %s ===", patient_id))
  
  list(
    success = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    messages = messages,
    progress = 100,
    patient_id = patient_id
  )
}
