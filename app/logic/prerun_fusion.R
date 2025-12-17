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
  # Build load commands only for available BAM files
  load_commands <- c()
  
  if (!is.null(tumor_bam) && nzchar(tumor_bam)) {
    # IGV Desktop needs absolute paths - prepend /input_files/ if not already absolute
    if (!grepl("^/", tumor_bam)) {
      tumor_bam <- file.path("/input_files", tumor_bam)
    }
    load_commands <- c(load_commands, paste0("load ", tumor_bam))
  }
  
  if (!is.null(chimeric_bam) && nzchar(chimeric_bam)) {
    # IGV Desktop needs absolute paths - prepend /input_files/ if not already absolute
    if (!grepl("^/", chimeric_bam)) {
      chimeric_bam <- file.path("/input_files", chimeric_bam)
    }
    # Only add chimeric BAM if it's different from tumor BAM
    if (is.null(tumor_bam) || chimeric_bam != tumor_bam) {
      load_commands <- c(load_commands, paste0("load ", chimeric_bam))
    }
  }
  
  # Note: Don't use "new" command - it loads default genome (hg19) then switches to hg38
  # Just use "genome" command directly to load the desired genome
  header <- c(paste0("genome ", genome_build),
              paste0("snapshotDirectory ", output_dir),
              load_commands,
              "viewaspairs",
              "maxPanelHeight 10000",
              "preference SAM.SHOW_SOFT_CLIPPED TRUE",
              "preference SAM.SHOW_JUNCTION_TRACK TRUE",
              "preference SAM.COLOR_BY NONE",
              "preference IGV.Bounds 94,0,1280,1024")
  
  writeLines(paste(header, collapse = "\n"), con = batch_file)
  
  # pro každý řádek: goto + snapshot (jen basename, když máš snapshotDirectory)
  for (i in seq_len(nrow(fusions_tab))) {
    goto_line     <- paste0("goto ", fusions_tab$chrom_break_pos[i])
    snapshot_name <- basename(fusions_tab$png_path[i])
    snap_line     <- paste0("snapshot ", snapshot_name)
    write(goto_line, file = batch_file, append = TRUE)
    write(snap_line, file = batch_file, append = TRUE)
  }
  
  # IGV will exit automatically after processing all commands
  write("exit", file = batch_file, append = TRUE)
  return(batch_file)
}


#' Create fusion table from Excel/TSV file for IGV snapshots
#' @param fusion_file Path to fusion file
#' @param sample_name Sample name
#' @param output_base_dir Base directory for output
#' @param zoom Zoom level around breakpoint (default: 251)
createFusionTableFromFile <- function(fusion_file, sample_name, output_base_dir, zoom = 251) {
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
  fusions_tab[, png_path := sprintf("igv_snapshots/%s/%s_%03d.png", sample_name, sample_name, id)]
  fusions_tab <- unique(fusions_tab)

  return(fusions_tab)
}

#' Run IGV snapshot using batch file watcher (secure, no docker exec needed)
#' @param IGV_batch_file Path to IGV batch file in shared volume
#' @param timeout_seconds Maximum time to wait for IGV to process (default: 300s = 5 min)
#' @param prog_file Optional progress file to update during IGV processing
runIGVSnapshotParallel <- function(IGV_batch_file, timeout_seconds = 300, prog_file = NULL) {
  
  if (!file.exists(IGV_batch_file)) {
    message("ERROR: Batch file does not exist: ", IGV_batch_file)
    return(1)
  }
  
  message("[IGV WATCHER] Batch file created: ", IGV_batch_file)
  message("[IGV WATCHER] Batch file size: ", file.size(IGV_batch_file), " bytes")
  message("[IGV WATCHER] Waiting for IGV container watcher to process...")
  
  # Paths for status files
  done_file <- paste0(IGV_batch_file, ".done")
  error_file <- paste0(IGV_batch_file, ".error")
  progress_file <- paste0(IGV_batch_file, ".progress")  # IGV watcher can write progress here
  
  # Wait for IGV watcher to process the batch file
  start_time <- Sys.time()
  processed <- FALSE
  last_progress <- 5  # Start from 5% (after batch file creation)
  
  while (!processed && difftime(Sys.time(), start_time, units = "secs") < timeout_seconds) {
    # Check if done or error file exists
    if (file.exists(done_file)) {
      message("[IGV WATCHER] ✅ SUCCESS - IGV processing completed")
      message("[IGV WATCHER] Done file: ", done_file)
      if (!is.null(prog_file) && nzchar(prog_file)) writeLines("85", prog_file)
      processed <- TRUE
      return(0)
    } else if (file.exists(error_file)) {
      error_content <- tryCatch(readLines(error_file), error = function(e) "Unknown error")
      message("[IGV WATCHER] ❌ ERROR - IGV processing failed")
      message("[IGV WATCHER] Error: ", paste(error_content, collapse = "\n"))
      if (!is.null(prog_file) && nzchar(prog_file)) writeLines("85", prog_file)  # Mark as done anyway
      return(1)
    }
    
    # Check if IGV watcher is writing progress (optional feature)
    if (file.exists(progress_file)) {
      tryCatch({
        igv_progress <- as.integer(readLines(progress_file, n = 1))
        if (!is.na(igv_progress) && igv_progress > last_progress) {
          # Map IGV progress (0-100) to our range (5-85)
          mapped_progress <- 5 + (igv_progress * 0.8)  # Scale to 5-85%
          last_progress <- mapped_progress
          if (!is.null(prog_file) && nzchar(prog_file)) {
            writeLines(as.character(round(mapped_progress)), prog_file)
          }
        }
      }, error = function(e) {
        # Ignore errors reading progress file
      })
    }
    
    # Wait before checking again
    Sys.sleep(2)
  }
  
  # Timeout reached
  if (!processed) {
    message("[IGV WATCHER] ⏱️ TIMEOUT - IGV did not process batch file within ", timeout_seconds, " seconds (~", round(timeout_seconds/60, 1), " minutes)")
    message("[IGV WATCHER] ")
    message("[IGV WATCHER] 🔍 DIAGNOSTICS:")
    message("[IGV WATCHER]   Batch file: ", IGV_batch_file)
    message("[IGV WATCHER]   Batch file exists: ", file.exists(IGV_batch_file))
    message("[IGV WATCHER]   Expected done file: ", done_file)
    message("[IGV WATCHER]   Expected error file: ", error_file)
    message("[IGV WATCHER]   Done file exists: ", file.exists(done_file))
    message("[IGV WATCHER]   Error file exists: ", file.exists(error_file))
    message("[IGV WATCHER] ")
    message("[IGV WATCHER] 📝 TROUBLESHOOTING STEPS:")
    message("[IGV WATCHER]   1. Check if IGV container is running:")
    message("[IGV WATCHER]      docker ps | grep igv")
    message("[IGV WATCHER]   2. Check IGV container logs:")
    message("[IGV WATCHER]      docker logs igv-static")
    message("[IGV WATCHER]   3. Check if batch file is accessible in container:")
    message("[IGV WATCHER]      docker exec igv-static ls -lh /srv/igv-static/igv_snapshots/")
    message("[IGV WATCHER]   4. Manually test batch file processing:")
    message("[IGV WATCHER]      docker exec igv-static cat ", basename(IGV_batch_file))
    message("[IGV WATCHER]   5. Check watcher script is running in container:")
    message("[IGV WATCHER]      docker exec igv-static ps aux | grep watcher")
    return(2)
  }
  
  return(0)
}


#' Process IGV snapshots for a single patient
#' @param patient_id Patient identifier
#' @param file_list List of files for this patient (fusion, tumor, chimeric)
#' @param output_base_dir Base directory for output (usually "/output_files")
#' @param prog_file Optional progress file to update during processing
process_patient_igv <- function(sample, file_list, output_base_dir, prog_file = NULL) {

  # Get required files
  fusion_file <- file_list$fusion
  tumor_files <- file_list$tumor
  chimeric_files <- file_list$chimeric
  
  if (is.null(fusion_file) || length(fusion_file) == 0) { 
    message("No fusion file: ", sample); 
    return(FALSE) 
  }
  
  # Create fusion table
  fusions_tab <- createFusionTableFromFile(fusion_file, sample, output_base_dir)
  if (is.null(fusions_tab) || nrow(fusions_tab) == 0) { 
    message("No data: ", sample); 
    return(FALSE) 
  }
  

  # Create output directory
  output_dir <- file.path(output_base_dir, "igv_snapshots", sample)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # IGV needs absolute path for snapshotDirectory command
  # (IGV container has shared volume mounted at /output_files)
  igv_snapshot_dir <- file.path(output_base_dir, "igv_snapshots", sample)

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
      # Check if IGV container is likely running by checking if watcher directory exists
      # This is a basic check - the .done/.error files mechanism will catch actual issues
      watcher_check_dir <- dirname(output_dir)
      if (!dir.exists(watcher_check_dir)) {
        message("[IGV] ⚠️  Warning: IGV snapshot base directory doesn't exist: ", watcher_check_dir)
        message("[IGV] Creating directory, but IGV container may not be running")
      }
      
      # Filter out fusions that already have PNG snapshots (resume capability)
      fusions_tab[, png_exists := file.exists(file.path(output_base_dir, png_path))]
      missing_count <- sum(!fusions_tab$png_exists)
      
      if (missing_count == 0) {
        message("[IGV] All snapshots exist - skipping")
        return(TRUE)
      }
      
      message("[IGV] Generating ", missing_count, "/", nrow(fusions_tab), " snapshots")
      fusions_to_process <- fusions_tab[png_exists == FALSE]
      
      batch_file <- createIGVBatchFile(
        tumor_bam = tumor_bam,
        chimeric_bam = chimeric_bam,
        fusions_tab = fusions_to_process,
        batch_file = batch_file,
        output_dir = igv_snapshot_dir,
        genome_build = "hg38")
      
      # Calculate dynamic timeout based on number of missing fusions
      # Base: 300 seconds (5 min) + 0.5 seconds per fusion
      # For 900 fusions: 300 + (900 * 0.5) = 750 seconds (12.5 minutes)
      dynamic_timeout <- max(300, 300 + (missing_count * 0.5))
      message("[IGV] Calculated timeout for ", missing_count, " fusions: ", round(dynamic_timeout), " seconds (~", round(dynamic_timeout/60, 1), " minutes)")
      message("[IGV] ⚠️  Make sure IGV Docker container is running: docker ps | grep igv")
      
      # Spustit IGV snapshot pomocí watcheru (bezpečné, bez docker exec)
      exit_code <- runIGVSnapshotParallel(batch_file, timeout_seconds = dynamic_timeout, prog_file = prog_file)
      
      if (exit_code == 0) {
        message("[IGV] ✅ Snapshots created successfully: ", batch_file)
        return(TRUE)
      } else if (exit_code == 2) {
        message("[IGV] ⏱️ Timeout waiting for IGV watcher")
        return(FALSE)
      } else {
        message("[IGV] ❌ IGV processing failed")
        return(FALSE)
      }
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
process_arriba_pdf <- function(sample, arriba, output_base_dir) {
  if (is.null(arriba) || length(arriba) == 0) return(FALSE)
  
  # Find TSV and PDF files
  tsv_files <- arriba[grepl("\\.tsv$", arriba)]
  pdf_files <- arriba[grepl("\\.pdf$", arriba)]
  
  if (length(tsv_files) == 0 || length(pdf_files) == 0) return(FALSE)
  
  arriba_tsv <- fread(tsv_files[1])
  arriba_pdf <- pdf_files[1]
  
  output_dir <- file.path(output_base_dir, "arriba_reports", sample)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Get PDF page count
  pages <- NA_integer_
  pdfinfo_out <- tryCatch(
    system2("pdfinfo", args = shQuote(arriba_pdf), stdout = TRUE, wait = FALSE),
    error = function(e) character()
  )
  
  if (length(pdfinfo_out)) {
    p_line <- pdfinfo_out[grepl("^Pages:\\s+\\d+", pdfinfo_out)]
    if (length(p_line)) pages <- as.integer(sub("^Pages:\\s+", "", p_line[1]))
  }
  if (is.na(pages)) stop("Cannot determine PDF page count for ", sample)
  
  success_count <- 0
  if (length(list.files(output_dir, pattern = "\\.svg$", ignore.case = TRUE)) == 0) {
    message("[PDF] Converting ", pages, " pages to SVG...")
    for (i in seq_len(pages)) {
      output_svg <- file.path(output_dir, sprintf("%s_%03d.svg", sample, i))
      system2("pdftocairo", args = c("-svg", "-f", as.character(i), "-l", as.character(i), shQuote(arriba_pdf), shQuote(output_svg)), wait = TRUE)
      if (file.exists(output_svg)) success_count <- success_count + 1
    }
    message("[PDF] Created ", success_count, "/", pages, " SVG files")
  } else {
    success_count <- length(list.files(output_dir, pattern = "\\.svg$", ignore.case = TRUE))
  }
  
  return(success_count > 0)
}



#' Create manifest file for fusion images
#' @param patient_id Patient identifier
#' @param fusion_file Path to fusion file
#' @param arriba Vector of arriba file paths (TSV and PDF)
#' @param output_base_dir Base directory for output
create_fusion_manifest <- function(patient_id, fusion_file, arriba, output_base_dir) {
  
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
      
      # Assign SVG paths as RELATIVE paths (without output_files prefix)
      arriba_dt[, path := sprintf("arriba_reports/%s/%s_%03d.svg", patient_id, patient_id, .I)]
      
      # Clean gene names and prepare positions
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
    
    # Use same logic as createFusionTableFromFile to ensure consistent png_path
    required_cols <- c("gene1", "gene2", "chr1", "pos1", "chr2", "pos2")
    dt <- dt[, ..required_cols]
    dt <- dt[, .(gene1 = paste(unique(gene1), collapse = ","),
                 gene2 = paste(unique(gene2), collapse = ",")),
             by = .(chr1, pos1, chr2, pos2)]
    dt[, id := seq_len(.N)]
    dt[, path := sprintf("igv_snapshots/%s/%s_%03d.png", patient_id, patient_id, id)]
    dt <- unique(dt)
    
    # Split genes to match arriba format (multiple genes per position become separate rows)
    dt <- split_genes(dt)
    setnames(dt, "path", "png_path")
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
  output_base_dir <- shared_data$output_path()

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
  base_dirs <- c(file.path(output_base_dir, "igv_snapshots"), file.path(output_base_dir, "arriba_reports"), file.path(output_base_dir, "manifests", "fusion"))
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
  
  # 5) Process patients in PARALLEL using future_lapply
  message("Processing ", length(fusion_patients), " patients in parallel...")
  
  total_patients <- length(fusion_patients)
  
  # Použít future_lapply pro paralelní zpracování
  patient_results <- future.apply::future_lapply(
    seq_along(fusion_patients),
    function(i) {
      sample <- fusion_patients[i]
      file_list <- fusion_files[[sample]]
      
      if (is.null(file_list)) {
        return(list(
          success = FALSE, 
          fusions = 0, 
          error = paste0(sample, ": No files found")
        ))
      }
      
      message("Processing patient ", i, "/", total_patients, ": ", sample)
      
      patient_result <- tryCatch({
        # Count fusions for this patient
        patient_fusions <- 0
        if (!is.null(file_list$fusion) && length(file_list$fusion) > 0) {
          fusion_dt <- fread(file_list$fusion[1])
          patient_fusions <- nrow(fusion_dt)
        }
        
        # Process IGV snapshots (watcher will handle parallel execution safely)
        igv_success <- process_patient_igv(sample, file_list)
        
        # Process Arriba PDF
        arriba_success <- process_arriba_pdf(sample, file_list$arriba, output_base_dir = output_base_dir)
        
        # Create manifest
        fusion_file <- file_list$fusion
        if (length(fusion_file) > 0) {
          create_fusion_manifest(sample, fusion_file[1], file_list$arriba, output_base_dir = output_base_dir)
        }
        
        list(success = TRUE, fusions = patient_fusions, error = NULL)
        
      }, error = function(e) {
        message("Error processing patient ", sample, ": ", e$message)
        list(success = FALSE, fusions = 0, error = paste0(sample, ": ", e$message))
      })
      
      # Update progress
      current_progress <- round((i / total_patients) * 100)
      if (!is.null(prog_file) && nzchar(prog_file)) {
        writeLines(as.character(current_progress), prog_file)
      }
      
      return(patient_result)
    },
    future.seed = TRUE
  )
  
  # Sbírat errors ze všech results
  errors <- character()
  for (result in patient_results) {
    if (!is.null(result$error)) {
      errors <- c(errors, result$error)
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
fusion_patients_to_prerun <- function(fusion_patients, www_root) {
  if (length(fusion_patients) == 0) return(character(0))
  
  manifest_dir <- file.path(www_root, "manifests", "fusion")
  need <- logical(length(fusion_patients))
  
  for (i in seq_along(fusion_patients)) {
    sample <- fusion_patients[i]
    
    # Check if manifest exists
    manifest_file <- file.path(manifest_dir, paste0(sample, ".tsv"))
    has_manifest <- file.exists(manifest_file)
    
    if (!has_manifest) {
      # No manifest = need full prerun
      need[i] <- TRUE
      next
    }
    
    # Read manifest to get expected number of UNIQUE files
    tryCatch({
      manifest_dt <- fread(manifest_file)
      
      # Count UNIQUE paths (manifest has duplicates for multi-gene fusions)
      expected_pngs <- uniqueN(manifest_dt$png_path, na.rm = TRUE)
      expected_svgs <- uniqueN(manifest_dt$svg_path, na.rm = TRUE)
      
      # Count actual PNG files
      png_dir <- file.path(www_root, "igv_snapshots", sample)
      actual_pngs <- if (dir.exists(png_dir)) {
        length(list.files(png_dir, pattern = "\\.png$", ignore.case = TRUE))
      } else {
        0
      }
      
      # Count actual SVG files  
      svg_dir <- file.path(www_root, "arriba_reports", sample)
      actual_svgs <- if (dir.exists(svg_dir)) {
        length(list.files(svg_dir, pattern = "\\.svg$", ignore.case = TRUE))
      } else {
        0
      }
      
      # Need prerun if counts don't match (incomplete processing)
      need[i] <- (actual_pngs < expected_pngs) || (actual_svgs < expected_svgs)
      
      if (need[i]) {
        message("[PRERUN CHECK] ", sample, " incomplete: expected_pngs=", expected_pngs, 
                ", actual_pngs=", actual_pngs, ", expected_svgs=", expected_svgs, 
                ", actual_svgs=", actual_svgs)
      } else {
        message("[PRERUN CHECK] ", sample, " complete: ", actual_pngs, " PNGs, ", actual_svgs, " SVGs")
      }
      
    }, error = function(e) {
      message("[PRERUN CHECK] Error reading manifest for ", sample, ": ", e$message)
      need[i] <- TRUE  # If we can't read manifest, assume we need prerun
    })
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
prerun_fusion_patient <- function(patient_id, file_list, prog_file = NULL, output_base_dir) {
  message("[PRERUN] Starting for ", patient_id)
  
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
    
    # Step 1: Process IGV snapshots (5-85%)
    if (!is.null(prog_file)) writeLines("5", prog_file)
    igv_success <- process_patient_igv(patient_id, file_list, output_base_dir, prog_file = prog_file)
    if (!is.null(prog_file)) writeLines("85", prog_file)
    
    # Step 2: Process Arriba PDF (85-95%)
    arriba_success <- FALSE
    if (!is.null(file_list$arriba) && length(file_list$arriba) > 0) {
      arriba_success <- process_arriba_pdf(patient_id, file_list$arriba, output_base_dir)
    }
    
    if (!is.null(prog_file)) writeLines("95", prog_file)
    
    # Step 3: Create manifest (95-100%)
    fusion_file <- file_list$fusion
    if (length(fusion_file) > 0 && !is.null(file_list$arriba)) {
      create_fusion_manifest(patient_id, fusion_file[1], file_list$arriba, output_base_dir)
    } else {
      errors <- c(errors, paste0(patient_id, ": Missing files for manifest creation"))
    }
    
    if (!is.null(prog_file)) writeLines("100", prog_file)
    message("[PRERUN] Completed for ", patient_id, " - IGV:", igv_success, " Arriba:", arriba_success)
    message("[PRERUN] ", patient_id, " returning success=TRUE")
    
  }, error = function(e) {
    err_msg <- sprintf("[ERROR] %s: %s", patient_id, e$message)
    message(err_msg)
    message("[PRERUN] ", patient_id, " returning success=FALSE (error)")
    
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
