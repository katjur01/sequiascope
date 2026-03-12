# app/logic/prerun_fusion.R

box::use(
  shiny[reactiveVal],
  data.table[fread, fwrite, as.data.table, setnames, tstrsplit, uniqueN],
  openxlsx[read.xlsx],
  stringr[str_replace_all, str_remove]
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
#' @param session_dir Session directory for output
#' @param zoom Zoom level around breakpoint (default: 251)
createFusionTableFromFile <- function(fusion_file, sample_name, session_dir, zoom = 251) {
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
#' @param prog_start Progress percentage at IGV start (default 15, after Arriba)
#' @param prog_end   Progress percentage at IGV finish (default 90)
runIGVSnapshotParallel <- function(IGV_batch_file, timeout_seconds = 300, prog_file = NULL,
                                   prog_start = 15L, prog_end = 90L) {

  # ── Persistent log file (survives both success and failure) ───────────────
  # Location: <patient_dir>/<patient>_batch.txt.igv.log
  # Bash watcher IGV stdout lives in <patient>_batch.txt.log (separate file)
  igv_log_file <- paste0(IGV_batch_file, ".igv.log")

  igv_log <- function(..., level = "INFO") {
    ts  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    msg <- paste0("[", ts, "] [", level, "] ", paste0(..., collapse = ""))
    message(msg)
    tryCatch(
      cat(msg, "\n", file = igv_log_file, append = TRUE, sep = ""),
      error = function(e) NULL   # never crash because of logging
    )
  }

  # ── Paths for signal files ─────────────────────────────────────────────────
  done_file       <- paste0(IGV_batch_file, ".done")
  error_file      <- paste0(IGV_batch_file, ".error")
  progress_file   <- paste0(IGV_batch_file, ".progress")
  inprogress_file <- paste0(IGV_batch_file, ".inprogress")
  bash_log_file   <- paste0(IGV_batch_file, ".log")   # written by bash watcher

  if (!file.exists(IGV_batch_file)) {
    igv_log("Batch file does not exist: ", IGV_batch_file, level = "ERROR")
    return(1)
  }

  igv_log("=== IGV run started ===")
  igv_log("Batch file : ", IGV_batch_file)
  igv_log("Batch size : ", file.size(IGV_batch_file), " bytes")
  igv_log("Timeout    : ", timeout_seconds, " s")
  igv_log("Bash log   : ", bash_log_file, " (written by IGV container watcher)")

  # Helper: read .error, log details, write final entry to igv_log, return 1
  abort_with_error <- function(source) {
    error_content <- tryCatch(readLines(error_file), error = function(e) "(unreadable)")
    igv_log(source, " – tabulka fúzí se načte bez IGV obrázků", level = "ERROR")
    igv_log("Obsah .error: ", paste(error_content, collapse = " | "), level = "ERROR")
    igv_log("Bash log: ", bash_log_file, level = "ERROR")
    igv_log("=== IGV run ended (FAILED) ===")
    if (!is.null(prog_file) && nzchar(prog_file)) writeLines(as.character(prog_end), prog_file)
    return(1)
  }

  # Clear signal files from any previous run
  if (file.exists(error_file)) unlink(error_file)
  if (file.exists(done_file))  unlink(done_file)

  # ── Watcher alive check ───────────────────────────────────────────────────
  # Bash watcher runs only inside the Docker IGV container.
  # If no .inprogress / .done / .error appears within 15 s → watcher not running.
  igv_log("Čekám na potvrzení od watcheru (max 15 s)...")
  watcher_wait  <- 0L
  watcher_alive <- FALSE
  while (watcher_wait < 15L) {
    if (file.exists(inprogress_file) || file.exists(done_file) || file.exists(error_file)) {
      watcher_alive <- TRUE
      break
    }
    Sys.sleep(1)
    watcher_wait <- watcher_wait + 1L
  }

  if (!watcher_alive) {
    igv_log("Watcher neodpověděl za 15 s – IGV kontejner pravděpodobně není spuštěn", level = "WARN")
    igv_log("Tabulka fúzí se načte bez IGV obrázků", level = "WARN")
    igv_log("Tip: docker compose up -d igv", level = "WARN")
    igv_log("=== IGV run ended (NO WATCHER) ===")
    if (!is.null(prog_file) && nzchar(prog_file)) writeLines(as.character(prog_end), prog_file)
    return(3)  # 3 = watcher not running
  }

  igv_log("Watcher potvrdil zahájení zpracování (za ", watcher_wait, " s)")
  # ── End watcher alive check ───────────────────────────────────────────────

  # ── Parse batch file: get snapshot output dir and expected PNG count ──────
  # The batch file contains:
  #   snapshotDirectory /path/to/dir
  #   snapshot SomeName.png   (one per fusion)
  batch_lines       <- tryCatch(readLines(IGV_batch_file, warn = FALSE), error = function(e) character())
  snapshot_dir      <- sub("^snapshotDirectory\\s+", "",
                           grep("^snapshotDirectory", batch_lines, value = TRUE)[1])
  expected_png      <- sum(grepl("^snapshot ", batch_lines))
  igv_log("Snapshot dir  : ", snapshot_dir)
  igv_log("Expected PNGs : ", expected_png)
  # ── Main wait loop ────────────────────────────────────────────────────────
  start_time    <- Sys.time()
  processed     <- FALSE
  last_progress <- prog_start

  while (!processed && difftime(Sys.time(), start_time, units = "secs") < timeout_seconds) {

    # Early abort on .error (watchdog kill, Xvfb fail, 0 PNGs, hard timeout)
    if (file.exists(error_file)) return(abort_with_error("IGV error detected"))

    # ── Primary check: count actual PNGs on disk ──────────────────────────
    # This is the most reliable signal — works even when watcher is still
    # in cleanup/exit phase and hasn't written .done yet.
    actual_png <- 0L
    if (!is.na(snapshot_dir) && nzchar(snapshot_dir) && dir.exists(snapshot_dir)) {
      actual_png <- length(list.files(snapshot_dir, pattern = "\\.png$", ignore.case = TRUE))
    }

    if (expected_png > 0L && actual_png >= expected_png) {
      elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")))
      igv_log("Všechny PNG vytvořeny (", actual_png, "/", expected_png, ") za ", elapsed,
              " s – nečekám na exit IGV procesu")
      igv_log("=== IGV run ended (ALL PNG DONE) ===")
      if (!is.null(prog_file) && nzchar(prog_file)) writeLines(as.character(prog_end), prog_file)
      return(0)
    }

    # ── Fallback: .done file (IGV exited cleanly before we polled) ────────
    if (file.exists(done_file)) {
      elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")))
      igv_log("SUCCESS via .done – IGV dokončil zpracování za ", elapsed, " s")
      igv_log("=== IGV run ended (SUCCESS) ===")
      if (!is.null(prog_file) && nzchar(prog_file)) writeLines(as.character(prog_end), prog_file)
      return(0)
    }

    # ── Update progress bar based on actual PNG count ─────────────────────
    if (expected_png > 0L && actual_png > 0L) {
      mapped <- prog_start + as.integer((actual_png / expected_png) * (prog_end - prog_start))
      if (mapped > last_progress) {
        last_progress <- mapped
        if (!is.null(prog_file) && nzchar(prog_file))
          writeLines(as.character(mapped), prog_file)
      }
    }

    Sys.sleep(2)
  }
  # ── End wait loop ─────────────────────────────────────────────────────────

  # Timeout – log actual PNG count so user can see how far IGV got
  elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")))
  actual_png_final <- 0L
  if (!is.na(snapshot_dir) && nzchar(snapshot_dir) && dir.exists(snapshot_dir)) {
    actual_png_final <- length(list.files(snapshot_dir, pattern = "\\.png$", ignore.case = TRUE))
  }
  igv_log("TIMEOUT po ", elapsed, " s (limit: ", timeout_seconds, " s)", level = "WARN")
  igv_log("PNG na disku: ", actual_png_final, "/", expected_png, level = "WARN")
  igv_log("Tabulka fúzí se načte bez IGV obrázků", level = "WARN")
  igv_log("batch     : ", IGV_batch_file,  " (exists: ", file.exists(IGV_batch_file), ")", level = "WARN")
  igv_log(".done     : ", done_file,        " (exists: ", file.exists(done_file), ")",       level = "WARN")
  igv_log(".error    : ", error_file,       " (exists: ", file.exists(error_file), ")",      level = "WARN")
  igv_log(".inprogress: ", inprogress_file, " (exists: ", file.exists(inprogress_file), ")", level = "WARN")
  igv_log("Bash log  : ", bash_log_file,    " (exists: ", file.exists(bash_log_file), ")",   level = "WARN")
  igv_log("Tip: docker ps | grep igv && docker logs sequiascope-igv | tail -30", level = "WARN")
  igv_log("=== IGV run ended (TIMEOUT) ===")
  return(2)
}


#' Process IGV snapshots for a single patient
#' @param patient_id Patient identifier
#' @param file_list List of files for this patient (fusion, tumor, chimeric)
#' @param session_dir Session directory for output
#' @param prog_file Optional progress file to update during processing
process_patient_igv <- function(sample, file_list, session_dir, prog_file = NULL, genome_build) {

  # Get required files
  fusion_file <- file_list$fusion
  tumor_files <- file_list$tumor
  chimeric_files <- file_list$chimeric
  
  if (is.null(fusion_file) || length(fusion_file) == 0) { 
    message("No fusion file: ", sample); 
    return(FALSE) 
  }
  
  # Create fusion table
  fusions_tab <- createFusionTableFromFile(fusion_file, sample, session_dir)
  if (is.null(fusions_tab) || nrow(fusions_tab) == 0) { 
    message("No data: ", sample); 
    return(FALSE) 
  }
  

  # Create output directory
  output_dir <- file.path(session_dir, "igv_snapshots", sample)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # IGV needs absolute path for snapshotDirectory command
  # (IGV container has shared volume mounted at /output_files)
  igv_snapshot_dir <- file.path(session_dir, "igv_snapshots", sample)

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
      fusions_tab[, png_exists := file.exists(file.path(session_dir, png_path))]
      missing_count <- sum(!fusions_tab$png_exists)
      
      if (missing_count == 0) {
        message("[IGV] All snapshots exist - skipping")
        return(TRUE)
      }
      
      message("[IGV] Generating ", missing_count, "/", nrow(fusions_tab), " snapshots")
      fusions_to_process <- fusions_tab[png_exists == FALSE]
      
      message("[IGV] Using genome build: ", genome_build)
      batch_file <- createIGVBatchFile(
        tumor_bam = tumor_bam,
        chimeric_bam = chimeric_bam,
        fusions_tab = fusions_to_process,
        batch_file = batch_file,
        output_dir = igv_snapshot_dir,
        genome_build = genome_build)
      
      # Calculate dynamic timeout based on number of missing fusions.
      # Empirically: IGV needs ~60s startup + ~30s per snapshot (BAM loading over Docker volume).
      # Examples: 14 fusions → 480s (8 min), 20 fusions → 660s (11 min).
      # Capped at 1800s (matches watcher hard-timeout).
      dynamic_timeout <- min(1800, max(300, 60 + (missing_count * 30)))
      message("[IGV] Calculated timeout for ", missing_count, " fusions: ", round(dynamic_timeout), " seconds (~", round(dynamic_timeout/60, 1), " minutes)")
      message("[IGV] ⚠️  Make sure IGV Docker container is running: docker ps | grep igv")
      
      # Spustit IGV snapshot pomocí watcheru (bezpečné, bez docker exec)
      exit_code <- runIGVSnapshotParallel(batch_file, timeout_seconds = dynamic_timeout,
                                           prog_file = prog_file, prog_start = 15L, prog_end = 90L)
      
      sentinel_file <- file.path(igv_snapshot_dir, ".no_igv_watcher")
      if (exit_code == 0) {
        message("[IGV] ✅ Snapshots created successfully: ", batch_file)
        # Remove sentinel if it existed from a previous K8s run — IGV is now available.
        if (file.exists(sentinel_file)) unlink(sentinel_file)
        return(TRUE)
      } else if (exit_code == 2) {
        message("[IGV] ⏱️ Timeout waiting for IGV watcher")
        return(FALSE)
      } else if (exit_code == 3) {
        message("[IGV] ⚠️  IGV watcher neběží – snapshots přeskočeny, tabulka se načte bez obrázků")
        # Write sentinel so future app loads don't retry the 15s watcher wait.
        dir.create(igv_snapshot_dir, recursive = TRUE, showWarnings = FALSE)
        writeLines(format(Sys.time()), sentinel_file)
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
#' @param session_dir Session directory for output
process_arriba_pdf <- function(sample, arriba, session_dir) {
  if (is.null(arriba) || length(arriba) == 0) {
    message("[Arriba] ", sample, ": no arriba files provided, skipping")
    return(FALSE)
  }
  
  # Find TSV and PDF files
  tsv_files <- arriba[grepl("\\.tsv$", arriba)]
  pdf_files <- arriba[grepl("\\.pdf$", arriba)]
  
  if (length(tsv_files) == 0 || length(pdf_files) == 0) {
    message("[Arriba] ", sample, ": missing TSV or PDF (tsv=", length(tsv_files), ", pdf=", length(pdf_files), "), skipping")
    return(FALSE)
  }
  
  message("[Arriba] ", sample, ": starting PDF→SVG conversion (", pdf_files[1], ")")
  
  arriba_tsv <- fread(tsv_files[1])
  arriba_pdf <- pdf_files[1]
  
  output_dir <- file.path(session_dir, "arriba_reports", sample)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Get PDF page count
  pages <- NA_integer_
  pdfinfo_out <- tryCatch(
    system2("pdfinfo", args = shQuote(arriba_pdf), stdout = TRUE, wait = TRUE),
    error = function(e) character()
  )
  
  if (length(pdfinfo_out)) {
    p_line <- pdfinfo_out[grepl("^Pages:\\s+\\d+", pdfinfo_out)]
    if (length(p_line)) pages <- as.integer(sub("^Pages:\\s+", "", p_line[1]))
  }
  if (is.na(pages)) stop("Cannot determine PDF page count for ", sample)
  message("[Arriba] ", sample, ": PDF has ", pages, " pages → converting to SVG in ", output_dir)
  
  success_count <- length(list.files(output_dir, pattern = "\\.svg$", ignore.case = TRUE))

  if (success_count >= pages) {
    message("[Arriba] ", sample, ": SVG files already complete (", success_count, "/", pages, "), skipping conversion")
  } else {
    if (success_count > 0) {
      message("[Arriba] ", sample, ": incomplete previous run (", success_count, "/", pages, " SVGs), resuming...")
    } else {
      message("[PDF] Converting ", pages, " pages to SVG...")
    }
    for (i in seq_len(pages)) {
      output_svg <- file.path(output_dir, sprintf("%s_%03d.svg", sample, i))
      if (file.exists(output_svg)) next  # already done in a previous (partial) run
      exit_code <- system2("pdftocairo", args = c("-svg", "-f", as.character(i), "-l", as.character(i), shQuote(arriba_pdf), shQuote(output_svg)), wait = TRUE)
      if (exit_code != 0) {
        message("[Arriba] WARNING: pdftocairo returned exit code ", exit_code, " for page ", i, " of ", sample)
      } else if (!file.exists(output_svg)) {
        message("[Arriba] WARNING: page ", i, " — exit code 0 but SVG not created")
      } else {
        success_count <- success_count + 1
      }
    }
    message("[PDF] Created ", success_count, "/", pages, " SVG files")
  }
  
  message("[Arriba] ", sample, ": done (success_count=", success_count, ")")
  return(success_count > 0)
}



#' Create manifest file for fusion images
#' @param patient_id Patient identifier
#' @param fusion_file Path to fusion file
#' @param arriba Vector of arriba file paths (TSV and PDF)
#' @param session_dir Session directory where manifests will be stored
create_fusion_manifest <- function(patient_id, fusion_file, arriba, session_dir, has_bam = TRUE) {
  
  message("[MANIFEST] Creating manifest for patient: ", patient_id)
  message("[MANIFEST] Session dir: ", session_dir)
  message("[MANIFEST] has_bam: ", has_bam)
  
  # --- 1) Fusion file (required - provides base rows & IGV png paths) ---
  if (!file.exists(fusion_file)) {
    message("[MANIFEST] ❌ Fusion file does not exist: ", fusion_file)
    return(FALSE)
  }
  
  if (grepl("\\.xlsx?$", fusion_file)) {
    dt <- as.data.table(read.xlsx(fusion_file))
  } else {
    dt <- fread(fusion_file)
  }
  if ("chrom1" %in% names(dt)) setnames(dt, c("chrom1","chrom2"), c("chr1","chr2"))
  if ("pos1" %in% names(dt) && !is.numeric(dt$pos1)) dt[, pos1 := as.numeric(pos1)]
  if ("pos2" %in% names(dt) && !is.numeric(dt$pos2)) dt[, pos2 := as.numeric(pos2)]
  
  required_cols <- c("gene1", "gene2", "chr1", "pos1", "chr2", "pos2")
  dt <- dt[, ..required_cols]
  dt <- dt[, .(gene1 = paste(unique(gene1), collapse = ","),
               gene2 = paste(unique(gene2), collapse = ",")),
           by = .(chr1, pos1, chr2, pos2)]
  dt[, id := seq_len(.N)]
  dt[, path := if (has_bam) {
    sprintf("igv_snapshots/%s/%s_%03d.png", patient_id, patient_id, id)
  } else {
    NA_character_
  }]
  dt <- unique(dt)
  dt <- split_genes(dt)
  setnames(dt, "path", "png_path")

  # --- 2) Arriba TSV (optional - provides svg paths, NA if not available) ---
  tsv_dt <- NULL
  
  if (!is.null(arriba) && length(arriba) > 0) {
    tsv_files <- arriba[grepl("\\.tsv$", arriba)]
    if (length(tsv_files) > 0 && file.exists(tsv_files[1])) {
      tryCatch({
        arriba_dt <- fread(tsv_files[1])
        message("[MANIFEST] Arriba TSV loaded, rows: ", nrow(arriba_dt))
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
      }, error = function(e) {
        message("[MANIFEST] ⚠️ Failed to process arriba TSV: ", e$message)
        tsv_dt <<- NULL
      })
    } else {
      message("[MANIFEST] ⚠️ No valid arriba TSV found - svg_path will be NA")
    }
  } else {
    message("[MANIFEST] ℹ️ No arriba files - svg_path will be NA")
  }
  
  # --- 3) Merge: fusion data as base, join arriba if available ---
  if (!is.null(tsv_dt)) {
    manifest_dt <- merge(
      tsv_dt[, .(gene1, gene2, chr1, pos1, chr2, pos2, svg_path)],
      dt[, .(gene1, gene2, chr1, pos1, chr2, pos2, png_path)],
      all = TRUE, suffixes = c("_arriba", "_igv")
    )
  } else {
    manifest_dt <- dt[, .(gene1, gene2, chr1, pos1, chr2, pos2, png_path)]
    manifest_dt[, svg_path := NA_character_]
  }
  
  # --- 4) Write manifest ---
  manifest_dir <- file.path(session_dir, "manifests", "fusion")
  if (!dir.exists(manifest_dir)) dir.create(manifest_dir, recursive = TRUE)
  manifest_file <- file.path(manifest_dir, paste0(patient_id, ".tsv"))
  fwrite(manifest_dt, manifest_file, sep = "\t")
  
  message("[MANIFEST] ✓ Created manifest for ", patient_id, ": ", manifest_file)
  manifest_file
}


#' Main prerun function for fusion data
#' @param confirmed_paths Data frame with paths from upload_data
#' @param shared_data Reactive values object to update status
#' @export
prerun_fusion_data <- function(confirmed_paths, shared_data, prog_file = NULL) {
  message("Starting fusion data prerun...")
  session_dir <- shared_data$session_dir()
  
  # Get IGV genome selection (set in upload_data step1)
  genome_val <- shared_data$igv_genome()
  igv_genome <- if (!is.null(genome_val) && length(genome_val) > 0 && genome_val != "no_snapshot") {
    genome_val
  } else {
    "hg38"  # Default
  }
  message("[PRERUN] IGV genome: ", igv_genome)
  
  # Skip IGV snapshots if user selected "Don't create"
  skip_igv <- !is.null(genome_val) && length(genome_val) > 0 && genome_val == "no_snapshot"
  if (skip_igv) {
    message("[PRERUN] Skipping IGV snapshot generation per user selection")
  }

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
  base_dirs <- c(file.path(session_dir, "igv_snapshots"), file.path(session_dir, "arriba_reports"), file.path(session_dir, "manifests", "fusion"))
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
        
        # Process Arriba PDF first — fast (seconds)
        arriba_success <- process_arriba_pdf(sample, file_list$arriba, session_dir = session_dir)

        # Process IGV snapshots — slow (minutes), watcher handles parallel execution
        igv_success <- if (!skip_igv) {
          process_patient_igv(sample, file_list, session_dir, prog_file = NULL, genome_build = igv_genome)
        } else {
          message("[IGV] Skipping snapshots for patient ", sample)
          TRUE  # Skip but don't fail
        }

        # Create manifest
        fusion_file <- file_list$fusion
        if (length(fusion_file) > 0) {
          tumor_bams_s    <- if (!is.null(file_list$tumor))    file_list$tumor[grepl("\\.bam$",    file_list$tumor)]    else character(0)
          chimeric_bams_s <- if (!is.null(file_list$chimeric)) file_list$chimeric[grepl("\\.bam$", file_list$chimeric)] else character(0)
          will_have_pngs_s <- !skip_igv && length(tumor_bams_s) > 0 && length(chimeric_bams_s) > 0
          create_fusion_manifest(sample, fusion_file[1], file_list$arriba, session_dir = session_dir, has_bam = will_have_pngs_s)
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
#' Check which fusion patients need preprocessing (IGV snapshots + Arriba reports)
#' @param fusion_patients Character vector of patient IDs with fusion data
#' @param session_dir Path to session directory (e.g., "output_files/sessions/MOII_e117")
#' @return Character vector of patient IDs that need preprocessing
#' @export
fusion_patients_to_prerun <- function(fusion_patients, session_dir) {
  if (length(fusion_patients) == 0) return(character(0))
  
  manifest_dir <- file.path(session_dir, "manifests", "fusion")
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
      png_dir <- file.path(session_dir, "igv_snapshots", sample)
      actual_pngs <- if (dir.exists(png_dir)) {
        length(list.files(png_dir, pattern = "\\.png$", ignore.case = TRUE))
      } else {
        0
      }
      
      # Count actual SVG files  
      svg_dir <- file.path(session_dir, "arriba_reports", sample)
      actual_svgs <- if (dir.exists(svg_dir)) {
        length(list.files(svg_dir, pattern = "\\.svg$", ignore.case = TRUE))
      } else {
        0
      }
      
      # If IGV watcher was absent in a previous run, skip PNG check entirely.
      # The sentinel is cleared automatically if the watcher succeeds later.
      no_igv_sentinel <- file.path(session_dir, "igv_snapshots", sample, ".no_igv_watcher")
      if (file.exists(no_igv_sentinel)) {
        message("[PRERUN CHECK] ", sample, " – .no_igv_watcher sentinel found, skipping PNG check")
        need[i] <- (actual_svgs < expected_svgs)  # still run Arriba SVG if missing
        next
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


#' Clean up existing fusion outputs for a patient
#' Used when user explicitly chooses to start fresh
#' @param patient_id Patient identifier
#' @param session_dir Session directory
#' @export
cleanup_patient_fusion_outputs <- function(patient_id, session_dir) {
  message("[CLEANUP] Removing existing fusion outputs for ", patient_id)
  
  # Remove IGV snapshots
  igv_dir <- file.path(session_dir, "igv_snapshots", patient_id)
  if (dir.exists(igv_dir)) {
    unlink(igv_dir, recursive = TRUE, force = TRUE)
    message("[CLEANUP]   ✓ Removed IGV snapshots: ", igv_dir)
  }
  
  # Remove Arriba reports
  arriba_dir <- file.path(session_dir, "arriba_reports", patient_id)
  if (dir.exists(arriba_dir)) {
    unlink(arriba_dir, recursive = TRUE, force = TRUE)
    message("[CLEANUP]   ✓ Removed Arriba reports: ", arriba_dir)
  }
  
  # Remove manifest
  manifest_file <- file.path(session_dir, "manifests", "fusion", paste0(patient_id, ".tsv"))
  if (file.exists(manifest_file)) {
    unlink(manifest_file, force = TRUE)
    message("[CLEANUP]   ✓ Removed manifest: ", manifest_file)
  }
  
  message("[CLEANUP] Cleanup complete for ", patient_id)
}

#' Check fusion preprocessing status for a patient
#' Returns info about completed/missing outputs
#' @param patient_id Patient identifier
#' @param session_dir Session directory
#' @param fusion_file Path to fusion TSV file
#' @return List with: exists (logical), total_fusions, completed_snapshots, completed_arriba, has_manifest
#' @export
check_fusion_status <- function(patient_id, session_dir, fusion_file, arriba_files = NULL) {
  # Count expected IGV snapshots from fusion file (unique positions)
  total_fusions <- 0
  if (!is.null(fusion_file) && file.exists(fusion_file)) {
    tryCatch({
      if (grepl("\\.xlsx?$", fusion_file)) {
        dt <- as.data.table(read.xlsx(fusion_file))
      } else {
        dt <- fread(fusion_file)
      }
      if ("chrom1" %in% names(dt)) setnames(dt, c("chrom1", "chrom2"), c("chr1", "chr2"))
      if (all(c("chr1", "pos1", "chr2", "pos2") %in% names(dt))) {
        dt <- unique(dt[, .(chr1, pos1, chr2, pos2)])
        total_fusions <- nrow(dt)
      }
    }, error = function(e) {
      message("[STATUS] Error counting fusions: ", e$message)
    })
  }

  # Count expected Arriba SVGs from Arriba TSV (only Arriba-specific count)
  total_arriba <- 0L
  if (!is.null(arriba_files) && length(arriba_files) > 0) {
    tsv_files <- arriba_files[grepl("\\.tsv$", arriba_files)]
    if (length(tsv_files) > 0 && file.exists(tsv_files[1])) {
      tryCatch({
        arriba_dt <- fread(tsv_files[1])
        total_arriba <- nrow(arriba_dt)
      }, error = function(e) {
        message("[STATUS] Error counting arriba fusions: ", e$message)
      })
    }
  }
  
  # Check IGV snapshots
  igv_dir <- file.path(session_dir, "igv_snapshots", patient_id)
  completed_snapshots <- if (dir.exists(igv_dir)) {
    length(list.files(igv_dir, pattern = "\\.png$", ignore.case = TRUE))
  } else {
    0L
  }
  
  # Check Arriba reports
  arriba_dir <- file.path(session_dir, "arriba_reports", patient_id)
  completed_arriba <- if (dir.exists(arriba_dir)) {
    length(list.files(arriba_dir, pattern = "\\.svg$", ignore.case = TRUE))
  } else {
    0L
  }
  
  # Check manifest
  manifest_file <- file.path(session_dir, "manifests", "fusion", paste0(patient_id, ".tsv"))
  has_manifest <- file.exists(manifest_file)
  
  exists <- completed_snapshots > 0 || completed_arriba > 0 || has_manifest
  is_complete <- has_manifest && 
                 (total_fusions == 0 || completed_snapshots >= total_fusions)
  
  return(list(
    exists = exists,
    is_complete = is_complete,
    total_fusions = total_fusions,
    total_arriba = total_arriba,
    completed_snapshots = completed_snapshots,
    completed_arriba = completed_arriba,
    has_manifest = has_manifest
  ))
}

#' Process fusion data for a SINGLE patient (for parallel execution)
#' @param patient_id Patient identifier
#' @param file_list List of files for this patient (fusion, tumor, chimeric, arriba)
#' @param prog_file Path to progress file for cross-process communication
#' @param session_dir Session directory for output
#' @param igv_genome IGV genome build (e.g., "hg38", "hg19") - default "hg38"
#' @export
prerun_fusion_patient <- function(patient_id, file_list, prog_file = NULL, count_file = NULL, session_dir, igv_genome = "hg38") {
  # Handle "no_snapshot" — user chose not to create IGV snapshots
  skip_igv_patient <- identical(igv_genome, "no_snapshot")
  if (skip_igv_patient) {
    message("[PRERUN] IGV snapshots disabled (no_snapshot) for ", patient_id)
    igv_genome <- "hg38"  # unused but keep valid for any downstream code
  }
  message("[PRERUN] Starting for ", patient_id)
  message("[PRERUN] Session directory: ", session_dir)
  
  # Count total fusions here (in background worker) — avoids blocking main R thread
  total_fusions <- tryCatch({
    fusion_file <- file_list$fusion
    if (!is.null(fusion_file) && length(fusion_file) > 0 && file.exists(fusion_file[1])) {
      if (grepl("\\.xlsx?$", fusion_file[1])) {
        nrow(openxlsx::read.xlsx(fusion_file[1]))
      } else {
        nrow(data.table::fread(fusion_file[1]))
      }
    } else 0L
  }, error = function(e) 0L)
  message("[PRERUN] ", patient_id, " total fusions: ", total_fusions)

  # Write count immediately so the main session observer can display it within
  # the next 500 ms tick — long before arriba/IGV finishes.
  if (!is.null(count_file) && nzchar(count_file)) {
    writeLines(as.character(total_fusions), count_file)
  }
  
  # Initialize progress
  if (!is.null(prog_file) && nzchar(prog_file)) writeLines("0", prog_file)
  
  errors <- character()
  warnings <- character()
  messages <- character()
  
  tryCatch({
    # Create base directories if needed (inside session folder)
    # NO automatic cleanup - resume capability preserved!
    base_dirs <- c(
      file.path(session_dir, "igv_snapshots"),
      file.path(session_dir, "arriba_reports"),
      file.path(session_dir, "manifests", "fusion")
    )
    for (dir in base_dirs) {
      if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    }
    
    # Step 1: Process Arriba PDF (5–15 %) — fast, seconds
    # UI shows: "Processing Arriba images... X%"
    if (!is.null(prog_file)) writeLines("5", prog_file)
    arriba_success <- FALSE
    if (!is.null(file_list$arriba) && length(file_list$arriba) > 0) {
      arriba_success <- process_arriba_pdf(patient_id, file_list$arriba, session_dir)
    }
    if (!is.null(prog_file)) writeLines("20", prog_file)

    # Step 2: Process IGV snapshots (15–90 %) — slow, minutes
    # UI shows: "Processing IGV snapshots... X%"
    igv_success <- if (!skip_igv_patient) {
      process_patient_igv(patient_id, file_list, session_dir, prog_file = prog_file, genome_build = igv_genome)
    } else {
      message("[IGV] Skipping snapshots for ", patient_id, " (no_snapshot selected)")
      if (!is.null(prog_file)) writeLines("90", prog_file)
      TRUE
    }
    if (!is.null(prog_file)) writeLines("90", prog_file)

    # Step 3: Create fusion manifest (90–100 %)
    # UI shows: "Creating fusion manifest... X%"
    fusion_file <- file_list$fusion
    if (length(fusion_file) > 0 && !is.null(file_list$arriba)) {
      tumor_bams    <- if (!is.null(file_list$tumor))    file_list$tumor[grepl("\\.bam$",    file_list$tumor)]    else character(0)
      chimeric_bams <- if (!is.null(file_list$chimeric)) file_list$chimeric[grepl("\\.bam$", file_list$chimeric)] else character(0)
      will_have_pngs <- !skip_igv_patient && length(tumor_bams) > 0 && length(chimeric_bams) > 0
      create_fusion_manifest(patient_id, fusion_file[1], file_list$arriba, session_dir, has_bam = will_have_pngs)
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
    patient_id = patient_id,
    total_fusions = total_fusions
  )
}
