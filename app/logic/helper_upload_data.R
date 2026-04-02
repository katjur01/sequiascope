# app/logic/helper_upload_data.R
box::use(
  shiny[isTruthy],
  htmltools[tags,HTML,div,span,h2,h4,h5,br],
  stringr[str_detect,regex,fixed,str_replace,str_remove],
  reactable[reactable,colDef],
  data.table[fread,rbindlist],
  stats[setNames],
  readxl[read_xls,read_xlsx],
)

box::use(
  app/logic/load_data[read_file_header,read_by_extension,get_required_columns,MAF_COLUMN_CANDIDATES]
)

get_status_icon <- function(color, simple = FALSE, reason = "unknown") {
  
  # Definition of all possible tooltip messages by reason
  tooltip_messages <- list(
    # Red states
    "required_missing" = list(
      simple = "Required file missing",
      detailed = "Missing files"
    ),
    "required_missing_exp" = list(
      simple = "One or more selected tissue files are missing",
      detailed = "Missing files"
    ),
    "required_one_file" = list(
      simple   = "Provide exactly one expression file",
      detailed = "Multiple expression files found while no tissue was specified"
    ),

    # Green states
    "complete_pair_bam_bai" = list(
      simple   = "Paired files OK",
      detailed = "BAM and BAI files are complete"
    ),
    "complete_pair_pdf_tsv" = list(
      simple   = "Paired files OK",
      detailed = "Arriba PDF and TSV are complete"
    ),
    "single_file_ok" = list(
      simple = "File found",
      detailed = "Required file is available"
    ),
    "files_ok"  = list(
      simple = "Files found",
      detailed = "Required files are available"
    ),
    
    # Orange states
    "missing_pair_bam_bai" = list(
      simple   = "Missing BAM or BAI",
      detailed = "BAM/BAI pair is incomplete"
    ),
    "missing_pair_pdf_tsv" = list(
      simple   = "Missing PDF or TSV",
      detailed = "Arriba PDF/TSV pair is incomplete"
    ),
    "multiple_files" = list(
      simple = "Multiple files found. Only one file expected",
      detailed = "Too many files"
    ),
    "multiple_files_exp" = list(
      simple = "More files than selected tissues", 
      detailed = "Too many files"
    ),
    
    # GOI specific
    "goi_configured_missing" = list(
      simple = "GOI file path in config but file not found",
      detailed = "GOI file missing"
    ),
    
    # Reference files
    "kegg_missing_required" = list(
      simple = "Pathways file missing",
      detailed = "Pathways file missing"
    ),
    "kegg_missing_optional" = list(
      simple = "Pathways file missing",
      detailed = "Pathways file missing"
    ),
    "report_template_missing" = list(
      simple = "Report template missing",
      detailed = "Report template missing"
    ),
    
    # Pattern-based search
    "pattern_no_match" = list(
      simple = "No files found matching pattern",
      detailed = "Pattern matched no files"
    ),
    
    # Gray states
    "optional_missing" = list(
      simple = "Optional files not available",
      detailed = "Optional files not available"
    ),
    
    # Fallback
    "unknown" = list(
      simple = "Unknown status",
      detailed = "Unknown status"
    )
  )

  if (identical(reason, ""))                reason <- "files_ok"
  if (identical(reason, "complete_pair"))   reason <- "complete_pair_bam_bai"
  if (identical(reason, "missing_pair"))    reason <- "missing_pair_bam_bai"
  
  
  
  # Get message by reason
  message_type <- if (simple) "simple" else "detailed"
  tooltip_text <- if (!is.null(tooltip_messages[[reason]][[message_type]])) {
    tooltip_messages[[reason]][[message_type]]
  } else {
    tooltip_messages[["unknown"]][[message_type]]
  }
  
  if (simple) {
    simple_icons <- list(
      "red" = paste0('<i class="fas fa-times status-icon" style="color: #dc3545; font-size: 14px;" data-toggle="tooltip" data-placement="bottom" data-html="true" title="', tooltip_text, '"></i>'),
      "orange" = paste0('<i class="fas fa-exclamation status-icon" style="color: #fd7e14; font-size: 14px;" data-toggle="tooltip" data-placement="bottom" data-html="true" title="', tooltip_text, '"></i>'),
      "green" = paste0('<i class="fas fa-check status-icon" style="color: #28a745; font-size: 14px;" data-toggle="tooltip" data-placement="bottom" data-html="true" title="', tooltip_text, '"></i>'),
      "gray" = paste0('<i class="fas fa-minus status-icon" style="color: #6c757d; font-size: 14px;" data-toggle="tooltip" data-placement="bottom" data-html="true" title="', tooltip_text, '"></i>')
    )
    
    return(if (!is.null(simple_icons[[color]])) simple_icons[[color]] else simple_icons[["red"]])
    
  } else {
    icons <- list(
      "red" = paste0('<div class="status-icon" style="width: 20px; height: 20px; background-color: #dc3545; border-radius: 50%; display: flex; align-items: center; justify-content: center; margin: 0 auto;" data-toggle="tooltip" data-placement="bottom" data-html="true" title="', tooltip_text, '"><i class="fas fa-times" style="color: white; font-size: 10px;"></i></div>'),
      "orange" = paste0('<div class="status-icon" style="width: 20px; height: 20px; background-color: #fd7e14; border-radius: 50%; display: flex; align-items: center; justify-content: center; margin: 0 auto;" data-toggle="tooltip" data-placement="bottom" data-html="true" title="', tooltip_text, '"><i class="fas fa-exclamation" style="color: white; font-size: 10px;"></i></div>'),
      "green" = paste0('<div class="status-icon" style="width: 20px; height: 20px; background-color: #28a745; border-radius: 50%; display: flex; align-items: center; justify-content: center; margin: 0 auto;" data-toggle="tooltip" data-placement="bottom" data-html="true" title="', tooltip_text, '"><i class="fas fa-check" style="color: white; font-size: 10px;"></i></div>'),
      "gray" = paste0('<div class="status-icon" style="width: 20px; height: 20px; background-color: #6c757d; border-radius: 50%; display: flex; align-items: center; justify-content: center; margin: 0 auto;" data-toggle="tooltip" data-placement="bottom" data-html="true" title="', tooltip_text, '"><i class="fas fa-minus" style="color: white; font-size: 10px;"></i></div>')
    )
    
    return(if (!is.null(icons[[color]])) icons[[color]] else icons[["red"]])
  }
}

evaluate_file_status <- function(files, patient, file_type, dataset_type, patterns = NULL) {
  file_configs <- list(
    variant_somatic = list(extensions = "\\.(vcf|tsv)$", keywords = "somatic", required = TRUE),
    # BAM files: No dataset-type keyword required - rely on user-provided patterns (FFPE, krev, etc.)
    # This allows finding BAMs in primary_analysis folders that don't contain "somatic"/"germline"/"fusion"
    tumor_somatic   = list(extensions = "\\.(bam|bai)$", keywords = NULL, exclude = "qc_|feature_count|flagstat|idxstats|species_|transcriptome", required = FALSE, check_pair = TRUE, pair = "bam_bai"),
    normal_somatic  = list(extensions = "\\.(bam|bai)$", keywords = NULL, exclude = "qc_|feature_count|flagstat|idxstats|species_|transcriptome", required = FALSE, check_pair = TRUE, pair = "bam_bai"),
    TMB_somatic     = list(extensions = "mutation_loads\\.(tsv|xlsx|txt)$", keywords = "somatic", exclude = "report", required = FALSE),
    
    variant_germline = list(extensions = "\\.(vcf|tsv)$", keywords = "germline", required = TRUE),
    normal_germline  = list(extensions = "\\.(bam|bai)$", keywords = NULL, exclude = "qc_|feature_count|flagstat|idxstats|species_|transcriptome", required = FALSE, check_pair = TRUE, pair = "bam_bai"),
    
    # Fusion table: Exclude QC files and feature_count files which also have .tsv extension
    fusion_fusion    = list(extensions = "\\.(tsv|xlsx)$", keywords = "fusion|fuze", exclude = "arriba|STAR|qc_|feature_count|flagstat|idxstats|species_", required = TRUE),
    tumor_fusion     = list(extensions = "\\.(bam|bai)$", keywords = NULL, exclude = "Chimeric|transcriptome|qc_|feature_count|flagstat|idxstats|species_", required = FALSE, check_pair = TRUE, pair = "bam_bai"),
    chimeric_fusion  = list(extensions = "\\.(bam|bai)$", keywords = NULL, exclude = "transcriptome|qc_|feature_count|flagstat|idxstats|species_", required = FALSE, check_pair = TRUE, pair = "bam_bai"),
    arriba_fusion    = list(extensions = "\\.(pdf|tsv)$", keywords = "arriba", exclude = "discarded|STAR", required = FALSE, check_pair = TRUE, pair = "pdf_tsv"),
    
    expression_expression = list(extensions = "\\.(tsv|xlsx)$", keywords = "expression|RNAseq", exclude = "genes_of_interest", required = TRUE),
    goi_expression        = list(extensions = "\\.(tsv|xlsx)$", keywords = NULL, exclude = NULL, required = FALSE)  # Exact path from config, no pattern matching
  )
  
  config_key <- paste(file_type, dataset_type, sep = "_")
  config <- file_configs[[config_key]]
  if (is.null(config)) return("red")
  
  pattern_list <- character(0)
  if (!is.null(patterns)) {
    if (length(patterns) == 1) {
      pattern_list <- unique(trimws(unlist(strsplit(patterns, ","))))
    } else {
      pattern_list <- unique(trimws(as.character(patterns)))
    }
  }
  
  # Check if "none" is used as a marker for "search without additional pattern"
  # "none" means: search for BAM files using only keyword (e.g., "somatic", "fusion") + patient ID, no additional pattern
  search_without_pattern <- FALSE
  if (length(pattern_list) > 0 && any(tolower(pattern_list) == "none")) {
    search_without_pattern <- TRUE
    pattern_list <- pattern_list[tolower(pattern_list) != "none"]  # Remove "none" from pattern list
    
    # DEBUG: Confirm "none" detection
    message("[NONE DETECTED] config_key: ", config_key, ", patient: ", patient, ", search_without_pattern: TRUE")
  }
  
  # If no patterns are provided for BAM files (fusion tumor/chimeric or somatic tumor/normal or germline normal), don't search at all
  # UNLESS "none" was specified (search_without_pattern = TRUE)
  # Note: arriba_fusion is NOT included - it always searches by keyword "arriba" regardless of user patterns
  if ((config_key %in% c("tumor_fusion", "chimeric_fusion", "tumor_somatic", "normal_somatic", "normal_germline")) && 
      !search_without_pattern &&
      (is.null(patterns) || length(pattern_list) == 0 || all(!nzchar(pattern_list)))) {
    return(list(
      status = "gray",
      files = character(0),
      reason = "optional_missing",
      tissues = character(0),
      all_mapping = NULL,
      missing_tissues = NULL
    ))
  }
  
  base_keywords <- config$keywords
  
  # Build keyword lists — config keywords use regex(), user patterns use fixed().
  # Keeping them separate prevents config OR-syntax like "fusion|fuze" from being
  # treated as a literal string (which broke fusion search without user pattern).
  config_kws <- character(0)  # matched with regex()
  user_kws   <- character(0)  # matched with fixed() — literal strings

  if (config_key == "expression_expression") {
    config_kws    <- config$keywords
    keyword_regex <- config_kws  # kept for debug logging
  } else if (config_key %in% c("variant_somatic", "variant_germline", "fusion_fusion") && length(pattern_list) > 0 && any(nzchar(pattern_list))) {
    # User pattern REPLACES config keyword for these types
    user_kws      <- pattern_list[nzchar(pattern_list)]
    keyword_regex <- user_kws
  } else {
    if (search_without_pattern) {
      config_kws <- if (!is.null(config$keywords)) c(config$keywords) else character(0)
    } else {
      config_kws <- if (!is.null(config$keywords)) c(config$keywords) else character(0)
      user_kws   <- pattern_list[nzchar(pattern_list)]
    }
    config_kws    <- config_kws[nzchar(config_kws)]
    user_kws      <- user_kws[nzchar(user_kws)]
    keyword_regex <- c(config_kws, user_kws)  # for debug logging only
  }
  
  # DEBUG: Log keyword construction for tumor_fusion
  if (config_key %in% c("tumor_fusion", "chimeric_fusion", "tumor_somatic", "normal_somatic", "normal_germline")) {
    message("[FILE_EVAL] Evaluating ", config_key, " for patient: ", patient)
    message("[FILE_EVAL]   - pattern_list after parsing: ", paste(pattern_list, collapse=", "))
    message("[FILE_EVAL]   - search_without_pattern: ", search_without_pattern)
    message("[FILE_EVAL]   - all_keywords combined: ", paste(keyword_regex, collapse=", "))
    message("[FILE_EVAL]   - final keyword_regex (AND logic): ", paste(keyword_regex, collapse=" AND "))
  }
  
  relevant_files <- if (config_key == "goi_expression") {
    # GOI file comes as exact path from config - no filtering needed
    files
  } else if (config_key == "TMB_somatic") {
    files[str_detect(files, regex(config$keywords, ignore_case = TRUE)) & !str_detect(files, config$exclude)]
  } else {
    files[str_detect(files, patient)]
  }
  
  # DEBUG: Log file filtering for tumor_fusion
  if (config_key %in% c("tumor_fusion", "chimeric_fusion", "tumor_somatic", "normal_somatic", "normal_germline")) {
    message("[FILE_EVAL]   - relevant_files (matching patient): ", length(relevant_files))
    # File list omitted to reduce log verbosity
  }
  
  # GOI file comes as exact path - no keyword matching needed
  if (config_key == "goi_expression") {
    # Special handling for GOI - check if file actually exists
    if (length(relevant_files) > 0) {
      existing_files <- relevant_files[file.exists(relevant_files)]
      matched <- existing_files[str_detect(existing_files, config$extensions)]
      
      # If path was in config but file doesn't exist -> we need to signal this
      # Store the original path for later status determination
      if (length(existing_files) == 0 && length(relevant_files) > 0) {
        # Path was provided but file doesn't exist
        attr(matched, "goi_path_missing") <- TRUE
      }
    } else {
      matched <- character(0)
    }
  } else if (config_key == "expression_expression") {
    # Expression uses OR logic for keyword_regex (e.g. "expression|RNAseq").
    # This matches on the full path, so files inside an expression/ or RNAseq/
    # folder are found even if the filename itself has no tissue suffix.
    candidates <- relevant_files[
      str_detect(relevant_files, config$extensions) &
        str_detect(relevant_files, regex(keyword_regex, ignore_case = TRUE))]
    if (search_without_pattern) {
      # tissue = "none": we want ONLY {patient_id}.tsv — no tissue suffix in filename.
      # e.g. DZ1601.tsv ✓   DZ1601_spleen.tsv ✗   DZ1601_testis.tsv ✗
      strict_basename <- paste0("^", patient, "\\.[^.]+$")
      matched <- candidates[str_detect(basename(candidates), regex(strict_basename, ignore_case = TRUE))]
    } else {
      matched <- candidates
    }
  } else {
    # For other types: AND logic.
    # config_kws use regex() (may contain OR syntax like "fusion|fuze").
    # user_kws  use fixed() (literal strings, no regex interpretation).
    matched <- relevant_files[str_detect(relevant_files, config$extensions)]
    for (kw in config_kws) {
      matched <- matched[str_detect(matched, regex(kw, ignore_case = TRUE))]
    }
    for (kw in user_kws) {
      matched <- matched[str_detect(matched, fixed(kw, ignore_case = TRUE))]
    }
  }

  # For variant files: patient ID must appear in the BASENAME, not just in the folder path.
  # This removes intermediate pipeline files (Ensemble.sINDEL.tsv, VarDict.vcf, etc.) whose
  # patient ID is only present in a parent folder name like somatic_varcalls/AJ0910_FFPE/.
  if (config_key %in% c("variant_somatic", "variant_germline", "fusion_fusion")) {
    matched <- matched[str_detect(basename(matched), regex(patient, ignore_case = TRUE))]
  }

  # Cross-dataset contamination guard: a file whose path contains the OTHER dataset's
  # keyword almost certainly comes from the wrong project folder (e.g. 130_germline/
  # bleeding into a somatic search when variant_pattern replaces the config keyword).
  if (config_key == "variant_somatic") {
    matched <- matched[!str_detect(matched, regex("germline", ignore_case = TRUE))]
  } else if (config_key == "variant_germline") {
    matched <- matched[!str_detect(matched, regex("somatic", ignore_case = TRUE))]
  }

  # When "none" is specified (search_without_pattern), apply stricter filtering
  # to ensure we only match files with exact patient ID, not patient ID + additional text
  # e.g., "DZ1601.bam" ✓  but "DZ1601FFPE.bam" ✗
  if (search_without_pattern && (config_key %in% c("tumor_fusion", "chimeric_fusion", "tumor_somatic", "normal_somatic", "normal_germline"))) {
    # Build strict pattern that matches ONLY files where patient ID is followed by:
    #   1. Extension directly: DZ1601.bam, DZ1601.bam.bai ✓
    #   2. Separator + numbers + extension: DZ1601_001.bam, DZ1601_001.bam.bai ✓
    # BUT NOT files where patient ID + letters: DZ1601FFPE.bam ✗
    
    # Pattern: start of string (^), patient ID, optionally separator+digits, then .bam.bai, .bam, or .bai
    # Order matters: longer extensions first (bam.bai before bam)
    # Apply to basename only
    strict_pattern <- paste0("^", patient, "(?:[\\._-][0-9]+)?\\.(bam\\.bai|bam|bai)$")
    
    # Filter based on basename matching the strict pattern
    matched_basenames <- basename(matched)
    matches_strict <- str_detect(matched_basenames, regex(strict_pattern, ignore_case = TRUE))
    
    # DEBUG: Log strict pattern filtering BEFORE applying filter
    if (config_key %in% c("tumor_fusion", "chimeric_fusion", "tumor_somatic", "normal_somatic", "normal_germline")) {
      message("[FILE_EVAL]   - Applied strict pattern (none specified): ", strict_pattern)
      message("[FILE_EVAL]   - Files before strict filter: ", paste(basename(matched), collapse=", "))
      message("[FILE_EVAL]   - Matches strict pattern: ", paste(which(matches_strict), collapse=", "))
    }
    
    matched <- matched[matches_strict]
    
    # DEBUG: Log after filter
    if (config_key %in% c("tumor_fusion", "chimeric_fusion", "tumor_somatic", "normal_somatic", "normal_germline")) {
      message("[FILE_EVAL]   - Files after strict filter: ", length(matched))
      if (length(matched) > 0) {
        message("[FILE_EVAL]     Strict matched: ", paste(basename(matched), collapse=", "))
      }
    }
  }
  
  # DEBUG: Log matching results for BAM file types
  if (config_key %in% c("tumor_fusion", "chimeric_fusion", "tumor_somatic", "normal_somatic", "normal_germline")) {
    message("[FILE_EVAL]   - matched files (after extension + keyword filter): ", length(matched))
    if (length(matched) > 0) {
      message("[FILE_EVAL]     Matched: ", paste(basename(matched), collapse=", "))
    }
  }
  
  if (!is.null(config$exclude)) {
    matched <- matched[!str_detect(matched, regex(config$exclude, ignore_case = TRUE))]
  }
  
  # DEBUG: Log after exclude filter for BAM file types
  if (config_key %in% c("tumor_fusion", "chimeric_fusion", "tumor_somatic", "normal_somatic", "normal_germline")) {
    message("[FILE_EVAL]   - final matched files (after exclude filter): ", length(matched))
    if (length(matched) > 0) {
      message("[FILE_EVAL]     Final: ", paste(basename(matched), collapse=", "))
    } else {
      message("[FILE_EVAL]     ⚠️  NO FILES MATCHED!")
      message("[FILE_EVAL]     This usually means:")
      message("[FILE_EVAL]       1. Pattern is missing or incorrect")
      message("[FILE_EVAL]       2. Files don't contain ALL required keywords: ", paste(keyword_regex, collapse=" AND "))
      message("[FILE_EVAL]     👉 Check Step 1: Pattern must match your filename!")
    }
  }
  
  tissues_matched <- character(0)
  
  # When user provides a pattern for BAM files (or uses "-" for no pattern), it becomes "required"
  is_required_by_pattern <- (config_key %in% c("tumor_fusion", "chimeric_fusion", "tumor_somatic", "normal_somatic", "normal_germline")) &&
                             (search_without_pattern || (length(pattern_list) > 0 && any(nzchar(pattern_list))))
  is_required <- config$required || is_required_by_pattern
  
  # Keep copy of matched files before clearing (for debugging/UI display)
  matched_before_clear <- matched
  
  # BAM/BAI pairing
  if (!is.null(config$check_pair) && config$check_pair) {
    if (length(matched) == 0) {
      # Optional files with pattern: orange (not red) - user can proceed without this feature
      status <- if (is_required) "orange" else "gray"
      # Use pattern_no_match when user specified pattern but no files found
      reason <- if (is_required_by_pattern) "pattern_no_match" else if (is_required) "required_missing" else "optional_missing"
    } else {
      pair_type <- if (!is.null(config$pair)) config$pair else "bam_bai"
      pair_status <- check_pair(matched, patient, pair = pair_type)
      
      if (pair_status == "multiple") {
        status <- "orange"
        reason <- "multiple_files"
        matched <- character(0)  # Clear files - app should not use them
      } else if (pair_status == "incomplete") {
        status <- "orange"
        reason <- if (pair_type == "pdf_tsv") "missing_pair_pdf_tsv" else "missing_pair_bam_bai"
        matched <- character(0)  # Clear files - incomplete pairs are unusable
      } else if (pair_status == "complete") {
        status <- "green"
        reason <- if (pair_type == "pdf_tsv") "complete_pair_pdf_tsv" else "complete_pair_bam_bai"
      } else {
        status <- if (is_required) "orange" else "gray"
        reason <- if (is_required) "required_missing" else "optional_missing"
        matched <- character(0)  # No files found
      }
    }
  } else {
    # --- Non-BAM files ---
    if (config_key == "expression_expression") {
      # If no tissues specified (pattern_list empty)
      if (length(pattern_list) == 0 || all(!nzchar(pattern_list))) {
        if (length(matched) == 0) {
          status <- if (config$required) "red" else "gray"
          reason <- if (config$required) "required_missing" else "optional_missing"
          tissues_matched <- character(0)
        } else if (length(matched) == 1) {
          status <- "green"
          reason <- "single_file_ok"
          tissues_matched <- "none"
        } else {
          status <- "red"
          reason <- "required_one_file"
          tissues_matched <- rep("none", length(matched))
          # No need to clear matched - red status blocks user from proceeding anyway
        }
      } else {
        # Tissues specified -> always use mapping
        filtered <- create_tissue_file_mapping(
          expression_files = matched,
          tissues_str      = pattern_list
        )
        matched <- as.character(filtered$mapping)  # only detected files (without NA)
        status  <- switch(filtered$status,
          "green" = "green",
          "mixed" = "orange",
          "red"   = "red",
          "red")
        reason  <- if (status == "green") "files_ok" else "required_missing_exp"
        tissues_matched <- names(filtered$mapping)
        # No need to clear matched when red - red status blocks user from proceeding anyway
        
        # Also add full mapping to output (incl. NA for missing)
        attr(matched, "all_mapping") <- filtered$all_mapping
        attr(matched, "missing_tissues") <- filtered$missing_tissues
      }
      
    } else {
      # --- Other non-BAM types ---
      # Special handling for GOI: if path was in config but file doesn't exist -> orange
      if (config_key == "goi_expression" && !is.null(attr(matched, "goi_path_missing")) && attr(matched, "goi_path_missing")) {
        status <- "orange"
        reason <- "goi_configured_missing"  # Path configured but file not found
        matched <- character(0)  # Clear - file doesn't exist, don't pass to app
      } else if (length(matched) == 0) {
        status <- if (config$required) "red" else "gray"
        reason <- if (config$required) "required_missing" else "optional_missing"
      } else if (length(matched) == 1) {
        status <- "green"
        reason <- "single_file_ok"
      } else if (config_key != "expression_expression" && length(matched) > 1) {
        # Multiple files: RED for required files, ORANGE for optional
        status <- if (config$required) "red" else "orange"
        reason <- "multiple_files"
        matched <- character(0)  # Multiple files found - clear them (ambiguous which to use)
      } else {
        status <- "gray"
        reason <- "unknown"
      }
    }
  }
  
  return(list(
    status = status,
    files = matched,
    reason = reason,
    tissues = if (config_key == "expression_expression") tissues_matched else rep("none", length(matched)),
    all_mapping = if (config_key == "expression_expression") attr(matched, "all_mapping") else NULL,
    missing_tissues = if (config_key == "expression_expression") attr(matched, "missing_tissues") else NULL,
    found_files = matched_before_clear  # Keep original matched files for UI display (even when cleared)
  ))
}

#' @export
create_dataset_data <- function(dataset_type, files, goi_files, TMB_files, patients, path, tumor_pattern, normal_pattern, variant_pattern = NULL, tissues) {
  
  if (length(patients) == 0) return(NULL)
  
  dataset_columns <- get_dataset_columns(dataset_type)
  column_names <- names(dataset_columns)
  
  patient_results <- setNames(
    lapply(patients, function(patient) {
    
    if (dataset_type == "somatic") {
      list(
        variant = evaluate_file_status(files, patient, "variant", "somatic", if (!is.null(variant_pattern)) variant_pattern$somatic else NULL),
        tumor = evaluate_file_status(files, patient, "tumor", "somatic", tumor_pattern$somatic),
        normal = evaluate_file_status(files, patient, "normal", "somatic", normal_pattern$somatic),
        TMB = evaluate_file_status(TMB_files, patient, "TMB", "somatic")
      )
    } else if (dataset_type == "germline") {
      list(
        variant = evaluate_file_status(files, patient, "variant", "germline", if (!is.null(variant_pattern)) variant_pattern$germline else NULL),
        normal = evaluate_file_status(files, patient, "normal", "germline", normal_pattern$germline)
      )
    } else if (dataset_type == "fusion") {
      list(
        fusion = evaluate_file_status(files, patient, "fusion", "fusion", if (!is.null(variant_pattern)) variant_pattern$fusion else NULL),
        tumor = evaluate_file_status(files, patient, "tumor", "fusion", tumor_pattern$fusion),
        chimeric = evaluate_file_status(files, patient, "chimeric", "fusion", tumor_pattern$chimeric),
        arriba = evaluate_file_status(files, patient, "arriba", "fusion", tumor_pattern$arriba)
      )
    } else if (dataset_type == "expression") {
      expression_result = evaluate_file_status(files, patient, "expression", "expression", patterns = tissues)
      goi_result = evaluate_file_status(goi_files, patient, "goi", "expression")
      
      # full mapping (incl. NA for missing)
      if (!is.null(expression_result$all_mapping) && length(expression_result$all_mapping) > 0) {
        tissue_mapping_full <- expression_result$all_mapping
      } else {
        # no tissues specified -> "none" or empty
        if (length(expression_result$files) > 0 && length(expression_result$tissues) > 0) {
          tissue_mapping_full <- setNames(expression_result$files, expression_result$tissues)
        } else if (length(expression_result$files) > 0) {
          tissue_mapping_full <- setNames(expression_result$files, rep("none", length(expression_result$files)))
        } else {
          tissue_mapping_full <- character(0)
        }
      }
      
      # only detected (without NA)
      if (length(tissue_mapping_full) > 0) {
        tissue_mapping_detected <- tissue_mapping_full[!is.na(tissue_mapping_full)]
      } else {
        tissue_mapping_detected <- character(0)
      }
      
      list(
        expression = list(
          status = expression_result$status,
          files = expression_result$files,
          reason = expression_result$reason,
          tissue_mapping = tissue_mapping_detected,  # kept for existing logic
          tissue_mapping_full = tissue_mapping_full  # new: full list of tissues
        ),
        goi = goi_result
      )
    }
  }),
  patients
  )
  
  patient_data <- data.frame(
    patient = patients,
    stringsAsFactors = FALSE
  )
  
  if (dataset_type == "somatic") {
    patient_data$variant <- sapply(patient_results, function(x) get_status_icon(x$variant$status, simple = TRUE, reason = x$variant$reason))
    patient_data$tumor <- sapply(patient_results, function(x) get_status_icon(x$tumor$status, simple = TRUE, reason = x$tumor$reason))
    patient_data$normal <- sapply(patient_results, function(x) get_status_icon(x$normal$status, simple = TRUE, reason = x$normal$reason))

    patient_data$TMB <- sapply(patient_results, function(x) get_status_icon(x$TMB$status, simple = TRUE, reason = x$TMB$reason))
    patient_data$icon <- sapply(patient_results, function(x) {
      all_statuses <- c(x$variant$status, x$tumor$status, x$normal$status, x$TMB$status)
      all_reasons <- c(x$variant$reason, x$tumor$reason, x$normal$reason, x$TMB$reason)
      
      has_multiple_files <- x$variant$reason == "multiple_files"
      
      if (has_multiple_files) {
        get_status_icon("red", simple = FALSE, reason = "multiple_files")
      } else if ("red" %in% all_statuses) {
        red_reason <- all_reasons[all_statuses == "red"][1]
        get_status_icon("red", simple = FALSE, reason = red_reason)
      } else if ("orange" %in% all_statuses) {
        orange_reason <- all_reasons[all_statuses == "orange"][1]
        get_status_icon("orange", simple = FALSE, reason = orange_reason)
      } else if (all(all_statuses == "gray")) {
        get_status_icon("gray", simple = FALSE, reason = "optional_missing")
      } else {
        get_status_icon("green", simple = FALSE, reason = "files_ok")
      }
    })
    
    patient_data$files <- sapply(patient_results, function(x) {
      # For debugging: show found_files when multiple_files or other issues occur
      # Collect all found files (use found_files when available, fallback to files)
      all_files <- unique(c(
        if (!is.null(x$variant$found_files) && length(x$variant$found_files) > 0) x$variant$found_files else x$variant$files,
        if (!is.null(x$tumor$found_files) && length(x$tumor$found_files) > 0) x$tumor$found_files else x$tumor$files,
        if (!is.null(x$normal$found_files) && length(x$normal$found_files) > 0) x$normal$found_files else x$normal$files,
        if (!is.null(x$TMB$found_files) && length(x$TMB$found_files) > 0) x$TMB$found_files else x$TMB$files
      ))
      if (length(all_files) == 0) "" else paste(str_replace(all_files,path,""), collapse = ",\n")
    })
  } else if (dataset_type == "germline") {
    patient_data$variant <- sapply(patient_results, function(x) get_status_icon(x$variant$status, simple = TRUE, reason = x$variant$reason))
    patient_data$normal <- sapply(patient_results, function(x) get_status_icon(x$normal$status, simple = TRUE, reason = x$normal$reason))
    
    patient_data$icon <- sapply(patient_results, function(x) {
      all_statuses <- c(x$variant$status, x$normal$status)
      all_reasons <- c(x$variant$reason, x$normal$reason)
      
      has_multiple_files <- x$variant$reason == "multiple_files"
      
      if (has_multiple_files) {
        get_status_icon("red", simple = FALSE, reason = "multiple_files")
      } else if ("red" %in% all_statuses) {
        red_reason <- all_reasons[all_statuses == "red"][1]
        get_status_icon("red", simple = FALSE, reason = red_reason)
      } else if ("orange" %in% all_statuses) {
        orange_reason <- all_reasons[all_statuses == "orange"][1]
        get_status_icon("orange", simple = FALSE, reason = orange_reason)
      } else if (all(all_statuses == "gray")) {
        get_status_icon("gray", simple = FALSE, reason = "optional_missing")
      } else {
        get_status_icon("green", simple = FALSE, reason = "files_ok")
      }
    })
    
    patient_data$files <- sapply(patient_results, function(x) {
      # For debugging: show found_files when multiple_files or other issues occur
      all_files <- unique(c(
        if (!is.null(x$variant$found_files) && length(x$variant$found_files) > 0) x$variant$found_files else x$variant$files,
        if (!is.null(x$normal$found_files) && length(x$normal$found_files) > 0) x$normal$found_files else x$normal$files
      ))
      if (length(all_files) == 0) "" else paste(str_replace(all_files,path,""), collapse = ",\n")
    })
    
  } 
  else if (dataset_type == "fusion") {
    patient_data$fusion <- sapply(patient_results, function(x) get_status_icon(x$fusion$status, simple = TRUE, reason = x$fusion$reason))
    patient_data$tumor <- sapply(patient_results, function(x) get_status_icon(x$tumor$status, simple = TRUE, reason = x$tumor$reason))
    patient_data$chimeric <- sapply(patient_results, function(x) get_status_icon(x$chimeric$status, simple = TRUE, reason = x$chimeric$reason))
    patient_data$arriba <- sapply(patient_results, function(x) get_status_icon(x$arriba$status, simple = TRUE, reason = x$arriba$reason))
    
    patient_data$icon <- sapply(patient_results, function(x) {
      all_statuses <- c(x$fusion$status, x$tumor$status, x$chimeric$status, x$arriba$status)
      all_reasons <- c(x$fusion$reason, x$tumor$reason, x$chimeric$reason, x$arriba$reason)
      
      has_multiple_files <- x$fusion$reason == "multiple_files"
      
      if (has_multiple_files) {
        get_status_icon("red", simple = FALSE, reason = "multiple_files")
      } else if ("red" %in% all_statuses) {
        red_reason <- all_reasons[all_statuses == "red"][1]
        get_status_icon("red", simple = FALSE, reason = red_reason)
      } else if ("orange" %in% all_statuses) {
        orange_reason <- all_reasons[all_statuses == "orange"][1]
        get_status_icon("orange", simple = FALSE, reason = orange_reason)
      } else if (all(all_statuses == "gray")) {
        get_status_icon("gray", simple = FALSE, reason = "optional_missing")
      } else {
        get_status_icon("green", simple = FALSE, reason = "files_ok")
      }
    })
    
    patient_data$files <- sapply(patient_results, function(x) {
      # For debugging: show found_files when multiple_files or other issues occur
      all_files <- unique(c(
        if (!is.null(x$fusion$found_files) && length(x$fusion$found_files) > 0) x$fusion$found_files else x$fusion$files,
        if (!is.null(x$tumor$found_files) && length(x$tumor$found_files) > 0) x$tumor$found_files else x$tumor$files,
        if (!is.null(x$chimeric$found_files) && length(x$chimeric$found_files) > 0) x$chimeric$found_files else x$chimeric$files,
        if (!is.null(x$arriba$found_files) && length(x$arriba$found_files) > 0) x$arriba$found_files else x$arriba$files
      ))
      if (length(all_files) == 0) "" else paste(str_replace(all_files,path,""), collapse = ",\n")
    })
    
  } 
  else if (dataset_type == "expression") {
    patient_data$expression <- sapply(patient_results, function(x) get_status_icon(x$expression$status, simple = TRUE, reason = x$expression$reason))

    patient_data$tissue <- sapply(patient_results, function(x) {
      tm_full <- x$expression$tissue_mapping_full
      if (length(tm_full) > 0) paste(names(tm_full), collapse = "\n") else ""
    })
    
    patient_data$icon <- sapply(patient_results, function(x) {
      # Only check expression status (GOI is now in reference files table)
      status <- x$expression$status
      reason <- x$expression$reason
      
      if (status == "red") {
        get_status_icon("red", simple = FALSE, reason = reason)
      } else if (status == "orange") {
        get_status_icon("orange", simple = FALSE, reason = reason)
      } else if (status == "gray") {
        get_status_icon("gray", simple = FALSE, reason = "optional_missing")
      } else {
        get_status_icon("green", simple = FALSE, reason = "files_ok")
      }
    })
    
    patient_data$files <- sapply(patient_results, function(x) {
      tm_full <- x$expression$tissue_mapping_full
      
      lines <- character(0)
      
      if (length(tm_full) > 0) {
        expr_lines <- ifelse(is.na(tm_full), "-", as.vector(tm_full))
        expr_lines <- str_replace(expr_lines, path, "")
        lines <- c(lines, expr_lines)
      } else {
        # For debugging: prefer found_files if available
        expr_files <- if (!is.null(x$expression$found_files) && length(x$expression$found_files) > 0) {
          x$expression$found_files
        } else {
          x$expression$files
        }
        if (length(expr_files) > 0) {
          lines <- c(lines, str_replace(expr_files, path, ""))
        }
      }
      
      if (length(lines) == 0) "" else paste(lines, collapse = ",\n")
    })
    
  }

  cols <- c("icon", setdiff(colnames(patient_data), "icon"))
  patient_data <- patient_data[, cols]
  output_data <- list(
    patient_tab  = patient_data,      
    raw_results  = patient_results,   
    dataset_type = dataset_type,      
    root_path    = path,
    tissue_mappings = if (dataset_type == "expression") {
      lapply(patient_results, function(x) x$expression$tissue_mapping)
    } else NULL
  )
  
  return(output_data)
}


#' @export
validate_datasets_status <- function(datasets_data) {
  validation_results <- list(
    has_red_status = FALSE,
    has_orange_status = FALSE,
    red_patients_list = list(),
    orange_patients_list = list(),
    igv_issues = character(),
    arriba_issues = character(),
    TMB_issues = c(),
    missing_expression_tissues = list()
  )
  
  for (dataset in names(datasets_data)) {
    data <- datasets_data[[dataset]]$patient_tab
    if (is.null(data)) next
    
    for (i in 1:nrow(data)) {
      patient <- data$patient[i]
      icon_html <- data$icon[i]
      
      if (grepl("#dc3545", icon_html)) {
        validation_results$has_red_status <- TRUE
        validation_results$red_patients_list[[patient]] <- c(validation_results$red_patients_list[[patient]], dataset)
      }
      
      if (grepl("#fd7e14", icon_html)) {
        validation_results$has_orange_status <- TRUE
        
        if (dataset %in% c("somatic", "germline", "fusion")) {
          if (dataset == "somatic" && "TMB" %in% colnames(data) && grepl("#fd7e14", data$TMB[i])) {
            validation_results$TMB_issues <- c(validation_results$TMB_issues, patient)
          } else {
            # columns_to_check: all orange columns that are shown in the warning dialog
            # igv_columns:      subset that actually affect IGV snapshot availability
            #   somatic  → only tumor BAM (normal BAM missing ≠ IGV unavailable)
            #   germline → none (germline doesn't use IGV snapshots)
            #   fusion   → tumor + chimeric
            columns_to_check <- switch(dataset,
                                       "somatic" = c("tumor", "normal"),
                                       "germline" = c("normal"),
                                       "fusion" = c("tumor", "chimeric", "arriba"))
            igv_columns <- switch(dataset,
                                  "somatic"  = c("tumor"),
                                  "germline" = character(0),
                                  "fusion"   = c("tumor", "chimeric"))
            for (col in columns_to_check) {
              if (!col %in% colnames(data)) next
              cell_html <- data[[col]][i]
              if (!grepl("#fd7e14", cell_html)) next
              
              # Detect different types of orange issues
              if (grepl("Missing BAM or BAI", cell_html, fixed = TRUE)) {
                if (col %in% igv_columns)
                  validation_results$igv_issues <- c(validation_results$igv_issues, patient)
              } else if (grepl("Missing PDF or TSV", cell_html, fixed = TRUE)) {
                validation_results$arriba_issues <- c(validation_results$arriba_issues, patient)
              } else if (grepl("Multiple files found", cell_html, fixed = TRUE)) {
                if (col %in% igv_columns) {
                  validation_results$igv_issues <- c(validation_results$igv_issues, patient)
                } else if (col == "arriba") {
                  validation_results$arriba_issues <- c(validation_results$arriba_issues, patient)
                }
              } else if (grepl("Required file missing", cell_html, fixed = TRUE)) {
                if (col %in% igv_columns) {
                  validation_results$igv_issues <- c(validation_results$igv_issues, patient)
                } else if (col == "arriba") {
                  validation_results$arriba_issues <- c(validation_results$arriba_issues, patient)
                }
              }
              validation_results$orange_patients_list[[patient]] <- c(validation_results$orange_patients_list[[patient]], dataset)
            }
          }
    
        }
        # Note: GOI is now checked in reference files, not per-patient
      }
      
      # Collect missing tissues for expression warning dialog
      if (dataset == "expression") {
        raw <- datasets_data[[dataset]]$raw_results[[patient]]
        if (!is.null(raw)) {
          mt <- raw$expression$missing_tissues
          if (length(mt) > 0) {
            validation_results$missing_expression_tissues[[patient]] <- mt
          }
        }
      }
    }
  }
  
  validation_results$igv_issues <- unique(validation_results$igv_issues)
  validation_results$arriba_issues <- unique(validation_results$arriba_issues)
  return(validation_results)
}



# Function for checking BAM/BAI or PDF/TSV pairs
check_pair <- function(files, patient, pair = c("bam_bai","pdf_tsv")) {
  pair <- match.arg(pair)
  
  if (pair == "pdf_tsv") { # check_arriba_pair
    pdf_files <- files[str_detect(files, patient) & str_detect(files, "\\.pdf$")]
    tsv_files <- files[str_detect(files, patient) & str_detect(files, "\\.tsv$")]
    
    if (length(pdf_files) == 0 && length(tsv_files) == 0) {
      return("none")
    } else if (length(pdf_files) > 1 || length(tsv_files) > 1) {
      return("multiple") # too many PDF or TSV files
    } else if (length(pdf_files) > 0 && length(tsv_files) > 0) {
      return("complete")
    } else {
      return("incomplete")
    }
  } else { # pair == "bam_bai"
    bam_files <- files[str_detect(files, "\\.bam$") & str_detect(files, patient)]
    bai_files <- files[str_detect(files, "\\.bai$") & str_detect(files, patient)]
    
    if (length(bam_files) == 0) return("none")
    
    # Check if there are multiple BAM files
    if (length(bam_files) > 1) {
      return("multiple") # too many BAM files
    }
    
    # Check that each BAM has a matching BAI
    bam_bases <- str_remove(bam_files, "\\.bam$")
    bai_bases <- str_remove(bai_files, "\\.bam\\.bai$|\\.bai$")
    
    missing_bai <- setdiff(bam_bases, bai_bases)
    
    if (length(missing_bai) > 0) {
      return("incomplete") # have BAM but BAI is missing
    } else if (length(bam_files) > 0) {
      return("complete") # have both BAM and BAI
    } else {
      return("none")
    }
  }

}

# token-match at segment boundaries (/ . _ -), case-insensitive
.make_tissue_regex <- function(tissue_list) {
  if (!length(tissue_list)) return(NULL)
  esc <- function(x) gsub("([][(){}.+*?^$\\|\\\\])","\\\\\\1", x, perl = TRUE)
  paste(
    sprintf("(?i)(^|[\\/._-])%s([^\\/]*?)([\\/._-]|$)", sapply(tissue_list, esc)),
    collapse = "|"
  )
}


create_tissue_file_mapping <- function(expression_files, tissues_str) {
  if (is.null(tissues_str)) {
    tissues_clean <- NULL
  } else if (length(tissues_str) > 1) {
    tissues_clean <- paste(tissues_str, collapse = ",")
  } else if (length(tissues_str) == 1) {
    tissues_clean <- tissues_str
  } else {
    tissues_clean <- NULL
  }
  
  if (is.null(tissues_clean) || length(tissues_clean) == 0 || !nzchar(tissues_clean)) {
    return(list(
      mapping = setNames(character(0), character(0)),
      unmatched_files = expression_files,
      missing_tissues = character(0),
      status = if (length(expression_files) > 0) "green" else "red"
    ))
  }
  # Split by comma OR newline to support both formats:
  # - "blood,liver,breast" 
  # - "blood\nliver\nbreast"
  tissues <- trimws(unlist(strsplit(tissues_clean, "[,\n]+")))
  tissues <- tissues[nzchar(tissues)]
  tissues <- unique(tissues)
  original_tissues <- tissues  # Store original order
  
  if (length(tissues) == 0) {
    return(list(
      mapping = setNames(character(0), character(0)),
      unmatched_files = expression_files,
      missing_tissues = character(0),
      status = if (length(expression_files) > 0) "green" else "red"
    ))
  }
  
  # Sort tissues by decreasing length for matching (to prioritize specific names)
  tissues <- tissues[order(nchar(tissues), decreasing = TRUE)]
  
  # Create mapping: tissue -> file
  tissue_to_file <- setNames(rep(NA_character_, length(tissues)), tissues)
  matched_files <- character(0)
  
  # Filter only files matching the specified tissues
  tissue_regex <- .make_tissue_regex(tissues)
  relevant_files <- expression_files[str_detect(expression_files, tissue_regex)]
  
  # Assign files to tissues
  available_files <- relevant_files
  for (tissue in tissues) {
    rx <- .make_tissue_regex(c(tissue))
    matching_files <- available_files[str_detect(available_files, rx)]
    
    if (length(matching_files) == 0) next
    
    if (length(matching_files) > 1) {
      warning(paste("Multiple files found for tissue", tissue, "- using:", matching_files[1]))
    }
    
    tissue_to_file[tissue] <- matching_files[1]
    matched_files <- c(matched_files, matching_files[1])
    available_files <- available_files[available_files != matching_files[1]]
  }
  
  missing_tissues <- names(tissue_to_file)[is.na(tissue_to_file)]
  
  # Status depends on how many tissues have matching files
  if (length(missing_tissues) == length(tissues)) {
    status <- "red"    # ALL tissues missing - no expression files found at all
  } else if (length(missing_tissues) > 0) {
    status <- "mixed"  # SOME tissues missing, some found - user can proceed with warning
  } else {
    status <- "green"  # All specified tissues have exactly one file
  }
  
  # Reorder tissue_to_file to match original order
  tissue_to_file <- tissue_to_file[original_tissues]
  
  return(list(
    mapping = tissue_to_file[!is.na(tissue_to_file)][original_tissues[!is.na(tissue_to_file[original_tissues])]],
    unmatched_files = available_files,  # Remaining files not assigned to any tissue
    missing_tissues = missing_tissues,
    status = status,
    all_mapping = tissue_to_file
  ))
}


get_dataset_columns <- function(dataset_type) {
  columns_config <- list(
    somatic = list(
      icon = list(name = "Status", width = 80),
      patient = list(name = "Patient", width = 120),
      variant = list(name = "Variant file", width = 80),
      tumor = list(name = "Tumor BAM file", width = 90),
      normal = list(name = "Normal BAM file", width = 90),
      TMB = list(name = "Tumor mutation burden file", width = 130),
      files = list(name = "Detected files", minWidth = 300)
    ),
    germline = list(
      icon = list(name = "Status", width = 80),
      patient = list(name = "Patient", width = 120),
      variant = list(name = "Variant file", width = 80),
      normal = list(name = "Normal BAM file", width = 90),
      files = list(name = "Detected files", minWidth = 400)
    ),
    fusion = list(
      icon = list(name = "Status", width = 80),
      patient = list(name = "Patient", width = 120),
      fusion = list(name = "Fusion file", width = 80),
      tumor = list(name = "Tumor BAM file", width = 90),
      chimeric = list(name = "Chimeric BAM file", width = 90),
      arriba = list(name = "Arriba files", width = 80),
      files = list(name = "Detected files", minWidth = 300)
    ),
    expression = list(
      icon = list(name = "Status", width = 80),
      patient = list(name = "Patient", width = 120),
      expression = list(name = "Expression file", width = 105),
      tissue = list(name = "Tissue", width = 120), 
      files = list(name = "Detected files", minWidth = 400)
    )
  )
  
  return(columns_config[[dataset_type]])
}

#' @export
create_reactable <- function(data, dataset_type) {
  if (is.null(data)) return(NULL)
  
  columns_config <- get_dataset_columns(dataset_type)
  columns_def <- list()
  
  for (col_name in names(columns_config)) {
    config <- columns_config[[col_name]]
    
    if (col_name == "icon" || col_name %in% c("variant", "tumor", "normal", "TMB","fusion", "chimeric", "arriba","expression")) {
      columns_def[[col_name]] <- colDef(
        name = config$name,
        width = config$width,
        align = "center",
        html = TRUE,
        cell = function(value) value
      )
    } else if (col_name == "patient") {
      columns_def[[col_name]] <- colDef(
        name = config$name,
        width = config$width,
        align = "center"
      )
    } else if (col_name == "tissue") {
      columns_def[[col_name]] <- colDef(
        name = config$name,
        width = config$width,
        cell = function(value) {
          if (value == "") {
            div(value, style = "color: #999; font-style: italic;")
          } else {
            tissue_list <- strsplit(value, "\n")[[1]]
            div(
              lapply(tissue_list, function(tissue) {
                div(strsplit(tissue, ": ")[[1]], style = "margin-bottom: 2px; font-family: monospace; font-size: 11px;")
              })
            )
          }
        }
      )
    } else if (col_name == "files") {
      columns_def[[col_name]] <- colDef(
        name = config$name,
        minWidth = config$minWidth,
        cell = function(value) {
          if (value == "") {
            div("No files found", style = "color: #999; font-style: italic;")
          } else {
            files_list <- strsplit(value, ",\n")[[1]]
            div(
              lapply(files_list, function(file) {
                div(file, style = "margin-bottom: 2px; font-family: monospace; font-size: 11px;")
              })
            )
          }
        }
      )
    }
  }
  
  reactable(
    data,
    columns = columns_def,
    defaultPageSize = 10,
    striped = TRUE,
    highlight = TRUE,
    borderless = FALSE,
    compact = TRUE
  )
}

#' Create reference files data table
#' @param kegg_tab_path Path to kegg_tab file from config
#' @param goi_path Path to genes_of_interest file from config (can be NULL)
#' @param report_template_path Path to report_template file from config
#' @param datasets Vector of active dataset types (to determine if kegg_tab is required)
#' @return List with reference_tab and validation info
#' @export
create_reference_files_data <- function(kegg_tab_path, goi_path, report_template_path, datasets) {
  
  # Determine if kegg_tab is required based on active datasets
  # Required for: somatic (sankey plot), expression (expression profile & network graph)
  kegg_required <- any(c("somatic", "expression") %in% datasets)
  
  # Determine if GOI is relevant (only for expression dataset) 
  # In the future, HERE add other datasets when GOI is relevant for them
  goi_relevant <- "expression" %in% datasets
  
  # Check file existence (also check for empty strings)
  kegg_exists <- !is.null(kegg_tab_path) && nzchar(kegg_tab_path) && file.exists(kegg_tab_path)
  goi_exists <- !is.null(goi_path) && nzchar(goi_path) && file.exists(goi_path)
  report_exists <- !is.null(report_template_path) && nzchar(report_template_path) && file.exists(report_template_path)
  
  # Determine status for each file
  # kegg_tab
  if (is.null(kegg_tab_path) || !nzchar(kegg_tab_path)) {
    # Empty/NULL path - check if required
    if (kegg_required) {
      kegg_status <- "red"
      kegg_reason <- "kegg_missing_required"
    } else {
      kegg_status <- "gray"
      kegg_reason <- "optional_missing"
    }
  } else if (!kegg_exists) {
    # Path configured but file doesn't exist
    kegg_status <- if (kegg_required) "red" else "orange"
    kegg_reason <- if (kegg_required) "kegg_missing_required" else "kegg_missing_optional"
  } else {
    kegg_status <- "green"
    kegg_reason <- "single_file_ok"
  }
  
  # GOI (optional, only relevant for expression dataset)
  if (is.null(goi_path) || !nzchar(goi_path)) {
    goi_status <- "gray"
    goi_reason <- "optional_missing"
  } else if (!goi_exists) {
    # Only show orange warning if expression dataset is active
    if (goi_relevant) {
      goi_status <- "orange"
      goi_reason <- "goi_configured_missing"
    } else {
      # Not relevant for current datasets, show gray
      goi_status <- "gray"
      goi_reason <- "optional_missing"
    }
  } else {
    goi_status <- "green"
    goi_reason <- "single_file_ok"
  }
  
  # report_template (used by all datasets for report download - show warning if missing)
  if (is.null(report_template_path) || !nzchar(report_template_path)) {
    report_status <- "orange"
    report_reason <- "report_template_missing"
  } else if (!report_exists) {
    report_status <- "orange"
    report_reason <- "report_template_missing"
  } else {
    report_status <- "green"
    report_reason <- "single_file_ok"
  }
  
  # Create data frame for the table
  reference_tab <- data.frame(
    icon = get_status_icon(
      if (kegg_status == "red") "red" else if (any(c(kegg_status, goi_status, report_status) == "orange")) "orange" else if (all(c(kegg_status, goi_status, report_status) %in% c("green", "gray"))) "green" else "gray",
      simple = FALSE,
      reason = if (kegg_status == "red") kegg_reason else if (kegg_status == "orange") kegg_reason else if (goi_status == "orange") goi_reason else if (report_status == "orange") report_reason else "files_ok"
    ),
    kegg_tab = get_status_icon(kegg_status, simple = TRUE, reason = kegg_reason),
    goi = get_status_icon(goi_status, simple = TRUE, reason = goi_reason),
    report_template = get_status_icon(report_status, simple = TRUE, reason = report_reason),
    files = paste(c(
      if (!is.null(kegg_tab_path)) kegg_tab_path else "",
      if (!is.null(goi_path)) goi_path else "",
      if (!is.null(report_template_path)) report_template_path else ""
    ), collapse = ",\n"),
    stringsAsFactors = FALSE
  )
  
  return(list(
    reference_tab = reference_tab,
    kegg_status = kegg_status,
    goi_status = goi_status,
    report_status = report_status,
    kegg_required = kegg_required
  ))
}

#' Create reactable for reference files
#' @param data Reference files data frame
#' @return Reactable object
#' @export
create_reference_files_reactable <- function(data) {
  if (is.null(data)) return(NULL)
  
  reactable(
    data,
    columns = list(
      icon = colDef(
        name = "Status",
        width = 80,
        align = "center",
        html = TRUE,
        cell = function(value) value
      ),
      kegg_tab = colDef(
        name = "Pathways file",
        width = 120,
        align = "center",
        html = TRUE,
        cell = function(value) value
      ),
      goi = colDef(
        name = "Genes of interest file",
        width = 140,
        align = "center",
        html = TRUE,
        cell = function(value) value
      ),
      report_template = colDef(
        name = "Report template",
        width = 120,
        align = "center",
        html = TRUE,
        cell = function(value) value
      ),
      files = colDef(
        name = "Detected files",
        minWidth = 300,
        cell = function(value) {
          if (value == "") {
            div("No files configured", style = "color: #999; font-style: italic;")
          } else {
            files_list <- strsplit(value, ",\n")[[1]]
            files_list <- files_list[nzchar(files_list)]
            div(
              lapply(files_list, function(file) {
                div(file, style = "margin-bottom: 2px; font-family: monospace; font-size: 11px;")
              })
            )
          }
        }
      )
    ),
    defaultPageSize = 1,  # Only one row for reference files
    pagination = FALSE,   # No pagination needed
    striped = FALSE,
    highlight = TRUE,
    borderless = FALSE,
    compact = TRUE
  )
}

#' @export
build_confirmed_paths <- function(dataset_objects, root_path) {
  result_rows <- list()    
  
  for (dataset_name in names(dataset_objects)) {
    dataset_object    <- dataset_objects[[dataset_name]]
    patient_tab       <- dataset_object$patient_tab
    raw_results_list  <- dataset_object$raw_results
    dataset_root_path <- dataset_object$root_path
    tissue_mappings   <- dataset_object$tissue_mappings  # Get tissue mappings
    
    if (is.null(patient_tab) || nrow(patient_tab) == 0) next
    if (is.null(raw_results_list) || length(raw_results_list) != nrow(patient_tab)) next
    

    
    for (row_index in seq_len(nrow(patient_tab))) {
      patient_id    <- patient_tab$patient[row_index]
      file_type_map <- raw_results_list[[row_index]]
      if (is.null(file_type_map)) next
      
      for (file_type_name in names(file_type_map)) {
        file_paths <- file_type_map[[file_type_name]]$files
        if (length(file_paths) == 0) next

        is_absolute <- grepl("^/|^[A-Za-z]:", file_paths)
        absolute_paths <- file_paths
        absolute_paths[!is_absolute] <- file.path(dataset_root_path, sub("^/+", "", file_paths[!is_absolute]))
        
        # --- tissue assignment ---
        tissue_list <- rep("none", length(absolute_paths))
        if (identical(dataset_name, "expression") && identical(file_type_name, "expression")) {
          # Use tissue_mappings from dataset_object
          current_mapping <- tissue_mappings[[row_index]]  # Mapping for current patient
          if (!is.null(current_mapping) && length(current_mapping) > 0) {
            for (k in seq_along(absolute_paths)) {
              path_k <- absolute_paths[k]
              # Find tissue for this file path
              tissue <- names(current_mapping)[current_mapping == path_k]
              tissue_list[k] <- if (length(tissue) > 0) tissue[1] else "none"
            }
          }
        }

        result_rows[[length(result_rows) + 1]] <- data.frame(
          dataset   = rep(dataset_name, length(absolute_paths)),
          patient   = rep(patient_id, length(absolute_paths)),
          file_type = rep(file_type_name, length(absolute_paths)),
          path      = absolute_paths,
          tissue    = tissue_list,
          root_path = rep(root_path, length(absolute_paths)),
          stringsAsFactors = FALSE
        )
      }
      
    }
  }

  if (length(result_rows) == 0) {
    data.frame(
      dataset = character(), patient = character(),
      file_type = character(), path = character(),
      tissue = character(), root_path = character(),    
      stringsAsFactors = FALSE
    )
  } else {
    as.data.frame(rbindlist(result_rows, use.names = TRUE, fill = FALSE))
  }
}






# ============================================================================
# REQUIRED COLUMN VALIDATION
# ============================================================================

validate_file_columns <- function(file_path, dataset_type, patient_id) {
  
  required_cols <- get_required_columns(dataset_type)
  if (is.null(required_cols) || length(required_cols) == 0) {
    return(list(valid = TRUE, dataset = dataset_type, patient = patient_id))
  }
  
  # Split multiple paths
  if (length(file_path) == 1 && grepl(",", file_path)) {
    file_path <- trimws(unlist(strsplit(file_path, ",")))
  }
  
  # Validate each file
  for (fp in file_path) {
    if (is.null(fp) || is.na(fp) || fp == "") next
    
    actual_cols <- read_file_header(fp)
    if (is.null(actual_cols)) {
      return(list(
        valid = FALSE,
        error_type = "read_error",
        dataset = dataset_type,
        patient = patient_id,
        file_path = fp
      ))
    }
    
    # Kontrola case-insensitive:
    actual_cols_lower <- tolower(actual_cols)
    missing_cols <- required_cols[!tolower(required_cols) %in% actual_cols_lower]
    
    # Special handling for fusion: accept both chr1/chr2 and chrom1/chrom2 (case-insensitive)
    if (dataset_type == "fusion") {
      # If file has chrom1/chrom2, temporarily rename in actual_cols_lower for validation
      if (all(c("chrom1", "chrom2") %in% actual_cols_lower)) {
        actual_cols_lower[actual_cols_lower == "chrom1"] <- "chr1"
        actual_cols_lower[actual_cols_lower == "chrom2"] <- "chr2"
        # Recheck missing columns after renaming
        missing_cols <- required_cols[!tolower(required_cols) %in% actual_cols_lower]
      }
    }
    
    # Special handling for germline: accept any known MAF column as fallback for gnomad_nfe
    if (dataset_type == "germline") {
      has_maf <- any(MAF_COLUMN_CANDIDATES %in% actual_cols_lower)
      if (!has_maf) {
        return(list(
          valid = FALSE,
          error_type = "missing_columns",
          missing = c("gnomAD_NFE (or any population frequency column: gnomad_af, 1000g_eur_af, etc.)"),
          dataset = dataset_type,
          patient = patient_id,
          file_path = fp
        ))
      }
      # If validation flagged gnomad_nfe as missing but another MAF col is present, clear it
      missing_cols <- missing_cols[tolower(missing_cols) != "gnomad_nfe"]
    }
    
    
    # missing_cols <- setdiff(required_cols, actual_cols)
    if (length(missing_cols) > 0) {
      return(list(
        valid = FALSE,
        error_type = "missing_columns",
        missing = missing_cols,
        dataset = dataset_type,
        patient = patient_id,
        file_path = fp
      ))
    }
  }

  return(list(valid = TRUE, dataset = dataset_type, patient = patient_id))
}



# ============================================================================
# ALL DATASET VALIDATION
# ============================================================================

#' Validate columns for all datasets and patients
#' @param datasets_data Output from create_dataset_data() - list of datasets
#' @return List with validation results and error details
#' @export
validate_all_columns <- function(datasets_data) {
  
  validation_errors <- list()
  
  for (dataset in names(datasets_data)) {

    patient_tab <- datasets_data[[dataset]]$patient_tab
    data <- datasets_data[[dataset]]$raw_results
    
    if (is.null(patient_tab)) next
    
    for (i in 1:nrow(patient_tab)) {
      patient <- patient_tab$patient[i]

      file_path <- switch(dataset,
                          somatic = data[[patient]]$variant$files,
                          germline = data[[patient]]$variant$files,
                          fusion = data[[patient]]$fusion$files,
                          expression = data[[patient]]$expression$files,
                          NULL
      )

      # Split multiple paths
      if (is.null(file_path) || all(is.na(file_path)) || all(file_path == "")) next
      
      # If it's a single string with commas, split it
      if (length(file_path) == 1 && grepl(",", file_path)) {
        file_path <- trimws(unlist(strsplit(file_path, ",")))
      }
      
      # Validate columns
      validation <- validate_file_columns(file_path, dataset, patient)
      message("validation: ", paste0(validation,collapse = ", "))
      if (!validation$valid) {
        validation_errors <- c(validation_errors, list(validation))
      }
    }
  }
  
  return(list(
    has_errors = length(validation_errors) > 0,
    errors = validation_errors
  ))
}


# ============================================================================
# CREATE HTML ERROR MESSAGE
# ============================================================================

#' Create HTML error message for shinyalert
#' @param validation_result Output from validate_all_columns()
#' @return HTML formatted string
#' @export
create_column_error_message <- function(validation_result) {
  
  if (!validation_result$has_errors) {
    return(NULL)
  }
  
  error_lines <- lapply(validation_result$errors, function(err) {
    
    if (err$error_type == "read_error") {
      sprintf(
        "<li style='text-align: left; margin-bottom: 8px;'><b>%s</b> (<i>%s</i>) – <span style='color: #d32f2f;'>Cannot read file</span></li>",
        err$patient, err$dataset
      )
    } else if (err$error_type == "missing_columns") {
      # Format each missing column as a separate badge
      missing_badges <- sapply(err$missing, function(col) {
        sprintf("<span style='background-color: #fff3e0; color: #e65100; padding: 2px 6px; border-radius: 3px; font-family: monospace; font-size: 0.9em;'>%s</span>", col)
      })
      missing_html <- paste(missing_badges, collapse = " ")
      
      sprintf(
        "<li style='text-align: left; margin-bottom: 12px;'><b>%s</b> (<i>%s</i>)<br><span style='margin-left: 1em; color: #666;'>Missing columns:</span> %s</li>",
        err$patient, err$dataset, missing_html
      )
    } else {
      sprintf(
        "<li style='text-align: left; margin-bottom: 8px;'><b>%s</b> (<i>%s</i>) – <span style='color: #d32f2f;'>Unknown error</span></li>",
        err$patient, err$dataset
      )
    }
  })
  
  html_text <- paste0(
    "<div style='text-align: center;'><strong style='color: #d32f2f; font-size: 1.1em;'>❌ Required columns are missing in the following files:</strong></div><br>",
    "<ul style='text-align: left; margin-left: 1.5em; list-style-type: none; padding-left: 0;'>",
    paste(error_lines, collapse = ""),
    "</ul>",
    "<br><div style='text-align: center; color: #666; font-size: 0.95em;'>Please upload correct files or remove affected patients from the analysis.</div>"
  )
  
  return(html_text)
}


