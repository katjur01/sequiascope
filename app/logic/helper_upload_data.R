# app/logic/helper_upload_data.R
box::use(
  htmltools[tags,HTML,div,span,h2,h4,h5,br],
  stringr[str_detect,regex,str_replace,str_remove],
  reactable[reactable,colDef],
  data.table[rbindlist]
)

get_status_icon <- function(color, simple = FALSE, reason = "unknown") {
  
  # Definice všech možných tooltip zpráv podle reason - ENGLISH
  tooltip_messages <- list(
    # Červené stavy
    "required_missing" = list(
      simple = "Required file missing",
      detailed = "Missing files"
    ),
    
    # Zelené stavy  
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
    
    # Oranžové stavy
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
    
    # Šedé stavy
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
  
  
  
  # Získej zprávu podle reason
  message_type <- if (simple) "simple" else "detailed"
  tooltip_text <- if (!is.null(tooltip_messages[[reason]][[message_type]])) {
    tooltip_messages[[reason]][[message_type]]
  } else {
    tooltip_messages[["unknown"]][[message_type]]
  }
  
  if (simple) {
    simple_icons <- list(
      "red" = paste0('<span class="status-tooltip" data-tooltip="', tooltip_text, '"><i class="fas fa-times" style="color: #dc3545; font-size: 14px;"></i></span>'),
      "orange" = paste0('<span class="status-tooltip" data-tooltip="', tooltip_text, '"><i class="fas fa-exclamation" style="color: #fd7e14; font-size: 14px;"></i></span>'),
      "green" = paste0('<span class="status-tooltip" data-tooltip="', tooltip_text, '"><i class="fas fa-check" style="color: #28a745; font-size: 14px;"></i></span>'),
      "gray" = paste0('<span class="status-tooltip" data-tooltip="', tooltip_text, '"><i class="fas fa-minus" style="color: #6c757d; font-size: 14px;"></i></span>')
    )
    
    return(if (!is.null(simple_icons[[color]])) simple_icons[[color]] else simple_icons[["red"]])
    
  } else {
    icons <- list(
      "red" = paste0('<span class="status-tooltip" data-tooltip="', tooltip_text, '"><div style="width: 20px; height: 20px; background-color: #dc3545; border-radius: 50%; display: flex; align-items: center; justify-content: center; margin: 0 auto;"><i class="fas fa-times" style="color: white; font-size: 10px;"></i></div></span>'),
      "orange" = paste0('<span class="status-tooltip" data-tooltip="', tooltip_text, '"><div style="width: 20px; height: 20px; background-color: #fd7e14; border-radius: 50%; display: flex; align-items: center; justify-content: center; margin: 0 auto;"><i class="fas fa-exclamation" style="color: white; font-size: 10px;"></i></div></span>'),
      "green" = paste0('<span class="status-tooltip" data-tooltip="', tooltip_text, '"><div style="width: 20px; height: 20px; background-color: #28a745; border-radius: 50%; display: flex; align-items: center; justify-content: center; margin: 0 auto;"><i class="fas fa-check" style="color: white; font-size: 10px;"></i></div></span>'),
      "gray" = paste0('<span class="status-tooltip" data-tooltip="', tooltip_text, '"><div style="width: 20px; height: 20px; background-color: #6c757d; border-radius: 50%; display: flex; align-items: center; justify-content: center; margin: 0 auto;"><i class="fas fa-minus" style="color: white; font-size: 10px;"></i></div></span>')
    )
    
    return(if (!is.null(icons[[color]])) icons[[color]] else icons[["red"]])
  }
}

#' @export
create_dataset_data <- function(dataset_type, files, goi_files, patients, path, tumor_pattern, normal_pattern) {
  # patients je už vyhodnocená reactive hodnota (bez závorek)
  if (length(patients) == 0) return(NULL)
  
  # # Definice sloupců podle dataset typu
  dataset_columns <- get_dataset_columns(dataset_type)
  column_names <- names(dataset_columns)
  
  patient_results <- lapply(patients, function(patient) {
    
    if (dataset_type == "somatic") {
      list(
        variant = evaluate_file_status(files, patient, "variant", "somatic"),
        tumor = evaluate_file_status(files, patient, "tumor", "somatic", tumor_pattern$somatic),
        normal = evaluate_file_status(files, patient, "normal", "somatic",normal_pattern$somatic)
      )
    } else if (dataset_type == "germline") {
      list(
        variant = evaluate_file_status(files, patient, "variant", "germline"),
        normal = evaluate_file_status(files, patient, "normal", "germline", normal_pattern$germline)
      )
    } else if (dataset_type == "fusion") {
      list(
        fusion = evaluate_file_status(files, patient, "fusion", "fusion"),
        tumor = evaluate_file_status(files, patient, "tumor", "fusion", tumor_pattern$fusion),
        chimeric = evaluate_file_status(files, patient, "chimeric", "fusion", tumor_pattern$chimeric),
        arriba = evaluate_file_status(files, patient, "arriba", "fusion", tumor_pattern$arriba)
      )
    } else if (dataset_type == "expression") {
      list(
        expression = evaluate_file_status(files, patient, "expression", "expression"),
        goi = evaluate_file_status(goi_files, patient, "goi", "expression")
      )
    }
  })
  # Vytvoř základní data frame
  patient_data <- data.frame(
    patient = patients,
    stringsAsFactors = FALSE
  )
  
  # Přidej sloupce podle dataset typu
  if (dataset_type == "somatic") {
    patient_data$variant <- sapply(patient_results, function(x) get_status_icon(x$variant$status, simple = TRUE, reason = x$variant$reason))
    patient_data$tumor <- sapply(patient_results, function(x) get_status_icon(x$tumor$status, simple = TRUE, reason = x$tumor$reason))
    patient_data$normal <- sapply(patient_results, function(x) get_status_icon(x$normal$status, simple = TRUE, reason = x$normal$reason))
    
    # Celkový stav
    patient_data$icon <- sapply(patient_results, function(x) {
      all_statuses <- c(x$variant$status, x$tumor$status, x$normal$status)
      all_reasons <- c(x$variant$reason, x$tumor$reason, x$normal$reason)
      
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
    
    # Files - zůstává stejné
    patient_data$files <- sapply(patient_results, function(x) {
      all_files <- unique(c(x$variant$files, x$tumor$files, x$normal$files))
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
      all_files <- unique(c(x$variant$files, x$normal$files))
      if (length(all_files) == 0) "" else paste(str_replace(all_files,path,""), collapse = ",\n")
    })
    
  } else if (dataset_type == "fusion") {
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
      all_files <- unique(c(x$fusion$files, x$tumor$files, x$chimeric$files, x$arriba$files))
      if (length(all_files) == 0) "" else paste(str_replace(all_files,path,""), collapse = ",\n")
    })
    
  } else if (dataset_type == "expression") {
    patient_data$expression <- sapply(patient_results, function(x) get_status_icon(x$expression$status, simple = TRUE, reason = x$expression$reason))
    patient_data$goi <- sapply(patient_results, function(x) get_status_icon(x$goi$status, simple = TRUE, reason = x$goi$reason))
    
    patient_data$icon <- sapply(patient_results, function(x) {
      all_statuses <- c(x$expression$status, x$goi$status)
      all_reasons <- c(x$expression$reason, x$goi$reason)
      
      if ("red" %in% all_statuses) {
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
      all_files <- unique(c(x$expression$files, x$goi$files))
      if (length(all_files) == 0) "" else paste(str_replace(all_files,path,""), collapse = ",\n")
    })
  }
  cols <- c("icon", setdiff(colnames(patient_data), "icon"))
  patient_data <- patient_data[, cols]
  
  # Prepare output object with both table and raw data
  output_data <- list(
    patient_tab  = patient_data,      # data.frame for display in UI
    raw_results  = patient_results,   # list: per-patient, per-file_type info
    dataset_type = dataset_type,      # e.g. "somatic", "germline"
    root_path    = path               # absolute base path
  )
  
  return(output_data)
  # return(patient_data)
}


#' @export
validate_datasets_status <- function(datasets_data) {
  validation_results <- list(
    has_red_status = FALSE,
    has_orange_status = FALSE,
    red_patients_list = list(),
    orange_patients_list = list(),   # nechávám, pokud to ještě někde používáš
    igv_issues = character(),        # <-- NOVĚ: pacienti s chybějícím BAM/BAI
    arriba_issues = character(),     # <-- NOVĚ: pacienti s chybějícím PDF/TSV
    goi_issues = c()
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
        
        # IGV/Arriba sloupce — rozlišíme podle tooltip textu v buňce
        if (dataset %in% c("somatic", "germline", "fusion")) {
          columns_to_check <- switch(dataset,
                                     "somatic" = c("tumor", "normal"),
                                     "germline" = c("normal"),
                                     "fusion" = c("tumor", "chimeric", "arriba")
          )
          
          for (col in columns_to_check) {
            if (!col %in% colnames(data)) next
            cell_html <- data[[col]][i]
            if (!grepl("#fd7e14", cell_html)) next  # jen oranžové
            
            # Podle textu v tooltipu rozlišíme příčinu
            if (grepl("Missing BAM or BAI", cell_html, fixed = TRUE)) {
              validation_results$igv_issues <- c(validation_results$igv_issues, patient)
            } else if (grepl("Missing PDF or TSV", cell_html, fixed = TRUE)) {
              validation_results$arriba_issues <- c(validation_results$arriba_issues, patient)
            }
            
            # (volitelně) udrž i původní mapování datasetu
            validation_results$orange_patients_list[[patient]] <- c(validation_results$orange_patients_list[[patient]], dataset)
          }
        }
        
        # GOI (beze změny)
        if (dataset == "expression" && "goi" %in% colnames(data) && grepl("#fd7e14", data$goi[i])) {
          validation_results$goi_issues <- c(validation_results$goi_issues, patient)
        }
      }
    }
  }
  
  # Odstraň duplicity
  validation_results$igv_issues <- unique(validation_results$igv_issues)
  validation_results$arriba_issues <- unique(validation_results$arriba_issues)
  return(validation_results)
}



# Funkce pro kontrolu BAM/BAI mebo PDG/TSV párů
check_pair <- function(files, patient, pair = c("bam_bai","pdf_tsv")) {
  pair <- match.arg(pair)
  
  if (pair == "pdf_tsv") { # check_arriba_pair
    pdf_files <- files[str_detect(files, patient) & str_detect(files, "\\.pdf$")]
    tsv_files <- files[str_detect(files, patient) & str_detect(files, "\\.tsv$")]
    
    if (length(pdf_files) == 0 && length(tsv_files) == 0) {
      return("none")
    } else if (length(pdf_files) > 0 && length(tsv_files) > 0) {
      return("complete")
    } else {
      return("incomplete")
    }
  } else { # pair == "bam_bai"
    bam_files <- files[str_detect(files, "\\.bam$") & str_detect(files, patient)]
    bai_files <- files[str_detect(files, "\\.bai$") & str_detect(files, patient)]
    
    if (length(bam_files) == 0) return("none")
    
    # Kontrola, zda každý BAM má odpovídající BAI
    bam_bases <- str_remove(bam_files, "\\.bam$")
    bai_bases <- str_remove(bai_files, "\\.bam\\.bai$|\\.bai$")
    
    missing_bai <- setdiff(bam_bases, bai_bases)
    
    if (length(missing_bai) > 0) {
      return("incomplete") # máme BAM ale chybí BAI
    } else if (length(bam_files) > 0) {
      return("complete") # máme BAM i BAI
    } else {
      return("none")
    }
  }

}


evaluate_file_status <- function(files, patient, file_type, dataset_type, patterns = NULL) {
  # patient_files <- goi_files[str_detect(goi_files, patient)]
  
  file_configs <- list(
    variant_somatic = list(extensions = "\\.(vcf|tsv)$", keywords = "somatic", required = TRUE),
    tumor_somatic   = list(extensions = "\\.(bam|bai)$", keywords = "somatic", required = FALSE, check_pair = TRUE, pair = "bam_bai"),
    normal_somatic  = list(extensions = "\\.(bam|bai)$", keywords = "somatic", required = FALSE, check_pair = TRUE, pair = "bam_bai"),
    
    variant_germline = list(extensions = "\\.(vcf|tsv)$", keywords = "germline", required = TRUE),
    normal_germline  = list(extensions = "\\.(bam|bai)$", keywords = "germline", required = FALSE, check_pair = TRUE, pair = "bam_bai"),
    
    fusion_fusion    = list(extensions = "\\.(tsv|xlsx)$",keywords = "fusion", exclude = "arriba|STAR", required = TRUE),
    tumor_fusion     = list(extensions = "\\.(bam|bai)$", keywords = "fusion", exclude = "Chimeric|transcriptome", required = FALSE, check_pair = TRUE, pair = "bam_bai"),
    chimeric_fusion  = list(extensions = "\\.(bam|bai)$", keywords = "fusion", required = FALSE, check_pair = TRUE, pair = "bam_bai"),
    arriba_fusion    = list(extensions = "\\.(pdf|tsv)$", keywords = "fusion", exclude = "discarded|STAR", required = FALSE, check_pair = TRUE, pair = "pdf_tsv"),
    
    expression_expression = list(extensions = "\\.(tsv|xlsx)$", keywords = "expression|RNAseq", exclude = "report", required = TRUE),
    goi_expression        = list(extensions = "genes_of_interest\\.(tsv|xlsx)$", keywords = "expression|RNAseq", exclude = "report", required = FALSE)
  )
  
  config_key <- paste(file_type, dataset_type, sep = "_")
  config <- file_configs[[config_key]]
  if (is.null(config)) return("red")
  
  # ✅ Sestav finalní regex: výchozí + uživatelské keywords
  base_keywords <- config$keywords
  user_keywords <- if (!is.null(patterns) && nzchar(patterns)) {
    trimws(unlist(strsplit(patterns, ",")))
  } else {
    character(0)
  }
  
  all_keywords <- c(base_keywords, user_keywords)
  keyword_regex <- paste(all_keywords, collapse = "|")
  
  relevant_files <- if (config_key == "goi_expression") {
    files[basename(files) == "genes_of_interest.tsv" & str_detect(files, regex(config$keywords, ignore_case = TRUE)) & !str_detect(files, "report")]
  } else {
    files[str_detect(files, patient)]
  }
  
  
  matched <- relevant_files[
    str_detect(relevant_files, config$extensions) &
      str_detect(relevant_files, regex(keyword_regex, ignore_case = TRUE))]
  
  
  if (!is.null(config$exclude)) {
    matched <- matched[!str_detect(matched, regex(config$exclude, ignore_case = TRUE))]
  }
  
  # BAM/BAI pairing
  if (!is.null(config$check_pair) && config$check_pair) {
    if (length(matched) == 0) {
      status <- if (config$required) "red" else "gray"
      reason <- if (config$required) "required_missing" else "optional_missing"
    } else {
      pair_type <- if (!is.null(config$pair)) config$pair else "bam_bai"
      pair_status <- check_pair(matched, patient, pair = pair_type)
      
      if (pair_status == "incomplete") {
        status <- "orange"
        reason <- if (pair_type == "pdf_tsv") "missing_pair_pdf_tsv" else "missing_pair_bam_bai"
      } else if (pair_status == "complete") {
        status <- "green"
        reason <- if (pair_type == "pdf_tsv") "complete_pair_pdf_tsv" else "complete_pair_bam_bai"
      } else {
        status <- if (config$required) "red" else "gray"
        reason <- if (config$required) "required_missing" else "optional_missing"
      }
    }
  } else {
    # For non-BAM files
    if (length(matched) == 0) {
      status <- if (config$required) "red" else "gray"
      reason <- if (config$required) "required_missing" else "optional_missing"
    } else if (length(matched) == 1) {
      status <- "green"
      reason <- "single_file_ok"
    } else if (length(matched) > 1 & config_key != "expression_expression"){
      status <- "orange"
      reason <- "multiple_files"
    } else if (length(matched) >= 1 & config_key == "expression_expression"){
      status <- "green"
      reason <- "files_ok"
    } else {
      status <- "grey"
      reason <- "unknown"
    }
  }
  
  return(list(status = status, files = matched, reason = reason))
}

get_dataset_columns <- function(dataset_type) {
  columns_config <- list(
    somatic = list(
      icon = list(name = "Status", width = 80),
      patient = list(name = "Patient", width = 120),
      variant = list(name = "Variant file", width = 80),
      tumor = list(name = "Tumor BAM file", width = 90),
      normal = list(name = "Normal BAM file", width = 90),
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
      goi = list(name = "Genes of Interest file", width = 120),
      files = list(name = "Detected files", minWidth = 400)
    )
  )
  
  return(columns_config[[dataset_type]])
}

#' @export
create_reactable <- function(data, dataset_type) {
  if (is.null(data)) return(NULL)
  
  # Získej konfiguraci sloupců
  columns_config <- get_dataset_columns(dataset_type)
  
  # Vytvoř seznam sloupců pro reactable
  columns_def <- list()
  
  for (col_name in names(columns_config)) {
    config <- columns_config[[col_name]]
    
    if (col_name == "icon" || col_name %in% c("variant", "tumor", "normal", "fusion", "chimeric", "arriba","expression", "goi")) {
      # Sloupce s ikonami
      columns_def[[col_name]] <- colDef(
        name = config$name,
        width = config$width,
        align = "center",
        html = TRUE,
        cell = function(value) value
      )
    } else if (col_name == "patient") {
      # Sloupec s pacienty
      columns_def[[col_name]] <- colDef(
        name = config$name,
        width = config$width,
        align = "center"
      )
    } else if (col_name == "files") {
      # Sloupec se soubory
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

#' @export
build_confirmed_paths <- function(dataset_objects) {
  result_rows <- list()
  
  for (dataset_name in names(dataset_objects)) {
    dataset_object      <- dataset_objects[[dataset_name]]
    patient_tab         <- dataset_object$patient_tab
    raw_results_list    <- dataset_object$raw_results
    dataset_root_path   <- dataset_object$root_path
    
    if (is.null(patient_tab) || nrow(patient_tab) == 0) next
    if (is.null(raw_results_list) || length(raw_results_list) != nrow(patient_tab)) next
    
    # Iterate patients (row-wise mapping into raw_results_list)
    for (row_index in seq_len(nrow(patient_tab))) {
      patient_id     <- patient_tab$patient[row_index]
      file_type_map  <- raw_results_list[[row_index]]  # e.g., list(variant=list(...), tumor=list(...), ...)
      
      if (is.null(file_type_map)) next
      for (file_type_name in names(file_type_map)) {
        file_paths <- file_type_map[[file_type_name]]$files
        if (length(file_paths) == 0) next
        
        # Normalize to absolute paths; works even if file_paths are already absolute
        absolute_paths <- ifelse(
          grepl("^/|^[A-Za-z]:", file_paths),
          file_paths,
          file.path(dataset_root_path, sub("^/+", "", file_paths))
        )
        
        result_rows[[length(result_rows) + 1]] <- data.frame(
          dataset   = dataset_name,
          patient   = patient_id,
          file_type = file_type_name,
          path      = absolute_paths,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(result_rows) == 0) {
    data.frame(
      dataset = character(), patient = character(),
      file_type = character(), path = character(),
      stringsAsFactors = FALSE
    )
  } else {
    combined_paths <- as.data.frame(rbindlist(result_rows, use.names = TRUE, fill = FALSE))
  }
}
