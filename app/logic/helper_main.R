# app/logic/helper_main.R

box::use(
  shiny[reactiveVal,removeTab,appendTab,NS,onFlushed,updateTabsetPanel,tabPanel],
)

#' @export
get_patients <- function(confirmed_paths, dataset_type) {
  if (is.null(confirmed_paths) || !nrow(confirmed_paths)) return(character(0))
  rows <- subset(confirmed_paths, dataset == dataset_type)
  if (!nrow(rows)) return(character(0))
  
  required_type <- switch(
    dataset_type,
    "somatic"    = "variant",
    "germline"   = "variant",
    "fusion"     = "fusion",
    "expression" = "expression",
    "variant"  # default (pro jistotu)
  )
  
  patients <- unique(rows$patient)
  keep <- vapply(
    patients,
    function(p) any(rows$patient == p & rows$file_type == required_type),
    logical(1)
  )
  sort(patients[keep])
}


#' @export
get_files_fo_all_patients <- function(confirmed_paths, dataset_type) {
  rows <- subset(confirmed_paths, dataset == dataset_type)
  if (!nrow(rows)) return(list())
  
  # split per patient → per file_type
  split_by_patient <- split(rows, rows$patient)
  
  files_map <- lapply(split_by_patient, function(tab) {
    by_type <- split(tab$path, tab$file_type)
    # necháme klíče "tumor","chimeric","fusion","arriba" atd.
    by_type
  })
  
  # pojisti, že vršek listu má jména pacientů
  if (is.null(names(files_map)) || any(!nzchar(names(files_map)))) {
    names(files_map) <- vapply(split_by_patient, function(df) df$patient[1], character(1))
  }
  
  files_map
}


# helper: per-patient file map (file_type -> character(paths))
#' @export
get_files_by_patient <- function(confirmed_paths, dataset_type) {
  som_rows <- subset(confirmed_paths, dataset == dataset_type)
  if (!nrow(som_rows)) return(list())
  by_patient <- split(som_rows, som_rows$patient)
  lapply(by_patient, function(tab) split(tab$path, tab$file_type))
}

#' @export
add_dataset_tabs <- function(session,
                             confirmed_paths,
                             dataset_name,          # "somatic" | "germline" | ...
                             shared_data,           # reactiveValues()
                             added_tab_values,      # reactiveValues(list per dataset)
                             tabset_input_id,       # e.g. "somatic_tabset" (plain, no ns())
                             tab_value_prefix,      # e.g. "som_"
                             module_obj,            # e.g. somatic_var_call_table / germline_var_call_table
                             load_session_btn = NULL){  # optional: reactive for load session button (e.g. input$load_session_btn) {          
  ns <- session$ns
  
  # 1) Derive patients and file map for this dataset
  patients  <- get_patients(confirmed_paths, dataset_name)
  file_list <- get_files_by_patient(confirmed_paths, dataset_name)
  
  # 2) Ensure shared_data field exists and is a reactiveVal, then update it
  pat_var <- paste0(dataset_name, ".patients")
  if (!isTRUE(is.function(shared_data[[pat_var]]))) {
    shared_data[[pat_var]] <- reactiveVal(character(0))
  }
  shared_data[[pat_var]](patients)
  # 3) Remove old tabs for this dataset (if any)
  old_vals <- added_tab_values[[dataset_name]]
  if (length(old_vals)) {
    lapply(old_vals, function(val) removeTab(inputId = tabset_input_id, target = val))
    added_tab_values[[dataset_name]] <- character(0)
  }

  # 4) Append tabs for all patients and start their module servers
  new_vals <- character(0)
  invisible(lapply(patients, function(patient_id) {
    tab_value <- paste0(tab_value_prefix, patient_id)

    appendTab(
      inputId = tabset_input_id,
      tab = tabPanel(
        title = patient_id,
        value = tab_value,
        module_obj$ui(ns(paste0(dataset_name, "_tab_", patient_id)))  # namespaced module UI id
      ),
      select = FALSE
    )
    
    # Collect files for this patient; default to empty list if missing
    patient_files <- if (patient_id %in% names(file_list)) file_list[[patient_id]] else list()
    
    # Start server for this patient
    if (identical(dataset_name, "fusion") && !is.null(load_session_btn)) { # just for fusion dataset
      module_obj$server(paste0(dataset_name, "_tab_", patient_id), patient_id, shared_data, patient_files, load_session_btn)
    } else {
      module_obj$server(paste0(dataset_name, "_tab_", patient_id), patient_id, shared_data, patient_files)
    }
    
    new_vals <- c(new_vals, tab_value)
  }))
  added_tab_values[[dataset_name]] <- new_vals
  
  # 5) Select first patient tab after DOM is flushed
  if (length(patients)) {
    first_val <- paste0(tab_value_prefix, patients[[1]])
    session$onFlushed(function() {
      updateTabsetPanel(session, inputId = tabset_input_id, selected = first_val)
    }, once = TRUE)
  }
}

