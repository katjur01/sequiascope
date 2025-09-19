# app/logic/helper_main.R

box::use(
  shiny[reactiveVal,removeTab,appendTab,NS,onFlushed,updateTabsetPanel,tabPanel, reactive, observe, renderUI, uiOutput, tagList, div, span, HTML, icon, tags],
  bs4Dash[bs4Card],
  stats[setNames],
)
box::use(
  app/view/create_report
)
#' @export
get_files_fo_all_patients <- function(){
  return(message("I dont know who called me but in this function is nothing."))
}
  
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

get_files_by_patient <- function(confirmed_paths, dataset_type) {
  rows <- subset(confirmed_paths, dataset == dataset_type)
  if (!nrow(rows)) return(list())
  
  by_patient <- split(rows, rows$patient)
  lapply(by_patient, function(tab) {
    if (dataset_type == "expression") {
      list(files = split(tab$path, tab$file_type), tissues = setNames(tab$tissue, tab$path))
    } else {
      split(tab$path, tab$file_type)
    }
  })
}





#' @export
add_summary_panels <- function(session,
                               output,
                               shared_data,
                               ui_output_id,
                               summary_module,        # objekt s $ui a $server (tvůj modul summary)
                               mounted_ref){          # reactiveValues s polem $mounted (proti duplicitám); když NULL, vytvoří se uvnitř

  ns <- session$ns
  if (is.null(mounted_ref)) mounted_ref <- reactiveValues(mounted = character(0))
  
  patients_list <- reactive({ unique(c(shared_data$somatic.patients(),
                                       shared_data$germline.patients(),
                                       shared_data$fusion.patients(),
                                       shared_data$expression.patients())) })

  # 1) UI render – karty pro všechny aktuální pacienty
  output[[ui_output_id]] <- renderUI({
    pats <- patients_list()
    if (!length(pats)) return(div("Žádní pacienti k zobrazení."))
    
    tagList(lapply(pats, function(sample) {
      bs4Card(
        title = tagList(
          tags$head(tags$style(HTML(
            ".card-title {float: none !important;} .card-title { font-size: 20px; }"
          ))),
          span(sample),
          div(style = "float: right; margin-left: auto;",
              create_report$ui(ns(paste0("create_report_", sample))))
        ),
        icon = icon("person"),
        collapsible = FALSE,
        width = 12,
        summary_module$ui(ns(paste0("summary_table_", sample)))
      )
    }))
  })

  # 2) Server mount – jen pro NOVÉ pacienty
  observe({
    pats <- patients_list()
    already <- mounted_ref$mounted
    new_pats <- setdiff(pats, already)
    
    lapply(new_pats, function(sample) {
      summary_module$server(paste0("summary_table_", sample), sample, shared_data)
      create_report$server(paste0("create_report_", sample), sample, shared_data)
    })
    
    if (length(new_pats)) mounted_ref$mounted <- union(already, new_pats)
  })
  
  invisible(NULL)
}

set_dataset_patients <- function(shared_data, dataset_name, patients) {
  var_name <- paste0(dataset_name, ".patients")  # "somatic.patients" | "germline.patients" | ...
  shared_data[[var_name]](patients)
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
  set_dataset_patients(shared_data, dataset_name, patients)
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
    id_ns <- ns(paste0(dataset_name, "_tab_", patient_id))
    
    ui_args <- list(id_ns)
    
    patient_files <- if (patient_id %in% names(file_list)) file_list[[patient_id]] else list()
    
    if (identical(dataset_name, "expression")) { # just for expression dataset
      tissues <- patient_files$tissues
      tissues <- unique(tissues)
      tissues <- tissues[!is.na(tissues) & tissues != "none"]
      ui_args$tissue_list <- tissues

      if (!is.null(patient_files$files$goi)) ui_args$goi <- TRUE else ui_args$goi <- FALSE
    }
    # Start UI for this patient
    appendTab(
      inputId = tabset_input_id,
      tab = tabPanel(
        title = patient_id,
        value = tab_value,
        do.call(module_obj$ui, ui_args)
      ),
      select = FALSE
    )
    # Start server for this patient
    if (identical(dataset_name, "fusion") && !is.null(load_session_btn)) { # just for fusion dataset
      module_obj$server(
        paste0(dataset_name, "_tab_", patient_id), 
        patient_id, 
        shared_data, 
        patient_files, 
        load_session_btn)
    } else {
      module_obj$server(paste0(dataset_name, "_tab_", patient_id),
                        patient_id, 
                        shared_data, 
                        patient_files)
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

