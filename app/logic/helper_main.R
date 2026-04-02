# app/logic/helper_main.R

box::use(
  shiny[reactiveVal,removeTab,appendTab,NS,onFlushed,updateTabsetPanel,tabPanel, reactive, observe, observeEvent, renderUI, uiOutput, tagList, div, span, HTML, icon, tags, isTruthy],
  bs4Dash[bs4Card],
  stats[setNames],
)
box::use(
  app/view/create_report
)

#' @export
get_patients <- function(confirmed_paths, dataset_type) {
  if (is.null(confirmed_paths) || !nrow(confirmed_paths)) return(character(0))

  actual_dataset <- if (dataset_type == "network") "expression" else dataset_type
  
  rows <- subset(confirmed_paths, dataset == actual_dataset)
  if (!nrow(rows)) return(character(0))
  
  required_type <- switch(
    actual_dataset,
    "somatic"    = "variant",
    "germline"   = "variant",
    "fusion"     = "fusion",
    "expression" = "expression",
    "variant"
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
get_files_by_patient <- function(confirmed_paths, dataset_type) {

  actual_dataset <- if (dataset_type == "network") "expression" else dataset_type
  
  rows <- subset(confirmed_paths, dataset == actual_dataset)
  if (!nrow(rows)) return(list())
  
  by_patient <- split(rows, rows$patient)
  lapply(by_patient, function(tab) {
    if (actual_dataset == "expression") {
      list(files = split(tab$path, tab$file_type), tissues = setNames(tab$tissue, tab$path))
    } else {
      split(tab$path, tab$file_type)
    }
  })
}



#' @export
add_summary_boxes <- function(session,
                               output,
                               shared_data,
                               ui_output_id,
                               summary_module,        # object with $ui and $server (your summary module)
                               mounted_ref){          # reactiveValues with $mounted field (against duplicates); when NULL, created internally

  ns <- session$ns
  if (is.null(mounted_ref)) mounted_ref <- reactiveValues(mounted = character(0))
  
  patients_list <- reactive({ unique(c(shared_data$somatic.patients(),
                                       shared_data$germline.patients(),
                                       shared_data$fusion.patients(),
                                       shared_data$expression.patients())) })

  # 1) UI render – cards for all current patients
  output[[ui_output_id]] <- renderUI({
    pats <- patients_list()
    if (!length(pats)) return(div("No patients to display."))
    
    tagList(lapply(pats, function(sample) {
      # Check dataset availability for this patient
      dataset_availability <- list(
        somatic = sample %in% shared_data$somatic.patients(),
        germline = sample %in% shared_data$germline.patients(),
        fusion = sample %in% shared_data$fusion.patients(),
        expression = sample %in% shared_data$expression.patients()
      )
      
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
        summary_module$ui(ns(paste0("summary_table_", sample)), dataset_availability)
      )
    }))
  })

  # 2) Server mount – only for NEW patients
  observe({
    pats <- patients_list()
    already <- mounted_ref$mounted
    new_pats <- setdiff(pats, already)
    
    lapply(new_pats, function(sample) {
      # Check dataset availability for this patient
      dataset_availability <- list(
        somatic = sample %in% shared_data$somatic.patients(),
        germline = sample %in% shared_data$germline.patients(),
        fusion = sample %in% shared_data$fusion.patients(),
        expression = sample %in% shared_data$expression.patients()
      )
      
      summary_module$server(paste0("summary_table_", sample), sample, shared_data, dataset_availability)
      create_report$server(paste0("create_report_", sample), sample, shared_data)
    })
    
    if (length(new_pats)) {
      mounted_ref$mounted <- union(already, new_pats)
    }
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
                             patients,
                             shared_data,           # reactiveValues()
                             added_tab_values,      # reactiveValues(list per dataset)
                             tabset_input_id,       # e.g. "somatic_tabset" (plain, no ns())
                             tab_value_prefix,      # e.g. "som_"
                             module_obj,            # e.g. somatic_var_call_table / germline_var_call_table
                             load_session_btn = NULL){  # optional: reactive for load session button (e.g. input$load_session_btn) {          
  ns <- session$ns
  

  if (dataset_name != "network") set_dataset_patients(shared_data, dataset_name, patients)
  
  file_list <- get_files_by_patient(confirmed_paths, dataset_name)
  
  # Identify which patients are new (don't have tabs yet)
  old_vals <- added_tab_values[[dataset_name]]
  
  old_patient_ids <- if (length(old_vals)) {
    # Extract patient IDs from tab values (format: "{prefix}{patient_id}")
    gsub(paste0("^", tab_value_prefix), "", old_vals)
  } else {
    character(0)
  }
  
  # Identify patients to add and remove
  new_patient_ids <- setdiff(patients, old_patient_ids)
  removed_patient_ids <- setdiff(old_patient_ids, patients)
  
  # Remove tabs for patients that are no longer in the list
  if (length(removed_patient_ids) > 0) {
    removed_vals <- paste0(tab_value_prefix, removed_patient_ids)
    lapply(removed_vals, function(val) {
      removeTab(inputId = tabset_input_id, target = val)
    })
    # Update old_vals to exclude removed tabs
    old_vals <- setdiff(old_vals, removed_vals)
  }
  
  # Append tabs only for new patients and start their module servers
  new_vals <- character(0)
  invisible(lapply(new_patient_ids, function(patient_id) {
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
    
    if (identical(dataset_name, "network")) { # just for expression dataset
      tissues <- patient_files$tissues
      tissues <- unique(tissues)
      # For network graph we must keep "none" if that is the only tissue, because
      # networkGraph_cytoscape uses radioGroupButtons(choices = tissue_list) and
      # req(input$selected_tissue) — an empty choices vector would leave the input
      # NULL and block all data loading in the network module.
      real_tissues <- tissues[!is.na(tissues) & tissues != "none"]
      ui_args$tissue_list <- if (length(real_tissues) > 0) real_tissues else "none"
      ui_args$patient <- patient_id
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
    
    # Start server for this patient.
    # network: LAZY — mount only when user first navigates to that tab
    #          (network graph is heavy and purely optional).
    # All others: EAGER — must mount immediately so overview stats
    #          (somatic.overview, germline.overview, etc.) are computed
    #          and the Summary tab shows correct numbers.
    if (identical(dataset_name, "network")) {
      initialized <- reactiveVal(FALSE)
      local_tab_value  <- tab_value
      local_patient_id <- patient_id
      local_files      <- patient_files

      observeEvent(session$input[[tabset_input_id]], {
        current_tab <- session$input[[tabset_input_id]]
        if (!initialized() && !is.null(current_tab) && current_tab == local_tab_value) {
          message("\U0001f680 Lazy initializing network for patient: ", local_patient_id)
          module_obj$server(
            paste0(dataset_name, "_tab_", local_patient_id),
            local_patient_id,
            shared_data,
            local_files,
            file_list,
            tabset_input_id,
            local_tab_value)
          initialized(TRUE)
        }
      }, ignoreInit = TRUE)
    } else if (identical(dataset_name, "fusion") && !is.null(load_session_btn)) {
      module_obj$server(
        paste0(dataset_name, "_tab_", patient_id),
        patient_id,
        shared_data,
        patient_files,
        file_list,
        load_session_btn)
    } else {
      module_obj$server(
        paste0(dataset_name, "_tab_", patient_id),
        patient_id,
        shared_data,
        patient_files,
        file_list)
    }
    
    # Use <<- to modify parent scope variable from within lapply
    new_vals <<- c(new_vals, tab_value)
  }))
  # Append new tab values to existing ones (don't replace)
  added_tab_values[[dataset_name]] <- c(old_vals, new_vals)
  
  # 5) Select first patient tab after DOM is flushed
  if (length(patients)) {
    first_val <- paste0(tab_value_prefix, patients[[1]])
    session$onFlushed(function() {
      updateTabsetPanel(session, inputId = tabset_input_id, selected = first_val)
    }, once = TRUE)
  }
}


