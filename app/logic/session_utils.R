# app/logic/session_utils.R
box::use(
  shiny[reactive,isolate,showNotification],
  jsonlite[read_json,write_json]
)

#' @export
safe_extract <- function(x) {
  if (is.list(x) && length(x) == 0) return(NULL)
  else return(x)
}

#' @export
create_session_handlers <- function(selected_inputs, filter_state) {
  
  get_session_data <- reactive({
    lapply(selected_inputs, function(x) x())
  })
  # names(get_session_data()) <- names(selected_inputs)
  
  restore_session_data <- function(data) {
    
    safe_extract <- function(x) {
      if (is.list(x) && length(x) == 0) return(NULL)
      else return(x)
    }
    
    for (nm in names(data)) {
      if (!is.null(data[[nm]]) && nm %in% names(selected_inputs)) {
        selected_inputs[[nm]](safe_extract(data[[nm]]))
      }
    }
    filter_state$restore_ui_inputs(data)
  }
  
  return(list(
    get_session_data = get_session_data,
    restore_session_data = restore_session_data
  ))
}

#' @export
load_session <- function(file = "session_data.json", patient_modules_list) {
  if (!file.exists(file)) {
    showNotification("Session file not found.", type = "error")
    return(NULL)
  }
  
  session_data <- read_json(file, simplifyVector = TRUE)
  
  invisible(lapply(names(session_data), function(module_type) {
    if (module_type %in% names(patient_modules_list)) {
      module_group <- patient_modules_list[[module_type]]
      module_data <- session_data[[module_type]]
      
      invisible(lapply(names(module_data), function(patient) {
        if (!is.null(module_group[[patient]])) {
          module_group[[patient]]$restore_session_data(module_data[[patient]])
        }
      }))
    }
  }))
  
  showNotification("Session loaded successfully.", type = "message")
}


#' @export
save_session <- function(file = "session_data.json", patient_modules_list) {
  session_data <- lapply(patient_modules_list, function(module_group) {
    lapply(module_group, function(mod) isolate(mod$get_session_data()))
  })
  
  write_json(session_data, path = file, pretty = TRUE, auto_unbox = TRUE, na = "null")
  showNotification(paste("Session saved to", file), type = "message")
}
