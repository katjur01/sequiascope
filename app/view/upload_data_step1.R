# mod_step1.R

box::use(
  shiny[NS,tagList,fileInput,moduleServer,observe,reactive,textOutput,updateTextInput,renderText,req,textInput,observeEvent,textAreaInput,column,fluidRow,reactiveVal,
        isTruthy,actionButton,icon,updateTextAreaInput,uiOutput,renderUI,bindEvent,fluidPage,radioButtons,verbatimTextOutput,renderPrint,
        conditionalPanel,reactiveValues,showNotification],
  htmltools[tags,HTML,div,span,h2,h3,h4,h5,br],
  shinyFiles[shinyDirButton,shinyDirChoose,parseDirPath,getVolumes],
  shinyWidgets[actionBttn,prettySwitch,updatePrettySwitch, dropdown,tooltipOptions,virtualSelectInput,updateVirtualSelect],
  bs4Dash[box],
  stringi[stri_detect_regex],
  stringr[str_detect,regex],
  shinyalert[shinyalert],
)

# krok 1: cesta + pacienti
step1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$script(HTML(sprintf("$(document).on('change', '#%s', function() {
                                var selectedValues = $(this).val() || [];
                                Shiny.setInputValue('%s', selectedValues, {priority: 'event'});
                              });", ns("patient_list"), ns("patient_list_js")))),
    tags$head(tags$style(HTML(".folderInput-container{ display: flex; align-items: baseline; gap: 0px; }"))),
    tags$style(HTML(sprintf("#%s .card-title { float: none !important; }", ns("step1_box")))),
    box(id = ns("step1_box"), width = 12, headerBorder = FALSE, collapsible = FALSE,
      title = div(style = "display: flex; justify-content: space-between; align-items: stretch; float: none;",
      h3("Step 1: Select directory and patients", style = "display: inline-block; margin: 0;"),
      actionBttn(ns("load_session_btn"), "Load session", style = "jelly", icon = HTML('<i class="fa-solid fa-upload download-button"></i>'))),
    
        tags$label(h5("Select directory with patients data:")),
        div(class = "folderInput-container",
            shinyDirButton(ns("dir"), "Browse...", "Select directory with data", class = "btn btn-default"),
            textInput(ns("dir_path"), NULL, placeholder = "No directory selected", width = "100%")),
        tags$br(),
        div(style = "display: flex; align-items: baseline; gap: 20px;",
            column(6,
                   div(style = "display: flex; align-items: center; justify-content: space-between; gap: 10px;",
                       tags$label(h4("Patients"), style = "margin-bottom: 0;"),
                       dropdown(label = NULL,icon = HTML('<i class="fa-solid fa-plus download-button"></i>'),width = "300px",size = "sm",
                                textAreaInput(inputId = ns("new_patients"), label = "Enter patients:", placeholder = "e.g. P001, P002\nor\nP001\nP002",height= "110px"),
                                actionBttn(ns("confirm_add"), "Add new patients", size = "sm", style = "stretch", color = "success")
                       )), # search = TRUE
                   virtualSelectInput(ns("patient_list"), NULL, choices = character(0),hideClearButton = TRUE,keepAlwaysOpen = TRUE,multiple = TRUE,dropboxWrapper = "body")),
            column(6,
                   tags$label(h4("Select datasets for visualization:")),
                   div(style = "flex-direction: column; display: flex; align-items: baseline;",
                       prettySwitch(ns("somVariants_data"), label = "Somatic variant calling", status = "primary", slim = TRUE),
                       conditionalPanel(
                         condition = paste0("input['", ns("somVariants_data"), "'] == true"),
                         div(style = "display: flex; gap: 15px; padding-left: 40px;",
                             textAreaInput(inputId = ns("somatic_tumor_pattern"), label = "Pattern for tumor BAM files:", placeholder = "e.g. tumor, FFPE"),
                             textAreaInput(inputId = ns("somatic_normal_pattern"), label = "Pattern for normal BAM files:", placeholder = "e.g. normal, control"))
                       ),
                       prettySwitch(ns("germVariants_data"), label = "Germline variant calling", status = "primary", slim = TRUE),
                       conditionalPanel(
                         condition = paste0("input['", ns("germVariants_data"), "'] == true"),
                         div(style = "padding-left: 40px;",
                             textAreaInput(inputId = ns("germline_normal_pattern"), label = "Pattern for normal BAM files:", placeholder = "e.g. normal, control"))
                       ),
                       prettySwitch(ns("fusion_data"), label = "Fusion genes detection", status = "primary", slim = TRUE),
                       conditionalPanel(
                         condition = paste0("input['", ns("fusion_data"), "'] == true"),
                         div(style = "display: flex; gap: 15px; padding-left: 40px;",
                             textAreaInput(inputId = ns("fusion_tumor_pattern"), label = "Pattern for tumor BAM files:", placeholder = "e.g. tumor, fusion"),
                             textAreaInput(inputId = ns("fusion_chimeric_pattern"), label = "Pattern for chimeric BAM files:", placeholder = "e.g. chimeric"))
                       ),
                       prettySwitch(ns("expression_data"), label = "Expression profile", status = "primary", slim = TRUE),
                       conditionalPanel(
                         condition = paste0("input['", ns("expression_data"), "'] == true"),
                         div(style = "padding-left: 40px;",
                             textAreaInput(inputId = ns("tissue_list"), label = "Reference tissues (GTEx/HPA):", placeholder = "e.g. blood, liver")))
                       ))
        ),
        br(),
        div(style = "display: flex; justify-content: flex-end;",
            actionButton(ns("next1"), "Next"))
    )
  )
}

step1_server <- function(id, path, patients, datasets, tumor_pattern, normal_pattern, tissues) {
  moduleServer(id, function(input, output, session) {
    
    next1_btn <- reactiveVal(NULL)
    load_click <- reactiveVal(NULL)
    
    wd <- c(home = getwd())
    shinyDirChoose(input, "dir", roots = wd)
    
    observeEvent(input$dir, {
      chosen <- parseDirPath(wd, input$dir)
      path(chosen)  # 🌍 uložíme do globálního reactiveVal
      updateTextInput(session, "dir_path", value = chosen)
    })
    
    observeEvent(input$confirm_add, {
      if (is.null(input$new_patients) || trimws(input$new_patients) == "") {
        showNotification("Please enter patient names.", type = "warning", duration = 3)
        return()
      }
      
      # Rozdělení podle čárek NEBO nových řádků
      new <- unlist(strsplit(input$new_patients, "[,\n\r]+"))
      new <- trimws(new)
      new <- new[new != ""]
      new <- new[!is.na(new)]
      
      # Kontrola, zda zbyli nějací validní pacienti
      if (length(new) > 0) {
        all <- unique(c(patients(), new))
        updateVirtualSelect("patient_list", choices = all, selected = all, open = TRUE)
        updateTextAreaInput(session, "new_patients", value = "")
        patients(all)
        
        # Úspěšné přidání
        showNotification(
          paste("Added", length(new), "patient(s):", paste(new, collapse = ", ")), 
          type = "message", 
          duration = 3
        )
      } else {
        showNotification("No valid patients to add.", type = "warning", duration = 3)
        updateTextAreaInput(session, "new_patients", value = "")
      }
    })
    
    observe({
      selected <- c()
      if (isTRUE(input$somVariants_data))  selected <- c(selected, "somatic")
      if (isTRUE(input$germVariants_data)) selected <- c(selected, "germline")
      if (isTRUE(input$fusion_data))       selected <- c(selected, "fusion")
      if (isTRUE(input$expression_data))   selected <- c(selected, "expression")
      datasets(selected)
    })
    
    
    observeEvent(input$patient_list_js, {
      if (!is.null(input$patient_list_js)) {
        patients(input$patient_list_js)
      }
    }, ignoreNULL = FALSE)
    
    is_valid <- function() {
      path_ok <- !is.null(path()) && length(path()) > 0 && path() != ""
      patients_ok <- !is.null(patients()) && length(patients()) > 0
      datasets_ok <- !is.null(datasets()) && length(datasets()) > 0
      
      return(path_ok && patients_ok && datasets_ok)
    }
    
    
    
    observeEvent(input$next1, {
      
      if (is_valid()) {
        next1_btn(input$next1)
      } else {
        missing_items <- c()
        
        if (is.null(path()) || length(path()) == 0 || path() == "") {
          missing_items <- c(missing_items, "directory path")
        }
        
        if (is.null(patients()) || length(patients()) == 0) {
          missing_items <- c(missing_items, "patients")
        }
        
        if (is.null(datasets()) || length(datasets()) == 0) {
          missing_items <- c(missing_items, "datasets")
        }
        
        if (length(missing_items) == 1) {
          message_text <- paste("Please select", missing_items[1], "before proceeding to the next step.")
        } else if (length(missing_items) == 2) {
          message_text <- paste("Please select", paste(missing_items, collapse = " and "), "before proceeding to the next step.")
        } else {
          message_text <- paste("Please select", paste(missing_items[1:2], collapse = ", "), "and", missing_items[3], "before proceeding to the next step.")
        }
        
        shinyalert(
          title = "Missing Required Fields",
          text = message_text,
          type = "warning",
          showConfirmButton = TRUE,
          confirmButtonText = "OK",
          html = FALSE
        )
        next1_btn(NULL)
      }
    })
    
    ### patterns for bam files
    observeEvent(input$somatic_tumor_pattern, {
      tumor_pattern$somatic <- trimws(ifelse(isTruthy(input$somatic_tumor_pattern), input$somatic_tumor_pattern, ""))
    })
    
    observeEvent(input$somatic_normal_pattern, {
      normal_pattern$somatic <- trimws(ifelse(isTruthy(input$somatic_normal_pattern), input$somatic_normal_pattern, ""))
    })
    
    observeEvent(input$germline_normal_pattern, {
      normal_pattern$germline <- trimws(ifelse(isTruthy(input$germline_normal_pattern), input$germline_normal_pattern, ""))
    })
    
    observeEvent(input$fusion_tumor_pattern, {
      tumor_pattern$fusion <- trimws(ifelse(isTruthy(input$fusion_tumor_pattern), input$fusion_tumor_pattern, ""))
    })
    
    observeEvent(input$fusion_chimeric_pattern, {
      tumor_pattern$chimeric <- trimws(ifelse(isTruthy(input$fusion_chimeric_pattern), input$fusion_chimeric_pattern, ""))
    })
    
    observeEvent(input$tissue_list, {
      tissues(trimws(ifelse(isTruthy(input$tissue_list), input$tissue_list, "")))
    })

    observeEvent(input$load_session_btn, {
      # Klidně přidej confirm shinyalert tady; ale jednoduše stačí pulz:
      load_click(Sys.time())  # unikátní hodnota => pulse
    })
    
    nz <- function(x) if (is.null(x)) "" else x
    
    restore_ui_inputs <- function() {
      updateTextInput(session, "dir_path", value = nz(path()))
      p <- patients()
      updateVirtualSelect("patient_list", choices = p, selected = p, open = TRUE)
      
      dataset <- datasets()
      updatePrettySwitch(session, "somVariants_data",  value = "somatic"  %in% dataset)
      updatePrettySwitch(session, "germVariants_data", value = "germline" %in% dataset)
      updatePrettySwitch(session, "fusion_data",       value = "fusion"   %in% dataset)
      updatePrettySwitch(session, "expression_data",   value = "expression" %in% dataset)
      
      # patterns / tissues
      updateTextAreaInput(session, "somatic_tumor_pattern",   value = nz(tumor_pattern$somatic))
      updateTextAreaInput(session, "somatic_normal_pattern",  value = nz(normal_pattern$somatic))
      updateTextAreaInput(session, "germline_normal_pattern", value = nz(normal_pattern$germline))
      updateTextAreaInput(session, "fusion_tumor_pattern",    value = nz(tumor_pattern$fusion))
      updateTextAreaInput(session, "fusion_chimeric_pattern", value = nz(tumor_pattern$chimeric))
      updateTextAreaInput(session, "tissue_list",             value = nz(tissues()))
    }
    
    return(list(
      next1              = reactive(next1_btn(1)),
      load_request       = reactive(load_click()),
      restore_ui_inputs  = restore_ui_inputs
    ))
  })
}
