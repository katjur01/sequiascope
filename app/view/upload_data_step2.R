box::use(
  shiny[NS,tagList,fileInput,moduleServer,observe,reactive,textOutput,updateTextInput,renderText,req,textInput,observeEvent,textAreaInput,column,fluidRow,reactiveVal,
        isTruthy,actionButton,icon,updateTextAreaInput,uiOutput,renderUI,bindEvent,fluidPage,radioButtons,verbatimTextOutput,renderPrint,showModal,modalDialog],
  htmltools[tags,HTML,div,span,h2,h4,h5,br],
  shinyFiles[shinyDirButton,shinyDirChoose,parseDirPath,getVolumes],
  shinyWidgets[actionBttn,prettySwitch,updatePrettySwitch,pickerInput,updatePickerInput, dropdownButton,tooltipOptions,radioGroupButtons],
  bs4Dash[box,bs4Card],
  stringi[stri_detect_regex],
  stringr[str_detect,regex],
  shinyalert[shinyalert],
  reactable[reactable,reactableOutput,renderReactable,colDef],
)

box::use(
  app/logic/helper_upload_data[create_dataset_data,create_reactable,validate_datasets_status,build_confirmed_paths, validate_all_columns, create_column_error_message, create_reference_files_data, create_reference_files_reactable],
  app/logic/waiters[show_waiter, hide_waiter]
)

# DZ1601,MR1507,P001

step2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(sprintf("#%s .card-title { float: none !important; }", ns("step2_box")))),
    box(id = ns("step2_box"), width = 12, headerBorder = FALSE, collapsible = FALSE,
        title = div(style = "display: flex; justify-content: space-between; align-items: stretch; float: none;",
        h4("Step 2: Select data type", style = "display: inline-block; margin: 0;"),
        actionBttn(ns("refresh_files"), "Refresh", style = "jelly", icon = HTML('<i class="fa-solid fa-sync download-button"></i>'))),
      div(class = "collapsible-box",
          uiOutput(ns("dataset_boxes"))),
      br(),
      div(style = "display: flex; justify-content: flex-end;",
          actionButton(ns("prev2"), "Back"),
          actionButton(ns("confirm"), "Confirm"))
    )
  )
}



  step2_server <- function(id, path, patients, datasets, tumor_pattern, normal_pattern, tissues, step, shared_data) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      all_files <- reactiveVal()
      goi_files <- reactiveVal()
      TMB_files <- reactiveVal()
      confirmed_paths_state <- reactiveVal(NULL)
      # Trigger for manual evaluation (only on refresh or when step becomes 2)
      eval_trigger <- reactiveVal(0)
      
      # Reload config when returning to step 2 (e.g., after going back to step 1)
      observeEvent(step(), {
        if (step() == 2) {
          # Reload config from file
          shared_data$config_reload_trigger(shared_data$config_reload_trigger() + 1)
          # Also trigger re-evaluation of data
          eval_trigger(eval_trigger() + 1)
        }
      }, ignoreInit = FALSE)  # Run on init to load config first time
      
      # Initial load - trigger evaluation ONLY when step becomes 2
      # Initial load - trigger evaluation ONLY when step becomes 2
      observe({
        req(step() == 2)  # Only trigger when we're on step 2
        req(datasets(), all_files(), patients())
        # Trigger only once on initial load to step 2
        if (eval_trigger() == 0) {
          eval_trigger(1)
        }
      })
      
      # Load files ONLY when step 2 is active
      observe({
        req(step() == 2)  # Don't load files until step 2
        req(path(), patients())
        all_files_list <- list.files(path(), full.names = TRUE, recursive = TRUE)
        patient_pattern <- paste(patients(), collapse = "|")
        matches <- stri_detect_regex(all_files_list, patient_pattern) 
        all_files(all_files_list[matches])
      })
      
      # Load genes_of_interest from config if available
      observe({
        req(step() == 2)  # Don't load files until step 2
        
        # Pass path from config even if file doesn't exist
        # evaluate_file_status will check existence and return orange status if missing
        if (!is.null(shared_data$genes_of_interest_path())) {
          goi_files(shared_data$genes_of_interest_path())
        } else {
          goi_files(NULL)
        }
      })

      observe({
        req(step() == 2)  # Don't load files until step 2
        req(path(), patients())
        TMB_file_list <- list.files(path(), full.names = TRUE, recursive = TRUE)
        patient_pattern <- paste(patients(), collapse = "|")
        file_pattern <- paste(c("mutation_loads","TMB"), collapse = "|")
        matches <- stri_detect_regex(TMB_file_list, file_pattern) & !str_detect(TMB_file_list, regex(patient_pattern, ignore_case = TRUE))
        TMB_files(TMB_file_list[matches])
      })
      
      datasets_data <- reactive({
        req(datasets(), all_files(), patients(), path())
        # Only evaluate when trigger changes (manual refresh or initial load)
        req(eval_trigger())
        
        result <- list()
        for (dataset in datasets()) {
          result[[dataset]] <- create_dataset_data(dataset, all_files(), goi_files(), TMB_files(), patients(), path(), tumor_pattern, normal_pattern, tissues())
        }
        return(result)
      })
      
      # Reference files data
      reference_files_data <- reactive({
        req(datasets())
        # Depend on eval_trigger and config reload trigger
        req(eval_trigger())
        # Also depend on config paths to re-evaluate when config reloads
        kegg_path <- shared_data$kegg_tab_path()
        goi_path <- shared_data$genes_of_interest_path()
        report_path <- shared_data$report_template_path()
        
        create_reference_files_data(
          kegg_tab_path = kegg_path,
          goi_path = goi_path,
          report_template_path = report_path,
          datasets = datasets()
        )
      })

      # Upravte output$dataset_boxes aby používal cached data:
      output$dataset_boxes <- renderUI({
        req(datasets())
        
        data <- datasets_data()
        ref_data <- reference_files_data()
        
        tagList(
          # Tooltips are now handled globally by index.js
          # No need for local initialization
          
          # Patient datasets
          lapply(datasets(), function(dataset_name) {
            patient_tab   <- data[[dataset_name]]$patient_tab
            
            div(class = paste0("upload-", dataset_name, "-box"),
                bs4Card(
                  title       = h5(paste(dataset_name, "dataset")),
                  solidHeader = TRUE,
                  width       = 12,
                  collapsible = TRUE,
                  create_reactable(patient_tab, dataset_name)
                )
            )
          }),
          
          # Reference files table (always last, with gray background)
          div(class = "upload-reference-box",
              bs4Card(
                title       = h5("Reference files"),
                solidHeader = TRUE,
                width       = 12,
                collapsible = TRUE,
                create_reference_files_reactable(ref_data$reference_tab)
              )
          )
        )
      })

      # Refresh tlačítko
      observeEvent(input$refresh_files, {
        req(path(), patients())
        
        # Reload reference config from file
        shared_data$config_reload_trigger(shared_data$config_reload_trigger() + 1)
        
        # Znovu načti všechny soubory
        all_files_list <- list.files(path(), full.names = TRUE, recursive = TRUE)
        patient_pattern <- paste(patients(), collapse = "|")
        matches <- stri_detect_regex(all_files_list, patient_pattern) 
        all_files(all_files_list[matches])
        
        # Znovu načti goi soubor - POUZE z configu, žádný fallback
        if (!is.null(shared_data$genes_of_interest_path())) {
          goi_files(shared_data$genes_of_interest_path())
        } else {
          goi_files(NULL)
        }
        
        # Znovu načti TMB soubor
        file_pattern <- paste(c("mutation_loads","TMB"), collapse = "|")
        TMB_matches <- stri_detect_regex(all_files_list, file_pattern) & !str_detect(all_files_list, regex(patient_pattern, ignore_case = TRUE))
        TMB_files(all_files_list[TMB_matches])
        
        # Trigger evaluation
        eval_trigger(eval_trigger() + 1)
      })

      
      observeEvent(input$confirm, {
        req(datasets(), patients(), all_files())
        
        data <- datasets_data()
        ref_data <- reference_files_data()
        
      # ==========================================
      
        column_validation <- validate_all_columns(data)
        
        if (column_validation$has_errors) {
          confirmed_paths_state(NULL)
          
          error_html <- create_column_error_message(column_validation)
          
          shinyalert(
            title = "Missing Required Columns",
            text = HTML(error_html),
            type = "error",
            showConfirmButton = TRUE,
            confirmButtonText = "OK",
            html = TRUE
          )
          return()
        }
        
      # ========== REFERENCE FILES VALIDATION =========
      
        # Check if kegg_tab is red (required but missing)
        if (ref_data$kegg_status == "red") {
          confirmed_paths_state(NULL)
          
          shinyalert(
            title = "Critical reference file missing",
            text = HTML(paste0(
              "<div style='text-align: center;'><strong style='color: #d32f2f;'>Pathways file is required for your selected datasets.</strong></div><br>",
              "<div style='text-align: left; margin-left: 2em;'>The pathways file is necessary for:</div>",
              "<ul style='text-align: left; margin-left: 3em;'>",
              if ("somatic" %in% datasets()) "<li>Somatic variants analysis (Sankey plot)</li>" else "",
              if ("expression" %in% datasets()) "<li>Expression profile analysis</li>" else "",
              if ("expression" %in% datasets()) "<li>Network graph visualization</li>" else "",
              "</ul>",
              "<br><div style='text-align: center;'>Please configure the correct path in <code>reference_paths.json</code></div>"
            )),
            type = "error",
            showConfirmButton = TRUE,
            confirmButtonText = "OK",
            html = TRUE
          )
          return()
        }
        
      
      # ==========================================
      
        validation <- validate_datasets_status(data)

        # Pokud jsou červené stavy - blokovat postup
        if (validation$has_red_status) {
          confirmed_paths_state(NULL)
          # Připrav HTML seznam
          red_html_lines <- vapply(
            names(validation$red_patients_list),
            function(p) {
              datasets <- paste(sort(unique(validation$red_patients_list[[p]])), collapse = ", ")
              sprintf("<li style='text-align: left'><b>%s</b> – %s</li>", p, datasets)
            },
            character(1)
          )
          
          html_text <- paste0(
            "<div style='text-align: center;'><strong>Critical issues must be resolved before proceeding:</strong></div><br>",
            "<ul style='text-align: left; margin-left: 2em;'>",
            paste(red_html_lines, collapse = ""),
            "</ul>"
          )
          
          shinyalert(
            title = NULL,
            text = HTML(html_text),
            type = "error",
            showConfirmButton = TRUE,
            confirmButtonText = "OK",
            html = TRUE
          )
          return()
        }

        # Check for orange warnings (patient data or reference files)
        has_ref_warnings <- (ref_data$goi_status == "orange" || ref_data$report_status == "orange")
        
        if (validation$has_orange_status || has_ref_warnings) {
          warning_parts <- c()
          
          # IGV nepoběží (chybí BAM/BAI)
          if (length(validation$igv_issues) > 0) {
            igv_lines <- paste(sprintf("<li style='text-align: left'><b>%s</b></li>", validation$igv_issues), collapse = "")
            igv_html <- paste0(
              "<div style='text-align: center;'><strong>IGV snapshots will NOT be available (missing BAM/BAI) for:</strong></div><br>",
              "<ul style='text-align: left; margin-left: 2em;'>",
              igv_lines,
              "</ul>"
            )
            warning_parts <- c(warning_parts, igv_html)
          }
          
          # Arriba report nebude (chybí PDF/TSV)
          if (length(validation$arriba_issues) > 0) {
            arr_lines <- paste(sprintf("<li style='text-align: left'><b>%s</b></li>", validation$arriba_issues), collapse = "")
            arr_html <- paste0(
              "<div style='text-align: center;'><strong>Arriba report will NOT be shown (missing PDF/TSV) for:</strong></div><br>",
              "<ul style='text-align: left; margin-left: 2em;'>",
              arr_lines,
              "</ul>"
            )
            warning_parts <- c(warning_parts, arr_html)
          }
          
          # TMB část (ponecháno)
          if (length(validation$TMB_issues) > 0) {
            TMB_lines <- paste(sprintf("<li style='text-align: left'><b>%s</b></li>", validation$TMB_issues), collapse = "")
            TMB_html <- paste0(
              "<div style='text-align: center;'><strong>Tumor mutation burden information will be missing for:</strong></div><br>",
              "<ul style='text-align: left; margin-left: 2em;'>",
              TMB_lines,
              "</ul>"
            )
            warning_parts <- c(warning_parts, TMB_html)
          }
          
          # ===== REFERENCE FILES WARNINGS =====
          
          # GOI missing
          if (ref_data$goi_status == "orange") {
            goi_ref_html <- paste0(
              "<div style='text-align: center;'><strong>Genes of interest file is configured but not found</strong></div><br>",
              "<div style='text-align: center; color: #666;'>GOI analysis will be unavailable in Expression Profile module.</div>"
            )
            warning_parts <- c(warning_parts, goi_ref_html)
          }
          
          # Report template missing
          if (ref_data$report_status == "orange") {
            report_ref_html <- paste0(
              "<div style='text-align: center;'><strong>Report template is missing</strong></div><br>",
              "<div style='text-align: center; color: #666;'>Report download functionality will be unavailable for all datasets.</div>"
            )
            warning_parts <- c(warning_parts, report_ref_html)
          }
          
          warning_message <- paste0(paste(warning_parts, collapse = "<br>"), "<br>Do you want to continue anyway?")
          shinyalert(
            title = "Warnings Detected",
            text = HTML(warning_message),
            type = "warning",
            showConfirmButton = TRUE,
            showCancelButton = TRUE,
            confirmButtonText = "Continue Anyway",
            cancelButtonText = "Cancel",
            html = TRUE,
            callbackR = function(user_confirmed) {
              if (isTRUE(user_confirmed)) {
                confirmed_paths_state(build_confirmed_paths(data, path()))
              } else {
                confirmed_paths_state(NULL)
              }
            }
          )
          return()
        }

        confirmed_paths_state(build_confirmed_paths(data, path()))  # NO RED, NO ORANGE: pass data directly
        
        # Show waiter after confirming - will be hidden when summary/expression profile loads
        show_waiter("main-app", "Loading data and preparing modules...")
        
      })

      return(list(prev2 = reactive(input$prev2),
                  confirmed_paths = reactive(confirmed_paths_state())))  # dataset | patient | file_type | path
    })
  }
