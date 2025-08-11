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
  app/logic/helper_upload_data[create_dataset_data,create_reactable,validate_datasets_status,build_confirmed_paths]
)

# DZ1601,MR1507,P001

step2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 12, headerBorder = FALSE, collapsible = FALSE,
        title = div(style = "display: flex; justify-content: space-between; align-items: baseline;",
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



  step2_server <- function(id, path, patients, datasets, tumor_pattern, normal_pattern) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      all_files <- reactiveVal()
      goi_files <- reactiveVal()
      confirmed_paths_state <- reactiveVal(NULL)
      
      observe({
        req(path(), patients())
        all_files_list <- list.files(path(), full.names = TRUE, recursive = TRUE)
        patient_pattern <- paste(patients(), collapse = "|")
        matches <- stri_detect_regex(all_files_list, patient_pattern) 
        all_files(all_files_list[matches])
      })
      
      observe({
        req(path(), patients())
        goi_files_list <- list.files(path(), full.names = TRUE, recursive = TRUE)
        patient_pattern <- paste(patients(), collapse = "|") 
        matches <- stri_detect_regex(goi_files_list, "genes_of_interest") & !str_detect(goi_files_list, regex(patient_pattern, ignore_case = TRUE))
        goi_files(goi_files_list[matches])
      })

      datasets_data <- reactive({
        req(datasets(), all_files(), patients(), path())
        
        result <- list()
        for (dataset in datasets()) {
          result[[dataset]] <- create_dataset_data(dataset, all_files(), goi_files(), patients(), path(), tumor_pattern, normal_pattern)
        }
        return(result)
      })

      # Upravte output$dataset_boxes aby používal cached data:
      output$dataset_boxes <- renderUI({
        req(datasets())
        
        data <- datasets_data()
        
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
        })
      })

      # Refresh tlačítko
      observeEvent(input$refresh_files, {
        req(path(), patients())
        
        # Znovu načti všechny soubory
        all_files_list <- list.files(path(), full.names = TRUE, recursive = TRUE)
        patient_pattern <- paste(patients(), collapse = "|")
        matches <- stri_detect_regex(all_files_list, patient_pattern) 
        all_files(all_files_list[matches])
        
        # Znovu načti goi soubory
        goi_matches <- stri_detect_regex(all_files_list, "genes_of_interest") & !str_detect(all_files_list, regex(patient_pattern, ignore_case = TRUE))
        goi_files(all_files_list[goi_matches])
      })
      
      observeEvent(input$confirm, {
        req(datasets(), patients(), all_files())
        
        data <- datasets_data()
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
        
        
        # Pokud jsou oranžové stavy - zobrazit warning a zeptat se
        if (validation$has_orange_status) {
          warning_parts <- c()
          
          # IGV část
          if (length(validation$orange_patients_list) > 0) {
            igv_lines <- vapply(
              names(validation$orange_patients_list),
              function(p) {
                datasets <- paste(sort(unique(validation$orange_patients_list[[p]])), collapse = ", ")
                sprintf("<li style='text-align: left'><b>%s</b> – %s</li>", p, datasets)
              },
              character(1)
            )
            
            igv_html <- paste0(
              "<div style='text-align: center;'><strong>IGV tool will not be available for following patients:</strong></div><br>",
              "<ul style='text-align: left; margin-left: 2em;'>",
              paste(igv_lines, collapse = ""),
              "</ul>"
            )
            
            warning_parts <- c(warning_parts, igv_html)
          }
          
          # GOI část
          if (length(validation$goi_issues) > 0) {
            goi_lines <- paste(sprintf("<li style='text-align: left'><b>%s</b></li>", validation$goi_issues), collapse = "")
            
            goi_html <- paste0(
              "<div style='text-align: center;'><strong>Gene-of-interest analysis may be limited for following patients:</strong></div><br>",
              "<ul style='text-align: left; margin-left: 2em;'>",
              goi_lines,
              "</ul>"
            )
            
            warning_parts <- c(warning_parts, goi_html)
          }
          
          # Finální alert
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
              if (isTRUE(user_confirmed)) { # Build and store confirmed paths (allowed under ORANGE after user consent)
                confirmed_paths_state(build_confirmed_paths(data))
                showModal(modalDialog("Your selection has been confirmed!", easyClose = TRUE))
              } else { # User cancelled: do not pass anything
                confirmed_paths_state(NULL) 
              }})
          
          return()
        }
        

        confirmed_paths_state(build_confirmed_paths(data))  # NO RED, NO ORANGE: pass data directly
        # showModal(modalDialog("Your selection has been confirmed! All files are ready for analysis.", easyClose = TRUE))
        
      })

      return(list(prev2 = reactive(input$prev2),
                  confirmed_paths = reactive(confirmed_paths_state())))  # dataset | patient | file_type | path
    })
  }
