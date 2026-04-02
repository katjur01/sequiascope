box::use(
  shiny[NS,tagList,fileInput,moduleServer,observe,reactive,textOutput,updateTextInput,renderText,req,textInput,observeEvent,textAreaInput,column,fluidRow,reactiveVal,
        isTruthy,actionButton,icon,updateTextAreaInput,uiOutput,renderUI,bindEvent,fluidPage,radioButtons,verbatimTextOutput,renderPrint,showModal,modalDialog,invalidateLater],
  htmltools[tags,HTML,div,span,h2,h4,h5,br],
  shinyFiles[shinyDirButton,shinyDirChoose,parseDirPath,getVolumes],
  shinyWidgets[actionBttn,prettySwitch,updatePrettySwitch,pickerInput,updatePickerInput, dropdownButton,tooltipOptions,radioGroupButtons],
  bs4Dash[box,bs4Card],
  stringi[stri_detect_regex],
  stringr[str_detect,regex],
  shinyalert[shinyalert],
  reactable[reactable,reactableOutput,renderReactable,colDef],
  mirai[mirai, unresolved],
)

box::use(
  app/logic/helper_upload_data[create_dataset_data,create_reactable,validate_datasets_status,build_confirmed_paths, validate_all_columns, create_column_error_message, create_reference_files_data, create_reference_files_reactable],
  app/logic/waiter[show_waiter, hide_waiter]
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



  step2_server <- function(id, path, patients, datasets, tumor_pattern, normal_pattern, variant_pattern, tissues, step, shared_data) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      all_files <- reactiveVal()
      goi_files <- reactiveVal()
      TMB_files <- reactiveVal()
      confirmed_paths_state <- reactiveVal(NULL)
      # Trigger for manual evaluation (only on refresh or when step becomes 2)
      eval_trigger <- reactiveVal(0)
      # Async file scan state
      scan_result  <- reactiveVal(NULL)
      scan_pending <- reactiveVal(FALSE)
      scan_env     <- new.env(parent = emptyenv())
      
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
      
      # Async file scan — launched when entering step 2 or path changes
      # Uses Linux `find` via mirai subprocess so Shiny main thread is never blocked
      last_scanned_dirs <- reactiveVal(NULL)
      observe({
        req(step() == 2)
        p <- path()
        req(!is.null(p$projects) && length(p$projects) > 0)
        bam_dirs      <- if (!is.null(p$bams) && length(p$bams) > 0) p$bams else p$projects
        all_scan_dirs <- unique(c(p$projects, bam_dirs))
        # Skip re-scan if path hasn't changed (e.g. user went back to step1 to remove a
        # patient but kept the same folders — no need to run find again)
        if (identical(sort(all_scan_dirs), sort(last_scanned_dirs()))) return()
        last_scanned_dirs(all_scan_dirs)
        scan_result(NULL)
        scan_pending(TRUE)
        scan_env$task <- mirai({
          unique(unlist(lapply(dirs, function(d) {
            tryCatch(
              # -L: follow symlinks (handles NFS/K8s mounts exposed as symlinks)
              system2("find", c("-L", d, "-maxdepth", "5", "-type", "f"), stdout = TRUE),
              error = function(e) character(0)
            )
          })))
        }, dirs = all_scan_dirs)
      }) |> bindEvent(step(), path(), ignoreNULL = FALSE, ignoreInit = FALSE)

      # Polling: check if mirai scan finished (~every 500 ms)
      observe({
        req(scan_pending())
        invalidateLater(500)
        if (!is.null(scan_env$task) && !unresolved(scan_env$task)) {
          scan_result(scan_env$task$data)
          scan_pending(FALSE)
        }
      })

      # Derive all_files from scan_result (fast in-memory filter)
      observe({
        req(!is.null(scan_result()), length(patients()) > 0)
        p           <- path()
        patient_pattern <- paste(patients(), collapse = "|")
        all     <- scan_result()
        matches <- stri_detect_regex(all, patient_pattern)
        matched <- all[matches]

        # When a separate BAM folder is specified, exclude BAM/BAI files that come from
        # the project dirs.  Intermediate pipeline BAMs (realigned.bam, etc.) live inside
        # the project tree and would otherwise cause "multiple files found" for BAM rows.
        # The user-designated bam_dirs is the authoritative source for alignment files.
        has_separate_bams <- !is.null(p$bams) && length(p$bams) > 0
        if (has_separate_bams) {
          is_bam      <- str_detect(matched, regex("\\.(bam|bai)$", ignore_case = TRUE))
          from_bam_dir <- Reduce(`|`, lapply(p$bams, function(d) startsWith(matched, d)))
          matched <- matched[!is_bam | from_bam_dir]
        }

        all_files(matched)
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

      # Derive TMB_files from scan_result (fast in-memory filter)
      observe({
        req(!is.null(scan_result()), !is.null(path()$projects) && length(path()$projects) > 0)
        file_pattern    <- paste(c("mutation_loads", "TMB"), collapse = "|")
        patient_pattern <- paste(patients(), collapse = "|")
        all <- scan_result()
        matches <- stri_detect_regex(all, file_pattern) & !str_detect(all, regex(patient_pattern, ignore_case = TRUE))
        TMB_files(all[matches])
      })
      
      datasets_data <- reactive({
        req(datasets(), all_files(), patients())
        req(!is.null(path()$projects) && length(path()$projects) > 0)
        # Only evaluate when trigger changes (manual refresh or initial load)
        req(eval_trigger())
        # Use first project dir as the root_path metadata for dataset objects
        root_path_meta <- path()$projects[1]
        result <- list()
        for (dataset in datasets()) {
          result[[dataset]] <- create_dataset_data(dataset, all_files(), goi_files(), TMB_files(), patients(), root_path_meta, tumor_pattern, normal_pattern, variant_pattern, tissues())
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

      # Update output$dataset_boxes to use cached data:
      output$dataset_boxes <- renderUI({
        req(datasets())

        # Show spinner while async file scan is in progress
        if (isTRUE(scan_pending()) || is.null(scan_result())) {
          return(div(
            style = "display: flex; align-items: center; gap: 12px; padding: 24px 8px; color: #495057;",
            tags$i(class = "fa fa-spinner fa-spin", style = "font-size: 1.4em; color: #74c0fc;"),
            tags$span("Scanning files on storage, please wait…", style = "font-size: 0.95em;")
          ))
        }

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

      # Refresh button
      observeEvent(input$refresh_files, {
        req(!is.null(path()$projects) && length(path()$projects) > 0, patients())

        # Reload reference config from file
        shared_data$config_reload_trigger(shared_data$config_reload_trigger() + 1)

        # Reload GOI from config
        if (!is.null(shared_data$genes_of_interest_path())) {
          goi_files(shared_data$genes_of_interest_path())
        } else {
          goi_files(NULL)
        }

        # Re-launch async find scan — all_files and TMB_files update automatically when done
        p             <- path()
        bam_dirs      <- if (!is.null(p$bams) && length(p$bams) > 0) p$bams else p$projects
        all_scan_dirs <- unique(c(p$projects, bam_dirs))
        scan_result(NULL)
        scan_pending(TRUE)
        scan_env$task <- mirai({
          unique(unlist(lapply(dirs, function(d) {
            tryCatch(
              # -L: follow symlinks
              system2("find", c("-L", d, "-maxdepth", "5", "-type", "f"), stdout = TRUE),
              error = function(e) character(0)
            )
          })))
        }, dirs = all_scan_dirs)

        # Trigger evaluation (datasets_data will re-run once scan completes + all_files updates)
        eval_trigger(eval_trigger() + 1)
      })

      
      observeEvent(input$confirm, {
        req(datasets(), patients(), all_files())
        
        data <- datasets_data()
        ref_data <- reference_files_data()
        
      # ========== FAST CHECKS FIRST (no file I/O) =========

        # 1. Red dataset statuses — purely inspects already-computed HTML icons, instant
        validation <- validate_datasets_status(data)

        if (validation$has_red_status) {
          confirmed_paths_state(NULL)
          red_html_lines <- vapply(
            names(validation$red_patients_list),
            function(p) {
              ds <- paste(sort(unique(validation$red_patients_list[[p]])), collapse = ", ")
              sprintf("<li style='text-align: left'><b>%s</b> – %s</li>", p, ds)
            },
            character(1)
          )
          shinyalert(
            title = "Critical issues must be resolved before proceeding:",
            text = HTML(paste0("<ul>", paste(red_html_lines, collapse = ""), "</ul>")),
            type = "error",
            showConfirmButton = TRUE,
            confirmButtonText = "OK",
            html = TRUE
          )
          return()
        }

        # 2. Missing required reference file (kegg)
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

        # 3. Column validation — reads file headers, only reached if statuses are clean
        column_validation <- validate_all_columns(data)

        if (column_validation$has_errors) {
          confirmed_paths_state(NULL)

          error_html <- create_column_error_message(column_validation)
          scrollable_html <- paste0(
            "<div style='max-height:60vh; overflow-y:auto; padding-right:4px;'>",
            error_html,
            "</div>"
          )

          shinyalert(
            title = "Missing Required Columns",
            text = HTML(scrollable_html),
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
          
          # IGV will not run (missing BAM/BAI)
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
          
          # Arriba report will not be shown (missing PDF/TSV)
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
          
          # TMB section (kept)
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
          
          # ===== EXPRESSION TISSUE WARNINGS =====
          if (length(validation$missing_expression_tissues) > 0) {
            exp_lines <- vapply(names(validation$missing_expression_tissues), function(p) {
              tissues <- paste(validation$missing_expression_tissues[[p]], collapse = ", ")
              sprintf("<li style='text-align: left'><b>%s</b>: %s</li>", p, tissues)
            }, character(1))
            exp_html <- paste0(
              "<div style='text-align: center;'><strong>Some expression tissue files are missing and won't be used:</strong></div><br>",
              "<ul style='text-align: left; margin-left: 2em;'>",
              paste(exp_lines, collapse = ""),
              "</ul>"
            )
            warning_parts <- c(warning_parts, exp_html)
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
                confirmed_paths_state(NULL)  # force-invalidate so identical paths still re-trigger
                confirmed_paths_state(build_confirmed_paths(data, path()$projects[1]))
              } else {
                confirmed_paths_state(NULL)
              }
            }
          )
          return()
        }

        confirmed_paths_state(NULL)  # force-invalidate so identical paths still re-trigger
        confirmed_paths_state(build_confirmed_paths(data, path()$projects[1]))  # NO RED, NO ORANGE: pass data directly
        
            # Show waiter after confirming - will be hidden when summary/expression profile loads
        show_waiter("main-app", "Loading data and preparing modules...")
      })

      return(list(prev2 = reactive(input$prev2),
                  confirmed_paths = reactive(confirmed_paths_state())))  # dataset | patient | file_type | path
    })
  }
