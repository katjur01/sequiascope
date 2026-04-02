# mod_step1.R

box::use(
  shiny[NS,tagList,fileInput,moduleServer,observe,reactive,req,observeEvent,textAreaInput,column,fluidRow,reactiveVal,
        isTruthy,actionButton,icon,updateTextAreaInput,uiOutput,renderUI,fluidPage,
        conditionalPanel,reactiveValues,showNotification],
  htmltools[tags,HTML,div,span,h2,h3,h4,h5,br],
  shinyWidgets[actionBttn,prettySwitch,updatePrettySwitch, dropdown,tooltipOptions,virtualSelectInput,updateVirtualSelect,pickerInput,pickerOptions,updatePickerInput],
  bs4Dash[box],
  stringi[stri_detect_regex],
  stringr[str_detect,regex],
  shinyalert[shinyalert],
  shinyjs[useShinyjs,runjs],
  stats[setNames],
  app/logic/helper_igv[get_igv_genome_display_name, get_igv_genome_id],
  app/view/lazy_browser
)

# krok 1: cesta + pacienti
step1_ui <- function(id) {
  ns <- NS(id)
  useShinyjs()
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
    
        # ── Projects + BAMs ────────────────────────────────────────────────────
        fluidRow(
            column(6,
                   tags$label(h5("Select project folder(s):")),
                   div(style = "display: flex; gap: 8px; align-items: flex-start;",
                       actionButton(ns("toggle_proj_browser"), "Browse...",
                                    class = "btn btn-default btn-sm",
                                    style = "border-radius: 2px; white-space: nowrap;"),
                       lazy_browser$ui(ns("proj_browser"))
                   ),
                   uiOutput(ns("selected_projects_ui"))
            ),
            column(6,
                   tags$label(h5("BAM files location (optional):")),
                   tags$small(style = "color: #868e96; display: block; margin-bottom: 6px;",
                              "Leave empty if BAM files are in the same folders as projects."),
                   div(style = "display: flex; gap: 8px; align-items: flex-start;",
                       actionButton(ns("toggle_bam_browser"), "Browse...",
                                    class = "btn btn-default btn-sm",
                                    style = "border-radius: 2px; white-space: nowrap;"),
                       lazy_browser$ui(ns("bam_browser"))
                   ),
                   uiOutput(ns("selected_bams_ui"))
            )
        ),
        div(style = "display: flex; align-items: baseline; gap: 20px;",
            column(6,
                   div(style = "display: flex; align-items: center; justify-content: space-between; gap: 10px;",
                       tags$label(h4("Patients"), style = "margin-bottom: 0;"),
                       dropdown(label = NULL,icon = HTML('<i class="fa-solid fa-plus download-button"></i>'),width = "300px",size = "sm",
                                textAreaInput(inputId = ns("new_patients"), label = "Enter patients:", placeholder = "e.g. DZ1601, P002\nor\nDZ1601\nP002",height= "110px"),
                                div(style = "display: flex; justify-content: center; margin-top: 10px;",
                                    actionBttn(ns("confirm_add"), "Add new patients", size = "sm", style = "stretch", color = "success"))
                       )),
                   virtualSelectInput(ns("patient_list"), NULL, choices = character(0),hideClearButton = TRUE,keepAlwaysOpen = TRUE,multiple = TRUE,dropboxWrapper = "body")),
            column(6,
                   tags$label(h4("Select datasets for visualization:")),
                   div(style = "flex-direction: column; display: flex; align-items: baseline;",
                       div(style = "display: flex; align-items: center;",
                           prettySwitch(ns("somVariants_data"), label = "Somatic variant calling", status = "primary", slim = TRUE)),
                       conditionalPanel(
                         condition = paste0("input['", ns("somVariants_data"), "'] == true"),
                         div(style = "display: flex; gap: 15px; padding-left: 40px;",
                             textAreaInput(inputId = ns("somatic_variant_pattern"), label = "Pattern for variant file:",       placeholder = "e.g. variants or somatic"),
                             textAreaInput(inputId = ns("somatic_tumor_pattern"),   label = "Pattern for tumor BAM files:",   placeholder = "e.g. tumor or FFPE"),
                             textAreaInput(inputId = ns("somatic_normal_pattern"),  label = "Pattern for normal BAM files:",  placeholder = "e.g. normal or control"))
                       ),
                       div(style = "display: flex; align-items: center;",
                           prettySwitch(ns("germVariants_data"), label = "Germline variant calling", status = "primary", slim = TRUE)),
                       conditionalPanel(
                         condition = paste0("input['", ns("germVariants_data"), "'] == true"),
                         div(style = "display: flex; gap: 15px; padding-left: 40px;",
                             textAreaInput(inputId = ns("germline_variant_pattern"), label = "Pattern for variant file:",     placeholder = "e.g. variants or germline"),
                             textAreaInput(inputId = ns("germline_normal_pattern"),  label = "Pattern for normal BAM files:", placeholder = "e.g. normal or control"))
                       ),
                       div(style = "display: flex; align-items: center;",
                           prettySwitch(ns("fusion_data"), label = "Fusion genes detection", status = "primary", slim = TRUE)),
                       conditionalPanel(
                         condition = paste0("input['", ns("fusion_data"), "'] == true"),
                         div(style = "display: flex; flex-wrap: wrap; gap: 15px; padding-left: 40px; align-items: flex-start;",
                             # BAM pattern inputs stay together as a pair and never split
                             div(style = "display: flex; flex: 2 1 400px; gap: 15px;",
                                 div(style = "flex: 1;", textAreaInput(inputId = ns("fusion_variant_pattern"),  label = "Pattern for fusion table file:",   placeholder = "e.g. fusion")),
                                 div(style = "flex: 1;", textAreaInput(inputId = ns("fusion_tumor_pattern"),    label = "Pattern for tumor BAM files:",    placeholder = "e.g. tumor or fusion")),
                                 div(style = "flex: 1;", textAreaInput(inputId = ns("fusion_chimeric_pattern"), label = "Pattern for chimeric BAM files:",  placeholder = "e.g. chimeric"))
                             ),
                             # Genome picker wraps to a new line on narrower screens
                             div(style = "flex: 1 1 220px; min-width: 200px;",
                                 pickerInput(inputId = ns("igv_snapshot"), label = "Genome for IGV snapshot:",
                                   choices = c("GRCh38/hg38","hg38 1kg/GATK","GRCh37/hg19","T2T CHM13-v2.0/hs1","Custom","Dont create IGV snapshots"),
                                   choicesOpt = list(icon = c("fa-check","fa-check","fa-check","fa-check","fa-sliders","fa-xmark")),
                                   options = pickerOptions(container = "body", iconBase = "fas"), width = "100%"))
                         )
                       ),
                       div(style = "display: flex; align-items: center;",
                           prettySwitch(ns("expression_data"), label = "Expression profile", status = "primary", slim = TRUE)),
                       conditionalPanel(
                         condition = paste0("input['", ns("expression_data"), "'] == true"),
                         div(style = "padding-left: 40px;",
                             textAreaInput(inputId = ns("tissue_list"), label = "Reference tissues (GTEx/HPA):", placeholder = "e.g. blood, liver, lung")))
                       ))
        ),
        br(),
        div(style = "display: flex; justify-content: flex-end;",
            actionButton(ns("next1"), "Next"))
    )
  )
}

step1_server <- function(id, path, patients, datasets, tumor_pattern, normal_pattern, variant_pattern, tissues, shared_data) {
  moduleServer(id, function(input, output, session) {
    #ns <- NS(id)
    next1_btn <- reactiveVal(NULL)
    load_click <- reactiveVal(NULL)

    # ── Browser root detection ────────────────────────────────────────────────
    browser_root <- if (dir.exists("/input_files")) "/input_files"
                    else if (dir.exists(file.path(getwd(), "input_files"))) file.path(getwd(), "input_files")
                    else getwd()

    # ── Project browser (checkboxes mode) ────────────────────────────────────
    proj_browser <- lazy_browser$server(
      "proj_browser",
      root         = browser_root,
      mode         = "checkboxes",
      open_trigger = reactive(input$toggle_proj_browser)
    )

    # ── BAM browser (checkboxes mode, optional) ──────────────────────────────
    bam_browser <- lazy_browser$server(
      "bam_browser",
      root         = browser_root,
      mode         = "checkboxes",
      open_trigger = reactive(input$toggle_bam_browser)
    )

    # Mirror project browser selection directly into path()
    observeEvent(proj_browser$selected_dirs(), {
      dirs <- proj_browser$selected_dirs()
      path(list(projects = dirs, bams = path()$bams))
      if (length(dirs) > 0) {
        shared_data$data_path(dirs[1])
        shared_data$projects_path(dirs)
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Mirror BAM browser selection directly into path()
    observeEvent(bam_browser$selected_dirs(), {
      dirs <- bam_browser$selected_dirs()
      path(list(projects = path()$projects, bams = dirs))
      if (length(dirs) > 0) shared_data$bam_path(dirs[1])
      else shared_data$bam_path(NULL)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Show currently selected BAM folders as a plain list
    output$selected_bams_ui <- renderUI({
      bams <- path()$bams
      if (is.null(bams) || length(bams) == 0) {
        return(tags$div(
          style = "color: #868e96; font-size: 0.85em; font-style: italic; margin: 4px 0 8px;",
          "No BAM folders selected yet."
        ))
      }
      tagList(
        tags$div(style = "font-size: 0.85em; font-weight: 600; margin: 6px 0 2px;",
                 sprintf("Selected BAM folder%s:", if (length(bams) > 1) "s" else "")),
        tags$ul(
          style = "margin: 0 0 6px 0; padding-left: 18px; font-size: 0.85em;",
          lapply(bams, function(p)
            tags$li(tags$i(class = "fa fa-folder", style = "color: #f59f00; margin-right: 5px;"),
                    basename(p),
                    title = p))
        )
      )
    })

    # Show currently selected project folders as a plain list
    output$selected_projects_ui <- renderUI({
      projs <- path()$projects
      if (is.null(projs) || length(projs) == 0) {
        return(tags$div(
          style = "color: #868e96; font-size: 0.85em; font-style: italic; margin: 4px 0 8px;",
          "No project folders selected yet."
        ))
      }
      tagList(
        tags$div(style = "font-size: 0.85em; font-weight: 600; margin: 6px 0 2px;",
                 sprintf("Selected project folder%s:", if (length(projs) > 1) "s" else "")),
        tags$ul(
          style = "margin: 0 0 6px 0; padding-left: 18px; font-size: 0.85em;",
          lapply(projs, function(p)
            tags$li(tags$i(class = "fa fa-folder", style = "color: #f59f00; margin-right: 5px;"),
                    basename(p),
                    title = p))
        )
      )
    })
    
    observeEvent(input$confirm_add, {
      if (is.null(input$new_patients) || trimws(input$new_patients) == "") {
        showNotification("Please enter patient names.", type = "warning", duration = 3)
        return()
      }
      
      # Split by commas OR newlines
      new <- unlist(strsplit(input$new_patients, "[,\n\r]+"))
      new <- trimws(new)
      new <- new[new != ""]
      new <- new[!is.na(new)]
      
      # Check if any valid patients remain
      if (length(new) > 0) {
        all <- unique(c(patients(), new))
        updateVirtualSelect("patient_list", choices = all, selected = all, open = TRUE)
        updateTextAreaInput(session, "new_patients", value = "")
        patients(all)
        
        # Successful addition
        showNotification(
          paste("Added", length(new), "patient(s):", paste(new, collapse = ", ")), 
          type = "message", 
          duration = 3
        )
        
        # Close dropdown after successful add
     #   runjs("$('#" + ns("confirm_add") + "').closest('.dropdown').find('[data-toggle=dropdown]').dropdown('hide');")
      } else {
        showNotification("No valid patients to add.", type = "warning", duration = 3)
        updateTextAreaInput(session, "new_patients", value = "")
        
        # Close dropdown even if no valid patients
    #    runjs("$('#" + ns("confirm_add") + "').closest('.dropdown').find('[data-toggle=dropdown]').dropdown('hide');")
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
      proj     <- path()$projects
      path_ok     <- !is.null(proj) && length(proj) > 0
      patients_ok <- !is.null(patients()) && length(patients()) > 0
      datasets_ok <- !is.null(datasets()) && length(datasets()) > 0
      return(path_ok && patients_ok && datasets_ok)
    }
    
    
    
    observeEvent(input$next1, {
      
      if (is_valid()) {
        # Save IGV genome selection to shared_data (convert display name → igv_id)
        igv_selection <- input$igv_snapshot
        if (!is.null(igv_selection)) {
          igv_id <- get_igv_genome_id(igv_selection, shared_data$custom_genome_config)
          shared_data$igv_genome(igv_id)
          message("[UPLOAD] IGV genome selected: ", igv_selection, " -> ", igv_id)
        } else {
          shared_data$igv_genome("hg38")  # Default
        }
        
        next1_btn(input$next1)
      } else {
        missing_items <- c()
        
        proj <- path()$projects
        if (is.null(proj) || length(proj) == 0) {
          missing_items <- c(missing_items, "project folder(s)")
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

    observeEvent(input$somatic_variant_pattern, {
      variant_pattern$somatic <- trimws(ifelse(isTruthy(input$somatic_variant_pattern), input$somatic_variant_pattern, ""))
    })

    observeEvent(input$germline_variant_pattern, {
      variant_pattern$germline <- trimws(ifelse(isTruthy(input$germline_variant_pattern), input$germline_variant_pattern, ""))
    })

    observeEvent(input$fusion_variant_pattern, {
      variant_pattern$fusion <- trimws(ifelse(isTruthy(input$fusion_variant_pattern), input$fusion_variant_pattern, ""))
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
      # path()$projects and path()$bams are shown via reactive uiOutputs — no manual text update needed
      p <- patients()
      message("🔄 [restore_ui_inputs] Restoring patients: ", paste(p, collapse = ", "))
      
      # Update virtualSelect with existing patients
      updateVirtualSelect(session = session, inputId = "patient_list", choices = p, selected = p)
      
      # Clear new_patients field (it's for adding NEW patients, not showing existing ones)
      updateTextAreaInput(session, "new_patients", value = "")
      
      message("✅ [restore_ui_inputs] VirtualSelect updated, TextArea cleared")
      
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
      updateTextAreaInput(session, "somatic_variant_pattern", value = nz(variant_pattern$somatic))
      updateTextAreaInput(session, "germline_variant_pattern",value = nz(variant_pattern$germline))
      updateTextAreaInput(session, "fusion_variant_pattern",  value = nz(variant_pattern$fusion))
      updateTextAreaInput(session, "tissue_list",             value = nz(tissues()))
      
      # Restore IGV genome selection picker (convert igv_id → display name)
      igv_genome_val <- shared_data$igv_genome()
      if (!is.null(igv_genome_val) && length(igv_genome_val) > 0) {
        selected_display <- get_igv_genome_display_name(igv_genome_val, shared_data$custom_genome_config)
        updatePickerInput(session, "igv_snapshot", selected = selected_display)
        message("🔄 [restore_ui_inputs] Restored IGV genome picker: ", igv_genome_val, " -> ", selected_display)
      }
    }
    
    return(list(
      next1              = reactive(next1_btn()),
      load_request       = reactive(load_click()),
      restore_ui_inputs  = restore_ui_inputs
    ))
  })
}
