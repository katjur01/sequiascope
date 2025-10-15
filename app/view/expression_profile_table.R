# app/view/expression_profile_table.R

box::use(
  shiny[moduleServer, NS, tagList, fluidRow, fluidPage, column, tabPanel, reactive, req, observe, div, observeEvent, reactiveVal, icon, splitLayout, h4, bindEvent,conditionalPanel,isTruthy,
        updateSelectInput, selectInput, numericInput, actionButton, renderPlot, plotOutput, uiOutput, renderUI, verbatimTextOutput, renderPrint, reactiveValues, isolate,downloadButton],
  reactable,
  bs4Dash[box,tabBox],
  reactable[colDef, reactableOutput, renderReactable, reactable, getReactableState, colGroup, JS],
  htmltools[tags,HTML,h6,h5],
  plotly[plotlyOutput, renderPlotly, toWebGL],
  shinyWidgets[radioGroupButtons, checkboxGroupButtons, updateCheckboxGroupButtons,prettyCheckboxGroup, updatePrettyCheckboxGroup, dropdown, dropdownButton, actionBttn,
               awesomeCheckboxGroup, pickerInput, updatePickerInput],
  data.table[rbindlist, dcast.data.table, as.data.table, melt.data.table, copy],
  grDevices[colorRampPalette],
  pheatmap[pheatmap],
  stats[setNames],
  magrittr[`%>%`],
  shinyalert[shinyalert,useShinyalert],
  shinyjs[useShinyjs,hide,show],
  data.table[data.table,as.data.table,is.data.table,uniqueN]
)

box::use(
  app/logic/plots[prepare_barPlot_data, create_barPlot, prepare_volcano, volcanoPlot, ggvolcanoPlot, classify_volcano_genes],
  app/logic/waiters[use_spinner],
  app/logic/load_data[get_inputs, load_data],
  app/logic/reactable_helpers[custom_colGroup_setting],
  app/logic/filter_columns[map_checkbox_names,colnames_map_list,generate_columnsDef],
  app/logic/prepare_table[prepare_expression_table, prepare_goi_table, set_pathway_colors],
  app/logic/networkGraph_helper[get_pathway_list],
  app/logic/session_utils[create_session_handlers, register_module, safe_extract, nz, ch]
)

ui <- function(id, tissue_list, goi = FALSE) {
  ns <- NS(id)
  useShinyjs()
  
  tabs <- list()
  
  # GOI will be created only when goi = TRUE
  if (isTRUE(goi)) {
    tabs <- c(tabs, list(
      tabPanel(title = "Genes of Interest", value = "genesOfinterest",
               tagList(tags$head(tags$style(HTML("#download_btn { background-color: transparent; border: 1px solid transparent; margin-top: -1px; }
                                            #download_btn:hover { box-shadow: 0 5px 11px 0 rgba(0, 0, 0, .18), 0 4px 15px 0 rgba(0, 0, 0, .15); }"))),
                       fluidRow(
                         div(style = "width: 100%; text-align: right; display: flex; flex-direction: row-reverse;",
                             dropdownButton(inputId = ns("download_btn"),label = NULL, right = TRUE, width = "240px",icon = HTML('<i class="fa-solid fa-download download-button"></i>'),
                                            selectInput(ns("export_data_table_goi"), "Select data:", choices = c("All data" = "all", "Filtered data" = "filtered")),
                                            selectInput(ns("export_format_table_goi"), "Select format:", choices = c("CSV" = "csv", "TSV" = "tsv", "Excel" = "xlsx")),
                                            downloadButton(ns("Table_download_goi"), "Download")),
                             filterTab_ui(ns("filterTab_dropdown_goi"), tissue_list))
                       )),
               use_spinner(reactableOutput(ns("goi_expression_table"))),
               div(
                 tags$br(),
                 actionButton(ns("selectDeregulated_button_goi"), "Select deregulated genes for report", status = "info"),
                 tags$br(),
                 fluidRow(column(8, reactableOutput(ns("selectDeregulated_tab_goi")))),
                 tags$br(),
                 fluidRow(column(3, actionButton(ns("delete_button_goi"), "Delete genes", icon = icon("trash-can"))))),
               tags$br(),
               plot_ui(ns("plot_goi"))
      )
      
    ))
  }
  
  # All Genes tab - always available
  tabs <- c(tabs, list(
    tabPanel(title = "All Genes", value = "allGenes",
             fluidRow(
               div(style = "width: 100%; text-align: right;",
                   dropdownButton(label = NULL, right = TRUE, width = "240px",icon = HTML('<i class="fa-solid fa-download download-button"></i>'),
                                  selectInput(ns("export_data_table"), "Select data:", choices = c("All data" = "all", "Filtered data" = "filtered")),
                                  selectInput(ns("export_format_table"), "Select format:", choices = c("CSV" = "csv", "TSV" = "tsv", "Excel" = "xlsx")),
                                  downloadButton(ns("Table_download"), "Download")),
                   filterTab_ui(ns("filterTab_dropdown"), tissue_list))
             ),
             use_spinner(reactableOutput(ns("expression_table"))),
             div(
               tags$br(),
               actionButton(ns("selectDeregulated_button"), "Select deregulated genes for report", status = "info"),
               tags$br(),
               fluidRow(column(8, reactableOutput(ns("selectDeregulated_tab")))),
               tags$br(),
               fluidRow(column(3, actionButton(ns("delete_button"), "Delete genes", icon = icon("trash-can"))))),
             tags$br(),
             plot_ui(ns("plot"))
    )
  ))
  
  
  tabbox_id <- if (isTRUE(goi)) "expression_profile_tabs_goi" else "expression_profile_tabs_allGenes"
  
  do.call(tabBox, c(list(id = ns(tabbox_id), width = 12, collapsible = FALSE, selected = "genesOfinterest", headerBorder = FALSE), tabs))
}

server <- function(id, patient, shared_data, patient_files, file_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      req(tissue_list)
      overview_dt <- data.table(tissues = unique(tissue_list))
      shared_data$expression.overview[[ patient ]] <- overview_dt
    })
    
    prepare_data <- reactive({
      data <- load_data(patient_files, "expression", patient)
      prepare_expression_table(data)
    })
    
    has_goi <- reactive({
      tryCatch({
        files <- patient_files$files$goi
        result <- isTruthy(files) &&
          length(unlist(files)) > 0 &&
          all(file.exists(unlist(files)))
        return(result)
      }, error = function(e) {
        return(FALSE)
      })
    })
    
    prepare_goi_dt <- reactive({
      req(prepare_data())
      if (!has_goi()) return(NULL)
      
      tryCatch({
        goi_data <- prepare_goi_table(prepare_data()$dt, patient_files$files$goi)
        return(goi_data)
      }, error = function(e) {
        return(NULL)
      })
    })
    
    data <- reactive(prepare_data()$dt)
    tissue_list <- prepare_data()$tissues
    colnames_list <- prepare_data()$columns
    
    # 1. ALL GENES
    all_genes_logic <- create_expression_logic(
      session = session,
      ns = ns,
      data = data,
      tissue_list = tissue_list,
      colnames_list = colnames_list,
      patient = patient,
      expression_var = shared_data$expression.variants.all,
      pathway_list = get_pathway_list("all_genes"),
      expr_tag = "all_genes",
      suffix = ""
    )
    
    # 2. GOI
    goi_logic <- NULL
    if (has_goi() && !is.null(prepare_goi_dt())) {
      goi_logic <- create_expression_logic(
        session = session,
        ns = ns,
        data = prepare_goi_dt,  # <-- TADY: GOI data
        tissue_list = tissue_list,
        colnames_list = colnames_list,
        patient = patient,
        expression_var = shared_data$expression.variants.goi,
        pathway_list = get_pathway_list("genes_of_interest",prepare_goi_dt()),
        expr_tag = "genes_of_interest",
        suffix = "_goi"
      )
    }
    
    methods <- list(
      get_session_data = function() {
        result <- list()
        if (!is.null(all_genes_logic)) result$all_genes <- all_genes_logic$get_session_data()
        if (!is.null(goi_logic)) result$goi <- goi_logic$get_session_data()
        return(result)},
      restore_session_data = function(data) {
        if (!is.null(data$all_genes) && !is.null(all_genes_logic)) all_genes_logic$restore_session_data(data$all_genes)
        if (!is.null(data$goi) && !is.null(goi_logic)) goi_logic$restore_session_data(data$goi)}
    )
    
    register_module(shared_data, "expression", patient, methods)
    
    return(methods)
  })
}



create_expression_logic <- function(session, ns, data, tissue_list, colnames_list, patient, expression_var, pathway_list, expr_tag, suffix = "") {
  
  is_restoring_session <- reactiveVal(FALSE)
  
  map_list <- colnames_map_list("expression", colnames_list$all_columns)
  mapped_checkbox_names <- map_checkbox_names(map_list)
  
  filter_state <- filterTab_server(paste0("filterTab_dropdown", suffix), colnames_list, data, mapped_checkbox_names, tissue_list, pathway_list, is_restoring_session)
  
  selected_tissues_final <- reactiveVal(tissue_list)
  selected_pathway_final <- reactiveVal(pathway_list)
  selected_columns <- reactiveVal(colnames_list$default_columns)
  selected_genes <- reactiveVal(data.frame(patient = character(), feature_name = character(), geneid = character()))

  tissue_filters_final <- setNames(
    lapply(tissue_list, function(t) reactiveVal(character(0))), 
    tissue_list
  )
  
  defaults_applied <- reactiveVal(FALSE)
  
  observe({
    req(filter_state$selected_tissue())
    req(filter_state$selected_columns())
    
    if (!defaults_applied() && !is_restoring_session()) {
      selected_tissues_final(filter_state$selected_tissue())
      selected_pathway_final(filter_state$selected_pathway())
      selected_columns(filter_state$selected_columns())
      
      for (tissue in tissue_list) {
        tissue_filter <- filter_state[[paste0("tissue_filter_", tissue)]]
        if (!is.null(tissue_filter)) tissue_filters_final[[tissue]](tissue_filter())
      }
      defaults_applied(TRUE)
    }
  })
  
  filtered_data <- reactive({
    req(data())
    df <- data()
    pathways_selected <- selected_pathway_final()
    tissues <- selected_tissues_final()
    base_cols <- c("sample","feature_name","geneid","pathway","mean_log2FC")

    if (!is.null(pathways_selected) && length(pathways_selected) > 0 && length(pathways_selected) < length(pathway_list)) {
      pattern <- paste(pathways_selected, collapse = "|")
      df <- df[grepl(pattern, pathway)]
    }

    for (t in tissue_list) {
      filters <- tissue_filters_final[[t]]()
      if (!length(filters)) next
      
      if ("log2FC > 1" %in% filters) {
        col <- paste0("log2FC_", t)
        if (col %in% names(df)) df <- df[as.numeric(get(col)) >  1]
      }
      if ("log2FC < -1" %in% filters) {
        col <- paste0("log2FC_", t)
        if (col %in% names(df)) df <- df[as.numeric(get(col)) < -1]
      }
      if ("p-value < 0.05" %in% filters) {
        col <- paste0("p_value_", t)
        if (col %in% names(df)) df <- df[as.numeric(get(col)) < 0.05]
      }
      if ("p-adj < 0.05" %in% filters) {
        col <- paste0("p_adj_", t)
        if (col %in% names(df)) df <- df[as.numeric(get(col)) < 0.05]
      }
    }

    if (is.null(tissues) || !length(tissues)) return(df[, ..base_cols])
    
    selected_cols <- unlist(lapply(tissues, function(t) c(paste0("log2FC_", t), paste0("p_value_", t), paste0("p_adj_", t))))
    valid_cols <- intersect(selected_cols, names(df))
    df_filtered <- df[, c(base_cols, valid_cols), with = FALSE]
    
    return(df_filtered)
  })

  
  # Call generate_columnsDef to generate colDef setting for reactable
  column_defs <- reactive({
    req(data())
    req(selected_columns())
    generate_columnsDef(names(data()), selected_columns(), "expression", map_list)
  })
  
  table_name <- if (suffix == "_goi") "goi_expression_table" else "expression_table"
  
  session$output[[table_name]] <- renderReactable({
    req(filtered_data())
    req(column_defs())
    filtered_data_local <- filtered_data()
    deregulated_genes <- selected_genes()
    
    reactable(
      as.data.frame(filtered_data_local),
      class = "expression-table",
      columns = column_defs(),
      resizable = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 20, 50, 100),
      defaultPageSize = 20,
      striped = TRUE,
      wrap = FALSE,
      highlight = TRUE,
      outlined = TRUE,
      filterable = TRUE,
      compact = TRUE,
      defaultColDef = colDef(sortNALast = TRUE, align = "center"),
      columnGroups = custom_colGroup_setting("expression", selected_tissues_final()),
      defaultSorted = list("geneid" = "asc"),
      rowStyle = function(index) {
        gene_in_row <- filtered_data_local$feature_name[index]
        var_in_row <- filtered_data_local$geneid[index]
        if (var_in_row %in% deregulated_genes$geneid &           
            gene_in_row %in% deregulated_genes$feature_name) {
          list(backgroundColor = "#B5E3B6",fontWeight = "bold")
        } else {
          NULL
        }
      },
      selection = "multiple",
      onClick = JS("function(rowInfo, column, event) {
                      if (event.target.classList.contains('rt-expander') || event.target.classList.contains('rt-expander-button')) {
                      } else {
                          rowInfo.toggleRowSelected();}}")
    )
  })
  
  button_name <- paste0("selectDeregulated_button", suffix)
  delete_button_name <- paste0("delete_button", suffix)
  selected_tab_name <- paste0("selectDeregulated_tab", suffix)
  
  selected_gene <- reactive({
    selected_row <- getReactableState(table_name, "selected")
    req(selected_row)
    filtered_data()[selected_row, c("feature_name","geneid")]
  })
  
  observeEvent(session$input[[button_name]], {
    selected_rows <- getReactableState(table_name, "selected")
    req(selected_rows)
    
    new_variants <- filtered_data()[selected_rows, c("sample", "feature_name", "geneid", "pathway", "mean_log2FC")]
    new_variants$sample <- patient
    
    current_variants <- selected_genes()
    new_unique_variants <- new_variants[!(new_variants$feature_name %in% current_variants$feature_name &
                                            new_variants$geneid %in% current_variants$geneid), ]
    
    if (nrow(new_unique_variants) > 0) selected_genes(rbind(current_variants, new_unique_variants))
    
    global_data <- expression_var()
    
    if (is.null(global_data) || !is.data.table(global_data) || !("sample" %in% names(global_data))) {
      global_data <- data.table(
        sample = character(),
        feature_name = character(),
        geneid = character(),
        pathway = character(),
        mean_log2FC = character()
      )
    }
    
    global_data <- global_data[sample != patient]
    updated_global_data <- rbind(global_data, selected_genes())
    expression_var(updated_global_data)
  })
  
  session$output[[selected_tab_name]] <- renderReactable({
    genes <- selected_genes()
    if (is.null(genes) || nrow(genes) == 0) {
      return(NULL)
    } else {
      genes <- as.data.table(genes)[,.(sample, feature_name, geneid, pathway, mean_log2FC)]
      reactable(
        as.data.frame(genes),
        columns = list(
          feature_name = colDef(name = "Gene name"),
          geneid = colDef(name = "Gene ID"),
          mean_log2FC = colDef(name = "log2FC")),
        selection = "multiple", onClick = "select")
    }
  })
  
  observeEvent(session$input[[delete_button_name]], {
    rows <- getReactableState(selected_tab_name, "selected")
    req(rows)
    
    current_variants <- selected_genes()
    updated_variants <- current_variants[-rows, ]
    selected_genes(updated_variants)
    
    global_data <- expression_var()
    if (!is.null(global_data) && is.data.table(global_data)) {
      global_data <- global_data[sample != patient]
    } else {
      global_data <- data.table(
        sample = character(),
        gene1 = character(),
        gene2 = character(),
        overall_support = integer(),
        position1 = character(),
        position2 = character(),
        arriba.confidence = character(),
        arriba.site1 = character(),
        arriba.site2 = character()
      )
    }
    
    if (nrow(updated_variants) > 0) {
      updated_global_data <- rbind(global_data, as.data.table(updated_variants))
    } else {
      updated_global_data <- global_data
    }
    
    expression_var(updated_global_data)
    session$sendCustomMessage("resetReactableSelection", updated_variants)
    
    if (nrow(updated_variants) == 0) hide(delete_button_name)
    
  })
  
  observeEvent(session$input[[button_name]], {
    if (is.null(selected_genes()) || nrow(selected_genes()) == 0) {
      hide(delete_button_name)
      shinyalert(
        title = "No deregulated genes selected",
        text = "Please select the deregulated genes for report from table above.",
        type = "warning",
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        callbackR = function(value) {}
      )
    } else {
      show(delete_button_name)
    }
  })
  
  observe({
    genes <- selected_genes()
    if (!is.null(genes) && nrow(genes) > 0) show(delete_button_name) else hide(delete_button_name)
  })
  
  observeEvent(filter_state$confirm(), {
    selected_tissues_final(filter_state$selected_tissue())
    selected_pathway_final(filter_state$selected_pathway())
    selected_columns(filter_state$selected_columns())
    
    for (t in tissue_list) {
      r <- filter_state[[paste0("tissue_filter_", t)]]
      if (!is.null(r)) tissue_filters_final[[t]](isolate(r()))
    }
  })
  
  plot_id <- if (suffix == "") "plot" else paste0("plot", suffix)
  plot_server(plot_id, patient, data, expr_tag, tissue_list)
  
  ###########################
  ## get / restore session ##
  ###########################
  
  selected_inputs_list <- list(
    selected_tissue = selected_tissues_final,
    selected_pathway = selected_pathway_final,
    selected_columns = selected_columns,
    selected_genes = selected_genes
  )
  
  for (tissue in tissue_list) {
    selected_inputs_list[[paste0("tissue_filter_", tissue)]] <- tissue_filters_final[[tissue]]
  }
  
  session_handlers <- create_session_handlers(
    selected_inputs = selected_inputs_list,
    filter_state = filter_state,
    is_restoring = is_restoring_session
  )
  
  return(list(
    get_session_data = session_handlers$get_session_data,
    restore_session_data = session_handlers$restore_session_data,
    filter_state = filter_state,
    is_restoring = is_restoring_session,
    filtered_data = filtered_data
  ))
}



filterTab_server <- function(id,colnames_list, data, mapped_checkbox_names, tissue_list, pathway_list, is_restoring = NULL) {
  moduleServer(id, function(input, output, session) {
    
    sanitize <- function(x) gsub("[^A-Za-z0-9]+", "_", x)
    
    # ===== HELPER FUNCTIONS =====
    
    # Funkce pro normalizaci column selection
    normalize_column_selection <- function(selection, choices_map, default_cols) {
      if (is.null(selection) || length(selection) == 0) {
        return(ch(default_cols))
      }
      
      choice_labels <- names(choices_map)
      choice_vals <- ch(unname(choices_map))
      
      # Převeď labels na values kde je to potřeba
      label2val <- setNames(choice_vals, choice_labels)
      
      sel_normalized <- character(0)
      for (item in ch(selection)) {
        if (item %in% choice_vals) {
          sel_normalized <- c(sel_normalized, item)  # už je value
        } else if (item %in% choice_labels) {
          sel_normalized <- c(sel_normalized, label2val[[item]])  # převeď label na value
        }
      }
      
      # Vrať jen platné hodnoty
      return(intersect(unique(sel_normalized), choice_vals))
    }
    
    observe({
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", choices = mapped_checkbox_names[order(mapped_checkbox_names)], selected = colnames_list$default_columns,
                                prettyOptions = list(status = "primary",icon = icon("check"),outline = FALSE))
      updatePickerInput(session, "filter_pathway", choices = pathway_list, selected = character(0),
                        options = list(`live-search` = TRUE,`actions-box` = TRUE,`multiple-separator` = ", ",`none-selected-text` = "Select pathways",`width` = "100%",`virtual-scroll` = 10,`tick-icon` = "fa fa-check",`dropupAuto` = FALSE))
    })


    
    # ===== EVENT HANDLERS =====
    
    observeEvent(input$show_all, {
      all_values <- ch(unname(mapped_checkbox_names))
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = all_values)
    })
    
    observeEvent(input$show_default, {
      default_values <- normalize_column_selection(
        selection = colnames_list$default_columns,
        choices_map = mapped_checkbox_names,
        default_cols = colnames_list$default_columns
      )
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = default_values)
    })
    
    tissue_ids <- setNames(sapply(tissue_list, sanitize, USE.NAMES = FALSE), tissue_list)
    
    tissue_reactives <- setNames(
      lapply(tissue_list, function(t) {
        t_raw <- t; t_id <- tissue_ids[[t]]; force(t_raw); force(t_id)
        reactive({
          val <- input[[paste0("select_filter_", t_id)]]
          if (is.null(val)) character(0) else as.character(val)
        })
      }),
      paste0("tissue_filter_", tissue_list)  # jméno reaktivu necháme s „raw“ názvem tkáně
    )
    
    
    restore_ui_inputs <- function(state) {
      if (!is.null(state$selected_tissue))  updateCheckboxGroupButtons(session, "select_tissue",  selected = safe_extract(state$selected_tissue))
      if (!is.null(state$selected_pathway)) updatePickerInput(session, "filter_pathway",          selected = safe_extract(state$selected_pathway))
      if (!is.null(state$selected_columns)) updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = safe_extract(state$selected_columns))
      
      for (t in tissue_list) {
        tid <- tissue_ids[[t]]
        if (!is.null(state[[paste0("tissue_filter_", t)]])) {
          updateCheckboxGroupButtons(session, paste0("select_filter_", tid),
                                     selected = safe_extract(state[[paste0("tissue_filter_", t)]]))
        }
      }
    }

    return_list <- list(
      confirm = reactive(input$confirm_btn),
      selected_tissue = reactive(input$select_tissue),
      selected_pathway = reactive(input$filter_pathway),
      selected_columns = reactive(input$colFilter_checkBox),
      restore_ui_inputs = restore_ui_inputs
    )
    
    return_list <- c(return_list, tissue_reactives)
    
    return(return_list)
  })
}



filterTab_ui <- function(id, tissue_list){
  ns <- NS(id)
  sanitize <- function(x) gsub("[^A-Za-z0-9]+", "_", x)
  choices_list <- c("log2FC > 1", "log2FC < -1", "p-value < 0.05", "p-adj < 0.05")
  
  tagList(
    tags$head(tags$style(HTML(".sw-dropdown .action-button #my-filter-btn {background-color: transparent; border: none; margin-top: -1px;}
                               .dropdown-toggle::after {display: none !important;}
                               .dropdown {position: relative; z-index: 2000;}
                               .bttn-material-circle {box-shadow: 0 0 0 0;}"))),
    dropdown(
      style = "material-circle",
      label = NULL,
      right = TRUE,
      class = "my-filter-btn",
      size = "md",
      # width = "480px",
      icon = HTML('<i class="fa-solid fa-filter download-button"></i>'),
      fluidRow(style = "display: flex; align-items: stretch; max-width: 90rem;",
               column(6,
                      box(width = 12, title = tags$div(style = "padding-top: 8px;","Filter data by:"),closable = FALSE, collapsible = FALSE,style = "height: 100%;",
                          fluidRow(#class = "filterTab-select-tissue",
                            checkboxGroupButtons(ns("select_tissue"),"Tissues:",choices = tissue_list,selected = tissue_list,individual = TRUE)),
                          fluidRow(#class = "filter_pathway",
                            pickerInput(ns("filter_pathway"), "Pathways",choices = character(0), multiple = TRUE,
                                        options = list(`live-search` = TRUE,`actions-box` = TRUE,`multiple-separator` = ", ",`none-selected-text` = "Select pathways",`width` = "90%",`virtual-scroll` = 10,`tick-icon` = "fa fa-check",`dropupAuto` = FALSE))),
                          tags$span("Tissue values:", style = "font-size: 1rem; font-weight: bold; isplay: inline-block; margin-bottom: .5rem;"),
                          tagList(lapply(tissue_list, function(tissue) {
                            tid <- sanitize(tissue)
                            fluidRow(
                              column(2, h6(tissue),style = "display: flex; align-items: center;"),
                              column(10, checkboxGroupButtons(ns(paste0("select_filter_", tid)), NULL, choices = choices_list, selected = character(0), individual = TRUE))
                            )}))
                      )),
               column(6,
                      box(width = 12, title = tags$div(style = "padding-top: 8px;","Select columns:"),closable = FALSE,collapsible = FALSE,height = "100%",
                          div(style = "flex: 1; min-width: 300px;",
                              div(class = "two-col-checkbox-group",
                                  prettyCheckboxGroup(ns("colFilter_checkBox"),label = NULL,choices = character(0))),
                              div(style = "display: flex; gap: 10px; width: 100%;",
                                  actionButton(ns("show_all"), label = "Show All", style = "flex-grow: 1; width: 0;"),
                                  actionButton(ns("show_default"), label = "Show Default", style = "flex-grow: 1; width: 0;"))))
               )),
      div(style = "display: flex; justify-content: center;", 
          actionBttn(ns("confirm_btn"), "Apply changes", style = "stretch", color = "success"))
    )
  )
}



plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("selected_plot_ui"))
  )
}


plot_server <- function(id, patient, data, expr_flag, tissue_names) {
  moduleServer(id, function(input, output, session) {
    
    ### render ui ###
    
    output$selected_plot_ui <- renderUI({
      ns <- session$ns
      
      width_px <- if (expr_flag == "all_genes") "600px" else "600px"
      height_px <- if (expr_flag == "all_genes") "800px" else "1500px"
      
      tagList(
        div(class = "collapsible-box",
            box(width = 12, closable = FALSE, collapsible = TRUE, collapsed = TRUE, title = tags$div(style = "padding-top: 8px;","Volcano plot"),
                column(6, div(class = "filterTab-select-tissue",
                              radioGroupButtons(ns("selected_tissue"), "Choose a tissue :", choices = tissue_names, justified = TRUE))),
                fluidRow(
                  column(6, use_spinner(plotlyOutput(outputId = ns("volcanoPlot_blood")))),
                  column(1,),
                  column(1, numericInput(ns("padj_cutoff"), "p-adj cutoff:", value = 0.05, min = 0, step = 0.01)),
                  column(1, numericInput(ns("logfc_cutoff"), "log2FC cutoff:", value = 1, min = 0, step = 0.1)),
                  column(1, numericInput(ns("top_n"), "Gene labels:", value = 0, min = 0, step = 1))
                )
            )
        ),
        div(class = "collapsible-box",
            box(width = 12, closable = FALSE,collapsible = TRUE, collapsed = TRUE, title = tags$div(style = "padding-top: 8px;","Heatmap"),
                use_spinner(plotOutput(outputId = ns("heatmapPlot"), width = width_px, height = height_px)))
        )
      )
    })
    
    # 🔥 Heatmap rendering
    
    #####
    # This heatmap is for top 20 expressed genes, selected for each tissue independently. Infinit values and NA's where set to 0. In this example, there is no negative log2FC present.
    # Other possibilities:
    #      1. top 20 genes general
    #      2. top 20 up-regulated or top 20 down-regulated
    # I should also set some treshold for number of tissues displayed in heatmap.
    #####
    
    output$heatmapPlot <- renderPlot({
      req(heatmap_matrix())
      
      min_val <- min(heatmap_matrix(), na.rm = TRUE)
      max_val <- max(heatmap_matrix(), na.rm = TRUE)
      
      if (min_val >= 0) {
        custom_palette <- colorRampPalette(c("white", "red"))(255)
      } else if (max_val <= 0) {
        custom_palette <- colorRampPalette(c("blue", "white"))(255)
      } else {
        custom_palette <- colorRampPalette(c("blue", "white", "red"))(255)
      }
      
      plot_titul <- if (expr_flag == "all_genes") "Top 20 selected genes" else "All genes of interest"
      
      pheatmap(heatmap_matrix(),
               scale = "none",
               cluster_rows = TRUE,
               cluster_cols = TRUE,
               show_rownames = TRUE,
               color = custom_palette,
               main = plot_titul)
    })
    # # 🌋 Volcano plot rendering
    
    output$ggvolcanoPlot <- renderPlot({
      req(data())
      
      dt_all <- rbindlist(lapply(tissue_names, function(tissue) {
        dt <- prepare_volcano(data(), tissue)
        classify_volcano_genes(dt)}))
      
      ggvolcanoPlot(dt_all)
    })
    observeEvent(input$selected_tissue, {
      output$volcanoPlot_blood <- renderPlotly({
        req(data())
        # toWebGL()
        volcanoPlot(prepare_volcano(data(), input$selected_tissue), input$selected_tissue)
      })
    })
    
    
    
    heatmap_matrix <- reactive({
      req(data())  # Ujisti se, že data jsou dostupná
      
      data_dt <- as.data.table(data())
      
      if(expr_flag == "all_genes"){
        p_adj_cols <- grep("^p_adj_", names(data_dt), value = TRUE)
        data_dt[, (p_adj_cols) := lapply(.SD, as.numeric), .SDcols = p_adj_cols]
        
        # Vybrat top 20 genů pro každou tkáň
        top_20_by_tissue <- lapply(p_adj_cols, function(col) {
          data_dt[get(col) > 0][order(get(col)), .(
            geneid, feature_name,
            tissue = gsub("^p_adj_", "", col),
            log2FC = get(gsub("p_adj", "log2FC", col)),
            p_adj = get(col)
          )][1:20]
        })
        
        top_20_dt <- unique(rbindlist(top_20_by_tissue))
        heatmap_data <- dcast.data.table(top_20_dt, geneid + feature_name ~ tissue, value.var = "log2FC", fill = NA)
        
        
      } else {
        log2FC_cols <- grep("^log2FC_", names(data_dt), value = TRUE)
        long_data <- melt.data.table(data_dt, id.vars = c("geneid", "feature_name"), measure.vars = log2FC_cols, variable.name = "tissue", value.name = "log2FC")
        long_data[, tissue := gsub("^log2FC_", "", tissue)]
        heatmap_data <- dcast.data.table(long_data, geneid + feature_name ~ tissue, value.var = "log2FC", fill = NA)
        
      }
      
      
      heatmap_matrix <- as.matrix(heatmap_data[, -c("geneid", "feature_name"), with = FALSE])
      heatmap_matrix <- apply(heatmap_matrix, 2, as.numeric)
      rownames(heatmap_matrix) <- heatmap_data$feature_name
      
      # Ošetření NA hodnot
      heatmap_matrix[is.na(heatmap_matrix)] <- 0
      
      return(heatmap_matrix)
    })
    
    
  })
}