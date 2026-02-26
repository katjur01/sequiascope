# app/view/expression_profile_table.R

box::use(
  shiny[moduleServer, NS, tagList, fluidRow, fluidPage, column, tabPanel, reactive, req, observe, div, observeEvent, reactiveVal, icon, splitLayout, h4, bindEvent,conditionalPanel,isTruthy,
        updateSelectInput, selectInput, numericInput, renderPlot, plotOutput, uiOutput, renderUI, verbatimTextOutput, renderPrint, reactiveValues, isolate,
        downloadButton,is.reactive],
  reactable,
  bs4Dash[actionButton,box,tabBox],
  reactable[colDef, reactableOutput, renderReactable, reactable, getReactableState, colGroup, JS],
  htmltools[tags,HTML,h6,h5],
  plotly[plotlyOutput, renderPlotly, toWebGL],
  shinyWidgets[radioGroupButtons, checkboxGroupButtons, updateCheckboxGroupButtons,prettyCheckboxGroup, updatePrettyCheckboxGroup, dropdown, dropdownButton, actionBttn,
               awesomeCheckboxGroup, pickerInput, updatePickerInput],
  data.table[rbindlist, dcast.data.table, as.data.table, melt.data.table, copy, setnames],
  grDevices[colorRampPalette],
  # pheatmap[pheatmap],
  stats[setNames],
  magrittr[`%>%`],
  shinyalert[shinyalert,useShinyalert],
  shinyjs[useShinyjs,hide,show],
  data.table[data.table,as.data.table,is.data.table,uniqueN]
)

box::use(
  app/logic/plots[prepare_barPlot_data, create_barPlot, prepare_volcano, volcanoPlot, ggvolcanoPlot, classify_volcano_genes],
  app/logic/waiters[use_spinner],
  app/logic/load_data[load_data],
  app/logic/helper_reactable[custom_colGroup_setting],
  app/logic/filter_columns[map_checkbox_names, colnames_map_list, generate_columnsDef, expand_expression_columns, contract_expression_columns],
  app/logic/prepare_table[prepare_expression_table, prepare_goi_table, set_pathway_colors],
  app/logic/helper_networkGraph[get_pathway_list],
  app/logic/session_utils[create_session_handlers, register_module, safe_extract, nz, ch],
  app/logic/export_functions[get_table_download_handler]
)

ui <- function(id, tissue_list, goi = FALSE) {
  ns <- NS(id)
  useShinyjs()
  
  tabs <- list()
  
  # GOI will be created only when goi = TRUE
  if (isTRUE(goi)) {
    tabs <- c(tabs, list(
      tabPanel(title = "Genes of Interest", value = "genesOfinterest",
               tagList(
               # tags$head(tags$style(HTML("
               #     button[id$='download_btn_goi'].btn.dropdown-toggle { 
               #       background-color: transparent !important; 
               #       background: transparent !important;
               #       border: none !important;
               #       box-shadow: none !important;
               #       transition: box-shadow 0.3s ease;
               #     }
               #     button[id$='download_btn_goi'].btn.dropdown-toggle:hover { 
               #       background-color: transparent !important;
               #       background: transparent !important;
               #       border: none !important;
               #       box-shadow: 0 5px 11px 0 rgba(0, 0, 0, .18), 0 4px 15px 0 rgba(0, 0, 0, .15) !important;
               #     }
               #   "))),
                 tags$head(tags$style(HTML("
                   .expression-filter-wrapper .dropdown { 
                      position: relative; 
                      z-index: 2000; 
                    }
                    
                    .expression-filter-wrapper .sw-dropdown-content { 
                      min-width: 1100px !important; 
                      width: auto !important; 
                      max-width: 90vw !important; 
                    }
                    
                    .expression-filter-wrapper .my-filter-btn .sw-dropdown-in { 
                      min-width: 1100px !important; 
                      width: auto !important; 
                    }
                   
                   .download-dropdown-wrapper .dropdown-toggle,
                   .expression-filter-wrapper .bttn-material-circle { 
                     background-color: transparent !important; 
                     background: transparent !important;
                     border: none !important;
                     box-shadow: none !important;
                     padding: 0 !important;
                     transition: box-shadow 0.3s ease;
                   }
                   
                   .download-dropdown-wrapper .dropdown-toggle:hover,
                   .expression-filter-wrapper .bttn-material-circle:hover { 
                     background-color: transparent !important;
                     background: transparent !important;
                     border: none !important;
                     box-shadow: 0 5px 11px 0 rgba(0, 0, 0, .18), 0 4px 15px 0 rgba(0, 0, 0, .15) !important;
                   }
                   
                   .download-dropdown-wrapper .dropdown-toggle:focus,
                   .expression-filter-wrapper .bttn-material-circle:focus {
                     outline: none !important;
                     border: none !important;
                     box-shadow: 0 5px 11px 0 rgba(0, 0, 0, .18), 0 4px 15px 0 rgba(0, 0, 0, .15) !important;
                   }
                   
                   /* Skrytí šipky u dropdownButton */
                   .download-dropdown-wrapper .dropdown-toggle::after {
                     display: none !important;
                   }
                 "))),
                       fluidRow(
                         div(style = "width: 100%; text-align: right; display: flex; flex-direction: row-reverse;",
                             div(class = "download-dropdown-wrapper",
                             dropdownButton(inputId = ns("download_btn_goi"),label = NULL, right = TRUE, width = "240px",icon = HTML('<i class="fa-solid fa-download download-button"></i>'),
                                            selectInput(ns("export_data_table_goi"), "Select data:", choices = c("All data" = "all", "Filtered data" = "filtered")),
                                            selectInput(ns("export_format_table_goi"), "Select format:", choices = c("CSV" = "csv", "TSV" = "tsv", "Excel" = "xlsx")),
                                            downloadButton(ns("table_download_goi"), "Download"))
                             ),
                             div(class = "expression-filter-wrapper",
                                 filterTab_ui(ns("filterTab_dropdown_goi"), tissue_list)))
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
             tagList(
             # tags$head(tags$style(HTML("
             #       button[id$='download_btn'].btn.dropdown-toggle { 
             #         background-color: transparent !important; 
             #         background: transparent !important;
             #         border: none !important;
             #         box-shadow: none !important;
             #         transition: box-shadow 0.3s ease;
             #       }
             #       button[id$='download_btn'].btn.dropdown-toggle:hover { 
             #         background-color: transparent !important;
             #         background: transparent !important;
             #         border: none !important;
             #         box-shadow: 0 5px 11px 0 rgba(0, 0, 0, .18), 0 4px 15px 0 rgba(0, 0, 0, .15) !important;
             #       }
             #     ")))),
             fluidRow(
               div(style = "width: 100%; text-align: right; display: flex; flex-direction: row-reverse;",
                   div(class = "download-dropdown-wrapper",
                   dropdownButton(inputId = ns("download_btn"), label = NULL, right = TRUE, width = "240px",icon = HTML('<i class="fa-solid fa-download download-button"></i>'),
                                  selectInput(ns("export_data_table"), "Select data:", choices = c("All data" = "all", "Filtered data" = "filtered")),
                                  selectInput(ns("export_format_table"), "Select format:", choices = c("CSV" = "csv", "TSV" = "tsv", "Excel" = "xlsx")),
                                  downloadButton(ns("table_download"), "Download"))
                   ),
                   div(class = "expression-filter-wrapper",
                       filterTab_ui(ns("filterTab_dropdown"), tissue_list)))
             ),
             use_spinner(reactableOutput(ns("expression_table"))),
             div(
               tags$br(),
               actionButton(ns("selectDeregulated_button"), "Select deregulated genes for report", status = "primary"),
               tags$br(),
               fluidRow(column(8, reactableOutput(ns("selectDeregulated_tab")))),
               tags$br(),
               fluidRow(column(3, actionButton(ns("delete_button"), "Delete genes", icon = icon("trash-can"))))),
             tags$br(),
             plot_ui(ns("plot"))
    ))
  ))
  
  
  tabbox_id <- if (isTRUE(goi)) "expression_profile_tabs_goi" else "expression_profile_tabs_allGenes"
  selected_tab <- if (isTRUE(goi)) "genesOfinterest" else "allGenes"
  
  do.call(tabBox, c(list(id = ns(tabbox_id), width = 12, collapsible = FALSE, selected = selected_tab, headerBorder = FALSE), tabs))
}

server <- function(id, patient, shared_data, patient_files, file_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(
      list(data(), tissue_list(), prepare_goi_dt()),
      ignoreNULL = FALSE,
      ignoreInit = FALSE,
      {
      req(data())
      req(tissue_list())
      
      # Funkce pro zpracování dat (GOI nebo All genes)
      process_expression_data <- function(dt, label) {
        all_deregulated_genes <- character(0)
        all_altered_pathways <- character(0)
        
        for (tissue in tissue_list()) {
          log2fc_col <- paste0("log2fc_", tissue)
          padj_col <- paste0("p_adj_", tissue)
          
          # Kontrola, zda sloupce existují
          if (!all(c(log2fc_col, padj_col) %in% names(dt))) {
            next
          }
          
          # Deregulated genes: |log2fc| > 1 AND p_adj < 0.05
          deregulated_genes <- dt[abs(as.numeric(get(log2fc_col))) > 1 & as.numeric(get(padj_col)) < 0.05]
          
          # DEBUG: Kolik genů má tato tkáň?
          message("🔍 [", label, "] Tissue: ", tissue, " has ", nrow(deregulated_genes), " deregulated genes")
          message("    Unique geneids: ", uniqueN(deregulated_genes$geneid))
          
          # Přidat geneid do seznamu
          if (nrow(deregulated_genes) > 0 && "geneid" %in% names(deregulated_genes)) {
            all_deregulated_genes <- c(all_deregulated_genes, deregulated_genes$geneid)
          }
          
          # Altered pathways: použít stejná data (deregulated_genes) pro pathways
          if (nrow(deregulated_genes) > 0 && "pathway" %in% names(deregulated_genes)) {
            pathways_split <- unlist(strsplit(deregulated_genes$pathway, ",\\s*"))
            pathways_split <- pathways_split[nzchar(pathways_split)]  # Odstranit prázdné
            all_altered_pathways <- c(all_altered_pathways, pathways_split)
          }
        }
        
        # DEBUG: Celkový výsledek
        message("📊 [", label, "] Total collected genes (with duplicates): ", length(all_deregulated_genes))
        message("📊 [", label, "] Unique genes after concatenation: ", uniqueN(all_deregulated_genes))
        
        return(list(
          genes = uniqueN(all_deregulated_genes),
          pathways = uniqueN(all_altered_pathways)
        ))
      }
      
      # Zpracovat All genes data
      all_genes_result <- process_expression_data(data(), "all_genes")
      
      # Zpracovat GOI data (pokud existují)
      goi_result <- NULL
      if (has_goi() && !is.null(prepare_goi_dt())) {
        goi_result <- process_expression_data(prepare_goi_dt(), "goi")
      }
      
      # Vytvořit formátované řetězce s oddělovačem |
      if (!is.null(goi_result)) {
        # Máme obě: GOI | All genes
        for_review_expr_str <- paste0(goi_result$genes, " | ", all_genes_result$genes)
        altered_pathways_str <- paste0(goi_result$pathways, " | ", all_genes_result$pathways)
      } else {
        # Nemáme GOI: - | All genes
        for_review_expr_str <- paste0("NA | ", all_genes_result$genes)
        altered_pathways_str <- paste0("NA | ", all_genes_result$pathways)
      }
      
      # Spočítat unikátní hodnoty napříč všemi tkáněmi
      overview_dt <- data.table(
        tissues = paste(unique(tissue_list()), collapse = ", "),
        for_review_expr = for_review_expr_str,
        altered_pathways = altered_pathways_str
      )
      
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
    tissue_list <- reactive(prepare_data()$tissues)
    colnames_list <- reactive(prepare_data()$columns)
    
    # 1. ALL GENES
    all_genes_logic <- create_expression_logic(
      session = session,
      ns = ns,
      data = data,
      tissue_list = tissue_list,
      colnames_list = colnames_list,
      patient = patient,
      expression_var = shared_data$expression.variants.all,
      pathway_list = get_pathway_list("all_genes", kegg_tab_path = shared_data$kegg_tab_path()),
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
        pathway_list = get_pathway_list("genes_of_interest",prepare_goi_dt(), kegg_tab_path = shared_data$kegg_tab_path()),
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
  
  # Helper pro získání hodnot - zvládne reactive i static
  get_tissue_list <- reactive({
    if (is.reactive(tissue_list)) tissue_list() else tissue_list
  })
  
  get_colnames_list <- reactive({
    if (is.reactive(colnames_list)) colnames_list() else colnames_list
  })
  
  # map_list musí být reactive pro dynamické aktualizace sloupců
  map_list <- reactive({
    req(get_colnames_list())
    req(data())
    req(get_tissue_list())
    colnames_map_list("expression", get_colnames_list()$all_columns, tissues = get_tissue_list())
  })
  
  # mapped_checkbox_names <- map_checkbox_names(map_list)
  mapped_checkbox_names <- reactive({
    req(data())
    req(get_colnames_list())
    req(map_list())
    map_checkbox_names(map_list(), get_colnames_list()$all_columns, is_expression = TRUE)
  })

  filter_state <- filterTab_server(paste0("filterTab_dropdown", suffix), get_colnames_list, data, mapped_checkbox_names, get_tissue_list, pathway_list, is_restoring_session)
  
  selected_tissues_final <- reactiveVal(NULL)
  selected_pathway_final <- reactiveVal(pathway_list)
  selected_columns <- reactiveVal(NULL)
  selected_genes <- reactiveVal(data.frame(patient = character(), feature_name = character(), geneid = character()))
  
  # Inicializuj hodnoty když jsou data připravená
  observe({
    req(get_tissue_list())
    req(get_colnames_list())
    if (is.null(selected_tissues_final())) {
      selected_tissues_final(get_tissue_list())
    }
    if (is.null(selected_columns())) {
      selected_columns(get_colnames_list()$default_columns)
    }
  })

  tissue_filters_final <- reactiveValues()
  
  # Inicializuj tissue_filters_final když jsou tissues dostupné
  observe({
    req(get_tissue_list())
    tissues <- get_tissue_list()
    for (t in tissues) {
      if (is.null(tissue_filters_final[[t]])) {
        tissue_filters_final[[t]] <- character(0)
      }
    }
  })
  
  defaults_applied <- reactiveVal(FALSE)
  
  observe({
    req(filter_state$selected_tissue())
    req(filter_state$selected_columns())
    
    if (!defaults_applied() && !is_restoring_session()) {
      # NOTE: selected_tissues_final is intentionally NOT overwritten here.
      # It is already initialised to the full tissue list by the observe above.
      # filter_state$selected_tissue() may not yet be stable (picker not fully
      # rendered) and overwriting it here caused the table to show only the first
      # tissue until the user clicked Apply.
      selected_pathway_final(filter_state$selected_pathway())
      selected_columns(filter_state$selected_columns())
      
      req(get_tissue_list())
      for (tissue in get_tissue_list()) {
        tissue_filter <- filter_state[[paste0("tissue_filter_", tissue)]]
        if (!is.null(tissue_filter)) {
          val <- tissue_filter()
          tissue_filters_final[[tissue]] <- val
        }
      }

      defaults_applied(TRUE)
    }
  })
  
  filtered_data <- reactive({
    req(data())
    req(selected_columns())
    df <- data()
    pathways_selected <- selected_pathway_final()
    
    # Apply pathway filter
    if (!is.null(pathways_selected) && length(pathways_selected) > 0 && length(pathways_selected) < length(pathway_list)) {
      pattern <- paste(pathways_selected, collapse = "|")
      df <- df[grepl(pattern, pathway)]
    }

    # Apply tissue-specific filters (log2FC, p-value, p-adj)
    req(get_tissue_list())
    for (t in get_tissue_list()) {
      filters <- tissue_filters_final[[t]]
      if (!length(filters)) next
      
      # log2FC filtering - OR logic if both conditions selected
      log2fc_gt1 <- "log2FC > 1" %in% filters
      log2fc_lt_minus1 <- "log2FC < -1" %in% filters
      
      if (log2fc_gt1 || log2fc_lt_minus1) {
        col <- paste0("log2fc_", t)
        if (col %in% names(df)) {
          if (log2fc_gt1 && log2fc_lt_minus1) {
            # Both selected: OR logic (> 1 OR < -1)
            df <- df[as.numeric(get(col)) > 1 | as.numeric(get(col)) < -1]
          } else if (log2fc_gt1) {
            # Only > 1
            df <- df[as.numeric(get(col)) > 1]
          } else {
            # Only < -1
            df <- df[as.numeric(get(col)) < -1]
          }
        }
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

    # Return only selected columns (preserve order from data - set by setcolorder in prepare_expression_table)
    expanded_cols <- expand_expression_columns(selected_columns(), names(df))
    valid_cols <- intersect(names(df), expanded_cols)  # intersect preserves order from first argument (names(df))
    
    # Filter out tissue columns for unselected tissues
    selected_tissues <- selected_tissues_final()
    if (!is.null(selected_tissues) && length(selected_tissues) > 0) {
      # Get all tissue column patterns
      all_tissues <- get_tissue_list()
      unselected_tissues <- setdiff(all_tissues, selected_tissues)
      
      # Remove columns for unselected tissues
      if (length(unselected_tissues) > 0) {
        tissue_pattern <- paste0("^(log2fc|p_value|p_adj)_(", paste(unselected_tissues, collapse = "|"), ")$")
        valid_cols <- valid_cols[!grepl(tissue_pattern, valid_cols)]
      }
    }
    
    df_filtered <- df[, ..valid_cols]
    
    return(df_filtered)
  })

  
  # Call generate_columnsDef to generate colDef setting for reactable
  column_defs <- reactive({
    req(data())
    req(selected_columns())
    req(map_list())
    # Expand wildcard selections (log2fc_*, p_value_*, p_adj_*) to actual tissue columns
    expanded_cols <- expand_expression_columns(selected_columns(), names(data()))
    
    generate_columnsDef(names(data()), expanded_cols, "expression", map_list())
  })
  

  table_name <- if (suffix == "_goi") "goi_expression_table" else "expression_table"
  
  session$output[[table_name]] <- renderReactable({
    req(filtered_data())
    req(column_defs())
    filtered_data_local <- filtered_data()
    deregulated_genes <- selected_genes()
    
    # Determine defaultSorted only if geneid column exists
    default_sorted <- if ("geneid" %in% names(filtered_data_local)) {
      list("geneid" = "asc")
    } else {
      NULL
    }
    
    # Determine columnGroups - keep groups but only with columns that exist in data
    all_col_groups <- custom_colGroup_setting("expression", selected_tissues_final())
    col_groups <- NULL
    if (!is.null(all_col_groups)) {
      # Filter each group's columns to only include those that exist
      col_groups <- lapply(all_col_groups, function(grp) {
        existing_cols <- grp$columns[grp$columns %in% names(filtered_data_local)]
        if (length(existing_cols) > 0) {
          grp$columns <- existing_cols
          return(grp)
        }
        return(NULL)
      })
      
      # Remove NULL groups (groups with no columns)
      col_groups <- Filter(Negate(is.null), col_groups)
      
      # If no groups remain, set to NULL
      if (length(col_groups) == 0) col_groups <- NULL
    }
    
    # rowStyle only if both feature_name and geneid exist
    row_style_fn <- if (all(c("feature_name", "geneid") %in% names(filtered_data_local))) {
      function(index) {
        gene_in_row <- filtered_data_local$feature_name[index]
        var_in_row <- filtered_data_local$geneid[index]
        if (var_in_row %in% deregulated_genes$geneid &           
            gene_in_row %in% deregulated_genes$feature_name) {
          list(backgroundColor = "#B5E3B6", fontWeight = "bold")
        } else {
          NULL
        }
      }
    } else {
      NULL
    }

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
      columnGroups = col_groups,
      defaultSorted = default_sorted,
      rowStyle = row_style_fn,
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
    
    # Get all required columns from the original data, not filtered_data
    # This ensures we have all necessary columns for the report
    required_cols <- c("feature_name", "geneid", "pathway", "mean_log2fc")
    available_cols <- intersect(required_cols, names(data()))
    
    new_variants <- data()[filtered_data()[selected_rows, which = TRUE], ..available_cols]
    new_variants$sample <- patient
    
    # Rename mean_log2fc to log2FC for consistency with report expectations
    if ("mean_log2fc" %in% names(new_variants)) {
      setnames(new_variants, "mean_log2fc", "log2FC")
    }
    
    current_variants <- selected_genes()
    new_unique_variants <- new_variants[!(new_variants$feature_name %in% current_variants$feature_name &
                                            new_variants$geneid %in% current_variants$geneid), ]
    
    if (nrow(new_unique_variants) > 0) {
      selected_genes(rbindlist(list(current_variants, new_unique_variants), fill = TRUE))
    }
    
    global_data <- expression_var()
    
    if (is.null(global_data) || !is.data.table(global_data) || !("sample" %in% names(global_data))) {
      global_data <- data.table(
        sample = character(),
        feature_name = character(),
        geneid = character(),
        pathway = character(),
        log2FC = character()
      )
    }
    
    global_data <- global_data[sample != patient]
    updated_global_data <- rbindlist(list(global_data, selected_genes()), fill = TRUE)
    expression_var(updated_global_data)
  })
  
  session$output[[selected_tab_name]] <- renderReactable({
    genes <- selected_genes()
    if (is.null(genes) || nrow(genes) == 0) {
      return(NULL)
    } else {
      genes <- as.data.table(genes)[,.(feature_name, geneid, pathway, log2FC)]
      reactable(
        as.data.frame(genes),
        columns = list(
          feature_name = colDef(name = "Gene name"),
          geneid = colDef(name = "Gene ID"),
          log2FC = colDef(name = "mean log2FC")),
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

    req(get_tissue_list())
    for (t in get_tissue_list()) {
      filter_key <- paste0("tissue_filter_", t)
      r <- filter_state[[filter_key]]
      if (!is.null(r)) {
        val <- isolate(r())
        tissue_filters_final[[t]] <- val
      }
    }
  })
  
  plot_id <- if (suffix == "") "plot" else paste0("plot", suffix)
  plot_server(plot_id, patient, data, expr_tag, get_tissue_list)
  
  ###########################
  ## Download handlers     ##
  ###########################
  
  download_button_id <- if (suffix == "_goi") "table_download_goi" else "table_download"
  
  session$output[[download_button_id]] <- get_table_download_handler(
    input = session$input,
    patient = patient,
    data = data,
    filtered_data = filtered_data,
    suffix = suffix
  )
  
  ###########################
  ## get / restore session ##
  ###########################
  
  selected_inputs_list <- list(
    selected_tissue = selected_tissues_final,
    selected_pathway = selected_pathway_final,
    selected_columns = selected_columns,
    selected_genes = selected_genes
  )
  
  # Inicializuj tissue filters do selected_inputs_list
  observe({
    req(get_tissue_list())
    for (tissue in get_tissue_list()) {
      selected_inputs_list[[paste0("tissue_filter_", tissue)]] <- tissue_filters_final[[tissue]]
    }
  })
  
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
    
    # Helper pro získání hodnot - zvládne reactive i static
    get_tissue_list <- reactive({
      if (is.reactive(tissue_list)) tissue_list() else tissue_list
    })
    
    get_colnames_list <- reactive({
      if (is.reactive(colnames_list)) colnames_list() else colnames_list
    })
    
    get_checkbox_names <- reactive({
      if (is.reactive(mapped_checkbox_names)) mapped_checkbox_names() else mapped_checkbox_names
    })
    
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
      req(get_checkbox_names())
      req(get_colnames_list())
      checkbox_names <- get_checkbox_names()
      
      # Convert tissue-specific columns to wildcards for checkbox selection
      default_cols_contracted <- contract_expression_columns(get_colnames_list()$default_columns)

      updatePrettyCheckboxGroup(session, "colFilter_checkBox", choices = checkbox_names[order(checkbox_names)], selected = default_cols_contracted,
                                prettyOptions = list(status = "primary",icon = icon("check"),outline = FALSE))
      updatePickerInput(session, "filter_pathway", choices = pathway_list, selected = character(0),
                        options = list(`live-search` = TRUE,`actions-box` = TRUE,`multiple-separator` = ", ",`none-selected-text` = "Select pathways",`width` = "100%",`virtual-scroll` = 10,`tick-icon` = "fa fa-check",`dropupAuto` = FALSE))
    })


    
    # ===== EVENT HANDLERS =====
    
    observeEvent(input$show_all, {
      req(mapped_checkbox_names())
      all_values <- ch(unname(mapped_checkbox_names()))
      message("#### all_values :",paste0(all_values,collapse =", "))
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = all_values)
    })
    
    observeEvent(input$show_default, {
      req(get_checkbox_names())
      req(get_colnames_list())

      # Contract tissue-specific columns to wildcards before normalizing
      default_cols_contracted <- contract_expression_columns(get_colnames_list()$default_columns)
      
      default_values <- normalize_column_selection(
        selection = default_cols_contracted,
        choices_map = get_checkbox_names(),
        default_cols = default_cols_contracted
      )
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = default_values)
    })
    
    tissue_ids_reactive <- reactive({
      req(get_tissue_list())
      setNames(sapply(get_tissue_list(), sanitize, USE.NAMES = FALSE), get_tissue_list())
    })
    
    # Build tissue reactives list - must be done outside of reactive context
    # but depends on tissues which are available immediately
    build_tissue_reactives <- function() {
      tissues <- get_tissue_list()
      if (is.null(tissues) || length(tissues) == 0) return(list())
      
      tissue_ids <- setNames(sapply(tissues, sanitize, USE.NAMES = FALSE), tissues)
      setNames(
        lapply(tissues, function(t) {
          t_raw <- t; t_id <- tissue_ids[[t]]; force(t_raw); force(t_id)
          reactive({
            val <- input[[paste0("select_filter_", t_id)]]
            if (is.null(val)) character(0) else as.character(val)
          })
        }),
        paste0("tissue_filter_", tissues)
      )
    }
    
    
    restore_ui_inputs <- function(state) {
      message("🎯 Restoring filter UI inputs for expression")
      
      if (!is.null(state$selected_tissue)) {
        val <- safe_extract(state$selected_tissue)
        message(sprintf("Restoring selected_tissue: %s", paste(val, collapse = ", ")))
        updateCheckboxGroupButtons(session, "select_tissue", selected = val)
      }
      
      if (!is.null(state$selected_pathway)) {
        val <- safe_extract(state$selected_pathway)
        message(sprintf("Restoring selected_pathway: %s", paste(val, collapse = ", ")))
        updatePickerInput(session, "filter_pathway", selected = val)
      }
      
      if (!is.null(state$selected_columns)) {
        val <- safe_extract(state$selected_columns)
        message(sprintf("Restoring selected_columns: %s", paste(val, collapse = ", ")))
        updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = val)
      }
      
      # Restore tissue-specific filters
      tissues <- isolate(tissue_list())
      tissue_ids_map <- isolate(tissue_ids_reactive())
      
      if (!is.null(tissues) && length(tissues) > 0 && !is.null(tissue_ids_map)) {
        for (t in tissues) {
          tid <- tissue_ids_map[[t]]
          state_key <- paste0("tissue_filter_", t)
          
          if (!is.null(state[[state_key]]) && !is.null(tid)) {
            val <- safe_extract(state[[state_key]])
            message(sprintf("Restoring tissue filter for %s (id: %s): %s", t, tid, paste(val, collapse = ", ")))
            updateCheckboxGroupButtons(session, paste0("select_filter_", tid), selected = val)
          }
        }
      }
      
      message("✅ Expression filter restore completed")
    }

    base_return_list <- list(
      confirm = reactive(input$confirm_btn),
      selected_tissue = reactive(input$select_tissue),
      selected_pathway = reactive(input$filter_pathway),
      selected_columns = reactive(input$colFilter_checkBox),
      restore_ui_inputs = restore_ui_inputs
    )
    
    # Add tissue reactives to return list
    tissue_reactives <- build_tissue_reactives()
    return_list <- c(base_return_list, tissue_reactives)
    
    return(return_list)
  })
}



filterTab_ui <- function(id, tissue_list){
  ns <- NS(id)
  sanitize <- function(x) gsub("[^A-Za-z0-9]+", "_", x)
  choices_list <- c("log2FC > 1", "log2FC < -1", "p-value < 0.05", "p-adj < 0.05")
  
  tagList(
    tags$head(tags$style(HTML("button:has(.download-button) .dropdown-toggle {border-radius: 0; padding: 0; background-color: transparent; border: none; float: right; margin-top: -1px;}
                               button:has(.download-button) .dropdown-toggle::after {display: none !important;}
                               button:has(.download-button) .glyphicon-triangle-bottom {display: none !important; width: 0 !important; margin: 0 !important; padding: 0 !important;}

                               .sw-dropdown .action-button #my-filter-btn { background-color: transparent; border: none; margin-top: -1px; }
                               .expression-filter-wrapper .dropdown { position: relative; z-index: 2000; }
                               .expression-filter-wrapper .bttn-material-circle { box-shadow: 0 0 0 0 !important; }
                               .expression-filter-wrapper .bttn-material-circle:hover { box-shadow: 0 5px 11px 0 rgba(0, 0, 0, .18), 0 4px 15px 0 rgba(0, 0, 0, .15) !important; background-color: transparent !important; border: none !important; }
                               .expression-filter-wrapper .bttn-material-circle:focus { box-shadow: 0 5px 11px 0 rgba(0, 0, 0, .18), 0 4px 15px 0 rgba(0, 0, 0, .15) !important; border: none !important; outline: none !important; }
                               .expression-filter-wrapper .sw-dropdown-content { min-width: 1100px !important; width: auto !important; max-width: 90vw !important; }
                               .expression-filter-wrapper .my-filter-btn .sw-dropdown-in { min-width: 1100px !important; width: auto !important; }"))),
    dropdown(
      style = "material-circle",
      label = NULL,
      right = TRUE,
      class = "my-filter-btn",
      size = "md",
      # width = "480px",
      icon = HTML('<i class="fa-solid fa-filter download-button"></i>'),
      fluidRow(style = "display: flex; align-items: stretch; min-width: 1150px;",
               column(6, style = "display: flex; flex-direction: column;",
                      div(style = "flex: 1; display: flex; flex-direction: column;",
                          box(width = 12, title = tags$div(style = "padding-top: 8px;","Filter data by:"),closable = FALSE, collapsible = FALSE,style = "flex: 1; display: flex; flex-direction: column;",
                              fluidRow(#class = "filterTab-select-tissue",
                                checkboxGroupButtons(ns("select_tissue"),"Tissues:",choices = tissue_list,selected = tissue_list,individual = TRUE)),
                              fluidRow(#class = "filter_pathway",
                                pickerInput(ns("filter_pathway"), "Pathways",choices = character(0), multiple = TRUE,
                                            options = list(`live-search` = TRUE,`actions-box` = TRUE,`multiple-separator` = ", ",`none-selected-text` = "Select pathways",`width` = "90%",`virtual-scroll` = 10,`tick-icon` = "fa fa-check",`dropupAuto` = FALSE))),
                              fluidRow(tags$span("Tissue values:",style = "font-size: 1rem; font-weight: bold;"),lapply(tissue_list, function(tissue) {
                                tid <- sanitize(tissue)
                                fluidRow(style = "display: flex; justify-content: space-between;",
                                  column(2.5, h6(tissue, style = "margin-right: 10px;"),style = "display: flex; align-items: center; padding-left: 15px;"),
                                  column(9.5, checkboxGroupButtons(ns(paste0("select_filter_", tid)), NULL, choices = choices_list, selected = character(0), individual = TRUE))
                                )}))
                          ))),
               column(6, style = "display: flex; flex-direction: column;",
                      div(style = "flex: 1; display: flex; flex-direction: column;",
                          box(width = 12, title = tags$div(style = "padding-top: 8px;","Select columns:"),closable = FALSE,collapsible = FALSE,style = "flex: 1; display: flex; flex-direction: column;",
                              div(style = "flex: 1; min-width: 300px; display: flex; flex-direction: column; justify-content: space-between;",
                                  div(class = "two-col-checkbox-group", style = "margin-bottom: 15px;",
                                      prettyCheckboxGroup(ns("colFilter_checkBox"),label = NULL,choices = character(0))),
                                  div(style = "display: flex; gap: 10px; width: 100%;",
                                      actionButton(ns("show_all"), label = "Show All", style = "flex-grow: 1; width: 0;"),
                                      actionButton(ns("show_default"), label = "Show Default", style = "flex-grow: 1; width: 0;")))
                          )))),
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
    
    # Helper pro získání tissue names - zvládne reactive i static
    get_tissue_names <- reactive({
      if (is.reactive(tissue_names)) tissue_names() else tissue_names
    })
    
    ### render ui ###
    
    output$selected_plot_ui <- renderUI({
      ns <- session$ns
      req(get_tissue_names())
      
      width_px <- if (expr_flag == "all_genes") "600px" else "600px"
      height_px <- if (expr_flag == "all_genes") "800px" else "1500px"
      
      tagList(
        div(class = "collapsible-box",
            box(width = 12, closable = FALSE, collapsible = TRUE, collapsed = TRUE, title = tags$div(style = "padding-top: 8px;","Volcano plot"),
                column(6, div(class = "filterTab-select-tissue",
                              radioGroupButtons(ns("selected_tissue"), "Choose a tissue :", choices = get_tissue_names(), justified = TRUE))),
                fluidRow(
                  column(6, use_spinner(plotlyOutput(outputId = ns("volcanoPlot_blood")))),
                  column(1,),
                  column(1, numericInput(ns("padj_cutoff"), "p-adj cutoff:", value = 0.05, min = 0, step = 0.01)),
                  column(1, numericInput(ns("logfc_cutoff"), "log2FC cutoff:", value = 1, min = 0, step = 0.1)),
                  column(1, numericInput(ns("top_n"), "Gene labels:", value = 0, min = 0, step = 1))
                )
            )
        )
        # div(class = "collapsible-box",
        #     box(width = 12, closable = FALSE,collapsible = TRUE, collapsed = TRUE, title = tags$div(style = "padding-top: 8px;","Heatmap"),
        #         use_spinner(plotOutput(outputId = ns("heatmapPlot"), width = width_px, height = height_px)))
        # )
      )
    })
    
    # # Heatmap rendering
    # 
    # #####
    # # This heatmap is for top 20 expressed genes, selected for each tissue independently. Infinit values and NA's where set to 0. In this example, there is no negative log2FC present.
    # # Other possibilities:
    # #      1. top 20 genes general
    # #      2. top 20 up-regulated or top 20 down-regulated
    # # I should also set some treshold for number of tissues displayed in heatmap.
    # #####
    # 
    # output$heatmapPlot <- renderPlot({
    #   req(heatmap_matrix())
    #   
    #   min_val <- min(heatmap_matrix(), na.rm = TRUE)
    #   max_val <- max(heatmap_matrix(), na.rm = TRUE)
    #   
    #   if (min_val >= 0) {
    #     custom_palette <- colorRampPalette(c("white", "red"))(255)
    #   } else if (max_val <= 0) {
    #     custom_palette <- colorRampPalette(c("blue", "white"))(255)
    #   } else {
    #     custom_palette <- colorRampPalette(c("blue", "white", "red"))(255)
    #   }
    #   
    #   plot_titul <- if (expr_flag == "all_genes") "Top 20 selected genes" else "All genes of interest"
    #   
    #   pheatmap(heatmap_matrix(),
    #            scale = "none",
    #            cluster_rows = TRUE,
    #            cluster_cols = TRUE,
    #            show_rownames = TRUE,
    #            color = custom_palette,
    #            main = plot_titul)
    # })
    # # Volcano plot rendering
    
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
        req(input$selected_tissue)
        
        # Získej reaktivní hodnoty z inputů
        padj_cutoff <- if (!is.null(input$padj_cutoff)) input$padj_cutoff else 0.05
        logfc_cutoff <- if (!is.null(input$logfc_cutoff)) input$logfc_cutoff else 1
        top_n <- if (!is.null(input$top_n)) input$top_n else 0
        
        # Připrav data
        dt <- prepare_volcano(data(), input$selected_tissue)
        
        # Vytvoř plot s cutoffs a top_n
        volcanoPlot(dt, input$selected_tissue, top_n = top_n, padj_cutoff = padj_cutoff, logfc_cutoff = logfc_cutoff)
      })
    })
    
    # Re-render volcano plot když se změní cutoffs nebo top_n
    observeEvent(c(input$padj_cutoff, input$logfc_cutoff, input$top_n), {
      req(data())
      req(input$selected_tissue)
      
      output$volcanoPlot_blood <- renderPlotly({
        # Získej reaktivní hodnoty z inputů
        padj_cutoff <- if (!is.null(input$padj_cutoff)) input$padj_cutoff else 0.05
        logfc_cutoff <- if (!is.null(input$logfc_cutoff)) input$logfc_cutoff else 1
        top_n <- if (!is.null(input$top_n)) input$top_n else 0
        
        # Připrav data
        dt <- prepare_volcano(data(), input$selected_tissue)
        
        # Vytvoř plot s cutoffs a top_n
        volcanoPlot(dt, input$selected_tissue, top_n = top_n, padj_cutoff = padj_cutoff, logfc_cutoff = logfc_cutoff)
      })
    }, ignoreInit = TRUE)
    
    
    
    # heatmap_matrix <- reactive({
    #   req(data())  # Ujisti se, že data jsou dostupná
    #   
    #   data_dt <- as.data.table(data())
    #   
    #   if(expr_flag == "all_genes"){
    #     p_adj_cols <- grep("^p_adj_", names(data_dt), value = TRUE)
    #     data_dt[, (p_adj_cols) := lapply(.SD, as.numeric), .SDcols = p_adj_cols]
    #     
    #     # Vybrat top 20 genů pro každou tkáň
    #     top_20_by_tissue <- lapply(p_adj_cols, function(col) {
    #       data_dt[get(col) > 0][order(get(col)), .(
    #         geneid, feature_name,
    #         tissue = gsub("^p_adj_", "", col),
    #         log2FC = get(gsub("p_adj", "log2FC", col)),
    #         p_adj = get(col)
    #       )][1:20]
    #     })
    #     
    #     top_20_dt <- unique(rbindlist(top_20_by_tissue))
    #     heatmap_data <- dcast.data.table(top_20_dt, geneid + feature_name ~ tissue, value.var = "log2FC", fill = NA)
    #     
    #     
    #   } else {
    #     log2FC_cols <- grep("^log2FC_", names(data_dt), value = TRUE)
    #     long_data <- melt.data.table(data_dt, id.vars = c("geneid", "feature_name"), measure.vars = log2FC_cols, variable.name = "tissue", value.name = "log2FC")
    #     long_data[, tissue := gsub("^log2FC_", "", tissue)]
    #     heatmap_data <- dcast.data.table(long_data, geneid + feature_name ~ tissue, value.var = "log2FC", fill = NA)
    #     
    #   }
    #   
    #   
    #   heatmap_matrix <- as.matrix(heatmap_data[, -c("geneid", "feature_name"), with = FALSE])
    #   heatmap_matrix <- apply(heatmap_matrix, 2, as.numeric)
    #   rownames(heatmap_matrix) <- heatmap_data$feature_name
    #   
    #   # Ošetření NA hodnot
    #   heatmap_matrix[is.na(heatmap_matrix)] <- 0
    #   
    #   return(heatmap_matrix)
    # })
    
    
  })
}