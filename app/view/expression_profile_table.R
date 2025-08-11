# app/view/expression_profile_table.R

box::use(
  shiny[moduleServer, NS, tagList, fluidRow, fluidPage, column, tabPanel, reactive, req, observe, div, observeEvent, reactiveVal, icon, splitLayout, h4, bindEvent,
        updateSelectInput, selectInput, numericInput, actionButton, renderPlot, plotOutput, uiOutput, renderUI, verbatimTextOutput, renderPrint, reactiveValues, isolate,downloadButton],
  reactable,
  bs4Dash[box],
  reactable[colDef, reactableOutput, renderReactable, reactable, getReactableState, colGroup, JS],
  htmltools[tags,HTML],
  plotly[plotlyOutput, renderPlotly, toWebGL],
  reactablefmtr[pill_buttons, data_bars],
  utils[head],
  shinyWidgets[radioGroupButtons, checkboxGroupButtons, updateCheckboxGroupButtons,prettyCheckboxGroup, updatePrettyCheckboxGroup, dropdown, dropdownButton, actionBttn,
               awesomeCheckboxGroup, pickerInput, updatePickerInput],
  data.table[rbindlist, dcast.data.table, as.data.table, melt.data.table, copy],
  grDevices[colorRampPalette],
  pheatmap[pheatmap],
  stats[setNames],
  magrittr[`%>%`],
  shinyalert[shinyalert,useShinyalert],
  shinyjs[useShinyjs,hide,show],
  data.table[data.table,as.data.table,is.data.table]
)

box::use(
  app/logic/plots[prepare_barPlot_data, create_barPlot, prepare_volcano, volcanoPlot, ggvolcanoPlot, classify_volcano_genes],
  app/logic/waiters[use_spinner],
  app/logic/load_data[get_inputs, load_data],
  app/logic/reactable_helpers[custom_colGroup_setting],
  app/logic/filter_columns[getColFilterValues,map_checkbox_names,colnames_map_list,generate_columnsDef],
  app/logic/prepare_table[prepare_expression_table, set_pathway_colors, get_tissue_list],
  app/logic/networkGraph_helper[get_pathway_list],
  app/logic/session_utils[create_session_handlers,safe_extract]
)


# Load and process data table
input_data <- function(sample,expr_flag){
  input_files <- get_inputs("per_sample_file") 
  # message("Loading data for expression profile: ", input_files$expression.files)
  data <- load_data(input_files$expression.files,"expression",sample,expr_flag) #expr_flag = "all_genes"|"genes_of_interest" #sample = "FZ0711"
  dt <- prepare_expression_table(data,expr_flag)
  return(dt)
}



ui <- function(id) {
  ns <- NS(id)
  useShinyjs()
  tagList(
    tags$head(tags$style(HTML(".btn-default.hover,.btn-default:active,.btn-default:hover {background-color: #fff; box-shadow: 0 5px 11px 0 rgba(0, 0, 0, 0.18), 0 4px 15px 0 rgba(0, 0, 0, 0.15); transition: box-shadow .4s ease-out;}"))),
    fluidRow(
      div(style = "width: 100%; text-align: right;",
          dropdownButton(label = NULL,right = TRUE,width = "240px",icon = HTML('<i class="fa-solid fa-download download-button"></i>'),
                         selectInput(ns("export_data_table"), "Select data:", choices = c("All data" = "all", "Filtered data" = "filtered")),
                         selectInput(ns("export_format_table"), "Select format:", choices = c("CSV" = "csv", "TSV" = "tsv", "Excel" = "xlsx")),
                         downloadButton(ns("Table_download"),"Download")),
          filterTab_ui(ns("filterTab_dropdown")))),
      use_spinner(reactableOutput(ns("expression_table"))),
    div(
      tags$br(),
      actionButton(ns("selectDeregulated_button"), "Select deregulated genes for report", status = "info"),
      tags$br(),
      fluidRow(
        column(8,reactableOutput(ns("selectDeregulated_tab")))),
      tags$br(),
      fluidRow(
        column(3,actionButton(ns("delete_button"),"Delete genes", icon = icon("trash-can"))))
    ),
    tags$br(),
    plot_ui(ns("plot"))
    
  )
}

### its on purpose that its just expression_var instead of shared_data$expression_var
server <- function(id,  patient, expr_tag, expression_var) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Načtení dat
    data <- reactive({
      input_data(patient, expr_tag)
    })

    colnames_list <- getColFilterValues("expression",expr_tag) # gives list of all_columns and default_columns
    map_list <- colnames_map_list("expression",expr_tag, colnames_list$all_columns) # gives list of all columns with their column definitions
    mapped_checkbox_names <- map_checkbox_names(map_list) # gives list of all columns with their display names for checkbox

    filter_state <- filterTab_server("filterTab_dropdown",colnames_list, data(),mapped_checkbox_names,expr_tag)
    
    # Reaktivní hodnoty filtrů
    selected_tissues_final <- reactiveVal(get_tissue_list())
    selected_pathway_final <- reactiveVal(get_pathway_list(expr_tag))
    log2fc_bigger1_final <- reactiveVal(NULL)
    log2fc_smaller1_final <- reactiveVal(NULL)
    pval_final <- reactiveVal(NULL)
    padj_final <- reactiveVal(NULL)
    log2fc_bigger1_btn_final <- reactiveVal(FALSE)
    log2fc_smaller1_btn_final <- reactiveVal(FALSE)
    pval_btn_final <- reactiveVal(FALSE)
    padj_btn_final <- reactiveVal(FALSE)
    selected_columns <- reactiveVal(colnames_list$default_columns)
    selected_genes <- reactiveVal(data.frame(patient = character(), feature_name = character(), geneid = character()))
    

      
    # Filtrace dat
    filtered_data <- reactive({
      req(data())
      message("▶ filtered_data computed")
      df <- copy(data())
      base_cols <- c("sample", "feature_name", "geneid", "pathway", "mean_log2FC")
      
      # --- Pathways filtr ---
      pathways_selected <- selected_pathway_final()
      if (!is.null(pathways_selected) && length(pathways_selected) > 0 && length(pathways_selected) < length(get_pathway_list(expr_tag))) {
        pattern <- paste(pathways_selected, collapse = "|")
        df <- df[grepl(pattern, pathway)]
      }
      
      # --- Tkáně a prahové hodnoty ---
      for (filter_name in c("log2fc_bigger1", "log2fc_smaller1", "pval", "padj")) {
        tissues <- get(paste0(filter_name, "_final"))()
        btn_state <- get(paste0(filter_name, "_btn_final"))()
        if (btn_state && length(tissues) > 0) {
          for (tissue in tissues) {
            col <- switch(filter_name,
                          "log2fc_bigger1" = paste0("log2FC_", tissue),
                          "log2fc_smaller1" = paste0("log2FC_", tissue),
                          "pval" = paste0("p_value_", tissue),
                          "padj" = paste0("p_adj_", tissue))
            if (col %in% names(df)) {
              df <- df[
                switch(filter_name,
                       "log2fc_bigger1" = get(col) > 1,
                       "log2fc_smaller1" = get(col) < -1,
                       "pval" = get(col) < 0.05,
                       "padj" = get(col) < 0.05)
              ]
            }
          }
        }
      }
      
      # --- Výběr sloupců ---
      tissues <- selected_tissues_final()
      if (is.null(tissues) || length(tissues) == 0) return(df[, ..base_cols])
      selected_cols <- unlist(lapply(tissues, function(tissue) {
        c(paste0("log2FC_", tissue), paste0("p_value_", tissue), paste0("p_adj_", tissue))
      }))
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
    
    
    output$expression_table <- renderReactable({
      req(filtered_data())
      req(column_defs())
      message("▶ Rendering reactable for expressions: ",expr_tag)
      filtered_data <- filtered_data() 
      deregulated_genes <- selected_genes() # seznam variant, které byly označeny jako patogenní
      
      reactable(
        as.data.frame(filtered_data),
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
          gene_in_row <- filtered_data$feature_name[index]
          var_in_row <- filtered_data$geneid[index]
          if (var_in_row %in% deregulated_genes$geneid &           # Pokud je aktuální řádek v seznamu patogenních variant, zvýrazníme ho
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
    
    
    # Sledování vybraného řádku a genů
    selected_gene <- reactive({
      selected_row <- getReactableState("expression_table", "selected")
      req(selected_row)
      filtered_data()[selected_row, c("feature_name","geneid")]  # Získání varianty z vybraného řádku
      # message("data expression tab: ", filtered_data()[selected_row, c("feature_name","geneid")])
    })
    
    # Akce po kliknutí na tlačítko pro přidání varianty
    observeEvent(input$selectDeregulated_button, {
      selected_rows <- getReactableState("expression_table", "selected")
      req(selected_rows)
      
      new_variants <- filtered_data()[selected_rows, c("sample", "feature_name", "geneid", "pathway", "mean_log2FC")]# c("feature_name","geneid","log2FC")
      new_variants$sample <- patient
      
      current_variants <- selected_genes()  # Stávající přidané varianty
      new_unique_variants <- new_variants[!(new_variants$feature_name %in% current_variants$feature_name &       # Porovnání - přidáme pouze ty varianty, které ještě nejsou v tabulce
                                              new_variants$geneid %in% current_variants$geneid), ]
      
      if (nrow(new_unique_variants) > 0) selected_genes(rbind(current_variants, new_unique_variants))
      
      # Aktualizace globální proměnné shared_data$expression_var:
      global_data <- expression_var()

      # Pokud je NULL nebo nemá správnou strukturu, inicializujeme
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
      
      # Přidáme nově aktualizované lokální data daného pacienta
      updated_global_data <- rbind(global_data, selected_genes())
      expression_var(updated_global_data)
    })
    
    output$selectDeregulated_tab <- renderReactable({
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

    observeEvent(input$delete_button, {
      rows <- getReactableState("selectDeregulated_tab", "selected")
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
      
      if (nrow(updated_variants) == 0) {
        hide("delete_button")
      }
    })
    
    # Při stisku tlačítka pro výběr
    observeEvent(input$selectDeregulated_button, {
      if (is.null(selected_genes()) || nrow(selected_genes()) == 0) {
        # Pokud nejsou vybrány žádné řádky, zůstaň u původního stavu
        # variant_selected(FALSE)
        hide("delete_button")
        shinyalert(
          title = "No deregulated genes selected",
          text = "Please select the deregulated genes for report from table above.",
          type = "warning",
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          callbackR = function(value) {
            # value bude TRUE pro OK, FALSE pro "Go to variant"
            if (!value) {
              # updateTabItems(session = session$userData$parent_session,  # použijeme parent session
              #                inputId = "sidebar_menu",  # bez namespace
              #                selected = "fusion_genes")
            }})
      } else {
        # Pokud jsou nějaké řádky vybrány, nastav fusion_selected na TRUE
        # variant_selected(TRUE)
        
        # Zobraz tlačítka pomocí shinyjs
        show("delete_button")
      }
    })

    observe({
      genes <- selected_genes()
      
      if (!is.null(genes) && nrow(genes) > 0) {
        show("delete_button")
      } else {
        hide("delete_button")
      }
    })

    
    # Obsluha tlačítka Confirm
    observeEvent(filter_state$confirm(), {
      selected_tissues_final(filter_state$selected_tissue())
      selected_pathway_final(filter_state$selected_pathway())
      selected_columns(filter_state$selected_columns())
      
      log2fc_bigger1_final(filter_state$log2fc_bigger1_tissue())
      log2fc_smaller1_final(filter_state$log2fc_smaller1_tissue())
      pval_final(filter_state$pval_tissue())
      padj_final(filter_state$padj_tissue())
      
      log2fc_bigger1_btn_final("log2FC > 1" %in% filter_state$log2fc_bigger1_btn())
      log2fc_smaller1_btn_final("log2FC < -1" %in% filter_state$log2fc_smaller1_btn())
      pval_btn_final("p-value < 0.05" %in% filter_state$pval_btn())
      padj_btn_final("p-adj < 0.05" %in% filter_state$padj_btn())
    })
    
    
    plot_server("plot", patient, data, expr_tag) 
    
    ###########################
    ## get / restore session ##
    ###########################
    
    session_handlers <- create_session_handlers(
      selected_inputs = list(
        selected_tissue = selected_tissues_final,
        selected_pathway = selected_pathway_final,
        selected_columns = selected_columns,
        
        log2fc_bigger1_tissue = log2fc_bigger1_final,
        log2fc_smaller1_tissue = log2fc_smaller1_final,
        pval_tissue = pval_final,
        padj_tissue = padj_final,
        
        log2fc_bigger1_btn = log2fc_bigger1_btn_final,
        log2fc_smaller1_btn = log2fc_smaller1_btn_final,
        pval_btn = pval_btn_final,
        padj_btn = padj_btn_final,
        
        selected_genes = selected_genes
      ),
      filter_state = filter_state
    )
    

    
    return(list(
      get_session_data = session_handlers$get_session_data,
      restore_session_data = session_handlers$restore_session_data,
      filter_state = filter_state
    ))
  })
}


filterTab_server <- function(id,colnames_list, data, mapped_checkbox_names,expr_tag) {
  moduleServer(id, function(input, output, session) {

    observe({
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", choices = mapped_checkbox_names[order(mapped_checkbox_names)], selected = colnames_list$default_columns,
                                prettyOptions = list(status = "primary",icon = icon("check"),outline = FALSE))
      updatePickerInput(session, "filter_pathway",
                  choices = get_pathway_list(expr_tag), selected = character(0),
options = list(`live-search` = TRUE,`actions-box` = TRUE,`multiple-separator` = ", ",`none-selected-text` = "Select pathways",`width` = "100%",`virtual-scroll` = 10,`tick-icon` = "fa fa-check",`dropupAuto` = FALSE)
      )
    })
  
    observe({
      updateCheckboxGroupButtons(session, "log2fc_bigger1_btn", selected = if (length(input$log2fc_bigger1_tissue) > 0) "log2FC > 1" else character(0))
    }) %>% bindEvent(input$log2fc_bigger1_tissue)
    
    observe({
      updateCheckboxGroupButtons(session, "log2fc_smaller1_btn", selected = if (length(input$log2fc_smaller1_tissue) > 0) "log2FC < -1" else character(0))
    }) %>% bindEvent(input$log2fc_smaller1_tissue)
    
    observe({
      updateCheckboxGroupButtons(session, "pval_btn",  selected = if (length(input$pval_tissue) > 0) "p-value < 0.05" else character(0))
    }) %>% bindEvent(input$pval_tissue)
    
    observe({
      updateCheckboxGroupButtons(session, "padj_btn", selected = if (length(input$padj_tissue) > 0) "p-adj < 0.05" else character(0))
    }) %>% bindEvent(input$padj_tissue)
    
    
    
    observeEvent(input$show_all, {
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = colnames_list$all_columns)
    })
    
    observeEvent(input$show_default, {
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = colnames_list$default_columns)
    })

    
    restore_ui_inputs <- function(data) {
      if (!is.null(data$selected_tissue)) updateCheckboxGroupButtons(session, "select_tissue", selected = safe_extract(data$selected_tissue))
      if (!is.null(data$selected_pathway)) updateCheckboxGroupButtons(session, "filter_pathway", selected = safe_extract(data$selected_pathway))
      if (!is.null(data$selected_columns)) updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = safe_extract(data$selected_columns))
      
      if (!is.null(data$log2fc_bigger1_tissue)) updatePickerInput(session, "log2fc_bigger1_tissue", selected = safe_extract(data$log2fc_bigger1_tissue))
      if (!is.null(data$log2fc_smaller1_tissue)) updatePickerInput(session, "log2fc_smaller1_tissue", selected = safe_extract(data$log2fc_smaller1_tissue))
      if (!is.null(data$pval_tissue)) updatePickerInput(session, "pval_tissue", selected = safe_extract(data$pval_tissue))
      if (!is.null(data$padj_tissue)) updatePickerInput(session, "padj_tissue", selected = safe_extract(data$padj_tissue))
      
      if (!is.null(data$log2fc_bigger1_btn)) updateCheckboxGroupButtons(session, "log2fc_bigger1_btn", selected = safe_extract(data$log2fc_bigger1_btn))
      if (!is.null(data$log2fc_smaller1_btn)) updateCheckboxGroupButtons(session, "log2fc_smaller1_btn", selected = safe_extract(data$log2fc_smaller1_btn))
      if (!is.null(data$pval_btn)) updateCheckboxGroupButtons(session, "pval_btn", selected = safe_extract(data$pval_btn))
      if (!is.null(data$padj_btn)) updateCheckboxGroupButtons(session, "padj_btn", selected = safe_extract(data$padj_btn))

    }
    
    return(list(
      confirm = reactive(input$confirm_btn),
      selected_tissue = reactive(input$select_tissue),
      selected_pathway = reactive(input$filter_pathway),
      selected_columns = reactive(input$colFilter_checkBox),
      
      log2fc_bigger1_tissue = reactive(input$log2fc_bigger1_tissue),
      log2fc_smaller1_tissue = reactive(input$log2fc_smaller1_tissue),
      pval_tissue = reactive(input$pval_tissue),
      padj_tissue = reactive(input$padj_tissue),
      
      log2fc_bigger1_btn = reactive(input$log2fc_bigger1_btn),
      log2fc_smaller1_btn = reactive(input$log2fc_smaller1_btn),
      pval_btn = reactive(input$pval_btn),
      padj_btn = reactive(input$padj_btn),
      
      restore_ui_inputs = restore_ui_inputs
    ))
    
  })
}



filterTab_ui <- function(id){
  ns <- NS(id)
  
  
  tagList(
    tags$head(tags$style(HTML(".sw-dropdown .action-button .my-filter-btn {background-color: transparent; border: none; margin-top: -1px;}
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
      fluidRow(style = "display: flex; align-items: stretch;",
         column(6,
            box(width = 12, title = tags$div(style = "padding-top: 8px;","Filter data by:"),closable = FALSE, collapsible = FALSE,style = "height: 100%;",
                fluidRow(#class = "filterTab-select-tissue",
                        checkboxGroupButtons(ns("select_tissue"),"Tissues:",choices = get_tissue_list(),selected = get_tissue_list(),individual = TRUE)),
                fluidRow(#class = "filter_pathway",
                    pickerInput(ns("filter_pathway"), "Pathways",choices = character(0), multiple = TRUE,
                                options = list(`live-search` = TRUE,`actions-box` = TRUE,`multiple-separator` = ", ",`none-selected-text` = "Select pathways",`width` = "100%",`virtual-scroll` = 10,`tick-icon` = "fa fa-check",`dropupAuto` = FALSE))),
                    tags$span("Tissue values:", style = "font-size: 1rem; font-weight: bold; isplay: inline-block; margin-bottom: .5rem;"),
                    div(style = "display: flex; gap: 10px; margin-bottom: -10px;",
                        # div(style = "width: 100%",
                        checkboxGroupButtons(ns("log2fc_bigger1_btn"),choices = "log2FC > 1",selected = "",individual = TRUE),
                        div(class = "filter_pathway",
                            pickerInput(ns("log2fc_bigger1_tissue"),choices = get_tissue_list(), multiple = TRUE, options = list(`live-search` = TRUE,`actions-box` = TRUE,`multiple-separator` = ", ",`none-selected-text` = "Select tissue",`width` = "100%",`virtual-scroll` = 10,`tick-icon` = "fa fa-check",`dropupAuto` = FALSE)
                                        ))),
                    div(style = "display: flex; gap: 10px; margin-bottom: -10px;",
                        checkboxGroupButtons(ns("log2fc_smaller1_btn"),choices = "log2FC < -1",selected = "",individual = TRUE),
                        div(class = "filter_pathway",
                            pickerInput(ns("log2fc_smaller1_tissue"), choices = get_tissue_list(), multiple = TRUE, options = list(`live-search` = TRUE,`actions-box` = TRUE,`multiple-separator` = ", ",`none-selected-text` = "Select tissue",`width` = "100%",`virtual-scroll` = 10,`tick-icon` = "fa fa-check",`dropupAuto` = FALSE)))),
                    div(style = "display: flex; gap: 10px; margin-bottom: -10px;",
                        checkboxGroupButtons(ns("pval_btn"),choices = "p-value < 0.05",selected = "",individual = TRUE),
                        div(class = "filter_pathway",
                            pickerInput(ns("pval_tissue"), choices = get_tissue_list(), multiple = TRUE, options = list(`live-search` = TRUE,`actions-box` = TRUE,`multiple-separator` = ", ",`none-selected-text` = "Select tissue",`width` = "100%",`virtual-scroll` = 10,`tick-icon` = "fa fa-check",`dropupAuto` = FALSE)))),
                    div(style = "display: flex; gap: 10px; margin-bottom: -10px;",
                        checkboxGroupButtons(ns("padj_btn"),choices = "p-adj < 0.05",selected = "",individual = TRUE),
                        div(class = "filter_pathway",
                            pickerInput(ns("padj_tissue"), choices = get_tissue_list(), multiple = TRUE, options = list(`live-search` = TRUE,`actions-box` = TRUE,`multiple-separator` = ", ",`none-selected-text` = "Select tissue",`width` = "100%",`virtual-scroll` = 10,`tick-icon` = "fa fa-check",`dropupAuto` = FALSE)))))),
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
    # )
    )
  )
}



plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("selected_plot_ui"))
  )
}


plot_server <- function(id, patient, data, expr_flag) {
  moduleServer(id, function(input, output, session) {
    
    tissue_names <- get_tissue_list()
    
    ### render ui ###
    
    output$selected_plot_ui <- renderUI({
      ns <- session$ns
      
      width_px <- if (expr_flag == "all_genes") "600px" else "600px"
      height_px <- if (expr_flag == "all_genes") "800px" else "1500px"
      
      tagList(
        div(class = "collapsible-box",
            box(width = 12, closable = FALSE, collapsible = TRUE, collapsed = TRUE, title = tags$div(style = "padding-top: 8px;","Volcano plot"),
                column(6, div(class = "filterTab-select-tissue",
                              radioGroupButtons(ns("selected_tissue"), "Choose a tissue :", choices = get_tissue_list(), justified = TRUE))),
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
      
      print(paste("Rendering heatmap for patient:", patient))
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
      message("Selected tissue: ",input$selected_tissue)
      
      output$volcanoPlot_blood <- renderPlotly({
        req(data())
        # toWebGL()
        volcanoPlot(prepare_volcano(data(), input$selected_tissue), input$selected_tissue)
      })
    })
    
    
    
    heatmap_matrix <- reactive({
      req(data())  # Ujisti se, že data jsou dostupná
      print(paste("Generating heatmap for patient:", patient))
      
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
      
      print(paste("Heatmap matrix generated for:", patient))
      return(heatmap_matrix)
    })
    
    
  })
}