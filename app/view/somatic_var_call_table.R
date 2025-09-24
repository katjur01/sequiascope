#app/view/somatic_var_call_table.R

box::use(
  shiny[NS, sliderInput, fluidRow, column, tagList, br, uiOutput, plotOutput, downloadButton, actionButton, numericInput, renderPlot, fluidPage, selectInput,
        icon,div,tabPanel,moduleServer,downloadHandler,observe, observeEvent,reactive,renderUI,updateSliderInput,updateNumericInput,req,isolate,
        reactiveVal,showModal,modalDialog,modalButton,isTruthy],
  reactable[colDef,reactableOutput,renderReactable,reactable,getReactableState,JS],
  bs4Dash[box,tabsetPanel,updateTabItems,updateNavbarTabs],
  htmltools[tags, span,HTML],
  shinyWidgets[pickerInput,updatePickerInput,dropdownButton,prettyCheckboxGroup,updatePrettyCheckboxGroup,actionBttn,pickerOptions,dropdown],
  networkD3[sankeyNetwork,renderSankeyNetwork,sankeyNetworkOutput],
  billboarder[billboarderOutput],
  stats[setNames],
  shinyalert[shinyalert,useShinyalert],
  shinyjs[useShinyjs,hide,show],
  data.table[fread,data.table,as.data.table,copy,is.data.table,uniqueN],
  magrittr[`%>%`],
  jsonlite[read_json],
)

box::use(
  app/logic/prepare_main_table_and_filters[map_column_names,map_gene_region_names,map_clin_sig_names],
  app/logic/vaf_plot[generate_vaf],
  app/logic/sankey_plot[sankey_plot],
  app/logic/waiters[use_spinner],
  app/view/selected_variants[render_selected_variants_ui,render_selected_variants_table,handle_delete_variant, handle_confirm_selected],
  app/view/export_functions[get_table_download_handler,get_sankey_download_handler,get_hist_download_handler],
  app/logic/load_data[get_inputs,load_data],
  app/logic/prepare_table[prepare_somatic_table,colFilter],
  app/logic/reactable_helpers[create_clinvar_filter,create_consequence_filter],
  app/logic/filter_columns[map_checkbox_names,colnames_map_list,generate_columnsDef],
  app/logic/session_utils[create_session_handlers,safe_extract, register_module]
)


# UI funkce pro modul nastavujici vzhled veskerych grafickych prvku v zalozce somatic variants

ui <- function(id) {
  ns <- NS(id)
  useShinyjs()
  tagList(

    fluidRow(
      div(style = "width: 100%; text-align: right;",
         dropdownButton(label = NULL,right = TRUE,width = "240px",icon = HTML('<i class="fa-solid fa-download download-button"></i>'),
           selectInput(ns("export_data_table"), "Select data:", choices = c("All data" = "all", "Filtered data" = "filtered")),
           selectInput(ns("export_format_table"), "Select format:", choices = c("CSV" = "csv", "TSV" = "tsv", "Excel" = "xlsx")),
           downloadButton(ns("Table_download"),"Download")),
         filterTab_ui(ns("filterTab_dropdown"))
         )),
     use_spinner(reactableOutput(ns("somatic_var_call_tab"))),
     tags$br(),
     div(style = "display: flex; justify-content: space-between; align-items: top; width: 100%;",
       div(
         tags$br(),
         actionButton(ns("selectPathogenic_button"), "Select variants as possibly oncogenic", status = "info"),
         tags$br(),
         fluidRow(
           column(12,reactableOutput(ns("selectPathogenic_tab")))),
         tags$br(),
         fluidRow(
           column(3,actionButton(ns("delete_button"),"Delete variants", icon = icon("trash-can"))))
       ),
       dropdown(label = "IGV", status = "primary", icon = icon("play"), right = TRUE, size = "md", width = "230px", 
         pickerInput(ns("idpick"), "Select patients for IGV:", choices = NULL, options = pickerOptions(actionsBox = FALSE, size = 4, maxOptions = 4, dropupAuto = FALSE, maxOptionsText = "Select max. 4 patients"),multiple = TRUE),
         div(style = "display: flex; justify-content: center; margin-top: 10px;",
             actionBttn(ns("go2igv_button"), label = "Go to IGV", style = "stretch", color = "primary", size = "sm", individual = TRUE)
         )
       )
     ),
     uiOutput(ns("confirm_button_ui")),
     tags$br(),
     div(class = "collapsible-box",
       box(width = 12, closable = FALSE,collapsible = TRUE, collapsed = TRUE, title = tags$div(style = "padding-top: 8px;","Tumor variant frequency histogram"),
         dropdownButton(label = "Export Circos Plot",right = TRUE,width = "240px",icon = HTML('<i class="fa-solid fa-download download-button"></i>'),
           downloadButton(ns("Hist_download"),"Download as PNG")),
         br(),br(),
         div(style = "width: 100%; margin: auto;",
           use_spinner(plotOutput(ns("Histogram"),height = "480px"))
         )
       )
     ),
     div(class = "collapsible-box",
       box(width = 12,closable = FALSE,collapsible = TRUE,collapsed = TRUE,title = tags$div(style = "padding-top: 8px;","Sankey diagram"),
         dropdownButton(label = "Export Sankey Plot",right = FALSE,width = "240px",icon = HTML('<i class="fa-solid fa-download download-button"></i>'),
           selectInput(ns("export_format"), "Select format:", choices = c("HTML" = "html", "PNG" = "png")),
           downloadButton(ns("Sankey_download"),"Download")),
         br(),
         tags$div(style = "display: flex; justify-content: space-between; width: 100%;",
           tags$span(style = "margin-left: 3cm;", "Variant"),
           tags$span(style = "margin-right: 2cm;", "Gene"),
           tags$span(style = "margin-right: 10cm;", "Pathway")),
         uiOutput(ns("diagram"))
       )
     )
  )
}

# Serverova funkce pro modul definující funkce veskerych prvku v zalozce somatic variants
server <- function(id, selected_samples, shared_data, file) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    is_restoring_session <- reactiveVal(FALSE)
    
    observe({
      req(data())
      if (!is.null(file$TMB)) {
        mutation_load <- load_data(file$TMB, "TMB", selected_samples)
        overview_dt <- data.table(
          for_review = uniqueN(data()[gnomAD_NFE <= 0.01 & tumor_depth > 10 & Consequence != "synonymous_variant" &
                                        (gene_region == "exon" | gene_region == "splice"), unique(var_name)]),
          TMB = unique(mutation_load$TMB))
      } else {
        overview_dt <- data.table(
          for_review = uniqueN(data()[gnomAD_NFE <= 0.01 & tumor_depth > 10 & Consequence != "synonymous_variant" &
                                        (gene_region == "exon" | gene_region == "splice"), unique(var_name)]))
      }

      shared_data$somatic.overview[[ selected_samples ]] <- overview_dt
    })
    
    # Load and process data table
    prepare_data <- reactive({
      message("Loading input data for somatic: ", file$variant)
      data <- load_data(file$variant, "varcall", selected_samples)
      prepare_somatic_table(data, colnames(data))
    })
    
    data <- reactive(prepare_data()$dt)
    colnames_list <- prepare_data()$columns

# colnames_list <- getColFilterValues("somatic") # gives list of all_columns and default_columns
map_list <- colnames_map_list("somatic") # gives list of all columns with their column definitions
mapped_checkbox_names <- map_checkbox_names(map_list) # gives list of all columns with their display names for checkbox


filter_state <- filterTab_server("filterTab_dropdown",colnames_list, data(),mapped_checkbox_names, is_restoring = is_restoring_session)

############

selected_tumor_depth <- reactiveVal(NULL)
selected_gnomAD_min  <- reactiveVal(NULL)
selected_gene_region <- reactiveVal(NULL)
selected_clinvar_sig <- reactiveVal(NULL)
selected_consequence <- reactiveVal(NULL)
selected_columns <- reactiveVal(colnames_list$default_columns)
selected_variants <- reactiveVal(data.frame(patient = character(),var_name = character(), Gene_symbol = character()))


  # Call generate_columnsDef to generate colDef setting for reactable
  column_defs <- reactive({
    req(data())
    req(selected_columns())
    generate_columnsDef(names(data()), selected_columns(), "somatic", map_list)
  })
  
  
  filtered_data <- reactive({
    req(data())
    dt <- copy(data())
  
    if (!is.null(selected_tumor_depth())) {
      dt <- dt[selected_tumor_depth() <= tumor_depth, ]
      if (nrow(dt) == 0) {
        message("⚠️ Tumor depth filter removed all rows. Returning empty data.table with original structure.")
        return(dt)}}
  
    if (!is.null(selected_gnomAD_min())) {
      dt <- dt[gnomAD_NFE <= selected_gnomAD_min()]
      if (nrow(dt) == 0) {
        message("⚠️ gnomAD filter removed all rows. Returning empty data.table with original structure.")
        return(dt)}}
  
    if (!is.null(selected_gene_region()) && length(selected_gene_region()) > 0) {
      dt <- dt[gene_region %in% selected_gene_region(), ]
      if (nrow(dt) == 0) {
        message("⚠️ Gene region filter removed all rows. Returning empty data.table with original structure.")
        return(dt)}}
  
    if (!is.null(selected_consequence()) && length(selected_consequence()) > 0) {
      if (nrow(dt) > 0) dt <- create_consequence_filter(dt, selected_consequence()) else message("⚠️ Skipping consequence filter - no rows to filter.")}
  
    return(dt)
  })

    output$somatic_var_call_tab <- renderReactable({
      req(filtered_data())
      req(column_defs())
      message("🟢 Rendering Reactable for somatic")
      filtered_data <- filtered_data()
      pathogenic_variants <- selected_variants() # seznam variant, které byly označeny jako patogenní

      reactable(
        as.data.frame(filtered_data),
        columns = column_defs(),
        resizable = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 20, 50, 100),
        defaultPageSize = 20,
        striped = TRUE,
        wrap = FALSE,
        highlight = TRUE,
        outlined = TRUE,
        defaultColDef = colDef(align = "center", sortNALast = TRUE),
        defaultSorted = list("fOne" = "desc", "CGC_Somatic" = "desc"),
        rowStyle = if (!is.null(pathogenic_variants) && nrow(pathogenic_variants) > 0) {
          function(index) {
            gene_in_row <- filtered_data$Gene_symbol[index]
            var_in_row <- filtered_data$var_name[index]
            if (var_in_row %in% pathogenic_variants$var_name &           # Pokud je aktuální řádek v seznamu patogenních variant, zvýrazníme ho
                gene_in_row %in% pathogenic_variants$Gene_symbol) {
              list(backgroundColor = "#B5E3B6",fontWeight = "bold")
            } else {
              NULL
            }
          }
        },
        selection = "multiple",
        onClick = JS("function(rowInfo, column, event) {
                        if (event.target.classList.contains('rt-expander') || event.target.classList.contains('rt-expander-button')) {
                        } else {
                            rowInfo.toggleRowSelected();}}"),
        class = "somatic-table"
      )
    })


    # Akce po kliknutí na tlačítko pro přidání varianty
    observeEvent(input$selectPathogenic_button, {
      selected_rows <- getReactableState("somatic_var_call_tab", "selected")
      req(selected_rows)

      new_variants <- filtered_data()[selected_rows, c("var_name", "Gene_symbol","tumor_variant_freq","tumor_depth", "Consequence",
                                                       "HGVSc","HGVSp","variant_type","Feature", "gnomAD_NFE")]  # Získání vybraných variant
      new_variants$sample <- selected_samples

      current_variants <- selected_variants()  # Stávající přidané varianty
      new_unique_variants <- new_variants[!(new_variants$var_name %in% current_variants$var_name &       # Porovnání - přidáme pouze ty varianty, které ještě nejsou v tabulce
                                              new_variants$Gene_symbol %in% current_variants$Gene_symbol), ]

      if (nrow(new_unique_variants) > 0) selected_variants(rbind(current_variants, new_unique_variants))

      # Aktualizace globální proměnné shared_data$somatic.variants:
      global_data <- shared_data$somatic.variants()

      # Pokud je NULL nebo nemá správnou strukturu, inicializujeme
      if (is.null(global_data) || !is.data.table(global_data) || !("sample" %in% names(global_data))) {
        global_data <- data.table(
          sample = character(),
          var_name = character(),
          Gene_symbol = character(),
          tumor_variant_freq = numeric(),
          tumor_depth = integer(),
          Consequence = character(),
          HGVSc = character(),
          HGVSp = character(),
          variant_type = character(),
          Feature = character(),
          gnomAD_NFE = numeric()
        )
      }

      # VŽDY provedeme odstranění záznamů daného pacienta — bezpodmínečně!
      global_data <- global_data[sample != selected_samples]


      # Přidáme nově aktualizované lokální data daného pacienta
      updated_global_data <- rbind(global_data, selected_variants())
      shared_data$somatic.variants(updated_global_data)
    })


    output$selectPathogenic_tab <- renderReactable({
      variants <- selected_variants()
      if (is.null(variants) || nrow(variants) == 0) {
        return(NULL)
      } else {
        variants <- as.data.table(variants)[,.(var_name,Gene_symbol,Consequence,HGVSc,HGVSp,Feature)]
        reactable(
          as.data.frame(variants),
          columns = list(
            var_name = colDef(name = "Variant name"),
            Gene_symbol = colDef(name = "Gene name"),
            Consequence = colDef(minWidth=160)),
          selection = "multiple", onClick = "select")
      }
    })

    observeEvent(input$delete_button, {
      rows <- getReactableState("selectPathogenic_tab", "selected")
      req(rows)

      current_variants <- selected_variants()
      updated_variants <- current_variants[-rows, ]
      selected_variants(updated_variants)

      global_data <- shared_data$somatic.variants()
      # if (!is.null(global_data) && is.data.table(global_data) && "sample" %in% colnames(global_data)) {
      if (!is.null(global_data) && is.data.table(global_data)) {
        global_data <- global_data[sample != selected_samples]
      } else {
        global_data <- data.table(
          sample = character(),
          var_name = character(),
          Gene_symbol = character(),
          tumor_variant_freq = character(),
          tumor_depth = character(),
          Consequence = character(),
          HGVSc = character(),
          HGVSp = character(),
          variant_type = character(),
          Feature = character(),
          gnomAD_NFE = character()
        )
      }

      if (nrow(updated_variants) > 0) {
        updated_global_data <- rbind(global_data, as.data.table(updated_variants))
      } else {
        updated_global_data <- global_data
      }

      shared_data$somatic.variants(updated_global_data)
      session$sendCustomMessage("resetReactableSelection", updated_variants)

      if (nrow(updated_variants) == 0) {
        hide("delete_button")
      }
    })

    # Při stisku tlačítka pro výběr
    observeEvent(input$selectPathogenic_button, {
      if (is.null(selected_variants()) || nrow(selected_variants()) == 0 ) {
        hide("delete_button")

        shinyalert(
          title = "No variant selected",
          text = "Please select the potentially oncogenic variants from table above.",
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
        show("delete_button")
      }
    })

    observe({
      variants <- selected_variants()
      if (!is.null(variants) && nrow(variants) > 0) {
        show("delete_button")
      } else {
        hide("delete_button")
      }
    })


    observeEvent(filter_state$confirm(), {
      message("🟢 Confirm button was clicked")
      selected_tumor_depth(filter_state$tumor_depth())
      selected_gnomAD_min(filter_state$gnomAD_min())
      selected_gene_region(filter_state$gene_region())
      selected_consequence(filter_state$consequence())
      selected_columns(filter_state$selected_columns())
    })

    # plot VAF histogram
    hist <- reactive({
      generate_vaf(filtered_data(),selected_variants())
    })
    output$Histogram <- renderPlot({ hist() },height = 480)

    # Rendering the Sankey network
    p <-reactiveVal()

    sankey_data <- reactive({
      sankey_plot(filtered_data())
    })
    output$sankey_plot <- renderSankeyNetwork({
      p(sankeyNetwork(
        Links = sankey_data()$links, Nodes = sankey_data()$nodes,
        Source = "IDsource", Target = "IDtarget",
        Value = "value", NodeID = "name",
        sinksRight = FALSE, fontSize = 15,
        height = sankey_data()$plot_height, width = "100%"))
      p()
    })
    output$diagram <- renderUI({
      sankeyNetworkOutput(ns("sankey_plot"), height = sankey_data()$plot_height)
    })

    #################
    ## export data ##
    #################
    output$Table_download <- get_table_download_handler(input,selected_samples,data(),filtered_data())
    output$Hist_download <- get_hist_download_handler(selected_samples, hist())
    output$Sankey_download <- get_sankey_download_handler(input, selected_samples, p())   # p = reaktive funkcion returning sankey object

    #############
    ## run IGV ##
    #############
    
    ## update IGV button choices
    observeEvent(shared_data$somatic.patients(), {
      patient_list <- shared_data$somatic.patients()
      if (is.null(patient_list)) patient_list <- character(0)
      prev <- input$idpick; if (is.null(prev)) prev <- character(0)
      sel  <- intersect(prev, patient_list)
      if (!length(sel) && length(patient_list) && !is.null(selected_samples)) {
        sel <- intersect(selected_samples, patient_list)
      }
      updatePickerInput(session, "idpick", choices = patient_list, selected = sel)
    }, ignoreInit = FALSE)
    
    
    observeEvent(input$go2igv_button, {
      selected_empty <- is.null(selected_variants()) || nrow(selected_variants()) == 0
      bam_empty <- is.null(shared_data$somatic.bam) || length(shared_data$somati._bam) == 0

      if (selected_empty || bam_empty) {
        shinyalert(
          title = "No variant or patient selected",
          text = "Please select at least one variant and one patient before inspecting them in IGV.",
          type = "warning",
          showCancelButton = FALSE,
          confirmButtonText = "OK")

      } else {
        shared_data$navigation_context("somatic")   # odkud otevíráme IGV

        bam_path  <- get_inputs("bam_file")
        # bam_list  <- lapply(input$idpick, function(id_val) {
        #   full_path <- grep(paste0(id_val, ".*\\.bam$"), bam_path$dna.tumor_bam, value = TRUE)
        #   list(name = id_val, file = sub(bam_path$path_to_folder, ".", full_path, fixed = TRUE))
        # })

        bam_list <- unlist(
          lapply(input$idpick, function(id_val) {
            ## 1) DNA-tumor BAM
            tumor_path <- grep(paste0(id_val, ".*\\.bam$"), bam_path$dna.tumor_bam, value = TRUE)
            tumor_track <- list(name = paste0(id_val, "tumor"), file = sub(bam_path$path_to_folder, ".", tumor_path, fixed = TRUE))

            ## 2) DNA-normal BAM
            normal_path <- grep(paste0(id_val, ".*\\.bam$"), bam_path$dna.normal_bam, value = TRUE)
            normal_track <- list(name = paste0(id_val, "normal"), file = sub(bam_path$path_to_folder, ".", normal_path, fixed = TRUE))

            list(tumor_track, normal_track)  # pořadí: tumor -> chimeric
          }), recursive = FALSE              # nerozbalujeme úplně, zůstane list tracků
        )

        shared_data$somatic.bam(bam_list)
        message("✔ Assigned somatic_bam: ", paste(sapply(bam_list, `[[`, "file"), collapse = ", "))

        updateNavbarTabs(session = session$userData$parent_session, inputId = "navbarMenu", selected = session$userData$parent_session$ns("hidden_igv"))
      }
    })

    ###########################
    ## get / restore session ##
    ###########################

    session_handlers <- create_session_handlers(
      selected_inputs = list(
        gnomAD_min = selected_gnomAD_min,
        tumor_depth = selected_tumor_depth,
        gene_regions = selected_gene_region,
        consequence = selected_consequence,
        selected_cols = selected_columns,
        selected_vars = selected_variants
      ),
      filter_state = filter_state
    )

    methods <- list(
      get_session_data     = session_handlers$get_session_data,
      restore_session_data = session_handlers$restore_session_data,
      filter_state         = filter_state,
      is_restoring         = is_restoring_session
    )
    
    register_module(shared_data, "somatic", selected_samples, methods)
    # register_module(shared_data, "fusion", selected_samples, methods)    # nové
    return(methods)

  })
}

filterTab_server <- function(id, colnames_list, data, mapped_checkbox_names, is_restoring = NULL) {
  moduleServer(id, function(input, output, session) {
    
    nz <- function(x, default) if (is.null(x) || !length(x)) default else x
    ch <- function(x) trimws(as.character(x))
    
    # Flag pro inicializaci
    initialized <- reactiveVal(FALSE)
    
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
    
    # Funkce pro update consequence
    update_consequence_choices <- function() {
      consequence_split <- unique(unlist(unique(data$consequence_trimws)))
      consequence_list <- sort(unique(ifelse(
        is.na(consequence_split) | consequence_split == "", 
        "missing_value", 
        consequence_split
      )))
      consequence_list <- ch(consequence_list)
      
      # Při první inicializaci použij default, jinak zachovej current
      if (!initialized()) {
        selected <- setdiff(consequence_list, "synonymous variant")
      } else {
        current <- isolate(input$consequence)
        selected <- if (is.null(current)) setdiff(consequence_list, "synonymous variant") else current
        selected <- intersect(ch(selected), consequence_list)  # jen platné hodnoty
      }
      
      updatePrettyCheckboxGroup(
        session, "consequence", 
        choices = consequence_list, 
        selected = selected,
        prettyOptions = list(status = "primary", icon = icon("check"), outline = FALSE)
      )
    }
    
    # Funkce pro update gene regions
    update_gene_region_choices <- function() {
      gene_choices <- sort(unique(ch(data$gene_region)))
      
      # Při první inicializaci použij default (exon, splice), jinak zachovej current
      if (!initialized()) {
        default_regions <- c("exon", "splice")
        # Jen ty, které skutečně existují v datech
        selected <- intersect(default_regions, gene_choices)
      } else {
        current <- isolate(input$gene_regions)
        selected <- if (is.null(current)) intersect(c("exon", "splice"), gene_choices) else current
        selected <- intersect(ch(selected), gene_choices)  # jen platné hodnoty
      }
      
      updatePrettyCheckboxGroup(
        session, "gene_regions", 
        choices = gene_choices, 
        selected = selected,
        prettyOptions = list(status = "primary", icon = icon("check"), outline = FALSE)
      )
    }
    
    # Funkce pro update column choices
    update_column_choices <- function() {
      # Seřaď choices podle názvů
      col_choices_ordered <- mapped_checkbox_names[order(names(mapped_checkbox_names))]
      
      # Normalizuj current selection
      current_selection <- isolate(input$colFilter_checkBox)
      default_selection <- if (!initialized() || is.null(current_selection) || length(current_selection) == 0) {
        colnames_list$default_columns
      } else {
        current_selection
      }
      
      selected_values <- normalize_column_selection(
        selection = default_selection,
        choices_map = col_choices_ordered,
        default_cols = colnames_list$default_columns
      )
      
      updatePrettyCheckboxGroup(
        session, "colFilter_checkBox", 
        choices = col_choices_ordered, 
        selected = selected_values,
        prettyOptions = list(status = "primary", icon = icon("check"), outline = FALSE)
      )
    }
    
    # Funkce pro update numeric inputs
    update_numeric_inputs <- function() {
      if (!initialized()) {
        if (isTruthy(is.na(input$tumor_depth))) {
          updateNumericInput(session, "tumor_depth", value = 10)
        }
        if (isTruthy(is.na(input$gnomAD_min))) {
          updateNumericInput(session, "gnomAD_min", value = 0.01)
        }
      }
    }
    
    # ===== MAIN OBSERVE =====
    
    observe({
      # Pokud probíhá restore session, přeskoč automatické aktualizace
      if (!is.null(is_restoring) && isTruthy(is_restoring())) {
        return()
      }
      
      update_consequence_choices()
      update_gene_region_choices()
      update_column_choices()
      update_numeric_inputs()
      
      if (!initialized()) {
        initialized(TRUE)
      }
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
    
    # ===== RESTORE FUNCTION =====
    
    restore_ui_inputs <- function(state) {
      message("🎯 Restoring filter UI inputs")
      
      if (!is.null(state$gnomAD_min)) {
        val <- safe_extract(state$gnomAD_min)
        message(sprintf("Restoring gnomAD_min: %s", val))
        updateNumericInput(session, "gnomAD_min", value = val)
      }
      
      if (!is.null(state$tumor_depth)) {
        val <- safe_extract(state$tumor_depth)
        message(sprintf("Restoring tumor_depth: %s", val))
        updateNumericInput(session, "tumor_depth", value = val)
      }
      
      if (!is.null(state$gene_regions)) {
        wanted <- ch(safe_extract(state$gene_regions))
        message(sprintf("Restoring gene_regions: %s", paste(wanted, collapse = ", ")))
        updatePrettyCheckboxGroup(session, "gene_regions", selected = wanted)
      }
      
      if (!is.null(state$consequence)) {
        wanted <- ch(safe_extract(state$consequence))
        message(sprintf("Restoring consequence: %s", paste(wanted, collapse = ", ")))
        updatePrettyCheckboxGroup(session, "consequence", selected = wanted)
      }
      
      if (!is.null(state$selected_cols)) {
        wanted <- ch(safe_extract(state$selected_cols))
        message(sprintf("Restoring selected_cols: %s", paste(wanted, collapse = ", ")))
        
        wanted_values <- normalize_column_selection(
          selection = wanted,
          choices_map = mapped_checkbox_names,
          default_cols = colnames_list$default_columns
        )
        
        message(sprintf("Final selected values: %s", paste(wanted_values, collapse = ", ")))
        updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = wanted_values)
      }
    }
    
    # ===== RETURN VALUES =====
    
    return(list(
      confirm           = reactive(input$confirm_btn),
      tumor_depth       = reactive(input$tumor_depth),
      gnomAD_min        = reactive(input$gnomAD_min),
      gene_regions      = reactive(input$gene_regions),
      consequence       = reactive(input$consequence),
      selected_columns  = reactive(input$colFilter_checkBox),
      restore_ui_inputs = restore_ui_inputs
    ))
  })
}


filterTab_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(
        # tags$style(HTML("
        #               .checkbox label {font-weight: normal !important;}
        #               .checkbox-group .checkbox {margin-bottom: 0px !important;}
        #               .my-blue-btn {background-color: #007bff;color: white;border: none;}
        #               .dropdown-menu .bootstrap-select .dropdown-toggle {border: 1px solid #ced4da !important; background-color: #fff !important;
        #                 color: #495057 !important; height: 38px !important; font-size: 16px !important; border-radius: 4px !important;
        #                 box-shadow: none !important;}
        #               .sw-dropdown-content {border: 1px solid #ced4da !important; border-radius: 4px !important; box-shadow: none !important;
        #                 background-color: white !important;}
        #                .glyphicon-triangle-bottom {font-size: 12px !important; line-height: 12px !important; vertical-align: middle;}
        #               #app-somatic_var_call_tab-igv_dropdownButton {width: 230px !important; height: 38px !important; font-size: 16px !important;}
        #               "))
      tags$style(HTML(".dropdown-toggle {border-radius: 0; padding: 0; background-color: transparent; border: none; float: right;margin-top -1px;}
                       .dropdown-toggle::after {display: none !important;}
                       .glyphicon-triangle-bottom {display: none !important; width: 0 !important; margin: 0 !important; padding: 0 !important;}"))
    ),
    dropdownButton(
      label = NULL,
      right = TRUE,
      icon = HTML('<i class="fa-solid fa-filter download-button"></i>'),
      fluidRow(style = "display: flex; align-items: stretch;",
        column(7,
               box(width = 12, title = tags$div(style = "padding-top: 8px;","Filter data by:"),closable = FALSE, collapsible = FALSE,style = "height: 100%;",
                   fluidRow(
                     column(6, numericInput(ns("tumor_depth"), tags$strong("Tumor coverage min"), value = 10, min = 0)),
                     column(6, numericInput(ns("gnomAD_min"), tags$strong("gnomAD NFE min"), value = 0.01, min = 0, max = 1))
                   ),
                   div(class = "two-col-checkbox-group", style = "margin-bottom: 15px;",
                       prettyCheckboxGroup(ns("gene_regions"), label = tags$strong("Gene region"), choices = character(0))),
                   div(class = "two-col-checkbox-group",
                       prettyCheckboxGroup(ns("consequence"), label = tags$strong("Consequence"), choices = character(0))))),
        column(5,
               box(width = 12, title = tags$div(style = "padding-top: 8px;","Select columns:"),closable = FALSE,collapsible = FALSE,height = "100%",
                   div(class = "two-col-checkbox-group",
                     prettyCheckboxGroup(ns("colFilter_checkBox"), label = NULL, choices = character(0))),
                   div(style = "display: flex; gap: 10px; width: 100%;",
                       actionButton(ns("show_all"), label = "Show All", style = "flex-grow: 1; width: 0;"),
                       actionButton(ns("show_default"), label = "Show Default", style = "flex-grow: 1; width: 0;"))))
      ),
      div(style = "display: flex; justify-content: center;",
          actionBttn(ns("confirm_btn"), "Apply changes", style = "stretch", color = "success"))
    )
  )
}