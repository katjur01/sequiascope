#app/view/somatic_var_call_table.R

box::use(
  shiny[NS, sliderInput, fluidRow, column, tagList, br, uiOutput, plotOutput, downloadButton, numericInput, renderPlot, fluidPage, selectInput,
        icon,div,tabPanel,moduleServer,downloadHandler,observe, observeEvent,reactive,renderUI,updateSliderInput,updateNumericInput,req,isolate,is.reactive,
        reactiveVal,showModal,modalDialog,modalButton,isTruthy],
  reactable[colDef,reactableOutput,renderReactable,reactable,getReactableState,JS],
  bs4Dash[box,tabsetPanel,updateTabItems,updateNavbarTabs,actionButton],
  htmltools[tags, span,HTML],
  shinyWidgets[pickerInput,updatePickerInput,dropdownButton,prettyCheckboxGroup,updatePrettyCheckboxGroup,actionBttn,pickerOptions,dropdown],
  networkD3[sankeyNetwork,renderSankeyNetwork,sankeyNetworkOutput],
  billboarder[billboarderOutput],
  stats[setNames],
  shinyalert[shinyalert,useShinyalert],
  shinyjs[useShinyjs,hide,show],
  data.table[fread,data.table,as.data.table,copy,is.data.table,uniqueN,rbindlist],
  magrittr[`%>%`],
  jsonlite[read_json],
)

box::use(
  app/logic/prepare_main_table_and_filters[map_column_names,map_gene_region_names,map_clin_sig_names],
  app/logic/vaf_plot[generate_vaf],
  app/logic/sankey_plot[sankey_plot],
  app/logic/waiters[use_spinner],
  app/logic/export_functions[get_table_download_handler,get_sankey_download_handler,get_hist_download_handler],
  app/logic/load_data[load_data],
  app/logic/prepare_table[prepare_somatic_table,colFilter],
  app/logic/helper_reactable[create_clinvar_filter,create_consequence_filter],
  app/logic/filter_columns[map_checkbox_names,colnames_map_list,generate_columnsDef],
  app/logic/session_utils[create_session_handlers, register_module, safe_extract, nz, ch],
  app/logic/helper_main[get_files_by_patient]
)


# UI funkce pro modul nastavujici vzhled veskerych grafickych prvku v zalozce somatic variants

ui <- function(id) {
  ns <- NS(id)
  useShinyjs()
  tagList(
    tags$head(tags$style(HTML(".download-dropdown-wrapper .dropdown-toggle {border-radius: 0; padding: 0; background-color: transparent; border: none; float: right; margin-top: -1px;}
                               .download-dropdown-wrapper .dropdown-toggle::after {display: none !important;}
                               .download-dropdown-wrapper .glyphicon-triangle-bottom {display: none !important; width: 0 !important; margin: 0 !important; padding: 0 !important;}
                               button:has(.fa-play) .glyphicon-triangle-bottom { display: none !important; }
                               button:has(.fa-play) .fa-play { font-size: 0.75em; }"))),
    fluidRow(
      div(class = "download-dropdown-wrapper", style = "width: 100%; text-align: right; display: flex; flex-direction: row-reverse;",
         dropdownButton(label = NULL,right = TRUE,width = "240px",icon = HTML('<i class="fa-solid fa-download download-button"></i>'),
           selectInput(ns("export_data_table"), "Select data:", choices = c("All data" = "all", "Filtered data" = "filtered")),
           selectInput(ns("export_format_table"), "Select format:", choices = c("CSV" = "csv", "TSV" = "tsv", "Excel" = "xlsx")),
           downloadButton(ns("Table_download"),"Download")),
         filterTab_ui(ns("filterTab_dropdown"))
         )),
     use_spinner(reactableOutput(ns("somatic_var_call_tab"))),
     tags$br(),
     div(style = "display: flex; justify-content: space-between; align-items: top; width: 100%;",
       column(6,
         tags$br(),
         actionButton(ns("selectPathogenic_button"), "Select variants as possibly oncogenic", status = "info"),
         tags$br(),
         fluidRow(
           column(12,reactableOutput(ns("selectPathogenic_tab")))),
         tags$br(),
         fluidRow(
           column(4,actionButton(ns("delete_button"),"Delete variants", icon = icon("trash-can"))))
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
         div(class = "download-dropdown-wrapper",
           dropdownButton(label = "Export Circos Plot",right = TRUE,width = "240px",icon = HTML('<i class="fa-solid fa-download download-button"></i>'),
           downloadButton(ns("Hist_download"),"Download as PNG"))),
         br(),br(),
         div(style = "width: 100%; margin: auto;",
           use_spinner(plotOutput(ns("Histogram"),height = "480px"))
         )
       )
     ),
     div(class = "collapsible-box",
       box(width = 12,closable = FALSE,collapsible = TRUE,collapsed = TRUE,title = tags$div(style = "padding-top: 8px;","Sankey diagram"),
         div(class = "download-dropdown-wrapper",
           dropdownButton(label = "Export Sankey Plot",right = FALSE,width = "240px",icon = HTML('<i class="fa-solid fa-download download-button"></i>'),
             selectInput(ns("export_format"), "Select format:", choices = c("HTML" = "html", "PNG" = "png")),
             downloadButton(ns("Sankey_download"),"Download"))),
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
server <- function(id, selected_samples, shared_data, file, file_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    is_restoring_session <- reactiveVal(FALSE)
    
    observe({
      req(data())
      if (!is.null(file$TMB)) {
        mutation_load <- load_data(file$TMB, "TMB", selected_samples)
        overview_dt <- data.table(
          for_review = length(data()[gnomad_nfe <= 0.01 & tumor_depth > 10 & consequence != "synonymous variant" &
                                        (gene_region == "exon" | gene_region == "splice"), unique(var_name)]),
          TMB = unique(mutation_load$TMB))
      } else {
        overview_dt <- data.table(
          for_review = length(data()[gnomad_nfe <= 0.01 & tumor_depth > 10 & consequence != "synonymous variant" &
                                        (gene_region == "exon" | gene_region == "splice"), unique(var_name)]))
      }

      shared_data$somatic.overview[[ selected_samples ]] <- overview_dt
    })

    # Load and process data table
    prepare_data <- reactive({
      message("Loading input data for somatic: ", file$variant)
      data <- load_data(file$variant, "somatic", selected_samples, shared_data$session_dir())
      prepare_somatic_table(data, colnames(data))
    })
    
    data <- reactive(prepare_data()$dt)
    # colnames_list <- prepare_data()$columns
    colnames_list_A <- reactive({
      req(prepare_data())
      prepare_data()$columns
    })

    
    map_list <- colnames_map_list("somatic") # gives list of all columns with their column definitions
    # mapped_checkbox_names <- map_checkbox_names(map_list) # gives list of all columns with their display names for checkbox
    mapped_checkbox_names <- reactive({
      req(data())
      req(colnames_list_A())
      map_checkbox_names(map_list, colnames_list_A()$all_columns)
    })
    colnames_list <- colnames_list_A() 
    # colnames_list <- reactive({
    #   if (is.reactive(colnames_list_A)) colnames_list_A() else colnames_list_A
    # })

    filter_state <- filterTab_server("filterTab_dropdown",colnames_list, data(),mapped_checkbox_names, is_restoring_session)
    
    ############
    
    selected_tumor_depth <- reactiveVal(NULL)
    selected_gnomAD_min  <- reactiveVal(NULL)
    selected_gene_region <- reactiveVal(NULL)
    selected_clinvar_sig <- reactiveVal(NULL)
    selected_consequence <- reactiveVal(NULL)
    selected_columns <- reactiveVal(colnames_list$default_columns)
    selected_variants <- reactiveVal(data.frame(patient = character(),var_name = character(), gene_symbol = character()))
    
    defaults_applied <- reactiveVal(FALSE)

    observe({
      req(filter_state$tumor_depth())
      req(filter_state$gnomAD_min())
      req(filter_state$gene_regions())
      req(filter_state$consequence())
      req(filter_state$selected_columns())
      
      if (!defaults_applied() && !is_restoring_session()) {      # Aplikuj defaults jen jednou, při prvním načtení
        selected_tumor_depth(filter_state$tumor_depth())
        selected_gnomAD_min(filter_state$gnomAD_min())
        selected_gene_region(filter_state$gene_regions())
        selected_consequence(filter_state$consequence())
        selected_columns(filter_state$selected_columns())
        
        defaults_applied(TRUE)
      }
    })
    
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
      if (nrow(dt) == 0) return(dt)}
  
    if (!is.null(selected_gnomAD_min())) {
      dt <- dt[gnomad_nfe <= selected_gnomAD_min()]
      if (nrow(dt) == 0) return(dt)}
  
    if (!is.null(selected_gene_region()) && length(selected_gene_region()) > 0) {
      dt <- dt[gene_region %in% selected_gene_region(), ]
      if (nrow(dt) == 0) return(dt)}
  
    if (!is.null(selected_consequence()) && length(selected_consequence()) > 0) {
      if (nrow(dt) > 0) dt <- create_consequence_filter(dt, selected_consequence())}
  
    return(dt)
  })

    output$somatic_var_call_tab <- renderReactable({
      req(filtered_data())
      req(column_defs())
      message("🟢 Rendering Reactable for somatic")
      filtered_data <- filtered_data()
      pathogenic_variants <- selected_variants() # seznam variant, které byly označeny jako patogenní
      # Dynamicky vytvoř defaultSorted jen pro sloupce které existují
      sort_cols <- c("fone", "cgc_somatic")
      existing_sort_cols <- intersect(sort_cols, names(filtered_data))
      default_sorted <- if (length(existing_sort_cols) > 0) {
        as.list(setNames(rep("desc", length(existing_sort_cols)), existing_sort_cols))
      } else {
        NULL
      }

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
        defaultSorted = default_sorted,
        rowStyle = if (!is.null(pathogenic_variants) && nrow(pathogenic_variants) > 0) {
          function(index) {
            gene_in_row <- filtered_data$gene_symbol[index]
            var_in_row <- filtered_data$var_name[index]
            if (var_in_row %in% pathogenic_variants$var_name &           # Pokud je aktuální řádek v seznamu patogenních variant, zvýrazníme ho
                gene_in_row %in% pathogenic_variants$gene_symbol) {
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
      
      message("🔍 Available columns in filtered_data: ", paste(names(filtered_data()), collapse = ", "))

      new_variants <- filtered_data()[selected_rows, c("var_name", "gene_symbol","tumor_variant_freq","tumor_depth", "consequence",
                                                       "hgvsc","hgvsp","variant_type","feature", "gnomad_nfe")]  # Získání vybraných variant
      new_variants$sample <- selected_samples

      current_variants <- selected_variants()  # Stávající přidané varianty
      new_unique_variants <- new_variants[!(new_variants$var_name %in% current_variants$var_name &       # Porovnání - přidáme pouze ty varianty, které ještě nejsou v tabulce
                                              new_variants$gene_symbol %in% current_variants$gene_symbol), ]

      if (nrow(new_unique_variants) > 0) selected_variants(rbind(current_variants, new_unique_variants))

      # Aktualizace globální proměnné shared_data$somatic.variants:
      global_data <- shared_data$somatic.variants()

      # Pokud je NULL nebo nemá správnou strukturu, inicializujeme
      if (is.null(global_data) || !is.data.table(global_data) || !("sample" %in% names(global_data))) {
        global_data <- data.table(
          sample = character(),
          var_name = character(),
          gene_symbol = character(),
          tumor_variant_freq = numeric(),
          tumor_depth = integer(),
          consequence = character(),
          hgvsc = character(),
          hgvsp = character(),
          variant_type = character(),
          feature = character(),
          gnomad_nfe = numeric()
        )
      }

      # VŽDY provedeme odstranění záznamů daného pacienta — bezpodmínečně!
      global_data <- global_data[sample != selected_samples]


      # Přidáme nově aktualizované lokální data daného pacienta
      updated_global_data <- rbindlist(list(global_data, selected_variants()), use.names = TRUE, fill = TRUE)
      
      shared_data$somatic.variants(updated_global_data)
    })


    output$selectPathogenic_tab <- renderReactable({
      variants <- selected_variants()
      if (is.null(variants) || nrow(variants) == 0) {
        return(NULL)
      } else {
        # Vyber pouze sloupce které existují
        required_cols <- c("var_name", "gene_symbol", "consequence", "feature")
        optional_cols <- c("hgvsc", "hgvsp")
        available_cols <- intersect(c(required_cols, optional_cols), names(variants))
        
        variants <- as.data.table(variants)[, available_cols, with = FALSE]
        
        # Dynamicky vytvoř column definitions jen pro existující sloupce
        col_list <- list(
          var_name = colDef(name = "Variant name"),
          gene_symbol = colDef(name = "Gene name"),
          consequence = colDef(name = "Consequence", minWidth = 160),
          feature = colDef(name = "Feature")
        )
        if ("hgvsc" %in% names(variants)) col_list$hgvsc <- colDef(name = "HGVSc")
        if ("hgvsp" %in% names(variants)) col_list$hgvsp <- colDef(name = "HGVSp")
        
        reactable(
          as.data.frame(variants),
          columns = col_list,
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
          gene_symbol = character(),
          tumor_variant_freq = character(),
          tumor_depth = character(),
          consequence = character(),
          hgvsc = character(),
          hgvsp = character(),
          variant_type = character(),
          feature = character(),
          gnomad_nfe = character()
        )
      }

      if (nrow(updated_variants) > 0) {
        updated_global_data <- rbindlist(list(global_data, as.data.table(updated_variants)), use.names = TRUE, fill = TRUE)
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
      sankey_plot(filtered_data(), shared_data$kegg_tab_path())
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
      message("selected_variants(): ", selected_variants())
      
      selected_empty <- is.null(selected_variants()) || nrow(selected_variants()) == 0
      selected_patients <- input$idpick
      no_patients_selected <- is.null(selected_patients) || length(selected_patients) == 0
      
      if (selected_empty) {
        shinyalert(
          title = "No variant selected",
          text = "Please select at least one variant before inspecting them in IGV.",
          type = "warning",
          showCancelButton = FALSE,
          confirmButtonText = "OK")
        
      } else if (no_patients_selected) {
        shinyalert(
          title = "No patient selected",
          text = "Please select at least one patient for IGV visualization.",
          type = "warning",
          showCancelButton = FALSE,
          confirmButtonText = "OK")
        
      } else {
        shared_data$navigation_context("somatic")   # odkud otevíráme IGV
        
        message("Selected patients for IGV: ", paste(selected_patients, collapse = ", "))
        
        # Build tracks for selected patients
        track_lists <- lapply(selected_patients, function(patient_id) {
          tracks <- list()
          
          if (!patient_id %in% names(file_list)) {
            message("No somatic files found for patient: ", patient_id)
            return(tracks)
          }
          
          fresh_fl      <- get_files_by_patient(isolate(shared_data$confirmed_paths()), "somatic")
          patient_files <- if (patient_id %in% names(fresh_fl)) fresh_fl[[patient_id]] else list()

          # Add tumor BAM track if available
          if ("tumor" %in% names(patient_files) && length(patient_files$tumor) > 0) {
            tumor_bam <- patient_files$tumor[grepl("\\.bam$", patient_files$tumor)][1]
            if (!is.na(tumor_bam)) {
              tracks <- c(tracks, list(list(
                name = paste0(patient_id, " Tumor"),
                file = tumor_bam
              )))
            }
          }
          
          # Add normal BAM track if available  
          if ("normal" %in% names(patient_files) && length(patient_files$normal) > 0) {
            normal_bam <- patient_files$normal[grepl("\\.bam$", patient_files$normal)][1]
            if (!is.na(normal_bam)) {
              tracks <- c(tracks, list(list(
                name = paste0(patient_id, " Normal"),
                file = normal_bam
              )))
            }
          }
          
          tracks
        })
        
        bam_list <- do.call(c, track_lists)   # flatten list
        
        # Log assembled tracks
        if (length(bam_list)) {
          files <- vapply(bam_list, function(x) x$file, character(1L))
          message("✔ Assigned somatic_bam (", length(bam_list), " tracks): ", paste(files, collapse = ", "))
        } else {
          message("✖ No tracks assembled for patients: ", paste(selected_patients, collapse = ", "))
        }
        
        shared_data$somatic.bam(bam_list)
        message("bam_list iin somoatic: ", bam_list)
        shared_data$somatic.patients.igv(selected_patients)
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
 
    return(methods)

  })
}

filterTab_server <- function(id, colnames_list, data, mapped_checkbox_names, is_restoring = NULL) {
  moduleServer(id, function(input, output, session) {

    # Flag pro inicializaci
    initialized <- reactiveVal(FALSE)
    
    get_checkbox_names <- reactive({
      if (is.reactive(mapped_checkbox_names)) {
        mapped_checkbox_names()
      } else {
        mapped_checkbox_names
      }
    })
    
    get_colnames_list <- reactive({
      if (is.reactive(colnames_list)) {
        colnames_list()
      } else {
        colnames_list
      }
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
      req(get_checkbox_names())
      req(get_colnames_list())
      
      checkbox_names <- get_checkbox_names()
      cols_list <- get_colnames_list()
      
      # Seřaď choices podle názvů
      col_choices_ordered <- checkbox_names[order(names(checkbox_names))]
      # col_choices_ordered <- mapped_checkbox_names[order(names(mapped_checkbox_names))]
      
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
        # default_cols = colnames_list$default_columns
        default_cols = cols_list$default_columns
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
      req(get_checkbox_names())
      
      checkbox_names <- get_checkbox_names()
      
      all_values <- ch(unname(checkbox_names))
      # all_values <- ch(unname(mapped_checkbox_names))
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = all_values)
    })
    
    observeEvent(input$show_default, {
      req(get_checkbox_names())
      req(get_colnames_list())
      
      checkbox_names <- get_checkbox_names()
      cols_list <- get_colnames_list()
      
      default_values <- normalize_column_selection(
        selection = cols_list$default_columns,
        choices_map = checkbox_names,
        default_cols = cols_list$default_columns
        # selection = colnames_list$default_columns,
        # choices_map = mapped_checkbox_names,
        # default_cols = colnames_list$default_columns
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
        
        checkbox_names <- isolate(get_checkbox_names())
        cols_list <- isolate(get_colnames_list())
        
        wanted_values <- normalize_column_selection(
          selection = wanted,
          # choices_map = mapped_checkbox_names,
          # default_cols = colnames_list$default_columns
          choices_map = checkbox_names,
          default_cols = cols_list$default_columns
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
    dropdownButton(
      label = NULL,
      right = TRUE,
      width = "1250px",
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