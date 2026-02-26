# app/view/germline_var_call_table.R

#########################################################################################################
## to co zpomaluje skript nejvíc je:
## 1. css styly u řádků Clinvar_sig, CGC_Germline, TruSight_genes a fOne (doba vzroste z 3s na 27s)
## 2. fce input_data - načítání dat a jejich úprava (doba trvání cca 8.5s)
#########################################################################################################

box::use(
  shiny[moduleServer,NS,h2,h3,tagList,div,tabsetPanel,tabPanel,observeEvent,fluidPage,fluidRow, reactive,icon,textInput,isTruthy,verbatimTextOutput,
        sliderInput,showModal,modalDialog,modalButton,column,uiOutput,renderUI,textOutput,renderText,reactiveVal,req,observe,outputOptions,checkboxInput,
        renderPrint,getDefaultReactiveDomain,selectInput,downloadButton,numericInput,updateNumericInput,isolate],
  bs4Dash[actionButton, box,popover,addPopover,updateNavbarTabs],
  reactable,
  reactable[reactable,reactableOutput,renderReactable,colDef,colGroup,JS,getReactableState],
  htmltools[tags,HTML],
  shinyWidgets[prettyCheckbox,prettyCheckboxGroup,updatePrettyCheckboxGroup,searchInput,pickerInput,updatePickerInput,dropdown,actionBttn,pickerOptions,dropdownButton],
  shinyalert[shinyalert,useShinyalert],
  shinyjs[useShinyjs,hide,show],
  data.table[data.table,as.data.table,uniqueN,copy,rbindlist,fread,is.data.table],
  stats[setNames],
  magrittr[`%>%`],
  # waiter[useWaitress,Waitress]

  # reactablefmtr
)

box::use(
  app/logic/load_data[load_data],
  app/logic/prepare_table[prepare_germline_table],
  app/logic/waiters[use_spinner],
  app/logic/helper_reactable[selectFilter,minRangeFilter,filterMinValue,create_clinvar_filter,create_consequence_filter],
  app/logic/filter_columns[map_checkbox_names,colnames_map_list,generate_columnsDef],
  app/logic/session_utils[create_session_handlers, register_module, safe_extract, nz, ch],
  app/logic/export_functions[get_table_download_handler],
  app/logic/helper_main[get_files_by_patient]
)


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
        filterTab_ui(ns("filterTab_dropdown")))),
    use_spinner(reactableOutput(ns("germline_var_call_tab"))),
    tags$br(),

    div(style = "display: flex; justify-content: space-between; align-items: top; width: 100%;",
      column(6,
        actionButton(ns("selectPathogenic_button"), "Select variants as possibly pathogenic", status = "info"),
        tags$br(),
        fluidRow(
          column(12,reactableOutput(ns("selectPathogenic_tab")))),
        tags$br(),
        fluidRow(
          column(4,actionButton(ns("delete_button"), "Delete variants", icon = icon("trash-can"))))
      ),
      dropdown(label = "IGV", status = "primary", icon = icon("play"), right = TRUE, size = "md", width = "230px", 
               pickerInput(ns("idpick"), "Select patients for IGV:", choices = NULL, options = pickerOptions(actionsBox = FALSE, size = 4, maxOptions = 4, dropupAuto = FALSE, maxOptionsText = "Select max. 4 patients"),multiple = TRUE),
               div(style = "display: flex; justify-content: center; margin-top: 10px;",
                   actionBttn(ns("go2igv_button"), label = "Go to IGV", style = "stretch", color = "primary", size = "sm", individual = TRUE)
               )
      )
    )
  )
}

server <- function(id, selected_samples, shared_data, file, file_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    is_restoring_session <- reactiveVal(FALSE)
    
    observe({
      req(data())
      overview_dt <- data.table(
        clinvar_N = length(data()[clinvar_sig %in% c("Pathogenic", "Likely_pathogenic", "Pathogenic/Likely_pathogenic",
                                                      "Pathogenic_(VUS)", "Likely_pathogenic (VUS)", "Pathogenic_(VUS)"), unique(var_name)]),

        for_review = length(data()[gnomad_nfe <= 0.01 & coverage_depth > 10 & consequence != "synonymous variant" &
                                      (gene_region == "exon" | gene_region == "splice"), unique(var_name)]))
      # print(overview_dt)
      shared_data$germline.overview[[ selected_samples ]] <- overview_dt
    })
    
    # Load and process data table
    prepare_data <- reactive({
      message("Loading input data for germline: ", file$variant)
      data <- load_data(file$variant, "germline", selected_samples, shared_data$session_dir())
      prepare_germline_table(data, colnames(data))
    })

    data <- reactive(prepare_data()$dt)
    colnames_list <- prepare_data()$columns
    
    map_list <- colnames_map_list("germline") # gives list of all columns with their column definitions
    # mapped_checkbox_names <- map_checkbox_names(map_list) # gives list of all columns with their display names for checkbox
    mapped_checkbox_names <- reactive({
      req(data())
      req(colnames_list)
      map_checkbox_names(map_list, colnames_list$all_columns)
    })
    
    # start <- Sys.time()
    filter_state <- filterTab_server("filterTab_dropdown",colnames_list, data(),mapped_checkbox_names, is_restoring = is_restoring_session)
    # end <- Sys.time()
    # message("⏱️ render UI filterTab: ", round(difftime(end, start, units = "secs"), 3), " sec")
    
    selected_coverage_depth <- reactiveVal(NULL)
    selected_gnomAD_min  <- reactiveVal(NULL)
    selected_gene_region <- reactiveVal(NULL)
    selected_clinvar_sig <- reactiveVal(NULL)
    selected_consequence <- reactiveVal(NULL)
    selected_columns <- reactiveVal(colnames_list$default_columns)
    selected_variants <- reactiveVal(data.frame(patient = character(),var_name = character(), gene_symbol = character()))

    defaults_applied <- reactiveVal(FALSE)
    
    observe({
      req(filter_state$coverage_depth())
      req(filter_state$gnomAD_min())
      req(filter_state$gene_regions())
      req(filter_state$clinvar_sig())
      req(filter_state$consequence())
      req(filter_state$selected_columns())

      if (!defaults_applied() && !is_restoring_session()) {      # Aplikuj defaults jen jednou, při prvním načtení

        selected_coverage_depth(filter_state$coverage_depth())
        selected_gnomAD_min(filter_state$gnomAD_min())
        selected_gene_region(filter_state$gene_regions())
        selected_clinvar_sig(filter_state$clinvar_sig())
        selected_consequence(filter_state$consequence())
        selected_columns(filter_state$selected_columns())
 
        defaults_applied(TRUE)
      }
    })
    
    column_defs <- reactive({
      req(data())
      req(selected_columns())
      generate_columnsDef(names(data()), selected_columns(), "germline", map_list)
    })
    
    filtered_data <- reactive({
      req(data())
      dt <- copy(data())
      
      if (!is.null(selected_coverage_depth())) {
        dt <- dt[selected_coverage_depth() <= coverage_depth, ]
        if (nrow(dt) == 0) return(dt)
      }
      if (!is.null(selected_gnomAD_min())) {
        dt <- dt[gnomad_nfe <= selected_gnomAD_min(), ]
        if (nrow(dt) == 0) return(dt)
      }
      if (!is.null(selected_gene_region()) && length(selected_gene_region()) > 0) {
        dt <- dt[gene_region %in% selected_gene_region(), ]
        if (nrow(dt) == 0) return(dt)
      }
      if (!is.null(selected_clinvar_sig()) && length(selected_clinvar_sig()) > 0) {
        dt <- create_clinvar_filter(dt, selected_clinvar_sig())
        if (nrow(dt) == 0) return(dt)
      }
      if (!is.null(selected_consequence()) && length(selected_consequence()) > 0) {
        if (nrow(dt) > 0) dt <- create_consequence_filter(dt, selected_consequence())}
      
      return(dt)
    })
    

    
    ##  Render reactable with conditional selection
    output$germline_var_call_tab <- renderReactable({
      # waitress <- Waitress$new(paste0("#", ns("germline_var_call_tab")), theme = "overlay-percent", infinite = TRUE)
      # waitress$start()
      req(filtered_data())
      req(column_defs())
      # start <- Sys.time()
      # message("Rendering Reactable for germline")
      filtered_data <- filtered_data() # tvoje data pro hlavní tabulku
      pathogenic_variants <- selected_variants() # seznam variant, které byly označeny jako patogenní

      # Dynamicky vytvoř defaultSorted jen pro sloupce které existují
      sort_cols <- c("cgc_germline", "trusight_genes", "fone")
      existing_sort_cols <- intersect(sort_cols, names(filtered_data))
      default_sorted <- if (length(existing_sort_cols) > 0) {
        as.list(setNames(rep("desc", length(existing_sort_cols)), existing_sort_cols))
      } else {
        NULL
      }

      tbl <- reactable(
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
            } else {
                NULL
          },
        selection = "multiple",
        onClick = JS("function(rowInfo, column, event) {
                        if (event.target.classList.contains('rt-expander') || event.target.classList.contains('rt-expander-button')) {
                        } else {
                            rowInfo.toggleRowSelected();}}"),
        class = "germline-table"
      )
      # end <- Sys.time()
      # message("⏱️ renderReactable duration: ", round(difftime(end, start, units = "secs"), 3), " sec")
      # waitress$close()
      tbl

    })
  
    # Akce po kliknutí na tlačítko pro přidání varianty
    observeEvent(input$selectPathogenic_button, {
      selected_rows <- getReactableState("germline_var_call_tab", "selected")
      req(selected_rows)
      
      new_variants <- filtered_data()[selected_rows, c("var_name", "gene_symbol","variant_freq","coverage_depth", "consequence",
                                                       "hgvsc","hgvsp","variant_type","feature", "clinvar_sig","gnomad_nfe")]  # Získání vybraných variant
      new_variants$sample <- selected_samples

      current_variants <- selected_variants()  # Stávající přidané varianty
      new_unique_variants <- new_variants[!(new_variants$var_name %in% current_variants$var_name &       # Porovnání - přidáme pouze ty varianty, které ještě nejsou v tabulce
                                              new_variants$gene_symbol %in% current_variants$gene_symbol), ]

      if (nrow(new_unique_variants) > 0) selected_variants(rbind(current_variants, new_unique_variants))
      
      # Aktualizace globální proměnné shared_data$germline.variants:
      global_data <- shared_data$germline.variants()

      # Pokud je NULL nebo nemá správnou strukturu, inicializujeme
      if (is.null(global_data) || !is.data.table(global_data) || !("sample" %in% names(global_data))) {
        global_data <- data.table(
          sample = character(),
          var_name = character(),
          gene_symbol = character(),
          variant_freq= character(),
          coverage_depth = character(),
          consequence = character(),
          hgvsc = character(),
          hgvsp = character(),
          variant_type = character(),
          feature = character(),
          clinvar_sig = character(), #(round(variant_freq * coverage_depth))/coverage_depth
          gnomad_nfe = character()
        )
      }

      global_data <- global_data[sample != selected_samples]
      
      # Přidáme nově aktualizované lokální data daného pacienta
      updated_global_data <- rbindlist(list(global_data, selected_variants()), use.names = TRUE, fill = TRUE)
      shared_data$germline.variants(updated_global_data)
      message("## shared_data$germline_var(): ", shared_data$germline.variants())
    })
    

    output$selectPathogenic_tab <- renderReactable({
      variants <- selected_variants()
      if (is.null(variants) || nrow(variants) == 0) {
        return(NULL)
      }
      
      # Vyber pouze sloupce které existují
      required_cols <- c("var_name", "gene_symbol", "consequence", "clinvar_sig", "feature")
      optional_cols <- c("hgvsc", "hgvsp")
      available_cols <- intersect(c(required_cols, optional_cols), names(variants))
      
      variants <- as.data.table(variants)[, available_cols, with = FALSE]
      
      # Dynamicky vytvoř column definitions jen pro existující sloupce
      col_list <- list(
        var_name = colDef(name = "Variant name"),
        gene_symbol = colDef(name = "Gene name"),
        consequence = colDef(name = "Consequence", minWidth = 160),
        clinvar_sig = colDef(name = "ClinVar significance", minWidth = 180),
        feature = colDef(name = "Feature")
      )
      if ("hgvsc" %in% names(variants)) col_list$hgvsc <- colDef(name = "HGVSc")
      if ("hgvsp" %in% names(variants)) col_list$hgvsp <- colDef(name = "HGVSp")
      
      reactable(
        as.data.frame(variants),
        columns = col_list,
        selection = "multiple", onClick = "select"
      )
    })

    observeEvent(input$delete_button, {
      rows <- getReactableState("selectPathogenic_tab", "selected")
      req(rows)
      
      current_variants <- selected_variants()
      updated_variants <- current_variants[-rows, ]
      selected_variants(updated_variants)
      
      global_data <- shared_data$germline.variants()
      if (!is.null(global_data) && is.data.table(global_data)) {
        global_data <- global_data[sample != selected_samples]
      } else {
        global_data <- data.table(
          sample = character(),
          var_name = character(),
          gene_symbol = character(),
          variant_freq= character(),
          coverage_depth = character(),
          consequence = character(),
          hgvsc = character(),
          hgvsp = character(),
          variant_type = character(),
          feature = character(),
          clinvar_sig = character(),
          gnomad_nfe = character()
        )
      }
      
      if (nrow(updated_variants) > 0) {
        updated_global_data <- rbindlist(list(global_data, as.data.table(updated_variants)), use.names = TRUE, fill = TRUE)
      } else {
        updated_global_data <- global_data
      }
      
      shared_data$germline.variants(updated_global_data)
      session$sendCustomMessage("resetReactableSelection", updated_variants)
      
      if (nrow(updated_variants) == 0) {
        hide("delete_button")
      }
    })
    
    # Při stisku tlačítka pro výběr varianty
    observeEvent(input$selectPathogenic_button, {
      if (is.null(selected_variants()) || nrow(selected_variants()) == 0) {
        # Pokud nejsou vybrány žádné řádky, zůstaň u původního stavu
        # variant_selected(FALSE)
        hide("delete_button")
        
        shinyalert(
          title = "No variant selected",
          text = "Please select the potentially pathogenic variants from table above.",
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
      variants <- selected_variants()
      
      if (!is.null(variants) && nrow(variants) > 0) {
        show("delete_button")
      } else {
        hide("delete_button")
      }
    })
    
    
    
    observeEvent(filter_state$confirm(), {
      message("🟢 Confirm button was clicked")
      selected_coverage_depth(filter_state$coverage_depth())
      selected_gnomAD_min(filter_state$gnomAD_min())
      selected_gene_region(filter_state$gene_regions())
      selected_clinvar_sig(filter_state$clinvar_sig())
      selected_consequence(filter_state$consequence())
      selected_columns(filter_state$selected_columns())
    })

    
    #############
    ## run IGV ##
    #############
    
    ## update IGV button choices
    observeEvent(shared_data$germline.patients(), {
      patient_list <- shared_data$germline.patients()
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
          confirmButtonText = "OK"
        )
        
      } else if (no_patients_selected) {
        shinyalert(
          title = "No patient selected",
          text = "Please select at least one patient for IGV visualization.",
          type = "warning",
          showCancelButton = FALSE,
          confirmButtonText = "OK"
        )
        
      } else {
        shared_data$navigation_context("germline")   # odkud otevíráme IGV
        message("Selected patients for IGV (germline): ", paste(selected_patients, collapse = ", "))
        
        # Vytvoř seznam BAM souborů pro každého pacienta
        track_lists <- lapply(selected_patients, function(patient_id) {
          tracks <- list()
          
          if (!patient_id %in% names(file_list)) {
            message("No germline files found for patient: ", patient_id)
            return(tracks)
          }
          
          fresh_fl      <- get_files_by_patient(isolate(shared_data$confirmed_paths()), "germline")
          patient_files <- if (patient_id %in% names(fresh_fl)) fresh_fl[[patient_id]] else list()

          # Přidej pouze normal BAM (germline)
          if ("normal" %in% names(patient_files) && length(patient_files$normal) > 0) {
            normal_bam <- patient_files$normal[grepl("\\.bam$", patient_files$normal)][1]
            if (!is.na(normal_bam)) {
              tracks <- c(tracks, list(list(
                name = paste0(patient_id, " Normal"),
                file = normal_bam
              )))
            }
          } else {
            message("No normal BAM for patient: ", patient_id)
          }
          
          tracks
        })
        
        bam_list <- do.call(c, track_lists)   # flatten list
        
        # Log info
        if (length(bam_list)) {
          files <- vapply(bam_list, function(x) x$file, character(1L))
          message("✔ Assigned germline_bam (", length(bam_list), " tracks): ", paste(files, collapse = ", "))
        } else {
          message("✖ No tracks assembled for patients: ", paste(selected_patients, collapse = ", "))
        }
        
        shared_data$germline.bam(bam_list)
        shared_data$germline.patients.igv(selected_patients)
        updateNavbarTabs(
          session = session$userData$parent_session,
          inputId = "navbarMenu",
          selected = session$userData$parent_session$ns("hidden_igv")
        )
      }
    })
    
    
    ###########################
    ## Download handler ######
    ###########################
    output$Table_download <- get_table_download_handler(
      input = input,
      patient = selected_samples,
      data = data,
      filtered_data = filtered_data,
      suffix = ""
    )
    
    ###########################
    ## get / restore session ##
    ###########################
    session_handlers <- create_session_handlers(
      selected_inputs = list(
        gnomAD_min = selected_gnomAD_min,
        coverage_depth = selected_coverage_depth,
        gene_regions = selected_gene_region,
        clinvar_sig = selected_clinvar_sig,
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
    
    register_module(shared_data, "germline", selected_samples, methods)
    return(methods)
  })
}



filterTab_server <- function(id, colnames_list, data, mapped_checkbox_names, is_restoring = NULL) {
  moduleServer(id, function(input, output, session) {
    
    initialized <- reactiveVal(FALSE)
    
    # ===== HELPERY =====
    # Normalizace výběru sloupců (umí přijmout labely i values)
    normalize_column_selection <- function(selection, choices_map, default_cols) {
      if (is.null(selection) || length(selection) == 0) {
        return(ch(default_cols))
      }
      choice_labels <- names(choices_map)
      choice_vals   <- ch(unname(choices_map))
      label2val <- setNames(choice_vals, choice_labels)
      
      sel_norm <- character(0)
      for (item in ch(selection)) {
        if (item %in% choice_vals) {
          sel_norm <- c(sel_norm, item)
        } else if (item %in% choice_labels) {
          sel_norm <- c(sel_norm, label2val[[item]])
        }
      }
      intersect(unique(sel_norm), choice_vals)
    }
    
    # Consequence
    update_consequence_choices <- function() {
      consequence_split <- unique(unlist(unique(data$consequence_trimws)))
      consequence_list <- sort(unique(ifelse(
        is.na(consequence_split) | consequence_split == "", 
        "missing_value", 
        consequence_split
      )))
      consequence_list <- ch(consequence_list)
      
      if (!initialized()) {
        selected <- setdiff(consequence_list, "synonymous variant")
      } else {
        current  <- isolate(input$consequence)
        selected <- if (is.null(current)) setdiff(consequence_list, "synonymous variant") else current
        selected <- intersect(ch(selected), consequence_list)
      }
      
      updatePrettyCheckboxGroup(
        session, "consequence",
        choices = consequence_list,
        selected = selected,
        prettyOptions = list(status = "primary", icon = icon("check"), outline = FALSE)
      )
    }
    
    # Gene regions
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
    
    # ClinVar significance
    update_clinvar_choices <- function() {
      clinvar_split <- unique(unlist(unique(data$clinvar_trimws)))
      clinvar_list <- sort(unique(ifelse(
        is.na(clinvar_split) | clinvar_split == "",
        "missing_value",
        clinvar_split
      )))
      clinvar_list <- ch(clinvar_list)
      
      if (!initialized()) {
        # výchozí: pokud existují benignní kategorie, vynech je; jinak vyber vše
        benign_like <- c("Benign", "Likely_benign", "Benign/Likely_benign", "benign", "likely_benign")
        selected <- if (length(intersect(benign_like, clinvar_list))) {
          setdiff(clinvar_list, intersect(benign_like, clinvar_list))
        } else {
          clinvar_list
        }
      } else {
        current  <- isolate(input$clinvar_sig)
        selected <- if (is.null(current)) clinvar_list else current
        selected <- intersect(ch(selected), clinvar_list)
      }
      
      updatePrettyCheckboxGroup(
        session, "clinvar_sig",
        choices  = clinvar_list,
        selected = selected,
        prettyOptions = list(status = "primary", icon = icon("check"), outline = FALSE)
      )
    }
    
    # Column choices
    update_column_choices <- function() {
      req(mapped_checkbox_names())
      
      # Seřaď podle LABELŮ (names)
      checkbox_names <- mapped_checkbox_names()
      col_choices_ordered <- checkbox_names[order(names(checkbox_names))]
      
      current_selection <- isolate(input$colFilter_checkBox)
      default_selection <- if (!initialized() || is.null(current_selection) || length(current_selection) == 0) {
        colnames_list$default_columns
      } else {
        current_selection
      }
      
      selected_values <- normalize_column_selection(
        selection   = default_selection,
        choices_map = col_choices_ordered,
        default_cols = colnames_list$default_columns
      )
      
      updatePrettyCheckboxGroup(
        session, "colFilter_checkBox",
        choices  = col_choices_ordered,
        selected = selected_values,
        prettyOptions = list(status = "primary", icon = icon("check"), outline = FALSE)
      )
    }
    
    # Numeric defaults
    update_numeric_inputs <- function() {
      if (!initialized()) {
        if (isTruthy(is.na(input$coverage_depth))) {
          updateNumericInput(session, "coverage_depth", value = 10)
        }
        if (isTruthy(is.na(input$gnomAD_min))) {
          updateNumericInput(session, "gnomAD_min", value = 0.01)
        }
      }
    }
    
    # ===== MAIN OBSERVE =====
    observe({
      # během restore nepřepisovat UI
      if (!is.null(is_restoring) && isTruthy(is_restoring())) return()
      
      update_consequence_choices()
      update_gene_region_choices()
      update_clinvar_choices()
      update_column_choices()
      update_numeric_inputs()
      
      if (!initialized()) initialized(TRUE)
    })
    
    # ===== HANDLERY =====
    observeEvent(input$show_all, {
      req(mapped_checkbox_names())
      all_values <- ch(unname(mapped_checkbox_names()))
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = all_values)
    })
    observeEvent(input$show_default, {
      req(mapped_checkbox_names())
      default_values <- normalize_column_selection(
        selection   = colnames_list$default_columns,
        choices_map = mapped_checkbox_names(),
        default_cols = colnames_list$default_columns
      )
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = default_values)
    })
    
    # ===== RESTORE =====
    restore_ui_inputs <- function(state) {
      message("🎯 Restoring germline filter UI inputs")
      
      if (!is.null(state$gnomAD_min)) {
        val <- safe_extract(state$gnomAD_min)
        message(sprintf("Restoring gnomAD_min: %s", val))
        updateNumericInput(session, "gnomAD_min", value = val)
      }
      
      if (!is.null(state$coverage_depth)) {
        val <- safe_extract(state$coverage_depth)
        message(sprintf("Restoring coverage_depth: %s", val))
        updateNumericInput(session, "coverage_depth", value = val)
      }
      
      if (!is.null(state$gene_regions)) {
        wanted <- ch(safe_extract(state$gene_regions))
        message(sprintf("Restoring gene_regions: %s", paste(wanted, collapse = ", ")))
        updatePrettyCheckboxGroup(session, "gene_regions", selected = wanted)
      }
      
      if (!is.null(state$clinvar_sig)) {
        wanted <- ch(safe_extract(state$clinvar_sig))
        message(sprintf("Restoring clinvar_sig: %s", paste(wanted, collapse = ", ")))
        updatePrettyCheckboxGroup(session, "clinvar_sig", selected = wanted)
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
          selection   = wanted,
          choices_map = isolate(mapped_checkbox_names()),
          default_cols = colnames_list$default_columns
        )
        message(sprintf("Final selected values: %s", paste(wanted_values, collapse = ", ")))
        updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = wanted_values)
      }
    }

    # ===== RETURN =====
    return(list(
      confirm           = reactive(input$confirm_btn),
      coverage_depth    = reactive(input$coverage_depth),
      gnomAD_min        = reactive(input$gnomAD_min),
      gene_regions      = reactive(input$gene_regions),
      clinvar_sig       = reactive(input$clinvar_sig),
      consequence       = reactive(input$consequence),
      selected_columns  = reactive(input$colFilter_checkBox),
      restore_ui_inputs = restore_ui_inputs
    ))
  })
}

filterTab_ui <- function(id){
  ns <- NS(id)
  tagList(
    dropdownButton(
      label = NULL,
      right = TRUE,
      width = "1250px",
      icon = HTML('<i class="fa-solid fa-filter download-button"></i>'),
      fluidRow(style = "display: flex; align-items: stretch;",
               column(7,
                      box(width = 12,title = tags$div(style = "padding-top: 8px;","Filter data by:"),closable = FALSE, collapsible = FALSE,style = "height: 100%;",
                          fluidRow(
                            column(6, numericInput(ns("coverage_depth"), tags$strong("Coverage min"), value = 10, min = 0)),
                            column(6, numericInput(ns("gnomAD_min"), tags$strong("gnomAD NFE min"), value = 0.01, min = 0, max = 1))
                          ),
                            div(
                              div(class = "two-col-checkbox-group", style = "margin-bottom: 15px;",
                                  prettyCheckboxGroup(ns("gene_regions"), label = tags$strong("Gene region"), choices = character(0))),
                              div(class = "two-col-checkbox-group", style = "margin-bottom: 15px;",
                                  prettyCheckboxGroup(ns("clinvar_sig"),label = tags$strong("ClinVar significance"),choices = character(0)))),
                            div(class = "two-col-checkbox-group",
                                prettyCheckboxGroup(ns("consequence"),label = tags$strong("Consequence"),choices = character(0)))
                      )
               ),
               column(5,
                      box(width = 12,title = tags$div(style = "padding-top: 8px;","Select columns:"),closable = FALSE,collapsible = FALSE,height = "100%",
                          div(class = "two-col-checkbox-group",
                              prettyCheckboxGroup(ns("colFilter_checkBox"),label = NULL,choices = character(0))),
                          div(style = "display: flex; gap: 10px; width: 100%;",
                              actionButton(inputId = ns("show_all"), label = "Show All", style = "flex-grow: 1; width: 0;"),
                              actionButton(inputId = ns("show_default"), label = "Show Default", style = "flex-grow: 1; width: 0;"))
                      )
               )
      ),
      
      div(style = "display: flex; justify-content: center; margin-top: 10px;",
          actionBttn(ns("confirm_btn"),"Apply changes",style = "stretch",color = "success",size = "md",individual = TRUE,value = 0))
    )
  )
}

