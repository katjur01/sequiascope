# app/view/germline_var_call_table.R

#########################################################################################################
## to co zpomaluje skript nejvíc je:
## 1. css styly u řádků Clinvar_sig, CGC_Germline, TruSight_genes a fOne (doba vzroste z 3s na 27s)
## 2. fce input_data - načítání dat a jejich úprava (doba trvání cca 8.5s)
#########################################################################################################

box::use(
  shiny[moduleServer,NS,h2,h3,tagList,div,tabsetPanel,tabPanel,observeEvent,fluidPage,fluidRow, reactive,icon,textInput,isTruthy,verbatimTextOutput,
        sliderInput,showModal,modalDialog,modalButton,column,uiOutput,renderUI,textOutput,renderText,reactiveVal,req,observe,outputOptions,checkboxInput,
        renderPrint,getDefaultReactiveDomain,selectInput,downloadButton,numericInput,updateNumericInput],
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
  app/logic/load_data[get_inputs,load_data],
  app/logic/prepare_table[prepare_germline_table],
  app/logic/waiters[use_spinner],
  app/logic/reactable_helpers[selectFilter,minRangeFilter,filterMinValue,create_clinvar_filter,create_consequence_filter],
  app/logic/filter_columns[map_checkbox_names,colnames_map_list,generate_columnsDef],
  app/logic/session_utils[create_session_handlers,safe_extract]
)


ui <- function(id) {
  ns <- NS(id)
  useShinyjs()
  tagList(
    # useWaitress(),
    fluidRow(
      div(style = "width: 100%; text-align: right;",
        dropdownButton(label = NULL,right = TRUE,width = "240px",icon = HTML('<i class="fa-solid fa-download download-button"></i>'),
                       selectInput(ns("export_data_table"), "Select data:", choices = c("All data" = "all", "Filtered data" = "filtered")),
                       selectInput(ns("export_format_table"), "Select format:", choices = c("CSV" = "csv", "TSV" = "tsv", "Excel" = "xlsx")),
                       downloadButton(ns("Table_download"),"Download")),
        filterTab_ui(ns("filterTab_dropdown")))),
    use_spinner(reactableOutput(ns("germline_var_call_tab"))),
    # reactableOutput(ns("germline_var_call_tab")),
    tags$br(),

    div(style = "display: flex; justify-content: space-between; align-items: top; width: 100%;",
      div(
        actionButton(ns("selectPathogenic_button"), "Select variants as possibly pathogenic", status = "info"),
        tags$br(),
        fluidRow(
          column(12,reactableOutput(ns("selectPathogenic_tab")))),
        tags$br(),
        fluidRow(
          column(3,actionButton(ns("delete_button"), "Delete variants", icon = icon("trash-can"))))
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

server <- function(id, selected_samples, shared_data, file,  load_session_btn = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Load and process data table
    prepare_data <- reactive({
      message("Loading input data for germline: ", file$variant)
      data <- load_data(file$variant, "varcall", selected_samples)
      prepare_germline_table(data, colnames(data))
    })

    # observe({
    #     req(data())
    #     overview_dt <- data.table(
    #         clinvar_N = uniqueN(data()[clinvar_sig %in% c("Pathogenic", "Likely_pathogenic", "Pathogenic/Likely_pathogenic",
    #                                                       "Pathogenic_(VUS)", "Likely_pathogenic (VUS)", "Pathogenic_(VUS)")]),
    #         for_review = uniqueN(data()[gnomAD_NFE <= 0.01 & coverage_depth > 10 & Consequence != "synonymous_variant" &
    #                                       (gene_region == "exon" | gene_region == "splice")]))
    #     shared_data$germline_overview[[ selected_samples ]] <- overview_dt
    # })

    data <- reactive(prepare_data()$dt)
    colnames_list <- prepare_data()$columns
    
    map_list <- colnames_map_list("germline") # gives list of all columns with their column definitions
    mapped_checkbox_names <- map_checkbox_names(map_list) # gives list of all columns with their display names for checkbox
    
    start <- Sys.time()
    filter_state <- filterTab_server("filterTab_dropdown",colnames_list, data(),mapped_checkbox_names)
    end <- Sys.time()
    message("⏱️ render UI filterTab: ", round(difftime(end, start, units = "secs"), 3), " sec")
    
    selected_coverage_depth <- reactiveVal(NULL)
    selected_gnomAD_min  <- reactiveVal(NULL)
    selected_gene_region <- reactiveVal(NULL)
    selected_clinvar_sig <- reactiveVal(NULL)
    selected_consequence <- reactiveVal(NULL)
    selected_columns <- reactiveVal(colnames_list$default_columns)
    selected_variants <- reactiveVal(data.frame(patient = character(),var_name = character(), Gene_symbol = character()))


    column_defs <- reactive({
      req(selected_columns())
      start <- Sys.time()
      x <- generate_columnsDef(names(data()), selected_columns(), "germline", map_list)
      end <- Sys.time()
      message("⏱️ reactive column_defs: ", round(difftime(end, start, units = "secs"), 3), " sec")
      x
    })
    

    filtered_data <- reactive({
      start <- Sys.time()
      dt <- data()
      selected_coverage_depth <- selected_coverage_depth()
      selected_gnomAD_min <- selected_gnomAD_min()
      selected_gene_region <- selected_gene_region()
      selected_clinvar_sig <- selected_clinvar_sig()
      selected_consequence <- selected_consequence()
      
      if (!is.null(selected_coverage_depth)) {
        dt <- dt[selected_coverage_depth <= coverage_depth, ]
      }
      if (!is.null(selected_gnomAD_min)) {
        dt <- dt[gnomAD_NFE <= selected_gnomAD_min]
      }
      if (!is.null(selected_gene_region) && length(selected_gene_region) > 0) {
        dt <- dt[gene_region %in% selected_gene_region, ]
      }
      if (!is.null(selected_clinvar_sig) && length(selected_clinvar_sig) > 0) {
        dt <- create_clinvar_filter(dt, selected_clinvar_sig)
      }
      if (!is.null(selected_consequence) && length(selected_consequence) > 0) {
        dt <- create_consequence_filter(dt, selected_consequence)
      }
      end <- Sys.time()
      message("⏱️ reactive filtered_data: ", round(difftime(end, start, units = "secs"), 3), " sec")
      return(dt)
    })

    
    ##  Render reactable with conditional selection
    output$germline_var_call_tab <- renderReactable({
      # waitress <- Waitress$new(paste0("#", ns("germline_var_call_tab")), theme = "overlay-percent", infinite = TRUE)
      # waitress$start()
      req(filtered_data())
      req(column_defs())
      start <- Sys.time()
      # message("Rendering Reactable for germline")
      filtered_data <- filtered_data() # tvoje data pro hlavní tabulku
      pathogenic_variants <- selected_variants() # seznam variant, které byly označeny jako patogenní


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
        defaultSorted = list("CGC_Germline" = "desc", "trusight_genes" = "desc", "fOne" = "desc"),
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
            } else {
                NULL
          },
        # columnGroups = list(
        #   colGroup(name = "Databases", columns = c("gnomAD_NFE", "clinvar_sig", "snpDB", "CGC_Germline", "trusight_genes", "fOne")),
        #   colGroup(name = "Annotation", columns = c("Consequence", "HGVSc", "HGVSp", "all_full_annot_name"))
        # ),
        selection = "multiple",
        onClick = JS("function(rowInfo, column, event) {
                        if (event.target.classList.contains('rt-expander') || event.target.classList.contains('rt-expander-button')) {
                        } else {
                            rowInfo.toggleRowSelected();}}"),
        class = "germline-table"
      )
      end <- Sys.time()
      message("⏱️ renderReactable duration: ", round(difftime(end, start, units = "secs"), 3), " sec")
      # waitress$close()
      tbl

    })
  
    # Akce po kliknutí na tlačítko pro přidání varianty
    observeEvent(input$selectPathogenic_button, {
      selected_rows <- getReactableState("germline_var_call_tab", "selected")
      req(selected_rows)
      
      new_variants <- filtered_data()[selected_rows, c("var_name", "Gene_symbol","variant_freq","coverage_depth", "Consequence",
                                                       "HGVSc","HGVSp","variant_type","Feature", "clinvar_sig","gnomAD_NFE")]  # Získání vybraných variant
      new_variants$sample <- selected_samples

      current_variants <- selected_variants()  # Stávající přidané varianty
      new_unique_variants <- new_variants[!(new_variants$var_name %in% current_variants$var_name &       # Porovnání - přidáme pouze ty varianty, které ještě nejsou v tabulce
                                              new_variants$Gene_symbol %in% current_variants$Gene_symbol), ]

      if (nrow(new_unique_variants) > 0) selected_variants(rbind(current_variants, new_unique_variants))
      
      # Aktualizace globální proměnné shared_data$germline_var:
      global_data <- shared_data$germline_var()

      # Pokud je NULL nebo nemá správnou strukturu, inicializujeme
      if (is.null(global_data) || !is.data.table(global_data) || !("sample" %in% names(global_data))) {
        global_data <- data.table(
          sample = character(),
          var_name = character(),
          Gene_symbol = character(),
          variant_freq= character(),
          coverage_depth = character(),
          Consequence = character(),
          HGVSc = character(),
          HGVSp = character(),
          variant_type = character(),
          Feature = character(),
          clinvar_sig = character(), #(round(variant_freq * coverage_depth))/coverage_depth
          gnomAD_NFE = character()
        )
      }

      global_data <- global_data[sample != selected_samples]
      
      # Přidáme nově aktualizované lokální data daného pacienta
      updated_global_data <- rbind(global_data, selected_variants())
      shared_data$germline_var(updated_global_data)
      message("## shared_data$germline_var(): ", shared_data$germline_var())
    })
    

    output$selectPathogenic_tab <- renderReactable({
      variants <- selected_variants()
      if (is.null(variants) || nrow(variants) == 0) {
        return(NULL)
      }
      
      reactable(
        variants,
        columns = list(
          var_name = colDef(name = "Variant name"),
          Gene_symbol = colDef(name = "Gene name"),
          Consequence = colDef(minWidth=160)),
        selection = "multiple", onClick = "select"
      )
    })

    observeEvent(input$delete_button, {
      rows <- getReactableState("selectPathogenic_tab", "selected")
      req(rows)
      
      current_variants <- selected_variants()
      updated_variants <- current_variants[-rows, ]
      selected_variants(updated_variants)
      
      global_data <- shared_data$germline_var()
      if (!is.null(global_data) && is.data.table(global_data)) {
        global_data <- global_data[sample != selected_samples]
      } else {
        global_data <- data.table(
          sample = character(),
          var_name = character(),
          Gene_symbol = character(),
          variant_freq= character(),
          coverage_depth = character(),
          Consequence = character(),
          HGVSc = character(),
          HGVSp = character(),
          variant_type = character(),
          Feature = character(),
          clinvar_sig = character(),
          gnomAD_NFE = character()
        )
      }
      
      if (nrow(updated_variants) > 0) {
        updated_global_data <- rbind(global_data, as.data.table(updated_variants))
      } else {
        updated_global_data <- global_data
      }
      
      shared_data$germline_var(updated_global_data)
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
      selected_gene_region(filter_state$gene_region())
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
      selected_empty <- is.null(selected_variants()) || nrow(selected_variants()) == 0
      bam_empty <- is.null(shared_data$germline.bam) || length(shared_data$germline.bam) == 0
      
      if (selected_empty || bam_empty) {
        shinyalert(
          title = "No variant or patient selected",
          text = "Please select at least one variant and one patient before inspecting them in IGV.",
          type = "warning",
          showCancelButton = FALSE,
          confirmButtonText = "OK")
        
      } else {
        shared_data$navigation_context("germline")   # odkud otevíráme IGV
        
        bam_path <- get_inputs("bam_file")
        bam_list <- lapply(input$idpick, function(id_val) {
            full_path <- grep(paste0(id_val, ".*\\.bam$"), bam_path$dna.normal_bam, value = TRUE)
            list(name = id_val, file = sub(bam_path$path_to_folder, ".", full_path, fixed = TRUE))  # relativní cesta)
        })
        
        shared_data$germline.bam(bam_list)
        message("✔ Assigned germline.bam: ",paste(sapply(bam_list, `[[`, "file"), collapse = ", "))
        
        updateNavbarTabs(session = session$userData$parent_session, inputId = "navbarMenu", selected = session$userData$parent_session$ns("hidden_igv"))
      }
    })
    
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
    

    return(list(       # return information to the main module
      get_session_data = session_handlers$get_session_data,
      restore_session_data = session_handlers$restore_session_data,
      filter_state = filter_state
    ))
    
  })
}





filterTab_server <- function(id,colnames_list, data, mapped_checkbox_names) {
  moduleServer(id, function(input, output, session) {
    observe({
      clinvar_split <- unique(unlist(unique(data$clinvar_trimws)))
      clinvar_list <- sort(unique(ifelse(is.na(clinvar_split) | clinvar_split == "", "missing value", clinvar_split)))
      consequence_split <- unique(unlist(unique(data$consequence_trimws)))
      consequence_list <- sort(unique(ifelse(is.na(consequence_split) | consequence_split == "", "missing_value", consequence_split)))
      
      updatePrettyCheckboxGroup(session, "gene_regions", choices = unique(data$gene_region), selected = c("exon", "intron"),
                                prettyOptions = list(status = "primary",icon = icon("check"),outline = FALSE))
      updatePrettyCheckboxGroup(session, "clinvar_sig", choices = clinvar_list, selected = setdiff(clinvar_list, "synonymous variant"),
                                prettyOptions = list(status = "primary",icon = icon("check"),outline = FALSE))
      updatePrettyCheckboxGroup(session, "consequence", choices = consequence_list, selected = setdiff(consequence_list, "synonymous variant"),
                                prettyOptions = list(status = "primary",icon = icon("check"),outline = FALSE))
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", choices = mapped_checkbox_names[order(mapped_checkbox_names)], selected = colnames_list$default_columns,
                                prettyOptions = list(status = "primary",icon = icon("check"),outline = FALSE))
      
      if(isTruthy(is.na(input$coverage_depth))) updateNumericInput(session, "coverage_depth", value = 10)
      if(isTruthy(is.na(input$gnomAD_min))) updateNumericInput(session, "gnomAD_min", value = 0.01)
    })
    
    observeEvent(input$show_all, {
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = colnames_list$all_columns)
    })
    
    observeEvent(input$show_default, {
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = colnames_list$default_columns)
    })
    
    restore_ui_inputs <- function(data) {
      if (!is.null(data$gnomAD_min)) updateNumericInput(session, "gnomAD_min", value = safe_extract(data$gnomAD_min))
      if (!is.null(data$tumor_depth)) updateNumericInput(session, "tumor_depth", value = safe_extract(data$tumor_depth))
      if (!is.null(data$gene_regions)) updatePrettyCheckboxGroup(session, "gene_regions", selected = safe_extract(data$gene_regions))
      if (!is.null(data$clinvar_sig)) updatePrettyCheckboxGroup(session, "clinvar_sig", selected = safe_extract(data$clinvar_sig))
      if (!is.null(data$consequence)) updatePrettyCheckboxGroup(session, "consequence", selected = safe_extract(data$consequence))
      if (!is.null(data$selected_cols)) updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = safe_extract(data$selected_cols))
    }
    
    return(list(
      confirm = reactive(input$confirm_btn),
      coverage_depth = reactive(input$coverage_depth),
      gnomAD_min = reactive(input$gnomAD_min),
      gene_regions = reactive(input$gene_regions),
      clinvar_sig = reactive(input$clinvar_sig),
      consequence = reactive(input$consequence),
      selected_columns = reactive(input$colFilter_checkBox),
      restore_ui_inputs = restore_ui_inputs
    ))
  })
}


filterTab_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML(".dropdown-toggle {border-radius: 0; padding: 0; background-color: transparent; border: none; float: right;margin-top -1px;}
                               .dropdown-toggle::after {display: none !important;}
                               .glyphicon-triangle-bottom {display: none !important; width: 0 !important; margin: 0 !important; padding: 0 !important;}"))
    ),
    dropdownButton(
      label = NULL,
      right = TRUE,
      # width = "480px",
      icon = HTML('<i class="fa-solid fa-filter download-button"></i>'),
      
      fluidRow(style = "display: flex; align-items: stretch;",
               column(8,
                      box(width = 12,title = tags$div(style = "padding-top: 8px;","Filter data by:"),closable = FALSE, collapsible = FALSE,style = "height: 100%;",
                          fluidRow(
                            column(6, numericInput(ns("tumor_depth"), tags$strong("Tumor coverage min"), value = 10, min = 0)),
                            column(6, numericInput(ns("gnomAD_min"), tags$strong("gnomAD NFE min"), value = 0.01, min = 0, max = 1))
                          ),
                          div(class = "card-body two-col-checkbox-group",
                            div(
                              div(class = "two-col-checkbox-group", style = "margin-bottom: 15px;",
                                  prettyCheckboxGroup(ns("gene_regions"), label = tags$strong("Gene region"), choices = character(0))),
                              div(class = "two-col-checkbox-group",
                                  prettyCheckboxGroup(ns("clinvar_sig"),label = tags$strong("ClinVar significance"),choices = character(0)))),
                            div(class = "two-col-checkbox-group",
                                prettyCheckboxGroup(ns("consequence"),label = tags$strong("Consequence"),choices = character(0)))
                      ))
               ),
               column(4,
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

