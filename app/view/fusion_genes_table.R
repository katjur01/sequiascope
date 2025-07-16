# app/view/fusion_genes_table.R

box::use(
  shiny[moduleServer,NS,h3,tagList,div,textInput,renderPrint,reactive,observe,observeEvent,icon,mainPanel,titlePanel,isolate,
        uiOutput,renderUI,HTML,req,reactiveVal,column,fluidRow,showModal,modalDialog,modalButton,selectInput,downloadButton,invalidateLater,
        reactiveValues,textOutput,renderText,reactiveValuesToList],
  reactable,
  reactable[reactable,colDef,reactableOutput,renderReactable,JS,getReactableState],
  htmltools[tags, p,span,HTML],
  bs4Dash[actionButton,bs4Card,box],
  shinyjs[useShinyjs,runjs,hide,show],
  reactablefmtr[pill_buttons,icon_assign],
  shinyalert[shinyalert,useShinyalert],
  data.table[data.table,uniqueN,as.data.table,copy,is.data.table,fifelse,setcolorder],
  shinyWidgets[pickerInput, dropdownButton,prettyCheckboxGroup,updatePrettyCheckboxGroup,actionBttn,pickerOptions,dropdown],
  stats[setNames],
  reactable.extras[reactable_extras_dependency],
)
box::use(
  app/logic/load_data[get_inputs,load_data],
  app/logic/prepare_table[prepare_fusion_genes_table,prepare_arriba_images], 
  app/logic/waiters[use_spinner],
  app/logic/patients_list[sample_list_fuze],
  app/logic/reactable_helpers[create_clinvar_filter,create_consequence_filter,update_fusion_data],
  app/logic/filter_columns[getColFilterValues,map_checkbox_names,colnames_map_list,generate_columnsDef],
  app/logic/session_utils[create_session_handlers,safe_extract]
)

# Load and process data table
input_data <- function(sample){
  filenames <- get_inputs("per_sample_file")
  # message("Loading data for fusion: ", filenames$fusions)
  data <- prepare_fusion_genes_table(load_data(filenames$fusions,"fusion",sample),sample)
  return(data)
}

##############  pozn  #####################
#ploty budu dělat pomocí balíku 
#nicméně je problém s instalací XML knihovny. 
#Důvod a co mám dělat je zde:
#  https://support.bioconductor.org/p/52539/
##########################################
#' @export
ui <- function(id) {
  ns <- NS(id)
  useShinyjs()
  tagList(
    reactable_extras_dependency(),
    fluidRow(
      div(style = "width: 100%; text-align: right;",
          dropdownButton(label = NULL,right = TRUE,width = "240px",icon = HTML('<i class="fa-solid fa-download download-button"></i>'),
                         selectInput(ns("export_data_table"), "Select data:", choices = c("All data" = "all", "Filtered data" = "filtered")),
                         selectInput(ns("export_format_table"), "Select format:", choices = c("CSV" = "csv", "TSV" = "tsv", "Excel" = "xlsx")),
                         downloadButton(ns("Table_download"),"Download")),
          uiOutput(ns("filterTab")))),
    use_spinner(reactableOutput(ns("fusion_genes_tab"))),
    tags$br(),
    div(style = "display: flex; justify-content: space-between; align-items: top; width: 100%;",
      div(
        tags$br(),
        actionButton(ns("selectFusion_button"), "Select fusion as causal", status = "info"),
        tags$br(),
        fluidRow(
          column(12,reactableOutput(ns("selectFusion_tab")))),
        tags$br(),
        fluidRow(
          column(3,actionButton(ns("delete_button"),"Delete genes", icon = icon("trash-can"))))
      ),
      dropdown(ns("igv_dropdownButton"), label = "IGV", status = "primary", icon = icon("play"), right = TRUE, size = "md",#width = 230, 
               pickerInput(ns("idpick"), "Select patients for IGV:", choices = sample_list_fuze(), options = pickerOptions(actionsBox = FALSE, size = 4, maxOptions = 4, dropupAuto = FALSE, maxOptionsText = "Select max. 4 patients"),multiple = TRUE),
               div(style = "display: flex; justify-content: center; margin-top: 10px;",
                   actionBttn(ns("go2igv_button"), label = "Go to IGV", style = "stretch", color = "primary", size = "sm", individual = TRUE)
               )
      )
    )
  )

}

#' @export
server <- function(id, selected_samples, shared_data, load_session_btn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # prepare_arriba_images(selected_samples)
    
  # Call loading function to load data
    data <- reactive({
      message("Loading input data for fusion")
      input_data(selected_samples) 
    })

    
    observe({
      req(data())
      dt <- data()
      overview_dt <- data.table(
        high_confidence = uniqueN(dt[arriba.confidence %in% "high"]),
        potencially_fused = uniqueN(dt[arriba.confidence %in% c("medium", "low", NA)]))
      shared_data$fusion_overview[[ selected_samples ]] <- overview_dt
    })

    colnames_list <- getColFilterValues("fusion") # gives list of all_columns and default_columns
    map_list <- colnames_map_list("fusion",session = session) # gives list of all columns with their column definitions
    mapped_checkbox_names <- map_checkbox_names(map_list) # gives list of all columns with their display names for checkbox
    
    
    output$filterTab <- renderUI({
      req(data())
      req(map_list)
      filterTab_ui(ns("filterTab_dropdown"),data(), colnames_list$default_columns, mapped_checkbox_names)
    })
    
    filter_state <- filterTab_server("filterTab_dropdown",colnames_list)
    
    
    ############
    notes_state <- reactiveVal(data.frame(row = integer(), value = character()))
    selected_columns <- reactiveVal(colnames_list$default_columns)
    selected_fusions <- reactiveVal(data.frame(gene1 = character(), gene2 = character()))
    visual_check_state <- reactiveVal(data.frame(row = integer(), value = character()))
    fusion_data_to_render <- reactiveVal(NULL)
      
      
  # Call generate_columnsDef to generate colDef setting for reactable
    column_defs <- reactive({
      req(data())
      req(selected_columns())
      generate_columnsDef(names(data()), selected_columns(), "fusion", map_list)
    })

    output$fusion_genes_tab <- renderReactable({
      req(fusion_data_to_render())
      pathogenic_fusions <- selected_fusions() # seznam fúzí, které byly označeny jako patogenní
      dt <- fusion_data_to_render()

      message("Rendering Reactable for fusion")
      reactable(as.data.frame(dt),
                          columns = column_defs(),
                          class = "fusion-table",
                          resizable = TRUE,
                          showPageSizeOptions = TRUE,
                          pageSizeOptions = c(10, 20, 50, 100),
                          defaultPageSize = 10,
                          striped = TRUE,
                          wrap = FALSE,
                          highlight = TRUE,
                          outlined = TRUE,
                          defaultColDef = colDef(
                            align = "center",
                            sortNALast = TRUE
                          ),
                          defaultSorted = list("arriba.confidence" = "asc","arriba.called" = "desc","starfus.called" = "desc"),
                          details = function(index) {
                            # row <- data[index, ]
                            svg_file <- dt$svg_path[index]
                            png_file <- dt$png_path[index]
                            tags$div(
                              style = "display: flex; align-items: center;",
                              if (file.exists(paste0("www/",svg_file))) {
                                tags$img(src = svg_file, style = "width:50%; height:auto; display:inline-block; vertical-align:top;")
                              } else {
                                tags$strong("Starfusion doesn't provide this picture.", style = "width:10%; height:auto; display:inline-block; vertical-align:middle; margin-left: 20px; font-weight: bold; text-align: center; margin-top:40px; margin-bottom:40px;")
                              },
                              if (file.exists(paste0("www/",png_file))) {
                                tags$img(src = png_file, style = "width:50%; height:auto; display:inline-block; vertical-align:top; margin-bottom:40px;")
                              } else {
                                tags$strong("IGV didn't snapshot this position.", style = "width:10%; height:auto; display:inline-block; vertical-align:middle; margin-left: 20px; font-weight: bold; text-align: center; margin-top:40px; margin-bottom:40px;")
                              }
                            )
                          },
                          rowStyle = function(index) {
                            gene1_in_row <- dt$gene1[index]
                            gene2_in_row <- dt$gene2[index]
                            
                            # Pokud je aktuální fúze v seznamu vybraných fúzí, obarvíme ji
                            if ((gene1_in_row %in% pathogenic_fusions$gene1 & 
                                 gene2_in_row %in% pathogenic_fusions$gene2) |
                                (gene1_in_row %in% pathogenic_fusions$gene2 & 
                                 gene2_in_row %in% pathogenic_fusions$gene1)) {
                              list(backgroundColor = "#B5E3B6", fontWeight = "bold")
                            } else {
                              NULL
                            }
                          },
                          selection = "multiple"
        )
  })

    ### Observe changes in the Notes input field (captured via JS onblur event)
    ### When the user edits a note, update the notes_state() reactive value
    ### If the note for the given row already exists → update it
    ### If it's a new note → append it as a new entry
    observeEvent(input[[paste0("notes_input", "_changed")]], {
      changed_data <- input[[paste0("notes_input", "_changed")]]
      row_index <- changed_data$row + 1  # JS 0-based → R 1-based
      new_value <- changed_data$value
      
      current_notes <- notes_state()
      if (row_index %in% current_notes$row) {
        current_notes$value[current_notes$row == row_index] <- new_value
      } else {
        current_notes <- rbind(current_notes, data.frame(row = row_index, value = new_value))
      }
      notes_state(current_notes)
    })
    
    ### Update the fusion data table whenever:
    ### - the source data is loaded
    ### - the user triggers session loading
    ### - visual check states or notes change
    observe({
      req(data())
      update_fusion_data(data(), visual_check_state(), notes_state(), fusion_data_to_render) })
    observeEvent(load_session_btn(),   { 
      update_fusion_data(data(), visual_check_state(), notes_state(), fusion_data_to_render) })
    observeEvent(visual_check_state(), { 
      update_fusion_data(data(), visual_check_state(), notes_state(), fusion_data_to_render) })

    ### Observe changes in the Visual Check radio buttons (captured via JS change event)
    ### Same logic as with Notes
    observeEvent(input[[paste0("visual_check", "_changed")]], {
      changed_data <- input[[paste0("visual_check", "_changed")]]
      row_index <- changed_data$row + 1  # JS 0-based → R 1-based
      new_value <- changed_data$value
      
      current_state <- visual_check_state()
      if (row_index %in% current_state$row) {
        current_state$value[current_state$row == row_index] <- new_value
      } else {
        current_state <- rbind(current_state, data.frame(row = row_index, value = new_value))
      }
      visual_check_state(current_state)
    })

    
    # Akce po kliknutí na tlačítko pro přidání fúze
    observeEvent(input$selectFusion_button, {
      selected_rows <- getReactableState("fusion_genes_tab", "selected")
      req(selected_rows)
      
      new_variants <- data()[selected_rows, c("gene1","gene2","overall_support","position1","position2","arriba.confidence","arriba.site1","arriba.site2")]  # Získání vybraných fúzí
      new_variants$sample <- selected_samples
      current_variants <- selected_fusions()  # Stávající přidané varianty
      new_unique_variants <- new_variants[!(new_variants$gene1 %in% current_variants$gene1 &       # Porovnání - přidáme pouze ty varianty, které ještě nejsou v tabulce
                                              new_variants$gene2 %in% current_variants$gene2), ]
      
      if (nrow(new_unique_variants) > 0) {      # Přidáme pouze unikátní varianty
        selected_fusions(rbind(current_variants, new_unique_variants))
      }
      
      # Aktualizace globální proměnné shared_data$germline_data:
      global_data <- shared_data$fusion_var()

      # Pokud je NULL nebo nemá správnou strukturu, inicializujeme
      if (is.null(global_data) || !is.data.table(global_data) || !("sample" %in% names(global_data))) {
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
      
      global_data <- global_data[sample != selected_samples]
      
      # Přidáme nově aktualizované lokální data daného pacienta
      updated_global_data <- rbind(global_data, selected_fusions())
      shared_data$fusion_var(updated_global_data)
      message("## shared_data$fusion_var(): ", shared_data$fusion_var())
    })
    
    output$selectFusion_tab <- renderReactable({
      variants <- selected_fusions()
      if (is.null(variants) || nrow(variants) == 0) {
        return(NULL)
      }
      
      reactable(
        variants,
        columns = list(
          gene1 = colDef(name = "Gene1"),
          gene2 = colDef(name = "Gene1")
          # remove = colDef(
          #   name = "",
          #   cell = function(value, index) {
          #     # actionButton(session$ns("delete"), label = "", class = "btn btn-danger btn-sm", icon = icon("remove"))
          #     # actionButton(session$ns(paste0("remove_", index)), label = "", class = "btn btn-danger btn-sm", icon = icon("remove"))
          #   })
        ),
        selection = "multiple", onClick = "select"
      )
    })
    
    observeEvent(input$delete_button, {
      rows <- getReactableState("selectFusion_tab", "selected")
      req(rows)
      current_variants <- selected_fusions()
      updated_variants <- current_variants[-rows, ]
      
      selected_fusions(updated_variants)
      shared_data$fusion_var(updated_variants)
      
      session$sendCustomMessage("resetReactableSelection",selected_fusions())
      
      if (nrow(selected_fusions()) == 0) {
        hide("delete_button")
      }
    })
    

    fusion_selected <- reactiveVal(FALSE)
    
    # Při stisku tlačítka pro výběr fúze
    observeEvent(input$selectFusion_button, {
      if (nrow(selected_fusions()) == 0) {
        # Pokud nejsou vybrány žádné řádky, zůstaň u původního stavu
        fusion_selected(FALSE)
        hide("delete_button")
        
        shinyalert(
          title = "No fusion selected",
          text = "Please select the potentially causal fusions from table above.",
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
        fusion_selected(TRUE)
        
        # Zobraz tlačítka pomocí shinyjs

        show("delete_button")
      }
    })
    
    observe({
      variants <- selected_fusions()
      
      if (!is.null(variants) && nrow(variants) > 0) {
        show("delete_button")
      } else {
        hide("delete_button")
      }
    })

    observeEvent(filter_state$confirm(), {
      message("🟢 Confirm button was clicked")
      selected_columns(filter_state$selected_columns())
    })
    
    #############
    ## run IGV ##
    #############
    
    observeEvent(input$go2igv_button, {
      selected_empty <- is.null(selected_fusions()) || nrow(selected_fusions()) == 0
      bam_empty <- is.null(shared_data$fusion_bam) || length(shared_data$fusion_bam) == 0
      
      if (selected_empty || bam_empty) {
        showModal(modalDialog(
          title = "Missing input",
          "You have not selected fusions or patients for visualization. Please return to the Fusion gene detection tab and define them.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        
      } else {
        shared_data$navigation_context("fusion")   # odkud otevíráme IGV
        bam_path  <- get_inputs("bam_file")
        
        bam_list <- unlist(
          lapply(input$idpick, function(id_val) {
            ## 1) RNA-tumor BAM
            tumor_path <- grep(paste0(id_val, ".*\\.bam$"), bam_path$rna.tumor_bam, value = TRUE)
            tumor_track <- list(name = paste0(id_val, " RNA"), file = sub(bam_path$path_to_folder, ".", tumor_path, fixed = TRUE))
            
            ## 2) Chimeric BAM
            chim_path <- grep(paste0(id_val, ".*Chimeric\\.out\\.bam$"), bam_path$rna.chimeric_bam, value = TRUE)
            chim_track <- list(name = paste0(id_val, " Chimeric"), file = sub(bam_path$path_to_folder, ".", chim_path, fixed = TRUE))
            
            message("#####.  list(tumor_track, chim_track) ###### : ",list(tumor_track, chim_track))
            list(tumor_track, chim_track)    # pořadí: tumor -> chimeric
          }), recursive = FALSE              # nerozbalujeme úplně, zůstane list tracků
        )
      
        shared_data$fusion_bam(bam_list)
        message("✔ Assigned fusion_bam: ", paste(sapply(bam_list, `[[`, "file"), collapse = ", "))
        
        shinyjs::runjs("document.querySelector('[data-value=\"app-hidden_igv\"]').click();")
      }
    })

    ###########################
    ## get / restore session ##
    ###########################
    
    
    session_handlers <- create_session_handlers(
      selected_inputs = list(
        selected_cols = selected_columns,
        selected_vars = selected_fusions,
        visual_check_state = visual_check_state,
        notes_state = notes_state
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



filterTab_server <- function(id,colnames_list) {
  moduleServer(id, function(input, output, session) {

    # observe({
    #   if(isTruthy(is.na(input$tumor_depth))) updateNumericInput(session, "tumor_depth", value = 10)
    # })
    # observe({
    #   if(isTruthy(is.na(input$gnomAD_min))) updateNumericInput(session, "gnomAD_min", value = 0.01)
    # })
    
    observeEvent(input$show_all, {
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = colnames_list$all_columns)
    })
    
    observeEvent(input$show_default, {
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = colnames_list$default_columns)
    })

    restore_ui_inputs <- function(data) {
      if (!is.null(data$selected_cols)) updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = safe_extract(data$selected_cols))
    }
    
    return(list(
      confirm = reactive(input$confirm_btn),
      # tumor_depth = reactive(input$tumor_depth),
      # gnomAD_min = reactive(input$gnomAD_min),
      # gene_regions = reactive(input$gene_regions),
      # consequence = reactive(input$consequence),
      selected_columns = reactive(input$colFilter_checkBox),
      restore_ui_inputs = restore_ui_inputs
    ))
  })
}



filterTab_ui <- function(id, data, default_columns, mapped_checkbox_names){
  ns <- NS(id)
  filenames <- get_inputs("per_sample_file")
  file_paths <- filenames$fusions[1]
  patient_names <- substr(basename(file_paths), 1, 6)
  
  tagList(
    tags$head(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"),
              tags$style(HTML(".dropdown-toggle {border-radius: 0; padding: 0; background-color: transparent; border: none; float: right;margin-top -1px;}
                    .checkbox label {font-weight: normal !important;}
                    .checkbox-group .checkbox {margin-bottom: 0px !important;}
                    .my-blue-btn {background-color: #007bff;color: white;border: none;}
                    .dropdown-menu .bootstrap-select .dropdown-toggle {border: 1px solid #ced4da !important; background-color: #fff !important;
                      color: #495057 !important; height: 38px !important; font-size: 16px !important; border-radius: 4px !important;
                      box-shadow: none !important;}
                    .sw-dropdown-content {border: 1px solid #ced4da !important; border-radius: 4px !important; box-shadow: none !important;
                      background-color: white !important;}
                    .glyphicon-triangle-bottom {font-size: 12px !important; line-height: 12px !important; vertical-align: middle;}
                    .glyphicon-triangle-bottom {display: none !important; width: 0 !important; margin: 0 !important; padding: 0 !important;}
                    #app-fusion_genes_tab-igv_dropdownButton {width: 230px !important; height: 38px !important; font-size: 16px !important;}
                    "))
    ),
    dropdownButton(
      label = NULL,
      right = TRUE,
      # width = "480px",
      icon = HTML('<i class="fa-solid fa-filter download-button"></i>'),
      fluidRow(style = "display: flex; align-items: stretch;",
               column(12,
                      box(width = 12,title = tags$div(style = "padding-top: 8px;","Select columns:"),closable = FALSE,collapsible = FALSE,height = "100%",
                          div(class = "two-col-checkbox-group",
                              prettyCheckboxGroup(
                                inputId = ns("colFilter_checkBox"),
                                label = NULL,
                                choices = mapped_checkbox_names[order(mapped_checkbox_names)],
                                selected = default_columns,
                                icon = icon("check"),
                                status = "primary",
                                outline = FALSE
                              )
                          ),
                          div(style = "display: flex; gap: 10px; width: 100%;",
                              actionButton(ns("show_all"), label = "Show All", style = "flex-grow: 1; width: 0;"),
                              actionButton(ns("show_default"), label = "Show Default", style = "flex-grow: 1; width: 0;"))
                      )
               )
      ),
      tags$br(),
      div(style = "display: flex; justify-content: center; margin-top: 10px;",
          actionBttn(ns("confirm_btn"),"Apply changes",style = "stretch",color = "success",size = "md",individual = TRUE,value = 0))
    )
  )
}


###### build_igv_tracks selected_bams ######: list(name = "DZ1601tumor", file = character(0))list(name = "DZ1601normal", file = "./230426_MOII_e117_krve/mapped/DZ1601krev.bam")list(name = "MR1507tumor", file = character(0))list(name = "MR1507normal", file = "./230426_MOII_e117_krve/mapped/MR1507krev.bam")
#
# fusionGenes_Ui <- fluidPage(
#   piechart_input("plots")
#   # table_ui("geneFusion_tab")
# )
#
# fusionGenes_Server <- function(input, output, session){
#   piechart_server("plots")
#   # table_server("geneFusion_tab")
# }
#
# shinyApp(ui = fusionGenes_Ui, server = fusionGenes_Server)
