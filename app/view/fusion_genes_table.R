# app/view/fusion_genes_table.R

box::use(
  shiny[moduleServer,NS,h3,tagList,div,textInput,renderPrint,reactive,observe,observeEvent,icon,mainPanel,titlePanel,isolate,
        uiOutput,renderUI,HTML,req,reactiveVal,column,fluidRow,showModal,modalDialog,modalButton,selectInput,downloadButton,invalidateLater,
        reactiveValues,textOutput,renderText,reactiveValuesToList],
  reactable,
  reactable[reactable,colDef,reactableOutput,renderReactable,JS,getReactableState],
  htmltools[tags, p,span,HTML],
  bs4Dash[actionButton,bs4Card,box,updateNavbarTabs],
  shinyjs[useShinyjs,runjs,hide,show],
  shinyalert[shinyalert,useShinyalert],
  data.table[data.table,uniqueN,as.data.table,copy,is.data.table,fifelse,setcolorder,fread,setnames],
  shinyWidgets[pickerInput,updatePickerInput,dropdownButton,prettyCheckboxGroup,updatePrettyCheckboxGroup,actionBttn,pickerOptions,dropdown],
)
box::use(
  app/logic/load_data[get_inputs,load_data],
  app/logic/prepare_table[prepare_fusion_genes_table], 
  app/logic/waiters[use_spinner],
  app/logic/reactable_helpers[create_clinvar_filter,create_consequence_filter,update_fusion_data],
  app/logic/filter_columns[getColFilterValues,map_checkbox_names,colnames_map_list,generate_columnsDef],
  app/logic/session_utils[create_session_handlers,safe_extract]
)

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
    fluidRow(
      div(style = "width: 100%; text-align: right;",
          dropdownButton(label = NULL,right = TRUE,width = "240px",icon = HTML('<i class="fa-solid fa-download download-button"></i>'),
                         selectInput(ns("export_data_table"), "Select data:", choices = c("All data" = "all", "Filtered data" = "filtered")),
                         selectInput(ns("export_format_table"), "Select format:", choices = c("CSV" = "csv", "TSV" = "tsv", "Excel" = "xlsx")),
                         downloadButton(ns("Table_download"),"Download")),
          filterTab_ui(ns("filterTab_dropdown")))),
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
      dropdown(label = "IGV", status = "primary", icon = icon("play"), right = TRUE, size = "md", width = "230px", 
               pickerInput(ns("idpick"), "Select patients for IGV:", choices = NULL, options = pickerOptions(actionsBox = FALSE, size = 4, maxOptions = 4, dropupAuto = FALSE, maxOptionsText = "Select max. 4 patients"),multiple = TRUE),
               div(style = "display: flex; justify-content: center; margin-top: 10px;",
                   actionBttn(ns("go2igv_button"), label = "Go to IGV", style = "stretch", color = "primary", size = "sm", individual = TRUE)
               )
      )
    )
  )

}

read_fusion_manifest <- function(sample, www_dir = "www") {
  man_path <- file.path(www_dir, "manifests", "fusion", paste0(sample, ".tsv"))
  if (!file.exists(man_path)) return(NULL)
  man_dt <- fread(man_path, na.strings = "NA")
  
  # cesty musí být relativní k www (bez ./)
  man_dt[, `:=`(svg_path = sub("^\\./", "", svg_path), png_path = sub("^\\./", "", png_path))]
  return(man_dt)
}

#' @export
server <- function(id, selected_samples, shared_data, file, load_session_btn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
   
    prerun_ready <- reactive({
      status <- shared_data$fusion_prerun_status()
      status %in% c("completed", "not_started")
    })
    
    # loading UI během prerunu (zachován tvůj vzhled)
    output$prerun_loading <- renderUI({
      if (!prerun_ready()) {
        status <- shared_data$fusion_prerun_status()
        progress <- shared_data$fusion_prerun_progress()
        div(style = "display:flex; flex-direction:column; align-items:center; justify-content:center; min-height:400px;",
          div(style = "text-align:center; padding:20px;",
              h3("Preparing Fusion Data...", style = "margin-bottom: 20px;"),
              div(style = "width:300px; background-color:#f8f9fa; border-radius:10px; padding:10px; margin:20px 0;",
                  div(style = paste0("width:", progress, "%; height:20px; background-color:#007bff; border-radius:5px; transition:width 0.3s ease;"))),
              p(paste0("Processing IGV snapshots and Arriba images... ", progress, "%"), style = "color:#6c757d; margin-top:10px;"),
              p("You can use other modules while this processes in the background.", style = "color:#6c757d; font-size:0.9em; margin-top:15px;")))
      } else NULL
    })
    

    data <- reactive({
      req(prerun_ready())
      
      message("[fusion] Loading input data for: ", file$fusion)
      data <- load_data(file$fusion, "fusion", selected_samples)
      manifest_dt <- read_fusion_manifest(selected_samples, www_dir = "www")
      patient_dt <- prepare_fusion_genes_table(selected_samples, as.data.table(data), manifest_dt, colnames(data))

      message(sprintf("[fusion] Rows: %d | has_svg: %d | has_png: %d",
                      nrow(patient_dt), sum(patient_dt$has_svg, na.rm = TRUE), sum(patient_dt$has_png, na.rm = TRUE)))
      
      # diagnostika prvního nenamapovaného řádku (pomáhá při ladění)
      if (any(is.na(patient_dt$png_path))) {
        bad <- patient_dt[is.na(png_path)][1]
        message("[fusion] example mismatch: ",
                paste(c(bad$sample, bad$gene1, bad$gene2, bad$chr1, bad$pos1, bad$chr2, bad$pos2), collapse=" | "))
      }
      
      patient_dt
    })
    
    fusion_data_to_render <- reactiveVal(NULL)
    observeEvent(data(), ignoreInit = FALSE, {
      fusion_data_to_render(data())
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
    
    filter_state <- filterTab_server("filterTab_dropdown",colnames_list, data(),mapped_checkbox_names)
    
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
                            svg_file <- dt$svg_path[index]
                            png_file <- dt$png_path[index]
                            # kontrola existence na disku bránila prázdnému <img src="">
                            svg_ok <- !is.na(svg_file) && nzchar(svg_file) && file.exists(file.path("www", svg_file))
                            png_ok <- !is.na(png_file) && nzchar(png_file) && file.exists(file.path("www", png_file))
                            
                            tags$div(
                              style = "display:flex; align-items:flex-start; gap:16px;",
                              if (svg_ok) tags$img(src = svg_file, style = "width:50%; height:auto;")
                              else tags$strong("Arriba image not available.",  style = "width:50%; text-align:center; margin:40px 0;"),
                              if (png_ok) tags$img(src = png_file, style = "width:50%; height:auto;")
                              else tags$strong("IGV snapshot not available.", style = "width:50%; text-align:center; margin:40px 0;")
                            )
                            # tags$div(
                            #   style = "display: flex; align-items: center;",
                            #   if (file.exists(paste0("www/",svg_file))) {
                            #     tags$img(src = svg_file, style = "width:50%; height:auto; display:inline-block; vertical-align:top;")
                            #   } else {
                            #     tags$strong("Starfusion doesn't provide this picture.", style = "width:10%; height:auto; display:inline-block; vertical-align:middle; margin-left: 20px; font-weight: bold; text-align: center; margin-top:40px; margin-bottom:40px;")
                            #   },
                            #   if (file.exists(paste0("www/",png_file))) {
                            #     tags$img(src = png_file, style = "width:50%; height:auto; display:inline-block; vertical-align:top; margin-bottom:40px;")
                            #   } else {
                            #     tags$strong("IGV didn't snapshot this position.", style = "width:10%; height:auto; display:inline-block; vertical-align:middle; margin-left: 20px; font-weight: bold; text-align: center; margin-top:40px; margin-bottom:40px;")
                            #   }
                            # )
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
      
      global_data <- shared_data$fusion_var()
      if (!is.null(global_data) && is.data.table(global_data)) {
        global_data <- global_data[sample != selected_samples]
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
      
      shared_data$fusion_var(updated_global_data)
      session$sendCustomMessage("resetReactableSelection", updated_variants)
      
      if (nrow(updated_variants) == 0) {
        hide("delete_button")
      }
    })
    
    fusion_selected <- reactiveVal(FALSE)
    
    # Při stisku tlačítka pro výběr fúze
    observeEvent(input$selectFusion_button, {
      if (is.null(selected_fusions()) || nrow(selected_fusions()) == 0) {
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
    
    ## update IGV button choices
    observeEvent(shared_data$fusion.patients(), {
      patient_list <- shared_data$fusion.patients()
      if (is.null(patient_list)) patient_list <- character(0)
      prev <- input$idpick; if (is.null(prev)) prev <- character(0)
      sel  <- intersect(prev, patient_list)
      if (!length(sel) && length(patient_list) && !is.null(selected_samples)) {
        sel <- intersect(selected_samples, patient_list)
      }
      updatePickerInput(session, "idpick", choices = patient_list, selected = sel)
    }, ignoreInit = FALSE)
    
    observeEvent(input$go2igv_button, {
      message("selected_fusions(): ", selected_fusions())

      
      selected_empty <- is.null(selected_fusions()) || nrow(selected_fusions()) == 0
      bam_empty <- is.null(shared_data$fusion.bam) || length(shared_data$fusion.bam) == 0
      
      if (selected_empty || bam_empty) {
        shinyalert(
          title = "No gene fusion or patient selected",
          text = "Please select at least one gene fusion and one patient before inspecting them in IGV.",
          type = "warning",
          showCancelButton = FALSE,
          confirmButtonText = "OK")
        
      } else {
        shared_data$navigation_context("fusion")   # odkud otevíráme IGV
        bam_path  <- get_inputs("bam_file")
        message("bam_path names: ", paste(names(bam_path), collapse=", "))
        
        
        # 1) Vemte ID ze zvolených fúzí (ne z input$idpick)
        ids <- unique(selected_fusions()$sample)
        message("IDs from selected_fusions(): ", paste(ids, collapse = ", "))
        
        # 2) Postavte seznam tracků bezpečně
        track_lists <- lapply(ids, function(id_val) {
          # pokud máte zvlášť vektory pro fuze a chimeric, použijte je
          # upravte názvy slotů podle toho, co vrací get_inputs("bam_file")
          fuze_vec <- bam_path$rna.fuze_bam %||% bam_path$rna.tumor_bam   # fallback, když slot neexistuje
          chim_vec <- bam_path$rna.chimeric_bam
          
          tumor_path <- grep(paste0(id_val, "\\.bam$"), fuze_vec, value = TRUE)
          chim_path  <- grep(paste0(id_val, ".*Chimeric\\.out\\.bam$"), chim_vec, value = TRUE)
          
          tracks <- list()
          if (length(tumor_path)) {
            tracks <- c(tracks, list(list(
              name = paste0(id_val, " RNA"),
              file = sub(bam_path$path_to_folder, ".", tumor_path, fixed = TRUE)
            )))
          }
          if (length(chim_path)) {
            tracks <- c(tracks, list(list(
              name = paste0(id_val, " Chimeric"),
              file = sub(bam_path$path_to_folder, ".", chim_path, fixed = TRUE)
            )))
          }
          tracks
        })
        
        bam_list <- do.call(c, track_lists)   # žádné unlist(..., recursive=FALSE)
        
        # Logy, které fakt něco ukážou
        if (length(bam_list)) {
          files <- vapply(bam_list, function(x) x$file, character(1L))
          message("✔ Assigned fusion_bam (", length(bam_list), " tracks): ", paste(files, collapse = ", "))
        } else {
          message("✖ No tracks assembled for IDs: ", paste(ids, collapse = ", "))
        }
        
        shared_data$fusion.bam(bam_list)
        
        updateNavbarTabs(session = session$userData$parent_session, inputId = "navbarMenu", selected = session$userData$parent_session$ns("hidden_igv"))
      }

      #   bam_list <- unlist(
      #     lapply(input$idpick, function(id_val) {
      #       ## 1) RNA-tumor BAM
      #       tumor_path <- grep(paste0(id_val, ".*\\.bam$"), bam_path$rna.tumor_bam, value = TRUE)
      #       tumor_track <- list(name = paste0(id_val, " RNA"), file = sub(bam_path$path_to_folder, ".", tumor_path, fixed = TRUE))
      #       
      #       ## 2) Chimeric BAM
      #       chim_path <- grep(paste0(id_val, ".*Chimeric\\.out\\.bam$"), bam_path$rna.chimeric_bam, value = TRUE)
      #       chim_track <- list(name = paste0(id_val, " Chimeric"), file = sub(bam_path$path_to_folder, ".", chim_path, fixed = TRUE))
      #       
      #       message("#####.  list(tumor_track, chim_track) ###### : ",list(tumor_track, chim_track))
      #       list(tumor_track, chim_track)    # pořadí: tumor -> chimeric
      #     }), recursive = FALSE              # nerozbalujeme úplně, zůstane list tracků
      #   )
      # 
      #   shared_data$fusion.bam(bam_list)
      #   message("✔ Assigned fusion_bam: ", paste(sapply(bam_list, `[[`, "file"), collapse = ", "))
      #   
      #   # 
      #   updateNavbarTabs(session$userData$parent_session, "navbarMenu", "app-hidden_igv")
      # }
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



filterTab_server <- function(id,colnames_list,data,mapped_checkbox_names) {
  moduleServer(id, function(input, output, session) {

    observe({
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", choices = mapped_checkbox_names[order(mapped_checkbox_names)], selected = colnames_list$default_columns,
                                prettyOptions = list(status = "primary",icon = icon("check"),outline = FALSE))
    })
    
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
               column(12,
                      box(width = 12,title = tags$div(style = "padding-top: 8px;","Select columns:"),closable = FALSE,collapsible = FALSE,height = "100%",
                          div(class = "two-col-checkbox-group",
                              prettyCheckboxGroup(ns("colFilter_checkBox"),label = NULL, choices = character(0))),
                          tags$br(),
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
