# app/view/fusion_genes_table.R

box::use(
  shiny[moduleServer,NS,h3,tagList,div,textInput,renderPrint,reactive,observe,observeEvent,icon,mainPanel,titlePanel,isolate,isTruthy,
        uiOutput,renderUI,HTML,req,reactiveVal,column,fluidRow,showModal,modalDialog,modalButton,selectInput,downloadButton,invalidateLater,
        reactiveValues,textOutput,renderText,reactiveValuesToList],
  reactable,
  reactable[reactable,colDef,reactableOutput,renderReactable,JS,getReactableState],
  htmltools[tags, p,span,HTML],
  bs4Dash[actionButton,bs4Card,box,updateNavbarTabs],
  shinyjs[useShinyjs,runjs,hide,show],
  shinyalert[shinyalert,useShinyalert],
  data.table[data.table,uniqueN,as.data.table,copy,is.data.table,fifelse,setcolorder,fread,setnames,rbindlist],
  shinyWidgets[pickerInput,updatePickerInput,dropdownButton,prettyCheckboxGroup,updatePrettyCheckboxGroup,actionBttn,pickerOptions,dropdown],
  stats[setNames],
  waiter[waiter_show, waiter_hide, spin_fading_circles],
  tools[file_path_sans_ext],
)
box::use(
  app/logic/load_data[load_data],
  app/logic/prepare_table[prepare_fusion_genes_table], 
  app/logic/waiters[use_spinner],
  app/logic/helper_reactable[create_clinvar_filter,create_consequence_filter,update_fusion_data],
  app/logic/filter_columns[map_checkbox_names,colnames_map_list,generate_columnsDef],
  app/logic/session_utils[create_session_handlers, register_module, safe_extract, ch],
  app/logic/helper_main[get_files_by_patient],
  app/logic/export_functions[get_table_download_handler]
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
    tags$head(tags$style(HTML(".download-dropdown-wrapper .dropdown-toggle {border-radius: 0; padding: 0; background-color: transparent; border: none; float: right; margin-top: -1px;}
                               .download-dropdown-wrapper .dropdown-toggle::after {display: none !important;}
                               .download-dropdown-wrapper .glyphicon-triangle-bottom {display: none !important; width: 0 !important; margin: 0 !important; padding: 0 !important;}
                               button:has(.fa-play) .glyphicon-triangle-bottom { display: none !important; }
                               button:has(.fa-play) .fa-play { font-size: 0.75em; }"))),
    div(style = "position: relative;",
      uiOutput(ns("prerun_loading")),
      div(id = ns("main_content"),
      fluidRow(
        div(class = "download-dropdown-wrapper", style = "width: 100%; text-align: right; display: flex; flex-direction: row-reverse;",
            dropdownButton(label = NULL,right = TRUE,width = "240px",icon = HTML('<i class="fa-solid fa-download download-button"></i>'),
                           selectInput(ns("export_data_table"), "Select data:", choices = c("All data" = "all", "Filtered data" = "filtered")),
                           selectInput(ns("export_format_table"), "Select format:", choices = c("CSV" = "csv", "TSV" = "tsv", "Excel" = "xlsx")),
                           downloadButton(ns("Table_download"),"Download")),
            filterTab_ui(ns("filterTab_dropdown")))),
      use_spinner(reactableOutput(ns("fusion_genes_tab"))),
      tags$br(),
      div(style = "display: flex; justify-content: space-between; align-items: top; width: 100%;",
        column(6,
          tags$br(),
          actionButton(ns("selectFusion_button"), "Select fusion as causal", status = "info"),
          tags$br(),
          fluidRow(
            column(12,reactableOutput(ns("selectFusion_tab")))),
          tags$br(),
          fluidRow(
            column(4,actionButton(ns("delete_button"),"Delete genes", icon = icon("trash-can"))))
        ),
        dropdown(label = "IGV", status = "primary", icon = icon("play"), right = TRUE, size = "md", width = "230px", 
                 pickerInput(ns("idpick"), "Select patients for IGV:", choices = NULL, options = pickerOptions(actionsBox = FALSE, size = 4, maxOptions = 4, dropupAuto = FALSE, maxOptionsText = "Select max. 4 patients"),multiple = TRUE),
                 div(style = "display: flex; justify-content: center; margin-top: 10px;",
                     actionBttn(ns("go2igv_button"), label = "Go to IGV", style = "stretch", color = "primary", size = "sm", individual = TRUE)
                 )
        )
      )
    )
    ) # end of relative wrapper
  )

}

read_fusion_manifest <- function(sample, www_dir) {
  man_path <- file.path(www_dir, "manifests", "fusion", paste0(sample, ".tsv"))
  if (!file.exists(man_path)) return(NULL)
  man_dt <- fread(man_path, na.strings = "NA")
  
  # cesty musí být relativní k www (bez ./)
  man_dt[, `:=`(svg_path = sub("^\\./", "", svg_path), png_path = sub("^\\./", "", png_path))]
  return(man_dt)
}

#' @export
server <- function(id, selected_samples, shared_data, file, file_list, load_session_btn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
   
    is_restoring_session <- reactiveVal(FALSE)
    
    # IGV snapshot watcher - sleduje .progress a .done soubory
    observe({
      patient_id <- selected_samples
      
      # Kontrolovat pouze pokud prerun běží
      if (patient_id %in% names(shared_data$fusion_prerun_status)) {
        status <- shared_data$fusion_prerun_status[[patient_id]]()
        
        if (status == "running") {
          # Pravidelná kontrola souborů (každé 2 sekundy)
          invalidateLater(2000)
          
          batch_file <- file.path(shared_data$output_path(), "igv_snapshots", patient_id, paste0(patient_id, "_batch.txt"))
          done_file <- paste0(file_path_sans_ext(batch_file), ".done")
          progress_file <- paste0(file_path_sans_ext(batch_file), ".progress")
          
          # Číst .progress soubor pokud existuje
          if (file.exists(progress_file)) {
            tryCatch({
              prog_content <- readLines(progress_file, warn = FALSE)
              if (length(prog_content) > 0) {
                # Format: "5/20" → extract first number
                parts <- strsplit(prog_content[1], "/")[[1]]
                if (length(parts) == 2) {
                  completed <- as.numeric(parts[1])
                  total <- as.numeric(parts[2])
                  if (!is.na(completed) && !is.na(total) && total > 0) {
                    # IGV snapshots are ~70% of total work (30% is Arriba PDF→SVG)
                    igv_progress <- round((completed / total) * 70)
                    if (patient_id %in% names(shared_data$fusion_prerun_progress)) {
                      current_prog <- shared_data$fusion_prerun_progress[[patient_id]]()
                      # Only update if new progress is higher (avoid going backwards)
                      if (igv_progress > current_prog) {
                        shared_data$fusion_prerun_progress[[patient_id]](igv_progress)
                      }
                    }
                  }
                }
              }
            }, error = function(e) {
              # Ignore read errors (file might be being written)
            })
          }
          
          # Zkontrolovat .done soubor
          if (file.exists(done_file)) {
            message("IGV snapshots completed for ", patient_id, " - found .done file")
            # Odstranit .done soubor
            if (file.exists(done_file)) file.remove(done_file)
            # Aktualizovat progress na 100%
            if (patient_id %in% names(shared_data$fusion_prerun_progress)) {
              shared_data$fusion_prerun_progress[[patient_id]](100)
            }
          }
        }
      }
    })
    
    # Check per-patient prerun status
    prerun_ready <- reactive({
      patient_id <- selected_samples
      
      # Check if this patient has a status tracker
      if (patient_id %in% names(shared_data$fusion_prerun_status)) {
        status <- shared_data$fusion_prerun_status[[patient_id]]()
        return(status %in% c("completed", "failed"))
      }
      
      # If no tracker exists, assume ready (already processed or no prerun needed)
      return(TRUE)
    })
    
    # Show/hide main content based on prerun status  
    observe({
      if (prerun_ready()) {
        show("main_content")
      } else {
        hide("main_content")
      }
    })
    
    # Loading UI během prerunu - pokryje celou kartu tmavým overlay
    output$prerun_loading <- renderUI({
      if (!prerun_ready()) {
        patient_id <- selected_samples
        
        # Get per-patient status and progress
        status <- if (patient_id %in% names(shared_data$fusion_prerun_status)) {
          shared_data$fusion_prerun_status[[patient_id]]()
        } else {
          "not_started"
        }
        
        progress <- if (patient_id %in% names(shared_data$fusion_prerun_progress)) {
          shared_data$fusion_prerun_progress[[patient_id]]()
        } else {
          0
        }
        
        # Get total fusions count
        total_fusions <- if (patient_id %in% names(shared_data$fusion_prerun_total_fusions)) {
          shared_data$fusion_prerun_total_fusions[[patient_id]]()
        } else {
          NULL
        }
        
        # Estimate time (approximately 0.5-1 second per fusion for IGV snapshots)
        # For very large datasets, this helps users understand the wait time
        est_time_text <- ""
        if (!is.null(total_fusions) && total_fusions > 0) {
          est_minutes <- ceiling(total_fusions * 0.5 / 60)  # Conservative estimate
          if (est_minutes > 5) {
            est_time_text <- sprintf(" (estimated time: ~%d minutes)", est_minutes)
          }
        }
        
        # Fusion count text
        fusion_info <- if (!is.null(total_fusions) && total_fusions > 0) {
          paste0(total_fusions, " fusion", if(total_fusions > 1) "s" else "", " detected", est_time_text)
        } else {
          "Counting fusions..."
        }
        
        div(
          style = "position: absolute;
                   top: 0;
                   left: 0;
                   right: 0;
                   bottom: 0;
                   min-height: 1000px;
                   background: rgba(52, 58, 64, 0.95);
                   display: flex; 
                   flex-direction: column;
                   align-items: center; 
                   justify-content: center; 
                   z-index: 1000;
                   padding: 40px;",
          div(
            style = "text-align: center; max-width: 600px;",
            tags$div(
              style = "font-size: 48px; color: #74c0fc; margin-bottom: 20px;",
              HTML('<i class="fas fa-spinner fa-spin"></i>')
            ),
            h3(paste0("Preparing Fusion Data for ", patient_id, "..."), 
               style = "margin-bottom: 10px; color: #fff;"),
            tags$div(
              style = "font-size: 1.2em; color: #4dabf7; margin-bottom: 25px; font-weight: 600;",
              fusion_info
            ),
            tags$div(
              style = "width: 100%; background-color: rgba(255, 255, 255, 0.2); border-radius: 10px; padding: 5px; margin: 20px 0;",
              tags$div(
                style = paste0("width: ", progress, "%; height: 30px; background: linear-gradient(90deg, #74c0fc, #4dabf7); 
                               border-radius: 8px; transition: width 0.3s ease;")
              )
            ),
            tags$p(
              {
                stage <- if (progress <= 15) {
                  "Processing Arriba images"       # 0–15 %: Arriba PDF → PNG
                } else if (progress <= 90) {
                  "Processing IGV snapshots"       # 15–90 %: IGV batch watcher
                } else {
                  "Creating fusion manifest"       # 90–100 %: writing .tsv manifest
                }
                paste0(stage, "... ", progress, "%")
              },
              style = "color: #e9ecef; margin-top: 15px; font-size: 1.1em; font-weight: 500;"
            ),
            tags$p(
              "You can use other modules while this processes in the background.",
              style = "color: #adb5bd; font-size: 0.95em; margin-top: 20px;"
            ),
            if (!is.null(total_fusions) && total_fusions > 100) {
              tags$p(
                HTML('<i class="fas fa-info-circle"></i> Large dataset detected. This may take some time to complete.'),
                style = "color: #ffc078; font-size: 0.9em; margin-top: 15px; font-style: italic;"
              )
            }
          )
        )
      } else {
        NULL
      }
    })
    

    prepare_data <- reactive({
      req(prerun_ready())
      
      session_dir <- shared_data$session_dir()

      message("[fusion] Loading input data for: ", file$fusion)
      data <- load_data(file$fusion, "fusion", selected_samples)
      manifest_dt <- read_fusion_manifest(selected_samples, www_dir = session_dir)
      # Add arriba.confidence_sort to column names since it will be created in prepare function
      all_col_names <- c(colnames(data), "arriba.confidence_sort")
      result <- prepare_fusion_genes_table(selected_samples, as.data.table(data), manifest_dt, all_col_names, shared_data, session = list(ns = session$ns))
      
      # prepare_fusion_genes_table returns list(dt, columns) when manifest exists,
      # or bare data.table when manifest is NULL (should not happen in normal workflow)
      patient_dt <- if (is.list(result) && !is.data.table(result)) result$dt else result

      message(sprintf("[fusion] Rows: %d | has_svg: %d | has_png: %d",
                      nrow(patient_dt), sum(patient_dt$has_svg, na.rm = TRUE), sum(patient_dt$has_png, na.rm = TRUE)))
      
      # diagnostika prvního nenamapovaného řádku (pomáhá při ladění)
      if (any(is.na(patient_dt$png_path))) {
        bad <- patient_dt[is.na(png_path)][1]
        message("[fusion] example mismatch: ",
                paste(c(bad$sample, bad$gene1, bad$gene2, bad$chr1, bad$pos1, bad$chr2, bad$pos2), collapse=" | "))
      }
      
      result
    })
    
    data <- reactive(prepare_data()$dt)
    colnames_list <- reactive({
      res <- prepare_data()
      if (is.list(res) && !is.data.table(res)) res$columns else NULL
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
      shared_data$fusion.overview[[ selected_samples ]] <- overview_dt
    })

    map_list <- colnames_map_list("fusion",session = list(ns = session$ns)) # gives list of all columns with their column definitions
    # mapped_checkbox_names <- map_checkbox_names(map_list) # gives list of all columns with their display names for checkbox
    mapped_checkbox_names <- reactive({
      req(data())
      req(colnames_list())
      map_checkbox_names(map_list, colnames_list()$all_columns)
    })
    
    filter_state <- filterTab_server("filterTab_dropdown", colnames_list, data(), mapped_checkbox_names, is_restoring = is_restoring_session)
    
    ############
    notes_state <- reactiveVal(data.frame(row = integer(), value = character()))
    selected_columns <- reactiveVal(NULL)
    observe({
      cl <- colnames_list()
      if (!is.null(cl) && !is.null(cl$default_columns) && is.null(isolate(selected_columns()))) {
        selected_columns(cl$default_columns)
      }
    })
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
                          defaultSorted = {
                            # Dynamicky vytvoř defaultSorted jen pro sloupce které existují
                            sort_spec <- list("arriba.confidence_sort" = "desc", "arriba.called" = "desc", "starfus.called" = "desc")
                            existing_sort <- sort_spec[names(sort_spec) %in% names(dt)]
                            if (length(existing_sort) > 0) existing_sort else NULL
                          },
                          details = function(index) {
                            svg_file <- dt$svg_path[index]
                            png_file <- dt$png_path[index]
                            output_dir <- shared_data$session_dir()
                            
                            # Helper function to read and encode image as base64 data URI
                            read_as_base64 <- function(file_path, mime_type) {
                              if (!file.exists(file_path)) return(NULL)
                              tryCatch({
                                raw_data <- readBin(file_path, "raw", file.info(file_path)$size)
                                encoded <- base64enc::base64encode(raw_data)
                                paste0("data:", mime_type, ";base64,", encoded)
                              }, error = function(e) {
                                message("[IMG ERROR] Failed to encode ", file_path, ": ", e$message)
                                NULL
                              })
                            }
                            
                            # Build full paths and check existence
                            svg_path <- if (!is.na(svg_file) && nzchar(svg_file)) file.path(output_dir, svg_file) else NULL
                            png_path <- if (!is.na(png_file) && nzchar(png_file)) file.path(output_dir, png_file) else NULL
                            
                            svg_ok <- !is.null(svg_path) && file.exists(svg_path)
                            png_ok <- !is.null(png_path) && file.exists(png_path)
                            
                            # Encode to base64 data URIs (no URL needed!)
                            svg_data <- if (svg_ok) read_as_base64(svg_path, "image/svg+xml") else NULL
                            png_data <- if (png_ok) read_as_base64(png_path, "image/png") else NULL
                            
                            tags$div(
                              style = "display:flex; align-items:flex-start; gap:16px;",
                              if (!is.null(svg_data)) tags$img(src = svg_data, style = "width:50%; height:auto;")
                              else tags$strong("Arriba image not available.",  style = "width:50%; text-align:center; margin:40px 0;"),
                              if (!is.null(png_data)) tags$img(src = png_data, style = "width:50%; height:auto;")
                              else tags$strong("IGV snapshot not available.", style = "width:50%; text-align:center; margin:40px 0;")
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
      
      new_variants <- data()[selected_rows, c("gene1","gene2","overall_support","arriba.reading_frame","position1","position2","arriba.confidence","arriba.site1","arriba.site2")]  # Získání vybraných fúzí
      new_variants$sample <- selected_samples
      current_variants <- selected_fusions()  # Stávající přidané varianty
      new_unique_variants <- new_variants[!(new_variants$gene1 %in% current_variants$gene1 &       # Porovnání - přidáme pouze ty varianty, které ještě nejsou v tabulce
                                              new_variants$gene2 %in% current_variants$gene2), ]
      
      if (nrow(new_unique_variants) > 0) {      # Přidáme pouze unikátní varianty
        selected_fusions(rbind(current_variants, new_unique_variants))
      }
      
      # Aktualizace globální proměnné shared_data$germline_data:
      global_data <- shared_data$fusion.variants()

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
      updated_global_data <- rbindlist(list(global_data, selected_fusions()), use.names = TRUE, fill = TRUE)
      shared_data$fusion.variants(updated_global_data)
      message("## shared_data$fusion.variants(): ", shared_data$fusion.variants())
    })
    
    output$selectFusion_tab <- renderReactable({
      variants <- selected_fusions()
      if (is.null(variants) || nrow(variants) == 0) {
        return(NULL)
      }
      
      reactable(
        variants <- as.data.table(variants)[,.(gene1,gene2,overall_support,arriba.reading_frame,position1,position2,arriba.confidence)],
        columns = list(
          gene1 = colDef(name = "Gene1"),
          gene2 = colDef(name = "Gene2"),
          overall_support = colDef(name = "Overall support"),
          arriba.reading_frame = colDef(name = "Reading frame"),
          position1 = colDef(name = "Position1",minWidth = 150),
          position2 = colDef(name = "Position2",minWidth = 150),
          arriba.confidence = colDef(name = "Arriba confidence")),
        selection = "multiple", onClick = "select"
      )
    })

    observeEvent(input$delete_button, {
      rows <- getReactableState("selectFusion_tab", "selected")
      req(rows)
      
      current_variants <- selected_fusions()
      updated_variants <- current_variants[-rows, ]
      selected_fusions(updated_variants)
      
      global_data <- shared_data$fusion.variants()
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
        updated_global_data <- rbindlist(list(global_data, as.data.table(updated_variants)), use.names = TRUE, fill = TRUE)
      } else {
        updated_global_data <- global_data
      }
      
      shared_data$fusion.variants(updated_global_data)
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
      bam_empty <- is.null(file$tumor) && is.null(file$chimeric)
      
      if (selected_empty) {
        shinyalert(
          title = "No gene fusionselected",
          text = "Please select at least one gene fusion before inspecting them in IGV.",
          type = "warning",
          showCancelButton = FALSE,
          confirmButtonText = "OK")
        
      } else {
        shared_data$navigation_context("fusion")   # odkud otevíráme IGV
    
        selected_patients <- unique(selected_fusions()$sample)
        message("Selected patients for IGV (germline): ", paste(selected_patients, collapse = ", "))
    
        # 2) Postavte seznam tracků bezpečně
        track_lists <- lapply(selected_patients, function(id_val) {
          tracks <- list()
          
          if (!id_val %in% names(file_list)) {
            message("No fusion files found for patient: ", id_val)
            return(tracks)
          }
          
          fresh_fl      <- get_files_by_patient(isolate(shared_data$confirmed_paths()), "fusion")
          patient_files <- if (id_val %in% names(fresh_fl)) fresh_fl[[id_val]] else list()

          # Add tumor RNA BAM track if available
          if ("tumor" %in% names(patient_files) && length(patient_files$tumor) > 0) {
            tumor_bam <- patient_files$tumor[grepl("\\.bam$", patient_files$tumor)][1]
            if (!is.na(tumor_bam)) {
              tracks <- c(tracks, list(list(
                name = paste0(id_val, " RNA"),
                file = tumor_bam
              )))
            }
          }
          
          # Add chimeric BAM track if available
          if ("chimeric" %in% names(patient_files) && length(patient_files$chimeric) > 0) {
            chim_bam <- patient_files$chimeric[grepl("\\.bam$", patient_files$chimeric)][1]
            if (!is.na(chim_bam)) {
              tracks <- c(tracks, list(list(
                name = paste0(id_val, " Chimeric"),
                file = chim_bam
              )))
            }
          }
          
          tracks
        })
        
        bam_list <- do.call(c, track_lists)   # žádné unlist(..., recursive=FALSE)
        
        # Logy, které fakt něco ukážou
        if (length(bam_list)) {
          files <- vapply(bam_list, function(x) x$file, character(1L))
          message("✔ Assigned fusion_bam (", length(bam_list), " tracks): ", paste(files, collapse = ", "))
        } else {
          message("✖ No tracks assembled for selected_patients: ", paste(selected_patients, collapse = ", "))
        }
        
        shared_data$fusion.bam(bam_list)
        shared_data$fusion.patients.igv(selected_patients)
        updateNavbarTabs(session = session$userData$parent_session, inputId = "navbarMenu", selected = session$userData$parent_session$ns("hidden_igv"))
      }
    })

    ###########################
    ## Download handler ######
    ###########################
    output$Table_download <- get_table_download_handler(
      input = input,
      patient = selected_samples,
      data = data,
      filtered_data = reactive(fusion_data_to_render()),
      suffix = ""
    )
    
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
    
    methods <- list(
      get_session_data     = session_handlers$get_session_data,
      restore_session_data = session_handlers$restore_session_data,
      filter_state         = filter_state,
      is_restoring         = is_restoring_session
    )
    
    register_module(shared_data, "fusion", selected_samples, methods)
    
    return(methods)
  })
}



filterTab_server <- function(id,colnames_list,data,mapped_checkbox_names, is_restoring = NULL) {
  moduleServer(id, function(input, output, session) {

    initialized <- reactiveVal(FALSE)
    
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
    
    # Funkce pro update column choices
    update_column_choices <- function() {
      req(mapped_checkbox_names())
      
      # Seřaď choices podle názvů
      checkbox_names <- mapped_checkbox_names()
      col_choices_ordered <- checkbox_names[order(names(checkbox_names))]
      
      # Normalizuj current selection
      current_selection <- isolate(input$colFilter_checkBox)
      default_selection <- if (!initialized() || is.null(current_selection) || length(current_selection) == 0) {
        colnames_list()$default_columns
      } else {
        current_selection
      }
      
      selected_values <- normalize_column_selection(
        selection = default_selection,
        choices_map = col_choices_ordered,
        default_cols = colnames_list()$default_columns
      )

      updatePrettyCheckboxGroup(
        session, "colFilter_checkBox", 
        choices = col_choices_ordered, 
        selected = selected_values,
        prettyOptions = list(status = "primary", icon = icon("check"), outline = FALSE)
      )
    }
    
    # ===== MAIN OBSERVE =====
    
    observe({
      # Pokud probíhá restore session, přeskoč automatické aktualizace
      if (!is.null(is_restoring) && isTruthy(is_restoring())) {
        return()
      }
      
      update_column_choices()
      
      if (!initialized()) {
        initialized(TRUE)
      }
    })

    # ===== EVENT HANDLERS =====
    
    observeEvent(input$show_all, {
      req(mapped_checkbox_names())
      all_values <- ch(unname(mapped_checkbox_names()))
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = all_values)
    })

    
    observeEvent(input$show_default, {
      req(mapped_checkbox_names())
      default_values <- normalize_column_selection(
        selection = colnames_list()$default_columns,
        choices_map = mapped_checkbox_names(),
        default_cols = colnames_list()$default_columns
      )

      updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = default_values)
    })

    # ===== RESTORE FUNCTION =====
    
    restore_ui_inputs <- function(state) {
      message("🎯 Restoring filter UI inputs")
      
      if (!is.null(state$selected_cols)) {
        wanted <- ch(safe_extract(state$selected_cols))
        message(sprintf("Restoring selected_cols: %s", paste(wanted, collapse = ", ")))
        
        wanted_values <- normalize_column_selection(
          selection = wanted,
          choices_map = isolate(mapped_checkbox_names()),
          default_cols = colnames_list()$default_columns
        )
        
        message(sprintf("Final selected values: %s", paste(wanted_values, collapse = ", ")))
        updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = wanted_values)
      }
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
    # tags$head(tags$style(HTML(".download-dropdown-wrapper .dropdown-toggle {border-radius: 0; padding: 0; background-color: transparent; border: none; float: right; margin-top: -1px;}
    #                            .download-dropdown-wrapper .dropdown-toggle::after {display: none !important;}
    #                            .download-dropdown-wrapper .glyphicon-triangle-bottom {display: none !important; width: 0 !important; margin: 0 !important; padding: 0 !important;}
    #                           "))
    # ),
    # div(class = "download-dropdown-wrapper", 
      dropdownButton(
        label = NULL,
        right = TRUE,
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
  # )
}
