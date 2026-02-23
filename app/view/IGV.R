box::use(
  shiny[br, NS,h3, tagList, div, observe, observeEvent, mainPanel, titlePanel, uiOutput, renderUI, fluidPage,fluidRow, moduleServer,
        reactiveValues, column, req, reactive, reactiveVal,showModal,modalDialog,modalButton, conditionalPanel, textInput, updateTextInput],
  htmltools[tags,HTML],
  shinyalert[shinyalert],
  bs4Dash[actionButton,box],
  shinyjs[useShinyjs, runjs, hide],
  shinyWidgets[pickerInput, pickerOptions],
  reactable,
  reactable[reactableOutput,renderReactable,reactable,JS],
  # processx[process]
)
box::use(
  app/logic/helper_igv[build_igv_tracks, get_igv_genome_id, build_custom_genome]#,start_static_server,stop_static_server]
)

#' @export
igv_ui <- function(id) {
  ns <- NS(id)
  
  # titlePanel("IGV Viewer")
  tagList(
    tags$style(HTML("#igv_page { width: 100%; height: 600px; margin: 0 auto; padding: 20px; box-sizing: border-box;}
                     #igv-igvDiv { width: 100%; height: auto;border: none; margin: 0 auto; padding: 20px; box-sizing: border-box;}")),
    useShinyjs(),
    tags$head(
      # tags$script(src = "https://cdn.jsdelivr.net/npm/igv@2.15.12/dist/igv.min.js")
      tags$script(src = "https://cdn.jsdelivr.net/npm/igv@3.3.0/dist/igv.min.js")),
    
    fluidRow(
      column(2, 
        div(
          textInput(ns("igv_genome_flag"), label = NULL, value = ""),
          conditionalPanel(
            condition = paste0("input['", ns("igv_genome_flag"), "'] == 'no_snapshot'"),
            pickerInput(inputId = ns("igv_browser_genome"),label = "Select reference genome:",
              choices = c("GRCh38/hg38", "hg38 1kg/GATK", "GRCh37/hg19", "T2T CHM13-v2.0/hs1"), selected = "GRCh38/hg38",
              options = pickerOptions(container = "body",iconBase = "fas"),width = "100%")))),
      column(2),
      column(2, 
        div(style = "display: flex; flex-direction: row-reverse; align-items: center; margin-top: 20px;",
          actionButton(ns("loadIGVButton"), "Load IGV Viewer",status = "info"))),
    ),
    br(),
    fluidRow(
      column(6, reactableOutput(ns("bookmarks")))),
    br(),
    uiOutput(ns("igvDivOutput"))
  )
}

#' @export
igv_server <- function(id, shared_data, root_path) {
  moduleServer(id, function(input, output, session) {
    
    igv_needs_refresh <- reactiveVal(FALSE)
    
    # Synchronize hidden input with shared_data for conditionalPanel
    observe({
      genome_val <- shared_data$igv_genome()
      # Always update, handle empty/NULL values with default
      if (!is.null(genome_val) && length(genome_val) > 0 && genome_val != "") {
        updateTextInput(session, "igv_genome_flag", value = genome_val)
        message("[IGV] Updated genome flag to: ", genome_val)
      } else {
        updateTextInput(session, "igv_genome_flag", value = "hg38")
        message("[IGV] No genome value, using default: hg38")
      }
    })
    
    # Hide the flag input (used only for conditionalPanel condition)
    observe({
      hide("igv_genome_flag")
    })
    
    selected_variants <- reactive({
      req(shared_data$navigation_context())
      from <- shared_data$navigation_context()
      if(!is.null(from)){
        message("🔍 Opened IGV from tab: ", from)
        if (from == "somatic") {
          return(data.frame(
            gene1 = shared_data$somatic.variants()$gene_symbol,
            position1 = gsub("_(\\d+)_.*$", ":\\1", shared_data$somatic.variants()$var_name),
            sample = shared_data$somatic.variants()$sample
          ))
        } else if (from == "germline") {
          return(data.frame(
            gene1 = shared_data$germline.variants()$gene_symbol,
            position1 = gsub("_(\\d+)_.*$", ":\\1", shared_data$germline.variants()$var_name),
            sample = shared_data$germline.variants()$sample
          ))
        } else if (from == "fusion") {
          return(data.frame(
            gene1 = shared_data$fusion.variants()$gene1,
            gene2 = shared_data$fusion.variants()$gene2,
            position1 = gsub("_(\\d+)_.*$", ":\\1", shared_data$fusion.variants()$position1),
            position2 = gsub("_(\\d+)_.*$", ":\\1", shared_data$fusion.variants()$position2),
            sample = shared_data$fusion.variants()$sample
          ))
        }
      }
    })
    
    selected_bams <- reactive({
      req(shared_data$navigation_context())
      from <- shared_data$navigation_context()
  
      if (is.null(from)) return(NULL)

      bam_list <- switch(from,
                         somatic = shared_data$somatic.bam(),
                         germline = shared_data$germline.bam(),
                         fusion = shared_data$fusion.bam(),
                         NULL)
      message("bam_list from IGV: ", bam_list)
      if (is.null(bam_list) || !length(bam_list)) return(NULL)
      
      # Path transformation happens in build_igv_tracks()
      return(bam_list)
    })
    
    selected_patients <- reactive({
      req(shared_data$navigation_context())
      from <- shared_data$navigation_context()
      
      if (is.null(from)) return(NULL)
      
      pat_list <- switch(from,
                         somatic = shared_data$somatic.patients.igv(),
                         germline = shared_data$germline.patients.igv(),
                         fusion = shared_data$fusion.patients.igv(),
                         NULL)
      
      if (is.null(pat_list) || !length(pat_list)) return(NULL)

      return(pat_list)
    })
    
    
    output$bookmarks <- renderReactable({
      req(selected_variants())
      reactable(selected_variants(),
                pagination = FALSE,
                striped = TRUE,
                wrap = FALSE,
                highlight = TRUE,
                outlined = TRUE,
                onClick = JS(sprintf("function(rowInfo, column) {
                  Shiny.setInputValue('%s', { index: rowInfo.index + 1 }, { priority: 'event' });
                }", session$ns("bookmarks_click")))
      )
    })
    
    observeEvent(selected_bams(), {
      # Pokud už bylo IGV načteno alespoň jednou
      if (!is.null(input$loadIGVButton) && input$loadIGVButton > 0) {
        igv_needs_refresh(TRUE)
        
        shinyalert(
          title = "Patient selection changed",
          text = "Do you want to reload IGV with the new patient data?",
          type = "info",
          html = TRUE,
          showCancelButton = TRUE,
          confirmButtonText = "Reload IGV",
          cancelButtonText = "Later",
          callbackR = function(value) {
            if (value) {
              shinyjs::click("loadIGVButton")
            }
          }
        )
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    observeEvent(input$loadIGVButton, {
      
      igv_needs_refresh(FALSE)
      
      message("Current selected_bams files: ", paste(selected_bams(), collapse = ", "))
      message("Current selected_variants files: ", paste(selected_variants(), collapse = ", "))
      
      selected_empty <- is.null(selected_variants()) || (is.data.frame(selected_variants()) && nrow(selected_variants()) == 0)
      bam_empty <- is.null(selected_bams()) || length(selected_bams()) == 0

      if (selected_empty) {
        shinyalert(
          title = "No variants selected",
          text = paste0("You have not selected any variants for visualization.<br><br>",
                        "Please return to the <b>", shared_data$navigation_context(), "</b> tab and select variants."),
          type = "warning",
          html = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK")
        return()
      }
      
      # Alert 2: Žádné BAM soubory (ani pro jednoho pacienta)
      if (bam_empty) {
        shinyalert(
          title = "No BAM files available",
          text = paste0("None of the selected patients have BAM files available for IGV visualization.<br><br>",
                        "Please ensure BAM files are uploaded in the <b>Upload data</b> tab, ",
                        "or select different patients with available data."),
          type = "warning",
          html = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK")
        return()
      }
    
      # # NOVÁ KONTROLA: Zkontroluj, jestli všichni vybraní pacienti mají BAM soubory
      selected_pats <- unique(selected_patients())
      
      sb <- selected_bams()
      
      # vytáhni vektor cest (funguje pro 1 i více tracků)
      paths <- character(0)
      if (!is.null(sb)) {
        if (is.list(sb) && "file" %in% names(sb) && is.character(sb$file)) {
          # případ: jen jeden track: list(name=..., file=...)
          paths <- sb$file
        } else if (is.list(sb)) {
          # případ: víc tracků: list(list(name=..., file=...), list(...), ...)
          paths <- unique(unlist(lapply(sb, function(x) if (is.list(x) && !is.null(x$file)) x$file), use.names = FALSE))
        }
      }
      
      # pro každého pacienta: je jeho ID někde v cestách?
      has_file <- vapply(
        selected_pats,
        function(id) any(grepl(id, paths, fixed = TRUE)),
        logical(1)
      )
      
      missing_patients <- selected_pats[!has_file]
      
      # volitelně: log
      message("missing_patients: ", if (length(missing_patients)) paste(missing_patients, collapse = ", ") else "<none>")
      
      if (length(missing_patients) > 0) {
        shinyalert(
          title = "Missing BAM files",
          text = paste0("The following patients have no BAM files available for visualization:<br><br>",
                        "<b>", paste(missing_patients, collapse = ", "), "</b><br><br>",
                        "IGV will load data only for patients with available BAM files."),
          type = "warning",
          html = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK")
      }
      
      # Pokračuj s vykreslením IGV
      output$igvDivOutput <- renderUI({ div(id = session$ns("igv-igvDiv")) })
      
      message("###### build_igv_tracks selected_bams ######: ", selected_bams())
      track_block <- build_igv_tracks(selected_bams(), igv_root = shared_data$igv_root())
      
      message("##### track_block ##### : ", track_block)
      
      # Get genome selection from shared_data (set in upload_data step1)
      # or from the picker if snapshots were skipped
      genome_val <- shared_data$igv_genome()

      igv_genome <- if (!is.null(genome_val) && length(genome_val) > 0 && genome_val != "no_snapshot") {
        genome_val  # Use genome selected in upload step1
      } else if (!is.null(input$igv_browser_genome)) {
        # Use genome selected in IGV module picker (convert display name → igv_id)
        get_igv_genome_id(input$igv_browser_genome, shared_data$custom_genome_config)
      } else {
        "hg38"  # Default
      }
      message("[IGV] Using genome: ", igv_genome, " (from ", if (!is.null(genome_val) && length(genome_val) > 0 && genome_val != "no_snapshot") "upload_step1" else "IGV_picker", ")")
      
      # Check if custom genome is being used
      custom_genome_config <- shared_data$custom_genome_config
      is_custom_genome <- !is.null(custom_genome_config) && 
                          !is.null(custom_genome_config$igv_id) && 
                          igv_genome == custom_genome_config$igv_id
      
      # Build genome configuration for IGV.js
      if (is_custom_genome) {
        # Use helper function to build custom genome
        genome_js <- build_custom_genome(custom_genome_config)
        # For custom genomes, use specific locus instead of 'all' to avoid FAI parsing issues
        # GRCh38 uses "1" not "chr1" naming
        default_locus <- "1:1-100000"
      } else {
        # Built-in genome - just use ID string
        genome_js <- sprintf("'%s'", igv_genome)
        default_locus <- "all"
      }
      
      runjs(sprintf("
    setTimeout(function() {
      if (typeof igv === 'undefined') {
        console.error('IGV library not loaded');
        alert('IGV library is not loaded. Please refresh the page and try again.');
        return;
      }
      
      var igvDiv = document.getElementById('%s');
      if (igvDiv) {
        var options = {
          genome: %s,
          locus: '%s',
          tracks: [%s],
          showNavigation: true,
          showRuler: true,
          showSampleName: true,
          showCenterGuide: true,
          trackHeight: 300,
          minTrackHeight: 50,
          maxTrackHeight: 1000,
          search: {
            url: 'https://www.ncbi.nlm.nih.gov/gene/?term=$$'
          }
        };
        igv.createBrowser(igvDiv, options).then(function(browser) {
          console.log('IGV browser created with genome: %s');
          window.igvBrowser = browser;
        }).catch(function(error) {
          console.error('Error creating IGV browser:', error);
        });
      } else {
        console.error('IGV div not found.');
      }
    }, 500);
    ", session$ns("igv-igvDiv"), genome_js, default_locus, track_block, igv_genome))
      # }
    })

    
    observeEvent(input$bookmarks_click, {
      selected <- input$bookmarks_click
      message("Clicked row info: ", selected$index)
      if (!is.null(selected)) {
        from <- shared_data$navigation_context()
        message("🔍 Opened IGV from tab: ", from)
        
        if (from == "somatic") {
          position <- selected_variants()$position1[selected$index]
        } else if (from == "germline") {
          position <- selected_variants()$position1[selected$index]
        } else if (from == "fusion") {
          position1 <- selected_variants()$position1[selected$index]
          position2 <- selected_variants()$position2[selected$index]
          position <- paste0(position1, " ", position2)
        } else {
          position <- NULL
        }
        
        message("Navigating to positions: ", position)
        
        runjs(sprintf("
          console.log('Navigating to positions: %s');
          if (window.igvBrowser) {
            console.log('IGV browser exists');
            window.igvBrowser.search('%s').then(function() {
              console.log('Navigation complete');
            }).catch(function(error) {
              console.error('Navigation error:', error);
            });
          } else {
            console.log('IGV browser does not exist');
          }", position, position))
      }
    })
  })
}
# 
# 
# 
# ui <- fluidPage(
#   box(id = "igv_page", title = "IGV Viewer", width = 10, collapsible = FALSE,
#     igv_ui("igv")
#   )
# )
# 
# server <- function(input, output, session) {
#   
#   # Spustíme statický server při startu celé aplikace
#   start_static_server(dir = "/Users/katerinajuraskova/Desktop/sequiaViz/input_files/MOII_e117/primary_analysis/230426_MOII_e117_fuze/mapped")
#                         #"/home/katka/BioRoots/sequiaViz/input_files/MOII_e117/primary_analysis/230426_MOII_e117_tkane/mapped")
#   
#   igv_server("igv")
#   
#   # Ukončení serveru při zavření celé session
#   session$onSessionEnded(function() {
#     stop_static_server()
#   })
#   
# }
# 
# shinyApp(ui, server, options = list(launch.browser = TRUE))
# 
