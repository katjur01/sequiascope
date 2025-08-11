box::use(
  shiny[br, NS,h3, tagList, div, observe, observeEvent, mainPanel, titlePanel, uiOutput, renderUI, HTML, fluidPage,fluidRow, moduleServer,
        reactiveValues, column, req, reactive, reactiveVal,showModal,modalDialog,modalButton],
  htmltools[tags],
  bs4Dash[actionButton,box],
  shinyjs[useShinyjs, runjs],
  reactable,
  reactable[reactableOutput,renderReactable,reactable,JS],
  # processx[process]
)
box::use(
  app/logic/igv_helper[build_igv_tracks,start_static_server,stop_static_server]
)

#' @export
igv_ui <- function(id) {
  ns <- NS(id)
  
  # titlePanel("IGV Viewer")
  tagList(
    tags$style(HTML("
       #igv_page {
          width: 100%;
          height: 600px;
          margin: 0 auto;
          padding: 20px;
          box-sizing: border-box;
      }
      #igv-igvDiv {
          width: 100%;
          height: auto;
          border: none; 
          margin: 0 auto;
          padding: 20px;
          box-sizing: border-box;
      }
  ")),
    useShinyjs(),
    tags$head(
      # tags$script(src = "https://cdn.jsdelivr.net/npm/igv@2.15.12/dist/igv.min.js")
      tags$script(src = "https://cdn.jsdelivr.net/npm/igv@3.3.0/dist/igv.min.js")
    ),
    fluidRow(
      column(6, reactableOutput(ns("bookmarks")))),
    br(),
    fluidRow(
      column(2, actionButton(ns("loadIGVButton"), "Load IGV Viewer"))
    ),
    
    br(),
    uiOutput(ns("igvDivOutput"))
    
  )
}

#' @export
igv_server <- function(id,shared_data) {
  moduleServer(id, function(input, output, session) {
    
    # values <- reactiveValues()
    # 
    # # Seznam vzorků
    # samples <- list(
    #   list(name = "DZ1601", file = "DZ1601fuze.bam"),
    #   list(name = "MR1507", file = "MR1507fuze.bam")
    # )
    # 
    # values$bookmark_df <- data.frame(
    #   gene1 = c("KANSL1", "KMT2A", "METTL13"),
    #   gene2 = c("LRRC37A4P", "MLLT3", "DNM3"),
    #   position1 = c("17:45597764", "11:118482495", "1:171814013"),
    #   position2 = c("17:45545676", "11:20365744", "1:171987759")
    # )
    #################################################
    ### Selected variant or fusion data + buttons ###
    #################################################
    # observe({
    #   # Výchozí hodnota pro selected_dt
    #   selected_dt(NULL)
    #   
    #   if (is.null(selected_variants) || nrow(selected_variants) == 0) {
    #     message("No variants selected.")
    #     # result_dt(tissue_table)
    #   } else {
    #     var_tab <- selected_variants[, .(Gene_symbol, variant = var_name)]
    #     var_tab <- unique(var_tab, by = "Gene_symbol")
    #   }
    # 
    # })

    selected_variants <- reactive({
      req(shared_data$navigation_context())
      from <- shared_data$navigation_context()
      if(!is.null(from)){
        message("🔍 Opened IGV from tab: ", from)
        if (from == "somatic") {
          return(data.frame(
            gene1 = shared_data$somatic_var()$Gene_symbol,
            position1 = gsub("_(\\d+)_.*$", ":\\1", shared_data$somatic_var()$var_name),
            sample = shared_data$somatic_var()$sample
          ))
        } else if (from == "germline") {
          return(data.frame(
            gene1 = shared_data$germline_var()$Gene_symbol,
            position1 = gsub("_(\\d+)_.*$", ":\\1", shared_data$germline_var()$var_name),
            sample = shared_data$germline_var()$sample
          ))
        } else if (from == "fusion") {
          return(data.frame(
            gene1 = shared_data$fusion_var()$gene1,
            gene2 = shared_data$fusion_var()$gene2,
            position1 = gsub("_(\\d+)_.*$", ":\\1", shared_data$fusion_var()$position1),
            position2 = gsub("_(\\d+)_.*$", ":\\1", shared_data$fusion_var()$position2),
            sample = shared_data$fusion_var()$sample
          ))
        }
      }
    })
    
    selected_bams <- reactive({
      req(shared_data$navigation_context())
      from <- shared_data$navigation_context()
      if(!is.null(from)){
        if (from == "somatic") {
          return(shared_data$somatic_bam())
        } else if (from == "germline") {
          return(shared_data$germline_bam())
        } else if (from == "fusion") {
          return(shared_data$fusion_bam())
        }
      }
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
    
    observeEvent(input$loadIGVButton, {
      message("Current selected_bams files: ", paste(selected_bams(), collapse = ", "))
      message("Current selected_variants files: ", paste(selected_variants(), collapse = ", "))
      selected_empty <- is.null(selected_variants()) || 
        (is.data.frame(selected_variants()) && nrow(selected_variants()) == 0)
      bam_empty <- is.null(selected_bams()) || length(selected_bams()) == 0
      
      if (selected_empty || bam_empty) {
        showModal(modalDialog(
          title = "Missing input",
          paste0("You have not selected variants or patients for visualization. Please return to the ",shared_data$navigation_context()," tab and define them."),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      } else {
        output$igvDivOutput <- renderUI({
          div(id = session$ns("igv-igvDiv"))
        })
      message("###### build_igv_tracks selected_bams ######: ",selected_bams())
      track_block <- build_igv_tracks(selected_bams())
      
      message("##### track_block ##### : ",track_block)
      # Krok 2: Po vykreslení spustíme JavaScript pro IGV s mírným zpožděním
      runjs(sprintf("
        setTimeout(function() {
          var igvDiv = document.getElementById('%s');
          if (igvDiv) {
            var options = {
              genome: 'hg38',
              locus: 'all',
              tracks: [%s],
              showNavigation: true,       // horní navigační lišta
              showRuler: true,            // číslování pozice nahoře
              showSampleName: true,       // pokud máš sampleName v tracku
              showCenterGuide: true,      // vertikální linka ve středu
              trackHeight: 300,           // výchozí výška jednoho BAM tracku
              minTrackHeight: 50,
              maxTrackHeight: 1000,
              search: {
                url: 'https://www.ncbi.nlm.nih.gov/gene/?term=$$'
              }
            };
            igv.createBrowser(igvDiv, options).then(function(browser) {
              console.log('IGV browser created');
              window.igvBrowser = browser;
            });
          } else {
            console.error('IGV div not found.');
          }
        }, 20);  // Zpoždění 20 ms k zajištění, že div je vykreslen
      ", session$ns("igv-igvDiv"), track_block))  # Přidáme správné ID divu s namespace
      }
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
