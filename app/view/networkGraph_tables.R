# app/view/networkGraph_tables.R

box::use(
  shiny[NS, moduleServer, observeEvent, observe, tagList, fluidPage, fluidRow, column,req],
  htmltools[tags, div,HTML],
  reactable[reactable,colDef,renderReactable,reactableOutput,JS],
  shinyjs[addClass,removeClass],
  utils[head]
)

selectedTab_UI <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML("
      .selectedTab-hidden {
        display: none !important; /* Skryjeme tabulku úplně, pokud není třeba */
      }

      .selectedTab-visible {
        display: block !important;
        resize: vertical;
        overflow: auto;
        border: none;
        height: 155px;
      }
      .row-highlighted {
          background-color: #FFFF99 !important;
      }
    "))),
    
    div(
      id = ns("selectedTab_container"),  # ID pro použití v JS
      class = "selectedTab-hidden",  # Výchozí stav: skrytý
      reactableOutput(ns("selected_tab"))
    )
  )
}


tab_UI <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML(".row-highlighted { background-color: #FFFF99 !important; }"))),
    fluidRow(
      column(6,reactableOutput(ns("network_tab"))),
      column(1,),
      column(5,reactableOutput(ns("subNetwork_tab")))
    ))
}
tab_server <- function(id, tissue_dt, subTissue_dt, selected_nodes,selected_dt,patient) {
  moduleServer(id, function(input, output, session) {
    
    
    
    ##### reactable calling #####
    output$network_tab <- renderReactable({
      message("Rendering Reactable for network")
      reactable(as.data.frame(tissue_dt()),
                columns = list(
                  feature_name = colDef(name = "Gene name", maxWidth = 100, filterable = TRUE),
                  geneid = colDef(name = "Ensembl id", width = 140, show = F),
                  refseq_id = colDef(name = "Refseq id", maxWidth = 80, show = F),
                  fc = colDef(show = F),
                  log2FC = colDef(name = "log2FC", maxWidth = 100),
                  p_adj = colDef(show = F),
                  pathway = colDef(name = "Pathway name", minWidth = 140, resizable = TRUE),
                  tissue = colDef(show = F),
                  sample = colDef(show = F)
                ),
                defaultPageSize = 10,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(10, 20, 50, 100),
                onClick = JS(sprintf("function(rowInfo, column) {
                  console.log('Clicked row gene name: ' + rowInfo.row.feature_name);
                  Shiny.setInputValue('%s', { gene: rowInfo.row.feature_name }, { priority: 'event' });
                  backgroundColor: '#ADD8E6';
                }", session$ns("selected_row"))),
                striped = TRUE,
                wrap = FALSE,
                highlight = TRUE,
                outlined = TRUE)
    })
    
    observeEvent(input$selected_row, {
      selectedRow <- input$selected_row
      message("Vybraný gen z tabulky: ", selectedRow)
      
      session$sendCustomMessage("cy-add-node-selection", list(gene = selectedRow, patientId = patient))
      session$sendCustomMessage("highlight-row", list(gene = selectedRow))
    })
    
    
    observe({
      req(selected_nodes())
      req(subTissue_dt())
      
      if (length(selected_nodes()) > 0) {
        output$subNetwork_tab <- renderReactable({
          message("Rendering Reactable for subNetwork")
          reactable(unique(subTissue_dt()[feature_name %in% selected_nodes()]),
                    columns = list(
                      feature_name = colDef(name = "Gene name", maxWidth = 120),
                      counts_tpm_round = colDef(name = "Counts TPM", maxWidth = 120),
                      log2FC = colDef(name = "log2FC", maxWidth = 100),
                      p_value = colDef(name = "P-value", maxWidth = 100),
                      pathway = colDef(name = "Pathway name", minWidth = 200, resizable = TRUE),
                      # Skrýt všechny ostatní sloupce
                      fc = colDef(show = FALSE),
                      geneid = colDef(show = FALSE),
                      refseq_id = colDef(show = FALSE),
                      p_adj = colDef(show = FALSE),
                      tissue = colDef(show = FALSE),
                      sample = colDef(show = FALSE),
                      size = colDef(show = FALSE),
                      mu = colDef(show = FALSE),
                      lower_than_p = colDef(show = FALSE),
                      higher_than_p = colDef(show = FALSE),
                      type = colDef(show = FALSE),
                      all_kegg_gene_names = colDef(show = FALSE),
                      gene_definition = colDef(show = FALSE),
                      num_of_paths = colDef(show = FALSE)
                    ),
                    defaultPageSize = 10,
                    showPageSizeOptions = TRUE,
                    pageSizeOptions = c(10, 20, 50, 100),
                    striped = TRUE,
                    wrap = FALSE,
                    highlight = TRUE,
                    outlined = TRUE)
        })
      } else {
        message("No nodes selected, no subnetwork table needed.")
        output$subNetwork_tab <- renderReactable({
          message("Rendering empty Reactable for subNetwork")
          reactable(data.frame())
        })
      }
    })
    
    # selected table is not visible till we select variant or fusion
    observe({
      data <- selected_dt()
      has_data <- !is.null(data) && nrow(data) > 0
      if (has_data) {
        addClass(selector = paste0("#", session$ns("selectedTab_container")), class = "selectedTab-visible")
        removeClass(selector = paste0("#", session$ns("selectedTab_container")), class = "selectedTab-hidden")
      } else {
        addClass(selector = paste0("#", session$ns("selectedTab_container")), class = "selectedTab-hidden")
        removeClass(selector = paste0("#", session$ns("selectedTab_container")), class = "selectedTab-visible")
      }
    })
    
    output$selected_tab <- renderReactable({
      data <- selected_dt()
      
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      required_columns <- c("Gene_symbol", "var_name", "fusion", "pathway")
      missing_columns <- setdiff(required_columns, colnames(data))
      # dont show empty columns
      for (col in missing_columns) {
        data[[col]] <- ""
      }
      # Vyber jen relevantní sloupce
      data <- data[, c("Gene_symbol", "var_name", "fusion", "pathway")]
      
      reactable(
        data,
        columns = list(
          Gene_symbol = colDef(name = "Gene name", minWidth = 120, maxWidth = 140),
          var_name = colDef(name = "Variant", width = 100, show = any(data$var_name != "")),  
          fusion = colDef(name = "Fusion", width = 100, show = any(data$fusion != "")),  
          pathway = colDef(name = "Pathway",minWidth = 180)
        ),
        resizable = TRUE,
        pagination = FALSE,  
        defaultPageSize = 3,  
        bordered = TRUE,
        highlight = TRUE,
        striped = TRUE,
        wrap = FALSE,
        style = list(maxHeight = "400px", overflowY = "auto")  
      )
    })
    
  })
}


