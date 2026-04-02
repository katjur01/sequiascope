# shiny::shinyAppDir(".", options = list(launch.browser = TRUE))


box::use(
  shiny[NS, moduleServer, observeEvent, observe, tagList, fluidPage, fluidRow, column, textInput, updateTextInput, actionButton, selectInput, reactive, req,reactiveVal,conditionalPanel,
        verbatimTextOutput, renderPrint,renderText,textOutput,htmlOutput,uiOutput,renderUI,icon,textAreaInput,updateTextAreaInput,isolate,isTruthy,debounce,getDefaultReactiveDomain,
        outputOptions,insertUI],
  httr[GET, status_code, content],
  bs4Dash[updateTabItems,addPopover,updateNavbarTabs],
  htmltools[h3, h4, h6, tags, div,HTML,p],
  jsonlite[fromJSON, toJSON,read_json],
  # cyjShiny[cyjShinyOutput, renderCyjShiny, cyjShiny, dataFramesToJSON, selectNodes,setNodeAttributes,selectFirstNeighbors,fit,fitSelected,clearSelection,getSelectedNodes],
  data.table[fread,setnames,as.data.table,data.table,copy,rbindlist,setDF],
  stats[aggregate,rnorm],
  readxl[read_excel],
  # graph[nodes],
  reactable[reactable,colDef,renderReactable,reactableOutput,JS],
  shinyWidgets[radioGroupButtons,pickerInput,searchInput,updatePickerInput,prettySwitch,dropdown,updatePrettySwitch,actionBttn],
  shinyjs[useShinyjs],
  shinyalert[shinyalert,useShinyalert],
  memoise[memoise],
  tools[toTitleCase],
  magrittr[`%>%`],
  utils[head],
)

box::use(
  app/logic/load_data[load_data],
  app/logic/waiter[use_spinner, use_waiter, show_waiter, hide_waiter],
  app/logic/helper_networkGraph[get_string_interactions,prepare_cytoscape_network,get_pathway_list],
  app/view/networkGraph_tables,

)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

ui <- function(id, tissue_list, patient) {
  ns <- NS(id)
  useShinyjs()
  use_waiter()  # 🔑 Aktivovat waiter pro loading screens

  tagList(
    tags$br(),
    fluidRow(
      column(6,  # 🔑 Left half - main graph
        fluidRow(
          column(4,
                 div(style = "width: 90%;",
                   div(style = "display: flex; flex-direction: column; width: 100%; align-items: flex-start;", 
                     tags$label("Pathway:", style = "margin-bottom: 5px; align-self: flex-start;"),
                     pickerInput(inputId=ns("selected_pathway"), NULL, selected = "EGFR tyrosine kinase inhibitor resistance",choices = NULL, options = list(`live-search` = TRUE), width = "100%")),
                   div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; margin-top: 10px;", 
                     tags$label("Choose layout", style = "align-self: flex-start;"),
                     tags$div(id = ns("helpPopover_layout"), style = "position: relative; z-index: 1000; cursor: pointer; padding: 5px;",
                              `data-toggle` = "popover",`data-placement` = "right",`data-trigger` = "hover",`data-html` = "true",
                              `data-title` = "Layout options",
                              `data-content` = "<b>Cola</b>: Ideal for hierarchical structures and smaller graphs.<br><br><b>FCoSE</b>: Best for large and complex networks.",
                              tags$i(class = "fa fa-question fa-xs", style = "color: #2596be;"))),
                   selectInput(ns("selected_layout"), NULL, choices = c("cola", "fcose"), selected = "cola", width = "100%"),
                   div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; margin-top: 10px;", 
                       tags$label("Edge display mode", style = "align-self: flex-start;"),
                       tags$div(id = ns("helpPopover_edge_mode"), style = "position: relative; z-index: 1000; cursor: pointer; padding: 5px;",
                                `data-toggle` = "popover",`data-placement` = "right",`data-trigger` = "hover",`data-html` = "true",
                                `data-title` = "Edge display modes",
                                `data-content` = "<b>Evidence</b>: Shows multiple colored edges per interaction, one for each source type (experiments, databases, etc.). Each edge displays its individual score.<br><br><b>Confidence</b>: Shows a single dark gray edge per interaction with width representing the combined confidence score from all evidence types.",
                                tags$i(class = "fa fa-question fa-xs", style = "color: #2596be;"))),
                   radioGroupButtons(inputId=ns("edge_mode"), NULL,
                                        choices = c("Evidence" = "evidence", "Confidence" = "confidence"),
                                        selected = "confidence",  # 🔑 Confidence as default
                                        justified = TRUE))),
          column(4,
                 div(style = "width: 90%;",
                     div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;", 
                         tags$label("Interaction sources", style = "align-self: flex-start;"),
                         tags$div(id = ns("helpPopover_sources"), style = "position: relative; z-index: 1000; cursor: pointer; padding: 5px;",
                                 `data-toggle` = "popover",`data-placement` = "right",`data-trigger` = "hover",`data-html` = "true",
                                  `data-title` = "Interaction evidence sources",
                                  `data-content` = "Select which types of evidence to include from STRING database:<br>• <b>experiments</b> – Laboratory experiments<br>• <b>databases</b> – Curated databases<br>• <b>textmining</b> – Scientific literature<br>• <b>coexpression</b> – Gene expression patterns<br>• <b>neighborhood</b> – Genomic location<br>• <b>gene_fusion</b> – Fusion genes<br>• <b>cooccurrence</b> – Phylogenetic profiles",
                                  tags$i(class = "fa fa-question fa-xs", style = "color: #2596be;"))),
                     pickerInput(inputId=ns("interaction_sources"), NULL, 
                                     selected = c("experiments","databases","textmining","coexpression","neighborhood","gene_fusion","cooccurrence"),
                                     choices = c("experiments","databases","textmining","coexpression","neighborhood","gene_fusion","cooccurrence"), 
                                     multiple = TRUE,
                                     options = list(`live-search` = TRUE, `actions-box` = TRUE), width = "100%"),
                    div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; margin-top: 10px;", 
                        tags$label("Interaction score:", style = "align-self: flex-start;"),
                        tags$div(id = ns("helpPopover_score"), style = "position: relative; z-index: 1000; cursor: pointer; padding: 5px;",
                                 `data-toggle` = "popover",`data-placement` = "right",`data-trigger` = "hover",`data-html` = "true",
                                 `data-title` = "Interaction confidence threshold",
                                 `data-content` = "Minimum confidence score to display interactions (0-1 scale):<br>• <b>0.9</b> – Highest confidence (fewer edges)<br>• <b>0.7</b> – High confidence<br>• <b>0.4</b> – Medium confidence (recommended)<br>• <b>0.15</b> – Low confidence (more edges)<br><br>Higher thresholds = fewer but more reliable interactions.",
                                 tags$i(class = "fa fa-question fa-xs", style = "color: #2596be;"))),
                    selectInput(inputId=ns("interaction_score"), NULL, selected = "0.4",
                                    choices = c("highest confidence (0.9)" = "0.9", "high confidence (0.7)" = "0.7", "medium confidence (0.4)" = "0.4", "low confidence (0.15)" = "0.15"), 
                                    width = "100%")
                 )),
          column(4,
            div(style = "display: flex; flex-direction: column; gap: 5px;",
                prettySwitch(ns("selectedSomVariants"), label = "Add somatic variants", status = "primary", slim = TRUE),
                prettySwitch(ns("selectedGermVariants"), label = "Add germline variants", status = "primary", slim = TRUE),
                prettySwitch(ns("selectedFusions"), label = "Add fusions", status = "primary", slim = TRUE)
        ))),
        fluidRow(
          column(11, networkGraph_tables$selectedTab_UI(ns("tab")))
        ),
        tags$br()
      ),
      column(1),
      column(5,  # 🔑 Right half - subgraph + controls
             fluidRow(
               column(6,
                      div(style = "display: flex; flex-direction: column; align-items: flex-start; width: 90%;",
                        div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                          tags$label("Add new genes:"),
                          tags$div(id = ns("helpPopover_addGene"), style = "position: relative; z-index: 1000; cursor: pointer; padding: 5px;",
                                   `data-toggle` = "popover",`data-placement` = "right",`data-trigger` = "hover",`data-html` = "true",
                                   `data-title` = "Add genes",
                                   `data-content` = "Enter gene names separated by commas or on separate lines.<br><br>Example 1:<br><b>BRCA1, TP53, FOXO3</b><br><br>Example 2: <br><b>BRCA1<br>TP53<br>FOXO3</b>",
                                   tags$i(class = "fa fa-question fa-xs", style = "color: #2596be;")),
                          ),
                        textAreaInput(ns("new_genes"), NULL, placeholder = "e.g. BRCA1, TP53\nor\nBRCA1\nTP53", rows = 1, resize = "vertical", width = "100%"), 
                        actionButton(ns("confirm_new_genes_btn"), label = "Add Genes", icon = icon("check"), width = "100%", style = "margin-top: 10px;"))),
               column(6,
                      div(style = "display: flex; flex-direction: column; align-items: flex-start; width: 90%;",
                        tags$label("Remove genes:"),  
                        div(style = "display: flex; flex-direction: column; width: 100%;",
                          pickerInput(inputId=ns("remove_genes"), NULL, 
                                      choices = NULL, multiple = TRUE, 
                                      options = list(`live-search` = TRUE,`actions-box` = TRUE,`multiple-separator` = ", ",`none-selected-text` = "Select gene name",`width` = "100%",`virtual-scroll` = 10,`tick-icon` = "fa fa-times")), 
                          actionButton(ns("confirm_remove_genes_btn"), label = "Remove genes", icon = icon("trash-can"), width = "100%", style = "margin-top: 10px;"))))
           )
     )
   ),

    uiOutput(ns("js_namespace")),
    fluidRow(
      column(6,
          div(style = "display: flex; justify-content: center;",
            actionButton(ns("clearSelection_btn"), label = "Clear selection", icon = icon("eraser")),
            actionButton(ns("selectNeighbors_btn"), label = "Select first neighbors"),
            actionButton(ns("fitGraph_btn"), label = "Fit graph")))
    ),
       
    fluidRow(
      column(6,div(id = ns("cyContainer"), style = "width: 100%; height: 600px;")),
      column(1,),
      column(5,div(id = ns("cySubsetContainer"), style = "width: 100%; height: 600px;"))
    ),
   fluidRow(
     column(6,div(class = "networkGraph-tissue-wrapper",
        radioGroupButtons(inputId=ns("selected_tissue"),"Choose a tissue :",choices = tissue_list,justified = TRUE)))
   ),
   networkGraph_tables$tab_UI(ns("tab"))
  )
}

server <- function(id, patient, shared_data, patient_files, file_list, tabset_input_id = NULL, tab_value = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cy_container_id <- paste0(ns("cyContainer"))
    cy_subset_container_id <- paste0(ns("cySubsetContainer"))
    
    # Initialize pathway choices - only when tab is active
    observe({
      req(active())
      pathway_choices <- get_pathway_list("all_genes", kegg_tab_path = shared_data$kegg_tab_path())
      updatePickerInput(
        session = session,
        inputId = "selected_pathway",
        choices = pathway_choices,
        selected = "EGFR tyrosine kinase inhibitor resistance"
      )
    })
    
    active <- reactive({
      if (is.null(tabset_input_id) || is.null(tab_value)) {
        return(FALSE)
      }
      
      main_nav <- session$userData$parent_session$input$navbarMenu
      on_network_page <- !is.null(main_nav) && grepl("network_graph", main_nav)
      
      if (!on_network_page) {
        return(FALSE)
      }
      
      current_tab <- session$userData$parent_session$input[[tabset_input_id]]
      is_active <- !is.null(current_tab) && current_tab == tab_value
      
      return(is_active)
    })
    
    # Reactive values
    interactions <- reactiveVal(NULL)
    sub_interactions <- reactiveVal(NULL)
    synchronized_nodes <- reactiveVal(character(0))
    new_genes_var <- reactiveVal(NULL)
    remove_genes_var <- reactiveVal(NULL)
    clear_all <- reactiveVal(FALSE)
    last_button_action <- reactiveVal(0)  # 🔑 Timestamp of last button action
    selected_dt <- reactiveVal(NULL)
    
    # 🔑 DEBOUNCE for interaction sources (1 second delay)
    interaction_sources_debounced <- debounce(reactive(input$interaction_sources), 1000)
    
    # Load data
    dt <- reactive({
      req(active())
      message("Loading input data for network graph: ", patient_files$expression)
      load_data(patient_files, "expression", patient)
    })
    
    subTissue_dt <- reactive({
      req(dt())
      req(input$selected_tissue)
      unique(dt()[tissue == input$selected_tissue])
    })
    
    pathway_dt <- reactive({
      req(dt())
      req(input$selected_pathway)
      unique(dt()[grepl(input$selected_pathway, pathway, fixed = TRUE)])
    })
    
    tissue_dt <- reactive({
      req(pathway_dt())
      req(input$selected_tissue)
      unique(pathway_dt()[tissue == input$selected_tissue])
    })
    
    # Observer to hide waiter when JS rendering is complete
    observeEvent(input$cy_render_complete, {
      message("⏱️ JS rendering complete signal received")
      hide_waiter(cy_container_id)
    })
    
    interactions <- reactive({
      req(tissue_dt())
      req(input$interaction_score)  # 🔑 Require score
      req(interaction_sources_debounced())  # 🔑 CHANGE: Use debounced version
      
      genes <- tissue_dt()[, feature_name]
      
      start_time <- Sys.time()
      message("⏱️ [interactions] START: Fetching STRING interactions for ", nrow(tissue_dt()), " genes")
      message("   Score threshold: ", input$interaction_score)
      message("   Sources: ", paste(interaction_sources_debounced(), collapse = ", "))
      message("   Genes (comma-separated): ", paste(genes, collapse = ", "))
      
      result <- get_string_interactions(
        genes,
        required_score = as.numeric(input$interaction_score),
        filter_sources = interaction_sources_debounced()  # 🔑 CHANGE: Use debounced
      )
      
      end_time <- Sys.time()
      message("⏱️ [interactions] DONE: ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds")
      message("   Returned ", if(is.data.frame(result)) nrow(result) else "non-df", " interactions")
      
      result
    })
    
    network_json <- reactive({
      req(tissue_dt())
      req(interaction_sources_debounced())  # 🔑 CHANGE: debounced version
      req(input$edge_mode)  # 🔑 Require edge_mode (triggers re-execution)
      ints <- interactions()
      req(ints)
      
      if (is.null(ints) || (!is.data.frame(ints) && !is.list(ints))) {
        message("Waiting for valid interactions data...")
        return(NULL)
      }
      
      start_time <- Sys.time()
      message("⏱️ [network_json] START: Preparing network JSON for ", nrow(tissue_dt()), " genes")
      message("   Selected sources: ", paste(interaction_sources_debounced(), collapse = ", "))
      message("   Edge mode: ", input$edge_mode)
      
      # 🔍 Debug: Check column names in tissue_dt
      message("🔍 tissue_dt column names: ", paste(names(tissue_dt()), collapse = ", "))
      
      result <- prepare_cytoscape_network(
        ints, 
        unique(tissue_dt()[, .(feature_name, log2fc)]),
        selected_sources = interaction_sources_debounced(),  # 🔑 CHANGE: debounced
        required_score = as.numeric(input$interaction_score),  # 🔑 Pass score threshold
        edge_mode = input$edge_mode  # 🔑 Pass edge mode
      )
      
      end_time <- Sys.time()
      message("⏱️ [network_json] DONE: ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds")
      message("   Network has ", length(result$elements$nodes), " nodes and ", length(result$elements$edges), " edges")
      
      result
    })
    
    output$js_namespace <- renderUI({
      tags$script(HTML(sprintf("var ns = '%s';", ns(""))))
    })
    
    ##################################
    ## Network node synchronization ##
    ##################################
    
    sync_nodes <- function(nodes_from_graph, current_genes, add_genes = NULL, remove_genes = NULL, clear_all, graph_is_source = FALSE, nodes_outside_graph = character(0)) {
      # 🔑 KEY CHANGE:
      # - When graph_is_source=TRUE: Use ONLY nodes_from_graph (pure user selection from graph)
      # - When graph_is_source=FALSE and we have nodes_outside_graph:
      #     Use nodes_from_graph + keep nodes_outside_graph
      # - Otherwise: Combine everything (button action)
      
      if (clear_all) {
        combined <- character(0)
      } else if (graph_is_source && is.null(add_genes) && is.null(remove_genes)) {
        # User changed selection in graph and all synchronized nodes ARE in graph
        # → use ONLY what is selected in graph (allows deselection)
        combined <- nodes_from_graph
      } else if (!graph_is_source && length(nodes_outside_graph) > 0 && is.null(add_genes) && is.null(remove_genes)) {
        # User changed selection in graph but we have nodes OUTSIDE graph (e.g. BRCA1)
        # → use nodes_from_graph + keep nodes_outside_graph
        # IMPORTANT: This preserves nodes_outside_graph even when user deselects nodes IN graph
        combined <- unique(c(nodes_from_graph, nodes_outside_graph))
      } else {
        # Button action (add/remove) → combine everything and apply changes
        combined <- unique(c(nodes_from_graph, current_genes))
        
        if (!is.null(add_genes) && length(add_genes) > 0) combined <- unique(c(combined, add_genes))
        if (!is.null(remove_genes) && length(remove_genes) > 0) combined <- setdiff(combined, remove_genes)
      }
      
      combined <- combined[!is.na(combined) & combined != ""]
      changed <- !setequal(synchronized_nodes(), combined)
      
      return(list(updated_nodes = combined, changed = changed))
    }
    
    ########################
    ## Network UI buttons ##
    ########################
    
    observeEvent(list(active(), input$selected_pathway, input$selected_tissue, network_json()), {
      req(active(), input$selected_pathway, input$selected_tissue, network_json())
      
      # ⏱️ Start timing and show waiter
      pathway_load_start <- Sys.time()
      message("⏱️ ==========================================")
      message("⏱️ PATHWAY LOAD START: ", input$selected_pathway)
      message("⏱️ Timestamp: ", pathway_load_start)
      
      show_waiter(cy_container_id, paste0("Loading pathway: ", input$selected_pathway))
      
      tryCatch({
        network_data <- network_json()
        network_data$containerId <- cy_container_id
        network_data$patientId <- patient
        
        message("⏱️ Network data prepared, sending to JS...")
        js_send_start <- Sys.time()
        message("   Time so far: ", round(difftime(js_send_start, pathway_load_start, units = "secs"), 2), " seconds")
        
        message("Initializing cytoscape for patient: ", patient)
        session$sendCustomMessage("cy-init", network_data)
        
        message("⏱️ cy-init message sent to JS")
        message("   Time for cy-init send: ", round(difftime(Sys.time(), js_send_start, units = "secs"), 2), " seconds")
        
        # 🔑 IMPORTANT: After graph creation, send current selection
        # (so nodes get selected in the new graph)
        if (length(synchronized_nodes()) > 0) {
          message("   📤 Sending selection to new graph: ", paste(synchronized_nodes(), collapse = ", "))
          session$sendCustomMessage("update-selected-from-gene-list", list(
            selected_nodes = synchronized_nodes(),
            patientId = patient
          ))
        }
        
        # ⏱️ Hide waiter - JS will hide it when rendering is complete
        # We'll add a callback from JS to hide it properly
        message("⏱️ R-side processing complete")
        message("   Total R time: ", round(difftime(Sys.time(), pathway_load_start, units = "secs"), 2), " seconds")
        message("⏱️ ==========================================")
      }, error = function(e) {
        message("Error initializing cytoscape: ", e$message)
      })
    })
    
    observe({
      req(tissue_dt())  # 🔑 FIX: Use tissue_dt() instead of subTissue_dt()
      # tissue_dt() depends on PATHWAY and TISSUE, so it updates on pathway change
      req(length(synchronized_nodes()) > 0)
      req(input$interaction_score)  # 🔑 Require score
      req(input$interaction_sources)  # 🔑 Require sources
      
      start_time <- Sys.time()
      message("⏱️ [sub_interactions] START: Fetching sub-interactions for ", length(synchronized_nodes()), " nodes")
      message("   Pathway: ", input$selected_pathway, ", Tissue: ", input$selected_tissue)
      
      # Use tissue_dt() for correct pathway and same parameters as main graph
      result <- get_string_interactions(
        unique(tissue_dt()[feature_name %in% synchronized_nodes(), feature_name]),
        required_score = as.numeric(input$interaction_score),
        filter_sources = input$interaction_sources
      )
      
      end_time <- Sys.time()
      message("⏱️ [sub_interactions] DONE: ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds")
      
      sub_interactions(result)
    })

    observe({
      req(active())  # 🔑 CRITICAL: Only run when tab is active
      current_nodes <- synchronized_nodes()
      req(input$interaction_sources)  # 🔑 Trigger on source change
      
      message("🔄 cy-subset observer triggered - current_nodes: ", paste(current_nodes, collapse = ", "))
      message("   Pathway: ", input$selected_pathway, ", Tissue: ", input$selected_tissue)
      message("   Selected sources: ", paste(interaction_sources_debounced(), collapse = ", "))
      
      if (length(current_nodes) == 0) {
        message("No nodes selected. Sending empty subgraph.")
        
        session$sendCustomMessage("cy-subset", list(
          elements = list(nodes = list(), edges = list()),
          containerId = cy_subset_container_id,
          patientId = patient
        ))
        # NOTE: update-selected-from-gene-list is sent from sync observeEvent, not here
      } else {
        req(sub_interactions())
        req(subTissue_dt())  # 🔑 Use subTissue_dt for FC data (all genes in tissue)
        
        message("Aktualizace podgrafu pro uzly: ", paste(current_nodes, collapse = ", "))
        subnetwork_data <- prepare_cytoscape_network(
          sub_interactions(), 
          unique(subTissue_dt()[feature_name %in% current_nodes, .(feature_name, log2fc)]),  # 🔑 subTissue_dt for all genes
          current_nodes,
          selected_sources = interaction_sources_debounced(),  # 🔑 CHANGE: debounced
          required_score = as.numeric(input$interaction_score),  # 🔑 Pass score threshold
          edge_mode = input$edge_mode  # 🔑 Pass edge mode
        )
        message("#### Subgraph ready - nodes: ", length(subnetwork_data$elements$nodes), ", edges: ", length(subnetwork_data$elements$edges))
        
        subnetwork_data$containerId <- cy_subset_container_id
        subnetwork_data$patientId <- patient
        
        session$sendCustomMessage("cy-subset", subnetwork_data)
        # NOTE: update-selected-from-gene-list is sent from sync observeEvent, not here
      }
    })
    

    # Debug: track changes to input$cySelectedNodes
    observe({
      req(active())  # 🔑 Only debug when tab is active
      message("👀 input$cySelectedNodes changed: ", paste(input$cySelectedNodes %||% "NULL", collapse = ", "))
    })
    
    observeEvent(list(input$cySelectedNodes, input$confirm_new_genes_btn, 
                      input$confirm_remove_genes_btn, input$clearSelection_btn, 
                      new_genes_var(), remove_genes_var()), {
                        
                        req(active())  # 🔑 CRITICAL: Only sync when tab is active
                        
                        message("=== SYNC NODES START ===")
                        message("🔍 TRIGGER CHECK:")
                        message("   cySelectedNodes: ", paste(input$cySelectedNodes %||% "NULL", collapse = ", "))
                        message("   new_genes_btn: ", input$confirm_new_genes_btn %||% 0)
                        message("   remove_genes_btn: ", input$confirm_remove_genes_btn %||% 0)
                        message("   clear_btn: ", input$clearSelection_btn %||% 0)
                        message("   new_genes_var: ", paste(new_genes_var() %||% "NULL", collapse = ", "))
                        message("   remove_genes_var: ", paste(remove_genes_var() %||% "NULL", collapse = ", "))
                        
                        # 🔑 KEY FIX: When trigger comes from button action (new_genes/remove_genes),
                        # use synchronized_nodes() instead of input$cySelectedNodes, because JS has not yet updated
                        is_button_trigger <- !is.null(new_genes_var()) || !is.null(remove_genes_var()) || clear_all()
                        
                        # 🔑 NEW GUARD: Ignore input$cySelectedNodes if button was pressed within the last 500ms
                        time_since_button <- as.numeric(Sys.time()) - last_button_action()
                        recently_used_button <- time_since_button < 0.5
                        
                        # 🔑 CRITICAL GUARD: Ignore input$cySelectedNodes when button was used recently
                        # Time-based timeout (500ms) is the ONLY reliable way to detect stale values
                        sync_nodes_count <- length(synchronized_nodes())
                        cysel_nodes_count <- length(input$cySelectedNodes %||% character(0))
                        cysel_nodes <- input$cySelectedNodes %||% character(0)
                        sync_nodes <- synchronized_nodes()
                        
                        # has_extra_nodes check was REMOVED - could not reliably distinguish
                        # stale values from legitimate new graph selections
                        # We rely SOLELY on the recently_used_button timeout (500ms)
                        
                        if (is_button_trigger) {
                          last_button_action(as.numeric(Sys.time()))  # Record time of button action
                          message("🔘 Button trigger detected, using synchronized_nodes()")
                        } else if (recently_used_button) {
                          message("⏱️ Ignoring stale input$cySelectedNodes (button action within 500ms)")
                          return()  # Ignoruj tento trigger
                        }
                        # has_extra_nodes and has_nodes_not_in_graph checks REMOVED
                        # We rely SOLELY on the 500ms timeout
                        
                        # 🔑 NEW GUARD: Detect JS failure (nodes were not found in graph)
                        # ONLY after button actions where we expect JS to select all nodes
                        # 
                        # If cysel has FEWER nodes than sync, it could be:
                        # 1. User deselected a node (LEGITIMATE) - all cysel nodes are in sync
                        # 2. JS failed (e.g. BRCA1 not in graph) - synchronized has nodes not in cysel
                        #    BUT all synchronized nodes SHOULD have been in cysel (button added them)
                        if (!is_button_trigger && sync_nodes_count > 0 && cysel_nodes_count < sync_nodes_count) {
                          # Check: Are all cysel nodes in synchronized?
                          all_cysel_in_sync <- all(cysel_nodes %in% sync_nodes)
                          
                          if (all_cysel_in_sync) {
                            # ✅ LEGITIMATE deselection - all cysel nodes are in sync, just fewer of them
                            message("✓ User deselected nodes in graph")
                          } else {
                            # ❌ PROBLEM - cysel contains nodes that are NOT in sync
                            # This should not happen, ignore
                            message("⚠️ Ignoring input$cySelectedNodes - contains nodes not in synchronized")
                            return()
                          }
                        }
                        
                        nodes_from_graph <- if (is_button_trigger || recently_used_button) {
                          synchronized_nodes()  # Use current synchronized value
                        } else {
                          input$cySelectedNodes %||% character(0)  # Use value from graph
                        }
                        
                        message("nodes_from_graph: ", paste(nodes_from_graph, collapse = ", "))
                        message("current_genes: ", paste(synchronized_nodes(), collapse = ", "))
                        message("new_genes: ", paste(new_genes_var() %||% character(0), collapse = ", "))
                        message("remove_genes: ", paste(remove_genes_var() %||% character(0), collapse = ", "))
                        message("clear_all: ", clear_all())
                        
                        # 🔑 KEY LOGIC for graph_is_source:
                        # Graph is source (use ONLY nodes_from_graph) ONLY when:
                        # 1. NOT a button trigger (user is clicking in graph)
                        # 2. AND nodes_from_graph is a SUPERSET of synchronized_nodes
                        #    (all nodes from synchronized are in nodes_from_graph, so we can use just the graph)
                        # 
                        # If synchronized contains nodes NOT in nodes_from_graph,
                        # we distinguish two situations:
                        # - Node is NOT in the main graph at all (e.g. BRCA1) → keep
                        # - Node IS in the main graph but not selected → user deselected it → remove
                        
                        # Získej seznam VŠECH uzlů v hlavním grafu
                        all_graph_nodes <- tissue_dt()$feature_name
                        
                        # Uzly v synchronized které NEJSOU v nodes_from_graph
                        nodes_not_selected <- setdiff(synchronized_nodes(), nodes_from_graph)
                        
                        # Z těch vyfiltruj POUZE uzly které NEJSOU v hlavním grafu vůbec
                        nodes_outside_graph <- setdiff(nodes_not_selected, all_graph_nodes)
                        
                        sync_is_subset_of_nodes <- length(nodes_outside_graph) == 0
                        use_graph_as_source <- !is_button_trigger && sync_is_subset_of_nodes
                        
                        if (!use_graph_as_source && length(nodes_outside_graph) > 0 && !is_button_trigger) {
                            message("ℹ️ Preserving nodes not in graph: ", paste(nodes_outside_graph, collapse = ", "))
                        }
                        
                        result <- sync_nodes(
                          nodes_from_graph = nodes_from_graph,
                          current_genes = synchronized_nodes(),
                          add_genes = new_genes_var(),
                          remove_genes = remove_genes_var(),
                          clear_all = clear_all(),
                          graph_is_source = use_graph_as_source,
                          nodes_outside_graph = nodes_outside_graph
                        )
                        
                        updated_nodes <- result$updated_nodes
                        changed <- result$changed
                        
                        message("updated_nodes: ", paste(updated_nodes, collapse = ", "))
                        message("changed: ", changed)
                        
                        if (changed) {
                          synchronized_nodes(updated_nodes)
                          message("✅ Synchronized nodes updated: ", paste(updated_nodes, collapse = ", "))
                          
                          # 🔑 KLÍČOVÁ OPRAVA: Synchronizovat výběr v hlavním grafu ihned
                          session$sendCustomMessage("update-selected-from-gene-list", list(
                            selected_nodes = updated_nodes,
                            patientId = patient
                          ))
                          message("📤 Sent selection update to main graph")
                        } else {
                          message("ℹ️ Nodes unchanged, no update needed")
                        }
                        
                        updateTextAreaInput(session, "new_genes", value = "")
                        updatePickerInput(session, "remove_genes", choices = synchronized_nodes(), selected = NULL)
                        
                        # 🔑 KLÍČOVÁ OPRAVA: Reset clear_all flag IHNED po Clear akci
                        if (clear_all()) {
                          clear_all(FALSE)
                          message("✅ Clear flag reset")
                        }
                        
                        new_genes_var(NULL)
                        remove_genes_var(NULL)
                        
                        message("=== SYNC NODES END ===")
                      })
    
    # Add genes button
    observeEvent(input$confirm_new_genes_btn, {
      message("➕ Add genes button clicked")
      
      new_genes <- input$new_genes
      
      if (!is.null(new_genes) && length(new_genes) > 0 && is.character(new_genes)) {
        # 🔑 Rozdělení podle čárek NEBO nových řádků (stejně jako u pacientů)
        new_genes <- trimws(unlist(strsplit(new_genes, "[,\n\r]+")))
        new_genes <- new_genes[new_genes != ""]
        
        if (length(new_genes) > 0) {
          # 🔑 VALIDACE: Zkontrolovat, jestli geny existují v datech
          available_genes <- subTissue_dt()$feature_name
          invalid_genes <- setdiff(new_genes, available_genes)
          valid_genes <- intersect(new_genes, available_genes)
          
          if (length(invalid_genes) > 0) {
            # Zobrazit warning pro neexistující geny
            shinyalert(
              title = "Invalid genes detected",
              text = paste0(
                "The following genes do not exist in the current dataset and will be ignored:\n\n",
                paste(invalid_genes, collapse = ", "),
                "\n\n",
                if (length(valid_genes) > 0) {
                  paste0("Valid genes that will be added:\n", paste(valid_genes, collapse = ", "))
                } else {
                  "No valid genes to add."
                }
              ),
              type = "warning",
              confirmButtonText = "OK"
            )
            
            message("   ⚠️ Invalid genes: ", paste(invalid_genes, collapse = ", "))
            
            # Pokud jsou nějaké validní geny, přidej je
            if (length(valid_genes) > 0) {
              message("   Adding valid genes: ", paste(valid_genes, collapse = ", "))
              new_genes_var(valid_genes)
            } else {
              message("   No valid genes to add")
              new_genes_var(NULL)
            }
          } else {
            # Všechny geny jsou validní
            message("   Adding genes: ", paste(new_genes, collapse = ", "))
            new_genes_var(new_genes)
          }
        }
      } else {
        new_genes_var(NULL)
      }
    })
    
    # Remove genes button
    observeEvent(input$confirm_remove_genes_btn, {
      message("➖ Remove genes button clicked")
      
      genes_to_remove <- input$remove_genes
      
      if (!is.null(genes_to_remove) && length(genes_to_remove) > 0) {
        message("   Removing genes: ", paste(genes_to_remove, collapse = ", "))
        remove_genes_var(genes_to_remove)
      } else {
        remove_genes_var(NULL)
      }
    })
    
    # Pathway/tissue change - clear subset
    observeEvent(list(input$selected_pathway, input$selected_tissue), {
      req(active())
      message("🔄 Pathway/tissue changed")
      
      # 🔑 DŮLEŽITÉ: NEZMĚNIT synchronized_nodes() - zachovat všechny uzly
      # Uživatel může mít uzly z Add button které nejsou v novém grafu
      # Tyto uzly by měly zůstat v selection
      
      # Selection se pošle v cy-init observeEvent (po vytvoření nového grafu)
      if (length(synchronized_nodes()) > 0) {
        message("   ℹ️ Keeping selection: ", paste(synchronized_nodes(), collapse = ", "))
      }
      
      # Vyčistit subset graf - bude se znovu renderovat v observe(current_nodes)
      session$sendCustomMessage("cy-subset", list(
        elements = list(nodes = list(), edges = list()),
        containerId = cy_subset_container_id,
        patientId = patient
      ))
      
      # 🔑 DŮLEŽITÉ: Vynuceně aktualizovat sub_interactions po změně pathway
      # aby se cy-subset observer spustil s novými daty
      if (length(synchronized_nodes()) > 0) {
        # Tento observe se spustí hned jak bude subTissue_dt() připravený
        message("   🔄 Requesting sub-interactions update for new pathway...")
      }
    }, priority = 20)
    
    ########################
    #### Network buttons ###
    ########################
    
    observeEvent(input$selected_layout, {
      session$sendCustomMessage("cy-layout", list(layout = input$selected_layout, patientId = patient))
    })
    
    # ✅ OPRAVENO: JEN JEDEN clearSelection_btn observer!
    observeEvent(input$clearSelection_btn, {
      message("🗑️ Clear selection button clicked")
      
      clear_all(TRUE)
      new_genes_var(NULL)
      remove_genes_var(NULL)
      synchronized_nodes(character(0))
      
      session$sendCustomMessage("update-selected-from-gene-list", list(
        selected_nodes = list(),
        patientId = patient
      ))
      session$sendCustomMessage("cy-subset", list(
        elements = list(nodes = list(), edges = list()),
        containerId = cy_subset_container_id,
        patientId = patient
      ))
      
      updatePickerInput(session, "remove_genes", choices = character(0), selected = NULL)
      updateTextAreaInput(session, "new_genes", value = "")
      
      message("✅ All selections cleared")
    })
    
    observeEvent(input$selectNeighbors_btn, {
      selected_gene <- input$selected_row
      session$sendCustomMessage("select-first-neighbors", list(gene = selected_gene, patientId = patient))
    })
    
    observeEvent(input$fitGraph_btn, {
      # Použít synchronized_nodes() jako single source of truth
      selected_genes <- synchronized_nodes()
      
      # 🔑 FILTROVAT: Poslat pouze uzly které JSOU v aktuální pathway
      req(tissue_dt())
      genes_in_pathway <- selected_genes[selected_genes %in% tissue_dt()$feature_name]
      
      if (length(genes_in_pathway) > 0) {
        message("🎯 Fit graph to genes in current pathway: ", paste(genes_in_pathway, collapse = ", "))
      } else {
        message("⚠️ No selected genes found in current pathway, fitting to entire graph")
      }
      
      session$sendCustomMessage("fit-selected-nodes", list(
        nodes = if (length(genes_in_pathway) > 0) genes_in_pathway else NULL,
        patientId = patient))
    })
    
    #################################################
    ### Selected variant or fusion data + buttons ###
    #################################################
    
    observe({
      som_vars <- as.data.table(shared_data$somatic.variants())
      germ_vars <- as.data.table(shared_data$germline.variants())
      fusions <- as.data.table(shared_data$fusion.variants())
      
      # Filter by current patient
      if (!is.null(som_vars) && nrow(som_vars) > 0 && "sample" %in% names(som_vars)) {
        som_vars <- som_vars[sample == patient]
      }
      if (!is.null(germ_vars) && nrow(germ_vars) > 0 && "sample" %in% names(germ_vars)) {
        germ_vars <- germ_vars[sample == patient]
      }
      if (!is.null(fusions) && nrow(fusions) > 0 && "sample" %in% names(fusions)) {
        fusions <- fusions[sample == patient]
      }
      
      selected_dt(NULL)
      
      if (is.null(som_vars) && is.null(germ_vars) && is.null(fusions)) {
        selected_dt(data.table(gene_symbol = character(0), var_name = character(0), 
                               fusion = character(0), pathway = character(0)))
        return()
      }
      
      variants_list <- list()
      
      if (!is.null(som_vars) && nrow(som_vars) > 0) {
        variants_list[[1]] <- som_vars[, .(gene_symbol, var_name = "somatic")]
      }
      
      if (!is.null(germ_vars) && nrow(germ_vars) > 0) {
        variants_list[[2]] <- germ_vars[, .(gene_symbol, var_name = "germline")]
      }
      
      combined_variants <- if(length(variants_list) > 0) {
        unique(rbindlist(variants_list, fill = TRUE))
      } else {
        data.table(gene_symbol = character(0), var_name = character(0))
      }
      
      fusions_dt <- if (!is.null(fusions) && nrow(fusions) > 0) {
        unique(rbindlist(list(
          fusions[, .(gene_symbol = gene1, fusion = "yes")],
          fusions[, .(gene_symbol = gene2, fusion = "yes")]
        )))
      } else {
        data.table(gene_symbol = character(0), fusion = character(0))
      }
      
      combined_selected <- merge(combined_variants, fusions_dt, by = "gene_symbol", all = TRUE)
      
      if (nrow(combined_selected) > 0) {
        # Filter pathways by current patient's sample
        pathways_info <- subTissue_dt()[
          feature_name %in% combined_selected$gene_symbol & sample == patient, 
          .(pathway = paste(unique(pathway), collapse = "; ")), 
          by = feature_name
        ]
        setnames(pathways_info, "feature_name", "gene_symbol")
        combined_selected <- merge(combined_selected, pathways_info, by = "gene_symbol", all.x = TRUE)
      }
      
      selected_dt(combined_selected)
    })
    
    # Variant borders configuration
    variant_config <- list(
      somatic = list(
        input_id = "selectedSomVariants",
        column = "var_name",
        value = "somatic",
        filter = function(dt) dt[var_name == "somatic", unique(gene_symbol)],
        alert_text = "You don't have any somatic variants selected as possibly oncogenic.",
        tab = "variants",
        box = "somatic"
      ),
      germline = list(
        input_id = "selectedGermVariants",
        column = "var_name",
        value = "germline",
        filter = function(dt) dt[var_name == "germline", unique(gene_symbol)],
        alert_text = "No germline variants are currently selected as possibly pathogenic.",
        tab = "variants",
        box = "germline"
      ),
      fusion = list(
        input_id = "selectedFusions",
        column = "fusion",
        value = "yes",
        filter = function(dt) dt[!is.na(fusion), unique(gene_symbol)],
        alert_text = "No fusion genes are currently selected.",
        tab = "fusion_genes",
        box = NULL
      )
    )
    
    lapply(names(variant_config), function(type) {
      config <- variant_config[[type]]
      
      observeEvent(list(input[[config$input_id]], input$selected_pathway, 
                        input$selected_tissue, selected_dt()), {
                          req(active())
                          req(selected_dt())
                          
                          # Check if switch is being turned ON
                          if (!isTruthy(input[[config$input_id]])) {
                            # Switch OFF - just remove borders
                            message("Removing ", type, " border")
                            session$sendCustomMessage("variant-border", list(type = type, nodes = character(0), patientId = patient))
                            return()
                          }
                          
                          # Switch ON - validate data availability
                          # First check if dataset exists at all
                          dataset_exists <- switch(type,
                            somatic = length(shared_data$somatic.patients()) > 0,
                            germline = length(shared_data$germline.patients()) > 0,
                            fusion = length(shared_data$fusion.patients()) > 0,
                            FALSE
                          )
                          
                          if (!dataset_exists) {
                            # NO DATA AVAILABLE - dataset doesn't exist
                            updatePrettySwitch(session, config$input_id, value = FALSE)
                            shinyalert(
                              title = "No data available",
                              text = paste0("Dataset for ", type, " is not available for this patient."),
                              type = "warning",
                              confirmButtonText = "OK",
                              showCancelButton = FALSE
                            )
                            return()
                          }
                          
                          # Dataset exists - now check if there are selected variants/fusions
                          sel_dt <- as.data.table(selected_dt())
                          nodes <- as.character(config$filter(sel_dt))
                          
                          if (length(nodes) == 0) {
                            # NO SELECTION FOUND - data exists but nothing selected
                            updatePrettySwitch(session, config$input_id, value = FALSE)
                            shinyalert(
                              title = "No selection found",
                              text = config$alert_text,
                              type = "warning",
                              confirmButtonText = "OK",
                              showCancelButton = TRUE,
                              cancelButtonText = "Go to page",
                              callbackR = function(value) {
                                if (isFALSE(value)) {
                                  # User clicked "Go to page"
                                  parent_ns <- session$userData$parent_session$ns
                                  updateNavbarTabs(
                                    session = session$userData$parent_session,
                                    inputId = "navbarMenu",
                                    selected = parent_ns(config$tab)
                                  )
                                  
                                  # Navigate to specific box for variants
                                  if (!is.null(config$box)) {
                                    session$userData$parent_session$sendCustomMessage(
                                      "scroll-to-box",
                                      list(box = config$box)
                                    )
                                  }
                                }
                              }
                            )
                            # Don't send variant-border message - switch is already OFF
                            return()
                          }
                          
                          # All checks passed - add borders
                          message("Adding ", type, " border for ", length(nodes), " nodes")
                          session$sendCustomMessage("variant-border", list(type = type, nodes = as.list(nodes), patientId = patient))
                        })
    })
    
    ##############
    ### others ###
    ##############
    
    # 🔑 NOVÉ: Handler pro zobrazení edge info
    observeEvent(input$edge_info, {
      req(input$edge_info$html)
      shinyalert(
        title = NULL,
        text = input$edge_info$html,
        html = TRUE,
        type = "",
        confirmButtonText = "Close",
        showCancelButton = FALSE,
        closeOnClickOutside = TRUE  # Umožní zavření kliknutím mimo okno
      )
    })
    
    networkGraph_tables$tab_server("tab", tissue_dt, subTissue_dt, selected_nodes = synchronized_nodes, selected_dt, patient)
    
    # Inicializace Bootstrap popoverů pomocí JavaScriptu + CSS pro z-index
    insertUI(
      selector = "head",
      where = "beforeEnd",
      ui = tags$head(
        tags$style(HTML("
          .popover { z-index: 999999 !important; }
          /* Dropdown seznamy mají vyšší z-index než helpPopover ikony */
          .bootstrap-select .dropdown-menu { z-index: 10000 !important; }
          .selectize-dropdown { z-index: 10000 !important; }
          /* HelpPopover ikony mají nižší z-index než dropdowny */
          [id*='helpPopover'] { z-index: 100 !important; }
        ")),
        tags$script(HTML("$(document).ready(function(){
                            $('[data-toggle=\"popover\"]').popover({
                              container: 'body' });
                          });"))
      ))
    
  })
}

