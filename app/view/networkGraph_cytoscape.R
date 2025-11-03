# shiny::shinyAppDir(".", options = list(launch.browser = TRUE))


box::use(
  shiny[NS, moduleServer, observeEvent, observe, tagList, fluidPage, fluidRow, column, textInput, updateTextInput, actionButton, selectInput, reactive, req,reactiveVal,conditionalPanel,
        verbatimTextOutput, renderPrint,renderText,textOutput,htmlOutput,uiOutput,renderUI,icon,textAreaInput,updateTextAreaInput,isolate,isTruthy,debounce,getDefaultReactiveDomain,
        outputOptions],
  httr[GET, status_code, content],
  bs4Dash[updateTabItems,addPopover],
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
  app/logic/waiters[use_spinner, use_waiter, show_waiter, hide_waiter],
  app/logic/networkGraph_helper[get_string_interactions,prepare_cytoscape_network,get_pathway_list],
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
    # tags$head(tags$script(HTML(sprintf("var cyContainerId = '%s'; var cySubsetContainerId = '%s';",
    #                                     ns("cyContainer"), ns("cySubsetContainer"))))),
    fluidRow(
      column(6,  # 🔑 Levá polovina - hlavní graf
        fluidRow(
          column(4,
                 div(style = "width: 90%;",
                   div(style = "display: flex; flex-direction: column; width: 100%; align-items: flex-start;", 
                     tags$label("Pathway:", style = "margin-bottom: 5px; align-self: flex-start;"),
                     pickerInput(inputId=ns("selected_pathway"), NULL, selected = "EGFR tyrosine kinase inhibitor resistance",choices = NULL, options = list(`live-search` = TRUE), width = "100%")),
                   div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; margin-top: 10px;", 
                     tags$label("Choose layout:", style = "align-self: flex-start;"),
                     tags$div(id = ns("helpPopover_layout"),tags$i(class = "fa fa-question fa-xs", style = "color: #2596be;"))),
                   selectInput(ns("selected_layout"), NULL, choices = c("cola", "fcose"), selected = "cola", width = "100%"))),
          column(4,
                 div(style = "width: 90%;",
                     div(style = "display: flex; flex-direction: column; width: 100%; align-items: flex-start;", 
                         tags$label("Interaction sources:", style = "margin-bottom: 5px; align-self: flex-start;"),
                         pickerInput(inputId=ns("interaction_sources"), NULL, 
                                     selected = c("experiments","databases","textmining","coexpression","neighborhood","gene_fusion","cooccurrence"),
                                     choices = c("experiments","databases","textmining","coexpression","neighborhood","gene_fusion","cooccurrence"), 
                                     multiple = TRUE,
                                     options = list(`live-search` = TRUE, `actions-box` = TRUE), width = "100%")),
                    div(style = "display: flex;  flex-direction: column; align-items: flex-start; width: 100%; margin-top: 10px;", 
                        tags$label("Interaction score:", style = "align-self: flex-start;"),
                        selectInput(inputId=ns("interaction_score"), NULL, selected = "0.4",
                                    choices = c("0.9 (highest)" = "0.9", "0.7 (high)" = "0.7", "0.4 (medium)" = "0.4", "0.15 (low)" = "0.15"), 
                                    width = "100%")),
                 )),
          column(4,
            div(style = "display: flex; flex-direction: column; gap: 5px;",
                prettySwitch(ns("selectedSomVariants"), label = "Add somatic variants", status = "primary", slim = TRUE),
                prettySwitch(ns("selectedGermVariants"), label = "Add germline variants", status = "primary", slim = TRUE),
                prettySwitch(ns("selectedFusions"), label = "Add fusions", status = "primary", slim = TRUE),
                prettySwitch(ns("hide_nodes"), label = "Hide disconnected nodes", status = "primary", slim = TRUE)
        ))),
       fluidRow(
        column(12, networkGraph_tables$selectedTab_UI(ns("tab")))
       )
      ),
      column(1),
      column(5,  # 🔑 Pravá polovina - podgraf + ovládací prvky
             fluidRow(
               column(6,
                      div(style = "display: flex; flex-direction: column; align-items: flex-start; width: 90%;",
                        div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                          tags$label("Add new genes:"),
                          tags$div(id = ns("helpPopover_addGene"),
                                   tags$i(class = "fa fa-question fa-xs", style = "color: #2596be;")),
                          ),
                        textAreaInput(ns("new_genes"), NULL, placeholder = "Enter gene names here...", rows = 1, resize = "vertical", width = "100%"), 
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
      pathway_choices <- get_pathway_list("all_genes", run = shared_data$run)
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
    last_button_action <- reactiveVal(0)  # 🔑 Timestamp poslední button akce
    selected_dt <- reactiveVal(NULL)
    
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
      unique(dt()[grepl(input$selected_pathway, pathway, fixed = TRUE),
                  -c("all_kegg_gene_names","counts_tpm_round","size","mu",
                     "lower_than_p","higher_than_p","type","gene_definition")])
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
      req(input$interaction_score)  # 🔑 Vyžadovat score
      req(input$interaction_sources)  # 🔑 Vyžadovat sources
      
      genes <- tissue_dt()[, feature_name]
      
      start_time <- Sys.time()
      message("⏱️ [interactions] START: Fetching STRING interactions for ", nrow(tissue_dt()), " genes")
      message("   Score threshold: ", input$interaction_score)
      message("   Sources: ", paste(input$interaction_sources, collapse = ", "))
      message("   Genes (comma-separated): ", paste(genes, collapse = ", "))
      
      result <- get_string_interactions(
        genes,
        required_score = as.numeric(input$interaction_score),
        filter_sources = input$interaction_sources
      )
      
      end_time <- Sys.time()
      message("⏱️ [interactions] DONE: ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds")
      message("   Returned ", if(is.data.frame(result)) nrow(result) else "non-df", " interactions")
      
      result
    })
    
    network_json <- reactive({
      req(tissue_dt())
      ints <- interactions()
      req(ints)
      
      if (is.null(ints) || (!is.data.frame(ints) && !is.list(ints))) {
        message("Waiting for valid interactions data...")
        return(NULL)
      }
      
      start_time <- Sys.time()
      message("⏱️ [network_json] START: Preparing network JSON for ", nrow(tissue_dt()), " genes")
      
      result <- prepare_cytoscape_network(
        ints, 
        unique(tissue_dt()[, .(feature_name, log2FC)])
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
      # 🔑 KLÍČOVÁ ZMĚNA: 
      # - Když graph_is_source=TRUE: Použij POUZE nodes_from_graph (čistý user selection z grafu)
      # - Když graph_is_source=FALSE a máme nodes_outside_graph:
      #     Použij nodes_from_graph + zachovej nodes_outside_graph
      # - Jinak: Kombinuj vše (button akce)
      
      if (clear_all) {
        combined <- character(0)
      } else if (graph_is_source && is.null(add_genes) && is.null(remove_genes)) {
        # User změnil selection v grafu a všechny synchronized uzly JSOU v grafu
        # → použij POUZE co je vybráno v grafu (umožní odznačení)
        combined <- nodes_from_graph
      } else if (!graph_is_source && length(nodes_outside_graph) > 0 && is.null(add_genes) && is.null(remove_genes)) {
        # User změnil selection v grafu ale máme uzly MIMO graf (např. BRCA1)
        # → použij nodes_from_graph + zachovej nodes_outside_graph
        # DŮLEŽITÉ: Toto zachová nodes_outside_graph i když user odznačí uzly V grafu
        combined <- unique(c(nodes_from_graph, nodes_outside_graph))
      } else {
        # Button akce (add/remove) → kombinuj vše a aplikuj změny
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
        
        # 🔑 DŮLEŽITÉ: Po vytvoření grafu poslat aktuální selection
        # (aby se uzly vybraly v novém grafu)
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
      req(tissue_dt())  # 🔑 OPRAVA: Použít tissue_dt() místo subTissue_dt()
      # tissue_dt() závisí na PATHWAY i TISSUE, takže se aktualizuje při změně pathway
      req(length(synchronized_nodes()) > 0)
      req(input$interaction_score)  # 🔑 Vyžadovat score
      req(input$interaction_sources)  # 🔑 Vyžadovat sources
      
      start_time <- Sys.time()
      message("⏱️ [sub_interactions] START: Fetching sub-interactions for ", length(synchronized_nodes()), " nodes")
      message("   Pathway: ", input$selected_pathway, ", Tissue: ", input$selected_tissue)
      
      # Použít tissue_dt() pro správnou pathway a stejné parametry jako hlavní graf
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
      current_nodes <- synchronized_nodes()
      message("🔄 cy-subset observer triggered - current_nodes: ", paste(current_nodes, collapse = ", "))
      message("   Pathway: ", input$selected_pathway, ", Tissue: ", input$selected_tissue)
      
      if (length(current_nodes) == 0) {
        message("Žádné uzly nejsou vybrány. Odesílám prázdný podgraf.")
        
        session$sendCustomMessage("cy-subset", list(
          elements = list(nodes = list(), edges = list()),
          containerId = cy_subset_container_id,
          patientId = patient
        ))
        # NOTE: update-selected-from-gene-list se posílá z sync observeEvent, ne zde
      } else {
        req(sub_interactions())
        req(subTissue_dt())  # 🔑 Pro FC data použít subTissue_dt (všechny geny v tissue)
        
        message("Aktualizace podgrafu pro uzly: ", paste(current_nodes, collapse = ", "))
        subnetwork_data <- prepare_cytoscape_network(
          sub_interactions(), 
          unique(subTissue_dt()[feature_name %in% current_nodes, .(feature_name, log2FC)]),  # 🔑 subTissue_dt pro všechny geny
          current_nodes
        )
        message("#### Podgraf připraven - nodes: ", length(subnetwork_data$elements$nodes), ", edges: ", length(subnetwork_data$elements$edges))
        
        subnetwork_data$containerId <- cy_subset_container_id
        subnetwork_data$patientId <- patient
        
        session$sendCustomMessage("cy-subset", subnetwork_data)
        # NOTE: update-selected-from-gene-list se posílá z sync observeEvent, ne zde
      }
    })
    

    # Debug: sledovat změny input$cySelectedNodes
    observe({
      message("👀 input$cySelectedNodes changed: ", paste(input$cySelectedNodes %||% "NULL", collapse = ", "))
    })
    
    observeEvent(list(input$cySelectedNodes, input$confirm_new_genes_btn, 
                      input$confirm_remove_genes_btn, input$clearSelection_btn, 
                      new_genes_var(), remove_genes_var()), {
                        
                        message("=== SYNC NODES START ===")
                        message("🔍 TRIGGER CHECK:")
                        message("   cySelectedNodes: ", paste(input$cySelectedNodes %||% "NULL", collapse = ", "))
                        message("   new_genes_btn: ", input$confirm_new_genes_btn %||% 0)
                        message("   remove_genes_btn: ", input$confirm_remove_genes_btn %||% 0)
                        message("   clear_btn: ", input$clearSelection_btn %||% 0)
                        message("   new_genes_var: ", paste(new_genes_var() %||% "NULL", collapse = ", "))
                        message("   remove_genes_var: ", paste(remove_genes_var() %||% "NULL", collapse = ", "))
                        
                        # 🔑 KLÍČOVÁ OPRAVA: Když trigger přichází z button akce (new_genes/remove_genes),
                        # použij synchronized_nodes() místo input$cySelectedNodes, protože JS se ještě neaktualizoval
                        is_button_trigger <- !is.null(new_genes_var()) || !is.null(remove_genes_var()) || clear_all()
                        
                        # 🔑 NOVÁ OCHRANA: Ignoruj input$cySelectedNodes pokud byl button stisknut v posledních 500ms
                        time_since_button <- as.numeric(Sys.time()) - last_button_action()
                        recently_used_button <- time_since_button < 0.5
                        
                        # 🔑 KRITICKÁ OCHRANA: Ignoruj input$cySelectedNodes když byl button použit nedávno
                        # Časový timeout (500ms) je JEDINÝ spolehlivý způsob jak detekovat staré hodnoty
                        sync_nodes_count <- length(synchronized_nodes())
                        cysel_nodes_count <- length(input$cySelectedNodes %||% character(0))
                        cysel_nodes <- input$cySelectedNodes %||% character(0)
                        sync_nodes <- synchronized_nodes()
                        
                        # has_extra_nodes check byl ODSTRANĚN - nedokázal spolehlivě rozlišit
                        # staré hodnoty od legitimních nových výběrů z grafu
                        # Spoléháme se POUZE na recently_used_button timeout (500ms)
                        
                        if (is_button_trigger) {
                          last_button_action(as.numeric(Sys.time()))  # Zaznamenej čas button akce
                          message("🔘 Button trigger detected, using synchronized_nodes()")
                        } else if (recently_used_button) {
                          message("⏱️ Ignoring stale input$cySelectedNodes (button action within 500ms)")
                          return()  # Ignoruj tento trigger
                        }
                        # has_extra_nodes a has_nodes_not_in_graph checks ODSTRANĚNY
                        # Spoléháme se POUZE na 500ms timeout
                        
                        # 🔑 NOVÁ OCHRANA: Detekuj JS selhání (uzly nebyly nalezeny v grafu)
                        # POUZE po button akcích kdy očekáváme že JS vybere všechny uzly
                        # 
                        # Pokud cysel má MÉNĚ uzlů než sync, může to být:
                        # 1. User odznačil uzel (LEGITIMNÍ) - všechny cysel uzly jsou v sync
                        # 2. JS selhal (např. BRCA1 není v grafu) - synchronized má uzly které nejsou v cysel
                        #    ALE všechny synchronized uzly MĚLY být v cysel (protože button je přidal)
                        if (!is_button_trigger && sync_nodes_count > 0 && cysel_nodes_count < sync_nodes_count) {
                          # Kontrola: Jsou všechny cysel uzly v synchronized?
                          all_cysel_in_sync <- all(cysel_nodes %in% sync_nodes)
                          
                          if (all_cysel_in_sync) {
                            # ✅ LEGITIMNÍ odznačení - všechny cysel uzly jsou v sync, jen jich je méně
                            message("✓ User deselected nodes in graph")
                          } else {
                            # ❌ PROBLÉM - cysel obsahuje uzly které NEJSOU v sync
                            # To by nemělo nastat, ignoruj
                            message("⚠️ Ignoring input$cySelectedNodes - contains nodes not in synchronized")
                            return()
                          }
                        }
                        
                        nodes_from_graph <- if (is_button_trigger || recently_used_button) {
                          synchronized_nodes()  # Použij aktuální synchronized hodnotu
                        } else {
                          input$cySelectedNodes %||% character(0)  # Použij hodnotu z grafu
                        }
                        
                        message("nodes_from_graph: ", paste(nodes_from_graph, collapse = ", "))
                        message("current_genes: ", paste(synchronized_nodes(), collapse = ", "))
                        message("new_genes: ", paste(new_genes_var() %||% character(0), collapse = ", "))
                        message("remove_genes: ", paste(remove_genes_var() %||% character(0), collapse = ", "))
                        message("clear_all: ", clear_all())
                        
                        # 🔑 KLÍČOVÁ LOGIKA pro graph_is_source:
                        # Graf je source (použij JEN nodes_from_graph) POUZE když:
                        # 1. NENÍ button trigger (uživatel kliká v grafu)
                        # 2. A nodes_from_graph je NADMNOŽINA synchronized_nodes
                        #    (všechny uzly ze synchronized jsou v nodes_from_graph, takže můžeme použít jen graf)
                        # 
                        # Pokud synchronized obsahuje uzly které NEJSOU v nodes_from_graph,
                        # rozlišíme dvě situace:
                        # - Uzel NENÍ v hlavním grafu vůbec (např. BRCA1) → zachovat
                        # - Uzel JE v hlavním grafu ale není vybraný → user ho odznačil → smazat
                        
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
        new_genes <- trimws(unlist(strsplit(new_genes, ",")))
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
      
      selected_dt(NULL)
      
      if (is.null(som_vars) && is.null(germ_vars) && is.null(fusions)) {
        selected_dt(data.table(Gene_symbol = character(0), var_name = character(0), 
                               fusion = character(0), pathway = character(0)))
        return()
      }
      
      variants_list <- list()
      
      if (!is.null(som_vars) && nrow(som_vars) > 0) {
        variants_list[[1]] <- som_vars[, .(Gene_symbol, var_name = "somatic")]
      }
      
      if (!is.null(germ_vars) && nrow(germ_vars) > 0) {
        variants_list[[2]] <- germ_vars[, .(Gene_symbol, var_name = "germline")]
      }
      
      combined_variants <- if(length(variants_list) > 0) {
        unique(rbindlist(variants_list, fill = TRUE))
      } else {
        data.table(Gene_symbol = character(0), var_name = character(0))
      }
      
      fusions_dt <- if (!is.null(fusions) && nrow(fusions) > 0) {
        unique(rbindlist(list(
          fusions[, .(Gene_symbol = gene1, fusion = "yes")],
          fusions[, .(Gene_symbol = gene2, fusion = "yes")]
        )))
      } else {
        data.table(Gene_symbol = character(0), fusion = character(0))
      }
      
      combined_selected <- merge(combined_variants, fusions_dt, by = "Gene_symbol", all = TRUE)
      
      if (nrow(combined_selected) > 0) {
        pathways_info <- subTissue_dt()[
          feature_name %in% combined_selected$Gene_symbol, 
          .(pathway = paste(unique(pathway), collapse = "; ")), 
          by = feature_name
        ]
        setnames(pathways_info, "feature_name", "Gene_symbol")
        combined_selected <- merge(combined_selected, pathways_info, by = "Gene_symbol", all.x = TRUE)
      }
      
      selected_dt(combined_selected)
    })
    
    # Variant borders configuration
    variant_config <- list(
      somatic = list(
        input_id = "selectedSomVariants",
        column = "var_name",
        filter = function(dt) dt[var_name == "somatic", unique(Gene_symbol)],
        alert_text = "You don't have any somatic variants selected as possibly oncogenic.",
        tab = "variants"
      ),
      germline = list(
        input_id = "selectedGermVariants",
        column = "var_name",
        filter = function(dt) dt[var_name == "germline", unique(Gene_symbol)],
        alert_text = "No germline variants are currently selected as possibly pathogenic.",
        tab = "variants"
      ),
      fusion = list(
        input_id = "selectedFusions",
        column = "fusion",
        filter = function(dt) dt[!is.na(fusion), unique(Gene_symbol)],
        alert_text = "No fusion genes are currently selected.",
        tab = "fusion_genes"
      )
    )
    
    lapply(names(variant_config), function(type) {
      config <- variant_config[[type]]
      
      observeEvent(list(input[[config$input_id]], input$selected_pathway, 
                        input$selected_tissue, selected_dt()), {
                          req(active())
                          req(selected_dt())
                          
                          if (!config$column %in% names(selected_dt())) {
                            if (isTruthy(input[[config$input_id]])) {
                              updatePrettySwitch(session, config$input_id, value = FALSE)
                              shinyalert(
                                title = "No data available",
                                text = config$alert_text,
                                type = "warning",
                                confirmButtonText = "OK",
                                showCancelButton = TRUE,
                                cancelButtonText = "Go to page",
                                callbackR = function(value) {}
                              )
                            }
                            session$sendCustomMessage("variant-border", list(type = type, nodes = character(0), patientId = patient))
                            return()
                          }
                          
                          sel_dt <- as.data.table(selected_dt())
                          nodes <- as.character(config$filter(sel_dt))
                          
                          if (isTruthy(input[[config$input_id]])) {
                            if (length(nodes) == 0) {
                              updatePrettySwitch(session, config$input_id, value = FALSE)
                              shinyalert(
                                title = "No selection found",
                                text = config$alert_text,
                                type = "warning",
                                confirmButtonText = "OK",
                                showCancelButton = TRUE,
                                cancelButtonText = "Go to page",
                                callbackR = function(value) {}
                              )
                            } else {
                              message("Adding ", type, " border for ", length(nodes), " nodes")
                              session$sendCustomMessage("variant-border", list(type = type, nodes = as.list(nodes), patientId = patient))
                            }
                          } else {
                            message("Removing ", type, " border")
                            session$sendCustomMessage("variant-border", list(type = type, nodes = character(0), patientId = patient))
                          }
                        })
    })
    
    ##############
    ### others ###
    ##############
    
    networkGraph_tables$tab_server("tab", tissue_dt, subTissue_dt, selected_nodes = synchronized_nodes, selected_dt, patient)
    
    addPopover(id = "helpPopover_addGene", options = list(
      title = "Write comma-separated text:",
      content = "example: BRCA1, TP53, FOXO3",
      placement = "right",
      trigger = "hover"
    ))
    
    addPopover(id = "helpPopover_layout", options = list(
      title = "Layout options:",
      placement = "right",
      trigger = "hover",
      content = "cola – Ideal for hierarchical structures and smaller graphs. FCOSE – Best for large and complex networks."
    ))
    
  })
}

