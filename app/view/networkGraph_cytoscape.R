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
)

box::use(
  app/logic/load_data[load_data],
  app/logic/waiters[use_spinner],
  app/logic/networkGraph_helper[get_string_interactions,prepare_cytoscape_network,get_pathway_list],
  app/view/networkGraph_tables,

)

ui <- function(id, tissue_list, patient) {
  ns <- NS(id)
  useShinyjs()

  tagList(
    # tags$head(tags$script(HTML(sprintf("var cyContainerId = '%s'; var cySubsetContainerId = '%s';",
    #                                     ns("cyContainer"), ns("cySubsetContainer"))))),
    fluidRow(
      column(8,
        fluidRow(
          column(3,
                 div(style = "width: 75%;",
                   div(style = "display: flex; flex-direction: column; width: 100%; align-items: flex-start;", 
                     tags$label("Pathway:", style = "margin-bottom: 5px; align-self: flex-start;"),
                     pickerInput(inputId=ns("selected_pathway"), NULL, selected = "EGFR tyrosine kinase inhibitor resistance",choices = get_pathway_list("all_genes"), options = list(`live-search` = TRUE), width = "100%")),
                   div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; margin-top: 10px;", 
                     tags$label("Choose layout:", style = "align-self: flex-start;"),
                     tags$div(id = ns("helpPopover_layout"),tags$i(class = "fa fa-question fa-xs", style = "color: #2596be;"))),
                   selectInput(ns("selected_layout"), NULL, choices = c("cola", "fcose"), selected = "cola", width = "100%"))),
          column(3.3,
            div(style = "display: flex; align-items: center; gap: 10px;",
                prettySwitch(ns("selectedSomVariants"), label = "Add possibly pathogenic somatic variants", status = "primary", slim = TRUE),
                ),
            div(style = "display: flex; align-items: center; gap: 10px;",
                prettySwitch(ns("selectedGermVariants"), label = "Add possibly pathogenic germline variants", status = "primary", slim = TRUE),
                ),
            div(style = "display: flex; align-items: center; gap: 10px;",
                prettySwitch(ns("selectedFusions"), label = "Add selected fusions", status = "primary", slim = TRUE),
                )), # right = TRUE, #width = "240px"
         column(6, networkGraph_tables$selectedTab_UI(ns("tab")))
       )
      ),
      # column(1,),
      column(4,
             fluidRow(
               column(6,
                      div(style = "display: flex; flex-direction: column; align-items: flex-start; width: 75%;",
                        div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                          tags$label("Add new genes:"),
                          tags$div(id = ns("helpPopover_addGene"),
                                   tags$i(class = "fa fa-question fa-xs", style = "color: #2596be;")),
                          ),
                        textAreaInput(ns("new_genes"), NULL, placeholder = "Enter gene names here...", rows = 1, resize = "vertical", width = "100%"), 
                        actionButton(ns("confirm_new_genes_btn"), label = "Add Genes", icon = icon("check"), width = "100%", style = "margin-top: 10px;"))),
               column(6,
                      div(style = "display: flex; flex-direction: column; align-items: flex-start; width: 75%;",
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

    active <- reactive({
      if (is.null(tabset_input_id) || is.null(tab_value)) {
        return(TRUE)  # Fallback pro testování
      }
      session$userData$parent_session$input[[tabset_input_id]] == tab_value
    })

    
    interactions <- reactiveVal(NULL)
    sub_interactions <- reactiveVal(NULL)
    synchronized_nodes <- reactiveVal(character(0))
    new_genes_var <- reactiveVal(NULL)
    remove_genes_var <- reactiveVal(NULL)
    clear_all <- reactiveVal(FALSE)
    # result_dt <- reactiveVal(NULL)
    selected_dt <- reactiveVal(NULL)
    

    
    
    # Load and process data table when the menuItem is active
    dt <- reactive({
      req(active())
      message("Loading input data for network graph: ", patient_files$expression)
      load_data(patient_files, "expression", patient)
    })


    # dt <- input_data("MR1507","all_genes")
    # dt <- input_data("FZ0711","all_genes")
    #
    # # subTissue_dt <- fread("input_files/MOII_e117/RNAseq21_NEW/MR1507/Blood_all_genes_oneRow.tsv")
    # dt <- fread("input_files/MOII_e117/RNAseq21_NEW/MR1507/Blood_all_genes_oneRow.tsv")
    # pathway_dt <- unique(dt[grepl("Metabolic pathways", all_kegg_paths_name, fixed = TRUE),-c("all_kegg_gene_names","counts_tpm_round","size","mu","lower_than_p","higher_than_p","type","gene_definition")])
    # tissue_dt <- unique(pathway_dt[tissue == "Breast"])
    # interactions <- get_string_interactions(tissue_dt[, feature_name])
    # network_json <- prepare_cytoscape_network(interactions, tissue_dt[, feature_name], tissue_dt[, log2FC])

    subTissue_dt <- reactive({
      req(dt())
      req(input$selected_tissue)
      unique(dt()[tissue == input$selected_tissue])
    })

    pathway_dt <- reactive({
      req(dt())
      req(input$selected_pathway)
      unique(dt()[grepl(input$selected_pathway, pathway,fixed = TRUE),-c("all_kegg_gene_names","counts_tpm_round","size","mu","lower_than_p","higher_than_p","type","gene_definition")])
    })

    tissue_dt <- reactive({
      req(pathway_dt())
      req(input$selected_tissue)
      unique(pathway_dt()[tissue == input$selected_tissue])
    })

    interactions <- reactive({
      req(tissue_dt())
      message("Fetching STRING interactions")
      get_string_interactions(tissue_dt()[, feature_name])
    })

    network_json <- reactive({
      req(active(), tissue_dt())

      ints <- interactions()
      req(ints)
      
      if (is.null(ints) || (!is.data.frame(ints) && !is.list(ints))) {
        message("Waiting for valid interactions data...")
        return(NULL)
      }
      
      message("Preparing network JSON for ", nrow(tissue_dt()), " genes")
      prepare_cytoscape_network(ints, unique(tissue_dt()[, .(feature_name, log2FC)]))
    })
    
    output$js_namespace <- renderUI({    # Předat jmenný prostor do JavaScriptu - umožní  používat ns v JS
      tags$script(HTML(sprintf("var ns = '%s';", ns(""))))
    })

    ##################################
    ## Network node synchronization ##
    ##################################

    # sync_nodes <- memoise(function(nodes_from_graph, current_genes, add_genes = NULL, remove_genes = NULL, clear_all) {
    #   
    #   ifelse(clear_all, combined <- character(0), combined <- unique(c(nodes_from_graph, current_genes)))
    # 
    #   if (!is.null(add_genes) && length(add_genes) > 0) combined <- unique(c(combined, add_genes))  # Přidání nových genů
    #   if (!is.null(remove_genes) && length(remove_genes) > 0) combined <- setdiff(combined, remove_genes)  # Odebrání genů
    # 
    #   combined <- combined[!is.na(combined) & combined != ""]  # Odstranění prázdných hodnot
    #   changed <- !setequal(synchronized_nodes(), combined)  # Kontrola změny
    # 
    #   return(list(updated_nodes = combined, changed = changed))  # Návrat aktualizovaných uzlů a informace o změně
    # })

    sync_nodes_impl <- function(nodes_from_graph, current_genes, add_genes = NULL, remove_genes = NULL, clear_all) {
      
      ifelse(clear_all, combined <- character(0), combined <- unique(c(nodes_from_graph, current_genes)))
      
      if (!is.null(add_genes) && length(add_genes) > 0) combined <- unique(c(combined, add_genes))
      if (!is.null(remove_genes) && length(remove_genes) > 0) combined <- setdiff(combined, remove_genes)
      
      combined <- combined[!is.na(combined) & combined != ""]
      changed <- !setequal(synchronized_nodes(), combined)
      
      return(list(updated_nodes = combined, changed = changed))
    }

    sync_nodes <- sync_nodes_impl

    ########################
    ## Network UI buttons ##
    ########################


    observeEvent(list(active(), input$selected_pathway, input$selected_tissue, network_json()), {
      req(active(), input$selected_pathway, input$selected_tissue, network_json())
      
      tryCatch({
        # json_data <- network_json()
        # json_data$containerId <- cy_container_id
        # json_data$patientId <- patient
        network_data <- network_json()
        
        network_data$containerId <- cy_container_id
        network_data$patientId <- patient
        
        # # Pokud je to JSON string, parsuj ho zpět
        # if (is.character(network_data)) {
        #   network_data <- fromJSON(network_data)
        # }
        # 
        # # Vytvoř nový list s přidanými parametry
        # json_data <- list(
        #   elements = network_data$elements,
        #   containerId = cy_container_id,
        #   patientId = patient
        # )

      
        message("Initializing cytoscape for patient: ", patient)
        # session$sendCustomMessage("cy-init", json_data)
        session$sendCustomMessage("cy-init", network_data)
      }, error = function(e) {
        message("Error initializing cytoscape: ", e$message)
      })
    })

    sub_interactions <- reactive({
      req(active(), subTissue_dt(), length(synchronized_nodes()) > 0)
      message("Fetching sub-interactions for ", length(synchronized_nodes()), " nodes")
      get_string_interactions(unique(subTissue_dt()[feature_name %in% synchronized_nodes(), feature_name]))
    }) %>% debounce(500)
 
    # observeEvent(list(active(), synchronized_nodes(), sub_interactions()), {
    #   req(active())
    #   
    #   current_nodes <- synchronized_nodes()
    #   
    #   if (length(current_nodes) == 0) {
    #     empty_json <- list(
    #       elements = list(nodes = list(), edges = list()),
    #       containerId = cy_subset_container_id,
    #       patientId = patient
    #     )
    #     session$sendCustomMessage("cy-subset", empty_json)
    #     session$sendCustomMessage("update-selected-from-gene-list", list(selected_nodes = list(), patientId = patient))
    #   } else {
    #     subnetwork_data <- prepare_cytoscape_network(
    #       sub_interactions(), 
    #       unique(subTissue_dt()[feature_name %in% current_nodes, .(feature_name, log2FC)]), 
    #       current_nodes
    #     )
    #     subnetwork_data$containerId <- cy_subset_container_id
    #     subnetwork_data$patientId <- patient
    #     
    #     session$sendCustomMessage("cy-subset", subnetwork_data)
    #     session$sendCustomMessage("update-selected-from-gene-list", list(selected_nodes = current_nodes, patientId = patient))
    #   }
    # })
    observeEvent(list(active(), synchronized_nodes(), sub_interactions()), {
      req(active())
      
      current_nodes <- synchronized_nodes()
      
      # Pokud nejsou žádné uzly, poslat clear signál
      if (length(current_nodes) == 0) {
        message("🧹 Clearing subset graph - no nodes selected")
        
        empty_json <- list(
          elements = list(nodes = list(), edges = list()),
          containerId = cy_subset_container_id,
          patientId = patient,
          clear = TRUE
        )
        
        session$sendCustomMessage("cy-subset", empty_json)
        session$sendCustomMessage("update-selected-from-gene-list", 
                                  list(selected_nodes = list(), patientId = patient))
        return()
      }
      
      # Získat sub-interakce
      sub_ints <- sub_interactions()
      
      # Validace dat
      if (is.null(sub_ints) || (!is.data.frame(sub_ints) && !is.list(sub_ints))) {
        message("⏳ Waiting for valid sub-interactions data...")
        return()
      }
      
      message("📊 Preparing subnetwork for ", length(current_nodes), " nodes")
      
      # Připravit data pro subgraf
      subnetwork_data <- tryCatch({
        prepare_cytoscape_network(
          sub_ints, 
          unique(subTissue_dt()[feature_name %in% current_nodes, .(feature_name, log2FC)]), 
          current_nodes
        )
      }, error = function(e) {
        message("❌ Error preparing subnetwork: ", e$message)
        return(NULL)
      })
      
      if (is.null(subnetwork_data)) {
        message("❌ Failed to prepare subnetwork data")
        return()
      }
      
      # Přidat metadata
      subnetwork_data$containerId <- cy_subset_container_id
      subnetwork_data$patientId <- patient
      subnetwork_data$clear <- FALSE
      
      # Debug výpis
      message("✅ Sending subnetwork with ", 
              length(subnetwork_data$elements$nodes), " nodes and ",
              length(subnetwork_data$elements$edges), " edges")
      
      # Odeslat data
      session$sendCustomMessage("cy-subset", subnetwork_data)
      session$sendCustomMessage("update-selected-from-gene-list", 
                                list(selected_nodes = current_nodes, patientId = patient))
    }, priority = 10)

#     
#     observeEvent(list(input$cySelectedNodes, input$confirm_new_genes_btn, input$confirm_remove_genes_btn, input$clearSelection_btn, new_genes_var(),remove_genes_var()), {
# 
#       message("nodes_from_graph: ", paste(input$cySelectedNodes, collapse = ", "))
#       message("current_genes: ", paste(synchronized_nodes(), collapse = ", "))
#       message("new_genes: ", paste(new_genes_var(), collapse = ", "))
#       message("remove_genes: ", paste(remove_genes_var(), collapse = ", "))
#       message("clear_all: ", clear_all())
# 
#       result <- sync_nodes(
#         nodes_from_graph = input$cySelectedNodes,
#         current_genes = synchronized_nodes(),
#         add_genes = new_genes_var(),
#         remove_genes = remove_genes_var(),
#         clear_all = clear_all()
#       )
# 
#       updated_nodes <- result$updated_nodes
#       changed <- result$changed
#       message("updated_nodes: ", paste(updated_nodes, collapse = ", "))
# 
#       if (changed) {
#         synchronized_nodes(updated_nodes) # Synchronizace uzlů
#         message("Synchronizované uzly byly aktualizovány: ", paste(updated_nodes, collapse = ", "))
#       } else {
#         message("Uzly se nezměnily, žádná aktualizace není potřeba.")
#       }
# 
#       # Reset vstupů pro přidání a odebrání genů
#       updateTextAreaInput(session, "new_genes", value = "")
#       updatePickerInput(session, "remove_genes", choices = synchronized_nodes(), selected = NULL)
# 
#       if (clear_all()) {
#         if (length(updated_nodes) == 0 && length(input$cySelectedNodes) == 0 && length(synchronized_nodes()) == 0 ) {
#           clear_all(FALSE)
#           message("Clear selection completed. Resetting clear_all to FALSE.")
#         }
#       }
# 
#       message("# konec hlavního observeEventu.")
#     })
# 
    observeEvent(list(input$cySelectedNodes, input$confirm_new_genes_btn, 
                      input$confirm_remove_genes_btn, input$clearSelection_btn, 
                      new_genes_var(), remove_genes_var()), {
                        
                        message("=== SYNC NODES START ===")
                        message("nodes_from_graph: ", paste(input$cySelectedNodes, collapse = ", "))
                        message("current_genes: ", paste(synchronized_nodes(), collapse = ", "))
                        message("new_genes: ", paste(new_genes_var(), collapse = ", "))
                        message("remove_genes: ", paste(remove_genes_var(), collapse = ", "))
                        message("clear_all: ", clear_all())
                        
                        result <- sync_nodes(
                          nodes_from_graph = input$cySelectedNodes,
                          current_genes = synchronized_nodes(),
                          add_genes = new_genes_var(),
                          remove_genes = remove_genes_var(),
                          clear_all = clear_all()
                        )
                        
                        updated_nodes <- result$updated_nodes
                        changed <- result$changed
                        
                        message("updated_nodes: ", paste(updated_nodes, collapse = ", "))
                        message("changed: ", changed)
                        
                        if (changed) {
                          synchronized_nodes(updated_nodes)
                          message("✅ Synchronized nodes updated: ", paste(updated_nodes, collapse = ", "))
                        } else {
                          message("ℹ️ Nodes unchanged, no update needed")
                        }
                        
                        # Reset vstupů
                        updateTextAreaInput(session, "new_genes", value = "")
                        updatePickerInput(session, "remove_genes", choices = synchronized_nodes(), selected = NULL)
                        
                        # Reset clear_all flagu po úspěšném vyčištění
                        if (clear_all()) {
                          if (length(updated_nodes) == 0 && length(input$cySelectedNodes) == 0) {
                            clear_all(FALSE)
                            message("✅ Clear completed, resetting clear_all flag")
                          }
                        }
                        
                        # Reset proměnných pro přidání/odebrání
                        new_genes_var(NULL)
                        remove_genes_var(NULL)
                        
                        message("=== SYNC NODES END ===")
                      })
    
    # Přidání nových genů do pickerInput po stisknutí tlačítka "Add Genes"
      observeEvent(input$confirm_new_genes_btn, {
        new_genes <- input$new_genes

        if (!is.null(new_genes) && length(new_genes) > 0 && is.character(new_genes)) {
          new_genes <- trimws(unlist(strsplit(new_genes, ",")))
          new_genes <- new_genes[new_genes != ""]
          new_genes_var(new_genes)
          updateTextAreaInput(session, "new_genes", value = "")
        } else {
          new_genes_var(NULL)
          message("Žádné nové geny nebyly zadány.")
        }
        message("# konec eventu confirm_new_genes_btn")
    })


   # Odebrání vybraných genů z pickerInput
      observeEvent(input$confirm_remove_genes_btn, {
        genes_to_remove <- input$remove_genes

        if (!is.null(genes_to_remove) && length(genes_to_remove) > 0) {
          remove_genes_var(genes_to_remove)
          updatePickerInput(session, "remove_genes", choices = synchronized_nodes(), selected = NULL)
        } else {
          remove_genes_var(NULL)
          message("Žádné geny nebyly vybrány k odebrání.")
        }
        message("# konec eventu confirm_remove_genes_btn.")
      })

      
      observeEvent(list(input$selected_pathway, input$selected_tissue), {
        # Když se změní pathway nebo tissue, vyčistit subset graf
        message("🔄 Pathway/tissue changed, clearing subset")
        
        empty_data <- list(
          elements = list(nodes = list(), edges = list()),
          containerId = cy_subset_container_id,
          patientId = patient,
          clear = TRUE
        )
        
        session$sendCustomMessage("cy-subset", empty_data)
        
        # NEMAZAT synchronized_nodes! - hlavní graf si je pamatuje správně
      }, priority = 20)  # Vyšší priorita, aby se provedlo před ostatními
    ########################
    #### Network buttons ###
    ########################

    observeEvent(input$selected_layout, {
      session$sendCustomMessage("cy-layout",list(layout = input$selected_layout, patientId = patient))
    })

    # observeEvent(input$clearSelection_btn, {
    #   clear_all(TRUE)
    #   new_genes_var(NULL)
    #   remove_genes_var(NULL)
    #   synchronized_nodes(character(0))  # Jasně nastaví stav synchronizovaných uzlů
    # 
    #   # Explicitní aktualizace UI komponent
    #   session$sendCustomMessage("update-selected-from-gene-list", list(selected_nodes = list(), patientId = patient))
    #   session$sendCustomMessage("cy-subset", toJSON(list(elements = list(nodes = list(), edges = list())), auto_unbox = TRUE))
    #   updatePickerInput(session, "remove_genes", choices = character(0), selected = NULL)
    #   updateTextAreaInput(session, "new_genes", value = "")
    # 
    #   message("All selections cleared.")
    # })
      # observeEvent(input$clearSelection_btn, {
      #   clear_all(TRUE)
      #   new_genes_var(NULL)
      #   remove_genes_var(NULL)
      #   synchronized_nodes(character(0))
      #   
      #   empty_data <- list(
      #     elements = list(nodes = list(), edges = list()),
      #     containerId = cy_subset_container_id,
      #     patientId = patient
      #   )
      #   
      #   session$sendCustomMessage("update-selected-from-gene-list", list(selected_nodes = list(), patientId = patient))
      #   session$sendCustomMessage("cy-subset", empty_data)  # ✅ Poslat list
      #   updatePickerInput(session, "remove_genes", choices = character(0), selected = NULL)
      #   updateTextAreaInput(session, "new_genes", value = "")
      #   
      #   message("All selections cleared.")
      # })
      
      observeEvent(input$clearSelection_btn, {
        message("🗑️ Clear selection button clicked")
        
        # 1. Reset všech reaktivních proměnných
        clear_all(TRUE)
        new_genes_var(NULL)
        remove_genes_var(NULL)
        synchronized_nodes(character(0))
        
        # 2. Poslat clear pro subset graf
        empty_data <- list(
          elements = list(nodes = list(), edges = list()),
          containerId = cy_subset_container_id,
          patientId = patient,
          clear = TRUE
        )
        
        session$sendCustomMessage("cy-subset", empty_data)
        session$sendCustomMessage("update-selected-from-gene-list", list(selected_nodes = list(), patientId = patient))
        updatePickerInput(session, "remove_genes", choices = character(0), selected = NULL)
        updateTextAreaInput(session, "new_genes", value = "")
        
        message("✅ All selections cleared")
      })
      
    observeEvent(input$selectNeighbors_btn, {
      selected_gene <- input$selected_row
      session$sendCustomMessage("select-first-neighbors", list(gene = selected_gene, patientId = patient))
    })

    observeEvent(input$fitGraph_btn, {
      selected_genes <- input$cySelectedNodes
      
      session$sendCustomMessage("fit-selected-nodes", list(
        nodes = if (length(selected_genes) > 0) selected_genes else NULL,
        patientId = patient))
    })

    #################################################
    ### Selected variant or fusion data + buttons ###
    #################################################

    observeEvent(active(), {
      req(active())
      
      som_vars <- as.data.table(shared_data$somatic.variants())
      germ_vars <- as.data.table(shared_data$germline.variants())
      fusions <- as.data.table(shared_data$fusion.variants())
      
      selected_dt(NULL)
      
      if (is.null(som_vars) && is.null(germ_vars) && is.null(fusions)) {
        selected_dt(data.table(Gene_symbol = character(0), var_name = character(0), 
                               fusion = character(0), pathway = character(0)))
        return()
      }
      
      # Použít rbindlist přímo místo postupného merge
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
      
      # Přidat pathways info efektivněji
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
    
    # Konfigurace pro každý typ varianty
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
    
    # Vytvořit observeEvent pro každý typ varianty
    lapply(names(variant_config), function(type) {
      config <- variant_config[[type]]
      
      observeEvent(list(input[[config$input_id]], input$selected_pathway, 
                        input$selected_tissue, selected_dt()), {
                          req(selected_dt())
                          
                          # Kontrola existence sloupce
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
                                callbackR = function(value) {
                                  if (!value) {
                                    # updateTabItems(session$userData$parent_session, 
                                    #                inputId = "sidebar_menu", 
                                    #                selected = config$tab)
                                  }
                                }
                              )
                            }
                            session$sendCustomMessage("variant-border", list(type = type, nodes = character(0), patientId = patient))
                            return()
                          }
                          
                          # Získat relevantní geny
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
                                callbackR = function(value) {
                                  if (!value) {
                                    # updateTabItems(session$userData$parent_session, 
                                    #                inputId = "sidebar_menu", 
                                    #                selected = config$tab)
                                  }
                                }
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

    networkGraph_tables$tab_server("tab", tissue_dt, subTissue_dt, selected_nodes = synchronized_nodes, selected_dt, patient) # tissue_dt = reactive(result_dt())

    addPopover(id = "helpPopover_addGene",options = list(title = "Write comma-separated text:",content = "example: BRCA1, TP53, FOXO3",placement = "right",trigger = "hover"))
    addPopover(id = "helpPopover_layout",options = list(title = "Layout options:",placement = "right",trigger = "hover",
                                                        content = "cola – Ideal for hierarchical structures and smaller graphs. FCOSE – Best for large and complex networks."))


  })
}


