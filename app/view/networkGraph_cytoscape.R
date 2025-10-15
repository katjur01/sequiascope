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
)

box::use(
  app/logic/load_data[load_data],
  app/logic/waiters[use_spinner],
  app/logic/networkGraph_helper[get_string_interactions,prepare_cytoscape_network,get_pathway_list],
  app/view/networkGraph_tables,

)

ui <- function(id, tissue_list) {
  ns <- NS(id)
  useShinyjs()
  tagList(
    tags$head(
      tags$script(src = "static/js/app.min.js"),
      tags$script(src = "static/js/cytoscape_init.js"),
      tags$script(HTML(sprintf("var cyContainerId = '%s'; var cySubsetContainerId = '%s'; var cySelectedNodesInputId = '%s';", 
                               ns("cyContainer"), ns("cySubsetContainer"), ns("cySelectedNodes"))))
    ),
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

server <- function(id, patient, shared_data, patient_files, file_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    interactions <- reactiveVal(NULL)
    sub_interactions <- reactiveVal(NULL)
    synchronized_nodes <- reactiveVal(character(0))
    new_genes_var <- reactiveVal(NULL)
    remove_genes_var <- reactiveVal(NULL)
    clear_all <- reactiveVal(FALSE)
    # result_dt <- reactiveVal(NULL)
    selected_dt <- reactiveVal(NULL)
    
    # Load and process data table
    dt <- reactive({
      message("Loading input data for network graph: ", patient_files)
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
      req(input$selected_tissue)
      req(dt())
      unique(dt()[tissue == input$selected_tissue])
    })

    pathway_dt <- reactive({
      req(input$selected_pathway)
      req(dt())
      unique(dt()[grepl(input$selected_pathway, pathway,fixed = TRUE),-c("all_kegg_gene_names","counts_tpm_round","size","mu","lower_than_p","higher_than_p","type","gene_definition")])
    })

    tissue_dt <- reactive({
      req(input$selected_tissue)
      req(pathway_dt())
      unique(pathway_dt()[tissue == input$selected_tissue])
    })


    observe({      # Fetch STRING interactions for the current tissue
      req(tissue_dt())
      message("Fetching STRING interactions")
      interactions(get_string_interactions(tissue_dt()[, feature_name]))
    })

    network_json <- reactive({      # Prepare the Cytoscape network using the fetched interactions and log2FC values
      req(tissue_dt(),interactions())
      prepare_cytoscape_network(interactions(), unique(tissue_dt()[, .(feature_name,log2FC)]))
    })

    output$js_namespace <- renderUI({    # Předat jmenný prostor do JavaScriptu - umožní  používat ns v JS
      tags$script(HTML(sprintf("var ns = '%s';", ns(""))))
    })

    ##################################
    ## Network node synchronization ##
    ##################################

    sync_nodes <- function(nodes_from_graph, current_genes, add_genes = NULL, remove_genes = NULL, clear_all) {

      ifelse(clear_all, combined <- character(0), combined <- unique(c(nodes_from_graph, current_genes)))

      if (!is.null(add_genes) && length(add_genes) > 0) combined <- unique(c(combined, add_genes))  # Přidání nových genů
      if (!is.null(remove_genes) && length(remove_genes) > 0) combined <- setdiff(combined, remove_genes)  # Odebrání genů

      combined <- combined[!is.na(combined) & combined != ""]  # Odstranění prázdných hodnot
      changed <- !setequal(synchronized_nodes(), combined)  # Kontrola změny

      return(list(updated_nodes = combined, changed = changed))  # Návrat aktualizovaných uzlů a informace o změně
    }


    ########################
    ## Network UI buttons ##
    ########################

    observeEvent(list(input$selected_pathway, input$selected_tissue), {
      req(input$selected_pathway, input$selected_tissue)
      message("Selected pathway: ", input$selected_pathway, ", Selected tissue: ", input$selected_tissue)
      session$sendCustomMessage("cy-init", network_json())
    })

    observe({      # Fetch STRING interactions for the current tissue
      req(subTissue_dt())
      message("Fetching STRING interactions")
      sub_interactions(get_string_interactions(unique(subTissue_dt()[feature_name %in% synchronized_nodes(),feature_name])))
    })

    observe({
      current_nodes <- synchronized_nodes()
      message("current_nodes v cy-subset eventu: ", paste(current_nodes, collapse = ", "))

      if (length(current_nodes) == 0) {
        message("Žádné uzly nejsou vybrány. Odesílám prázdný podgraf.")
        empty_json <- toJSON(list(elements = list(nodes = list(), edges = list())), auto_unbox = TRUE)
        session$sendCustomMessage("cy-subset", empty_json)
        session$sendCustomMessage("update-selected-from-gene-list", list(selected_nodes = list()))
      } else {
        message("Aktualizace podgrafu pro uzly: ", paste(current_nodes, collapse = ", "))
        subnetwork_json <- prepare_cytoscape_network(sub_interactions(), unique(subTissue_dt()[feature_name %in% current_nodes, .(feature_name,log2FC)]), current_nodes)
        session$sendCustomMessage("cy-subset", subnetwork_json)
        session$sendCustomMessage("update-selected-from-gene-list", list(selected_nodes = current_nodes))
      }
      message("# konec cy-subset eventu.")
    })


    observeEvent(list(input$cySelectedNodes, input$confirm_new_genes_btn, input$confirm_remove_genes_btn, input$clearSelection_btn, new_genes_var(),remove_genes_var()), {

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

      if (changed) {
        synchronized_nodes(updated_nodes) # Synchronizace uzlů
        message("Synchronizované uzly byly aktualizovány: ", paste(updated_nodes, collapse = ", "))
      } else {
        message("Uzly se nezměnily, žádná aktualizace není potřeba.")
      }

      # Reset vstupů pro přidání a odebrání genů
      updateTextAreaInput(session, "new_genes", value = "")
      updatePickerInput(session, "remove_genes", choices = synchronized_nodes(), selected = NULL)

      if (clear_all()) {
        if (length(updated_nodes) == 0 && length(input$cySelectedNodes) == 0 && length(synchronized_nodes()) == 0 ) {
          clear_all(FALSE)
          message("Clear selection completed. Resetting clear_all to FALSE.")
        }
      }

      message("# konec hlavního observeEventu.")
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

    ########################
    #### Network buttons ###
    ########################

    observeEvent(input$selected_layout, {
      session$sendCustomMessage("cy-layout",input$selected_layout)
    })

    observeEvent(input$clearSelection_btn, {
      # Nastavíme proměnné na výchozí hodnoty
      clear_all(TRUE)
      new_genes_var(NULL)
      remove_genes_var(NULL)
      synchronized_nodes(character(0))  # Jasně nastaví stav synchronizovaných uzlů

      # Explicitní aktualizace UI komponent
      session$sendCustomMessage("update-selected-from-gene-list", list(selected_nodes = list()))
      session$sendCustomMessage("cy-subset", toJSON(list(elements = list(nodes = list(), edges = list())), auto_unbox = TRUE))
      updatePickerInput(session, "remove_genes", choices = character(0), selected = NULL)
      updateTextAreaInput(session, "new_genes", value = "")

      message("All selections cleared.")
    })

    observeEvent(input$selectNeighbors_btn, {
      selected_gene <- input$selected_row
      session$sendCustomMessage("select-first-neighbors", list(gene = selected_gene))
    })

    observeEvent(input$fitGraph_btn, {
      selected_genes <- input$cySelectedNodes

      if (!is.null(selected_genes) && length(selected_genes) > 0) {
        message("Fitting view to selected nodes: ", paste(selected_genes, collapse = ", "))
        session$sendCustomMessage("fit-selected-nodes", list(nodes = selected_genes))
      } else {
        message("No nodes selected, fitting view to all nodes.")
        session$sendCustomMessage("fit-selected-nodes", list(nodes = NULL)) # NULL znamená vycentrování na celý graf
      }
    })

    #################################################
    ### Selected variant or fusion data + buttons ###
    #################################################
    observe({
      som_vars <- as.data.table(shared_data$somatic.variants())
      germ_vars <- as.data.table(shared_data$germline.variants())
      fusions <- as.data.table(shared_data$fusion.variants())
      tissue_table <- copy(tissue_dt())  # Výchozí tabulka

      # Výchozí hodnota pro selected_dt
      selected_dt(NULL)
      #
      # if ((is.null(som_vars)  || nrow(som_vars) == 0) &&
      #     (is.null(germ_vars) || nrow(germ_vars) == 0) &&
      #     (is.null(fusions)   || nrow(fusions) == 0)) {
      #   message("No somatic, germline or fusion variant selected.")
      #   result_dt(tissue_table)
      # } else {
      #   # Přidání somatic variant
      #   if (!is.null(som_vars) && nrow(som_vars) > 0) {
      #     selected_som_variants <- som_vars[, .(Gene_symbol, variant = var_name)]
      #     selected_som_variants <- unique(selected_som_variants, by = "Gene_symbol")
      #     tissue_table <- merge(tissue_table, selected_som_variants, by.x = "feature_name", by.y = "Gene_symbol", all.x = TRUE)
      #   }
      #
      #   # Přidání germline variant
      #   if (!is.null(germ_vars) && nrow(germ_vars) > 0) {
      #     selected_germ_variants <- germ_vars[, .(Gene_symbol, variant = var_name)]
      #     selected_germ_variants <- unique(selected_germ_variants, by = "Gene_symbol")
      #     tissue_table <- merge(tissue_table, selected_germ_variants, by.x = "feature_name", by.y = "Gene_symbol", all.x = TRUE)
      #   }
      #
      #   # Přidání fúzí
      #   if (!is.null(fusions) && nrow(fusions) > 0) {
      #     fusions <- fusions[, .(Gene_symbol = c(gene1, gene2), fusion = paste(paste(gene1, gene2, sep = "-"), collapse = ", "))]
      #     fusions <- unique(fusions, by = "Gene_symbol")
      #     tissue_table <- merge(tissue_table, fusions, by.x = "feature_name", by.y = "Gene_symbol", all.x = TRUE)
      #   }
      #
      #   result_dt(tissue_table)
      #   message("Updated result_dt with somatic, germline and/or fusion variants.")
      # }

      # Vytvoření selected_dt
      if ((!is.null(som_vars) && nrow(som_vars) > 0) || (!is.null(germ_vars) && nrow(germ_vars) > 0) || (!is.null(fusions) && nrow(fusions) > 0)) {
        selected_som_variants <- if (!is.null(som_vars) && nrow(som_vars) > 0) {
          unique(som_vars[, var_name := "somatic"])
        } else {
          data.frame(Gene_symbol = character(0))
        }

        selected_germ_variants <- if (!is.null(germ_vars) && nrow(germ_vars) > 0) {
          unique(germ_vars[, var_name := "germline"])
        } else {
          data.frame(Gene_symbol = character(0))
        }

        selected_fusions <- if (!is.null(fusions) && nrow(fusions) > 0) {
          fusions <- unique(fusions[, .(Gene_symbol = c(gene1, gene2), fusion = "yes")])
          # unique(fusions[, fusion := "yes"])
        } else {
          data.frame(Gene_symbol = character(0))
        }
        combined_variants <- unique(rbindlist(list(as.data.table(selected_som_variants)[, .(Gene_symbol, var_name = "somatic")],as.data.table(selected_germ_variants)[, .(Gene_symbol, var_name = "germline")])))
        combined_selected <- merge(combined_variants, selected_fusions, by = "Gene_symbol", all = TRUE)
        pathways_info <- subTissue_dt()[feature_name %in% combined_selected$Gene_symbol, .(Gene_symbol = feature_name, pathway)]
        combined_selected <- merge(combined_selected, pathways_info, by = "Gene_symbol", all.x = TRUE)
        setDF(combined_selected)

        selected_dt(combined_selected)
      } else {
        selected_dt(data.frame(Gene_symbol = character(0), variant = character(0), fusion = character(0), pathway = character(0)))
      }
    })



    observeEvent(list(input$selectedSomVariants, input$selectedGermVariants, input$selectedFusions, input$selected_pathway, input$selected_tissue), {
  
      if (isTruthy(input$selectedSomVariants)) {
        if ("var_name" %in% names(selected_dt())) {
          selected_dt <- as.data.table(selected_dt())
          somatic_nodes <- as.character(unique(selected_dt[var_name == "somatic", Gene_symbol]))

          print(somatic_nodes)
          if (length(somatic_nodes) == 0) {
            updatePrettySwitch(session, "selectedSomVariants", value = FALSE) # Reset prettySwitch na FALSE

            shinyalert(
              title = "No variants selected",
              text = "You don't have any somatic variants selected.",
              type = "warning",
              showCancelButton = TRUE,
              confirmButtonText = "OK",
              cancelButtonText = "Go to variant",
              callbackR = function(value) {
                # value bude TRUE pro OK, FALSE pro "Go to variant"
                if (!value) {
                  # updateTabItems(session = session$userData$parent_session,  # použijeme parent session
                  #                inputId = "sidebar_menu",  # bez namespace
                  #                selected = "fusion_genes")
                }})
          } else {
            message("Adding border for somatic variant nodes:", paste(somatic_nodes, collapse = ", "))
            session$sendCustomMessage("variant-border", list(type = "somatic", nodes = as.list(somatic_nodes)))
          }
        } else {
          # Reset prettySwitch na FALSE
          updatePrettySwitch(session, "selectedSomVariants", value = FALSE)

          shinyalert(
            title = "No variants selected",
            text = "No somatic variants are currently selected as possibly oncogenic.",
            type = "warning",
            showCancelButton = TRUE,
            confirmButtonText = "OK",
            cancelButtonText = "Go to variant",
            callbackR = function(value) {
              if (!value) {}})
        }

      } else {
        message("Removing border for somatic variant nodes.")
        session$sendCustomMessage("variant-border", list(type = "somatic", nodes = character(0)))
      }


      # Aktualizace pro germline varianty
      if (isTruthy(input$selectedGermVariants)) {
        if ("var_name" %in% names(selected_dt())) {
          selected_dt <- as.data.table(selected_dt())
          germline_nodes <- as.character(unique(selected_dt[var_name == "germline", Gene_symbol]))

          print(germline_nodes)
          if (length(germline_nodes) == 0) {
            updatePrettySwitch(session, "selectedGermVariants", value = FALSE) # Reset prettySwitch na FALSE

            shinyalert(
              title = "No variants selected",
              text = "You don't have any variants selected.",
              type = "warning",
              showCancelButton = TRUE,
              confirmButtonText = "OK",
              cancelButtonText = "Go to variant",
              callbackR = function(value) {
                # value bude TRUE pro OK, FALSE pro "Go to variant"
                if (!value) {
                  # updateTabItems(session = session$userData$parent_session,  # použijeme parent session
                  #                inputId = "sidebar_menu",  # bez namespace
                  #                selected = "fusion_genes")
                }})
          } else {
            message("Adding border for germline variant nodes:", paste(germline_nodes, collapse = ", "))
            session$sendCustomMessage("variant-border", list(type = "germline", nodes = as.list(germline_nodes)))
          }
        } else {
          # Reset prettySwitch na FALSE
          updatePrettySwitch(session, "selectedGermVariants", value = FALSE)

          shinyalert(
            title = "No variants selected",
            text = "No germline variants are currently selected as possibly pathogenic.",
            type = "warning",
            showCancelButton = TRUE,
            confirmButtonText = "OK",
            cancelButtonText = "Go to variant",
            callbackR = function(value) {
              if (!value) {}})
        }

      } else {
        message("Removing border for germline variant nodes.")
        session$sendCustomMessage("variant-border", list(type = "germline", nodes = character(0)))
      }

      # Aktualizace pro fúze
      if (isTruthy(input$selectedFusions)) {

        # Kontrola existence sloupce fusion
        if ("fusion" %in% names(selected_dt())) {
          # fusion_nodes <- selected_dt()[!is.na("fusion"), "Gene_symbol"]
          selected_dt <- as.data.table(selected_dt())
          fusion_nodes <- as.character(unique(selected_dt[!is.na(fusion), Gene_symbol]))

          if (length(fusion_nodes) == 0) {
            updatePrettySwitch(session, "selectedFusions", value = FALSE)
            shinyalert(
              title = "No fusions selected",
              text = "You don't have any fusions selected.",
              type = "warning",
              showCancelButton = TRUE,
              confirmButtonText = "OK",
              cancelButtonText = "Go to fusion",
              callbackR = function(value) {
                if (!value) {}}
            )
          } else {
            message("Adding border for fusion nodes:", paste(fusion_nodes, collapse = ", "))
            session$sendCustomMessage("variant-border", list(type = "fusion", nodes = as.list(fusion_nodes)))
            print(selected_dt())
          }
        } else {
          updatePrettySwitch(session, "selectedFusions", value = FALSE)
          shinyalert(
            title = "No fusions selected",
            text = "No fusion genes are currently selected as possibly pathogenic.",
            type = "warning",
            showCancelButton = TRUE,
            confirmButtonText = "OK",
            cancelButtonText = "Go to fusion",
            callbackR = function(value) {
              if (!value) {}})
        }
      } else {
        message("Removing border for fusion nodes.")
        session$sendCustomMessage("variant-border", list(type = "fusion", nodes = character(0)))
      }
    })

    ##############
    ### others ###
    ##############

    networkGraph_tables$tab_server("tab", tissue_dt, subTissue_dt, selected_nodes = synchronized_nodes, selected_dt) # tissue_dt = reactive(result_dt())

    addPopover(id = "helpPopover_addGene",options = list(title = "Write comma-separated text:",content = "example: BRCA1, TP53, FOXO3",placement = "right",trigger = "hover"))
    addPopover(id = "helpPopover_layout",options = list(title = "Layout options:",placement = "right",trigger = "hover",
                                                        content = "cola – Ideal for hierarchical structures and smaller graphs. FCOSE – Best for large and complex networks."))


  })
}


