# ## install/unistall package from dependencies with commands:
# # rhino::pkg_install("shiny")
# # rhino::pkg_remove("dplyr")
#
# restart R session
# rstudioapi::restartSession()  # Pokud používáš RStudio
#
# ## when .js file added or changed, run this command:
# rhino::build_js()
# #
# ## run this from console when the css style is changed ##
# rhino::build_sass()
#
# ## run shiny app with command:
# # shiny::runApp()
# # shiny::shinyAppDir(".", options = list(launch.browser = TRUE))
#
# script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(paste0(script_dir))
# working with git - checkout to different branch
# git status
# git checkout dev (git checkout -b new_branch)
# git add .
# git commit -m "Uložení aktuálních změn do nové větve"
# git push -u origin dev

box::use(
  rhino,
  shiny[h1,h2,h3,bootstrapPage,div,moduleServer,NS,renderUI,tags,uiOutput,icon,observeEvent,observe,reactive,isTruthy,
        fluidRow,fluidPage,mainPanel,tabPanel,titlePanel,tagList,HTML,textInput,sidebarLayout,sidebarPanel,includeScript,
        br,updateTabsetPanel,imageOutput,renderImage,reactiveVal,req,fixedPanel,reactiveValues,fileInput,showNotification],
  bs4Dash[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, sidebarMenu, menuItem, menuSubItem, dashboardControlbar,tabItems, tabItem, bs4Card,infoBox,tabBox,tabsetPanel,bs4ValueBox,
          controlbarMenu,controlbarItem,column,box,boxLabel,descriptionBlock,boxProfile,boxProfileItem,attachmentBlock,boxComment,userBlock,updateTabItems,boxDropdown,boxDropdownItem,dropdownDivider,
          navbarMenu,navbarTab,actionButton],
  # plotly[plot_ly,plotlyOutput,renderPlotly,layout],
  # reactable,
  # reactable[colDef],
  htmltools[tags,p,span],
  shinyWidgets[pickerInput,prettySwitch,dropdown],
  shinyjs[useShinyjs, runjs,toggle],
  utils[str],
  waiter[spin_1],
  jsonlite[read_json,write_json],
  # fresh[create_theme,bs4dash_vars,bs4dash_yiq,bs4dash_layout,bs4dash_sidebar_light,bs4dash_status,bs4dash_color]
  # promises[future_promise,`%...!%`,`%...>%`,catch],
  # future[plan,multisession],
  # microbenchmark[microbenchmark],
  # parallel[detectCores],
  # data.table
  # openxlsx[read.xlsx]
)

box::use(
  app/view/summary,
  app/view/fusion_genes_table,
  app/view/germline_var_call_table,
  app/view/somatic_var_call_table,
  app/view/expression_profile_table,
  app/logic/patients_list[patients_list,set_patient_to_sample],
  app/view/IGV,
  app/logic/igv_helper[start_static_server,stop_static_server],
#   app/logic/load_data[load_data,get_inputs],
  app/view/networkGraph_cytoscape,
  app/logic/load_data[get_inputs],
  app/view/create_report,
  app/logic/session_utils[save_session],

)


#####################################################



## Methylace nedělají na NGS ale na čipech = genom. pozice a k tomu naměřené intenzity. Výsledkem je report. ##

ui <- function(id){
  ns <- NS(id)
  useShinyjs()
  tags$head(
    tags$style(HTML("#app-plots_tabBox_box {
                          box-shadow: none !important;
                          border: none !important;}")),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"))
  

  dashboardPage(
    dark = NULL, 
    preloader = list(html = spin_1(), color = "#333e48"),
    header = dashboardHeader(
      nav = navbarMenu(id = ns("navbarMenu"),
                       navbarTab("IGV", tabName = ns("hidden_igv")),
        # navbarTab("Variant calling", tabName = ns("variant_calling")),
        navbarTab("Fusion genes", tabName = ns("fusion_genes")),
        navbarTab("Variant calling", tabName = ns("variant_calling")),
        navbarTab("Summary", tabName = ns("summary")),
        navbarTab("Expression profile", tabName = ns("expression_profile")),
        navbarTab("Network graph", tabName = ns("network_graph"))
      ),
      rightUi = tagList(
        tags$li(class = "dropdown", actionButton(ns("save_session_btn"), label = NULL, icon = icon("save"), title = "Save session")),
        tags$li(class = "dropdown", actionButton(ns("load_session_btn"), label = NULL, icon = icon("upload"), title = "Load session")))),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(#style = "background-color: white;",
      tabItems(
        tabItem(tabName = ns("summary"),
          fluidRow(
            div(style = "display: flex; flex-wrap: wrap; width: 100%;",
              do.call(tagList, lapply(patients_list(), function(sample) {
                bs4Card(
                  title = tagList(tags$head(tags$style(HTML(".card-title {float: none !important;}")),
                                            tags$style(HTML(".card-title { font-size: 20px; }"))),
                    span(sample),
                    div(style = "float: right; margin-left: auto;",
                        create_report$ui(ns(paste0("create_report_", sample)))
                      )
                  ), icon = icon("person"), collapsible = FALSE, width = 12, 
                  
                  summary$ui(ns(paste0("summary_table_", sample)))
                )
              }))
            )
          )
        ),

        tabItem(tabName = ns("variant_calling"),
                tabBox(id = ns("variant_calling_tabs"), width = 12, collapsible = FALSE, # title = uiOutput(ns("igv_dropdown_ui"))
                       
                       tabPanel("Somatic small variant calling",tabName = ns("somatic_var_call_panel"),value = "somatic",
                                tags$style(HTML(".btn-group > .btn.active {background-color: skyblue; color: white;}
                                           .btn-mygrey {background-color: lightgray; color: black;}
                                          ")),
                                
                                do.call(tabsetPanel, c(id = ns("tabset"), lapply(names(set_patient_to_sample("somatic")), function(sample) {
                                  tabPanel(title = sample, somatic_var_call_table$ui(ns(paste0("somatic_tab_", sample))))
                                })))),
                       tabPanel("Germline small variant calling",tabName = ns("germline_var_call_panel"),value = "germline",
                                do.call(tabsetPanel, c(id = ns("germline_patients"), lapply(names(set_patient_to_sample("germline")), function(sample) {
                                  tabPanel(title = sample, germline_var_call_table$ui(ns(paste0("germline_tab_", sample))))
                                })))
                       )


                )),
        tabItem(tabName = ns("fusion_genes"),
                bs4Card(width = 12,headerBorder = FALSE, collapsible = FALSE,
                  fluidPage(
                    # tags$head(
                    #   tags$script(src = "static/js/app.min.js"),
                    #   tags$script(src = "static/js/radio-button.js")),
                    #
                    ### JavaScript kód pro zachytávání změn v radio buttonech
                    tags$script(HTML("$(document).on('change', 'input[type=\"radio\"]', function() { 
                                        const inputName = $(this).attr('name');
                                        if (inputName && inputName.includes('visual_check') && inputName.includes('-')) {
                                          const rowIndex = parseInt($(this).closest('.fusion-radio-group').data('row'));
                                          const selectedValue = $(this).val();
                                          const namespaceId = inputName.replace(/_\\d+$/, '') + '_changed';
                                          Shiny.setInputValue(namespaceId, {
                                            row: rowIndex,
                                            value: selectedValue,
                                            timestamp: new Date().getTime()
                                          });}});")),
                    do.call(tabsetPanel, c(id = ns("fusion_genes_patients"), lapply(names(set_patient_to_sample("fusion")), function(sample) {
                      tabPanel(title = sample, fusion_genes_table$ui(ns(paste0("geneFusion_tab_", sample))))
                    })))
                  ))
                ),
        tabItem(tabName = ns("expression_profile"),
                bs4Card(width = 12,headerBorder = FALSE, collapsible = FALSE,
                  fluidPage(
                    do.call(tabsetPanel, c(id = ns("expression_profile_patients"),
                         lapply(names(set_patient_to_sample("expression")), function(patient) {
                           tabPanel(title = patient,
                              div(style = "margin-left: -7px;",
                                tabBox(id = ns(paste0("expression_profile_tabs_", patient)), width = 12, collapsible = FALSE,
                                 
                                       tabPanel("Genes of Interest",
                                                tabName = ns("genesOfinterest_panel"), value = "genesOfinterest",
                                                expression_profile_table$ui(ns(paste0("genesOfinterest_tab_", patient))))
                                       ,
                                       tabPanel("All Genes",
                                                tabName = ns("allGenes_panel"), value = "allGenes",
                                                expression_profile_table$ui(ns(paste0("allGenes_tab_", patient)))))
                             ))})))))),
        tabItem(h1("Gene Interactions Network"),tabName = ns("network_graph"),
                bs4Card(width = 12,headerBorder = FALSE, collapsible = FALSE,
                  fluidPage(
                    networkGraph_cytoscape$ui(ns("network_graph"))
                    ))
        ),
        tabItem(tabName = ns("hidden_igv"),
                tags$style(HTML("
                        #igv-igvDiv {
                          width: 100%;
                          height: auto;
                          border: none;
                          margin: 0 auto;
                          padding: 20px;
                          box-sizing: border-box;
                        }
                    ")),
                box(id = ns("igv_page"), title = "IGV Viewer",width = 12, collapsible = FALSE,
                    IGV$igv_ui(ns("igv"))
                )
        )
      )
    )
   )
}


server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shared_data <- reactiveValues(germline_var = reactiveVal(NULL), 
                                  fusion_var = reactiveVal(NULL), 
                                  somatic_var = reactiveVal(NULL),
                                  expression_goi_var = reactiveVal(NULL), #genes of interest
                                  expression_all_var = reactiveVal(NULL), # all genes
                                  germline_bam = reactiveVal(NULL), 
                                  fusion_bam = reactiveVal(NULL), 
                                  somatic_bam = reactiveVal(NULL),
                                  germline_overview = list(), 
                                  fusion_overview = list(),
                                  navigation_context = reactiveVal(NULL))     # somatic or germline or fusion     # from where are we opening IGV
    
    load_btn <- reactiveValues(somatic = FALSE,
                               germline = FALSE,
                               fusion = FALSE,
                               expression_goi = FALSE,
                               expression_all = FALSE,
                               summary = FALSE)
    
    observe({
      session$sendCustomMessage("initRadioSync", list())
    })
    
## run summary module

    lapply(patients_list(), function(patient) {
      summary$server(paste0("summary_table_", patient), patient, shared_data)
      create_report$server(paste0("create_report_", patient), patient, shared_data)
    })
#################
    ## Run fusion genes module
    samples_fuze <- set_patient_to_sample("fusion")
    fusion_module <- lapply(names(samples_fuze), function(patient) {
      fusion_genes_table$server(paste0("geneFusion_tab_", patient), samples_fuze[[patient]], shared_data, reactive({ input$load_session_btn }))
    })
    names(fusion_module) <- names(samples_fuze)
# ##################
    # Run somatic varcall module

    samples_som <- set_patient_to_sample("somatic")

    som_module <- lapply(names(samples_som), function(patient) {
      somatic_var_call_table$server(paste0("somatic_tab_", patient), samples_som[[patient]], shared_data)
    })
    names(som_module) <- names(samples_som)

# ##################

    # Run germline varcall module
    samples_germ <- set_patient_to_sample("germline")

    germ_module <- lapply(names(samples_germ), function(patient) {
      germline_var_call_table$server(paste0("germline_tab_", patient), samples_germ[[patient]],shared_data)
    })
    names(germ_module) <- names(samples_germ)

##################

    samples_expr <- set_patient_to_sample("expression")

    expression_module_goi <- lapply(names(samples_expr), function(patient) {
      expression_profile_table$server(paste0("genesOfinterest_tab_", patient),samples_expr[[patient]],"genes_of_interest",shared_data$expression_goi_var)
    })
    expression_module_all <- lapply(names(samples_expr), function(patient) {
      expression_profile_table$server(paste0("allGenes_tab_", patient),samples_expr[[patient]],"all_genes",shared_data$expression_all_var)
    })
    names(expression_module_goi) <- names(samples_expr)
    names(expression_module_all) <- names(samples_expr)
# ##################
    # run network graph module
# 
#     networkGraph_cytoscape$server("network_graph", shared_data)
# 
# 
# 
# 
# 
#     # # Spustíme statický server při startu celé aplikace
#     # start_static_server(dir = "/Users/katerinajuraskova/Desktop/sequiaViz/input_files/MOII_e117/primary_analysis/230426_MOII_e117_tkane/mapped")
#     # Spustíme statický server při startu celé aplikace
# 
#     path <- get_inputs("bam_file")
#     path_combined <- file.path(getwd(), path$path_to_folder)
#     path_clean <- sub("/+$", "", path_combined)
# 
#     start_static_server(dir = path_clean)   # paste0(getwd(),"/input_files/MOII_e117/primary_analysis/230426_MOII_e117_tkane/mapped"))
# 
#     IGV$igv_server("igv",shared_data)
# 
#     # Ukončení serveru při zavření celé session
#     session$onSessionEnded(function() {
#       stop_static_server()
#     })
# 
#     ###################################
#     ## save and restore user session ##
#     ###################################
#     
#     observeEvent(input$save_session_btn, {
#       save_session(file = "session_data.json", patient_modules = all_modules)
#     })
#     # session$onSessionEnded(function() { ## when session ends, save it automatically 
#     #   save_session(file = "session_data.json", patient_modules = all_modules)
#     # }
#     
#     
#     all_modules <- list(
#       somatic = som_module,
#       germline = germ_module,
#       fusion = fusion_module,
#       expression_goi = expression_module_goi,
#       expression_all = expression_module_all
#     )
# 
#     
#     observeEvent(input$load_session_btn,{
#       print("Load_session_btn was clicked. Setting all load_btn values as TRUE.")
#       load_btn$somatic = TRUE
#       load_btn$germline = TRUE
#       load_btn$fusion = TRUE
#       load_btn$expression_goi = TRUE
#       load_btn$expression_all = TRUE
#       load_btn$summary = TRUE
#     })
#     
#     observe({
#       session_data <- read_json("session_data.json", simplifyVector = TRUE)
#       #### === SOMATIC DEFERRED RESTORE ===
#       if (isTRUE(load_btn$somatic) && input$navbarMenu == "app-variant_calling" && input$variant_calling_tabs == "somatic") {
# 
#         for (patient in names(all_modules$somatic)) {
#           mod <- all_modules$somatic[[patient]]
#           mod$restore_session_data(session_data$somatic[[patient]])
#           mod$filter_state$restore_ui_inputs(session_data$somatic[[patient]])
#           print(paste("Restored UI inputs for patient:", patient))
#         }
#         load_btn$somatic <- FALSE
#         print("Deferred restore for somatic completed and flag reset.")
#       }
#       #### === GERMLINE DEFERRED RESTORE ===
#       if (isTRUE(load_btn$germline) && input$navbarMenu == "app-variant_calling" && input$variant_calling_tabs == "germline") {
# 
#         for (patient in names(all_modules$germline)) {
#           mod <- all_modules$germline[[patient]]
#           mod$restore_session_data(session_data$germline[[patient]])
#           mod$filter_state$restore_ui_inputs(session_data$germline[[patient]])
#         }
#         load_btn$germline <- FALSE
#         print("Deferred restore for GERMLINE completed and flag reset.")
#       }
#       #### === FUSION DEFERRED RESTORE ===
#       if (isTRUE(load_btn$fusion) &&
#           input$navbarMenu == "app-fusion_genes") {
# 
#         for (patient in names(all_modules$fusion)) {
#           mod <- all_modules$fusion[[patient]]
#           mod$restore_session_data(session_data$fusion[[patient]])
#           mod$filter_state$restore_ui_inputs(session_data$fusion[[patient]])
#         }
#         load_btn$fusion <- FALSE
#         print("Deferred restore for FUSION completed and flag reset.")
#       }
#       #### === EXPRESSION DEFERRED RESTORE ===
#       if (isTRUE(load_btn$expression_goi) &&
#           input$navbarMenu == "app-expression_profile") {
# 
#         for (patient in names(all_modules$expression_goi)) {
#           mod <- all_modules$expression_goi[[patient]]
#           mod$restore_session_data(session_data$expression_goi[[patient]])
#           mod$filter_state$restore_ui_inputs(session_data$expression_goi[[patient]])
#         }
#         load_btn$expression_goi <- FALSE
#         print("Deferred restore for EXPRESSION GOI completed and flag reset.")
#       }
#       if (isTRUE(load_btn$expression_all) &&
#           input$navbarMenu == "app-expression_profile") {
# 
#         for (patient in names(all_modules$expression_all)) {
#           mod <- all_modules$expression_all[[patient]]
#           mod$restore_session_data(session_data$expression_all[[patient]])
#           mod$filter_state$restore_ui_inputs(session_data$expression_all[[patient]])
#         }
#         load_btn$expression <- FALSE
#         print("Deferred restore for EXPRESSION ALL completed and flag reset.")
#       }
#       #### === SUMMARY DEFERRED RESTORE ===
#       if (isTRUE(load_btn$summary) && input$navbarMenu == "app-summary") {
# 
#         if (!is.null(session_data$somatic)) {
#           somatic_selected_vars <- lapply(session_data$somatic, function(p) p$selected_vars)
#           combined_somatic_vars <- do.call(rbind, somatic_selected_vars)
#           shared_data$somatic_var(combined_somatic_vars)
#         }
# 
#         if (!is.null(session_data$germline)) {
#           germline_selected_vars <- lapply(session_data$germline, function(p) p$selected_vars)
#           combined_germline_vars <- do.call(rbind, germline_selected_vars)
#           shared_data$germline_var(combined_germline_vars)
#         }
# 
#         if (!is.null(session_data$fusion)) {
#           fusion_selected_vars <- lapply(session_data$fusion, function(p) p$selected_vars)
#           combined_fusion_vars <- do.call(rbind, fusion_selected_vars)
#           shared_data$fusion_var(combined_fusion_vars)
#         }
# 
#         if (!is.null(session_data$expression_goi)) {
#           expression_goi_vars <- lapply(session_data$expression_goi, function(p) p$selected_genes)
#           combined_goi_vars <- do.call(rbind, expression_goi_vars)
#           shared_data$expression_goi_var(combined_goi_vars)
#         }
# 
#         if (!is.null(session_data$expression_all)) {
#           expression_all_vars <- lapply(session_data$expression_all, function(p) p$selected_genes)
#           combined_all_vars <- do.call(rbind, expression_all_vars)
#           shared_data$expression_all_var(combined_all_vars)
#         }
# 
#         load_btn$summary <- FALSE
#         print("Deferred restore for SUMMARY completed and flag reset.")
#       }
#     })
#     
#    
  })
}


# shinyApp(ui,server,options = list(launch.browser = TRUE))
