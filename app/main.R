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
        br,updateTabsetPanel, actionButton,imageOutput,renderImage,reactiveVal,req,fixedPanel,reactiveValues],
  bs4Dash[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, sidebarMenu, menuItem, menuSubItem, dashboardControlbar,tabItems, tabItem, bs4Card,infoBox,tabBox,tabsetPanel,bs4ValueBox,
          controlbarMenu,controlbarItem,column,box,boxLabel,descriptionBlock,boxProfile,boxProfileItem,attachmentBlock,boxComment,userBlock,updateTabItems,boxDropdown,boxDropdownItem,dropdownDivider,
          navbarMenu,navbarTab],
  # plotly[plot_ly,plotlyOutput,renderPlotly,layout],
  # reactable,
  # reactable[colDef],
  htmltools[tags,p,span],
  shinyWidgets[pickerInput,prettySwitch,dropdown],
  shinyjs[useShinyjs, runjs,toggle],
  utils[str]
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

)

#####################################################

## Methylace nedělají na NGS ale na čipech = genom. pozice a k tomu naměřené intenzity. Výsledkem je report. ##

#' @export
ui <- function(id){
  ns <- NS(id)
  useShinyjs()
  tags$head(
    tags$style(HTML("
    #app-plots_tabBox_box {
      box-shadow: none !important;
      border: none !important;
    }
  "))
  )
  

  dashboardPage(
    header = dashboardHeader(
      nav = navbarMenu(
        navbarTab("Variant calling", tabName = ns("variant_calling")),
        navbarTab("Summary", tabName = ns("summary")),
        navbarTab("Fusion genes", tabName = ns("fusion_genes")),

        navbarTab("Expression profile", tabName = ns("expression_profile")),
        navbarTab("Network graph", tabName = ns("network_graph")),
        navbarTab("IGV", tabName = ns("hidden_igv"))
      )
    ),
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
                             
                             tabPanel("Germline small variant calling",tabName = ns("germline_var_call_panel"),value = "germline",
                                      do.call(tabsetPanel, c(id = ns("germline_patients"), lapply(names(set_patient_to_sample("germline")), function(sample) {
                                        tabPanel(title = sample, germline_var_call_table$ui(ns(paste0("germline_tab_", sample))))
                                      })))
                             ),
                             tabPanel("Somatic small variant calling",tabName = ns("somatic_var_call_panel"),value = "somatic",
                                      tags$style(HTML(".btn-group > .btn.active {background-color: skyblue; color: white;}
                                                 .btn-mygrey {background-color: lightgray; color: black;}
                                                ")),
                                      
                                      do.call(tabsetPanel, c(id = ns("tabset"), lapply(names(set_patient_to_sample("somatic")), function(sample) {
                                        tabPanel(title = sample, somatic_var_call_table$ui(ns(paste0("somatic_tab_", sample))))
                                      }))))
                             

 
                      )),
              tabItem(tabName = ns("fusion_genes"),
                      bs4Card(width = 12,headerBorder = FALSE, collapsible = FALSE,
                        fluidPage(
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

#' @export
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
    
## run summary module

    lapply(patients_list(), function(patient) {

      summary$server(paste0("summary_table_", patient), patient, shared_data)
      create_report$server(paste0("create_report_", patient), patient, shared_data)
    })


#################
# filter table columns dropdown button for fusion

    ## run fusion genes module
    samples_fuze <- set_patient_to_sample("fusion")
    lapply(names(samples_fuze), function(patient) {
      fusion_genes_table$server(paste0("geneFusion_tab_", patient), samples_fuze[[patient]], shared_data)
    })
# ##################
    # Run somatic varcall module

    samples_som <- set_patient_to_sample("somatic")

    lapply(names(samples_som), function(patient) {
      somatic_var_call_table$server(paste0("somatic_tab_", patient), samples_som[[patient]], shared_data)
    })

##################

    # Run germline varcall module
    samples_germ <- set_patient_to_sample("germline")

    lapply(names(samples_germ), function(patient) {
      germline_var_call_table$server(paste0("germline_tab_", patient), samples_germ[[patient]],shared_data)
    })

##################

    samples_expr <- set_patient_to_sample("expression")

    observe({
      lapply(names(samples_expr), function(patient) {
        expression_profile_table$server(paste0("genesOfinterest_tab_", patient),samples_expr[[patient]],"genes_of_interest",shared_data$expression_goi_var)
        expression_profile_table$server(paste0("allGenes_tab_", patient),samples_expr[[patient]],"all_genes",shared_data$expression_all_var)
      })
    })


# ##################    
    # run network graph module

    # networkGraph_cytoscape$server("network_graph", shared_data)
    # 
    # 
    # 
    # 
    # 
    # # # Spustíme statický server při startu celé aplikace
    # # start_static_server(dir = "/Users/katerinajuraskova/Desktop/sequiaViz/input_files/MOII_e117/primary_analysis/230426_MOII_e117_tkane/mapped")
    # # Spustíme statický server při startu celé aplikace
    # 
    # 
    # path <- get_inputs("bam_file")
    # path_combined <- file.path(getwd(), path$path_to_folder)
    # path_clean <- sub("/+$", "", path_combined)
    # 
    # start_static_server(dir = path_clean)   # paste0(getwd(),"/input_files/MOII_e117/primary_analysis/230426_MOII_e117_tkane/mapped"))
    # 
    # IGV$igv_server("igv",shared_data)
    # 
    # # Ukončení serveru při zavření celé session
    # session$onSessionEnded(function() {
    #   stop_static_server()
    # })


  })
}

# shinyApp(ui,server,options = list(launch.browser = TRUE))
