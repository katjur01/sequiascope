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
  shiny[h1,h2,h3,bootstrapPage,div,moduleServer,NS,renderUI,tags,uiOutput,icon,observeEvent,observe,reactive,isTruthy,onFlushed,appendTab,removeTab,withProgress,
        fluidRow,fluidPage,mainPanel,tabPanel,titlePanel,tagList,HTML,textInput,sidebarLayout,sidebarPanel,includeScript,invalidateLater,isolate,
        br,updateTabsetPanel,imageOutput,renderImage,reactiveVal,req,fixedPanel,reactiveValues,fileInput,showNotification],
  bs4Dash[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, sidebarMenu, menuItem, menuSubItem, dashboardControlbar,tabItems, tabItem, bs4Card,infoBox,tabBox,tabsetPanel,bs4ValueBox,
          controlbarMenu,controlbarItem,column,box,boxLabel,descriptionBlock,boxProfile,boxProfileItem,attachmentBlock,boxComment,userBlock,updateTabItems,boxDropdown,boxDropdownItem,dropdownDivider,
          navbarMenu,navbarTab,actionButton,updateNavbarTabs, insertTab],
  # plotly[plot_ly,plotlyOutput,renderPlotly,layout],
  # reactable,
  # reactable[colDef],
  htmltools[tags,p,span],
  shinyWidgets[pickerInput,prettySwitch,dropdown],
  shinyjs[useShinyjs, runjs,toggle,hide,show],
  utils[str],
  waiter[spin_1],
  # jsonlite[read_json,write_json],
  # fresh[create_theme,bs4dash_vars,bs4dash_yiq,bs4dash_layout,bs4dash_sidebar_light,bs4dash_status,bs4dash_color]s
  # promises[future_promise,`%...!%`,`%...>%`,catch],
  # future[plan,multisession],
  # microbenchmark[microbenchmark],
  # parallel[detectCores],
  data.table[data.table],
  shinyalert[shinyalert],
  future[future, value, resolved, plan, multicore, multisession],
  promises[then, catch]
  # openxlsx[read.xlsx]
)

  plan(multisession, workers = 2)
  options(future.fork.enable = FALSE)


box::use(
  app/view/upload_data,
  app/view/summary,
  app/view/fusion_genes_table,
  app/view/germline_var_call_table,
  app/view/somatic_var_call_table,
  app/view/expression_profile_table,
  app/view/IGV,
  app/logic/helper_igv[start_static_server,stop_static_server],
  app/view/networkGraph_cytoscape,
  app/logic/session_utils[load_session, save_session, cleanup_old_sessions, create_session_cache],
  app/logic/prerun_fusion[fusion_patients_to_prerun,prerun_fusion_data, get_fusion_prerun_status],
  app/logic/helper_main[get_patients, get_files_by_patient, add_dataset_tabs, add_summary_boxes],
)

#####################################################


## Methylace nedělají na NGS ale na čipech = genom. pozice a k tomu naměřené intenzity. Výsledkem je report. ##

ui <- function(id){
  ns <- NS(id)
  useShinyjs()
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
    tags$style(HTML("#app-plots_tabBox_box {box-shadow: none !important; border: none !important;}")))
    
  dashboardPage(
    dark = NULL, 
    preloader = list(html = spin_1(), color = "#333e48"),
    header = dashboardHeader(
      nav = navbarMenu(id = ns("navbarMenu"),
        navbarTab("Upload data", tabName = ns("upload_data")),
        # navbarTab("Variant calling", tabName = ns("variant_calling")),
        navbarTab("Summary", tabName = ns("summary")),
        navbarTab("Fusion genes", tabName = ns("fusion_genes")),

        navbarTab("Variant calling", tabName = ns("variant_calling")),

        navbarTab("Expression profile", tabName = ns("expression_profile")),
        navbarTab("IGV", tabName = ns("hidden_igv")),
        navbarTab("Network graph", tabName = ns("network_graph"))
      ),
      rightUi = tagList(
        tags$li(class = "dropdown", actionButton(ns("save_session_btn"), label = NULL, icon = icon("save"), title = "Save session",class = "session-btn")))),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(#style = "background-color: white;",
      tabItems(
        tabItem(tabName = ns("upload_data"),
          fluidPage(
            # tags$head(
              # # look & feel pro zamknuté záložky
              # tags$style(HTML(".app-tab-locked {
              #                   pointer-events: none;
              #                   opacity: 0.5; cursor: not-allowed !important;}")),
              # # robustní lock/unlock + click guard
              # tags$script(HTML("(function(){
              #                     var menuLocked = true;
              #                     var allowedValue = null;
              # 
              #                     Shiny.addCustomMessageHandler('lockMenuExcept', function(msg){
              #                       // msg: { lock: bool, allowValue: 'ns(tabName)', menuId: 'ns(navbarMenu)' }
              #                       menuLocked = !!msg.lock;
              #                       allowedValue = msg.allowValue;
              # 
              #                       // Zamkni/odemkni všechny odkazy s data-value (funguje pro nav-link i dropdown-item)
              #                       var anchors = document.querySelectorAll('a.nav-link[data-value], a.dropdown-item[data-value]');
              #                       anchors.forEach(function(a){
              #                         var v = a.getAttribute('data-value');
              #                         if (!v) return;
              #                         if (menuLocked && v !== allowedValue) {
              #                           a.classList.add('app-tab-locked');
              #                           a.setAttribute('aria-disabled', 'true');
              #                           a.setAttribute('tabindex', '-1');
              #                         } else {
              #                           a.classList.remove('app-tab-locked');
              #                           a.removeAttribute('aria-disabled');
              #                           a.removeAttribute('tabindex');
              #                         }
              #                       });
              #                     });
              # 
              #                     // Tvrdý click guard i když by třída selhala
              #                     document.addEventListener('click', function(e){
              #                       var t = e.target.closest('a.nav-link[data-value], a.dropdown-item[data-value]');
              #                       if (!t) return;
              #                       if (menuLocked) {
              #                         var v = t.getAttribute('data-value');
              #                         if (v && v !== allowedValue) {
              #                           e.preventDefault();
              #                           e.stopImmediatePropagation();
              #                           return false;
              #                         }
              #                       }
              #                     }, true); // capture fáze pro jistotu
              #                   })();"))),
            upload_data$ui(ns("upload_data_table")))
        ),
        tabItem(tabName = ns("summary"),
          fluidRow(
            div(style = "display: flex; flex-wrap: wrap; width: 100%;",
                uiOutput(ns("summary_table")))
        )),
        tabItem(tabName = ns("variant_calling"),
              tabBox(id = ns("variant_calling_tabs"), width = 12, collapsible = FALSE,
                     tabPanel("Somatic small variant calling",tabName = ns("somatic_var_call_panel"),value = "somatic",
                          div(class = "patient-tabs",
                            tabsetPanel(id = ns("somatic_tabset")))),
                     tabPanel("Germline small variant calling",tabName = ns("germline_var_call_panel"),value = "germline",
                              div(class = "patient-tabs",
                                  tabsetPanel(id = ns("germline_tabset"))))
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
                    div(class = "patient-tabs",
                        tabsetPanel(id = ns("fusion_tabset"))),
                  ))
                ),
        tabItem(tabName = ns("expression_profile"),
                bs4Card(width = 12,headerBorder = FALSE, collapsible = FALSE,
                  fluidPage(
                    tags$style(HTML(".btn-group > .btn.active {background-color: skyblue; color: white;}
                                     .btn-mygrey {background-color: lightgray; color: black;}")),
                    div(class = "patient-tabs", style = "box-shadow: none !important;",
                        tabsetPanel(id = ns("expression_tabset")))
                    ))),
        tabItem(tabName = ns("network_graph"),
                bs4Card(width = 12,headerBorder = FALSE, collapsible = FALSE,
                  fluidPage(
                    div(class = "patient-tabs",
                        tabsetPanel(id = ns("network_graph"))),
                    # uiOutput(ns("network_graph"))
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
    session$userData$parent_session <- session  # for going to different navbarMenu from other modules
    message("[future plan] ", paste(class(future::plan()), collapse = " / "))

    
    shared_data <- reactiveValues(
      somatic.variants = reactiveVal(NULL),
      somatic.patients  = reactiveVal(character(0)),
      somatic.patients.igv = reactiveVal(NULL),
      somatic.bam = reactiveVal(NULL),
      somatic.overview = list(),
      germline.variants = reactiveVal(NULL),
      germline.patients  = reactiveVal(character(0)),
      germline.patients.igv = reactiveVal(NULL),
      germline.bam = reactiveVal(NULL),
      germline.overview = list(),
      fusion.variants = reactiveVal(NULL),
      fusion.patients = reactiveVal(character(0)),
      fusion.patients.igv = reactiveVal(NULL),
      fusion.bam = reactiveVal(NULL),
      fusion.overview = list(),
      expression.variants.goi = reactiveVal(NULL), #genes of interest
      expression.variants.all = reactiveVal(NULL), # all genes
      expression.patients = reactiveVal(character(0)),
      expression.overview = list(),
      session_loaded = reactiveVal(FALSE),
      navigation_context = reactiveVal(NULL),     # somatic or germline or fusion     # from where are we opening IGV
      session_dir = reactiveVal(NULL),
      is_loading_session = reactiveVal(FALSE),
      
      # NOVÉ: Přidat pro fusion prerun status
      fusion_prerun_status = reactiveVal("not_started"),
      fusion_prerun_progress = reactiveVal(0),
      fusion_prerun_future = NULL  # pro sledování future objektu
    )
    shared_data$upload_modules <- reactiveVal(list())
    shared_data$upload_pending <- reactiveVal(list()) 
    shared_data$somatic_modules <- reactiveVal(list())  # patient -> list(methods)
    shared_data$somatic_pending <- reactiveVal(list())  # patient -> state (k načtení)
    shared_data$germline_modules <- reactiveVal(list())
    shared_data$germline_pending <- reactiveVal(list()) 
    shared_data$fusion_modules <- reactiveVal(list())
    shared_data$fusion_pending <- reactiveVal(list()) 
    shared_data$expression_modules <- reactiveVal(list())
    shared_data$expression_pending <- reactiveVal(list()) 
    
    # Track which tab values were added per dataset (so we can remove/replace on reconfirm)
    added_tab_values <- reactiveValues(
      somatic  = character(0),
      germline = character(0),
      fusion = character(0),
      expression = character(0),
      network = character(0)
    )
    
    observe({ session$sendCustomMessage("initRadioSync", list()) })
    
    #######################################################################################
    #### upload data module - lock all other tabs until data is uploaded and confirmed ####
    #######################################################################################
    upload <- upload_data$server("upload_data_table", shared_data)
  
    
    # NOVÝ: Observer pro sledování fusion prerun statusu (pro debugging)
    observe({
      status <- shared_data$fusion_prerun_status()
      progress <- shared_data$fusion_prerun_progress()
      
      if (status == "running") {
        message("Fusion prerun progress: ", progress, "%")
      } else if (status == "completed") {
        message("Fusion prerun completed successfully!")
      } else if (status == "failed") {
        message("Fusion prerun failed!")
      }
    })

    observeEvent(upload$confirmed_paths(), {
      confirmed_paths <- upload$confirmed_paths()   # make visible to helper above; or pass as arg
      mounted_summary <- reactiveValues(mounted = character(0))
      
      
      somatic_patients <- get_patients(confirmed_paths, "somatic")
      germline_patients <- get_patients(confirmed_paths, "germline")
      fusion_patients <- get_patients(confirmed_paths, "fusion")
      expression_patients <- get_patients(confirmed_paths, "expression")
      
      session_dir <- isolate(shared_data$session_dir())

      if (isTRUE(shared_data$is_loading_session())) {
        message("📂 Loading session - using existing cache from: ", session_dir)
        
        if (is.null(session_dir) || session_dir == "") {
          message("❌ ERROR: session_dir is NULL or empty during session load!")
          showNotification("⚠️ Session directory not set! Creating new cache...", type = "warning")
          shared_data$is_loading_session(FALSE)
        } else {
          somatic_cache <- file.path(session_dir, "in_library_somatic.rds")
          germline_cache <- file.path(session_dir, "in_library_germline.rds")
          
          message("🔍 Checking for cache files:")
          message("   Somatic: ", somatic_cache, " -> ", file.exists(somatic_cache))
          message("   Germline: ", germline_cache, " -> ", file.exists(germline_cache))
          
          if (!file.exists(somatic_cache) || !file.exists(germline_cache)) {
            message("❌ Cache files missing!")
            showNotification("⚠️ Cache files missing! Creating new cache...", type = "warning")
            shared_data$is_loading_session(FALSE)
          } else {
            message("✅ Cache files found - will use existing cache")
          }
        }
      }
      
      # Vytvoř nový cache POUZE pokud NENÍ load session
      if (!isTRUE(shared_data$is_loading_session())) {

        # Cleanup old sessions
        cleanup_old_sessions("sessions", days = 7)
        
        if (length(somatic_patients) > 0 || length(germline_patients) > 0 ) {
          # Create new session directory
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          new_session_dir <- file.path("sessions", paste0("session_", timestamp))
          dir.create(new_session_dir, recursive = TRUE)
          shared_data$session_dir(new_session_dir)
          
          message("📂 New session directory: ", new_session_dir)
        }

        if (length(somatic_patients) > 0) {
          # Create somatic cache
          withProgress(message = "Creating cache for somatic variants...", {
            create_session_cache(
              all_files = get_files_by_patient(confirmed_paths, "somatic"),
              all_patients = somatic_patients,
              session_dir = new_session_dir,
              variant_type = "somatic"
            )
          })
        } else {
          message("⏭️  Skipping somatic cache - no data available")
        }
        

        
        if (length(germline_patients) > 0) {
          # Create germline cache
          withProgress(message = "Creating cache for germline variants...", {
            create_session_cache(
              all_files = get_files_by_patient(confirmed_paths, "germline"),
              all_patients = germline_patients,
              session_dir = new_session_dir,
              variant_type = "germline"
            )
          })
        } else {
          message("⏭️  Skipping germline cache - no data available")
        }
        
      } else {
        message("✅ Using existing cache from: ", session_dir)
        shared_data$is_loading_session(FALSE)
        message("🔄 Reset is_loading_session to FALSE")
      }
      
      # ## Somatic
      if (length(somatic_patients) > 0)  add_dataset_tabs(session, confirmed_paths, "somatic", somatic_patients, shared_data, added_tab_values, "somatic_tabset", "som_", somatic_var_call_table)
        # # # ## Germline
      if (length(germline_patients) > 0) add_dataset_tabs(session, confirmed_paths, "germline", germline_patients, shared_data, added_tab_values, "germline_tabset", "germ_", germline_var_call_table)
        ## Fusion
      if (length(fusion_patients) > 0) add_dataset_tabs(session, confirmed_paths, "fusion", fusion_patients, shared_data, added_tab_values, "fusion_tabset", "fus_", fusion_genes_table, reactive(input$load_session_btn))
        # # ## Expression & network graph
      if (length(expression_patients) > 0) {
        # add_dataset_tabs(session, confirmed_paths, "expression", expression_patients, shared_data, added_tab_values, "expression_tabset", "expr_", expression_profile_table, reactive(input$load_session_btn))
        add_dataset_tabs(session, confirmed_paths, "network", expression_patients, shared_data, added_tab_values, "network_graph", "net_", networkGraph_cytoscape)
      }
        
        ## Summary
      add_summary_boxes(session, output, shared_data, "summary_table", summary, mounted_summary)
  

      # ## IGV + static server mount (ONCE)
      if (length(somatic_patients) > 0 || length(germline_patients) > 0 || length(fusion_patients) > 0){
        if (is.null(shared_data$igv_server_started)) shared_data$igv_server_started <- reactiveVal(FALSE)
        if (is.null(shared_data$igv_root)) shared_data$igv_root <- reactiveVal(NULL)
        
        if (!isTRUE(shared_data$igv_server_started())) {
  
          root_path <- unique(confirmed_paths$root_path)
          root_path <- sub("/+$", "", root_path)
  
          start_static_server(root_path)
          
          shared_data$igv_root(root_path)  
          shared_data$igv_server_started(TRUE)
      
          session$onSessionEnded(function() {
            stop_static_server()
          })
        }
        
        IGV$igv_server("igv", shared_data, root_path)
    }
      
      observeEvent(input$save_session_btn, {
        shinyalert(
          title = "Confirm Save",
          text  = "Do you really want to save/overwrite the session?",
          type  = "warning",
          showCancelButton = TRUE,
          confirmButtonText = "Yes, save it",
          cancelButtonText  = "Cancel",
          callbackR = function(x) {
            if (isTRUE(x)) {
              session_file <- file.path(shared_data$session_dir(), "session_data.json")
              save_session(session_file, shared_data)
              showNotification("Session successfully saved.", type = "message")
            } else {
              showNotification("Saving session was canceled.", type = "default")
            }
          }
        )
      })
      
      
      
      # 
#       fusion_patients <- get_patients(confirmed_paths, "fusion")
#       patients_to_run <- fusion_patients_to_prerun(fusion_patients, "www")
# 
#       # NOVÝ KÓD: Spustit fusion prerun na pozadí
# 
#       if (length(patients_to_run) > 0) {
#         message("Starting fusion prerun in background for: ", paste(patients_to_run, collapse = ", "))
#         shared_data$fusion_prerun_status <- reactiveVal("running")
#         shared_data$fusion_prerun_progress <- reactiveVal(0)
# 
#         # omez vstupní tabulku jen na tyto pacienty
#         confirmed_subset <- subset(confirmed_paths, patient %in% patients_to_run & dataset == "fusion")
# 
#         prog_file <- file.path(tempdir(), paste0("fusion_", as.integer(Sys.time()), ".progress"))
#         shared_data$fusion_prerun_progress_file <- prog_file
# 
#         fusion_future <- future({
#           message("[DEBUG] fusion prerun: sleeping 30s to simulate heavy work...")
# 
#           # Sys.sleep(15)
# 
# 
#           # prerun_fusion_data(confirmed_subset, shared_data)
# 
#           writeLines("0", prog_file)
#           ok <- FALSE
#           try({
#             ok <- isTRUE(prerun_fusion_data(confirmed_subset, NULL, prog_file = prog_file))
#           })
#           writeLines("100", prog_file)
#           ok
# 
#           TRUE
#         })
# 
#         shared_data$fusion_prerun_future <- fusion_future
# 
#       #   fusion_status_observer <- observe({
#       #     invalidateLater(1000)
#       #     if (!is.null(shared_data$fusion_prerun_future) && resolved(shared_data$fusion_prerun_future)) {
#       #       tryCatch({
#       #         value(shared_data$fusion_prerun_future)
#       #         shared_data$fusion_prerun_status <- reactiveVal("completed")
#       #         message("Fusion prerun completed successfully!")
#       #       }, error = function(e) {
#       #         shared_data$fusion_prerun_status <- reactiveVal("failed")
#       #         message("Fusion prerun failed: ", e$message)
#       #       })
#       #       shared_data$fusion_prerun_future <- NULL
#       #       fusion_status_observer$destroy()
#       #     }
#       #   })
# 
#         fusion_status_observer <- observe({
#           invalidateLater(1000)
# 
#           # čti průběžný progress ze souboru
#           pf <- isolate(shared_data$fusion_prerun_progress_file)
#           if (!is.null(pf) && nzchar(pf) && file.exists(pf)) {
#             p <- suppressWarnings(as.integer(readLines(pf, n = 1)))
#             if (!is.na(p)) shared_data$fusion_prerun_progress(p)  # <<< setter, ne reactiveVal()
#           }
# 
#           # dokončení future
#           if (!is.null(shared_data$fusion_prerun_future) &&
#               future::resolved(shared_data$fusion_prerun_future)) {
# 
#             ok <- tryCatch(future::value(shared_data$fusion_prerun_future), error = function(e) FALSE)
# 
#             shared_data$fusion_prerun_status(if (ok) "completed" else "failed")  # <<< setter
#             shared_data$fusion_prerun_progress(100)                               # <<< setter
# 
#             # úklid
#             shared_data$fusion_prerun_future <- NULL
#             if (!is.null(pf) && file.exists(pf)) unlink(pf)
#             shared_data$fusion_prerun_progress_file <- NULL
# 
#             fusion_status_observer$destroy()
#           }
#         })
# 
# 
#       } else {
#         print("### Fusion prerun skript is not needed for any selected patient.")
#       }
# 
      # Optionally focus the whole Variant calling page
      updateNavbarTabs(session, "navbarMenu", selected = ns("summary"))


  
    }, ignoreInit = TRUE)
    
  })
}


# shinyApp(ui,server,options = list(launch.browser = TRUE))
