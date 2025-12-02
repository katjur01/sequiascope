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
  utils[str, capture.output],
  waiter[spin_1, useWaiter, waiter_show, waiter_hide, waiter_update, spin_fading_circles],
  # jsonlite[read_json,write_json],
  # fresh[create_theme,bs4dash_vars,bs4dash_yiq,bs4dash_layout,bs4dash_sidebar_light,bs4dash_status,bs4dash_color]s
  # promises[future_promise,`%...!%`,`%...>%`,catch],
  # future[plan,multisession],
  # microbenchmark[microbenchmark],
  # parallel[detectCores],
  data.table[data.table, as.data.table, fread],
  shinyalert[shinyalert],
  future[future, value, resolved, plan, multicore, multisession],
  promises[then, catch],
  parallel[detectCores],
  openxlsx[read.xlsx]
)

  # Dynamic worker configuration based on available physical CPU cores (Kubernetes safe)
  n_workers <- min(parallel::detectCores(logical = FALSE), 2)
  message("Setting up ", n_workers, " parallel workers")
  plan(multisession, workers = n_workers)
  options(future.fork.enable = FALSE)


box::use(
  app/view/upload_data,
  app/view/summary,
  app/view/fusion_genes_table,
  app/view/germline_var_call_table,
  app/view/somatic_var_call_table,
  app/view/expression_profile_table,
  app/view/IGV,
  # app/logic/helper_igv[start_static_server,stop_static_server],
  app/view/networkGraph_cytoscape,
  app/logic/session_utils[load_session, save_session, cleanup_old_sessions, create_session_cache],
  app/logic/prerun_fusion[fusion_patients_to_prerun,prerun_fusion_data,prerun_fusion_patient,get_fusion_prerun_status],
  app/logic/test_background_process[test_background_worker_single,test_background_worker],
  app/logic/helper_main[get_patients, get_files_by_patient, add_dataset_tabs, add_summary_boxes],
  app/logic/helper_waiter[show_waiter_with_progress, update_waiter_progress, wait_for_summary_rendered, get_waiter_js],
  app/logic/navigation_lock[lock_navigation, unlock_navigation, get_navigation_lock_css, get_navigation_lock_js],
)

#####################################################


## Methylace nedělají na NGS ale na čipech = genom. pozice a k tomu naměřené intenzity. Výsledkem je report. ##

ui <- function(id){
  ns <- NS(id)
  useShinyjs()
  useWaiter()
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
      tags$head(
        tags$script(src = "static/js/app.min.js"),
        tags$script(src = "static/js/cytoscape_init.js"),
        get_waiter_js(),
        get_navigation_lock_css(),
        get_navigation_lock_js(),
        tags$script(HTML("
          Shiny.addCustomMessageHandler('scroll-to-box', function(message) {
            // Get the namespace prefix from the current URL or use default
            var nsPrefix = '';
            
            // Switch to appropriate tab in variant_calling_tabs (bs4Dash tabBox)
            if (message.box === 'somatic' || message.box === 'germline') {
              // Find the tabBox element by ID
              var tabBoxId = $('[id$=\"variant_calling_tabs\"]').attr('id');
              if (tabBoxId) {
                // Extract namespace
                nsPrefix = tabBoxId.replace('variant_calling_tabs', '');
                
                // Use bs4Dash's tab switching mechanism
                var tabId = nsPrefix + 'variant_calling_tabs';
                var tabValue = message.box; // 'somatic' or 'germline'
                
                // Find and click the appropriate tab link
                $('a[data-value=\"' + tabValue + '\"]').tab('show');
              }
            }
            
            // Scroll to the box after a short delay (to ensure tab is rendered)
            setTimeout(function() {
              var boxClass = message.box + '-box';
              var element = $('.' + boxClass);
              if (element.length > 0) {
                $('html, body').animate({
                  scrollTop: element.offset().top - 100
                }, 500);
              }
            }, 300);
          });
        "))
      ),
      tabItems(
        tabItem(tabName = ns("upload_data"),
          fluidPage(
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
      
      # Per-patient fusion prerun tracking (paralelní zpracování)
      fusion_prerun_status = list(),      # patient_id -> reactiveVal("not_started"|"running"|"completed"|"failed")
      fusion_prerun_progress = list(),    # patient_id -> reactiveVal(0-100)
      fusion_prerun_future = list(),      # patient_id -> future object
      fusion_prerun_observer = list(),    # patient_id -> observer object
      fusion_prerun_errors = reactiveVal(character()),  # Track all errors
      fusion_prerun_total_fusions = list()  # patient_id -> reactiveVal(number of fusions)
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
    
    ############################
    ### for data testing!
    # shared_data$run <- "docker"
    shared_data$run <- "local"
    ############################
    
    # Track which tab values were added per dataset (so we can remove/replace on reconfirm)
    added_tab_values <- reactiveValues(
      somatic  = character(0),
      germline = character(0),
      fusion = character(0),
      expression = character(0),
      network = character(0)
    )
    
    observe({ session$sendCustomMessage("initRadioSync", list()) })
    
    # Flag to track if waiter was already hidden after data load
    waiter_hidden <- reactiveVal(FALSE)
    
    # Lock navigation on app start - only Upload data is accessible
    observe({
      lock_navigation(session, ns("upload_data"))
    })
    
    #######################################################################################
    #### upload data module - lock all other tabs until data is uploaded and confirmed ####
    #######################################################################################
    upload <- upload_data$server("upload_data_table", shared_data)
  
    
    # Observer removed - now using per-patient tracking
    # (fusion_prerun_status is now a list, not a reactiveVal)

    observeEvent(upload$confirmed_paths(), {
      message("🚀 [OBSERVE] confirmed_paths changed - starting data loading...")
      
      # Unlock all navigation tabs after successful data confirmation
      unlock_navigation(session)
      
      # Show waiter with progress bar at the start
      show_waiter_with_progress(session)
      
      confirmed_paths <- upload$confirmed_paths()   # make visible to helper above; or pass as arg
      mounted_summary <- reactiveValues(mounted = character(0))
      
      
      update_waiter_progress(session, 10, "Initializing...")
      
      somatic_patients <- get_patients(confirmed_paths, "somatic")
      germline_patients <- get_patients(confirmed_paths, "germline")
      fusion_patients <- get_patients(confirmed_paths, "fusion")
      expression_patients <- get_patients(confirmed_paths, "expression")
      
      update_waiter_progress(session, 15, "Checking session...")
      
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

        update_waiter_progress(session, 20, "Preparing session...")
        
        # Cleanup old sessions
        cleanup_old_sessions("sessions", days = 7)
        
        # Use existing session_dir if available, otherwise create new one
        existing_session_dir <- isolate(shared_data$session_dir())
        
        if (length(somatic_patients) > 0 || length(germline_patients) > 0 || 
            length(fusion_patients) > 0 || length(expression_patients) > 0) {
          
          if (is.null(existing_session_dir) || existing_session_dir == "") {
            # Create new session directory only if none exists
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            new_session_dir <- file.path("sessions", paste0("session_", timestamp))
            dir.create(new_session_dir, recursive = TRUE)
            shared_data$session_dir(new_session_dir)
            message("📂 New session directory: ", new_session_dir)
          } else {
            # Reuse existing session directory
            new_session_dir <- existing_session_dir
            message("♻️  Reusing existing session directory: ", new_session_dir)
          }
        }

        if (length(somatic_patients) > 0) {
          update_waiter_progress(session, 25, "Creating somatic cache...")
          # Create somatic cache
          create_session_cache(
            all_files = get_files_by_patient(confirmed_paths, "somatic"),
            all_patients = somatic_patients,
            session_dir = new_session_dir,
            variant_type = "somatic"
          )
          update_waiter_progress(session, 40, "Somatic cache complete")
        } else {
          message("⏭️  Skipping somatic cache - no data available")
          update_waiter_progress(session, 40, "No somatic data")
        }
        

        
        if (length(germline_patients) > 0) {
          update_waiter_progress(session, 45, "Creating germline cache...")
          # Create germline cache
          create_session_cache(
            all_files = get_files_by_patient(confirmed_paths, "germline"),
            all_patients = germline_patients,
            session_dir = new_session_dir,
            variant_type = "germline"
          )
          update_waiter_progress(session, 60, "Germline cache complete")
        } else {
          message("⏭️  Skipping germline cache - no data available")
          update_waiter_progress(session, 60, "No germline data")
        }
        
      } else {
        message("✅ Using existing cache from: ", session_dir)
        shared_data$is_loading_session(FALSE)
        message("🔄 Reset is_loading_session to FALSE")
        update_waiter_progress(session, 60, "Using existing cache")
      }
      
      update_waiter_progress(session, 65, "Loading somatic data...")
      # ## Somatic
      if (length(somatic_patients) > 0)  add_dataset_tabs(session, confirmed_paths, "somatic", somatic_patients, shared_data, added_tab_values, "somatic_tabset", "som_", somatic_var_call_table)
        
      update_waiter_progress(session, 70, "Loading germline data...")
      # # # ## Germline
      if (length(germline_patients) > 0) add_dataset_tabs(session, confirmed_paths, "germline", germline_patients, shared_data, added_tab_values, "germline_tabset", "germ_", germline_var_call_table)
        
      update_waiter_progress(session, 75, "Loading fusion data...")
      ## Fusion
      if (length(fusion_patients) > 0) add_dataset_tabs(session, confirmed_paths, "fusion", fusion_patients, shared_data, added_tab_values, "fusion_tabset", "fus_", fusion_genes_table, reactive(input$load_session_btn))
        
      update_waiter_progress(session, 80, "Loading expression data...")
      # # ## Expression & network graph
      if (length(expression_patients) > 0) {
        add_dataset_tabs(session, confirmed_paths, "expression", expression_patients, shared_data, added_tab_values, "expression_tabset", "expr_", expression_profile_table, reactive(input$load_session_btn))
        update_waiter_progress(session, 85, "Loading network graph...")
        add_dataset_tabs(session, confirmed_paths, "network", expression_patients, shared_data, added_tab_values, "network_graph", "net_", networkGraph_cytoscape)
      }

      update_waiter_progress(session, 90, "Creating summary...")
      ## Summary
      add_summary_boxes(session, output, shared_data, "summary_table", summary, mounted_summary)
  

      update_waiter_progress(session, 95, "Initializing IGV...")
      # ## IGV + static server mount (ONCE)
      if (length(somatic_patients) > 0 || length(germline_patients) > 0 || length(fusion_patients) > 0){
        if (is.null(shared_data$igv_server_started)) shared_data$igv_server_started <- reactiveVal(FALSE)
        if (is.null(shared_data$igv_root)) shared_data$igv_root <- reactiveVal(NULL)
        
        if (!isTRUE(shared_data$igv_server_started())) {
  
          root_path <- unique(confirmed_paths$root_path)
          root_path <- sub("/+$", "", root_path)
  
          # start_static_server(root_path)
          
          shared_data$igv_root(root_path)  
          message("root_path in main: ", root_path)
          shared_data$igv_server_started(TRUE)
      
          # session$onSessionEnded(function() {
          #   stop_static_server()
          # })
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
      
      
      # ═══════════════════════════════════════════════════════════════════════════
      # FUSION PRERUN - PARALLEL processing for each patient
      # ═══════════════════════════════════════════════════════════════════════════
      
      # Initialize flag to track if prerun has started
      if (is.null(shared_data$fusion_prerun_started)) {
        shared_data$fusion_prerun_started <- reactiveVal(FALSE)
      }
      
      # Only run prerun once per session
      if (!shared_data$fusion_prerun_started()) {
        fusion_patients <- get_patients(confirmed_paths, "fusion")
        patients_to_run <- fusion_patients_to_prerun(fusion_patients, "www")

        if (length(patients_to_run) > 0) {
          message("[PRERUN INIT] Starting PARALLEL fusion prerun for ", length(patients_to_run), " patients: ", paste(patients_to_run, collapse = ", "))
          shared_data$fusion_prerun_started(TRUE)  # Set flag to prevent re-running
          
          # Clear old errors
          shared_data$fusion_prerun_errors(character())
        
        # Get file list for all fusion patients
        fusion_files <- get_files_by_patient(confirmed_paths, "fusion")
        
        # ═══════════════════════════════════════════════════════════════════════════
        # Launch one future per patient (parallel execution with REAL processing)
        # ═══════════════════════════════════════════════════════════════════════════
        for (patient_id in patients_to_run) {
          message("[LAUNCH] Starting REAL background process for patient: ", patient_id)
          
          # Initialize per-patient tracking
          shared_data$fusion_prerun_status[[patient_id]] <- reactiveVal("running")
          shared_data$fusion_prerun_progress[[patient_id]] <- reactiveVal(0)
          
          # Create progress file for this patient
          prog_file <- file.path(tempdir(), paste0("fusion_", patient_id, "_", as.integer(Sys.time()), ".progress"))
          
          # Get file list for this patient
          file_list <- fusion_files[[patient_id]]
          
          # Count total fusions for this patient for UI display
          total_fusions <- 0
          if (!is.null(file_list$fusion) && length(file_list$fusion) > 0 && file.exists(file_list$fusion[1])) {
            tryCatch({
              if (grepl("\\.xlsx?$", file_list$fusion[1])) {
                fusion_dt <- as.data.table(openxlsx::read.xlsx(file_list$fusion[1]))
              } else {
                fusion_dt <- data.table::fread(file_list$fusion[1])
              }
              total_fusions <- nrow(fusion_dt)
            }, error = function(e) {
              message("[FUSION COUNT] Error counting fusions for ", patient_id, ": ", e$message)
            })
          }
          shared_data$fusion_prerun_total_fusions[[patient_id]] <- reactiveVal(total_fusions)
          message("[FUSION COUNT] Patient ", patient_id, " has ", total_fusions, " fusions")
          message("[MAIN] file_list for ", patient_id, ":")
          message("[MAIN]   - is.null: ", is.null(file_list))
          message("[MAIN]   - class: ", paste(class(file_list), collapse=", "))
          if (!is.null(file_list)) {
            message("[MAIN]   - names: ", paste(names(file_list), collapse=", "))
            message("[MAIN]   - fusion: ", if(!is.null(file_list$fusion)) paste(file_list$fusion, collapse=", ") else "NULL")
            message("[MAIN]   - arriba: ", if(!is.null(file_list$arriba)) paste(file_list$arriba, collapse=", ") else "NULL")
          }
          
          # Launch future for this patient with REAL processing
          message("[MAIN] About to launch future for ", patient_id)
          
          # Use explicit globals list to speed up future creation
          patient_future <- future({
            prerun_fusion_patient(patient_id, file_list, prog_file, output_base_dir = "./www")
          }, globals = list(
            patient_id = patient_id,
            file_list = file_list, 
            prog_file = prog_file,
            prerun_fusion_patient = prerun_fusion_patient
          ), stdout = TRUE, conditions = "message")
          
          message("[MAIN] Future launched for ", patient_id)
          
          shared_data$fusion_prerun_future[[patient_id]] <- list(
            future = patient_future,
            prog_file = prog_file
          )
          
          # Create observer for this patient
          local({
            pid <- patient_id  # Capture in closure
            pfile <- prog_file
            
            observer <- observe({
              invalidateLater(500)  # Check twice per second
              
              # Read progress from file
              if (file.exists(pfile)) {
                p <- suppressWarnings(as.integer(readLines(pfile, n = 1)))
                if (!is.na(p)) {
                  shared_data$fusion_prerun_progress[[pid]](p)
                  # message("[OBSERVER] ", pid, " progress: ", p, "%")  # Comment out to reduce console spam
                }
              }
              
              # Check if this patient's future completed
              fut_data <- shared_data$fusion_prerun_future[[pid]]
              if (!is.null(fut_data) && future::resolved(fut_data$future)) {
                
                result <- tryCatch({
                  val <- future::value(fut_data$future)
                  
                  # Print captured stdout/stderr from worker
                  if (!is.null(attr(val, "stdout"))) {
                    cat("[WORKER OUTPUT ", pid, "]\n", attr(val, "stdout"), "\n", sep = "")
                  }
                  
                  # Print messages captured in result structure
                  if (!is.null(val$messages) && length(val$messages) > 0) {
                    message("[MESSAGES from ", pid, "]:")
                    for (msg in val$messages) message("  ", msg)
                  }
                  
                  # Print warnings
                  if (!is.null(val$warnings) && length(val$warnings) > 0) {
                    message("[WARNINGS from ", pid, "]:")
                    for (wrn in val$warnings) warning("  ", wrn, call. = FALSE)
                  }
                  
                  # Print errors
                  if (!is.null(val$errors) && length(val$errors) > 0) {
                    message("[ERRORS from ", pid, "]:")
                    for (err in val$errors) message("  ", err)
                  }
                  
                  val
                }, error = function(e) {
                  message("[ERROR] ", pid, ": ", e$message)
                  list(success = FALSE, errors = paste0(pid, ": ", e$message), progress = 0)
                })
                
                message("[COMPLETED] ", pid, " - Success: ", result$success)
                
                # Update status
                shared_data$fusion_prerun_status[[pid]](if (result$success) "completed" else "failed")
                shared_data$fusion_prerun_progress[[pid]](100)
                
                # Track errors
                if (!result$success && length(result$errors) > 0) {
                  current_errors <- shared_data$fusion_prerun_errors()
                  shared_data$fusion_prerun_errors(c(current_errors, result$errors))
                }
                
                # Cleanup
                if (file.exists(pfile)) unlink(pfile)
                shared_data$fusion_prerun_future[[pid]] <- NULL
                observer$destroy()
                
                message("[CLEANUP] Observer destroyed for ", pid)
              }
            })
            
            shared_data$fusion_prerun_observer[[pid]] <- observer
          })
        }
        
        message("[PARALLEL LAUNCH] All ", length(patients_to_run), " patient futures started")
        
        # Global notification when all complete (runs once, then destroys itself)
        all_complete_observer <- observe({
          invalidateLater(1000)
          
          # Check if all patients completed
          statuses <- sapply(patients_to_run, function(pid) {
            if (pid %in% names(shared_data$fusion_prerun_status)) {
              shared_data$fusion_prerun_status[[pid]]()
            } else {
              "not_started"
            }
          })
          
          all_done <- all(statuses %in% c("completed", "failed"))
          
          if (all_done) {
            errors <- shared_data$fusion_prerun_errors()
            if (length(errors) > 0) {
              showNotification(
                paste0("⚠️ Fusion prerun completed with ", length(errors), " error(s)"),
                type = "warning",
                duration = 10
              )
            } else {
              showNotification(
                paste0("✅ All ", length(patients_to_run), " patients processed successfully!"),
                type = "message",
                duration = 5
              )
            }
            
            # Destroy this observer so it doesn't keep showing notifications
            all_complete_observer$destroy()
            message("[ALL COMPLETE] Observer destroyed")
          }
        })

        } else {
          message("[PRERUN INIT] Fusion prerun not needed - all patients already processed")
          # Mark all fusion patients as completed since they already have processed data
          for (patient_id in fusion_patients) {
            shared_data$fusion_prerun_status[[patient_id]] <- reactiveVal("completed")
            message("[PRERUN INIT] Marked patient ", patient_id, " as completed")
          }
          shared_data$fusion_prerun_started(TRUE)
        }
      } else {
        message("[PRERUN INIT] Fusion prerun skipped - already started")
      }

      # Optionally focus the whole Variant calling page
      update_waiter_progress(session, 98, "Finalizing...")

      # updateNavbarTabs(session, "navbarMenu", selected = ns("expression_profile"))
      
      # Wait for UI to switch tabs and render Summary
      update_waiter_progress(session, 100, "Complete!")
      updateNavbarTabs(session, "navbarMenu", selected = ns("summary"))
      
      # Request JS to notify when Summary is fully rendered (with proper namespace)
      wait_for_summary_rendered(session, ns)
      
      # Reset flag for this data load
      waiter_hidden(FALSE)
  
    }, ignoreInit = TRUE)
    
    # Hide waiter when Summary tab is fully rendered
    observeEvent(input$summary_rendered, {
      # Only hide waiter once per data load
      if (!waiter_hidden()) {
        message("✅ Summary tab fully rendered - hiding waiter")
        waiter_hide(id = NA)
        waiter_hidden(TRUE)
      }
    })
    
  })
}


# shinyApp(ui,server,options = list(launch.browser = TRUE))
