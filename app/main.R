# ## install/unistall package from dependencies with commands:
# # rhino::pkg_install("shiny")
# # rhino::pkg_remove("dplyr")
#
# restart R session
# rstudioapi::restartSession()  # If using RStudio
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
# git commit -m "Save current changes to new branch"
# git push -u origin dev
box::use(
  rhino,
  shiny[h1,h2,h3,bootstrapPage,div,moduleServer,NS,renderUI,tags,uiOutput,icon,observeEvent,observe,reactive,isTruthy,onFlushed,appendTab,removeTab,withProgress,
        fluidRow,fluidPage,mainPanel,tabPanel,titlePanel,tagList,HTML,textInput,sidebarLayout,sidebarPanel,includeScript,invalidateLater,isolate,
        br,updateTabsetPanel,imageOutput,renderImage,reactiveVal,req,fixedPanel,reactiveValues,fileInput,showNotification,showTab,hideTab,addResourcePath],
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
  mirai[mirai, daemons, unresolved],
  later[later],
  parallel[detectCores],
  openxlsx[read.xlsx],
  jsonlite[fromJSON]
)

  # Set up mirai daemon pool — one daemon per physical core minus one for Shiny.
  # mirai() NEVER blocks the main thread regardless of how many tasks are queued.
  # Tasks in excess of the pool size are automatically queued without blocking.
  #
  # WARNING: in Kubernetes/containers, detectCores() reads the HOST core count,
  # not the container CPU limit — this can spawn hundreds of R processes (OOMKill).
  # Priority: MIRAI_WORKERS env var > cgroup CPU quota > detectCores() fallback.
  env_workers <- suppressWarnings(as.integer(Sys.getenv("MIRAI_WORKERS", unset = NA)))

  get_container_cpu_limit <- function() {
    # cgroups v2 (modern k8s nodes)
    cg2 <- "/sys/fs/cgroup/cpu.max"
    if (file.exists(cg2)) {
      val <- trimws(readLines(cg2, n = 1L))
      if (!startsWith(val, "max")) {
        parts <- strsplit(val, " ")[[1]]
        quota  <- as.numeric(parts[1])
        period <- as.numeric(parts[2])
        if (!is.na(quota) && !is.na(period) && period > 0)
          return(max(floor(quota / period), 1L))
      }
    }
    # cgroups v1
    quota_file  <- "/sys/fs/cgroup/cpu/cpu.cfs_quota_us"
    period_file <- "/sys/fs/cgroup/cpu/cpu.cfs_period_us"
    if (file.exists(quota_file) && file.exists(period_file)) {
      quota  <- as.numeric(readLines(quota_file,  n = 1L))
      period <- as.numeric(readLines(period_file, n = 1L))
      if (!is.na(quota) && quota > 0 && !is.na(period) && period > 0)
        return(max(floor(quota / period), 1L))
    }docker system prune -f
    docker compose down
    docker compose build
    docker compose up -d
    return(NULL)
  }

  detected_cores  <- parallel::detectCores(logical = FALSE)
  container_cores <- get_container_cpu_limit()

  n_workers <- if (!is.na(env_workers) && env_workers >= 1L) {
    env_workers
  } else if (!is.null(container_cores)) {
    max(container_cores - 1L, 1L)
  } else {
    max(detected_cores - 1L, 1L)
  }

  message(sprintf(
    "Setting up %d mirai daemons [MIRAI_WORKERS=%s, cgroup_cores=%s, host_cores=%d]",
    n_workers,
    Sys.getenv("MIRAI_WORKERS", unset = "unset"),
    if (is.null(container_cores)) "unavailable" else as.character(container_cores),
    detected_cores
  ))
  daemons(n = n_workers)


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
  app/logic/prerun_fusion[fusion_patients_to_prerun,prerun_fusion_data,prerun_fusion_patient,get_fusion_prerun_status,check_fusion_status,cleanup_patient_fusion_outputs],

  app/logic/helper_main[get_patients, get_files_by_patient, add_dataset_tabs, add_summary_boxes],
  app/logic/waiter[show_waiter_with_progress, update_waiter_progress, hide_waiter_progress, wait_for_summary_rendered, get_waiter_js],
  app/logic/navigation_lock[lock_navigation, unlock_navigation, get_navigation_lock_css, get_navigation_lock_js],
  app/logic/helper_prerun_dialog[check_and_show_fusion_dialog],
)

#####################################################


## Methylation is not done on NGS but on chips = genomic positions and measured intensities. The result is a report. ##

ui <- function(id){
  ns <- NS(id)
  useShinyjs()
  useWaiter()
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"))
    
  dashboardPage(
    dark = NULL, 
    help = NULL,
    preloader = list(html = spin_1(), color = "#333e48"),
    header = dashboardHeader(
      nav = navbarMenu(id = ns("navbarMenu"),
        navbarTab("Upload data", tabName = ns("upload_data")),
        navbarTab("Summary", tabName = ns("summary")),
        navbarTab("Variant calling", tabName = ns("variant_calling")),
        navbarTab("Fusion genes", tabName = ns("fusion_genes")),
        navbarTab("Expression profile", tabName = ns("expression_profile")),
        navbarTab("IGV", tabName = ns("hidden_igv")),
        navbarTab("Network graph", tabName = ns("network_graph"))
      ),
      rightUi = tagList(
        tags$li(class = "dropdown",
          tags$a(href = "javascript:void(0);",onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'});", ns("save_session_btn")),title = "Save session",class = "btn btn-link",style = "color: #74c0fc; font-size: 18px; padding: 12px 10px;",icon("floppy-disk"))),
        tags$li(class = "dropdown",
          tags$a(href = "https://katjur01.github.io/seqUIaSCOPE/",target = "_blank", title = "Open documentation", class = "btn btn-link", style = "color: #74c0fc; font-size: 18px; padding: 12px 10px; margin-right: 30px;", icon("circle-question"))))),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(#style = "background-color: white;",
      tags$head(
        tags$style(HTML("
          .navbar-nav > li > a.btn-link:focus,
          .navbar-nav > li > a.btn-link:active,
          .navbar-nav > li > a.btn-link:hover {
            background-color: transparent !important;
            box-shadow: none !important;
            outline: none !important;
          }
        ")),
        tags$script(src = "static/js/app.min.js"),
        tags$script(src = "static/js/cytoscape_init.js"),
        get_waiter_js(),
        get_navigation_lock_css(),
        get_navigation_lock_js(),
        tags$script(HTML("
          // Prevent accidental page refresh/close with warning
          var dataModified = false;  // Track if user has made changes
          var allowUnload = false;   // Flag to allow unload after save
          
          // Set flag when user interacts with data (selections, filters, etc.)
          $(document).on('shiny:inputchanged', function(event) {
            // Ignore internal state changes
            if (!event.name.includes('_rendered') && 
                !event.name.includes('_state') &&
                !event.name.includes('_rows_selected') &&
                event.name !== 'summary_rendered') {
              dataModified = true;
            }
          });
          
          // Set flag when data is confirmed/loaded
          Shiny.addCustomMessageHandler('data-loaded', function(message) {
            dataModified = true;
          });
          
          // Show browser warning before leaving page
          window.addEventListener('beforeunload', function(e) {
            if (dataModified && !allowUnload) {
              // Modern browsers show standard message: 'Leave site? Changes you made may not be saved.'
              // with buttons: 'Leave' and 'Stay'
              e.preventDefault();
              e.returnValue = '';
              return '';
            }
          });
          
          // Handler to allow unload after successful save
          Shiny.addCustomMessageHandler('allow-unload', function(message) {
            allowUnload = true;
            dataModified = false;
          });
        ")),
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
                    ### JavaScript code for capturing radio button changes
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
                    div(class = "patient-tabs",
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
    message("[mirai] daemon pool ready")

    # Load output directory from config
    config <- jsonlite::fromJSON("reference_paths.json")
    output_dirname <- config$output_dir  # e.g. "output_files"
    
    # Load custom genome configuration (if defined)
    custom_genome_config <- if (!is.null(config$custom_genome)) config$custom_genome else NULL
    
    if (!is.null(custom_genome_config)) message("🧬 Custom genome configured: ", custom_genome_config$display_name, " (", custom_genome_config$igv_id, ")")
  
    # Detect environment: check if /output_files exists (Docker/K8s mount) or use local
    if (dir.exists(paste0("/", output_dirname))) {
      output_base <- paste0("/", output_dirname)
      message("📦 Container environment detected - using ", output_base)
    } else {
      output_base <- paste0("./", output_dirname)
      message("💻 Running locally - output: ", output_base)
    }
    
    # Register sessions directory as static resource — allows fusion_genes_table to
    # serve IGV snapshots and Arriba SVGs via /sessions/... URL without base64 encoding.
    sessions_dir <- file.path(output_base, "sessions")
    dir.create(sessions_dir, recursive = TRUE, showWarnings = FALSE)
    addResourcePath("sessions", sessions_dir)
    message("🗂️ Static resource path registered: /sessions → ", sessions_dir)
    
    shared_data <- reactiveValues(
      data_path     = reactiveVal(NULL),  # User-selected data directory path (first project folder)
      bam_path      = reactiveVal(NULL),  # User-selected BAM directory (if separate from projects)
      projects_path = reactiveVal(NULL),  # All selected project folders
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
      igv_genome = reactiveVal(character(0)), # genome for IGV
      custom_genome_config = custom_genome_config, # custom genome configuration from reference_paths.json
      
      # Per-patient fusion prerun tracking (parallel processing)
      fusion_prerun_status = list(),      # patient_id -> reactiveVal("not_started"|"running"|"completed"|"failed")
      fusion_prerun_progress = list(),    # patient_id -> reactiveVal(0-100)
      fusion_prerun_future = list(),      # patient_id -> future object
      fusion_prerun_observer = list(),    # patient_id -> observer object
      fusion_prerun_errors = reactiveVal(character()),  # Track all errors
      fusion_prerun_total_fusions = list(),  # patient_id -> reactiveVal(number of fusions)

      upload_modules = reactiveVal(list()),
      upload_pending = reactiveVal(list()),
      somatic_modules = reactiveVal(list()),  # patient -> list(methods)
      somatic_pending = reactiveVal(list()),  # patient -> state (to be loaded)
      germline_modules = reactiveVal(list()),
      germline_pending = reactiveVal(list()),
      fusion_modules = reactiveVal(list()),
      fusion_pending = reactiveVal(list()),
      expression_modules = reactiveVal(list()),
      expression_pending = reactiveVal(list()),
      confirmed_paths = reactiveVal(NULL)  # Always-current file index — updated on every re-confirm so modules can read fresh BAM paths
    )
    ############################
    ### Output path setup (already detected above)
    shared_data$output_path <- reactiveVal(output_base)
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
    
    # Initialize fusion prerun reactive values (BEFORE observeEvent)
    shared_data$fusion_prerun_started <- reactiveVal(FALSE)
    shared_data$fusion_prerun_user_confirmed <- reactiveVal(FALSE)
    shared_data$pending_data_load <- reactiveVal(NULL)  # Store confirmed_paths when waiting for user dialog
    
    #######################################################################################
    #### upload data module - lock all other tabs until data is uploaded and confirmed ####
    #######################################################################################
    upload <- upload_data$server("upload_data_table", shared_data)
    
    # Observer removed - now using per-patient tracking
    # (fusion_prerun_status is now a list, not a reactiveVal)

    observeEvent(upload$confirmed_paths(), {
      confirmed_paths <- upload$confirmed_paths()
      req(confirmed_paths)  # NULL is set momentarily before the real value — ignore that flush
      message("🚀 [OBSERVE] confirmed_paths changed - starting data loading...")
      output_dir <- shared_data$output_path()
      unlock_navigation(session) # Unlock all navigation tabs after successful data confirmation
      mounted_summary <- reactiveValues(mounted = character(0))
      
      # Check for existing fusion outputs and show resume/clean-start dialog if needed
      if (check_and_show_fusion_dialog(confirmed_paths, output_dir, shared_data)) return()
      
      # No existing outputs - proceed directly with data loading
      shared_data$fusion_prerun_started(FALSE)    # Allow re-run check on every re-confirm
      shared_data$fusion_prerun_user_confirmed(TRUE)
      shared_data$pending_data_load(confirmed_paths)
    }, ignoreInit = TRUE)
    
    # ═══════════════════════════════════════════════════════════════════════════
    # SEPARATE OBSERVER: Start data loading when user confirms (or no dialog shown)
    # ═══════════════════════════════════════════════════════════════════════════
    observe({
      req(shared_data$fusion_prerun_user_confirmed())  # Wait for user confirmation
      confirmed_paths <- isolate(shared_data$pending_data_load())
      req(confirmed_paths)  # Only proceed if we have paths
      
      message("[DATA LOAD] Starting data loading with user confirmation")
      
      output_dir <- shared_data$output_path()
      mounted_summary <- reactiveValues(mounted = character(0))
      
      # Show waiter with progress bar — reset flag so the summary_rendered observer
      # fires again on this data load (guard against stale TRUE from previous load)
      waiter_hidden(FALSE)
      show_waiter_with_progress(session)
      update_waiter_progress(session, 10, "Initializing...")

      # Save previous file index before publishing the new one (needed for expression diff below)
      prev_confirmed_paths <- isolate(shared_data$confirmed_paths())

      # Publish current file index so all module servers can read fresh BAM paths reactively
      shared_data$confirmed_paths(confirmed_paths)

      somatic_patients <- get_patients(confirmed_paths, "somatic")
      germline_patients <- get_patients(confirmed_paths, "germline")
      fusion_patients <- get_patients(confirmed_paths, "fusion")
      expression_patients <- get_patients(confirmed_paths, "expression")

      # Show/hide Variant Calling tabs based on available data
      if (length(somatic_patients) > 0) {
        showTab(inputId = "variant_calling_tabs", target = "somatic",
                select = (length(germline_patients) == 0))
      } else {
        hideTab(inputId = "variant_calling_tabs", target = "somatic")
      }
      if (length(germline_patients) > 0) {
        showTab(inputId = "variant_calling_tabs", target = "germline",
                select = (length(somatic_patients) == 0))
      } else {
        hideTab(inputId = "variant_calling_tabs", target = "germline")
      }

      update_waiter_progress(session, 15, "Checking session...")
      
      session_dir <- isolate(shared_data$session_dir())
      
      # If session_dir is relative path (old format), convert to absolute path in output_files
      if (!is.null(session_dir) && session_dir != "" && !startsWith(session_dir, output_dir)) {
        session_name <- basename(session_dir)  # e.g., "MOII_e117" or "demo_data"
        session_dir <- file.path(output_dir, "sessions", session_name)
        shared_data$session_dir(session_dir)  # Update with absolute path
        message("📂 Converted relative session path to absolute: ", session_dir)
      }

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
      
      # Create new cache ONLY if NOT loading a session
      if (!isTRUE(shared_data$is_loading_session())) {

        update_waiter_progress(session, 20, "Preparing session...")
        
        # Cleanup old sessions
        cleanup_old_sessions(file.path(output_dir, "sessions"), days = 31)
        
        # Use existing session_dir if available, otherwise create new one
        existing_session_dir <- isolate(shared_data$session_dir())
        
        if (length(somatic_patients) > 0 || length(germline_patients) > 0 || 
            length(fusion_patients) > 0 || length(expression_patients) > 0) {
          
          if (is.null(existing_session_dir) || existing_session_dir == "") {
            # Create session directory based on dataset name (from input folder)
            data_path <- isolate(shared_data$data_path())
            
            if (!is.null(data_path) && data_path != "") {
              # Name session after sorted project folder basenames (joined with _)
              proj_names   <- sort(basename(isolate(shared_data$projects_path())))
              dataset_name <- paste(proj_names, collapse = "_")
              
              # Sanitize folder name (remove problematic characters)
              dataset_name <- gsub("[^A-Za-z0-9_-]", "_", dataset_name)
              
              # Fallback to timestamp if sanitized name is empty
              if (!nzchar(dataset_name)) {
                dataset_name <- paste0("session_", format(Sys.time(), "%d_%m_%Y_%H%M%S"))
                message("⚠️  Could not derive session name from project folders — using timestamp fallback: ", dataset_name)
              }
              
              # Create session directory (without timestamp for reusability)
              new_session_dir <- file.path(output_dir, "sessions", dataset_name)
              
              # Check if session already exists
              if (dir.exists(new_session_dir)) {
                # Session exists - reuse it (cache, manifests, IGV snapshots will be preserved)
                message("♻️  Reusing existing session: ", dataset_name)
                message("    Cache and processed fusion data will be preserved for faster loading")
              } else {
                # Create new session directory
                dir.create(new_session_dir, recursive = TRUE)
                message("📂 New session directory: ", dataset_name)
              }
              
              shared_data$session_dir(new_session_dir)
            } else {
              # Fallback to timestamp if data_path is not available
              timestamp <- format(Sys.time(), "%d_%m_%Y_%H%M%S")
              new_session_dir <- file.path(output_dir, "sessions", paste0("session_", timestamp))
              dir.create(new_session_dir, recursive = TRUE)
              shared_data$session_dir(new_session_dir)
              message("📂 New session directory (fallback): ", new_session_dir)
            }
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
      # ## Expression & network graph
      # Per-patient rebuild: only remove+re-add tabs whose tissue_list or goi flag changed.
      # This avoids spawning duplicate module servers (which caused infinite re-loading).
      if (length(expression_patients) > 0) {
        prev_expr_files <- if (!is.null(prev_confirmed_paths)) get_files_by_patient(prev_confirmed_paths, "expression") else list()
        new_expr_files  <- get_files_by_patient(confirmed_paths, "expression")

        expr_sig <- function(pf) {
          if (is.null(pf)) return(list(tissues = character(0), goi = FALSE))
          tissues <- sort(unique(pf$tissues[!is.na(pf$tissues) & pf$tissues != "none"]))
          list(tissues = tissues, goi = !is.null(pf$files$goi))
        }

        for (pid in expression_patients) {
          old_sig <- expr_sig(prev_expr_files[[pid]])
          new_sig <- expr_sig(new_expr_files[[pid]])
          if (!identical(old_sig, new_sig)) {
            # Tissue list or GOI changed — remove the old tab so add_dataset_tabs re-creates it
            tab_val <- paste0("expr_", pid)
            net_val <- paste0("net_",  pid)
            if (tab_val %in% added_tab_values$expression) {
              removeTab(inputId = "expression_tabset", target = tab_val)
              added_tab_values$expression <- setdiff(added_tab_values$expression, tab_val)
            }
            if (net_val %in% added_tab_values$network) {
              removeTab(inputId = "network_graph", target = net_val)
              added_tab_values$network <- setdiff(added_tab_values$network, net_val)
            }
            message("🔄 Rebuilding expression tab for ", pid, " (tissue/goi changed)")
          }
        }

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
          
          # Extract input_files directory (static server root)
          # Remove everything after input_files to get the server root
          igv_server_root <- sub("(.*input_files).*", "\\1", root_path)
          message("root_path in main: ", root_path)
          message("igv_server_root in main: ", igv_server_root)
          
          shared_data$igv_root(igv_server_root)  
          shared_data$igv_server_started(TRUE)
      
          # session$onSessionEnded(function() {
          #   stop_static_server()
          # })
        }
        
        IGV$igv_server("igv", shared_data, root_path)
    }
      
      # ═══════════════════════════════════════════════════════════════════════════
      # FUSION PRERUN - PARALLEL processing for each patient
      # ═══════════════════════════════════════════════════════════════════════════
      
      # Start preprocessing only if user confirmed and not already started
      if (isTRUE(shared_data$fusion_prerun_user_confirmed()) && !shared_data$fusion_prerun_started()) {
        fusion_patients <- get_patients(confirmed_paths, "fusion")
        current_session_dir <- isolate(shared_data$session_dir())
        
        # Only proceed if we have a valid session directory
        if (is.null(current_session_dir) || current_session_dir == "") {
          message("[PRERUN INIT] No session directory - skipping fusion prerun")
        } else {
          # For patients that gained BAMs since their last prerun, the manifest will have
          # 0 expected PNGs (snapshots were skipped). Delete such manifests so
          # fusion_patients_to_prerun re-includes them for a snapshot run.
          current_fusion_files <- get_files_by_patient(confirmed_paths, "fusion")
          for (pid in fusion_patients) {
            pf       <- if (pid %in% names(current_fusion_files)) current_fusion_files[[pid]] else list()
            has_bams <- length(pf$tumor) > 0 || length(pf$chimeric) > 0
            if (has_bams) {
              manifest_file <- file.path(current_session_dir, "manifests", "fusion", paste0(pid, ".tsv"))
              if (file.exists(manifest_file)) {
                manifest_dt   <- tryCatch(data.table::fread(manifest_file), error = function(e) NULL)
                expected_pngs <- if (!is.null(manifest_dt) && "png_path" %in% names(manifest_dt)) {
                  data.table::uniqueN(manifest_dt$png_path, na.rm = TRUE)
                } else 0L
                if (expected_pngs == 0L) {
                  message("[PRERUN RESET] ", pid, " — BAMs newly available, deleting stale manifest to force snapshot re-run")
                  file.remove(manifest_file)
                  if (pid %in% names(shared_data$fusion_prerun_status)) {
                    shared_data$fusion_prerun_status[[pid]]("not_started")
                  }
                }
              }
            }
          }

          patients_to_run <- fusion_patients_to_prerun(fusion_patients, current_session_dir)

        if (length(patients_to_run) > 0) {
          message("[PRERUN INIT] Starting PARALLEL fusion prerun for ", length(patients_to_run), " patients: ", paste(patients_to_run, collapse = ", "))
          shared_data$fusion_prerun_started(TRUE)  # Set flag to prevent re-running
          
          # Clear old errors
          shared_data$fusion_prerun_errors(character())
        
        # Get file list for all fusion patients
        fusion_files <- get_files_by_patient(confirmed_paths, "fusion")

        # Switch to summary tab then dispatch mirai jobs.
        # The waiter stays visible until JS confirms the summary DOM is rendered
        # (observeEvent(input$summary_rendered) calls hide_waiter_progress).
        # This prevents the flash of upload-data UI between waiter-hide and tab-render.
        update_waiter_progress(session, 100, "Preparing summary...")
        updateNavbarTabs(session, "navbarMenu", selected = ns("summary"))
        wait_for_summary_rendered(session, ns)  # JS polls DOM, fires summary_rendered input
        # Server-side fallback: if JS message is lost (K8s proxy, slow browser),
        # force-hide after 3 s so the user is never permanently stuck.
        later(function() {
          if (!isolate(waiter_hidden())) {
            message("⏱️ Waiter fallback (3s) — forcing hide")
            hide_waiter_progress(session)
            waiter_hidden(TRUE)
            session$sendCustomMessage("data-loaded", list())
          }
        }, delay = 3)
        message("⏳ Waiter visible — waiting for summary DOM before hiding")

        # ═══════════════════════════════════════════════════════════════════════════
        # Launch one mirai job per patient.
        # mirai() NEVER blocks the main thread — tasks queue inside the daemon pool.
        # Pool size = detectCores()-1 (set at startup). All patients are dispatched
        # instantly; excess tasks wait in the queue without freezing Shiny.
        # ═══════════════════════════════════════════════════════════════════════════
        for (patient_id in patients_to_run) {
          message("[LAUNCH] Starting REAL background process for patient: ", patient_id)
          
          # Initialize per-patient tracking
          shared_data$fusion_prerun_status[[patient_id]] <- reactiveVal("running")
          message("[STATUS] ", patient_id, " -> running")
          shared_data$fusion_prerun_progress[[patient_id]] <- reactiveVal(0)
          shared_data$fusion_prerun_total_fusions[[patient_id]] <- reactiveVal(0)  # updated from future result
          
          # Create progress file for this patient
          prog_file  <- file.path(tempdir(), paste0("fusion_", patient_id, "_", as.integer(Sys.time()), ".progress"))
          count_file <- file.path(tempdir(), paste0("fusion_", patient_id, "_", as.integer(Sys.time()), ".count"))
          
          # Get file list for this patient
          file_list <- fusion_files[[patient_id]]
          
          # Use explicit globals list to speed up future creation
          current_session_dir <- isolate(shared_data$session_dir())
          
          # Get IGV genome selection
          # NOTE: pass "no_snapshot" through as-is — prerun_fusion_patient
          # uses it to skip IGV generation.  Do NOT replace it with "hg38" here.
          genome_val <- isolate(shared_data$igv_genome())
          current_igv_genome <- if (!is.null(genome_val) && length(genome_val) > 0) {
            genome_val
          } else {
            "hg38"  # Default when nothing selected
          }
          
          message("[MAIN] Dispatching mirai job for ", patient_id)

          # mirai() returns immediately — job is queued in the daemon pool.
          # If all daemons are busy the task waits in the queue WITHOUT blocking
          # the main R thread (unlike future() which would block).
          patient_job <- mirai(
            {
              prerun_fusion_patient(patient_id, file_list, prog_file,
                                    count_file  = count_file,
                                    session_dir = current_session_dir,
                                    igv_genome  = current_igv_genome)
            },
            patient_id            = patient_id,
            file_list             = file_list,
            prog_file             = prog_file,
            count_file            = count_file,
            current_session_dir   = current_session_dir,
            current_igv_genome    = current_igv_genome,
            prerun_fusion_patient = prerun_fusion_patient
          )

          message("[MAIN] mirai job dispatched for ", patient_id)

          shared_data$fusion_prerun_future[[patient_id]] <- list(
            mirai      = patient_job,
            prog_file  = prog_file,
            count_file = count_file,
            start_time = Sys.time()
          )
          
          # Create observer for this patient
          local({
            pid        <- patient_id
            pfile      <- prog_file
            cfile      <- count_file
            
            observer <- observe({
              invalidateLater(500)  # Check twice per second
              
              # Get future data
              fut_data <- shared_data$fusion_prerun_future[[pid]]
              if (is.null(fut_data)) return()
              
              # Check for timeout (max 30 minutes per patient)
              elapsed <- as.numeric(difftime(Sys.time(), fut_data$start_time, units = "mins"))
              if (elapsed > 30) {
                message("[TIMEOUT] ", pid, " exceeded 30 minutes - marking as failed")
                shared_data$fusion_prerun_status[[pid]]("failed")
                message("[STATUS] ", pid, " -> failed (timeout)")
                shared_data$fusion_prerun_progress[[pid]](0)
                
                # Track error
                current_errors <- shared_data$fusion_prerun_errors()
                shared_data$fusion_prerun_errors(c(current_errors, paste0(pid, ": Timeout after 30 minutes")))
                
                # Cleanup
                if (file.exists(pfile)) unlink(pfile)
                if (file.exists(cfile)) unlink(cfile)
                shared_data$fusion_prerun_future[[pid]] <- NULL
                observer$destroy()
                return()
              }
              
              # Read progress from file
              if (file.exists(pfile)) {
                p <- suppressWarnings(as.integer(readLines(pfile, n = 1)))
                if (!is.na(p)) {
                  shared_data$fusion_prerun_progress[[pid]](p)
                }
              }

              # Read fusion count as soon as worker writes it (within first 500ms tick)
              if (isolate(shared_data$fusion_prerun_total_fusions[[pid]]()) == 0 &&
                  file.exists(cfile)) {
                n <- suppressWarnings(as.integer(readLines(cfile, n = 1)))
                if (!is.na(n) && n > 0) {
                  shared_data$fusion_prerun_total_fusions[[pid]](n)
                  message("[FUSION COUNT] ", pid, " = ", n, " fusions (early read from count_file)")
                }
              }
              
              # Check if this patient's mirai job completed
              fut_data <- shared_data$fusion_prerun_future[[pid]]
              if (!is.null(fut_data) && !unresolved(fut_data$mirai)) {

                result <- tryCatch({
                  val <- fut_data$mirai$data

                  # mirai signals errors as a mirai_error object
                  if (inherits(val, "mirai_error")) {
                    stop(paste0("Worker error: ", as.character(val)))
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

                  # Print errors logged inside the worker
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

                # Update total_fusions count from worker result
                if (!is.null(result$total_fusions) && result$total_fusions > 0) {
                  shared_data$fusion_prerun_total_fusions[[pid]](result$total_fusions)
                  message("[FUSION COUNT] ", pid, " = ", result$total_fusions, " fusions")
                }

                # Update status
                new_status <- if (result$success) "completed" else "failed"
                shared_data$fusion_prerun_status[[pid]](new_status)
                message("[STATUS] ", pid, " -> ", new_status)
                shared_data$fusion_prerun_progress[[pid]](100)
                
                # Track errors
                if (!result$success && length(result$errors) > 0) {
                  current_errors <- shared_data$fusion_prerun_errors()
                  shared_data$fusion_prerun_errors(c(current_errors, result$errors))
                }
                
                # Cleanup
                if (file.exists(pfile)) unlink(pfile)
                if (file.exists(cfile)) unlink(cfile)
                shared_data$fusion_prerun_future[[pid]] <- NULL
                observer$destroy()
                
                message("[CLEANUP] Observer destroyed for ", pid)
              }
            })
            
            shared_data$fusion_prerun_observer[[pid]] <- observer
          })
        }
        
        message("[PARALLEL LAUNCH] All ", length(patients_to_run), " mirai jobs dispatched")
        
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
            message("[STATUS] ", patient_id, " -> completed (already processed)")
          }
          shared_data$fusion_prerun_started(TRUE)
        }
        }  # End of session_dir check
      } else if (!shared_data$fusion_prerun_user_confirmed()) {
        message("[PRERUN INIT] Fusion prerun waiting for user confirmation")
      } else {
        message("[PRERUN INIT] Fusion prerun already started - skipping")
      }

      # Fallback: for code paths that didn't go through the futures branch
      # (no patients to run, already processed, no fusion data, etc.)
      if (!isolate(waiter_hidden())) {
        update_waiter_progress(session, 100, "Preparing summary...")
        updateNavbarTabs(session, "navbarMenu", selected = ns("summary"))
        wait_for_summary_rendered(session, ns)
        later(function() {
          if (!isolate(waiter_hidden())) {
            message("\u23f1\ufe0f Waiter fallback (3s) \u2014 forcing hide (no futures path)")
            hide_waiter_progress(session)
            waiter_hidden(TRUE)
            session$sendCustomMessage("data-loaded", list())
          }
        }, delay = 3)
        message("\u23f3 Waiter visible \u2014 waiting for summary DOM (no futures path)")
      }

      # Reset flags for this data load
      shared_data$pending_data_load(NULL)
      shared_data$fusion_prerun_user_confirmed(FALSE)  # Reset to prevent re-triggering
  
    })
    
    # Save session button — registered once at server level (NOT inside the data-loading
    # observe block) so clicking Save always shows exactly one alert regardless of
    # how many times the user has re-confirmed data.
    observeEvent(input$save_session_btn, {
      shinyalert(
        title = "Save Session",
        text  = "Save your current work? This will preserve all selected variants, genes, and settings.",
        type  = "info",
        showCancelButton = TRUE,
        confirmButtonText = "Save",
        cancelButtonText  = "Cancel",
        callbackR = function(x) {
          if (isTRUE(x)) {
            session_dir <- shared_data$session_dir()
            if (!is.null(session_dir) && session_dir != "") {
              session_file <- file.path(session_dir, "session_data.json")
              save_session(session_file, shared_data)
              session$sendCustomMessage("allow-unload", list())
            } else {
              showNotification("⚠️ No session to save - please load data first.", type = "warning", duration = 5)
            }
          }
        }
      )
    }, ignoreInit = TRUE)

    # Hide waiter when Summary tab is fully rendered
    observeEvent(input$summary_rendered, {
      # Only hide waiter once per data load
      if (!waiter_hidden()) {
        message("✅ Summary tab fully rendered - hiding waiter")
        hide_waiter_progress(session)
        waiter_hidden(TRUE)
        
        # Signal JavaScript that data is loaded - enable beforeunload warning
        session$sendCustomMessage("data-loaded", list())
      }
    })
    
  })
}


# shinyApp(ui,server,options = list(launch.browser = TRUE))
