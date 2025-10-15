 box::use(
  shiny[NS,tagList,fileInput,moduleServer,observe,reactive,textOutput,updateTextInput,renderText,req,textInput,observeEvent,textAreaInput,column,fluidRow,reactiveVal,
        isTruthy,actionButton,icon,updateTextAreaInput,uiOutput,renderUI,bindEvent,fluidPage,radioButtons,verbatimTextOutput,renderPrint,reactiveValuesToList,showNotification,
        outputOptions,conditionalPanel,reactiveValues,isolate],
  htmltools[tags,HTML,div,span,h2,h4,br],
  shinyFiles[shinyDirButton,shinyDirChoose,parseDirPath,getVolumes],
  shinyWidgets[prettySwitch,updatePrettySwitch,pickerInput,updatePickerInput, dropdownButton,tooltipOptions,radioGroupButtons],
  bs4Dash[addPopover,box,updateNavbarTabs],
  stringi[stri_detect_regex],
  stringr[str_detect,regex],
  shinyalert[shinyalert],
  # shinyjs[useShinyjs,runjs],
)
# 
 box::use(
  app/view/upload_data_step1,
  app/view/upload_data_step2,
  app/logic/session_utils[create_session_handlers,safe_extract, register_module, load_session],

 )
 
 ui <- function(id) {
   ns <- NS(id)
   tagList(
     # tags$head(tags$script("window.addEventListener('beforeunload', function(e) {
     #                        e.preventDefault();
     #                        e.returnValue = 'Are you sure you want to refresh? All your progress will be lost.';});")),
     conditionalPanel(
       condition = sprintf("output['%s'] == 1", ns("step")),
       upload_data_step1$step1_ui(ns("first_step"))
     ),
     conditionalPanel(
       condition = sprintf("output['%s'] == 2", ns("step")),
       upload_data_step2$step2_ui(ns("second_step"))
     )
   )
 }
 
 server <- function(id, shared_data) {
   moduleServer(id, function(input, output, session) {
     ns <- session$ns
     step <- reactiveVal(1)
     # patients <- reactiveVal(character(0))
     # path     <- reactiveVal(NULL)
     # datasets <- reactiveVal(character(0))
     # tumor_pattern <- reactiveValues(somatic = NULL, fusion = NULL, chimeric = NULL, arriba = NULL)
     # normal_pattern  <- reactiveValues(somatic = NULL, germline = NULL)
     # tissues <- reactiveVal(NULL)
     #####
     # patients <- reactiveVal(c("DZ1601","MR1507"))
     # path     <- reactiveVal("/home/katka/BioRoots/sequiaViz/input_files/MOII_e117")
     # # path     <- reactiveVal("/Users/katerinajuraskova/Desktop/sequiaViz/input_files/MOII_e117")
     # datasets <- reactiveVal(c("somatic","germline","fusion","expression")) #
     # tumor_pattern <- reactiveValues(somatic = NULL, fusion = "fuze", chimeric = "chimeric", arriba = NULL)
     # normal_pattern  <- reactiveValues(somatic = NULL, germline = NULL)
     # tissues <- reactiveVal(c("Blood","Blood_Vessel"))
     #####
     patients <- reactiveVal("DZ1601")
     # patients <- reactiveVal(c("DZ1601","MR1507"))
     path     <- reactiveVal("/home/katka/BioRoots/sequiaViz/input_files/MOII_e117")
     datasets <- reactiveVal("expression")
     tumor_pattern <- reactiveValues(somatic = NULL, fusion = NULL, chimeric = NULL, arriba = NULL)
     normal_pattern  <- reactiveValues(somatic = NULL, germline = NULL)
     tissues <- reactiveVal(c("blood","blood_vessel"))
     
     confirmed_paths_state <- reactiveVal(NULL)
     
     step1 <- upload_data_step1$step1_server("first_step",  path, patients, datasets, tumor_pattern, normal_pattern, tissues)
     # step2 <- upload_data_step2$step2_server("second_step", path, patients, datasets, tumor_pattern, normal_pattern, tissues)
     # step2 <- upload_data_step2$step2_server("second_step",  path=path, patients=patients, datasets =reactiveVal(c("somatic","germline","fusion","expression")), tumor_pattern=reactiveValues(chimeric = "chimeric",fusion = "fuze"), normal_pattern=NULL, tissues = reactiveVal(c("Blood","Blood_Vessel")))
     step2 <- upload_data_step2$step2_server("second_step",  path=path, patients=patients, datasets =reactiveVal(c("expression")), tumor_pattern=NULL, normal_pattern=NULL, tissues = reactiveVal(c("blood","blood_vessel")))
     
     output$step <- renderText(step())
     outputOptions(output, "step", suspendWhenHidden = FALSE) # zajistí, že inputy běží i když jsou skryté

     observeEvent(step1$next1(), {
       if (!is.null(step1$next1())) {
         step(2)
       }
     })
     observeEvent(step2$prev2(), step(1))
     

     observeEvent(step2$confirmed_paths(), {
       confirmed_paths_state(step2$confirmed_paths())
     }, ignoreInit = TRUE)
     
     
     methods <- list(
       get_session_data = reactive({
         list(
           step            = step(),
           patients        = patients(),
           path            = path(),
           datasets        = datasets(),
           tissues         = tissues(),
           tumor_pattern   = reactiveValuesToList(tumor_pattern,  all.names = TRUE),
           normal_pattern  = reactiveValuesToList(normal_pattern, all.names = TRUE),
           confirmed_paths = confirmed_paths_state()  # volitelné; můžeš vypustit, pokud je to velké
         )
       }),
       restore_session_data = function(state) {
         if (!is.null(state$step))            step(state$step)
         if (!is.null(state$path))            path(state$path)
         if (!is.null(state$patients))        patients(state$patients)
         if (!is.null(state$datasets))        datasets(state$datasets)
         if (!is.null(state$tissues))         tissues(state$tissues)
         
         if (!is.null(state$tumor_pattern) && length(state$tumor_pattern)) {
           for (nm in names(state$tumor_pattern)) tumor_pattern[[nm]] <- state$tumor_pattern[[nm]]
         }
         if (!is.null(state$normal_pattern) && length(state$normal_pattern)) {
           for (nm in names(state$normal_pattern)) normal_pattern[[nm]] <- state$normal_pattern[[nm]]
         }
         
         if (!is.null(state$confirmed_paths)) confirmed_paths_state(state$confirmed_paths)
         
         if (!is.null(step1$restore_ui_inputs)) step1$restore_ui_inputs()
       }
     )
     
     # zaregistruj modul (id zvol klidně pevně "upload" – nebo z `id`)
     observe({
       register_module(shared_data, "upload", module_id = "upload", methods)
     })
     
     observeEvent(step1$load_request(), {
       shinyalert(
         title = "Confirm Load",
         text  = "Do you really want to load the session? This will overwrite current selections.",
         type  = "warning",
         showCancelButton = TRUE,
         confirmButtonText = "Yes, load it",
         cancelButtonText  = "Cancel",
         callbackR = function(ok) {
           if (isTRUE(ok)) {
             session_base <- "sessions"

             if (!dir.exists(session_base)) {
               showNotification("❌ No sessions directory found!", type = "error")
               return()
             }
             
             session_dirs <- list.dirs(session_base, full.names = TRUE, recursive = FALSE)
             
             if (length(session_dirs) == 0) {
               showNotification("❌ No session directories found!", type = "error")
               return()
             }
             
             latest_session <- session_dirs[which.max(file.info(session_dirs)$mtime)]
             session_file <- file.path(latest_session, "session_data.json")

             if (!file.exists(session_file)) {
               showNotification(paste("❌ Session file not found:", session_file), type = "error")
               return()
             }
             
             abs_session_dir <- normalizePath(latest_session, mustWork = TRUE)
             shared_data$session_dir(abs_session_dir)
             shared_data$is_loading_session(TRUE)

             load_session(session_file, shared_data)
             
             showNotification("✅ Session successfully loaded.", type = "message")
             step(2)
           } else {
             showNotification("Loading session was canceled.", type = "default")
           }
         }
       )
     })
     
     return(list(confirmed_paths = reactive(confirmed_paths_state())))
   })
 }

