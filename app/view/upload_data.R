 box::use(
  shiny[NS,tagList,fileInput,moduleServer,observe,reactive,textOutput,updateTextInput,renderText,req,textInput,observeEvent,textAreaInput,column,fluidRow,reactiveVal,
        isTruthy,actionButton,icon,updateTextAreaInput,uiOutput,renderUI,bindEvent,fluidPage,radioButtons,verbatimTextOutput,renderPrint,
        outputOptions,conditionalPanel,reactiveValues],
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
  app/view/upload_data_step2
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
 
 server <- function(id) {
   moduleServer(id, function(input, output, session) {
     ns <- session$ns
     step <- reactiveVal(1)
     patients <- reactiveVal(character(0))
     path     <- reactiveVal(NULL)
     datasets <- reactiveVal(character(0))
     tumor_pattern <- reactiveValues(somatic = NULL, fusion = NULL, chimeric = NULL)
     normal_pattern  <- reactiveValues(somatic = NULL, germline = NULL)
     confirmed_paths_state <- reactiveVal(NULL)
     
     step1 <- upload_data_step1$step1_server("first_step",  path, patients, datasets, tumor_pattern, normal_pattern)
     step2 <- upload_data_step2$step2_server("second_step", path, patients, datasets, tumor_pattern, normal_pattern)
     # step2 <- upload_data_step2$step2_server("second_step",  path=reactiveVal("/Users/katerinajuraskova/Desktop/sequiaViz/input_files/MOII_e117"), patients=reactiveVal(c("DZ1601","MR1507","P001")), datasets =reactiveVal("somatic"), tumor_pattern=NULL, normal_pattern=NULL)

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
     
     return(list(confirmed_paths = reactive(confirmed_paths_state())))
   })
 }

