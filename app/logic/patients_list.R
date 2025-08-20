#' # app/logic/patients_list.R
#' #
#' box::use(
#'   shinyWidgets[virtualSelectInput,dropdown],
#'   shiny[h3,tags,icon],
#' )
#' 
#' 
#' #' #' @export
#' #' patients_list <- function(){
#' #'   patients <- c("FZ0711")
#' #'   return(patients)
#' #' }
#' #' 
#' #' #' @export
#' #' sample_list_fuze <- function(){
#' #'   patients <- c("FZ0711fuze")
#' #'   return(patients)
#' #' }
#' #' 
#' #' #' @export
#' #' sample_list_som <- function(){
#'   # patients <- c("FZ0711")
#' #'   return(patients)
#' #' }
#' #' 
#' #' #' @export
#' #' sample_list_germ <- function(){
#' #'   patients <- c("FZ0711krev")
#' #'   return(patients)
#' #' }
#' #' 
#' #' #' @export
#' #' sample_list_expr <- function(){
#' #'   patients <- c("FZ0711")
#' #'   return(patients)
#' #' }
#' 
#' #' @export
#' patients_list <- function(){
#'   patients <- c("DZ1601","LK0302","MR1507","VH0452")
#'   return(patients)
#' }
#' 
#' #' @export
#' sample_list_fuze <- function(){
#'   patients <- c("DZ1601fuze","LK0302fuze","MR1507fuze","VH0452fuze")
#'   return(patients)
#' }
#' 
#' #' @export
#' sample_list_som <- function(){
#'   patients <- c("DZ1601","MR1507","VH0452")
#'   return(patients)
#' }
#' 
#' #' @export
#' sample_list_germ <- function(){
#'   patients <- c("DZ1601krev","MR1507krev","VH0452krev")
#'   return(patients)
#' }
#' 
#' #' @export
#' sample_list_expr <- function(){
#'   patients <- c("DZ1601","MR1507")
#'   return(patients)
#' }
#' 
#' #' @export
#' set_patient_to_sample <- function(tag){
#'   if (tag == "germline") {
#'     sample_list <- sample_list_germ()
#'   } else if (tag == "somatic") {
#'     sample_list <- sample_list_som()
#'   } else if (tag == "fusion"){
#'     sample_list <- sample_list_fuze()
#'   } else if (tag == "expression"){
#'     sample_list <- sample_list_expr()
#'   } else {
#'     stop("Invalid tag. Use 'somatic','germline','fusion' or 'expression'.")
#'   }
#'   
#'   patients_with_samples <- patients_list()[sapply(patients_list(), function(patient) {
#'     any(sapply(sample_list, function(sample) grepl(patient, sample)))
#'   })]
#'   
#'   named_samples <- sample_list[sapply(sample_list, function(sample) {
#'     any(sapply(patients_with_samples, function(patient) grepl(patient, sample)))
#'   })]
#'   
#'   names(named_samples) <- sapply(named_samples, function(sample) {
#'     patient <- patients_with_samples[sapply(patients_with_samples, function(patient) grepl(patient, sample))]
#'     return(patient)
#'   })
#'   
#'   return(named_samples)
#' }
#' 
#' # patient_list <- function(analysis_type){
#' #
#' #   if(analysis_type == "fusion"){
#' #     patients <- c("DZ1601fuze","LK0302fuze","MR1507fuze","VH0452fuze")
#' #   } else if (analysis_type == "somatic"){
#' #     patients <- c("DZ1601","MR1507","VH0452")
#' #   } else if (analysis_type == "germinal"){
#' #     patients <- c("DZ1601","MR1507","VH0452")
#' #   } else {
#' #     patients <- NULL
#' #   }
#' #   return(patients)
#' # }
#' 
#' 
#' 
#' # #' @export
#' # ui <- function(id) {
#' #   ns <- NS(id)
#' #
#' #
#' # }
#' 
#' 
#' # #' @export
#' # server <- function(id, data) {
#' #   ns <- NS(id)
#' #   moduleServer(id, function(input, output, session) {
#' #
#' #   })
#' # }
