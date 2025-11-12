# Test background process to verify async execution works

box::use(
  future[future, value, resolved]
)

box::export(test_background_worker_single, test_background_worker)

#' Test background process for a SINGLE patient (for parallel execution)
#' @param prog_file Path to progress file for cross-process communication
#' @param patient_id Patient ID to simulate processing
#' @export
test_background_worker_single <- function(prog_file, patient_id) {
  message(sprintf("=== TEST WORKER STARTED for %s ===", patient_id))
  
  # Patient-specific delays (simulate different dataset sizes)
  patient_delays <- list(
    DZ1601 = 4,   # Medium patient: 4 seconds per step
    MR1507 = 8    # Slow patient: 8 seconds per step (large dataset)
  )
  
  delay <- if (patient_id %in% names(patient_delays)) patient_delays[[patient_id]] else 3
  
  message(sprintf("[TEST] %s: Processing with %ds delay per step", patient_id, delay))
  
  # Simulate 3 main processing steps
  steps <- list(
    list(progress = 0, delay = 0, message = "Starting..."),
    list(progress = 20, delay = delay, message = "Creating IGV snapshots..."),
    list(progress = 50, delay = delay, message = "Converting PDF to SVG..."),
    list(progress = 80, delay = delay * 0.7, message = "Creating manifest..."),
    list(progress = 100, delay = 0.5, message = "Complete!")
  )
  
  for (step in steps) {
    message(sprintf("[TEST] %s - Progress: %d%% - %s", patient_id, step$progress, step$message))
    writeLines(as.character(step$progress), prog_file)
    
    if (step$delay > 0) {
      Sys.sleep(step$delay)
    }
  }
  
  message(sprintf("=== TEST WORKER COMPLETED for %s ===", patient_id))
  
  list(success = TRUE, errors = character(), progress = 100, patient_id = patient_id)
}


#' Legacy test worker for multiple patients (kept for reference)
#' @param prog_file Path to progress file for cross-process communication
#' @param patients Vector of patient IDs to simulate processing
#' @export
test_background_worker <- function(prog_file, patients = c("DZ1601", "MR1507")) {
  message("=== TEST BACKGROUND PROCESS STARTED ===")
  message("Simulating processing for patients: ", paste(patients, collapse = ", "))
  
  # Patient-specific delays (MR1507 has more data = longer processing)
  patient_delays <- list(
    DZ1601 = 2,   # Fast patient: 2 seconds per step
    MR1507 = 4    # Slow patient: 4 seconds per step (larger dataset)
  )
  
  total_steps <- length(patients) * 3  # 3 steps per patient (IGV, PDF, manifest)
  current_step <- 0
  
  for (patient in patients) {
    delay <- if (patient %in% names(patient_delays)) patient_delays[[patient]] else 2
    
    message(sprintf("[TEST] Processing patient: %s (delay: %ds per step)", patient, delay))
    
    # Step 1: IGV snapshots
    current_step <- current_step + 1
    progress <- round((current_step / total_steps) * 100)
    message(sprintf("[TEST] %s - Progress: %d%% - IGV snapshots...", patient, progress))
    writeLines(as.character(progress), prog_file)
    Sys.sleep(delay)
    
    # Step 2: PDF to SVG conversion
    current_step <- current_step + 1
    progress <- round((current_step / total_steps) * 100)
    message(sprintf("[TEST] %s - Progress: %d%% - PDF conversion...", patient, progress))
    writeLines(as.character(progress), prog_file)
    Sys.sleep(delay)
    
    # Step 3: Create manifest
    current_step <- current_step + 1
    progress <- round((current_step / total_steps) * 100)
    message(sprintf("[TEST] %s - Progress: %d%% - Creating manifest...", patient, progress))
    writeLines(as.character(progress), prog_file)
    Sys.sleep(delay / 2)  # Manifest is faster
  }
  
  # Final completion
  writeLines("100", prog_file)
  message("=== TEST BACKGROUND PROCESS COMPLETED ===")
  
  list(success = TRUE, errors = character(), progress = 100)
}
