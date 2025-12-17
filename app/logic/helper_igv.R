# app/logic/igv_helper.R

box::use(
  processx[process]
)

options(igv.port = 8080)

# Vygeneruje track objekty pro JavaScript
#' @export
# app/logic/helper_igv.R
# -----------------------------------------------------------------------------
# Tato verze helperu je určena pro Docker Compose architekturu se dvěma kontejnery:
# - sequiaviz-shiny   ... Shiny aplikace (běží na portu 8080)
# - igv-static        ... Node.js http-server (běží na portu 8081, sdílí /input_files)
#
# IGV browser v Shiny bude načítat data z http://igv-static:8081/<soubor>
# -----------------------------------------------------------------------------

#' @export
build_igv_tracks <- function(samples) {
  if (is.null(samples) || length(samples) == 0) {
    warning("⚠️  build_igv_tracks: prázdný vstup samples.")
    return("")
  }
  
  # V docker-compose se hostname "igv-static" automaticky překládá na druhý kontejner.
  igv_host <- Sys.getenv("IGV_HOST", "localhost")
  igv_port <- Sys.getenv("IGV_PORT", "8081")
  
  message("[IGV TRACKS] Building tracks for ", length(samples), " BAM files")
  message("[IGV TRACKS] IGV_HOST=", igv_host, ", IGV_PORT=", igv_port)
  
  tracks <- lapply(samples, function(sample) {
    # Ošetření: každý sample musí mít položky name a file
    if (is.null(sample$name) || is.null(sample$file)) {
      warning("⚠️  Sample chybí 'name' nebo 'file': ", paste(sample, collapse = ", "))
      return(NULL)
    }
    
    # Diagnostic: Check if BAM file exists on disk
    bam_path <- sample$file
    bam_exists <- file.exists(bam_path)
    bai_exists <- file.exists(paste0(bam_path, ".bai"))
    
    message("[IGV TRACKS] Track: ", sample$name)
    message("[IGV TRACKS]   BAM: ", bam_path, " → ", if(bam_exists) "✓ EXISTS" else "✗ NOT FOUND")
    message("[IGV TRACKS]   BAI: ", bam_path, ".bai → ", if(bai_exists) "✓ EXISTS" else "✗ NOT FOUND")
    message("[IGV TRACKS]   URL: http://", igv_host, ":", igv_port, "/", bam_path)
    
    if (!bam_exists) {
      warning("⚠️  BAM file does not exist: ", bam_path)
    }
    if (!bai_exists) {
      warning("⚠️  BAI index does not exist: ", bam_path, ".bai")
    }
    
    # Generuj IGV track JSON (pro JS interpretaci)
    sprintf(
      "{
        name: '%s',
        url: 'http://%s:%s/%s',
        indexURL: 'http://%s:%s/%s.bai',
        format: 'bam'
      }",
      sample$name, igv_host, igv_port, sample$file,
      igv_host, igv_port, sample$file
    )
  })
  
  # Odstraň NULL a spoj do jednoho bloku
  tracks <- Filter(Negate(is.null), tracks)
  paste(tracks, collapse = ",\n")
}

# build_igv_tracks <- function(samples) {
#   port <- getOption("igv.port")
#   tracks <- lapply(samples, function(sample) {
#     sprintf("{
#               name: '%s',
#               url: 'http://127.0.0.1:%d/%s',
#               indexURL: 'http://127.0.0.1:%d/%s.bai',
#               format: 'bam'
#             }", sample$name, port, sample$file, port, sample$file)
#   })
#   paste(tracks, collapse = ",\n")
# }





# Spouští CORS-kompatibilní server pomocí npx http-server přes processx
## !! nakonci cesty NESMÍ být lomítko !!
## !! cesta musí být úplná !!
#' @export
# start_static_server <- function(dir) {
#   port <- getOption("igv.port")
# 
#   if (!requireNamespace("processx", quietly = TRUE)) {
#     stop("Balíček 'processx' není nainstalovaný. Nainstaluj ho přes install.packages('processx').")
#   }
# 
#   # Zjisti, jestli port už něco používá
#   server_check <- system(paste0("lsof -ti tcp:", port), intern = TRUE)
# 
#   # Pokud ano – zabij starý proces
#   if (length(server_check) > 0) {
#     message("⚠️ Port ", port, " je obsazený. Ukončuji předchozí proces...")
#     for (pid in server_check) {
#       system(paste("kill -9", pid))
#     }
#     Sys.sleep(1)
#   }
# 
#   # Spusť server
#   assign("cors_server", process$new(
#     "npx",
#     c("http-server", dir, "-p", as.character(port), "--cors", "--no-cache"),
#     stdout = NULL, stderr = NULL,
#     supervise = TRUE
#   ), envir = .GlobalEnv)
# 
#   Sys.sleep(1)
#   message("✅ IGV statický server běží na http://127.0.0.1:", port)
# }

#' @export
# stop_static_server <- function() {
#   if (exists("cors_server", envir = .GlobalEnv, inherits = FALSE)) {
#     proc <- get("cors_server", envir = .GlobalEnv)
# 
#     if (!is.null(proc) && inherits(proc, "process") && proc$is_alive()) {
#       proc$kill()
#       message("🛑 IGV statický server byl ukončen.")
#     }
# 
#     rm("cors_server", envir = .GlobalEnv)
#   } else {
#     message("ℹ️ Žádný IGV statický server neběží (proměnná neexistuje).")
#   }
# }



