# app/logic/igv_helper.R

box::use(
  processx[process]
)

options(igv.port = 8080)

# -----------------------------------------------------------------------------
# IGV Genome Mapping Helpers
# -----------------------------------------------------------------------------

#' Get mapping between IGV genome IDs and display names
#' @export
get_igv_genome_map <- function() {
  c(
    "hg38" = "GRCh38/hg38",
    "hg38_1kg" = "hg38 1kg/GATK",
    "hg19" = "GRCh37/hg19",
    "hs1" = "T2T CHM13-v2.0/hs1",
    "no_snapshot" = "Dont create IGV snapshots"
  )
}

#' Convert IGV genome ID to display name
#' @param igv_id Genome ID (e.g., "hg38", "custom")
#' @param custom_genome_config Custom genome configuration (optional)
#' @return Display name (e.g., "GRCh38/hg38", "Custom")
#' @export
get_igv_genome_display_name <- function(igv_id, custom_genome_config = NULL) {
  if (is.null(igv_id) || length(igv_id) == 0 || igv_id == "") {
    return("GRCh38/hg38")  # Default
  }
  
  # Check if custom genome
  if (!is.null(custom_genome_config) && 
      !is.null(custom_genome_config$igv_id) &&
      igv_id == custom_genome_config$igv_id) {
    return("Custom")
  }
  
  # Use standard mapping
  genome_map <- get_igv_genome_map()
  if (igv_id %in% names(genome_map)) {
    return(genome_map[[igv_id]])
  }
  
  # Fallback
  return("GRCh38/hg38")
}

#' Convert display name to IGV genome ID
#' @param display_name Display name (e.g., "GRCh38/hg38", "Custom")
#' @param custom_genome_config Custom genome configuration (optional)
#' @return Genome ID (e.g., "hg38", "custom")
#' @export
get_igv_genome_id <- function(display_name, custom_genome_config = NULL) {
  if (is.null(display_name) || length(display_name) == 0 || display_name == "") {
    return("hg38")  # Default
  }
  
  # Check for special cases
  if (display_name == "Dont create IGV snapshots") {
    return("no_snapshot")
  }
  
  if (display_name == "Custom") {
    if (!is.null(custom_genome_config) && !is.null(custom_genome_config$igv_id)) {
      return(custom_genome_config$igv_id)
    } else {
      warning("Custom genome selected but not configured, using hg38")
      return("hg38")
    }
  }
  
  # Reverse lookup in standard mapping
  genome_map <- get_igv_genome_map()
  genome_id <- names(genome_map)[genome_map == display_name]
  
  if (length(genome_id) > 0) {
    return(genome_id[1])
  }
  
  # Fallback
  return("hg38")
}

# -----------------------------------------------------------------------------
# IGV URL Building
# -----------------------------------------------------------------------------

#' Build base URL for IGV static server
#' @return Base URL string (e.g., "http://localhost:8081" or "https://example.com/igv-static")
#' @export
get_igv_base_url <- function() {
  igv_host <- Sys.getenv("IGV_HOST", "localhost")
  igv_port <- Sys.getenv("IGV_PORT", "8081")
  igv_path_prefix <- Sys.getenv("IGV_PATH_PREFIX", "")
  
  # Determine protocol based on port (443 = HTTPS, otherwise HTTP)
  protocol <- if (igv_port == "443") "https" else "http"
  
  # Build base URL (omit port for standard 80/443)
  if ((protocol == "http" && igv_port == "80") || (protocol == "https" && igv_port == "443")) {
    sprintf("%s://%s%s", protocol, igv_host, igv_path_prefix)
  } else {
    sprintf("%s://%s:%s%s", protocol, igv_host, igv_port, igv_path_prefix)
  }
}

#' Build custom genome configuration for IGV.js
#' @param custom_genome_config Custom genome configuration from reference_paths.json
#' @return JavaScript object string for custom genome
#' @export
build_custom_genome <- function(custom_genome_config) {
  if (is.null(custom_genome_config)) {
    stop("custom_genome_config is NULL")
  }
  
  base_url <- get_igv_base_url()
  
  # Build URLs from paths in config (relative to /input_files mount)
  fasta_url <- sprintf("%s/%s", base_url, custom_genome_config$genome)
  fai_url <- sprintf("%s/%s", base_url, custom_genome_config$index)
  
  message("[IGV] Custom genome URLs (client-side browser access):")
  message("  FASTA: ", fasta_url)
  message("  INDEX: ", fai_url)
  
  sprintf("{
          id: '%s',
          name: '%s',
          fastaURL: '%s',
          indexURL: '%s'
        }",
    custom_genome_config$igv_id,
    custom_genome_config$display_name,
    fasta_url,
    fai_url
  )
}

# -----------------------------------------------------------------------------
# IGV Track Building
# -----------------------------------------------------------------------------

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
build_igv_tracks <- function(samples, igv_root = NULL) {
  if (is.null(samples) || length(samples) == 0) {
    warning("⚠️  build_igv_tracks: prázdný vstup samples.")
    return("")
  }
  
  # Get base URL using centralized helper
  base_url <- get_igv_base_url()
  
  message("[IGV TRACKS] Building tracks for ", length(samples), " BAM files")
  message("[IGV TRACKS] Base URL: ", base_url)
  if (!is.null(igv_root)) {
    message("[IGV TRACKS] IGV_ROOT: ", igv_root)
  }
  
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
    
    if (!bam_exists) {
      warning("⚠️  BAM file does not exist: ", bam_path)
    }
    if (!bai_exists) {
      warning("⚠️  BAI index does not exist: ", bam_path, ".bai")
    }
    
    # Convert absolute path to relative path for IGV static server
    relative_path <- bam_path
    if (!is.null(igv_root) && nzchar(igv_root)) {
      # Remove igv_root prefix and any leading slashes
      relative_path <- sub(paste0("^", igv_root, "/?"), "", bam_path)
      message("[IGV TRACKS]   Relative path: ", relative_path)
    }
    
    # Build full URL (base_url already contains protocol, host, port, and prefix)
    full_url <- sprintf("%s/%s", base_url, relative_path)
    message("[IGV TRACKS]   URL: ", full_url)
    
    # Generuj IGV track JSON (pro JS interpretaci)
    sprintf(
      "{
        name: '%s',
        url: '%s',
        indexURL: '%s.bai',
        format: 'bam'
      }",
      sample$name, full_url, full_url
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



