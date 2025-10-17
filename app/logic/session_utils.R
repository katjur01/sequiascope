# app/logic/session_utils.R
box::use(
  shiny[reactive,isolate,showNotification, getDefaultReactiveDomain, reactiveVal, invalidateLater],
  jsonlite[read_json,write_json],
  data.table[rbindlist, as.data.table, fread, first, uniqueN, setnames],
  readxl[read_excel,cell_limits],
  tools[file_path_sans_ext,file_ext],
)

#' @export
safe_extract <- function(x) {
  if (is.list(x) && length(x) == 0) return(NULL)
  else return(x)
}
#' @export
nz <- function(x, default) if (is.null(x) || !length(x)) default else x
#' @export
ch <- function(x) trimws(as.character(x))

# ============================================
# GENERAL MODUL REGISTR SYSTEM
# ============================================

create_module_registry <- function(shared_data, module_type) {
  list(
    register = function(module_id, methods) {
      registry_key <- paste0(module_type, "_modules")
      pending_key <- paste0(module_type, "_pending")
      
      # Přidej do registry
      reg <- isolate(shared_data[[registry_key]]()); if (is.null(reg)) reg <- list()
      reg[[module_id]] <- methods
      shared_data[[registry_key]](reg)
      
      # Zkontroluj pending restore
      pend <- isolate(shared_data[[pending_key]]())
      if (!is.null(pend) && !is.null(pend[[module_id]])) {
        state <- pend[[module_id]]
        if (!is.null(methods$restore_session_data)) methods$restore_session_data(state)
        pend[[module_id]] <- NULL
        shared_data[[pending_key]](pend)
      }
    },
    
    get_registry = function() {
      registry_key <- paste0(module_type, "_modules")
      isolate(shared_data[[registry_key]]())
    },
    
    get_pending = function() {
      pending_key <- paste0(module_type, "_pending")
      isolate(shared_data[[pending_key]]())
    },
    
    set_pending = function(pending_data) {
      pending_key <- paste0(module_type, "_pending")
      shared_data[[pending_key]](pending_data)
    }
  )
}

#' @export
create_session_handlers <- function(selected_inputs, filter_state, is_restoring = NULL, session = getDefaultReactiveDomain()) {

  restoring_session <- if (!is.null(is_restoring)) is_restoring else reactiveVal(FALSE)
  
  get_session_data <- reactive({
    lapply(selected_inputs, function(x) x())
  })
  
  restore_session_data <- function(data) {
    message("🔄 Starting session restore")

    restoring_session(TRUE)

    for (nm in names(data)) {
      if (!is.null(data[[nm]]) && nm %in% names(selected_inputs)) {
        selected_inputs[[nm]](safe_extract(data[[nm]]))
      }
    }

    if (!is.null(filter_state$restore_ui_inputs)) {
      session$onFlushed(function() {
        session$onFlushed(function() {
          filter_state$restore_ui_inputs(data)

          session$onFlushed(function() {
            restoring_session(FALSE)
            message("✅ Session restore completed")
          }, once = TRUE)
          
        }, once = TRUE)
      }, once = TRUE)
    } else {
      restoring_session(FALSE)
    }
  }
  
  list(
    get_session_data = get_session_data,
    restore_session_data = restore_session_data,
    is_restoring = restoring_session
  )
}

# ============================================
# GENERAL LOAD/SAVE FUNCTIONS
# ============================================

#' @export
load_session <- function(file, shared_data, module_configs = NULL) {
  if (!file.exists(file)) {
    showNotification(paste("❌ Session file not found:", file), type = "error")
    return(invisible(NULL))
  }
  
  message("📄 Reading session file: ", file)
  session_json <- read_json(file, simplifyVector = TRUE)
  current_session_dir <- isolate(shared_data$session_dir())
  
  if (!is.null(session_json$data)) {
    session_data <- session_json$data
  } else {
    session_data <- session_json
  }
  
  if (is.null(module_configs)) module_configs <- get_default_module_configs()
  
  for (module_type in names(module_configs)) {
    if (!is.null(session_data[[module_type]])) {
      config <- module_configs[[module_type]]
      restore_module_type(
        module_type = module_type,
        session_data = session_data[[module_type]],
        shared_data = shared_data,
        config = config
      )
    }
  }
  
  invalidateLater(2000, getDefaultReactiveDomain())
  return(invisible(NULL))
}

restore_module_type <- function(module_type, session_data, shared_data, config) {
  
  registry_key <- paste0(module_type, "_modules")
  pending_key <- paste0(module_type, "_pending")
  
  reg <- shared_data[[registry_key]]()
  pend <- shared_data[[pending_key]]()
  
  for (module_id in names(session_data)) {
    state <- session_data[[module_id]]
    m <- reg[[module_id]]
    
    if (!is.null(m)) {
      if (!is.null(m$restore_session_data)) m$restore_session_data(state)
      if (!is.null(m$filter_state) && !is.null(m$filter_state$restore_ui_inputs)) {
        m$filter_state$restore_ui_inputs(state)
      }
    } else {
      pend[[module_id]] <- state
    }
  }
  
  shared_data[[pending_key]](pend)
  
  if (!is.null(config$post_restore)) config$post_restore(session_data, shared_data)
}

#' @export
save_session <- function(file = "session_data.json", shared_data, module_types = NULL) {
  
  if (is.null(module_types)) module_types <- get_available_module_types(shared_data)
  
  session <- list()
  
  for (module_type in module_types) {
    registry_key <- paste0(module_type, "_modules")
    reg <- shared_data[[registry_key]]()
    
    if (!is.null(reg) && length(reg) > 0) {
      module_data <- lapply(names(reg), function(module_id) {
        isolate(reg[[module_id]]$get_session_data())
      })
      names(module_data) <- names(reg)
      session[[module_type]] <- module_data
    }
  }
  
  session_metadata <- list(
    data = session,
    session_dir = nz(shared_data$session_dir(), NULL),
    saved_at = Sys.time()
  )
  
  write_json(session_metadata, file, auto_unbox = TRUE, pretty = TRUE, na = "null")
  showNotification(paste("Session saved to", file), type = "message")
}

# ============================================
# KONFIGURATION FUNCTIONS
# ============================================

get_default_module_configs <- function() {
  list(
    somatic = list(
      post_restore = function(session_data, shared_data) {
        combined_somatic <- rbindlist(
          lapply(names(session_data), function(pat) {
            st <- session_data[[pat]]
            sv <- st$selected_vars
            if (is.null(sv)) return(NULL)
            dt <- as.data.table(sv)
            if (nrow(dt) == 0) return(NULL)    # <--- PŘIDAT TENTO ŘÁDEK
            if (!"sample" %in% names(dt)) dt[, sample := pat]
            dt
          }),
          use.names = TRUE, fill = TRUE
        )
        
        shared_data$somatic.variants(combined_somatic)
      }
    ),
    
    upload = list(
      post_restore = NULL  # Upload nepotřebuje speciální handling
    ),
    
    germline = list(
      post_restore = function(session_data, shared_data) {
        combined_germline <- rbindlist(
          lapply(names(session_data), function(pat) {
            st <- session_data[[pat]]
            sv <- st$selected_vars
            if (is.null(sv)) return(NULL)
            dt <- as.data.table(sv)
            if (nrow(dt) == 0) return(NULL)    # <--- PŘIDAT TENTO ŘÁDEK
            if (!"sample" %in% names(dt)) dt[, sample := pat]
            dt
          }),
          use.names = TRUE, fill = TRUE
        )
        
        shared_data$germline.variants(combined_germline)
      }
    ),
    
    fusion = list(
      post_restore = function(session_data, shared_data) {
        combined_fusion <- rbindlist(
          lapply(names(session_data), function(pat) {
            st <- session_data[[pat]]
            sv <- st$selected_vars
            if (is.null(sv)) return(NULL)
            dt <- as.data.table(sv)
            if (nrow(dt) == 0) return(NULL)    # <--- PŘIDAT TENTO ŘÁDEK
            if (!"sample" %in% names(dt)) dt[, sample := pat]
            dt
          }),
          use.names = TRUE, fill = TRUE
        )
        
        shared_data$fusion.variants(combined_fusion)
      }
    ),
    # v get_default_module_configs() -> expression$post_restore = function(session_data, shared_data) { ... }
    
    expression = list(
      post_restore = function(session_data, shared_data) {
        as_dt_safe <- function(x) {
          if (is.null(x)) return(NULL)
          if (is.data.frame(x)) return(as.data.table(x))
          if (is.list(x) && length(x) > 0) return(as.data.table(x))
          NULL
        }
        
        all_genes_data <- list()
        goi_data <- list()
        
        for (patient in names(session_data)) {
          patient_data <- session_data[[patient]]
          
          # ALL GENES
          if (!is.null(patient_data$all_genes)) {
            sel <- patient_data$all_genes$selected_genes
            dt  <- as_dt_safe(sel)
            if (!is.null(dt) && nrow(dt) > 0) {
              if (!"sample" %in% names(dt)) dt[, sample := patient]
              all_genes_data[[patient]] <- dt
            }
          }
          
          # GOI
          if (!is.null(patient_data$goi)) {
            sel <- patient_data$goi$selected_genes
            dt  <- as_dt_safe(sel)
            if (!is.null(dt) && nrow(dt) > 0) {
              if (!"sample" %in% names(dt)) dt[, sample := patient]
              goi_data[[patient]] <- dt
            }
          }
        }
        
        if (length(all_genes_data) > 0) {
          shared_data$expression.variants.all(rbindlist(all_genes_data, use.names = TRUE, fill = TRUE))
        }
        if (length(goi_data) > 0) {
          shared_data$expression.variants.goi(rbindlist(goi_data, use.names = TRUE, fill = TRUE))
        }
      }
    )
  )
}


get_available_module_types <- function(shared_data) {
  all_keys <- names(shared_data)
  module_keys <- all_keys[grepl("_modules$", all_keys)]
  gsub("_modules$", "", module_keys)
}

#' @export
register_module <- function(shared_data, module_type, module_id, methods) {
  registry <- create_module_registry(shared_data, module_type)
  registry$register(module_id, methods)
}



# ====================================================
# CREATE CACHE WITH var_name + all_full_annot_name columns
# ====================================================

#' @export
create_session_cache <- function(all_files, all_patients, session_dir, variant_type = "somatic") {
  
  if (!dir.exists(session_dir)) {
    dir.create(session_dir, recursive = TRUE)
  }
  
  cache_file <- file.path(session_dir, paste0("in_library_", variant_type, ".rds"))
  
  message("Creating in_library cache for ", variant_type, " from ", length(all_patients), " samples...")
  start_time <- Sys.time()

  # LOAD ONLY var_name a all_full_annot_name columns
  dt_list <- lapply(all_patients, function(patient) {
    tryCatch({
    
      var_file <- all_files[[patient]]$variant
      
      # Načti POUZE tyto 2 sloupce
      dt <- read_by_extension(var_file, select = c("var_name", "all_full_annot_name"))
      sample_name <- patient
      dt[, sample := sample_name]

      message("    ✓ Loaded ", nrow(dt), " variants for sample ", sample_name)
      
      return(dt)

    }, error = function(e) {
      warning("Chyba při načítání: ", basename(var_file), " - ", e$message)
      return(NULL)
    })
  })
  
  dt_list <- dt_list[!sapply(dt_list, is.null)]
  
  if (length(dt_list) == 0) {
    warning("Žádná data pro cache v ", variant_type, " - cache nebude vytvořen")
    return(invisible(NULL))
  }
  
  # Sloučím všechny
  dt_all <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
  
  # Počítej výskyty podle all_full_annot_name (UNIKÁTNÍ identifikátor)
  variant_counts <- dt_all[, .(
    in_library = paste0(.N, "/", length(all_patients)),
    sample_count = .N,
    samples = paste(unique(sample), collapse = ";"),
    var_name = first(var_name)  # Zachovej var_name pro display
  ), by = all_full_annot_name]
  
  # Seřaď podle nejčastějších variant
  variant_counts <- variant_counts[order(-sample_count)]
  
  cache_metadata <- list(
    data = variant_counts,
    created = Sys.time(),
    variant_type = variant_type,
    n_samples = length(all_patients),
    n_variants = nrow(variant_counts),
    n_unique_var_names = uniqueN(variant_counts$var_name),
    sample_files = all_files
  )
  
  saveRDS(cache_metadata, cache_file)
  
  elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 2)
  message("✓ Cache vytvořen: ", 
          nrow(variant_counts), " unique variants (all_full_annot_name), ",
          cache_metadata$n_unique_var_names, " unique genes (var_name) ",
          "z ", length(all_patients), " samples (", elapsed, "s)")
  
  return(invisible(cache_metadata))
}


# ============================================
# ADD in_library FROM CACHE
# ============================================
#' @export
add_in_library_from_session <- function(dt, session_dir, variant_type) {
  cache_file <- file.path(session_dir, paste0("in_library_", variant_type, ".rds"))
  
  if (!file.exists(cache_file)) {
    message("ℹ️  Cache pro ", variant_type, " neexistuje - sloupec in_library nebude přidán")
    if (!"in_library" %in% colnames(dt)) {
      dt[, `:=`(in_library = "1/1", sample_count = 1L)]
    }
    return(dt)
  }
  
  # PŘIDAT: Try-catch pro bezpečné načtení cache
  cache_metadata <- tryCatch({
    readRDS(cache_file)
  }, error = function(e) {
    warning("⚠️  Nelze načíst cache soubor: ", cache_file, " - ", e$message)
    return(NULL)
  })
  
  # Pokud se nepodařilo načíst cache, použij default hodnoty
  if (is.null(cache_metadata)) {
    if (!"in_library" %in% colnames(dt)) {
      dt[, `:=`(in_library = "1/1", sample_count = 1L)]
    }
    return(dt)
  }
  
  if (!"all_full_annot_name" %in% colnames(dt)) {
    warning("Data nemají sloupec all_full_annot_name! Nelze přidat in_library.")
    dt[, `:=`(in_library = "1/1", sample_count = 1L)]
    return(dt)
  }

  cache_data <- cache_metadata$data
  
  # Info pro uživatele
  message("Používám cache: ", cache_metadata$n_samples, " samples, ", 
          cache_metadata$n_variants, " unique variants, ",
          format(cache_metadata$created, "%Y-%m-%d %H:%M"))
  
  # Merge podle all_full_annot_name
  dt <- merge(
    dt, 
    cache_data[, .(all_full_annot_name, in_library, sample_count)], 
    by = "all_full_annot_name", 
    all.x = TRUE
  )
  
  # Varianty které nejsou v cache
  dt[is.na(in_library), `:=`(
    in_library = "1/1",
    sample_count = 1L
  )]
  
  return(dt)
}



# ============================================
# CACHE CLEANING
# ============================================
#' @export
cleanup_old_sessions <- function(base_dir = "sessions", days = 7) {
  if (!dir.exists(base_dir)) return(invisible(NULL))
  
  all_dirs <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  if (length(all_dirs) == 0) return(invisible(NULL))
  
  old_dirs <- all_dirs[file.info(all_dirs)$mtime < Sys.time() - days * 24 * 60 * 60]
  
  if (length(old_dirs) > 0) {
    message("🧹 Cleaning up ", length(old_dirs), " old session directories (>", days, " days old)")
    unlink(old_dirs, recursive = TRUE, force = TRUE)
  }
  
  return(invisible(NULL))
}


# HELPER FUNCTIONS 
# DZ1601,MR1507
read_by_extension <- function(file_path, select = NULL) {
  ext <- tolower(file_ext(file_path))
  if (ext == "tsv") {
    # TSV - podporuje column selection
    if (!is.null(select)) {
      return(fread(file_path, select = select))
    } else {
      return(fread(file_path))
    }
    
  } else if (ext == "vcf") {
    # VCF - musíme načíst celý a pak filtrovat
    dt <- parse_vcf(file_path)
    
    if (!is.null(select)) {
      # Filtruj sloupce po načtení
      return(dt[, ..select])
    }
    return(dt)
    
  } else {
    stop("Nepodporovaný formát: ", file_path, ". Podporovány jsou pouze TSV a VCF.")
  }
}


# Jednoduchý VCF parser
parse_vcf <- function(file_path) {
  # Najdi kde začínají data (za headerem)
  con <- file(file_path, "r")
  skip_lines <- 0
  
  while (TRUE) {
    line <- readLines(con, n = 1)
    if (length(line) == 0) break
    
    if (grepl("^#CHROM", line)) {
      break
    }
    skip_lines <- skip_lines + 1
  }
  close(con)
  
  # Načti VCF jako tabulku
  dt <- fread(file_path, skip = skip_lines)
  
  # Přejmenuj první sloupec (odstraň #)
  if (names(dt)[1] == "#CHROM") {
    setnames(dt, "#CHROM", "CHROM")
  }
  
  return(dt)
}

