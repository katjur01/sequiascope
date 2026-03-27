# lazy_browser.R
#
# Reusable Shiny module: one-level-at-a-time directory browser.
# Never scans recursively — always fast, regardless of storage size.
#
# Two modes:
#   "navigate"   – single-folder selection (original behaviour).
#                  Returns: selected_path (reactiveVal<character(1)>)
#   "checkboxes" – multi-folder selection from one level.
#                  Subdirs are shown as checkboxes.  "Add checked" appends
#                  them to selected_dirs.  User navigates to a parent, checks
#                  desired subfolders, then clicks "Add checked folders".
#                  Returns: selected_dirs (reactiveVal<character vector>)
#
# The toggle button lives in the PARENT module.
# Pass it in via `open_trigger` reactive.
#
# Usage – navigate mode:
#   lazy_browser$ui(ns("proj_browser"))
#   b <- lazy_browser$server("proj_browser", root = "/input_files",
#                             open_trigger = reactive(input$toggle))
#   b$selected_path()   # NULL until user clicks "Select this folder"
#
# Usage – checkboxes mode:
#   lazy_browser$ui(ns("proj_browser"))
#   b <- lazy_browser$server("proj_browser", root = "/input_files",
#                             mode = "checkboxes",
#                             open_trigger = reactive(input$toggle))
#   b$selected_dirs()   # character(0) until user adds folders

box::use(
  shiny[moduleServer, NS, uiOutput, renderUI, reactiveVal, reactive,
        observeEvent, observe, icon, tagList, req],
  htmltools[tags, HTML, div],
  shinyWidgets[actionBttn],
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- function(id) {
  uiOutput(NS(id, "panel"))
}

# ── Server ────────────────────────────────────────────────────────────────────
# Parameters:
#   root          – top-most directory shown
#   mode          – "navigate" (default) or "checkboxes"
#   open_trigger  – reactive(); each change toggles the panel
#
# Returns list with all three slots (unused one will be NULL / empty):
#   selected_path  – reactiveVal<character(1)>   (navigate mode)
#   selected_dirs  – reactiveVal<character vec>  (checkboxes mode)
#   open           – reactiveVal<logical>
server <- function(id,
                   root = if (dir.exists("/input_files")) "/input_files"
                          else if (dir.exists(file.path(getwd(), "input_files"))) file.path(getwd(), "input_files")
                          else getwd(),
                   mode         = "navigate",
                   open_trigger = NULL) {
  moduleServer(id, function(input, output, session) {

    browser_root        <- root
    browse_open         <- reactiveVal(FALSE)
    current_browse_path <- reactiveVal(NULL)
    selected_path       <- reactiveVal(NULL)        # navigate mode result
    selected_dirs       <- reactiveVal(character(0)) # checkboxes mode result

    # Cache subdirs listing — computed once per path change, reused by renderUI
    # AND the checkbox observer so list.dirs() is only called once per click.
    cached_subdirs <- reactive({
      current <- current_browse_path()
      if (is.null(current)) return(character(0))
      tryCatch(
        sort(list.dirs(current, recursive = FALSE, full.names = TRUE)),
        error = function(e) character(0)
      )
    })

    # React to parent toggle button
    if (!is.null(open_trigger)) {
      observeEvent(open_trigger(), {
        if (!browse_open()) {
          if (is.null(current_browse_path())) current_browse_path(browser_root)
          browse_open(TRUE)
        } else {
          browse_open(FALSE)
        }
      }, ignoreInit = TRUE)
    }

    # Navigate into a subdirectory (both modes)
    observeEvent(input$navigate, {
      target <- input$navigate
      if (!is.null(target) && dir.exists(target)) {
        current_browse_path(target)
      }
    }, ignoreNULL = TRUE)

    # Go up one level (stay within root)
    observeEvent(input$up, {
      current <- current_browse_path()
      parent  <- dirname(current)
      if (nchar(parent) > 0 && parent != current && startsWith(current, browser_root)) {
        current_browse_path(parent)
      }
    })

    # ── checkboxes mode: immediate sync scoped to current folder ─────────────
    # ignoreNULL=TRUE so navigating away (stale input, no new event) never fires this.
    # Only modifies items belonging to the currently displayed folder; items from
    # other folders in selected_dirs are left untouched.
    observeEvent(input$checked_subdirs, {
      req(!is.null(current_browse_path()))
      current_subdirs <- cached_subdirs()  # uses cache — no extra list.dirs call
      checked <- if (is.null(input$checked_subdirs)) character(0) else input$checked_subdirs
      new_sel <- unique(c(
        setdiff(selected_dirs(), current_subdirs),   # keep items from other folders
        intersect(checked, current_subdirs)           # use checked state for this folder
      ))
      selected_dirs(new_sel)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # ── navigate mode: confirm single selection ──────────────────────────────
    observeEvent(input$select, {
      chosen <- current_browse_path()
      if (!is.null(chosen) && dir.exists(chosen)) {
        selected_path(chosen)
        browse_open(FALSE)
        message("[lazy_browser] Selected: ", chosen)
      }
    })

    # ── checkboxes mode: clear all selected dirs ─────────────────────────────
    observeEvent(input$clear_dirs, {
      selected_dirs(character(0))
    })

    # ── Render panel ──────────────────────────────────────────────────────────
    output$panel <- renderUI({
      if (!browse_open()) return(NULL)

      current <- current_browse_path()
      req(current)

      subdirs <- cached_subdirs()  # uses cache — no extra list.dirs call

      rel_path <- sub(paste0("^", browser_root), "", current)
      segments <- Filter(nchar, strsplit(rel_path, "/")[[1]])

      make_nav_js <- function(full_path) {
        sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
                session$ns("navigate"), full_path)
      }

      breadcrumbs <- tagList(
        tags$a(href = "#", onclick = make_nav_js(browser_root),
               style = "color: #495057;", "root"),
        lapply(seq_along(segments), function(i) {
          crumb_path <- paste0(browser_root, "/", paste(segments[1:i], collapse = "/"))
          tagList(
            tags$span(" / ", style = "color: #adb5bd;"),
            tags$a(href = "#", onclick = make_nav_js(crumb_path),
                   style = "color: #495057;", segments[i])
          )
        })
      )

      # ── Navigate mode: clickable folder links ───────────────────────────────
      if (mode == "navigate") {

        dir_items <- if (length(subdirs) > 0) {
          lapply(subdirs, function(d) {
            tags$div(
              style = "padding: 2px 4px;",
              tags$a(
                href    = "#",
                onclick = make_nav_js(d),
                style   = "color: #1971c2; text-decoration: none;",
                tags$i(class = "fa fa-folder", style = "color: #f59f00; margin-right: 6px;"),
                basename(d)
              )
            )
          })
        } else {
          tags$div("(no subdirectories)",
                   style = "color: #868e96; font-style: italic; padding: 4px 8px;")
        }

        tags$div(
          style = "border: 1px solid #dee2e6; border-radius: 6px; padding: 10px 14px;
                   margin: 4px 0 8px 0; background: #f8f9fa;",
          tags$div(
            style = "display: flex; align-items: center; gap: 8px; margin-bottom: 8px;",
            actionBttn(session$ns("select"), "Select this folder",
                       style = "unite", size = "sm", color = "success"),
            actionBttn(session$ns("up"), label = NULL,
                       icon  = icon("arrow-up"), style = "unite", size = "sm")
          ),
          tags$div(style = "font-size: 0.82em; margin-bottom: 8px; color: #495057;",
                   breadcrumbs),
          tags$div(style = "max-height: 260px; overflow-y: auto; font-size: 0.9em;",
                   dir_items)
        )

      # ── Checkboxes mode: each row has checkbox (select) + link (navigate) ─────
      } else {

        # Depend on selected_dirs so pre-check state stays in sync after uncheck
        cur_selected <- selected_dirs()

        # Unique JS identifiers per namespace instance
        cb_class  <- paste0("lazy-cb-",   gsub("[^a-zA-Z0-9]", "_", session$ns("x")))
        sync_fn   <- paste0("lazySync_",  gsub("[^a-zA-Z0-9]", "_", session$ns("x")))
        filter_fn <- paste0("lazyFilter_",gsub("[^a-zA-Z0-9]", "_", session$ns("x")))
        scroll_key <- paste0("lazyScroll_",gsub("[^a-zA-Z0-9]", "_", session$ns("x")))
        list_id   <- paste0("lazyList_",  gsub("[^a-zA-Z0-9]", "_", session$ns("x")))
        input_id  <- session$ns("checked_subdirs")

        sync_script <- tags$script(HTML(sprintf(
          "window['%s'] = function() {
             var vals = [];
             document.querySelectorAll('.%s:checked').forEach(function(cb) {
               vals.push(cb.value);
             });
             Shiny.setInputValue('%s', vals.length ? vals : null, {priority: 'event'});
           };
           (function() {
             var el = document.getElementById('%s');
             if (el) window['%s'] = el.scrollTop;
           })();
           window['%s'] = function(val) {
             var q = val.toLowerCase();
             document.querySelectorAll('#%s .lazy-row').forEach(function(row) {
               row.style.display = (row.dataset.name.toLowerCase().indexOf(q) >= 0) ? '' : 'none';
             });
           };
           setTimeout(function() {
             var el = document.getElementById('%s');
             if (el && window['%s'] !== undefined) el.scrollTop = window['%s'];
           }, 0);",
          sync_fn, cb_class, input_id,
          list_id, scroll_key,
          filter_fn, list_id,
          list_id, scroll_key, scroll_key
        )))

        dir_rows <- if (length(subdirs) > 0) {
          lapply(subdirs, function(d) {
            is_sel       <- d %in% cur_selected
            input_attrs  <- list(
              type     = "checkbox",
              value    = d,
              class    = cb_class,
              onchange = sprintf("window['%s']()", sync_fn),
              style    = "cursor: pointer; width: 14px; height: 14px; flex-shrink: 0; margin: 0;"
            )
            if (is_sel) input_attrs[["checked"]] <- "checked"
            tags$div(
              class = "lazy-row",
              `data-name` = tolower(basename(d)),
              style = "display: flex; align-items: center; padding: 3px 4px; gap: 8px;",
              do.call(tags$input, input_attrs),
              tags$i(class = "fa fa-folder", style = "color: #f59f00; flex-shrink: 0;"),
              tags$a(
                href    = "#",
                onclick = make_nav_js(d),
                style   = "color: #1971c2; text-decoration: none; margin-left: 2px;",
                basename(d)
              )
            )
          })
        } else {
          list(tags$div("(no subdirectories)",
                        style = "color: #868e96; font-style: italic; padding: 4px 8px;"))
        }

        tags$div(
          style = "border: 1px solid #dee2e6; border-radius: 6px; padding: 10px 14px;
                   margin: 4px 0 8px 0; background: #f8f9fa;",
          sync_script,
          tags$div(style = "font-size: 0.82em; margin-bottom: 8px; color: #495057;",
                   breadcrumbs),
          tags$input(
            type        = "text",
            placeholder = "Search folders…",
            oninput     = sprintf("window['%s'](this.value)", filter_fn),
            style       = "width: 100%%; margin-bottom: 6px; padding: 3px 6px;
                           font-size: 0.85em; border: 1px solid #ced4da; border-radius: 4px;"
          ),
          tags$div(
            id    = list_id,
            style = "max-height: 260px; overflow-y: auto; font-size: 0.9em;",
            dir_rows
          )
        )
      }
    })

    list(
      selected_path = selected_path,
      selected_dirs = selected_dirs,
      open          = browse_open
    )
  })
}
