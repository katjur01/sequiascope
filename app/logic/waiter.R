# Waiter helpers — progress overlay + per-element spinners

box::use(
  shiny[...],
  waiter[waiter_show, waiter_hide, spin_fading_circles, useWaiter],
  shinycssloaders[withSpinner],
  htmltools[tagList, tags, h3]
)

#' Spinner wrapper for reactable/plot outputs
#' @export
use_spinner <- function(ui_element) {
  withSpinner(ui_element, type = 3, color = "#74c0fc", color.background = "#EEEEEE")
}

#' Initialize waiter for a module
#' @export
use_waiter <- function() {
  useWaiter()
}

#' Show waiter overlay for an element
#' @export
show_waiter <- function(id, text = "Loading...") {
  waiter_show(
    id = id,
    html = tagList(
      spin_fading_circles(),
      shiny::h3(text, style = "color: white; margin-top: 20px;")
    ),
    color = "rgba(0, 0, 0, 0.8)"
  )
}

#' Hide waiter overlay for an element
#' @export
hide_waiter <- function(id) {
  waiter_hide(id)
}

#' Show waiter with progress bar
#' 
#' @param session Shiny session object
#' @export
show_waiter_with_progress <- function(session) {
  # Add body class immediately so CSS hides underlying content before waiter renders
  session$sendCustomMessage("waiter-show-active", list())
  waiter_show(
    id = NA,
    html = tagList(
      spin_fading_circles(),
      h3("Loading and processing data...", style = "color: white; margin-top: 20px;"),
      tags$div(
        id = "waiter-progress",
        style = "width: 300px; margin: 20px auto;",
        tags$div(
          style = "background: rgba(255,255,255,0.2); border-radius: 10px; padding: 3px;",
          tags$div(
            id = "waiter-progress-bar",
            style = "width: 0%; height: 20px; background: #74c0fc; border-radius: 7px; transition: width 0.3s ease;"
          )
        ),
        tags$div(
          id = "waiter-progress-text",
          style = "color: white; text-align: center; margin-top: 10px; font-size: 14px;",
          "0%"
        )
      )
    ),
    color = "rgba(0, 0, 0, 0.8)"
  )
}

#' Hide the full-page waiter overlay
#'
#' Uses the session object explicitly to avoid getDefaultReactiveDomain() issues
#' inside box modules.
#' @param session Shiny session object
#' @export
hide_waiter_progress <- function(session) {
  session$sendCustomMessage("waiter-hide", list(id = NA))  # waiter package built-in handler
  session$sendCustomMessage("waiter-body-hide", list())    # our CSS class cleanup
}

#' Update waiter progress
#' 
#' @param session Shiny session object
#' @param percent Progress percentage (0-100)
#' @param text Optional text to display (defaults to "X%")
#' @export
update_waiter_progress <- function(session, percent, text = NULL) {
  session$sendCustomMessage("waiter-update", list(
    percent = percent,
    text = if (is.null(text)) paste0(percent, "%") else text
  ))
}

#' Request notification when Summary tab is rendered
#' 
#' @param session Shiny session object
#' @param ns Namespace function
#' @export
wait_for_summary_rendered <- function(session, ns) {
  session$sendCustomMessage("wait-for-summary", list(
    inputId = ns("summary_rendered")
  ))
}

#' Get JavaScript code for waiter progress handlers
#' 
#' @return HTML tagList with CSS and JavaScript handlers
#' @export
get_waiter_js <- function() {
  tagList(
    # Ensure the waiter overlay is always on top of Shiny re-renders.
    # invalidateLater() observers cause Shiny outputs to briefly flash above the
    # overlay during recalculation. Setting z-index to INT_MAX and hiding
    # content under the overlay via body.waiter-active prevents the flash.
    tags$style(HTML("
      .waiter-overlay {
        z-index: 2147483647 !important;
      }
      body.waiter-active > *:not(.waiter-overlay):not(script):not(style) {
        visibility: hidden !important;
      }
    ")),
    tags$script(HTML("
    Shiny.addCustomMessageHandler('waiter-update', function(data) {
      var bar = document.getElementById('waiter-progress-bar');
      var text = document.getElementById('waiter-progress-text');
      if (bar) bar.style.width = data.percent + '%';
      if (text) text.textContent = data.text;
    });

    Shiny.addCustomMessageHandler('waiter-show-active', function(data) {
      document.body.classList.add('waiter-active');
    });

    Shiny.addCustomMessageHandler('waiter-body-hide', function(data) {
      document.body.classList.remove('waiter-active');
    });
    
    // Monitor when Summary tab is fully rendered
    Shiny.addCustomMessageHandler('wait-for-summary', function(data) {
      var attempts = 0;
      var maxAttempts = 60; // 60 * 250ms = 15s safety timeout
      var checkRendered = setInterval(function() {
        attempts++;
        var summaryBoxes = document.querySelectorAll('[id*=summary_table]');
        if (summaryBoxes.length > 0 || attempts >= maxAttempts) {
          clearInterval(checkRendered);
          setTimeout(function() {
            Shiny.setInputValue(data.inputId, String(Date.now()), {priority: 'event'});
          }, summaryBoxes.length > 0 ? 300 : 0);
        }
      }, 250);
    });
  "))
  )
}
