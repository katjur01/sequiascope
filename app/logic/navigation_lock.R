# Navigation Locking Helpers
# Functions for managing tab locking to control user navigation flow

box::use(
  shiny[...],
  htmltools[tags, HTML]
)

#' Lock navigation tabs except specified one
#' 
#' @param session Shiny session object
#' @param allowed_value Tab value that should remain accessible (e.g., ns("upload_data"))
#' @export
lock_navigation <- function(session, allowed_value) {
  session$sendCustomMessage("lockMenuExcept", list(
    lock = TRUE,
    allowValue = allowed_value
  ))
}

#' Unlock all navigation tabs
#' 
#' @param session Shiny session object
#' @export
unlock_navigation <- function(session) {
  session$sendCustomMessage("lockMenuExcept", list(
    lock = FALSE,
    allowValue = NULL
  ))
}

#' Get CSS styles for locked tabs
#' 
#' @return HTML style tag with CSS for locked tabs
#' @export
get_navigation_lock_css <- function() {
  tags$style(HTML("
    .app-tab-locked {
      pointer-events: none;
      opacity: 0.5; 
      cursor: not-allowed !important;
    }
  "))
}

#' Get JavaScript code for navigation locking mechanism
#' 
#' @return HTML script tag with JavaScript handlers
#' @export
get_navigation_lock_js <- function() {
  tags$script(HTML("
    (function(){
      var menuLocked = true;
      var allowedValue = null;

      Shiny.addCustomMessageHandler('lockMenuExcept', function(msg){
        // msg: { lock: bool, allowValue: 'ns(tabName)', menuId: 'ns(navbarMenu)' }
        menuLocked = !!msg.lock;
        allowedValue = msg.allowValue;

        // Lock/unlock all links with data-value (works for nav-link and dropdown-item)
        var anchors = document.querySelectorAll('a.nav-link[data-value], a.dropdown-item[data-value]');
        anchors.forEach(function(a){
          var v = a.getAttribute('data-value');
          if (!v) return;
          if (menuLocked && v !== allowedValue) {
            a.classList.add('app-tab-locked');
            a.setAttribute('aria-disabled', 'true');
            a.setAttribute('tabindex', '-1');
          } else {
            a.classList.remove('app-tab-locked');
            a.removeAttribute('aria-disabled');
            a.removeAttribute('tabindex');
          }
        });
      });

      // Hard click guard in case the class check fails
      document.addEventListener('click', function(e){
        var t = e.target.closest('a.nav-link[data-value], a.dropdown-item[data-value]');
        if (!t) return;
        if (menuLocked) {
          var v = t.getAttribute('data-value');
          if (v && v !== allowedValue) {
            e.preventDefault();
            e.stopImmediatePropagation();
            return false;
          }
        }
      }, true); // capture phase for safety
    })();
  "))
}
