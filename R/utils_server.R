#' Reset inputs on data upload page
#' 
#' @description Resets all form inputs on the data upload page to their default
#' states, clearing file selections, toggles, and dropdowns to allow users to
#' start fresh with new data uploads.
#'
#' @noRd 
.reset_upload_pg <- function() {
  shinyjs::reset("sample_upload")
  shinyjs::reset("pstrat_upload")
  shinyjs::reset("toggle_sample")
  shinyjs::reset("toggle_pstrat")
  shinyjs::reset("toggle_table")
  shinyjs::reset("link_geo")
  shinyjs::reset("acs_year")

  # reset the accordion to show the sample data panel
  bslib::accordion_panel_open(
    id = "accordion",
    values = "sample"
  )

  # reset popover states
  shinyjs::show("sample_spec_popover")
  shinyjs::show("link_acs_popover")
  shinyjs::hide("pstrat_upload_popover")
}


#' Reset form inputs on model fitting page
#'
#' @description Resets all model specification form inputs to their default
#' states, clearing selections and resetting input fields for predictor
#' selection, MCMC parameters, and file uploads.
#'
#' @param vars List containing variable choices for fixed effects, varying
#'   effects, and interactions to reset selection inputs
#'
#' @return No return value, called for side effect of resetting form inputs
#'
#' @importFrom shinyjs reset
#'
#' @noRd
.reset_model_pg <- function(vars) {
  shinyWidgets::updateVirtualSelect(
    inputId = "fixed",
    choices = vars$fixed,
    selected = NULL
  )
  
  shinyWidgets::updateVirtualSelect(
    inputId = "varying",
    choices = vars$varying,
    selected = NULL
  )
  
  shinyWidgets::updateVirtualSelect(
    inputId = "interaction",
    choices = list(),
    selected = NULL
  )
  
  shinyjs::reset("predictor_select")
  shinyjs::reset("iter_select")
  shinyjs::reset("iter_kb")
  shinyjs::reset("chain_select")
  shinyjs::reset("seed_select")
  shinyjs::reset("spec_kb")
  shinyjs::reset("sens_kb")
  shinyjs::reset("fit_upload")
}

#' Start busy state
#'
#' @description Initiates a busy state for a button by adding a spinning icon,
#' disabling the button, and showing a loading overlay to indicate ongoing
#' processing to users.
#'
#' @param session Shiny session object for updating UI elements
#' @param id Character. Input ID of the button to set to busy state
#' @param label Character. New label text to display on the busy button
#'
#' @return No return value, called for side effect of updating UI state
#'
#' @importFrom shiny updateActionButton icon
#' @importFrom shinyjs disable
#'
#' @noRd
.start_busy <- function(session, id, label) {
  updateActionButton(
    session = session,
    inputId = id,
    label = label,
    icon = icon("spinner", class = "fa-spin")
  )

  shinyjs::disable(id)
  .show_waiter(transparency = 0)
}

#' Stop busy state
#'
#' @description Ends a busy state for a button by updating the icon to indicate
#' success or failure, re-enabling the button, and hiding the loading overlay.
#'
#' @param session Shiny session object for updating UI elements
#' @param id Character. Input ID of the button to remove from busy state
#' @param label Character. New label text to display on the button
#' @param success Logical. Whether the operation was successful (TRUE shows
#'   check icon, FALSE shows warning icon)
#'
#' @return No return value, called for side effect of updating UI state
#'
#' @importFrom shiny updateActionButton icon
#' @importFrom shinyjs enable
#'
#' @noRd
.stop_busy <- function(session, id, label, success) {
  updateActionButton(
    session = session,
    inputId = id,
    label = label,
    icon = if(success) icon("check") else icon("exclamation-triangle")
  )

  shinyjs::enable(id)
  waiter::waiter_hide()
}


#' Navigate to analyze tab
#' 
#' @noRd
.to_analyze <- function(session) {
  bslib::nav_select(
    id = "navbar",
    selected = "nav_analyze",
    session = session
  )

  bslib::nav_select(
    id = "navbar_analyze",
    selected = "nav_analyze_upload",
    session = session
  )
}


#' Show user guide modal
#'
#' @description Displays the user guide in a modal dialog with specified
#' accordion panel open. Provides comprehensive help documentation for
#' application usage.
#'
#' @param open Character. Which accordion panel should be open by default.
#'   If NULL, uses default from .create_guide function
#'
#' @return No return value, called for side effect of showing modal
#'
#' @importFrom shiny showModal modalDialog modalButton
#'
#' @noRd
.show_guide <- function(open = NULL) {
  showModal(
    modalDialog(
      title = "User Guide",
      .create_guide(open),
      size = "xl",
      easyClose = TRUE,
      footer = modalButton("Close")
    )
  )
}


#' Show waiter UI
#'
#' @description Displays a loading spinner with a message to indicate ongoing
#' processing to users.
#'
#' @param loading_type Character. The type of loading UI to display.
#'
#' @noRd
.show_waiter <- function(loading_type = NULL, transparency = 0.9) {
  waiter::waiter_show(
    html = .waiter_ui(loading_type),
    color = waiter::transparent(transparency)
  )
}

#' Show alert modal dialog
#'
#' @description Displays a warning modal dialog with a triangle exclamation icon
#' and custom message to alert users of important information or errors.
#'
#' @param message Character. The message content to display in the alert modal
#' @param session Shiny session object for displaying the modal
#'
#' @return No return value, called for side effect of showing modal
#'
#' @importFrom shiny showModal modalDialog tagList icon
#'
#' @noRd
.show_alert <- function(message) {
  showModal(
    modalDialog(
      title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
      message
    )
  )
}

#' Show notification modal dialog
#'
#' @description Displays a notification modal dialog with a bell icon and
#' custom message to inform users of status updates or general information.
#'
#' @param message Character. The message content to display in the notification modal
#' @param session Shiny session object for displaying the modal
#'
#' @return No return value, called for side effect of showing modal
#'
#' @importFrom shiny showModal modalDialog tagList icon
#'
#' @noRd
.show_notif <- function(message) {
  showModal(
    modalDialog(
      title = tagList(icon("bell", "fa"), "Notification"),
      message
    )
  )
}