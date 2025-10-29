#' Reset inputs on data upload page
#' 
#' @description Resets all form inputs on the data upload page to their default
#' states, clearing file selections, toggles, and dropdowns to allow users to
#' start fresh with new data uploads.
#'
#' @noRd
#' @keywords internal 
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
  shinyjs::hide("example_popover")
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
#' @noRd
#' @keywords internal
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
#' @noRd
#' @keywords internal
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
#' @noRd
#' @keywords internal
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
#' @keywords internal
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
#' @noRd
#' @keywords internal
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
#' @keywords internal
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
#' @noRd
#' @keywords internal
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
#' @noRd
#' @keywords internal
.show_notif <- function(message) {
  showModal(
    modalDialog(
      title = tagList(icon("bell", "fa"), "Notification"),
      message
    )
  )
}

#' Show demo mode message
#'
#' @noRd 
#' @keywords internal
.show_demo_notif <- function() {
  shinyWidgets::sendSweetAlert(
    title = "Information",
    text = tags$p("The web version of the MRP interface currently serves as a demo. We are working to provide computation and memory support for Bayesian model estimation. The native version can be installed from ", tags$a("GitHub.", href = "https://github.com/mrp-interface/shinymrp", target = "_blank")),
    type = "info"
  )
}

#' Show CmdStanR installation alert
#' 
#' @noRd 
#' @keywords internal
.show_backend_alert <- function() {
  .show_alert(
    tags$p(
      "The ",
      tags$code("cmdstanr"),
      " package is not installed or configured properly. Please refer to the installation instructions at ",
      tags$a(
        "https://mc-stan.org/cmdstanr/articles/cmdstanr.html.",
        href = "https://mc-stan.org/cmdstanr/articles/cmdstanr.html",
        target = "_blank"
      )
    )
  )
}

#' Show an alert if CmdStanR and CmdStan are installed
#' @noRd
#' @keywords internal
.stop_if_no_backend <- function() {
  if (!.require_cmdstanr_cmdstan(error = FALSE)) {
    .show_backend_alert()
    stop()
  }
}

#' Show alert when users try to fit model in demo mode
#' 
#' @noRd 
#' @keywords internal
.stop_if_fit_in_demo <- function() {
  if (.get_config("demo")) {
    .show_notif(
      tags$p(
        "This functionality is currently not available for the web version of the MRP interface. Try the example model estimation provided under ",
        tags$b("Upload Estimation Results"),
        "."
      )
    )
    stop()
  }
}

#' Show an alert if MCMC parameters are invalid
#' 
#' @param n_iter Numeric. Number of MCMC iterations to validate
#' @param n_chains Numeric. Number of MCMC chains to validate
#' @param seed Numeric. Random seed value to validate
#'
#' @noRd 
#' @keywords internal
.stop_if_bad_mcmc_params <- function(n_iter, n_chains, seed) {
  out <- .check_mcmc_params(n_iter, n_chains, seed)
  if(!out$valid) {
    .show_alert(
      tagList(
        tags$ul(
          purrr::map(out$msg, ~ tags$li(.x))
        )
      )
    )
    stop()
  }
}

#' Show an alert if maximum number of models is reached
#' 
#' @param n_models Numeric. Current number of models
#' 
#' @noRd 
#' @keywords internal
.stop_if_max_models <- function(n_models) { 
  if(n_models + 1 > .const()$ui$model$max_models) {
    .show_alert("Maximum number of models reached. Please remove existing models to add more.")
    stop()
  }
}

#' Show an alert if no predictors are selected
#' 
#' @param n_fix Numeric. Number of fixed effects selected
#' @param n_vary Numeric. Number of varying effects selected
#'
#' @noRd
#' @keywords internal
.stop_if_no_effects <- function(n_fix, n_vary) {
  if(n_fix + n_vary == 0) {
    .show_alert("No predictor has been selected. Please include at least one.")
    stop()
  }
}

#' Show an alert if priors are invalid
#' @param priors List of prior specifications to validate
#' 
#' @noRd
#' @keywords internal
.stop_if_bad_priors <- function(priors) {
  valid_priors <- purrr::map(priors, function(prior) {
    prior %>%
      .clean_prior_syntax() %>%
      .check_prior_syntax()
  }) %>%
    unlist()
  
  if(!all(valid_priors)) {
    .show_alert("Invalid prior provided. Please check the User Guide for the list of available priors.")
    stop()
  }
}