#' Run the Shiny Application
#'
#' @param ... arguments passed into golem options via `with_golem_options()`.
#'   See `?golem::get_golem_options` for details.
#' @inheritParams shiny::shinyApp
#' @param launch.browser Logical; if TRUE (default) open in an external browser
#'   even when running inside RStudio. If FALSE, use RStudio Viewer (when available).
#' @export
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  launch.browser = TRUE,
  ...
) {
  # Build the app object
  app <- golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,          
      server = app_server, 
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )

  # Launch behavior
  if (interactive()) {
    # Ensure external browser if requested, without leaving global side effects
    old <- getOption("shiny.launch.browser")
    on.exit(options(shiny.launch.browser = old), add = TRUE)
    options(shiny.launch.browser = isTRUE(launch.browser))

    shiny::runApp(app, launch.browser = TRUE)
    invisible(app)
  } else {
    # In non-interactive contexts (R CMD check, tests), just return the app object
    app
  }
}
