#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # set file upload size limit
  options(shiny.maxRequestSize = 200*1024^2)

  # set ggplot2 theme
  ggplot2::theme_set(
    ggplot2::theme_light(
      base_family = "Arial",
      base_size   = 20
    ) +
      ggplot2::theme(
        plot.title   = ggplot2::element_text(hjust = 0.5),
        plot.caption = ggplot2::element_text(hjust = 0.5),
        plot.margin  = ggplot2::margin(1, 1, 1, 1, "cm")
      )
  )

  global <- reactiveValues(
    input = input,
    output = output,
    session = session,
    metadata = NULL,
    models = NULL,
    workflow = NULL,

    # reactivity triggers
    prep_ver = 0,
    mrp_ver = 0,
    model_ver = 0
  )

  global$trigger_prep_change <- function() {
    global$prep_ver <- global$prep_ver + 1
  }

  global$trigger_mrp_change <- function() {
    global$mrp_ver <- global$mrp_ver + 1
  }

  global$trigger_model_change <- function() {
    global$model_ver <- global$model_ver + 1
  }

  # --------------------------------------------------------------------------
  # Flags for conditionalPanel
  # --------------------------------------------------------------------------

  # whether data has time information
  output$is_timevar <- reactive(global$metadata$is_timevar)
  outputOptions(output, "is_timevar", suspendWhenHidden = FALSE)

  # special use case
  output$special_case <- reactive(global$metadata$special_case)
  outputOptions(output, "special_case", suspendWhenHidden = FALSE)

  # distribution family
  output$family <- reactive(global$metadata$family)
  outputOptions(output, "family", suspendWhenHidden = FALSE)

  # --------------------------------------------------------------------------
  # Initialize Shiny modules
  # --------------------------------------------------------------------------

  mod_home_server(module_ids$home, global)
  mod_analyze_upload_server(module_ids$analyze$upload, global)
  mod_analyze_visualize_server(module_ids$analyze$visualize, global)
  mod_analyze_model_server(module_ids$analyze$model, global)
  mod_analyze_result_server(module_ids$analyze$result, global)
  mod_learn_preprocess_server(module_ids$learn$preprocess, global)


  # --------------------------------------------------------------------------
  # Navigation event handlers
  # --------------------------------------------------------------------------

  # Check if a version of the interface is selected
  observeEvent(input$navbar, {
    if(input$navbar == "nav_analyze") {
      if(is.null(global$workflow)) {
        showModal(
          modalDialog(
            title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
            "Please select a version of the interface.",
            footer = actionButton(
              inputId = "to_home",
              label = "Go to home"
            )
          )
        )
      }
    }
  })

  observeEvent(input$to_home, {
    bslib::nav_select(,
      id = "navbar",
      selected = "nav_home"
    )

    removeModal()
  })

  # Check if the data upload is valid
  observeEvent(input$navbar_analyze, {
    if (input$navbar_analyze %in% c("nav_analyze_visualize", "nav_analyze_model", "nav_analyze_result")) {
      if (!global$workflow$check_data_exists() ||
          !global$workflow$check_mrp_exists()) {

        message <- if (!global$workflow$check_data_exists()) {
          "Invalid input data. Please make sure your data passes all requirements."
        } else if (!global$workflow$check_mrp_exists()) {
          "Invalid poststratification table. Please provide information for linking your data to the ACS data or upload your own poststatification table."
        }

        showModal(modalDialog(
          title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
          message,
          footer = actionButton(
            inputId = "to_upload",
            label = "Go to data upload page"
          )
        ))
      }
    }
  })
  
  observeEvent(input$to_upload, {
    bslib::nav_select(
      id = "navbar_analyze",
      selected = "nav_analyze_upload"
    )

    removeModal()
  })

  # --------------------------------------------------------------------------
  # Show user guide modal
  # --------------------------------------------------------------------------
  observeEvent(input$show_guide, {
    .show_guide()
  })

  # close loading spinner
  Sys.sleep(1) # prevent flashing
  waiter::waiter_hide()
}

