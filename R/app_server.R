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
  ggplot2::theme_set(ggplot2::theme_light(base_family = "Arial", base_size = 20))

  global <- reactiveValues(
    web_version = FALSE,
    data_format = NULL,
    input = input,
    output = output,
    session = session,
    extdata = list(
      zip_tract = readr::read_csv(app_sys("extdata/zip_tract.csv"), show_col_types = FALSE, col_types = readr::cols(.default = "c")),
      pstrat_covid = readr::read_csv(app_sys("extdata/pstrat_covid.csv"), show_col_types = FALSE) |> format_geocode(),
      covar_covid = readr::read_csv(app_sys("extdata/covar_covid.csv"), show_col_types = FALSE) |> format_geocode(),
      pstrat_poll = readr::read_csv(app_sys("extdata/pstrat_poll.csv"), show_col_types = FALSE),
      zip_county_state = readr::read_csv(app_sys("extdata/zip_county_state.csv"), show_col_types = FALSE) |> clean_chr(),
      fips = list(
        county = readr::read_csv(app_sys("extdata/fips_county.csv"), show_col_types = FALSE) |> clean_chr(),
        state = readr::read_csv(app_sys("extdata/fips_state.csv"), show_col_types = FALSE) |> clean_chr()
      ),
      geojson = list(
        county = qs::qread(app_sys("extdata/geojson_county.RDS")),
        state = qs::qread(app_sys("extdata/geojson_state.RDS"))
      )
    ),
    link_data = NULL,
    plot_data = NULL,
    uploaded_model = NULL,
    models = NULL,
    poststratified_models = NULL
  )

  
  # make interface selection flag available for conditionalPanel
  output$data_format <- reactive(global$data_format)
  outputOptions(output, "data_format", suspendWhenHidden = FALSE)

  # make link data flag available for conditionalPanel
  output$no_geo <- reactive(is.null(global$link_data$link_geo))
  outputOptions(output, "no_geo", suspendWhenHidden = FALSE)

  # initialize modules
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
      if(is.null(global$data_format)) {
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
      if (is.null(global$data) || is.null(global$mrp)) {
        message <- if (is.null(global$data)) {
          "Invalid input data. Please make sure your data passes all requirements."
        } else if (is.null(global$mrp)) {
          "Invalid post-stratification table. Please provide information for linking your data to the ACS data or upload your own poststatification table."
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

  observeEvent(input$show_guide, {
    show_guide()
  })
}

