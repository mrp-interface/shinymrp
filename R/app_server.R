#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  global <- reactiveValues(
    web_version = FALSE,
    data_format = NULL,
    input = input,
    output = output,
    session = session,
    extdata = list(
      zip_tract = readr::read_csv(app_sys("extdata/zip_tract.csv"), show_col_types = FALSE, col_types = readr::cols(.default = "c")),
      pstrat_covid = readr::read_csv(app_sys("extdata/pstrat_covid.csv"), show_col_types = FALSE) |> fix_geocode(),
      covar_covid = readr::read_csv(app_sys("extdata/covar_covid.csv"), show_col_types = FALSE) |> fix_geocode(),
      pstrat_poll = readr::read_csv(app_sys("extdata/pstrat_poll.csv"), show_col_types = FALSE),
      zip_county_state = readr::read_csv(app_sys("extdata/zip_county_state.csv"), show_col_types = FALSE) |> to_lower_case(),
      fips = list(
        county = readr::read_csv(app_sys("extdata/fips_county.csv"), show_col_types = FALSE) |> to_lower_case(),
        state = readr::read_csv(app_sys("extdata/fips_state.csv"), show_col_types = FALSE) |> to_lower_case()
      ),
      geojson = list(
        county = readRDS(app_sys("extdata/geojson_county.RDS")),
        state = readRDS(app_sys("extdata/geojson_state.RDS")) |> filter_geojson(c("11", "72"), omit = TRUE)
      )
    ),
    mrp_input = NULL,
    link_data = NULL,
    plotdata = NULL,
    models = NULL,
    poststratified_models = NULL,
    model_count = 0
  )

  
  # make interface selection flag available for conditionalPanel
  output$data_format <- reactive(global$data_format)
  outputOptions(output, "data_format", suspendWhenHidden = FALSE)

  # make link data flag available for conditionalPanel
  output$no_geo <- reactive(is.null(global$link_data$link_geo))
  outputOptions(output, "no_geo", suspendWhenHidden = FALSE)

  # initialize modules
  mod_home_server(module_ids$home, global)
  mod_persist_server(module_ids$persist, global)
  mod_analyze_upload_server(module_ids$analyze$upload, global)
  mod_analyze_visualize_server(module_ids$analyze$visualize, global)
  mod_analyze_model_server(module_ids$analyze$model, global)
  mod_analyze_result_server(module_ids$analyze$result, global)
  mod_learn_preprocess_server(module_ids$learn$preprocess, global)
}
