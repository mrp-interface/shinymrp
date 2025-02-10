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
  ggplot2::theme_set(theme_light(base_family = "Arial", base_size = 18))

  global <- reactiveValues(
    web_version = FALSE,
    covid = NULL,
    input = input,
    output = output,
    session = session,
    extdata = list(
      covid = list(
        pstrat_all = readr::read_csv(app_sys("extdata/pstrat_all.csv"), show_col_types = FALSE) |> mutate(zip = as.character(zip)),
        covar_all = readr::read_csv(app_sys("extdata/covariates_all.csv"), show_col_types = FALSE) |> mutate(zip = as.character(zip)),
        map_geojson = readRDS(app_sys("extdata/county_geojson.RDS")),
        fips = readr::read_csv(app_sys("extdata/fips.csv"), show_col_types = FALSE)
      ),
      poll = list(
        pstrat_data = readr::read_csv(app_sys("extdata/pstrat.csv"), show_col_types = FALSE),
        map_geojson = readRDS(app_sys("extdata/state_geojson.RDS")),
        fips = readr::read_csv(app_sys("extdata/fips.csv"), show_col_types = FALSE) |> mod_fips()
      )
    ),
    mrp_input = NULL,
    plotdata = NULL,
    models = NULL,
    poststratified_models = NULL,
    model_count = 0
  )
  
  # make interface selection flag available for conditionalPanel
  output$covid <- reactive(global$covid)
  outputOptions(output, "covid", suspendWhenHidden = FALSE)
  mod_home_server(module_ids$home, global)
  mod_persist_server(module_ids$persist, global)
  mod_analyze_upload_server(module_ids$analyze$upload, global)
  mod_analyze_visualize_server(module_ids$analyze$visualize, global)
  mod_analyze_model_server(module_ids$analyze$model, global)
  mod_analyze_result_server(module_ids$analyze$result, global)
  mod_learn_preprocess_server(module_ids$learn$preprocess, global)
  # mod_learn_interface_server(module_ids$learn$interface, global)
}
