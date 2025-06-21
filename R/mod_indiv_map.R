#' indiv_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_indiv_map_ui <- function(id) {
  ns <- NS(id)
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 350,
      position = "right",
      open = TRUE,
      DT::dataTableOutput(
        outputId = ns("sample_size_table")
      )
    ),
    highcharter::highchartOutput(
      outputId = ns("sample_size_map"), 
      height = GLOBAL$ui$plot$map_height,
      width = "100%"
    )
  )
}
    
#' indiv_map Server Functions
#'
#' @noRd 
#' @importFrom dplyr mutate
#' @importFrom rlang .data
mod_indiv_map_server <- function(id, mrp_input, link_geo, geojson, fips_codes){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Render the sample size map
    output$sample_size_map <- highcharter::renderHighchart({
      req(link_geo())

      geo <- if(link_geo() == "zip") "county" else link_geo()

      mrp_input() %>%
        prep_sample_size(
          fips_codes = fips_codes[[geo]],
          geo = geo,
          for_map = TRUE
        ) %>%
        mutate(value = .data$count) %>%
        choro_map(
          geojson()[[geo]],
          geo = geo,
          config = list(
            main_title = sprintf("Sample Size Map"),
            hover_title = "Sample Size"
          )
        )
    })

    # Render the sample size table
    output$sample_size_table <- DT::renderDataTable({
      req(link_geo())

      geo <- if(link_geo() == "zip") "county" else link_geo()

      mrp_input() %>%
        prep_sample_size(
          fips_codes = fips_codes[[geo]],
          geo = geo,
          for_map = FALSE
        ) %>%
        DT::datatable(
          options = list(
            lengthChange = FALSE,
            searching = FALSE,
            info = FALSE,
            ordering = FALSE,
            pagingType = "simple"
          )
        )
    })
  })
}
    
## To be copied in the UI
# mod_indiv_map_ui("indiv_map_1")
    
## To be copied in the server
# mod_indiv_map_server("indiv_map_1")
