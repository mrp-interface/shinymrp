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
  
  tagList(
    # Conditionally display a message if no geographic map is available
    conditionalPanel(
      condition = sprintf("output['%s']", ns("no_geo")),
      p("Map unavailable", class = "alt_text")
    ),
    conditionalPanel(
      condition = sprintf("!output['%s']", ns("no_geo")),
      bslib::layout_columns(
        col_widths = c(9, 3),
        plotly::plotlyOutput(
          outputId = ns("sample_size_map"), 
          height = GLOBAL$ui$small_map_height,
          width = "100%"
        ),
        DT::dataTableOutput(
          outputId = ns("sample_size_table")
        )
      )
    )
  )
}
    
#' indiv_map Server Functions
#'
#' @noRd 
mod_indiv_map_server <- function(id, mrp_input, link_geo, geojson, fips_codes){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Flag to indicate if geographic data is available
    output$no_geo <- reactive({
      is.null(link_geo())
    })
    outputOptions(output, "no_geo", suspendWhenHidden = FALSE)
    
    # Render the sample size map
    output$sample_size_map <- plotly::renderPlotly({
      req(link_geo())

      geo <- if(link_geo() == "zip") "county" else link_geo()

      mrp_input() |>
        prep_sample_size(
          fips_codes = fips_codes[[geo]],
          geo = geo,
          for_map = TRUE
        ) |>
        mutate(value = count) |>
        choro_map(
          geojson()[[geo]],
          map_title = sprintf("Sample Size Map"),
          colorbar_title = "Sample\nSize",
          geo = geo
        )
    })

    # Render the sample size table
    output$sample_size_table <- DT::renderDataTable({
      req(link_geo())

      geo <- if(link_geo() == "zip") "county" else link_geo()

      mrp_input() |>
        prep_sample_size(
          fips_codes = fips_codes[[geo]],
          geo = geo,
          for_map = FALSE
        ) |>
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
