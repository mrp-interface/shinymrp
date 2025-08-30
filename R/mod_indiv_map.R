#' indiv_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @keywords internal 
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
      height = .const()$plot$ui$map_height,
      width = "100%"
    )
  )
}
    
#' indiv_map Server Functions
#'
#' @noRd
#' @keywords internal 
#' @importFrom dplyr mutate
#' @importFrom rlang .data
mod_indiv_map_server <- function(id, workflow, fips_codes){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Render the sample size map
    output$sample_size_map <- highcharter::renderHighchart({
      req(workflow()$link_data()$link_geo)

      workflow()$sample_size_map()
    })

    # Render the sample size table
    output$sample_size_table <- DT::renderDataTable({
      req(workflow()$link_data()$link_geo)

      workflow()$sample_size_table()
    })
  })
}
    
## To be copied in the UI
# mod_indiv_map_ui("indiv_map_1")
    
## To be copied in the server
# mod_indiv_map_server("indiv_map_1")
