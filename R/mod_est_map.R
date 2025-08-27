#' est_map UI Function
#'
#' @description A shiny Module for displaying geographical MRP estimates.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList conditionalPanel selectInput selectizeInput plotOutput
mod_est_map_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui"))
}
    
#' est_map Server Functions
#'
#' @noRd 
#' @importFrom dplyr rename left_join filter
#' @importFrom rlang .data
mod_est_map_server <- function(id, workflow, model, geo_scale, geo_view, geo_subset){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$ui <- renderUI({
      req(model(), geo_scale(), geo_view())
      
      .est_map_ui(
        ns = ns,
        model = model(),
        geo_scale = geo_scale(),
        geo_view = geo_view()
      )
    })
    
    output$map <- highcharter::renderHighchart({
      req(model(), geo_scale())

      workflow()$estimate_map(
        model(),
        geo_scale(),
        input$map_slider
      )
    })
    
    # --------------------------------------------------------------------------
    # Plot for geographic subgroup estimates (subset plots)
    # --------------------------------------------------------------------------
    output$plot <- renderPlot({
      req(model(), geo_scale())
      
      workflow()$estimate_plot_geo(
        model = model(),
        geo = geo_scale(),
        subset = geo_subset()
      )

    }, height = function() {
      req(model())

      .plot_height(
        n = length(geo_subset()) + 1,
        is_timevar = model()$metadata()$is_timevar
      )
    })
    
  })
}
    
## To be copied in the UI
# mod_est_map_ui("est_map_1")
    
## To be copied in the server
# mod_est_map_server("est_map_1", model, global)
