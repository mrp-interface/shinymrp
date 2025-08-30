#' geo_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @keywords internal 
#'
#' @importFrom shiny NS tagList 
mod_geo_plot_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 350,
      position = "right",
      open = TRUE,
      DT::dataTableOutput(outputId = ns("table"))
    ),
    plotOutput(
      outputId = ns("plot"),
      height = .plot_height()
    )
  )
}
    
#' geo_plot Server Functions
#'
#' @noRd
#' @keywords internal 
#' @importFrom dplyr mutate select rename
#' @importFrom rlang sym .data
mod_geo_plot_server <- function(id, workflow, covar){
  moduleServer(id, function(input, output, session){
    # Render the plot
    output$plot <- renderPlot({
      req(workflow()$plot_data()$raw_covariates)

      workflow()$covar_hist(covar)
    })
    
    # Render the table
    output$table <- DT::renderDT({
      req(workflow()$plot_data()$raw_covariates)

      workflow()$covar_table(covar)
    })
  })
}
    
## To be copied in the UI
# mod_geo_plot_ui("geo_plot_1")
    
## To be copied in the server
# mod_geo_plot_server("geo_plot_1")
