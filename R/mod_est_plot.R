#' est_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_est_plot_ui <- function(id) {
  ns <- NS(id)
  plotOutput(outputId = ns("plot"))
}
    
#' est_plot Server Functions
#'
#' @noRd 
mod_est_plot_server <- function(id, data, plotdata) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$plot <- renderPlot({
      req(data())
      
      if ("time" %in% names(data())) {
        plot_est_temporal(data(), plotdata$dates)
      } else {
        plot_est_static(data())
      }
      
    }, height = function() {
      if (plotdata$n_plots > 1) {
        GLOBAL$ui$subplot_height * plotdata$n_plots
      } else {
        GLOBAL$ui$plot_height
      }
    })
  })
}
    
## To be copied in the UI
# mod_est_plot_ui("est_plot_1")
    
## To be copied in the server
# mod_est_plot_server("est_plot_1")
