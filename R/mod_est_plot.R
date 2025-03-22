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
mod_est_plot_server <- function(id, model, var) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$plot <- renderPlot({
      req(model())
  
      est_df <- model()$est[[var]]
      
      if ("time" %in% names(est_df)) {
        plot_est_temporal(est_df, model()$plotdata$dates)
      } else {
        plot_est_static(est_df)
      }
      
    }, height = function() {
      if(model()$data_format %in% c("temporal_covid", "temporal_other")) {
        GLOBAL$ui$subplot_height * (length(model()$mrp$levels[[var]]) + 1)
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
