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
#' @importFrom dplyr mutate
#' @importFrom rlang .data
mod_est_plot_server <- function(id, model, var) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$plot <- renderPlot({
      req(model())
  
      est_df <- model()$est[[var]] %>%
        mutate(factor = factor(.data$factor, levels = model()$mrp$levels[[var]]))

      if (model()$metadata$is_timevar) {
        plot_est_timevar(
          plot_df = est_df,
          dates = model()$plotdata$dates,
          metadata = model()$metadata
        )
      } else {
        plot_est_static(
          plot_df = est_df,
          metadata = model()$metadata
        )
      }
      
    }, height = function() {
      req(model())
      
      if(model()$metadata$is_timevar) {
        GLOBAL$plot$ui$subplot_height * (length(model()$mrp$levels[[var]]) + 1)
      } else {
        GLOBAL$plot$ui$plot_height
      }
    })
  })
}
    
## To be copied in the UI
# mod_est_plot_ui("est_plot_1")
    
## To be copied in the server
# mod_est_plot_server("est_plot_1")
