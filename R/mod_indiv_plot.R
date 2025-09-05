#' indiv_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @keywords internal 
mod_indiv_plot_ui <- function(id) {
  ns <- NS(id)
  plotOutput(
    outputId = ns("plot"),
    width = "100%",
    height = .plot_height()
  )
}
    
#' indiv_plot Server Functions
#'
#' @noRd
#' @keywords internal 
mod_indiv_plot_server <- function(id, workflow, demo_var){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$plot <- renderPlot({
      req(workflow()$mrp_data())

      workflow()$demo_bars(demo_var)
    })
  })
}
    
## To be copied in the UI
# mod_indiv_plot_ui("indiv_plot_1")
    
## To be copied in the server
# mod_indiv_plot_server("indiv_plot_1")
