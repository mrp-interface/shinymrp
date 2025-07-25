#' indiv_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_indiv_plot_ui <- function(id) {
  ns <- NS(id)
  plotOutput(outputId = ns("plot"), width = "100%", height = GLOBAL$plot$ui$plot_height)
}
    
#' indiv_plot Server Functions
#'
#' @noRd 
#' @importFrom dplyr mutate select filter
#' @importFrom rlang sym .data
mod_indiv_plot_server <- function(id, data, demo_var){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$plot <- renderPlot({
      req(data()$input[[demo_var]], data()$new[[demo_var]])

      input_data <- data()$input %>%
        as_factor(data()$levels[demo_var]) %>%
        mutate(demo = !!sym(demo_var)) %>%
        select(.data$demo, .data$total)

      new_data <- data()$new
      if ("time" %in% names(new_data)) {
        new_data <- new_data %>% filter(.data$time == 1)
      }
      new_data <- new_data %>%
        as_factor(data()$levels[demo_var]) %>%
        mutate(demo = !!sym(demo_var)) %>%
        select(.data$demo, .data$total)
      
      plot_demographic(input_data, new_data)
    })
  })
}
    
## To be copied in the UI
# mod_indiv_plot_ui("indiv_plot_1")
    
## To be copied in the server
# mod_indiv_plot_server("indiv_plot_1")
