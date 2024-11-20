#' persist UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_persist_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(class = "feedback_container",
      tags$a(
        "Feedback",
        href = "https://docs.google.com/forms/d/e/1FAIpQLSdqjTlLsdziJNnPjGGR7vYbNxYeAGdLg5oAxEGMD1EA92g-UQ/viewform?usp=sf_link",
        target = "_blank",
        class = "btn btn-info feedback"
      )
    ),
    tags$div(class = "help_btn_container",
      shinyWidgets::actionBttn(
        inputId = ns("show_guide"),
        style = "material-circle",
        label = NULL,
        icon = icon("question", "fa")
      )         
    )
  )
}
    
#' persist Server Functions
#'
#' @noRd 
mod_persist_server <- function(id, global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    observeEvent(input$show_guide, {
      show_guide("workflow", session)
    })
  })
}
    
## To be copied in the UI
# mod_persist_ui("persist_1")
    
## To be copied in the server
# mod_persist_server("persist_1")
