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
    tags$div(
      id = ns("feedback"),
      class = "feedback_container",
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
        size = "l",
        color = "success",
        label = NULL,
        icon = icon("question", "fa")
      )         
    ),
    tags$script(HTML(sprintf("
    $(window).on('scroll', function() {
      var atTop = ($(this).scrollTop() <= 10); // Boolean: true if at top, false otherwise
      Shiny.setInputValue('%s', atTop);
    });", ns("at_top"))))
  )
}
    
#' persist Server Functions
#'
#' @noRd 
mod_persist_server <- function(id, global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    shinyjs::addClass("show_guide", "spin")
    shinyjs::delay(2000, shinyjs::removeClass("show_guide", "spin"))
 
    observeEvent(input$show_guide, {
      show_guide("workflow")
    })
    
    observeEvent(input$at_top, {
      if(input$at_top) {
        shinyjs::show("feedback", anim = TRUE, animType = "fade", time = 0.1) 
      } else {
        shinyjs::hide("feedback", anim = TRUE, animType = "fade", time = 0.1)
      }
    })
  })
}
    
## To be copied in the UI
# mod_persist_ui("persist_1")
    
## To be copied in the server
# mod_persist_server("persist_1")
