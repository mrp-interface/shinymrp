#' Persistent UI Elements Module UI Function
#'
#' @description Creates persistent UI elements that remain visible across all
#' pages of the application. Includes a feedback button that links to an external
#' form and a help button that shows user guides. The feedback button visibility
#' is controlled by scroll position, appearing only when the user is at the top
#' of the page.
#'
#' @param id Character string. The module's namespace identifier.
#'
#' @return A `tagList` containing persistent UI elements:
#' \itemize{
#'   \item Feedback button linking to external Google Form
#'   \item Help button for displaying user guides
#'   \item JavaScript code for scroll position tracking
#' }
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tags icon
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
    
#' Persistent UI Elements Module Server Function
#'
#' @description Server logic for persistent UI elements. Manages the visibility
#' of the feedback button based on scroll position, handles help button clicks
#' to show user guides, and provides visual feedback with animations. Uses
#' JavaScript integration to track scroll position and dynamically show/hide
#' elements.
#'
#' @param id Character string. The module's namespace identifier.
#' @param global Reactive values object containing global application state
#'
#' @return Server function for the persistent UI module. Handles scroll-based
#' visibility, help guide display, and UI animations for persistent elements.
#'
#' @noRd
#'
#' @importFrom shiny moduleServer observeEvent
#' @importFrom shinyjs addClass removeClass delay show hide
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
