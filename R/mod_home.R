#' Home Module UI Function
#'
#' @description Creates the home page interface for the MRP application, providing
#' navigation options for different data types (time-varying vs cross-sectional) and
#' specific data formats (COVID, poll, other). The UI presents a hierarchical selection
#' system with cards for each data type option.
#'
#' @param id Character string. The module's namespace identifier.
#'
#' @return A \code{tagList} containing the home page UI elements including title,
#' data type selection cards, and conditional panels for different workflows.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList conditionalPanel actionButton tags
mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(class = "position-fixed top-50 start-50 translate-middle", style = "width: 55%;",
      # Add custom font family and increase size for the h1 tag
      tags$h1("M.R.P.", 
              class = "text-center display-1 fw-lighter pt-5",
              style = "font-family: 'Trebuchet MS', 'Lucida Sans Unicode', 'Lucida Grande', 'Lucida Sans', Arial, sans-serif; font-size: 12rem;"),
      tags$p("An Interface for applying Multilevel Regression and Poststratification", 
             class = "fs-3 fst-italic text-center pb-4"),
      
      # Main panel group: two panels side-by-side with reduced width
      conditionalPanel(
        condition = sprintf("output['%s'] == 'main'", ns("panel_group")),
        tags$div(class = "row justify-content-center mx-auto mt-5",
          tags$div(class = "col-md-6", 
            card(
              card_header(
                tags$p("Time-varying Data", class = "fs-4 fw-bold text-center m-0")
              ),
              card_body(
                tags$p("Collected over time", class = "fst-italic text-center"),
                actionButton(inputId = ns("set_temporal"), label = "Select", 
                             class = "w-100 mt-3")
              )
            )
          ),
          tags$div(class = "col-md-6", 
            card(
              card_header(
                tags$p("Cross-sectional Data", class = "fs-4 fw-bold text-center m-0")
              ),
              card_body(
                tags$p("Collected at a single time point", class = "fst-italic text-center"),
                actionButton(inputId = ns("set_static"), label = "Select", 
                             class = "w-100 mt-3")
              )
            )
          )
        )
      ),
      
      # Temporal panel group: two panels side-by-side with reduced width
      conditionalPanel(
        condition = sprintf("output['%s'] == 'temporal'", ns("panel_group")),
        tags$div(class = "mb-3 ms-3",
          actionButton(inputId = ns("back_to_main"), label = "\u2190 Back", 
                       class = "btn btn-secondary")
        ),
        tags$div(class = "row justify-content-center mx-auto mt-1",
          tags$div(class = "col-md-6", 
            card(
              card_header(
                tags$p("COVID Data", class = "fs-4 fw-bold text-center m-0")
              ),
              card_body(
                tags$p("Data linking for ZIP-code-level covariates and poststratification", 
                       class = "fst-italic text-center"),
                actionButton(inputId = ns("set_temporal_covid"), label = "Start", 
                             class = "w-100 mt-3")
              )
            )
          ),
          tags$div(class = "col-md-6", 
            card(
              card_header(
                tags$p("Other Time-varying Data", class = "fs-4 fw-bold text-center m-0")
              ),
              card_body(
                tags$p("Data linking for poststratification at state, county, or ZIP-code level", 
                       class = "fst-italic text-center"),
                actionButton(inputId = ns("set_temporal_other"), label = "Start", 
                             class = "w-100 mt-3")
              )
            )
          )
        )
      ),
      
      # Static panel group: two panels side-by-side with reduced width
      conditionalPanel(
        condition = sprintf("output['%s'] == 'static'", ns("panel_group")),
        tags$div(class = "mb-3 ms-3",
          actionButton(inputId = ns("back_to_main"), label = "\u2190 Back", 
                       class = "btn btn-secondary")
        ),
        tags$div(class = "row justify-content-center mx-auto mt-1",
          tags$div(class = "col-md-6", 
            card(
              card_header(
                tags$p("Poll Data", class = "fs-4 fw-bold text-center m-0")
              ),
              card_body(
                tags$p("Data linking for poststratification at state level", 
                       class = "fst-italic text-center"),
                actionButton(inputId = ns("set_static_poll"), label = "Start", 
                             class = "w-100 mt-3")
              )
            )
          ),
          tags$div(class = "col-md-6", 
            card(
              card_header(
                tags$p("Other Cross-sectional Data", class = "fs-4 fw-bold text-center m-0")
              ),
              card_body(
                tags$p("Data linking for poststratification at state, county or ZIP-code level", 
                       class = "fst-italic text-center"),
                actionButton(inputId = ns("set_static_other"), label = "Start", 
                             class = "w-100 mt-3")
              )
            )
          )
        )
      )
    )
  )
}



#' Home Module Server Function
#'
#' @description Server logic for the home page module. Manages panel navigation,
#' handles data format selection, updates global state, and navigates to the
#' analyze section based on user choices. Also handles CmdStan installation
#' and demo mode notifications.
#'
#' @param id Character string. The module's namespace identifier.
#' @param global Reactive values object containing global application state,
#' including session, input, and data_format variables.
#'
#' @return Server function for the home module. Sets up reactive values for
#' panel navigation and observes user interactions to update global state.
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactiveVal reactive outputOptions observeEvent updateNavbarPage
mod_home_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    panel_group <- reactiveVal("main")
    output$panel_group <- reactive(panel_group())
    outputOptions(output, "panel_group", suspendWhenHidden = FALSE)
    
    if (get_config("demo")) {
      shinyWidgets::sendSweetAlert(
        title = "Information",
        text = tags$p("The web version of the MRP interface currently serves as a demo. We are working to provide computation and memory support for Bayesian model estimation. The native version can be installed from ", tags$a("GitHub.", href = "https://github.com/mrp-interface/shinymrp", target = "_blank")),
        type = "info"
      )
    }

    # install CmdStan if not installed already
    if (get_config("install_cmdstan") &&
        is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
          
      waiter::waiter_show(
        html = waiter_ui("setup"),
        color = waiter::transparent(0.9)
      )

      cmdstanr::check_cmdstan_toolchain(fix = TRUE)
      cmdstanr::install_cmdstan(check_toolchain = FALSE)

      waiter::waiter_hide()
    }
    
    observeEvent(global$input$navbar, {
      if(global$input$navbar == "nav_home") {
        panel_group("main")
      }
    })
  


    observeEvent(input$back_to_main, {
      panel_group("main")
    })

    observeEvent(input$set_static, {
      panel_group("static")
    })

    observeEvent(input$set_temporal, {
      panel_group("temporal")
    })

    observeEvent(input$set_static_poll, {
      updateNavbarPage(global$session,
                       inputId = "navbar",
                       selected = "nav_analyze")
      
      updateNavbarPage(global$session,
                       inputId = "navbar_analyze",
                       selected = "nav_analyze_upload")
                       
      global$data_format <- "static_poll"
    })

    observeEvent(input$set_static_other, {
      updateNavbarPage(global$session,
                       inputId = "navbar",
                       selected = "nav_analyze")
      
      updateNavbarPage(global$session,
                       inputId = "navbar_analyze",
                       selected = "nav_analyze_upload")

      global$data_format <- "static_other"
    })
    
    observeEvent(input$set_temporal_covid, {
      updateNavbarPage(global$session,
                       inputId = "navbar",
                       selected = "nav_analyze")
      
      updateNavbarPage(global$session,
                       inputId = "navbar_analyze",
                       selected = "nav_analyze_upload")

      global$data_format <- "temporal_covid"
    })

    observeEvent(input$set_temporal_other, {      
      updateNavbarPage(global$session,
                       inputId = "navbar",
                       selected = "nav_analyze")
      
      updateNavbarPage(global$session,
                       inputId = "navbar_analyze",
                       selected = "nav_analyze_upload")

      global$data_format <- "temporal_other"
    })
  })
}
