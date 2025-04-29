#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(class = "position-fixed top-50 start-50 translate-middle", style = "width: 55%;",
      # Add custom font family and increase size for the h1 tag
      tags$h1("M.R.P.", 
              class = "text-center display-1 fw-lighter pt-5",
              style = "font-family: 'Trebuchet MS', 'Lucida Sans Unicode', 'Lucida Grande', 'Lucida Sans', Arial, sans-serif; font-size: 12rem;"),
      tags$p("An interface for applying Multilevel Regression and Poststratification", 
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
          actionButton(inputId = ns("back_to_main"), label = "←", 
                       class = "btn")
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
          actionButton(inputId = ns("back_to_main"), label = "←", 
                       class = "btn")
        ),
        tags$div(class = "row justify-content-center mx-auto mt-1",
          tags$div(class = "col-md-6", 
            card(
              card_header(
                tags$p("Poll Data", class = "fs-4 fw-bold text-center m-0")
              ),
              card_body(
                tags$p("Data linking for poststratification at state level (must include education)", 
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
                tags$p("Data linking for poststratification at state, county or ZIP-code level (without education)", 
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



#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    panel_group <- reactiveVal("main")
    output$panel_group <- reactive(panel_group())
    outputOptions(output, "panel_group", suspendWhenHidden = FALSE)
    
    observe({
      if(global$web_version) {
        shinyWidgets::sendSweetAlert(
          title = "Information",
          text = tags$p("The web version of the MRP interface currently serves as a demo. We are working to provide computation and memory support for Bayesian model estimation. The native version can be installed from ", tags$a("GitHub.", href = "https://github.com/mrp-interface/shinymrp", target = "_blank")),
          type = "info"
        )
      } else {
        # install CmdStan if not installed already
        if(is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
          waiter::waiter_show(
            html = waiter_ui("setup"),
            color = waiter::transparent(0.9)
          )

          cmdstanr::check_cmdstan_toolchain(fix = TRUE)
          cmdstanr::install_cmdstan(check_toolchain = FALSE)

          waiter::waiter_hide()
        }
      }
    })
    
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
