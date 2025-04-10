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
    tags$div(class = "landing_container",
      tags$h1("M.R.P.", class = "landing_header"),
      tags$p("An interface for applying Multilevel Regression and Poststratification", 
             class = "landing_subheader"),
      
      # Main panel group: two panels side-by-side
      conditionalPanel(
        ns = ns,
        condition = "output.panel_group == 'main'",
        tags$div(class = "row justify-content-center",
          tags$div(class = "col-md-6",
            card(
              card_header(
                tags$p("Time-varying Data", style = "font-size: 1.2em; font-weight: bold; text-align: center;")
              ),
              card_body(
                tags$p("Collected over time", style = "font-style: italic; text-align: center;"),
                actionButton(inputId = ns("set_temporal"), label = "Start")
              )
            )
          ),
          tags$div(class = "col-md-6",
            card(
              card_header(
                tags$p("Cross-sectional Data", style = "font-size: 1.2em; font-weight: bold; text-align: center;")
              ),
              card_body(
                tags$p("Collected at a single time point", style = "font-style: italic; text-align: center;"),
                actionButton(inputId = ns("set_static"), label = "Start")
              )
            )
          )
        )
      ),
      
      # Temporal panel group: two panels side-by-side
      conditionalPanel(
        ns = ns,
        condition = "output.panel_group == 'temporal'",
        tags$div(class = "row justify-content-center",
          tags$div(class = "col-md-6",
            card(
              card_header(
                tags$p("COVID Data", style = "font-size: 1.2em; font-weight: bold; text-align: center;")
              ),
              card_body(
                tags$p("Data linking for ZIP-code-level covariates and poststratification data", 
                       style = "font-style: italic; text-align: center;"),
                actionButton(inputId = ns("set_temporal_covid"), label = "Start")
              )
            )
          ),
          tags$div(class = "col-md-6",
            card(
              card_header(
                tags$p("Other Time-varying Data", style = "font-size: 1.2em; font-weight: bold; text-align: center;")
              ),
              card_body(
                tags$p("Data linking for state-level, county-level, or ZIP-code-level poststratification data", 
                       style = "font-style: italic; text-align: center;"),
                actionButton(inputId = ns("set_temporal_other"), label = "Start")
              )
            )
          )
        )
      ),
      
      # Static panel group: two panels side-by-side
      conditionalPanel(
        ns = ns,
        condition = "output.panel_group == 'static'",
        tags$div(class = "row justify-content-center",
          tags$div(class = "col-md-6",
            card(
              card_header(
                tags$p("Cross-sectional Data with Education", style = "font-size: 1.2em; font-weight: bold; text-align: center;")
              ),
              card_body(
                tags$p("Data linking for state-level poststratification data with education data", 
                       style = "font-style: italic; text-align: center;"),
                actionButton(inputId = ns("set_static_poll"), label = "Start")
              )
            )
          ),
          tags$div(class = "col-md-6",
            card(
              card_header(
                tags$p("Other Cross-sectional Data", style = "font-size: 1.2em; font-weight: bold; text-align: center;")
              ),
              card_body(
                tags$p("Data linking for state-level, county-level, or ZIP-code-level poststratification data without education data", 
                       style = "font-style: italic; text-align: center;"),
                actionButton(inputId = ns("set_static_other"), label = "Start")
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
