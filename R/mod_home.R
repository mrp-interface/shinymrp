#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(class = "landing_container",
      tags$h1("M.R.P.", class = "landing_header"),
      tags$p("An interface for applying Multilevel Regression and Poststratification", class = "landing_subheader"),
      tags$div(class = "landing_panels justify",
        tags$div(class = "panel panel-primary landing_panel",
          tags$div(class = "panel-heading landing_panel_heading", "Spatio-temporal Data"),
          tags$div(class = "panel-body landing_panel_body",
            tags$p("Collected over time and by geography", style = "font-style: italic; text-align: center;"),
            actionButton(
              inputId = ns("set_covid"),
              label = "Start"
            )
          )
        ),
        tags$div(class = "panel panel-primary landing_panel",
          tags$div(class = "panel-heading landing_panel_heading", "Cross-sectional Data"),
          tags$div(class = "panel-body landing_panel_body",
            tags$p("Collected at a single time point", style = "font-style: italic;  text-align: center;"),
            actionButton(
              inputId = ns("set_poll"),
              label = "Start"
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

    observeEvent(input$set_poll, {
      updateNavbarPage(global$session,
                       inputId = "navbar",
                       selected = "nav_analyze")

      updateNavbarPage(global$session,
                       inputId = "navbar_analyze",
                       selected = "nav_analyze_upload")

      # show_notif("Switched to interface for cross-sectional data", global$session)

      global$covid <- FALSE
    })


    observeEvent(input$set_covid, {
      updateNavbarPage(global$session,
                       inputId = "navbar",
                       selected = "nav_analyze")

      updateNavbarPage(global$session,
                       inputId = "navbar_analyze",
                       selected = "nav_analyze_upload")


      # show_notif("Switched to interface for spatio-temporal data with measurement error", global$session)

      global$covid <- TRUE
    })


  })
}
