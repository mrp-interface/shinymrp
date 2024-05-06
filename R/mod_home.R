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
            tags$p("Collected over time and by geography"),
            tags$p("Example: COVID-19 hospital test records", style = "display: inline"),
            downloadButton(
              outputId = ns("save_st"),
              label = NULL,
              style = "padding: 0 2px"
            ),
            actionButton(
              inputId = ns("set_covid"),
              label = "Start"
            )
          )
        ),
        tags$div(class = "panel panel-primary landing_panel",
          tags$div(class = "panel-heading landing_panel_heading", "Cross-sectional Data"),
          tags$div(class = "panel-body landing_panel_body",
            tags$p("Collected at a single time point"),
            tags$p("Example: the Cooperative Election Study data", style = "display: inline"),
            downloadButton(
              outputId = ns("save_cs"),
              label = NULL,
              style = "padding: 0 2px"
            ),
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

    output$save_cs <- downloadHandler(
      filename = function() { "example_cs_w_state.csv" },
      content = function(file) {
        read.csv(app_sys("extdata/example_cs_w_state.csv")) |> write.csv(file, row.names = FALSE)
      }
    )

    output$save_st <- downloadHandler(
      filename = function() { "example_st.csv" },
      content = function(file) {
        read.csv(app_sys("extdata/example_st.csv")) |> write.csv(file, row.names = FALSE)
      }
    )

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
