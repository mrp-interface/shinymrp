#' Home Module UI Function
#'
#' @description Creates the home page interface for the MRP application, providing
#' navigation options for different data types (time-varying vs cross-sectional) and
#' specific data formats (COVID, poll, other). The UI presents a hierarchical selection
#' system with cards for each data type option.
#'
#' @param id Character string. The module's namespace identifier.
#'
#' @return A `tagList` containing the home page UI elements including title,
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
      
      
      # Time-varying panel group
      conditionalPanel(
        condition = sprintf("output['%s'] != 'main'", ns("panel_group")),
        tags$div(class = "mb-3 ms-3",
          actionButton(inputId = ns("back_btn"), label = "\u2190 Back", 
                       class = "btn btn-secondary")
        )
      ),
      tags$div(class = "row justify-content-center mx-auto mt-1",
        tags$div(class = "col-md-6", 
          bslib::card(class = "h-100",
            bslib::card_header(
              tags$div(class = "fs-4 fw-bold text-center m-2",
                textOutput(ns("left_panel_title"))
              )
            ),
            bslib::card_body(class = "d-flex flex-column",
              tags$div(class = "fst-italic text-center",
                textOutput(ns("left_panel_text"))
              ),
              actionButton(
                inputId = ns("left_panel_btn"),
                label = "Start", 
                class = "w-100 mt-auto"
              )
            )
          )
        ),
        tags$div(class = "col-md-6", 
          bslib::card(class = "h-100",
            bslib::card_header(
              tags$div(class = "fs-4 fw-bold text-center m-2",
                textOutput(ns("right_panel_title"))
              )
            ),
            bslib::card_body(class = "d-flex flex-column",
              tags$div(class = "fst-italic text-center",
                textOutput(ns("right_panel_text"))
              ),
              actionButton(
                inputId = ns("right_panel_btn"),
                label = "Start", 
                class = "w-100 mt-auto"
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
  moduleServer(id, function(input, output, session){
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

    output$left_panel_title <- renderText({
      req(panel_group())
      
      switch(panel_group(),
        "main" = "Time-varying Data",
        "timevar" = "COVID Data",
        "static" = "Polling Data",
        "timevar_general" = "Binary Outcome",
        "static_general" = "Binary Outcome"
      )
    })
    
    output$left_panel_text <- renderText({
      req(panel_group())
      
      switch(panel_group(),
        "main" = "Collected over time",
        "timevar" = "Data linking for ZIP-code-level covariates and poststratification",
        "static" = "Data linking for post-stratification at state level",
        "timevar_general" = "Data with a binary outcome variable",
        "static_general" = "Data with a binary outcome variable"
      )
    })
    
    output$right_panel_title <- renderText({
      req(panel_group())
      
      switch(panel_group(),
        "main" = "Cross-sectional Data",
        "timevar" = "General Time-varying Data",
        "static" = "General Cross-sectional Data",
        "timevar_general" = "Continuous Outcome",
        "static_general" = "Continuous Outcome"
      )
    })

    output$right_panel_text <- renderText({
      req(panel_group())
      
      switch(panel_group(),
        "main" = "Collected at a single time point",
        "timevar" = "Data linking for post-stratification at state, county, or ZIP-code level",
        "static" = "Data linking for post-stratification at state, county, or ZIP-code level",
        "timevar_general" = "Data with a continuous outcome variable",
        "static_general" = "Data with a continuous outcome variable"
      )
    })

    observeEvent(input$left_panel_btn, {
      req(panel_group())

      if (panel_group() == "main") {
        panel_group("timevar")
      } else if (panel_group() == "timevar") {
        global$metadata <- list(
          is_timevar = TRUE,
          special_case = "covid",
          family = "binomial"
        )

        to_analyze(global$session)
      } else if (panel_group() == "static") {
        global$metadata <- list(
          is_timevar = FALSE,
          special_case = "poll",
          family = "binomial"
        )

        to_analyze(global$session)
      } else if (panel_group() == "timevar_general") {
        global$metadata <- list(
          is_timevar = TRUE,
          special_case = NULL,
          family = "binomial"
        )

        to_analyze(global$session)
      } else if (panel_group() == "static_general") {
        global$metadata <- list(
          is_timevar = FALSE,
          special_case = NULL,
          family = "binomial"
        )

        to_analyze(global$session)
      }
    })

    observeEvent(input$right_panel_btn, {
      req(panel_group())

      if (panel_group() == "main") {
        panel_group("static")
      } else if (panel_group() == "timevar") {
        panel_group("timevar_general")
      } else if (panel_group() == "static") {
        panel_group("static_general")
      } else if (panel_group() == "timevar_general") {
        global$metadata <- list(
          is_timevar = TRUE,
          special_case = NULL,
          family = "normal"
        )

        to_analyze(global$session)
      } else if (panel_group() == "static_general") {
        global$metadata <- list(
          is_timevar = FALSE,
          special_case = NULL,
          family = "normal"
        )

        to_analyze(global$session)
      }
    })


    #------------------------------------------------------------------
    # When the navbar is set to home, reset the panel group to main
    #------------------------------------------------------------------
    observeEvent(global$input$navbar, {
      if(global$input$navbar == "nav_home") {
        panel_group("main")
      }
    })

    #------------------------------------------------------------------
    # Event handlers for back button
    #------------------------------------------------------------------
    observeEvent(input$back_btn, {
      req(panel_group())

      if (panel_group() == "timevar") {
        panel_group("main")
      } else if (panel_group() == "static") {
        panel_group("main")
      } else if (panel_group() == "timevar_general") {
        panel_group("timevar")
      } else if (panel_group() == "static_general") {
        panel_group("static")
      }
    })
  })
}
