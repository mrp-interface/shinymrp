#' analyze_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analyze_upload_ui <- function(id){
  ns <- NS(id)
  tags$div(class = "pad_top",
    sidebarLayout(
      sidebarPanel(width = 3,
        tags$p("Upload individual-level or aggregated data (examples below)"),
        shinyWidgets::radioGroupButtons(
            inputId = ns("toggle_input"),
            label = NULL,
            choices = c("Individual-level" = "indiv", "Aggregated" = "agg"),
            selected = "agg",
            justified = TRUE,
            size = "sm",
            checkIcon = list(
              yes = tags$i(class = "fa fa-circle-check", style = "color: white"),
              no = tags$i(class = "fa fa-circle-o", style = "color: white")
            )
        ),
        fileInput(
          inputId = ns("input_data"),
          label = NULL,
          accept = c(".csv", ".xlsx", ".sas7bdat")
        ),
        tags$p(class = "ref",
          "For ", tags$u("requirements for input data"), "and preprocessing code, open",
          actionLink(
            inputId = ns("show_upload_guide"),
            label = "Guide.",
            class = "action_link"
          ),
          "For a detailed description of the prepropressing procedure, go to the",
          actionLink(
            inputId = ns("to_preprocess"),
            label = "Preprocessing",
            class = "action_link"
          ),
          "page."
        ),
        tags$div(style = "margin-top: 25px",
          conditionalPanel(
            condition = "output.covid",
            tags$p("Example: COVID-19 hospital test records")
          ),
          conditionalPanel(
            condition = "!output.covid",
            tags$p("Example: The Cooperative Election Study data")
          ),
          tags$div(class = "justify pad_top",
            actionButton(
              inputId = ns("use_indiv_example"),
              label = "Invididual-level",
              icon = icon("table", class = "fa button_icon"),
              width = "49.5%"
            ),
            actionButton(
              inputId = ns("use_agg_example"),
              label = "Aggregated",
              icon = icon("table", class = "fa button_icon"),
              width = "49.5%"
            ),
          )
        )
      ),
      mainPanel(width = 9,
        uiOutput(outputId = ns("main_panel"))
      )
    )
  )
}

#' analyze_upload Server Functions
#'
#' @noRd
mod_analyze_upload_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    rawdata <- reactiveVal()

    observeEvent(global$input$navbar, {
      if(global$input$navbar == "nav_analyze" & is.null(global$covid)) {
          showModal(
            modalDialog(
              title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
              "Please select a version of the interface.",
              footer = actionButton(
                inputId = ns("to_home"),
                label = "Go to home"
              )
            )
          )
      }
    })

    observeEvent(input$to_home, {
      updateTabsetPanel(global$session,
        inputId = "navbar",
        selected = "nav_home"
      )

      removeModal(global$session)
    })

    observeEvent(global$covid, {
      shinyjs::reset("input_data")
      shinyjs::reset("toggle_input")

      rawdata(NULL)
      global$data <- NULL
      global$mrp <- NULL
      global$plotdata <- NULL
    })
    
    output$main_panel <- renderUI({
      req(rawdata())
      
      tagList(
        tags$div(class = "justify",
          shinyWidgets::radioGroupButtons(
            inputId = ns("toggle_table"),
            label = NULL,
            choices = c("Raw" = "raw", "Preprocessed" = "prep"),
            checkIcon = list(
              yes = tags$i(class = "fa fa-circle-check", style = "color: white"),
              no = tags$i(class = "fa fa-circle-o", style = "color: white")
            )
          ),
          shinyBS::bsTooltip(ns("toggle_table"), "\"Preprocessed\" table only shows when data has been preprocessed properly", placement = "right"),
          tags$p(sprintf("*The preview only includes the first %d rows of the data", GLOBAL$ui$preview_size))
        ),
        DT::dataTableOutput(outputId = ns("table"))
      )
    })


    output$table <- DT::renderDataTable({
      df <- if(input$toggle_table == "raw") rawdata() else global$data

      df |>
        head(GLOBAL$ui$preview_size) |>
        DT::datatable(
          options = list(
            scrollX = TRUE,
            lengthChange = FALSE,
            searching = FALSE
          )
        )
    })

    observeEvent(input$input_data, {

      # reset variables
      global$data <- NULL
      global$mrp <- NULL
      global$plotdata <- NULL

      # read in data
      path <- input$input_data$datapath
      if(stringr::str_ends(path, "csv")) {
        readr::read_csv(path, show_col_types = FALSE) |> rawdata()
      } else if (stringr::str_ends(path, "(xlsx|xls)")) {
        readxl::read_excel(path, guess_max = 5000) |> rawdata()
      } else if (stringr::str_ends(path, "sas7bdat")) {
        haven::read_sas(path) |> rawdata()
      } # else no necessary due to fileInput constraint


      waiter::waiter_show(
        html = waiter_ui("wait"),
        color = waiter::transparent(0.9)
      )


      if(input$toggle_input == "indiv") {
        # aggregate raw data
        out <- try({
          if(global$covid) {
            global$data <- rawdata() |>
              aggregate_covid(age_bounds = GLOBAL$bounds$covid$age) |>
              prep(list(), to_char = c("zip"))
          } else {
            global$data <- rawdata() |>
              aggregate_poll(age_bounds = GLOBAL$bounds$poll$age)
          }
        }, silent = TRUE)

        if ("try-error" %in% class(out)) {
          show_alert("Unsuccessful data processing. Please check the Learn > Interface page for input data requirements.", global$session)
        } else {
          show_notif("Input data has been preprocessed. You may proceed to the next page.", global$session)
        }

      } else {
        # check input aggregated data
        out <- try({
          if(global$covid) {
            errors <- check_covid_data(rawdata(), GLOBAL$expected_columns$covid)

            if(length(errors) == 0) {
              global$data <- rawdata() |>
                find_columns(GLOBAL$expected_columns$covid) |>
                prep(GLOBAL$levels$covid,
                  to_lower = c("sex", "race"),
                  to_char = c("zip")
                )
            } else if(length(errors) == 1 & "date" %in% names(errors)) {
              global$data <- rawdata() |>
                find_columns(GLOBAL$expected_columns$covid) |>
                prep(GLOBAL$levels$covid,
                  to_lower = c("sex", "race"),
                  to_char = c("zip")
                ) |>
                select(-date)
            }
          } else {
            errors <- check_poll_data(rawdata(), GLOBAL$expected_columns$poll)

            if(length(errors) == 0) {
              global$data <- rawdata() |>
                find_columns(GLOBAL$expected_columns$poll) |>
                prep(GLOBAL$levels$poll,
                  to_lower = c("sex", "race", "edu")
                )
            } else if(length(errors) == 1 & "state" %in% names(errors)) {
              global$data <- rawdata() |>
                find_columns(GLOBAL$expected_columns$poll) |>
                prep(GLOBAL$levels$poll,
                  to_lower = c("sex", "race", "edu")
                ) |>
                select(-state)
            }
          }
        }, silent = TRUE)

        if ("try-error" %in% class(out)) {
          show_alert("Input data does not meet all requirements. Please check Guide (bottom right corner) for input data requirements.", global$session)
        } else {
          if(length(errors) == 0) {
            show_notif("All requirements are met. You may proceed to the next page.", global$session)
          } else {
            show_alert(
              tagList(
                tags$ul(
                  purrr::map(unlist(errors), ~ tags$li(.x))
                ),
                tags$p("Please check Guide (bottom right corner) for input data requirements.")
              ),
              global$session
            )
          }
        }
      }

      # prepare data for model fitting and plotting
      if(!is.null(global$data)) {
        if(global$covid) {
          c(patient, pstrat_data, covariates, raw_covariates) %<-% link_ACS(
              global$data,
              global$extdata$covid$tract_data,
              global$extdata$covid$zip_tract
            )

          c(input_data, new_data, levels, vars) %<-% prepare_data_covid(
              patient,
              pstrat_data,
              covariates,
              GLOBAL$levels$covid
            )


          global$mrp <- list(
            input = input_data,
            new = new_data,
            new_stan = stan_factor_covid(new_data, GLOBAL$levels$covid),
            levels = levels,
            vars = vars
          )

          global$plotdata <- list(
            dates = if("date" %in% names(global$data)) get_dates(global$data) else NULL,
            geojson = filter_geojson(global$extdata$covid$map_geojson, global$mrp$levels$county),
            raw_covariates = raw_covariates
          )

        } else {
          global$data$state <- to_fips(global$data$state, global$extdata$poll$fips)

          covariates <- get_state_predictors(rawdata())
          covariates$state <- to_fips(covariates$state, global$extdata$poll$fips)

          c(input_data, new_data, levels, vars) %<-% prepare_data_poll(
            global$data,
            global$extdata$poll$pstrat_data,
            covariates,
            GLOBAL$levels$poll
          )

          global$mrp <- list(
            input = input_data,
            new = new_data,
            levels = levels,
            vars = vars
          )

          if("state" %in% names(global$data)) {
            global$plotdata <- list(
              geojson = filter_geojson(global$extdata$poll$map_geojson, global$mrp$levels$state)
            )
          }
        }
      }

      waiter::waiter_hide()

    })

    observeEvent(input$use_indiv_example, {
      waiter::waiter_show(
        html = waiter_ui("wait"),
        color = waiter::transparent(0.9)
      )

      if(global$covid) {
        readr::read_csv(app_sys("extdata/covid_test_records_individual.csv"), show_col_types = FALSE) |> rawdata()

        global$data <- rawdata() |>
          aggregate_covid(age_bounds = GLOBAL$bounds$covid$age) |>
          prep(list(), to_char = c("zip"))
      } else {
        readr::read_csv(app_sys("extdata/CES_data_individual.csv"), show_col_types = FALSE) |> rawdata()

        global$data <- rawdata() |>
          aggregate_poll(age_bounds = GLOBAL$bounds$poll$age)
      }


      if(global$covid) {
        c(patient, pstrat_data, covariates, raw_covariates) %<-% link_ACS(
            global$data,
            global$extdata$covid$tract_data,
            global$extdata$covid$zip_tract
          )

        c(input_data, new_data, levels, vars) %<-% prepare_data_covid(
            patient,
            pstrat_data,
            covariates,
            GLOBAL$levels$covid
          )


        global$mrp <- list(
          input = input_data,
          new = new_data,
          levels = levels,
          vars = vars
        )

        global$plotdata <- list(
          dates = if("date" %in% names(global$data)) get_dates(global$data) else NULL,
          geojson = filter_geojson(global$extdata$covid$map_geojson, global$mrp$levels$county),
          raw_covariates = raw_covariates
        )

      } else {
        global$data$state <- to_fips(global$data$state, global$extdata$poll$fips)

        covariates <- get_state_predictors(rawdata())
        covariates$state <- to_fips(covariates$state, global$extdata$poll$fips)

        c(input_data, new_data, levels, vars) %<-% prepare_data_poll(
          global$data,
          global$extdata$poll$pstrat_data,
          covariates,
          GLOBAL$levels$poll
        )

        global$mrp <- list(
          input = input_data,
          new = new_data,
          levels = levels,
          vars = vars
        )

        if("state" %in% names(global$data)) {
          global$plotdata <- list(
            geojson = filter_geojson(global$extdata$poll$map_geojson, global$mrp$levels$state)
          )
        }
      }

      waiter::waiter_hide()
    })

    observeEvent(input$use_agg_example, {
      if(global$covid) {
        readr::read_csv(app_sys("extdata/covid_test_records_aggregated.csv"), show_col_types = FALSE) |> rawdata()
        global$data <- rawdata() |> mutate(zip = as.character(zip))
      } else {
        readr::read_csv(app_sys("extdata/CES_data_aggregated.csv"), show_col_types = FALSE) |> rawdata()
        global$data <- rawdata()
      }

      if(global$covid) {
        c(patient, pstrat_data, covariates, raw_covariates) %<-% link_ACS(
            global$data,
            global$extdata$covid$tract_data,
            global$extdata$covid$zip_tract
          )

        c(input_data, new_data, levels, vars) %<-% prepare_data_covid(
            patient,
            pstrat_data,
            covariates,
            GLOBAL$levels$covid
          )


        global$mrp <- list(
          input = input_data,
          new = new_data,
          levels = levels,
          vars = vars
        )

        global$plotdata <- list(
          dates = if("date" %in% names(global$data)) get_dates(global$data) else NULL,
          geojson = filter_geojson(global$extdata$covid$map_geojson, global$mrp$levels$county),
          raw_covariates = raw_covariates
        )

      } else {
        global$data$state <- to_fips(global$data$state, global$extdata$poll$fips)

        covariates <- get_state_predictors(rawdata())
        covariates$state <- to_fips(covariates$state, global$extdata$poll$fips)

        c(input_data, new_data, levels, vars) %<-% prepare_data_poll(
          global$data,
          global$extdata$poll$pstrat_data,
          covariates,
          GLOBAL$levels$poll
        )

        global$mrp <- list(
          input = input_data,
          new = new_data,
          levels = levels,
          vars = vars
        )

        if("state" %in% names(global$data)) {
          global$plotdata <- list(
            geojson = filter_geojson(global$extdata$poll$map_geojson, global$mrp$levels$state)
          )
        }
      }

    })

    observeEvent(input$show_upload_guide, {
      show_guide("upload_data", global$session)
    })

    observeEvent(input$to_preprocess, {
      updateNavbarPage(global$session,
        inputId = "navbar",
        selected = "nav_learn_preprocess"
      )
    })
  })
}
