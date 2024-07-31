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
        tags$p("Indicate input data format then select a file (CSV/Excel/SAS)"),
        shinyWidgets::radioGroupButtons(
            inputId = ns("toggle_input"),
            label = NULL,
            choices = c("Individual Data" = "indiv", "Aggregated Data" = "agg"),
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
        tags$div(class = "pad_bottom",
          HTML("<details><summary>Example</summary>"),
          tags$div(class = "justify pad_top",
            actionButton(
              inputId = ns("use_indiv_example"),
              label = "Invididual data",
              icon = icon("table", class = "fa button_icon"),
              width = "49.5%"
            ),
            actionButton(
              inputId = ns("use_agg_example"),
              label = "Aggregated data",
              icon = icon("table", class = "fa button_icon"),
              width = "49.5%"
            ),
          ),
          HTML("</details>"),
        ),
        tags$p(
          "For ", tags$u("requirements for input data"), "and preprocessing code, go to the",
          actionLink(
            inputId = ns("to_interface"),
            label = "Interface",
            class = "action_link"
          ),
          "page. For a detailed description of the prepropressing procedure, go to the",
          actionLink(
            inputId = ns("to_preprocess"),
            label = "Preprocessing",
            class = "action_link"
          ),
          "page."
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

    observeEvent(global$covid, {
      shinyjs::reset("input_data")
      shinyjs::reset("toggle_input")

      rawdata(NULL)
      global$data <- NULL
      global$mrp_input <- NULL
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
          tags$p("*The table only shows a subset of the data")
        ),
        DT::dataTableOutput(outputId = ns("table")),
        downloadButton(
          outputId = ns("download_data"),
          label = "Download"
        )
      )
    })


    output$table <- DT::renderDataTable({
      df <- if(input$toggle_table == "raw") rawdata() else global$data

      df |>
        head(100) |>
        DT::datatable(
          options = list(
            scrollX = TRUE,
            lengthChange = FALSE
          )
        )
    })

    observeEvent(input$input_data, {

      # reset variables
      global$data <- NULL
      global$mrp_input <- NULL
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
              aggregate_covid(age_bounds = global$static$bounds$covid$age) |>
              prep(list(), to_char = c("zip"))
          } else {
            global$data <- rawdata() |>
              aggregate_poll(age_bounds = global$static$bounds$poll$age)
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
            errors <- check_covid_data(rawdata(), global$static$expected_columns$covid)

            if(length(errors) == 0) {
              global$data <- rawdata() |>
                find_columns(global$static$expected_columns$covid) |>
                prep(global$static$levels$covid,
                  to_lower = c("sex", "race"),
                  to_char = c("zip")
                )
            } else if(length(errors) == 1 & "date" %in% names(errors)) {
              global$data <- rawdata() |>
                find_columns(global$static$expected_columns$covid) |>
                prep(global$static$levels$covid,
                  to_lower = c("sex", "race"),
                  to_char = c("zip")
                ) |>
                select(-date)
            }
          } else {
            errors <- check_poll_data(rawdata(), global$static$expected_columns$poll)

            if(length(errors) == 0) {
              global$data <- rawdata() |>
                find_columns(global$static$expected_columns$poll) |>
                prep(global$static$levels$poll,
                  to_lower = c("sex", "race", "edu")
                )
            } else if(length(errors) == 1 & "state" %in% names(errors)) {
              global$data <- rawdata() |>
                find_columns(global$static$expected_columns$poll) |>
                prep(global$static$levels$poll,
                  to_lower = c("sex", "race", "edu")
                ) |>
                select(-state)
            }
          }
        }, silent = TRUE)

        if ("try-error" %in% class(out)) {
          show_alert("Input data does not meet all requirements. Please check the Learn > Interface page for input data requirements.", global$session)
        } else {
          if(length(errors) == 0) {
            show_notif("All requirements are met. You may proceed to the next page.", global$session)
          } else {
            show_alert(
              tagList(
                tags$ul(
                  purrr::map(unlist(errors), ~ tags$li(.x))
                )
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

          c(brms_input, brms_new, levels, vars) %<-% prepare_brms_covid(
              patient,
              pstrat_data,
              covariates,
              global$static$levels$covid
            )

          global$mrp_input <- list(
            brms_input = brms_input,
            brms_new = brms_new,
            levels = levels,
            vars = vars
          )

          global$plotdata <- list(
            dates = if("date" %in% names(global$data)) get_dates(global$data) else NULL,
            geojson = filter_geojson(global$extdata$covid$map_geojson, global$mrp_input$levels$county),
            raw_covariates = raw_covariates
          )

        } else {
          global$data$state <- to_fips(global$data$state, global$extdata$poll$fips)

          covariates <- get_state_predictors(rawdata())
          covariates$state <- to_fips(covariates$state, global$extdata$poll$fips)

          c(brms_input, brms_new, levels, vars) %<-% prepare_brms_poll(
            global$data,
            global$extdata$poll$pstrat_data,
            covariates,
            global$static$levels$poll
          )

          global$mrp_input <- list(
            brms_input = brms_input,
            brms_new = brms_new,
            levels = levels,
            vars = vars
          )

          if("state" %in% names(global$data)) {
            global$plotdata <- list(
              geojson = filter_geojson(global$extdata$poll$map_geojson, global$mrp_input$levels$state)
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
          aggregate_covid(age_bounds = global$static$bounds$covid$age) |>
          prep(list(), to_char = c("zip"))
      } else {
        readr::read_csv(app_sys("extdata/CES_data_individual.csv"), show_col_types = FALSE) |> rawdata()

        global$data <- rawdata() |>
          aggregate_poll(age_bounds = global$static$bounds$poll$age)
      }


      if(global$covid) {
        c(patient, pstrat_data, covariates, raw_covariates) %<-% link_ACS(
            global$data,
            global$extdata$covid$tract_data,
            global$extdata$covid$zip_tract
          )

        c(brms_input, brms_new, levels, vars) %<-% prepare_brms_covid(
            patient,
            pstrat_data,
            covariates,
            global$static$levels$covid
          )

        global$mrp_input <- list(
          brms_input = brms_input,
          brms_new = brms_new,
          levels = levels,
          vars = vars
        )

        global$plotdata <- list(
          dates = if("date" %in% names(global$data)) get_dates(global$data) else NULL,
          geojson = filter_geojson(global$extdata$covid$map_geojson, global$mrp_input$levels$county),
          raw_covariates = raw_covariates
        )

      } else {
        global$data$state <- to_fips(global$data$state, global$extdata$poll$fips)

        covariates <- get_state_predictors(global$data)
        covariates$state <- to_fips(covariates$state, global$extdata$poll$fips)

        c(brms_input, brms_new, levels, vars) %<-% prepare_brms_poll(
          global$data,
          global$extdata$poll$pstrat_data,
          covariates,
          global$static$levels$poll
        )

        global$mrp_input <- list(
          brms_input = brms_input,
          brms_new = brms_new,
          levels = levels,
          vars = vars
        )

        if("state" %in% names(global$data)) {
          global$plotdata <- list(
            geojson = filter_geojson(global$extdata$poll$map_geojson, global$mrp_input$levels$state)
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

        c(brms_input, brms_new, levels, vars) %<-% prepare_brms_covid(
            patient,
            pstrat_data,
            covariates,
            global$static$levels$covid
          )

        global$mrp_input <- list(
          brms_input = brms_input,
          brms_new = brms_new,
          levels = levels,
          vars = vars
        )

        global$plotdata <- list(
          dates = if("date" %in% names(global$data)) get_dates(global$data) else NULL,
          geojson = filter_geojson(global$extdata$covid$map_geojson, global$mrp_input$levels$county),
          raw_covariates = raw_covariates
        )

      } else {
        global$data$state <- to_fips(global$data$state, global$extdata$poll$fips)

        covariates <- get_state_predictors(global$data)
        covariates$state <- to_fips(covariates$state, global$extdata$poll$fips)

        c(brms_input, brms_new, levels, vars) %<-% prepare_brms_poll(
          global$data,
          global$extdata$poll$pstrat_data,
          covariates,
          global$static$levels$poll
        )

        global$mrp_input <- list(
          brms_input = brms_input,
          brms_new = brms_new,
          levels = levels,
          vars = vars
        )

        if("state" %in% names(global$data)) {
          global$plotdata <- list(
            geojson = filter_geojson(global$extdata$poll$map_geojson, global$mrp_input$levels$state)
          )
        }
      }

    })

    observeEvent(input$to_interface, {
      updateNavbarPage(global$session,
        inputId = "navbar",
        selected = "nav_learn_interface"
      )
    })

    observeEvent(input$to_preprocess, {
      updateNavbarPage(global$session,
        inputId = "navbar",
        selected = "nav_learn_preprocess"
      )
    })

    output$download_data <- downloadHandler(
      filename = function() { if(input$toggle_table == "raw") "raw_data.csv" else "preprocessed_data.csv" },
      content = function(file) {
        df <- if(input$toggle_table == "raw") rawdata() else global$data

        if(is.null(df)) {
          readr::write_csv(data.frame(), file)
        } else {
          readr::write_csv(df, file)
        }

      }
    )
  })
}
