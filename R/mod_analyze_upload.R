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
        tags$h4(tags$u("Step 1"), " Upload data"),
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
        uiOutput(outputId = ns("input_feedback")),
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
            condition = "output.data_format == 'temporal_covid'",
            tags$p("Example: COVID-19 hospital test records")
          ),
          conditionalPanel(
            condition = "output.data_format == 'static_poll'",
            tags$p("Example: The Cooperative Election Study data")
          ),
          conditionalPanel(
            condition = "output.data_format == 'temporal_other' || output.data_format == 'static_other'",
            tags$p("Example")
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
          ),
          tags$div(style = "margin-top: 40px",
            tags$h4(tags$u("Step 2"), " Link to ACS data"),
            conditionalPanel(
              condition = "output.data_format == 'temporal_covid'",
              tags$div(
                class = "panel panel-info",
                tags$div(
                  class = "panel-heading",
                  tagList(icon("circle-info", "fa"), "Note")
                ),
                tags$div(
                  class = "panel-body",
                  tags$p("Input data is automatically linked to 5-year ACS data (2017-2021) through ZIP codes")
                )
              )
            ),
            conditionalPanel(
              condition = "output.data_format == 'static_poll'",
              tags$div(
                class = "panel panel-info",
                tags$div(
                  class = "panel-heading",
                  tagList(icon("circle-info", "fa"), "Note")
                ),
                tags$div(
                  class = "panel-body",
                  tags$p("Input data is automatically linked to 5-year ACS data (2013-2018) through state")
                )
              )
            ),
            conditionalPanel(
              condition = "output.data_format != 'temporal_covid' &&
                           output.data_format != 'static_poll'", 
              selectInput(
                inputId = ns("link_geo"),
                label = "Select geography level for poststratification",
                choices = NULL
              ),
              selectInput(
                inputId = ns("acs_year"),
                label = "Select year 5-year ACS data to link to",
                choices = NULL
              ),
                tags$div(class = "justify",
                actionButton(
                  inputId = ns("link_acs"),
                  label = "Link",
                  width = "30%"
                ),
                textOutput(ns("link_status"), inline = TRUE)
                )
            )
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
    link_status <- reactiveVal()
    input_errors <- reactiveVal()
    input_warnings <- reactiveVal()

    observeEvent(global$input$navbar, {
      if(global$input$navbar == "nav_analyze" && is.null(global$data_format)) {
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

    observeEvent(global$data_format, {
      shinyjs::reset("input_data")
      shinyjs::reset("toggle_input")
      shinyjs::reset("link_geo")
      shinyjs::reset("acs_year")

      rawdata(NULL)
      link_status(NULL)
      global$data <- NULL
      global$mrp <- NULL
      global$plotdata <- NULL
    })
    
    output$input_feedback <- renderUI({
      req(rawdata())
      
      if (length(input_errors()) > 0) {
        tags$div(
          class = "panel panel-danger",
          tags$div(
            class = "panel-heading",
            tagList(icon("circle-xmark", "fa"), "Error")
          ),
          tags$div(
            class = "panel-body",
            tags$p("Input data does not meet all requirements. Please check Guide (bottom right corner) for input data requirements.")
          )
        )
          
      } else {
        tags$div(
          class = "panel panel-success",
          tags$div(
            class = "panel-heading",
            tagList(icon("circle-check", "fa"), "Success")
          ),
          tags$div(
            class = "panel-body",
            tags$p("All requirements are met. You may proceed to data linking or the next page.")
          )
        )
      }
    })

    output$link_status <- renderText(link_status())
    
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
          shinyBS::bsTooltip(ns("toggle_table"), "\"Preprocessed\" table only shows when data has been preprocessed", placement = "right"),
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
      waiter::waiter_show(
        html = waiter_ui("wait"),
        color = waiter::transparent(0.9)
      )
      
      # Reset state
      global$data <- NULL
      global$mrp <- NULL
      global$plotdata <- NULL

      # Read in data
      tryCatch({
        # Read in data first
        path <- input$input_data$datapath
        if(stringr::str_ends(path, "csv")) {
          rawdata(readr::read_csv(path, show_col_types = FALSE))
        } else if (stringr::str_ends(path, "(xlsx|xls)")) {
          rawdata(readxl::read_excel(path, guess_max = 5000))
        } else if (stringr::str_ends(path, "sas7bdat")) {
          rawdata(haven::read_sas(path))
        }
        
        errors <- list()
        warnings <- list()
        
        # Process the data only if reading was successful
        if(!is.null(rawdata())) {
          # Clean data
          data <- clean_data(rawdata())

          data <- if(global$data_format == "temporal_covid") {
            rename_columns_covid(data)
          } else {
            rename_columns(data)
          }

          # Aggregate if needed
          if(input$toggle_input == "indiv") {
            # Check for common dataframe issues
            c(errors, warnings) %<-% check_data(
              data,
              GLOBAL$expected_types$indiv[[global$data_format]]
            )
            print(errors)

            if(length(errors) == 0) {
              if(global$data_format == "temporal_covid") {
                data <- data |> aggregate_covid(GLOBAL$levels$temporal_covid)
              } else {
                data <- data |> aggregate_data(GLOBAL$levels[[global$data_format]])
              }
            }
          } else {
            # Check for common dataframe issues
            c(errors, warnings) %<-% check_data(
              data, 
              GLOBAL$expected_types$agg[[global$data_format]]
            )
          }
          
          # Update global data only if no errors
          if(length(errors) == 0) {
            global$data <- data
          }
        }
        
        # Update reactives with validation results
        input_errors(errors)
        input_warnings(warnings)
        
      }, error = function(e) {
        # Capture the actual error message
        err_msg <- paste("Error processing data:", e$message)
        input_errors(list(unexpected = err_msg))
        warning(err_msg) # Log the error to console
      }, finally = {
        # Always hide the waiter
        waiter::waiter_hide()
      })
    })

    observeEvent(input$use_indiv_example, {
      waiter::waiter_show(
        html = waiter_ui("wait"),
        color = waiter::transparent(0.9)
      )

      if(global$data_format == "temporal_covid") {
        readr::read_csv(app_sys("extdata/example/data/covid_test_records_individual.csv"), show_col_types = FALSE) |> rawdata()
        global$data <- rawdata() |>
          clean_data() |>
          aggregate_covid(expected_levels = GLOBAL$levels$temporal_covid)
      } else if (global$data_format == "temporal_other") {
        readr::read_csv(app_sys("extdata/example/data/timevarying_data_individual.csv"), show_col_types = FALSE) |> rawdata()
        global$data <- rawdata() |>
          clean_data() |>
          aggregate_data(expected_levels = GLOBAL$levels$temporal_other)
      } else if(global$data_format == "static_poll") {
        readr::read_csv(app_sys("extdata/example/data/CES_data_individual.csv"), show_col_types = FALSE) |> rawdata()
        global$data <- rawdata() |>
          clean_data() |>
          aggregate_data(expected_levels = GLOBAL$levels$static_poll)
      } else if(global$data_format == "static_other") {
        readr::read_csv(app_sys("extdata/example/data/crosssectional_data_individual.csv"), show_col_types = FALSE) |> rawdata()
        global$data <- rawdata() |>
          clean_data() |>
          aggregate_data(expected_levels = GLOBAL$levels$static_other)
      }
      
      input_errors(NULL)
      input_warnings(NULL)
      
      waiter::waiter_hide()
    })

    observeEvent(input$use_agg_example, {
      if(global$data_format == "temporal_covid") {
        readr::read_csv(app_sys("extdata/example/data/covid_test_records_aggregated.csv"), show_col_types = FALSE) |> rawdata()
        global$data <- rawdata() |> clean_data()
      } else if(global$data_format == "temporal_other") {
        readr::read_csv(app_sys("extdata/example/data/timevarying_data_aggregated.csv"), show_col_types = FALSE) |> rawdata()
        global$data <- rawdata() |> clean_data()
      } else if(global$data_format == "static_poll") {
        readr::read_csv(app_sys("extdata/example/data/CES_data_aggregated.csv"), show_col_types = FALSE) |> rawdata()
        global$data <- rawdata() |> clean_data()
      } else if(global$data_format == "static_other") {
        readr::read_csv(app_sys("extdata/example/data/crosssectional_data_aggregated.csv"), show_col_types = FALSE) |> rawdata()
        global$data <- rawdata() |> clean_data()
      }
      
      input_errors(NULL)
      input_warnings(NULL)
    })

    observeEvent(global$data, {
      link_status(NULL)

      if(!is.null(global$data)) {

        if(global$data_format == "temporal_covid") {
          c(input_data, new_data, levels, vars) %<-% prepare_data_covid(
            global$data,
            global$extdata$pstrat_covid,
            global$extdata$covar_covid,
            GLOBAL$levels$temporal_covid,
            GLOBAL$vars
          )
          
          global$mrp <- list(
            input = input_data,
            new = new_data,
            levels = levels,
            vars = vars
          )

          global$plotdata <- list(
            dates = if("date" %in% names(global$data)) get_dates(global$data) else NULL,
            geojson = list(county = filter_geojson(global$extdata$geojson$county, global$mrp$levels$county)),
            raw_covariates = global$extdata$covar_covid |> filter(zip %in% unique(input_data$zip))
          )
          
          global$link_data <- list(
            link_geo = "zip",
            acs_year = NULL
          )

        } else if (global$data_format == "static_poll") {
          c(input_data, new_data, levels, vars) %<-% prepare_data_poll(
            global$data,
            global$extdata$pstrat_poll,
            global$extdata$fips$county,
            GLOBAL$levels$static_poll,
            GLOBAL$vars
          )
          
          global$mrp <- list(
            input = input_data,
            new = new_data,
            levels = levels,
            vars = vars
          )

          global$plotdata <- list(
            geojson = list(state = filter_geojson(global$extdata$geojson$state, global$mrp$levels$state))
          )

          global$link_data <- list(
            link_geo = "state",
            acs_year = NULL
          )
        } else {
          smallest_geo_index <- intersect(names(global$data), GLOBAL$vars$geo) |>
            purrr::map_int(~which(GLOBAL$vars$geo == .x)) |>
            min()
          choices <- c(GLOBAL$vars$geo[smallest_geo_index:length(GLOBAL$vars$geo)], "Do not include geography")

          updateSelectInput(session,
            inputId = "link_geo",
            choices = choices
          )
          
          years <- 2019:2023
          choices <- paste0(years - 4, "-", years)
          updateSelectInput(session,
            inputId = "acs_year",
            choices = choices
          )
        }
      }

      waiter::waiter_hide()
    })
    
    observeEvent(input$link_acs, {
      # prepare data for model fitting and plotting
      if(!is.null(global$data)) {
        waiter::waiter_show(
          html = waiter_ui("wait"),
          color = waiter::transparent(0.9)
        )

        # store user's selections for data linking
        global$link_data <- list(
          link_geo = if(input$link_geo %in% GLOBAL$vars$geo) input$link_geo else NULL,
          acs_year = input$acs_year
        )
        
        # retrieve ACS data based on user's selection
        tract_data <- readr::read_csv(app_sys(stringr::str_interp("extdata/acs/acs_${global$link_data$acs_year}.csv")), show_col_types = FALSE)

        c(input_data, new_data, levels, vars) %<-% prepare_data(
          input_data = global$data,
          tract_data = tract_data,
          zip_tract = global$extdata$zip_tract,
          zip_county_state = global$extdata$zip_county_state,
          demo_levels = GLOBAL$levels[[global$data_format]],
          vars_global = GLOBAL$vars,
          link_geo = global$link_data$link_geo
        )
        
        global$mrp <- list(
          input = input_data,
          new = new_data,
          levels = levels,
          vars = vars,
          link_geo = global$link_data$link_geo
        )

        # prepare data for plotting
        plotdata <- list()
        plotdata$dates <- if("date" %in% names(global$data)) get_dates(global$data) else NULL
        plotdata$geojson <- names(global$extdata$geojson) |>
          setNames(nm = _) |>
          purrr::map(~filter_geojson(
            geojson = global$extdata$geojson[[.x]], 
            geoids = global$mrp$levels[[.x]]
          ))

        global$plotdata <- if(length(plotdata) > 0) plotdata else NULL

        link_status("Linked successfully")

        waiter::waiter_hide()
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
