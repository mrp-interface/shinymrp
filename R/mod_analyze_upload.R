#' analyze_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analyze_upload_ui <- function(id) {
  ns <- NS(id)
  
  bslib::layout_sidebar(
    #---------------------------------------------------------------------------
    # Sidebar
    #---------------------------------------------------------------------------
    sidebar = sidebar(
      width = 350,

      bslib::accordion(
        id = ns("accordion"),
        multiple = FALSE,
        bslib::accordion_panel(
          title = "Sample",
          value = "sample",
          tags$p(tags$strong("Upload individual-level or aggregated data (examples below)")),
          shinyWidgets::radioGroupButtons(
            inputId = ns("toggle_input"),
            label = NULL,
            choices = c("Individual-level" = "indiv", "Aggregated" = "agg"),
            selected = "agg",
            justified = TRUE,
            size = "sm"
          ),
          fileInput(
            inputId = ns("sample_upload"),
            label = NULL,
            accept = GLOBAL$ui$data_accept
          ),
          uiOutput(ns("sample_feedback")),
          p(class = "mt-0 small",
            "For", tags$u("input data requirements,"), "open the",
            actionLink(ns("show_upload_guide"), label = "User Guide."),
            "For a detailed description of the preprocessing procedure and examples of preprocessing code, go to the",
            actionLink(ns("to_preprocess"), label = "Preprocessing"), "page."
          ),
          # Example data label
          div(class = "mt-4",
            conditionalPanel(
              condition = "output.data_format == 'temporal_covid'",
              p("Example: COVID-19 hospital test records", class = "fst-italic small")
            ),
            conditionalPanel(
              condition = "output.data_format == 'static_poll'",
              p("Example: The Cooperative Election Study data", class = "fst-italic small")
            ),
            conditionalPanel(
              condition = "output.data_format == 'temporal_other' || output.data_format == 'static_other'",
              p("Example", class = "fst-italic small")
            ),
            tags$div(
              class = "d-flex gap-2 mb-3",
              actionButton(ns("use_indiv_example"), "Individual-level", icon("table")),
              actionButton(ns("use_agg_example"), "Aggregated", icon("table"))
            )
          )
        ),
        bslib::accordion_panel(
          title = "Poststratification Data",
          value = "pstrat",
          tags$p("Upload poststratification data or provide information for linking the input data to the ACS data.",
            class = "small"
          ),
          tags$div(class = "mt-2",
            actionButton(
              inputId = ns("pstrat_upload_popover_btn"),
              label =  "Upload poststratification data",
              icon = icon("chevron-down"),
              class = "btn btn-sm btn-secondary"
            ),
            tags$div(id = ns("pstrat_upload_popover"),
              bslib::card(class = "mt-2",
                bslib::card_body(
                  fileInput(
                    inputId = ns("pstrat_upload"),
                    label = "Upload poststratification data",
                    accept = GLOBAL$ui$data_accept
                  )
                )
              )
            )
          ),
          tags$div(class = "mt-2",
            actionButton(
              inputId = ns("link_acs_popover_btn"),
              label = "Link to ACS Data",
              icon = icon("chevron-down"),
              class = "btn btn-sm btn-secondary"
            ),
            tags$div(id = ns("link_acs_popover"),
              bslib::card(class = "mt-2",
                bslib::card_body(
                  selectInput(ns("link_geo"), label = "Select geography scale for poststratification", choices = NULL),
                  selectInput(ns("acs_year"), label = "Select 5-year ACS data to link to", choices = NULL),
                  actionButton(ns("link_acs"), label = "Link", class = "btn-primary w-100") 
                )
              )
            )
          )
        )
      )
    ),
    #---------------------------------------------------------------------------
    # Main Window
    #---------------------------------------------------------------------------
    conditionalPanel(
      condition = sprintf("output['%s']", ns("file_uploaded")),
      bslib::layout_columns(
        col_widths = c(4, 8),
        conditionalPanel(
          condition = sprintf("output['%s'] == true", ns("data_processed")),
          div(class = "d-flex align-items-start gap-2",
            # Toggle button for table view
            shinyWidgets::radioGroupButtons(
              inputId = ns("toggle_table"),
              label = NULL,
              size = "sm",
              choices = c("Raw" = "raw", "Preprocessed" = "prep")
            ),
            # Download button for preprocessed data 
            conditionalPanel(
              condition = sprintf("input['%s'] == 'prep'", ns("toggle_table")),
              downloadButton(
                outputId = ns("download_preprocessed"),
                label = NULL,
                class = "btn btn-primary btn-sm"
              )
            )
          )
        ),
        # Info text
        tags$p(
          sprintf("*The preview only includes the first %d rows of the data", 
                 GLOBAL$ui$preview_size), 
          class = "small text-muted text-end"
        )
      ),
      DT::dataTableOutput(outputId = ns("table"))
    )
  )
}


#' analyze_upload Server Functions
#'
#' @noRd
mod_analyze_upload_server <- function(id, global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rawdata <- reactiveVal()
    input_errors <- reactiveVal()
    input_warnings <- reactiveVal()

    #---------------------------------------------------------------------------
    # Reactive outputs for conditional panels
    #---------------------------------------------------------------------------
    output$file_uploaded <- reactive(!is.null(rawdata()))
    outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
    
    output$data_processed <- reactive(!is.null(global$data))
    outputOptions(output, "data_processed", suspendWhenHidden = FALSE)


    # --------------------------------------------------------------------------
    # Reset everything when data format changes
    # --------------------------------------------------------------------------
    observeEvent(global$data_format, {
      shinyjs::reset("input_data")
      shinyjs::reset("toggle_input")
      shinyjs::reset("link_geo")
      shinyjs::reset("acs_year")

      rawdata(NULL)
      input_errors(NULL)
      input_warnings(NULL)
      global$data <- NULL
      global$mrp <- NULL
      global$plotdata <- NULL
      global$link_data <- NULL

      # reset the accordion to show the sample data panel
      bslib::accordion_panel_open(
        id = "accordion",
        values = "sample",
        session = session
      )

      # reset the poststratification data panel
      shinyjs::hide("link_acs_popover")
      shinyjs::show("pstrat_upload_popover")
    })
    
    # --------------------------------------------------------------------------
    # Make poststratification data options exclusive
    # --------------------------------------------------------------------------
    observeEvent(
      eventExpr = list(
        input$pstrat_upload_popover_btn,
        input$link_acs_popover_btn
      ),
      handlerExpr = {
        shinyjs::toggle(id = "pstrat_upload_popover")
        shinyjs::toggle(id = "link_acs_popover")
      }
    )


    # --------------------------------------------------------------------------
    # Show feedback about input data
    # --------------------------------------------------------------------------
    output$sample_feedback <- renderUI({
      req(rawdata())
      
      if (!is.null(input_errors())) {
        if (length(input_errors()) > 0) {
          tags$div(
            tagList(icon("circle-xmark", "fa"), "Error"),
            tags$p("Input data does not meet all requirements. Please check the user guide for input data requirements.", class = "small"),
          )
        } else {
          tags$div(
            tagList(icon("circle-check", "fa"), "Success"),
            tags$p("All requirements are met. You may proceed to data linking or the next page.", class = "small")
          )
        }
      }
    })

    
    # Table output renderer
    output$table <- DT::renderDT({
      req(rawdata())
      
      df <- if(is.null(input$toggle_table) || input$toggle_table == "raw") {
        rawdata()
      } else {
        req(global$data) # Ensure global$data exists when "prep" is selected
        global$data
      }
      
      df |>
        head(GLOBAL$ui$preview_size) |>
        DT::datatable(
          options = list(
            scrollX = TRUE,
            lengthChange = FALSE,
            searching = FALSE,
            info = FALSE
          )
        )
    })

    # Preprocessed data download handler
    output$download_preprocessed <- downloadHandler(
      filename = function() {
        paste0("preprocessed_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(global$data)
        readr::write_csv(global$data, file)
      }
    )

    # Handle file upload
    observeEvent(input$sample_upload, {
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
        path <- input$sample_upload$datapath
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

          # Find and rename columns
          data <- if(global$data_format == "temporal_covid" &&
                      input$toggle_input == "indiv") {
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
      }, finally = {
        # Always hide the waiter
        waiter::waiter_hide()
      })
    })

    # Use individual-level example data
    observeEvent(input$use_indiv_example, {
      waiter::waiter_show(
        html = waiter_ui("wait"),
        color = waiter::transparent(0.9)
      )

      file_name <- switch(global$data_format,
        "temporal_covid" = "covid_data_individual.csv",
        "temporal_other" = "timevarying_data_individual.csv",
        "static_poll" = "ces_data_individual.csv",
        "static_other" = "crosssectional_data_individual.csv"
      )

      readr::read_csv(app_sys(paste0("extdata/example/data/", file_name)), show_col_types = FALSE) |> rawdata()

      cleaned_data <- rawdata() |> clean_data()

      # Use the appropriate aggregation function
      if(global$data_format == "temporal_covid") {
        global$data <- cleaned_data |> aggregate_covid(expected_levels = GLOBAL$levels$temporal_covid)
      } else {
        global$data <- cleaned_data |> aggregate_data(expected_levels = GLOBAL$levels[[global$data_format]])
      }
      
      waiter::waiter_hide()
    })

    # Use aggregated example data
    observeEvent(input$use_agg_example, {
      waiter::waiter_show(
        html = waiter_ui("wait"),
        color = waiter::transparent(0.9)
      )

      file_name <- switch(global$data_format,
        "temporal_covid" = "covid_data_aggregated.csv",
        "temporal_other" = "timevarying_data_aggregated.csv",
        "static_poll" = "ces_data_aggregated.csv",
        "static_other" = "crosssectional_data_aggregated.csv"
      )

      readr::read_csv(app_sys(paste0("extdata/example/data/", file_name)), show_col_types = FALSE) |> rawdata()
      global$data <- rawdata() |> clean_data()

      waiter::waiter_hide()
    })


    #---------------------------------------------------------------------------
    # Update select input for linking to ACS
    #---------------------------------------------------------------------------
    observeEvent(global$data, {
      req(global$data)

      if(global$data_format == "temporal_covid") {
        link_geos <- c("zip")
        acs_years <- 2021
      } else if (global$data_format == "static_poll") {
        link_geos <- c("state")
        acs_years <- 2018
      } else {
        smallest_geo_index <- intersect(names(global$data), GLOBAL$vars$geo) |>
          purrr::map_int(~which(GLOBAL$vars$geo == .x)) |>
          min()
        link_geos <- c(GLOBAL$vars$geo[smallest_geo_index:length(GLOBAL$vars$geo)], "Do not include geography")
        acs_years <- 2019:2023
      }

      updateSelectInput(session,
        inputId = "link_geo",
        choices = link_geos
      )

      updateSelectInput(session,
        inputId = "acs_year",
        choices = paste0(acs_years - 4, "-", acs_years)
      )

      # Update the accordion to show the poststratification data panel
      bslib::accordion_panel_open(
        id = "accordion",
        values = "pstrat",
        session = session
      )

      waiter::waiter_hide()
    })
    

    #---------------------------------------------------------------------------
    # Create poststratification data
    #---------------------------------------------------------------------------
    observeEvent(input$link_acs, {
      req(global$data)

      start_busy(
        session = session,
        id = "link_acs",
        label = "Linking..."
      )

      # delay the execution to allow the UI to update
      shinyjs::delay(10, {
        success <- FALSE

        tryCatch({
          # store user's selections for data linking
          global$link_data <- list(
            link_geo = if(input$link_geo %in% GLOBAL$vars$geo) input$link_geo else NULL,
            acs_year = input$acs_year
          )

          if(global$data_format == "temporal_covid") {
            # prepare data for MRP
            c(input_data, new_data, levels, vars) %<-% prepare_data_covid(
              global$data,
              global$extdata$pstrat_covid,
              global$extdata$covar_covid,
              GLOBAL$levels$temporal_covid,
              GLOBAL$vars
            )
            
            # set global MRP data
            global$mrp <- list(
              input = input_data,
              new = new_data,
              levels = levels,
              vars = vars
            )

            # prepare data for plotting
            global$plotdata <- list(
              dates = if("date" %in% names(global$data)) get_dates(global$data) else NULL,
              geojson = list(county = filter_geojson(global$extdata$geojson$county, global$mrp$levels$county)),
              raw_covariates = global$extdata$covar_covid |> filter(zip %in% unique(input_data$zip))
            )

          } else if (global$data_format == "static_poll") {
            # prepare data for MRP
            c(input_data, new_data, levels, vars) %<-% prepare_data_poll(
              global$data,
              global$extdata$pstrat_poll,
              global$extdata$fips$county,
              GLOBAL$levels$static_poll,
              GLOBAL$vars
            )
            
            # set global MRP data
            global$mrp <- list(
              input = input_data,
              new = new_data,
              levels = levels,
              vars = vars
            )

            # prepare data for plotting
            global$plotdata <- list(
              geojson = list(state = filter_geojson(global$extdata$geojson$state, global$mrp$levels$state))
            )

          } else {
            # retrieve ACS data based on user's selection
            tract_data <- readr::read_csv(app_sys(stringr::str_interp("extdata/acs/acs_${global$link_data$acs_year}.csv")), show_col_types = FALSE)

            # prepare data for MRP
            c(input_data, new_data, levels, vars) %<-% prepare_data(
              input_data = global$data,
              tract_data = tract_data,
              zip_tract = global$extdata$zip_tract,
              zip_county_state = global$extdata$zip_county_state,
              demo_levels = GLOBAL$levels[[global$data_format]],
              vars_global = GLOBAL$vars,
              link_geo = global$link_data$link_geo
            )
            
            # set global MRP data
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

            global$plotdata <- nullify(plotdata)
          }
          
          # set success to TRUE if no errors occurred
          success <- TRUE
        }, error = function(e) {
          warnings(paste("Error linking data:", e$message))
        }, finally = {
          stop_busy(
            session = session,
            id = "link_acs",
            label = if(success) "Linking complete" else "Linking failed",
            success = success
          )
        })
      })

    })


    observeEvent(
      eventExpr = list(
        global$data_format,
        global$data,
        input$link_geo,
        input$acs_year
      ),
      handlerExpr = {
        updateActionButton(
          session = session,
          inputId = "link_acs",
          label = "Link",
          icon = character(0)
        )
      }
    )


    observeEvent(input$show_upload_guide, {
      show_guide("upload")
    })

    observeEvent(input$to_preprocess, {
      bslib::nav_select(
        id = "navbar",
        selected = "nav_learn_preprocess",
        session = global$session
      )
    })

  })
}
