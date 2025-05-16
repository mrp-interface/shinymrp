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
      width = 375,

      bslib::accordion(
        id = ns("accordion"),
        multiple = FALSE,
        bslib::accordion_panel(
          title = "Sample",
          value = "sample",
          tags$p(tags$strong("Upload individual-level or aggregated sample data")),
          shinyWidgets::radioGroupButtons(
            inputId = ns("toggle_sample"),
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
          tags$div(class = "mt-4",
            conditionalPanel(
              condition = "output.data_format == 'temporal_covid'",
              tags$p(tags$u("Example"), ": COVID-19 hospital test records")
            ),
            conditionalPanel(
              condition = "output.data_format == 'static_poll'",
              tags$p(tags$u("Example"), ": 2018 Cooperative Congressional Election Study")
            ),
            conditionalPanel(
              condition = "output.data_format == 'temporal_other' || output.data_format == 'static_other'",
              tags$p(tags$u("Example data"))
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
          conditionalPanel(
            condition = sprintf("!output['%s']", ns("data_processed")),
            bslib::card(
              class = "bg-warning mb-3",  # yellow background & border
              bslib::card_body(
                tags$div(
                  style = "display: flex; align-items: center;",
                  shiny::icon("exclamation-triangle", class = "me-2"),  # Bootstrap margin-end
                  tags$span("Please upload sample data first", class = "fw-semibold")
                )
              )
            )
          ),
          conditionalPanel(
            condition = "output.data_format == 'temporal_covid' || output.data_format == 'static_poll'",
            tags$p("Provide information for linking the input data to the ACS data.",
              class = "small"
            ),
          ),
          conditionalPanel(
            condition = "output.data_format == 'temporal_other' || output.data_format == 'static_other'",
            tags$p("Provide information for linking the input data to the ACS data or upload poststratification data.",
              class = "small"
            ),
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
                  selectizeInput(ns("link_geo"), label = "Select geography scale for poststratification", choices = NULL),
                  selectizeInput(ns("acs_year"), label = "Select 5-year ACS data to link to", choices = NULL, options = list(dropdownParent = "body")),
                  actionButton(ns("link_acs"), label = "Link", class = "btn-primary w-100") 
                )
              )
            )
          ),
          conditionalPanel(
            condition = "output.data_format == 'temporal_other' || output.data_format == 'static_other'",
            tags$div(class = "mt-2",
              actionButton(
                inputId = ns("pstrat_upload_popover_btn"),
                label =  "Upload poststratification data",
                icon = icon("chevron-down"),
                class = "btn btn-sm btn-secondary"
              ),
              tags$div(id = ns("pstrat_upload_popover"),
                bslib::card(class = "mt-2",
                  bslib::card_body(class = "gap-3",
                    tags$p(tags$strong("Upload individual-level or aggregated poststratification data")),
                    shinyWidgets::radioGroupButtons(
                      inputId = ns("toggle_pstrat"),
                      label = NULL,
                      choices = c("Individual-level" = "indiv", "Aggregated" = "agg"),
                      selected = "agg",
                      justified = TRUE,
                      size = "sm"
                    ),
                    fileInput(
                      inputId = ns("pstrat_upload"),
                      label = NULL,
                      accept = GLOBAL$ui$data_accept
                    ),
                    uiOutput(ns("pstrat_feedback")),
                    tags$p(class = "mt-0", tags$u("Example data")),
                    downloadButton(
                      outputId = ns("save_pstrat_example"),
                      label = "Aggregated",
                      class = "btn w-100"
                    )
                  )
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
    
    raw_sample <- reactiveVal()
    raw_pstrat <- reactiveVal()
    sample_errors <- reactiveVal()
    pstrat_errors <- reactiveVal()
    

    #---------------------------------------------------------------------------
    # Reactive outputs for conditional panels
    #---------------------------------------------------------------------------
    output$file_uploaded <- reactive(!is.null(raw_sample()))
    outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
    
    output$data_processed <- reactive(!is.null(global$data))
    outputOptions(output, "data_processed", suspendWhenHidden = FALSE)


    # --------------------------------------------------------------------------
    # Reset everything when data format changes
    # --------------------------------------------------------------------------
    observeEvent(global$data_format, {
      shinyjs::reset("sample_upload")
      shinyjs::reset("pstrat_upload")
      shinyjs::reset("toggle_sample")
      shinyjs::reset("toggle_pstrat")
      shinyjs::reset("link_geo")
      shinyjs::reset("acs_year")

      raw_sample(NULL)
      raw_pstrat(NULL)
      sample_errors(NULL)
      pstrat_errors(NULL)
      global$data <- NULL
      global$mrp <- NULL
      global$plot_data <- NULL
      global$link_data <- NULL

      # reset the accordion to show the sample data panel
      bslib::accordion_panel_open(
        id = "accordion",
        values = "sample",
        session = session
      )

      # reset the poststratification data panel
      shinyjs::show("link_acs_popover")
      shinyjs::hide("pstrat_upload_popover")
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
      req(raw_sample())

      if (!is.null(sample_errors())) {
        if (length(sample_errors()) > 0) {
          tags$div(
            tagList(icon("circle-xmark", "fa"), "Error"),
            tags$p("Input data does not meet all requirements. Please check the user guide for data requirements.", class = "small"),
          )
        } else {
          tags$div(
            tagList(icon("circle-check", "fa"), "Success"),
            tags$p("All requirements are met. You may proceed to the Poststratification Data section'.", class = "small")
          )
        }
      }
    })

    
    # Table output renderer
    output$table <- DT::renderDT({
      req(raw_sample())
      
      df <- if(is.null(input$toggle_table) || input$toggle_table == "raw") {
        raw_sample()
      } else {
        req(global$data) # Ensure global$data exists when "prep" is selected
        global$data
      }
      
      df |>
        head(GLOBAL$ui$preview_size) |>
        DT::datatable(
          options = list(
            columnDefs = list(
              list(className = "dt-left", targets = "_all")
            ),
            scrollX = TRUE,
            lengthChange = FALSE,
            searching = FALSE,
            info = FALSE
          )
        ) |>
        DT::formatStyle(
          columns = c("positive"),
          `max-width` = "150px"
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

    # Handle sample data upload
    observeEvent(input$sample_upload, {
      waiter::waiter_show(
        html = waiter_ui("wait"),
        color = waiter::transparent(0.9)
      )
      
      # Reset state
      global$data <- NULL
      global$mrp <- NULL
      global$plot_data <- NULL

      tryCatch({
        read_data(input$sample_upload$datapath) |> raw_sample()

        global$data <- preprocess(
          data = raw_sample(),
          data_format = global$data_format,
          zip_county_state = global$extdata$zip_county_state,
          const = GLOBAL,
          is_sample = TRUE,
          is_aggregated = input$toggle_sample == "agg"
        )

        sample_errors(character(0))
      
      }, error = function(e) {
        # show error message
        error_message <- paste("Error processing data:\n", e$message)
        sample_errors(error_message)
        message(error_message)
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

      indiv_file_name <- switch(global$data_format,
        "temporal_covid" = "timevarying_covid_individual.csv",
        "temporal_other" = "timevarying_other_individual.csv",
        "static_poll"    = "crosssectional_poll_individual.csv",
        "static_other"   = "crosssectional_other_individual.csv"
      )

      agg_file_name <- switch(global$data_format,
        "temporal_covid" = "timevarying_covid_aggregated.csv",
        "temporal_other" = "timevarying_other_aggregated.csv",
        "static_poll"    = "crosssectional_poll_aggregated.csv",
        "static_other"   = "crosssectional_other_aggregated.csv"
      )

      readr::read_csv(app_sys(paste0("extdata/example/data/", indiv_file_name)), show_col_types = FALSE) |> raw_sample()
      global$data <- readr::read_csv(app_sys(paste0("extdata/example/data/", agg_file_name)), show_col_types = FALSE) |>
        clean_data() |>
        mutate(state = to_fips(state, global$extdata$fips$county, "state"))
      
      waiter::waiter_hide()
    })

    # Use aggregated example data
    observeEvent(input$use_agg_example, {
      waiter::waiter_show(
        html = waiter_ui("wait"),
        color = waiter::transparent(0.9)
      )

      file_name <- switch(global$data_format,
        "temporal_covid" = "timevarying_covid_aggregated.csv",
        "temporal_other" = "timevarying_other_aggregated.csv",
        "static_poll"    = "crosssectional_poll_aggregated.csv",
        "static_other"   = "crosssectional_other_aggregated.csv"
      )

      readr::read_csv(app_sys(paste0("extdata/example/data/", file_name)), show_col_types = FALSE) |> raw_sample()
      global$data <- raw_sample() |>
        clean_data() |>
        mutate(state = to_fips(state, global$extdata$fips$county, "state"))

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
        # find the smallest geography in the data
        idx <- match(names(global$data), GLOBAL$vars$geo) |> na.omit()
        link_geos <- if (length(idx) > 0) {
          c(GLOBAL$vars$geo[min(idx):length(GLOBAL$vars$geo)], "Do not include geography")
        } else {
          "Do not include geography"
        }

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
    # Create poststratification data from ACS data
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
            global$mrp <- prepare_mrp_covid(
              input_data = global$data,
              pstrat_data = global$extdata$pstrat_covid,
              covariates = global$extdata$covar_covid,
              demo_levels = create_expected_levels(global$data_format),
              vars_global = GLOBAL$vars
            )

            # prepare data for plotting
            global$plot_data <- list(
              dates = if("date" %in% names(global$data)) get_dates(global$data) else NULL,
              geojson = list(county = filter_geojson(global$extdata$geojson$county, global$mrp$levels$county)),
              raw_covariates = global$extdata$covar_covid |> filter(zip %in% unique(global$mrp$input$zip))
            )

          } else if (global$data_format == "static_poll") {
            new_data <- global$extdata$pstrat_poll |>
              mutate(state = to_fips(state, global$extdata$fips$county, "state"))

            global$mrp <- prepare_mrp_custom(
              input_data = global$data,
              new_data = new_data,
              fips_county_state = global$extdata$fips$county,
              demo_levels = create_expected_levels(global$data_format),
              vars_global = GLOBAL$vars,
              link_geo = "state",
              need_time = global$data_format %in% c("temporal_covid", "temporal_other")
            )

            # prepare data for plotting
            global$plot_data <- list(
              geojson = list(state = filter_geojson(global$extdata$geojson$state, global$mrp$levels$state))
            )

          } else {
            # retrieve ACS data based on user's selection
            tract_data <- readr::read_csv(app_sys(stringr::str_interp("extdata/acs/acs_${global$link_data$acs_year}.csv")), show_col_types = FALSE)

            # prepare data for MRP
            global$mrp <- prepare_mrp_acs(
              input_data = global$data,
              tract_data = tract_data,
              zip_tract = global$extdata$zip_tract,
              demo_levels = create_expected_levels(global$data_format),
              vars_global = GLOBAL$vars,
              link_geo = global$link_data$link_geo,
              need_time = global$data_format %in% c("temporal_covid", "temporal_other")
            )


            # prepare data for plotting
            plot_data <- list()
            plot_data$dates <- if("date" %in% names(global$data)) get_dates(global$data) else NULL
            plot_data$geojson <- names(global$extdata$geojson) |>
              setNames(nm = _) |>
              purrr::map(~filter_geojson(
                geojson = global$extdata$geojson[[.x]], 
                geoids = global$mrp$levels[[.x]]
              ))

            global$plot_data <- nullify(plot_data)
          }
          
          # set success to TRUE if no errors occurred
          success <- TRUE

        }, error = function(e) {
          message(paste("Error linking data:\n", e$message))
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

    #----------------------------------------------------------------------------
    # Show feedback about poststratification data
    #----------------------------------------------------------------------------
    output$pstrat_feedback <- renderUI({
      req(raw_pstrat())

      if (!is.null(pstrat_errors())) {
        if (length(pstrat_errors()) > 0) {
          tags$div(
            tagList(icon("circle-xmark", "fa"), "Error"),
            tags$p("Poststratification data does not meet all requirements. Please check the user guide for data requirements.", class = "small"),
          )
        } else {
          tags$div(
            tagList(icon("circle-check", "fa"), "Success"),
            tags$p("All requirements are met. You may proceed to the next page.", class = "small")
          )
        }
      }
    })

    #----------------------------------------------------------------------------
    # Handle poststratification data upload
    #----------------------------------------------------------------------------
    observeEvent(input$pstrat_upload, {
      waiter::waiter_show(
        html = waiter_ui("wait"),
        color = waiter::transparent(0.9)
      )

      tryCatch({
        # Read in data first
        read_data(input$pstrat_upload$datapath) |> raw_pstrat()

        # Process data
        new_data <- preprocess(
          data = raw_pstrat(),
          data_format = global$data_format,
          zip_county_state = global$extdata$zip_county_state,
          const = GLOBAL,
          is_sample = FALSE,
          is_aggregated = input$toggle_pstrat == "agg"
        )

        # Compare to sample data
        check_pstrat(new_data, global$data, create_expected_levels(global$data_format))

        # Find the smallest common geography
        link_geo <- NULL
        common <- intersect(names(global$data), names(new_data))
        smallest <- get_smallest_geo(common, GLOBAL$vars$geo)
        if (!is.null(smallest)) {
          link_geo <- smallest$geo
        }

        # Store linking geography
        global$link_data <- list(
          link_geo = link_geo,
          acs_year = NULL
        )

        # Prepare data for MRP
        global$mrp <- prepare_mrp_custom(
          input_data = global$data,
          new_data = new_data,
          fips_county_state = global$extdata$fips$county,
          demo_levels = create_expected_levels(global$data_format),
          vars_global = GLOBAL$vars,
          link_geo = link_geo,
          need_time = global$data_format %in% c("temporal_covid", "temporal_other")
        )


        # prepare data for plotting
        plot_data <- list()
        plot_data$dates <- if("date" %in% names(global$data)) get_dates(global$data) else NULL
        plot_data$geojson <- names(global$extdata$geojson) |>
          setNames(nm = _) |>
          purrr::map(~filter_geojson(
            geojson = global$extdata$geojson[[.x]], 
            geoids = global$mrp$levels[[.x]]
          ))

        global$plot_data <- nullify(plot_data)

        # Trigger success feedback
        pstrat_errors(character(0))

      }, error = function(e) {
        # show error message
        error_message <- paste("Error processing data:\n", e$message)
        pstrat_errors(error_message)
        message(error_message)
        
        # reset reactives
        global$link_data <- NULL
        global$mrp <- NULL
        global$plot_data <- NULL
        
      }, finally = {
        # Always hide the waiter
        waiter::waiter_hide()
      })
    })




    #----------------------------------------------------------------------------
    # Reset link button
    #----------------------------------------------------------------------------
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

    # Example poststratification data download handler
    output$save_pstrat_example <- downloadHandler(
      filename = function() {
        # Name based on data format
        prefix <- switch(global$data_format,
          "temporal_other" = "timevarying_other",
          "static_other" = "crosssectional_other",
          "temporal_covid" = "timevarying_covid",
          "static_poll" = "crosssectional_poll"
        )
        paste0(prefix, "_pstrat_example_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        # Determine which example file to use based on data format
        example_file <- switch(global$data_format,
          "temporal_other" = "timevarying_other_pstrat.csv",
          "static_other" = "crosssectional_other_pstrat.csv",
          "temporal_covid" = "timevarying_covid_pstrat.csv",
          "static_poll" = "crosssectional_poll_pstrat.csv"
        )
        
        # Read the example file and write it to the download location
        readr::read_csv(
          app_sys(paste0("extdata/example/data/", example_file)), 
          show_col_types = FALSE
        ) |> 
          readr::write_csv(file)
      }
    )
  })
}
