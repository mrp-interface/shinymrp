#' analyze_result UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
mod_analyze_result_ui <- function(id){
  ns <- NS(id)
  tags$div(class = "pad_top",
    navlistPanel(widths = c(3, 9),
      id = ns("navbar"),
      tabPanel(
        selectInput(
          inputId = ns("model_select"),
          label = "Select a model",
          choices = NULL
        )
      ),
      tabPanel("Raw vs MRP",
        value = "nav_overall",
        plotOutput(outputId = ns("est_overall"))
      ),
      tabPanel("By subgroup",
        value = "nav_subgroup",
        tabsetPanel(
          id = ns("navbar_subgroup"),
          tabPanel("Sex",
            value = "nav_subgroup_sex",
            mod_est_plot_ui(ns("est_sex"))
          ),
          tabPanel("Race",
            value = "nav_subgroup_race",
            mod_est_plot_ui(ns("est_race"))
          ),
          tabPanel("Age",
            value = "nav_subgroup_age",
            mod_est_plot_ui(ns("est_age"))
          ),
          tabPanel("Education",
            value = "nav_subgroup_edu",
            mod_est_plot_ui(ns("est_edu"))
          ),
          tabPanel("Geography",
            value = "nav_subgroup_geo",
            conditionalPanel(ns = ns,
              condition = "output.no_geo",
              tags$p("Map unavailable", class = "alt_text")
            ),
            conditionalPanel(ns = ns,
              condition = "!output.no_geo",
              tags$div(class = "pad_top",
                conditionalPanel(ns = ns,
                  condition = "output.data_format == 'temporal_other' ||
                               output.data_format == 'static_other'",
                  selectInput(
                    inputId = ns("geo_scale_select"),
                    label = "Select geographic scale",
                    choices = NULL
                  )
                ),
                plotly::plotlyOutput(ns("est_geo_map"), height = "700px")
              ),
              tags$div(class = "pad_top",
                conditionalPanel(ns = ns,
                  condition = "output.data_format == 'temporal_covid' ||
                               output.data_format == 'temporal_other'",
                  selectizeInput(
                    inputId = ns("geo_select"),
                    label = "Select one or more counties (max = 5)",
                    choices = NULL,
                    multiple = TRUE,
                    options = list(maxItems = 5)
                  )
                ),
                plotOutput(ns("est_geo_plot"))
              )
            )
          )
        )
      )
    )
  )
}

#' analyze_result Server Functions
#'
#' @noRd
mod_analyze_result_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    selected_model <- reactive(global$poststratified_models[[input$model_select]])
    model_select_buffer <- reactive(input$model_select)
    selected_scale <- reactive({
      req(selected_model())

      if(selected_model()$data_format == "temporal_covid") {
        "county"
      } else if (selected_model()$data_format == "static_poll") {
        "state"
      } else {
        input$geo_scale_select
      }
   })

    output$no_geo <- reactive(is.null(selected_model()$link_data$link_geo))
    outputOptions(output, "no_geo", suspendWhenHidden = FALSE)
    output$data_format <- reactive(selected_model()$data_format)
    outputOptions(output, "data_format", suspendWhenHidden = FALSE)


    # Initialize demographic plot modules
    mod_est_plot_server("est_sex", selected_model, "sex")

    mod_est_plot_server("est_race", selected_model, "race")

    mod_est_plot_server("est_age", selected_model, "age")

    mod_est_plot_server("est_edu", selected_model, "edu")

    output$est_overall <- renderPlot({
      req(selected_model())

      if(global$data_format %in% c("temporal_covid", "temporal_other")) {
        plot_prev(
          selected_model()$mrp$input,
          selected_model()$plotdata$dates,
          selected_model()$est$overall,
          show_caption = TRUE
        )
      } else {
        selected_model()$est$overall |>
          mutate(
            data = "Estimate",
            lower = est - std,
            median = est,
            upper = est + std
          ) |>
          select(data, lower, median, upper) |>
          plot_support(selected_model()$mrp$input)
      }
    }, height = function() GLOBAL$ui$plot_height)

    output$est_geo_map <- plotly::renderPlotly({
      req(selected_model())

      geo <- selected_scale()

      selected_model()$est[[geo]] |>
        prep_est(
          fips_codes = global$extdata$fips[[geo]],
          geo = geo,
          dates = if("dates" %in% names(selected_model()$plotdata)) selected_model()$plotdata$dates else NULL
        ) |>
        mutate(value = est) |>
        choro_map(
          selected_model()$plotdata$geojson[[geo]],
          map_title = "MRP Estimate of Positive Response Rate",
          colorbar_title = "Positive\nResponse\nRate",
          geo = geo
        ) |>
        suppressWarnings()
    })

    output$est_geo_plot <- renderPlot({
      req(selected_model())
      
      geo <- selected_scale()
      fips_df <- global$extdata$fips[[geo]] |> fips_upper()

      plof_df <- selected_model()$est[[geo]] |>
        rename("fips" = "factor") |>
        left_join(fips_df, by = "fips") |>
        rename("factor" = geo)
 
      if(global$data_format %in% c("temporal_covid", "temporal_other")) {
        plof_df |>
          filter(factor %in% input$geo_select) |>
          plot_est_temporal(selected_model()$plotdata$dates)
      } else {
        plof_df |> plot_est_static()
      }
      
    }, height = function() {
      if(global$data_format %in% c("temporal_covid", "temporal_other")) {
        GLOBAL$ui$subplot_height * (length(input$geo_select) + 1)
      } else {
        GLOBAL$ui$plot_height
      }
    })

    observeEvent(global$input$navbar_analyze, {
      # When user navigates to "Results" page
      if(global$input$navbar_analyze == "nav_analyze_result") {

        if(is.null(global$mrp)) {
          showModal(
            modalDialog(
              title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
              "Invalid input data.",
              footer = actionButton(
                inputId = ns("to_upload"),
                label = "Go to data upload page"
              )
            ),
            session = global$session
          )
        }

        # omit pre-poststratification models
        global$poststratified_models <- purrr::keep(global$models, ~ !is.null(.x$fit$pstrat))

        if(length(global$poststratified_models) == 0) {
          showModal(
            modalDialog(
              title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
              "No model with poststratified estimates found. Make sure to run poststratification after fitting models.",
              footer = actionButton(
                inputId = ns("to_model"),
                label = "Go to model page"
              )
            ),
            session = global$session
          )
        } else {
          choices <- names(global$poststratified_models)
          selected <- if(model_select_buffer() %in% choices) model_select_buffer() else choices[1]

          # update model select
          updateSelectInput(session,
            inputId = "model_select",
            choices = choices,
            selected = selected
          )
        }
      }
    })

    observeEvent(input$to_upload, {
      updateTabsetPanel(global$session,
        inputId = "navbar_analyze",
        selected = "nav_analyze_upload"
      )

      removeModal(global$session)
    })

    observeEvent(input$to_model, {
      updateTabsetPanel(global$session,
        inputId = "navbar_analyze",
        selected = "nav_analyze_model"
      )

      removeModal(global$session)
    })

    observeEvent(selected_model(), {
      if(input$model_select != "") {
        # update geographic scale select
        choices <- intersect(names(selected_model()$est), GLOBAL$vars$geo)
        updateSelectInput(session,
          inputId = "geo_scale_select",
          choices = choices,
          selected = choices[1]
        )        

        # show/hide tabs based on data format
        for(tab in c("nav_subgroup_sex", "nav_subgroup_race", "nav_subgroup_age", "nav_subgroup_edu", "nav_subgroup_geo")) {
          showTab("navbar_subgroup", tab)
        }

        if (global$data_format != "static_poll") {
          hideTab("navbar_subgroup", "nav_subgroup_edu")
        }
      }
    })

    observeEvent(selected_scale(), {
      geo <- selected_scale()

      # update geographic region select when geographic scale changes
      if(geo != "") {

        fips_df <- global$extdata$fips[[geo]] |>
          filter(fips %in% global$mrp$levels[[geo]])|>
          fips_upper()
        choices <- sort(fips_df[[geo]])
        
        updateSelectInput(session,
          inputId = "geo_select",
          choices = choices,
          selected = choices[1]
        )
      }
    })

  })
}
