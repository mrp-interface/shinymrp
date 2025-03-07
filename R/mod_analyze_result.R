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
      tabPanel(
        selectInput(
          inputId = ns("model_select"),
          label = "Select a model",
          choices = NULL
        )
      ),
      tabPanel("Raw vs MRP",
        plotOutput(outputId = ns("est_overall"))
      ),
      tabPanel("By subgroup",
        tabsetPanel(
          tabPanel("Sex",
            mod_est_plot_ui(ns("est_sex"))
          ),
          tabPanel("Race",
            mod_est_plot_ui(ns("est_race"))
          ),
          tabPanel("Age",
            mod_est_plot_ui(ns("est_age"))
          ),
          tabPanel("Geography",
            conditionalPanel(
              condition = "output.no_geo",
              tags$p("Map unavailable", class = "alt_text")
            ),
            conditionalPanel(
              condition = "!output.no_geo",
              tags$div(class = "pad_top",
                conditionalPanel(
                  condition = "output.data_format == 'temporal_other' ||
                               output.data_format == 'static_other'",
                  selectInput(
                    inputId = ns("geo_scale_select"),
                    label = "Select geographic scale",
                    choices = NULL
                  )
                ),
                plotly::plotlyOutput(outputId = ns("est_geo_map"),
                                    height = "700px")
              ),
              tags$div(class = "pad_top",
                conditionalPanel(
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
                plotOutput(outputId = ns("est_geo_plot"))
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
    selected_geo <- reactive(
      if(global$data_format == "temporal_covid") {
        "county"
      } else if (global$data_format == "static_poll") {
        "state"
      } else {
        input$geo_scale_select
      }
    )
    model_select_buffer <- reactiveVal()
    
    # Initialize demographic plot modules
    mod_est_plot_server("est_sex",
      data = reactive(selected_model()$est$sex),
      plotdata = list(
        dates = global$plotdata$dates,
        n_plots = if(global$data_format %in% c("temporal_covid", "temporal_other")) length(global$mrp$levels$sex) + 1 else 1
      )
    )

    mod_est_plot_server("est_race",
      data = reactive(selected_model()$est$race),
      plotdata = list(
        dates = global$plotdata$dates,
        n_plots = if(global$data_format %in% c("temporal_covid", "temporal_other")) length(global$mrp$levels$race) + 1 else 1
      )
    )

    mod_est_plot_server("est_age",
      data = reactive(selected_model()$est$age),
      plotdata = list(
        dates = global$plotdata$dates,
        n_plots = if(global$data_format %in% c("temporal_covid", "temporal_other")) length(global$mrp$levels$age) + 1 else 1
      )
    )

    output$est_overall <- renderPlot({
      req(selected_model())
      
      if(global$data_format %in% c("temporal_covid", "temporal_other")) {
        plot_prev(
          global$mrp$input,
          global$plotdata$dates,
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
          plot_support(global$mrp$input)
      }
    }, height = function() GLOBAL$ui$plot_height)

    output$est_geo_map <- plotly::renderPlotly({
      req(selected_model())

      selected_model()$est[[selected_geo()]] |>
        prep_est(
          fips_codes = global$extdata$fips[[selected_geo()]],
          geo = selected_geo(),
          dates = if("dates" %in% names(global$plotdata)) global$plotdata$dates else NULL
        ) |>
        mutate(value = est) |>
        choro_map(
          global$plotdata$geojson[[selected_geo()]],
          map_title = "MRP Estimate of Positive Response Rate",
          colorbar_title = "Positive\nResponse\nRate",
          geo = selected_geo()
        ) |>
        suppressWarnings()
    })

    output$est_geo_plot <- renderPlot({
      req(selected_model())

      plof_df <- selected_model()$est[[selected_geo()]] |>
        rename("fips" = "factor") |>
        left_join(global$extdata$fips[[selected_geo()]], by = "fips") |>
        rename("factor" = selected_geo())
 
      if(global$data_format %in% c("temporal_covid", "temporal_other")) {
        plof_df |>
          filter(factor %in% input$geo_select) |>
          plot_est_temporal(global$plotdata$dates)
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
        }

        # update model select
        updateSelectInput(session,
          inputId = "model_select",
          choices = names(global$poststratified_models),
          selected = if(model_select_buffer() %in% names(global$poststratified_models)) model_select_buffer() else NULL
        )
        
        # update geographic scale select
        choices <- intersect(names(global$mrp$levels), c("county", "state"))
        updateSelectInput(session,
          inputId = "geo_scale_select",
          choices = choices,
          selected = choices[1]
        )

        # update geographic region select
        fips_df <- global$extdata$fips[[choices[1]]] |>
          filter(fips %in% global$mrp$levels[[choices[1]]])
        choices <- sort(fips_df[[selected_geo()]])
        
        updateSelectInput(session,
          inputId = "geo_select",
          choices = choices,
          selected = choices[1]
        )
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
    
    observeEvent(input$model_select, {
      model_select_buffer(input$model_select)
    })

    observeEvent(input$geo_scale_select, {
      # update geographic region select when geographic scale changes
      if(input$geo_scale_select != "") {
        fips_df <- global$extdata$fips[[input$geo_scale_select]] |>
          filter(fips %in% global$mrp$levels[[input$geo_scale_select]])
        choices <- sort(fips_df[[input$geo_scale_select]])
        
        updateSelectInput(session,
          inputId = "geo_select",
          choices = choices,
          selected = choices[1]
        )
      }
    })

  })
}
