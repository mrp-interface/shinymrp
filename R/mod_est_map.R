#' est_map UI Function
#'
#' @description A shiny Module for displaying geographical MRP estimates.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList conditionalPanel selectInput selectizeInput plotOutput
#' @importFrom plotly plotlyOutput
mod_est_map_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui"))
}
    
#' est_map Server Functions
#'
#' @noRd 
mod_est_map_server <- function(id, model, global, geo_scale, geo_view, geo_subset){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$ui <- renderUI({
      req(model(), geo_scale(), geo_view())

      geo <- geo_scale()
      time_indices <- model()$est[[geo]][["time"]]
      dates <- model()$plot_data$dates

      switch(geo_view(),
        "map" = tagList(
          highcharter::highchartOutput(
            outputId = ns("est_geo_map"),
            height = GLOBAL$ui$map_height
          ),
          # Only show slider if we have temporal data
          if (!is.null(time_indices)) {
            if(!is.null(dates)) {
              div(
                class = "mx-4",
                sliderInput(
                inputId = ns("map_slider"),
                  label = NULL,
                  min = as.Date(dates[1], format = GLOBAL$ui$date_format),
                  max = as.Date(dates[length(dates)], format = GLOBAL$ui$date_format),
                  step = 7,
                  value = as.Date(dates[1], format = GLOBAL$ui$date_format),
                  width = "100%",
                  animate = GLOBAL$ui$animation
                )
              )
            } else {
              div(
                class = "mx-4",
                sliderInput(
                  inputId = ns("map_slider"),
                  label = NULL,
                  min = 1,
                  max = max(time_indices),
                  step = 1,
                  value = 1,
                  width = "100%",
                  animate = GLOBAL$ui$animation
                )
              )
            }
          }
        ),
        "line_scatter" = plotOutput(ns("est_geo_plot")),
        NULL
      )
    })
    
    output$est_geo_map <- highcharter::renderHighchart({
      req(model(), geo_scale())

      geo <- geo_scale()
      
      time_index <- if(!is.null(model()$plot_data$dates)) {
        which(as.character(format(input$map_slider, GLOBAL$ui$date_format)) == model()$plot_data$dates)
      } else {
        input$map_slider
      }

      plot_df <- model()$est[[geo]] |> 
        prep_est(
          fips_codes = global$extdata$fips[[geo]],
          geo = geo,
          time_index = time_index
        )


      plot_df |>
        choro_map(
          model()$plot_data$geojson[[geo]],
          main_title = "MRP Estimate of Positive Response Rate",
          sub_title = "MRP Estimate",
          geo = geo,
          config = list(
            minValue = 0,
            maxValue = max(model()$est[[geo]]$est)
          )
        )
    })
    
    # --------------------------------------------------------------------------
    # Plot for geographic subgroup estimates (subset plots)
    # --------------------------------------------------------------------------
    output$est_geo_plot <- renderPlot({
      req(model(), geo_scale())
      
      geo <- isolate(geo_scale())
      fips_df <- global$extdata$fips[[geo]] |> fips_upper()

      plot_df <- model()$est[[geo]] |>
        rename("fips" = "factor") |>
        left_join(fips_df, by = "fips") |>
        rename("factor" = geo) |>
        filter(factor %in% geo_subset())

      if(model()$data_format %in% c("temporal_covid", "temporal_other")) {
        plot_est_temporal(plot_df, model()$plot_data$dates)
      } else {
        plot_est_static(plot_df)
      }
    }, height = function() {
      if(model()$data_format %in% c("temporal_covid", "temporal_other")) {
        GLOBAL$ui$subplot_height * (length(geo_subset()) + 1)
      } else {
        GLOBAL$ui$plot_height
      }
    })
    
  })
}
    
## To be copied in the UI
# mod_est_map_ui("est_map_1")
    
## To be copied in the server
# mod_est_map_server("est_map_1", model, global)
