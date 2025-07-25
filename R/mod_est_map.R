#' est_map UI Function
#'
#' @description A shiny Module for displaying geographical MRP estimates.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList conditionalPanel selectInput selectizeInput plotOutput
mod_est_map_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui"))
}
    
#' est_map Server Functions
#'
#' @noRd 
#' @importFrom dplyr rename left_join filter
#' @importFrom rlang .data
mod_est_map_server <- function(id, model, global, geo_scale, geo_view, geo_subset){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$ui <- renderUI({
      req(model(), geo_scale(), geo_view())

      geo <- geo_scale()
      time_indices <- model()$est[[geo]][["time"]]
      dates <- model()$plotdata$dates

      switch(geo_view(),
        "map" = tagList(
          highcharter::highchartOutput(
            outputId = ns("map"),
            height = GLOBAL$plot$ui$map_height
          ),
          # Only show slider if we have time-varying data
          if (!is.null(time_indices)) {
            if(!is.null(dates)) {
              div(
                class = "mx-4",
                sliderInput(
                inputId = ns("map_slider"),
                  label = NULL,
                  min = as.Date(dates[1], format = GLOBAL$ui$format$date),
                  max = as.Date(dates[length(dates)], format = GLOBAL$ui$format$date),
                  step = 7,
                  value = as.Date(dates[1], format = GLOBAL$ui$format$date),
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
        "line_scatter" = plotOutput(ns("plot")),
        NULL
      )
    })
    
    output$map <- highcharter::renderHighchart({
      req(model(), geo_scale())

      geo <- geo_scale()
      time_indices <- model()$est[[geo]][["time"]]
      dates <- model()$plotdata$dates
      
      time_index <- if (!is.null(time_indices) && !is.null(dates)) {
        which(as.character(format(input$map_slider, GLOBAL$ui$format$date)) == model()$plotdata$dates)
      } else if (!is.null(time_indices)) {
        input$map_slider
      } else {
        NULL
      }

      model()$est[[geo]] %>% 
        prep_est(
          fips_codes = fips_[[geo]],
          geo = geo,
          time_index = time_index
        ) %>%
        choro_map(
          model()$plotdata$geojson[[geo]],
          geo = geo,
          config = list(
            minValue = 0,
            maxValue = max(model()$est[[geo]]$est),
            main_title = "MRP Estimate",
            hover_title = "Estimate"
          )
        )
    })
    
    # --------------------------------------------------------------------------
    # Plot for geographic subgroup estimates (subset plots)
    # --------------------------------------------------------------------------
    output$plot <- renderPlot({
      req(model(), geo_scale())
      
      geo <- isolate(geo_scale())
      fips_df <- fips_[[geo]] %>% fips_upper()

      plot_df <- model()$est[[geo]] %>%
        rename("fips" = "factor") %>%
        left_join(fips_df, by = "fips") %>%
        rename("factor" = geo) %>%
        filter(factor %in% geo_subset())

      if(model()$metadata$is_timevar) {
        plot_est_timevar(
          plot_df = plot_df,
          dates = model()$plotdata$dates,
          metadata = model()$metadata
        )
      } else {
        plot_est_static(
          plot_df = plot_df,
          metadata = model()$metadata
        )
      }
    }, height = function() {
      req(model())

      if(model()$metadata$is_timevar) {
        GLOBAL$plot$ui$subplot_height * (length(geo_subset()) + 1)
      } else {
        GLOBAL$plot$ui$plot_height
      }
    })
    
  })
}
    
## To be copied in the UI
# mod_est_map_ui("est_map_1")
    
## To be copied in the server
# mod_est_map_server("est_map_1", model, global)
