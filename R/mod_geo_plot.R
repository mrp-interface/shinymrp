#' geo_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_geo_plot_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(9, 3),
    plotOutput(outputId = ns("plot"), height = GLOBAL$ui$plot_height),
    DT::dataTableOutput(outputId = ns("table"))
  )
}
    
#' geo_plot Server Functions
#'
#' @noRd 
mod_geo_plot_server <- function(id, data, varname, config){
  moduleServer(id, function(input, output, session){
    # Render the plot
    output$plot <- renderPlot({
      req(data())
      
      # Calculate statistics for description
      count <- switch(config$operation,
        ">" = sum(data()[[varname]] > config$threshold, na.rm = TRUE),
        ">=" = sum(data()[[varname]] >= config$threshold, na.rm = TRUE),
        "<" = sum(data()[[varname]] < config$threshold, na.rm = TRUE),
        "<=" = sum(data()[[varname]] <= config$threshold, na.rm = TRUE)
      )
      total <- nrow(data())
      perc <-  count / total * 100
      threshold <- if(config$threshold > 1) config$threshold else config$threshold * 100

      # Create plot
      data() |>
        mutate(covar = !!sym(varname)) |>
        plot_geographic(
          breaks = config$breaks,
          description = sprintf(config$description, count, total, perc, threshold),
          definition = config$definition,
          name = config$name
        )
    })
    
    # Render the table
    output$table <- DT::renderDT({
      req(data())

      decimal_places <- if(median(data()[[varname]]) > 100) 0 else 4

      data() |>
        mutate(measure = round(!!sym(varname), decimal_places)) |>
        select(zip, measure) |>
        DT::datatable(
          options = list(
            lengthChange = FALSE,
            searching = FALSE,
            info = FALSE,
            ordering = FALSE,
            pagingType = "simple"
          )
        )
    })
  })
}
    
## To be copied in the UI
# mod_geo_plot_ui("geo_plot_1")
    
## To be copied in the server
# mod_geo_plot_server("geo_plot_1")
