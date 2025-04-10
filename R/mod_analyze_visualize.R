#' analyze_visualize UI Function
#'
#' @description A Shiny Module that uses selectInputs in a sidebar to pick which plot to display.
#'
#' @param id A unique id for the module.
#'
#' @noRd
#' @importFrom shiny NS tagList sidebarLayout sidebarPanel mainPanel uiOutput selectInput conditionalPanel
mod_analyze_visualize_ui <- function(id){
  ns <- NS(id)
  
  # Sidebar layout with dynamic select inputs:
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 350,
      # First selectInput: choose the plot category.
      selectInput(
        inputId = ns("plot_category"),
        label = "Select plot category",
        choices = NULL
      ),
      selectInput(
        inputId = ns("plot_subcategory"),
        label = "",
        choices = NULL
      )
    ),
    uiOutput(ns("plot_output"))
  )
}

#' analyze_visualize Server Functions
#'
#' @noRd
mod_analyze_visualize_server <- function(id, global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Update the subcategory selectInput based on the main category selection.
    observeEvent(input$plot_category, {
      # Define the subcategory choices for each category.
      indiv_choices <- c("Sex", "Race", "Age", "Geography")
      indiv_choices_poll <- c("Sex", "Race", "Age", "Education", "Geography")
      geo_choices   <- c("Education", "Poverty", "Employment", "Income", "Urbanicity", "ADI")

      choices <- if (input$plot_category == "Individual Characteristics") {
        updateSelectInput(session, "plot_subcategory", label = "Select characteristic")
        switch(global$data_format,
          "static_poll" = indiv_choices_poll,
          indiv_choices
        )
      } else if (input$plot_category == "Geographic Characteristics") {
        updateSelectInput(session, "plot_subcategory", label = "Select characteristic")
        geo_choices
      } else if (input$plot_category == "Positive Response Rate") {
        updateSelectInput(session, "plot_subcategory", label = "Select plot type")
        switch(global$data_format,
          "temporal_covid" = c("Overall", "By Geography"),
          "temporal_other" = c("Overall", "By Geography"),
          "static_poll" = "By Geography",
          "static_other" = "By Geography"
        )
      } else {
        character(0)  # No choices if the category is not recognized.
      }

      # Update the subcategory selectInput with the new choices.
      updateSelectInput(session, "plot_subcategory", choices = choices, selected = choices[1])
    })

    # Render the UI for the selected plot.
    output$plot_output <- renderUI({
      req(input$plot_category, input$plot_subcategory)

      category <- isolate(input$plot_category)
      subcategory <- isolate(input$plot_subcategory)

      if (category == "Individual Characteristics") {
        switch(subcategory,
          "Sex" = mod_indiv_plot_ui(ns("indiv_sex")),
          "Race" = mod_indiv_plot_ui(ns("indiv_race")),
          "Age" = mod_indiv_plot_ui(ns("indiv_age")),
          "Education" = mod_indiv_plot_ui(ns("indiv_edu")),
          "Geography" = mod_indiv_map_ui(ns("indiv_geo"))
        )
      } else if (category == "Geographic Characteristics") {
        switch(subcategory,
          "Education" = mod_geo_plot_ui(ns("geo_edu")),
          "Poverty" = mod_geo_plot_ui(ns("geo_poverty")),
          "Employment" = mod_geo_plot_ui(ns("geo_employ")),
          "Income" = mod_geo_plot_ui(ns("geo_income")),
          "Urbanicity" = mod_geo_plot_ui(ns("geo_urban")),
          "ADI" = mod_geo_plot_ui(ns("geo_adi"))
        )
      } else if (category == "Positive Response Rate") {
        switch(subcategory,
          "Overall" = plotOutput(ns("positive_plot"), height = GLOBAL$ui$plot_height),
          "By Geography" = plotly::plotlyOutput(ns("positive_map"), height = GLOBAL$ui$map_height)
        )
      }
    })

    # --------------------------------------------------------------------------
    # Module Server Calls for Individual-level Plots
    # --------------------------------------------------------------------------
    mod_indiv_plot_server("indiv_sex", reactive(global$mrp), "sex")
    mod_indiv_plot_server("indiv_race", reactive(global$mrp), "race")
    mod_indiv_plot_server("indiv_age", reactive(global$mrp), "age")
    mod_indiv_plot_server("indiv_edu", reactive(global$mrp), "edu")

  
    # --------------------------------------------------------------------------
    # Sample Size Map and Table
    # --------------------------------------------------------------------------
    mod_indiv_map_server(
      "indiv_geo",
      reactive(global$mrp$input),
      reactive(global$link_data$link_geo),
      reactive(global$plotdata$geojson),
      global$extdata$fips
    )

    # --------------------------------------------------------------------------
    # Module Server Calls for Geographic-level Plots
    # --------------------------------------------------------------------------
    mod_geo_plot_server("geo_edu", reactive(global$plotdata$raw_covariates), "college", list(
      threshold   = 0.5,
      operation   = ">=",
      breaks      = seq(0, 1, 0.05),
      description = "\n%d ZIP codes out of %d (%.2f%%) have %d%% or more people who have earned an Associate's degree or higher.",
      definition  = "Higher education measure of a zip code is defined as the percentage of the residing population\nwho have earned an Associate's degree or higher.",
      name        = "Higher education measure"
    ))
    
    mod_geo_plot_server("geo_poverty", reactive(global$plotdata$raw_covariates), "poverty", list(
      threshold   = 0.2,
      operation   = "<=",
      breaks      = seq(0, 1, 0.05),
      description = "%d zip codes out of %d (%.2f%%) have %d%% or less people whose ratio of income to poverty level in the past 12 months\nis below 100%%.",
      definition  = "Poverty measure of a zip code is defined as the percentage of the residing population\nwhose ratio of income to poverty level in the past 12 months is below 100%%.",
      name        = "Poverty measure"
    ))
    
    mod_geo_plot_server("geo_employ", reactive(global$plotdata$raw_covariates), "employment", list(
      threshold   = 0.5,
      operation   = ">=",
      breaks      = seq(0, 1, 0.05),
      description = "\n%d zip codes out of %d (%.2f%%) have %d%% or more people who is employed as a part of the civilian labor force.",
      definition  = "Employment rate of a zip code is defined as the percentage of the residing population\nwho are employed as a part of the civilian labor force.",
      name        = "Employment rate"
    ))
    
    mod_geo_plot_server("geo_income", reactive(global$plotdata$raw_covariates), "income", list(
      threshold   = 70784,
      operation   = ">",
      breaks      = seq(0, 150000, 5000),
      description = "%d zip codes out of %d (%.2f%%) have average value of tract-level median household income in the past 12 months\ngreater than %d dollars (2021 US median income according to the ACS).",
      definition  = "Income measure of a zip code is defined as the average value of tract-level median household income in the past 12 months\nweighted by tract population counts.",
      name        = "Average of median household income"
    ))
    
    mod_geo_plot_server("geo_urban", reactive(global$plotdata$raw_covariates), "urbanicity", list(
      threshold   = 0.95,
      operation   = ">=",
      breaks      = seq(0, 1, 0.05),
      description = "\n%d zip codes out of %d (%.2f%%) have %d%% or more tracts classified as urban.",
      definition  = "Urbanicity of a zip code is defined as the percentage of covered census tracts classified as urban\nweighted by tract population counts.",
      name        = "Urbanicity"
    ))
    
    mod_geo_plot_server("geo_adi", reactive(global$plotdata$raw_covariates), "ADI", list(
      threshold   = 80,
      operation   = ">",
      breaks      = seq(0, 100, 5),
      description = "\n%d zip codes out of %d (%.2f%%) have ADI over %d.",
      definition  = "Area Deprivation Index (ADI) of a zip code is the average ADI across covered census tracts\nweighted by tract population counts (refer to Learn > Preprocess page for the definition of ADI).",
      name        = "Area Deprivation Index"
    ))


    # --------------------------------------------------------------------------
    # Plot for Positive Response Rate
    # --------------------------------------------------------------------------
    output$positive_plot <- renderPlot({
      req(input$plot_subcategory == "Overall")
      plot_prev(global$mrp$input, global$plotdata$dates)
    })

    # --------------------------------------------------------------------------
    # Map for Positive Response Rate
    # --------------------------------------------------------------------------
    output$positive_map <- plotly::renderPlotly({
      req(global$link_data$link_geo)

      geo <- if(global$link_data$link_geo == "zip") "county" else global$link_data$link_geo

      if("time" %in% names(global$mrp$input)) {
        global$mrp$input |>
          prep_raw_prev(
            fips_codes = global$extdata$fips[[geo]],
            geo = geo
          ) |>
          mutate(value = max_prev) |>
          choro_map(
            global$plotdata$geojson[[geo]],
            map_title = "Positive Response Rate Across Weeks",
            colorbar_title = "Highest\nWeekly\nPositive\nResponse\nRate",
            geo = geo
          )
      } else {
        global$mrp$input |>
          prep_raw_support(
            fips_codes = global$extdata$fips[[geo]],
            geo = geo
          ) |>
          mutate(value = support) |>
          choro_map(
            global$plotdata$geojson[[geo]],
            map_title = "Positive Response Rate",
            colorbar_title = "Positive\nResponse\nRate",
            geo = geo
          )
      }
    })
    
    # --------------------------------------------------------------------------
    # Navigation event handlers and modals
    # --------------------------------------------------------------------------
    observeEvent(global$input$navbar_analyze, {
      if (global$input$navbar_analyze == "nav_analyze_visualize") {
        if (is.null(global$mrp)) {
          showModal(modalDialog(
            title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
            "Invalid input data.",
            footer = actionButton(ns("to_upload"), "Go to data upload page")
          ), session = global$session)
        } else {
          # Update the plot category choices based on the data format
          choices <- if (global$data_format == "temporal_covid") {
            c("Individual Characteristics", "Geographic Characteristics", "Positive Response Rate")
          } else {
            c("Individual Characteristics", "Positive Response Rate")
          }
          updateSelectInput(session, "plot_category", choices = choices, selected = choices[1])
        }
      }
    })
    
    observeEvent(input$to_upload, {
      bslib::nav_select(
        id = "navbar_analyze",
        selected = "nav_analyze_upload",
        session = global$session
      )

      removeModal(global$session)
    })
    
  })
}
