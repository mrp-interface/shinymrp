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
    class = "p-0",
    fillable = TRUE,
    sidebar = bslib::sidebar(
      width = 375,
      # First selectInput: choose the plot category.
      selectInput(
        inputId = ns("plot_category"),
        label = "1. Select plot category",
        choices = NULL,
        width = "100%"
      ),
      selectInput(
        inputId = ns("plot_subcategory"),
        label = "",
        choices = NULL,
        width = "100%"
      ),
      conditionalPanel(
        condition = sprintf("(output.data_format == 'temporal_covid' || output.data_format == 'temporal_other') && input['%s'] == 'by_geo'", 
                            ns("plot_subcategory")),
        bslib::card(
          selectizeInput(
            inputId = ns("extreme_select"),
            label = "Select quantity",
            choices = c("Highest Weekly Rate" = "max",
                        "Lowest Weekly Rate" = "min"),
            options = list(dropdownParent = "body")
          )
        )
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

    # Update the plot category selectInput based on the linking geography.
    observeEvent(global$link_data, {
      choices <- GLOBAL$ui$plot_selection$vis_main
      if (is.null(global$link_data$link_geo)) {
        choices <- choices[!choices == "geo"]
      }

      # Update the plot category selectInput with the new choices.
      updateSelectInput(session, "plot_category", choices = choices)
      
      # Reset extreme select input
      shinyjs::reset("extreme_select")
    })


    # Update the subcategory selectInput based on the main category selection.
    observeEvent(input$plot_category, {
      # Define the subcategory choices for each category.

      if (input$plot_category == "indiv") {
        label <- "2. Select characteristic"
        choices <- GLOBAL$ui$plot_selection$indiv
        if(global$data_format != "static_poll") {
          choices <- choices[!choices == "edu"]
        }
      } else if (input$plot_category == "geo") {
        label <- "2. Select characteristic"
        choices <- switch(global$data_format,
          "temporal_covid" = c(GLOBAL$ui$plot_selection$geo,
                               GLOBAL$ui$plot_selection$geo_covar),
          "temporal_other" = GLOBAL$ui$plot_selection$geo,
          "static_poll"    = GLOBAL$ui$plot_selection$geo,
          "static_other"   = GLOBAL$ui$plot_selection$geo
        )
      } else if (input$plot_category == "pos_rate") {
        label <- "2. Select plot type"
        choices <- GLOBAL$ui$plot_selection$pos_rate

        if (global$data_format %in% c("static_poll", "static_other")) {
          choices <- choices[!choices == "overall"]
        }

        if (is.null(global$link_data$link_geo)) {
          choices <- choices[!choices == "by_geo"]
        }
      } else {
        label <- character(0)  # No label if the category is not recognized.
        choices <- NULL  # No choices if the category is not recognized.
      }

      # Update the subcategory selectInput with the new choices and label.
      updateSelectInput(
        session = session,
        inputId = "plot_subcategory",
        choices = choices,
        label = label)
    })

    # Render the UI for the selected plot.
    output$plot_output <- renderUI({
      req(input$plot_category, input$plot_subcategory)

      category <- isolate(input$plot_category)
      subcategory <- isolate(input$plot_subcategory)

      if (category == "indiv") {
        switch(subcategory,
          "sex" = mod_indiv_plot_ui(ns("indiv_sex")),
          "race" = mod_indiv_plot_ui(ns("indiv_race")),
          "age" = mod_indiv_plot_ui(ns("indiv_age")),
          "edu" = mod_indiv_plot_ui(ns("indiv_edu"))
        )
      } else if (category == "geo") {
        switch(subcategory,
          "sample" = mod_indiv_map_ui(ns("geo_sample")),
          "edu" = mod_geo_plot_ui(ns("geo_edu")),
          "poverty" = mod_geo_plot_ui(ns("geo_poverty")),
          "employ" = mod_geo_plot_ui(ns("geo_employ")),
          "income" = mod_geo_plot_ui(ns("geo_income")),
          "urban" = mod_geo_plot_ui(ns("geo_urban")),
          "adi" = mod_geo_plot_ui(ns("geo_adi"))
        )
      } else if (category == "pos_rate") {
        switch(subcategory,
          "overall" = plotOutput(ns("positive_plot"), height = GLOBAL$ui$plot_height),
          "by_geo" = highcharter::highchartOutput(ns("positive_map"), height = GLOBAL$ui$map_height)
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
      "geo_sample",
      reactive(global$mrp$input),
      reactive(global$link_data$link_geo),
      reactive(global$plot_data$geojson),
      global$extdata$fips
    )

    # --------------------------------------------------------------------------
    # Module Server Calls for Geographic-level Plots
    # --------------------------------------------------------------------------
    mod_geo_plot_server("geo_edu", reactive(global$plot_data$raw_covariates), "college", list(
      threshold   = 0.5,
      operation   = ">=",
      breaks      = seq(0, 1, 0.05),
      description = "\n%d ZIP codes out of %d (%d%%) have %d%% or more people who have earned an Associate's degree or higher.",
      definition  = "Higher education measure of a zip code is defined as the percentage of the residing population\nwho have earned an Associate's degree or higher.",
      name        = "Higher education measure"
    ))
    
    mod_geo_plot_server("geo_poverty", reactive(global$plot_data$raw_covariates), "poverty", list(
      threshold   = 0.2,
      operation   = "<=",
      breaks      = seq(0, 1, 0.05),
      description = "%d zip codes out of %d (%d%%) have %d%% or less people whose ratio of income to poverty level in the past 12 months\nis below 100%%.",
      definition  = "Poverty measure of a zip code is defined as the percentage of the residing population\nwhose ratio of income to poverty level in the past 12 months is below 100%%.",
      name        = "Poverty measure"
    ))
    
    mod_geo_plot_server("geo_employ", reactive(global$plot_data$raw_covariates), "employment", list(
      threshold   = 0.5,
      operation   = ">=",
      breaks      = seq(0, 1, 0.05),
      description = "\n%d zip codes out of %d (%d%%) have %d%% or more people who is employed as a part of the civilian labor force.",
      definition  = "Employment rate of a zip code is defined as the percentage of the residing population\nwho are employed as a part of the civilian labor force.",
      name        = "Employment rate"
    ))
    
    mod_geo_plot_server("geo_income", reactive(global$plot_data$raw_covariates), "income", list(
      threshold   = 70784,
      operation   = ">",
      breaks      = seq(0, 150000, 5000),
      description = "%d zip codes out of %d (%d%%) have average value of tract-level median household income in the past 12 months\ngreater than %d dollars (2021 US median income according to the ACS).",
      definition  = "Income measure of a zip code is defined as the average value of tract-level median household income in the past 12 months\nweighted by tract population counts.",
      name        = "Average of median household income"
    ))
    
    mod_geo_plot_server("geo_urban", reactive(global$plot_data$raw_covariates), "urbanicity", list(
      threshold   = 0.95,
      operation   = ">=",
      breaks      = seq(0, 1, 0.05),
      description = "\n%d zip codes out of %d (%d%%) have %d%% or more tracts classified as urban.",
      definition  = "Urbanicity of a zip code is defined as the percentage of covered census tracts classified as urban\nweighted by tract population counts.",
      name        = "Urbanicity"
    ))
    
    mod_geo_plot_server("geo_adi", reactive(global$plot_data$raw_covariates), "ADI", list(
      threshold   = 80,
      operation   = ">",
      breaks      = seq(0, 100, 5),
      description = "\n%d zip codes out of %d (%d%%) have ADI over %d.",
      definition  = "Area Deprivation Index (ADI) of a zip code is the average ADI across covered census tracts\nweighted by tract population counts (refer to Learn > Preprocess page for the definition of ADI).",
      name        = "Area Deprivation Index"
    ))


    # --------------------------------------------------------------------------
    # Plot for Positive Response Rate
    # --------------------------------------------------------------------------
    output$positive_plot <- renderPlot({
      req(input$plot_subcategory == "overall")
      plot_prev(global$mrp$input, global$plot_data$dates)
    })

    # --------------------------------------------------------------------------
    # Map for Positive Response Rate
    # --------------------------------------------------------------------------
    output$positive_map <- highcharter::renderHighchart({
      req(global$link_data$link_geo)

      geo <- if(global$link_data$link_geo == "zip") "county" else global$link_data$link_geo

      if(global$data_format %in% c("temporal_covid", "temporal_other")) {
        plot_df <- global$mrp$input %>%
          prep_raw_prev(
            fips_codes = global$extdata$fips[[geo]],
            geo = geo,
            extreme_type = input$extreme_select
          )

        choro_map(
          plot_df,
          global$plot_data$geojson[[geo]],
          main_title = "Weekly Positive Response Rate By Geography",
          sub_title = switch(input$extreme_select,
            "max" = "Highest Weekly Rate",
            "min" = "Lowest Weekly Rate"
          ),
          geo = geo,
          config = if (max(plot_df$value) == 0) list(minValue = 0, maxValue = 1) else NULL
        )
      } else {
        global$mrp$input %>%
          prep_raw_support(
            fips_codes = global$extdata$fips[[geo]],
            geo = geo
          ) %>%
          mutate(value = .data$support) %>%
          choro_map(
            global$plot_data$geojson[[geo]],
            main_title = "Positive Response Rate By Geography",
            sub_title = "Positive Response Rate",
            geo = geo
          )
      }
    })
    
  })
}
