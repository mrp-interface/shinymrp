#' analyze_visualize UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analyze_visualize_ui <- function(id){
  ns <- NS(id)
  tags$div(class = "pad_top",
    navlistPanel(widths = c(3, 9),
      id = ns("navbar"),
      tabPanel("Individual Characteristics",
        value = "nav_indiv",
        tabsetPanel(
          id = ns("navbar_indiv"),
          tabPanel("Sex",
            value = "nav_indiv_sex",
            mod_indiv_plot_ui(ns("indiv_sex"))
          ),
          tabPanel("Race",
            value = "nav_indiv_race",
            mod_indiv_plot_ui(ns("indiv_race"))
          ),
          tabPanel("Age",
            value = "nav_indiv_age",
            mod_indiv_plot_ui(ns("indiv_age"))
          ),
          tabPanel("Education",
            value = "nav_indiv_edu",
            mod_indiv_plot_ui(ns("indiv_edu"))
          ),
          tabPanel("Geography",
            value = "nav_indiv_geo",
            tags$div(class = "pad_top",
              conditionalPanel(
                condition = "output.no_geo",
                tags$p("Map unavailable", class = "alt_text")
              ),
              column(width = 9,
                    plotly::plotlyOutput(outputId = ns("sample_size_map"),
                                          height = GLOBAL$ui$small_map_height)
              ),
              column(width = 3,
                    DT::dataTableOutput(outputId = ns("sample_size_table"))
              )
            )
          )
        )
      ),
      tabPanel("Geographic Characteristics",
        value = "nav_geo",
        tabsetPanel(
          id = ns("navbar_geo"),
          tabPanel("Education",
            value = "nav_geo_edu",
            mod_geo_plot_ui(ns("geo_edu"))
          ),
          tabPanel("Poverty",
            value = "nav_geo_poverty",
            mod_geo_plot_ui(ns("geo_poverty"))
          ),
          tabPanel("Employment",
            value = "nav_geo_employ",
            mod_geo_plot_ui(ns("geo_employ"))
          ),
          tabPanel("Income",
            value = "nav_geo_income",
            mod_geo_plot_ui(ns("geo_income"))
          ),
          tabPanel("Urbanicity",
            value = "nav_geo_urban",
            mod_geo_plot_ui(ns("geo_urban"))
          ),
          tabPanel("ADI",
            value = "nav_geo_adi",
            mod_geo_plot_ui(ns("geo_adi"))
          )
        )
      ),
      tabPanel("Positive Response Rate",
        value = "nav_positive",
        tabsetPanel(
          id = ns("navbar_positive"),
          tabPanel("Overall",
            value = "nav_positive_overall",
            plotOutput(
              outputId = ns("positive_plot"),
              height = GLOBAL$ui$plot_height
            )
          ),
          tabPanel("By geography",
            value = "nav_positive_by_geo",
            tags$div(class = "pad_top",
              conditionalPanel(
                condition = "output.no_geo",
                tags$p("Map unavailable", class = "alt_text")
              ),
              plotly::plotlyOutput(
                outputId = ns("positive_map"),
                height = GLOBAL$ui$larger_map_height
              )
            )
          )
        )
      )
    )
  )
}

#' analyze_visualize Server Functions
#'
#' @noRd
mod_analyze_visualize_server <- function(id, global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Initialize individual plot modules
    mod_indiv_plot_server("indiv_sex", reactive(global$mrp), "sex")
    mod_indiv_plot_server("indiv_race", reactive(global$mrp), "race")
    mod_indiv_plot_server("indiv_age", reactive(global$mrp), "age")
    mod_indiv_plot_server("indiv_edu", reactive(global$mrp), "edu")
    
    # Education
    mod_geo_plot_server("geo_edu", 
      reactive(global$plotdata$raw_covariates),
      "college",
      list(
        threshold = 0.5,
        operation = ">=",
        breaks = seq(0, 1, 0.05),
        description = "\n%d ZIP codes out of %d (%.2f%%) have %d%% or more people who have earned an Associate's degree or higher.",
        definition = "Higher education measure of a zip code is defined as the percentage of the residing population\nwho have earned an Associate's degree or higher.",
        name = "Higher education measure"
      )
    )
    
    # Poverty
    mod_geo_plot_server("geo_poverty",
      reactive(global$plotdata$raw_covariates),
      "poverty",
      list(
        threshold = 0.2,
        operation = "<=",
        breaks = seq(0, 1, 0.05),
        description = "%d zip codes out of %d (%.2f%%) have %d%% or less people whose ratio of income to poverty level in the past 12 months\nis below 100%%.",
        definition = "Poverty measure of a zip code is defined as the percentage of the residing population\nwhose ratio of income to poverty level in the past 12 months is below 100%.",
        name = "Poverty measure"
      )
    )

    # Employment
    mod_geo_plot_server("geo_employ",
      reactive(global$plotdata$raw_covariates),
      "employment",
      list(
        threshold = 0.5,
        operation = ">=",
        breaks = seq(0, 1, 0.05),
        description = "\n%d zip codes out of %d (%.2f%%) have %d%% or more people who is employed as a part of the civilian labor force.",
        definition = "Employment rate of a zip code is defined as the percentage of the residing population\nwho are employed as a part of the civilian labor force.",
        name = "Employment rate"
      )
    )

    # Income
    mod_geo_plot_server("geo_income",
      reactive(global$plotdata$raw_covariates),
      "income",
      list(
        threshold = 70784,
        operation = ">",
        breaks = seq(0, 150000, 5000),
        description = "%d zip codes out of %d (%.2f%%) have average value of tract-level median household income in the past 12 months\ngreater than %d dollars (2021 US median income according to the ACS).",
        definition = "Income measure of a zip code is defined as the average value of tract-level median household income in the past 12 months\nweighted by tract population counts.",
        name = "Average of median household income"
      )
    )

    # Urbanicity
    mod_geo_plot_server("geo_urban",
      reactive(global$plotdata$raw_covariates),
      "urbanicity",
      list(
        threshold = 0.95,
        operation = ">=",
        breaks = seq(0, 1, 0.05),
        description = "\n%d zip codes out of %d (%.2f%%) have %d%% or more tracts classified as urban.",
        definition = "Urbanicity of a zip code is defined as the percentage of covered census tracts classified as urban\nweighted by tract population counts.",
        name = "Urbanicity"
      )
    )

    # ADI
    mod_geo_plot_server("geo_adi",
      reactive(global$plotdata$raw_covariates),
      "ADI",
      list(
        threshold = 80,
        operation = ">",
        breaks = seq(0, 100, 5),
        description = "\n%d zip codes out of %d (%.2f%%) have ADI over %d.",
        definition = "Area Deprivation Index (ADI) of a zip code is the average ADI across covered census tracts\nweighted by tract population counts (refer to Learn > Preprocess page for the definition of ADI).",
        name = "Area Deprivation Index"
      )
    )

    output$sample_size_map <- plotly::renderPlotly({
      req(global$link_data$link_geo)

      geo <- if(global$link_data$link_geo == "zip") "county" else global$link_data$link_geo

      global$mrp$input |>
        prep_sample_size(
          fips_codes = global$extdata$fips[[geo]],
          geo = geo,
          for_map = TRUE
        ) |>
        mutate(value = count) |>
        choro_map(
          global$plotdata$geojson[[geo]],
          map_title = sprintf("Sample Size Map"),
          colorbar_title = "Sample\nSize",
          geo = geo
        )
    })

    output$sample_size_table <- DT::renderDataTable({
      req(global$link_data$link_geo)

      geo <- if(global$link_data$link_geo == "zip") "county" else global$link_data$link_geo

      global$mrp$input |>
        prep_sample_size(
          fips_codes = global$extdata$fips[[geo]],
          geo = geo,
          for_map = FALSE
        ) |>
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
    
    output$positive_plot <- renderPlot({
      req(global$mrp)

      plot_prev(global$mrp$input, global$plotdata$dates)

    })
    
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

    # Show/hide tabs based on data format
    observeEvent(global$data_format, {
      # show main tabs
      for(tab in c("nav_indiv", "nav_geo", "nav_positive")) {
        showTab("navbar", tab)
      }

      # show individual-level tabs
      for(tab in c("nav_indv_sex", "nav_indiv_race", "nav_indiv_age", "nav_indiv_edu")) {
        showTab("navbar_indiv", tab)
      }

      # show geographic-level tabs
      for(tab in c("nav_geo_edu", "nav_geo_poverty", "nav_geo_employ", "nav_geo_income", "nav_geo_urban", "nav_geo_adi")) {
        showTab("navbar_geo", tab)
      }

      # show positive response rate tabs
      for(tab in c("nav_positive_overall", "nav_positive_by_geo")) {
        showTab("navbar_positive", tab)
      }

      if(global$data_format == "temporal_covid") {
        hideTab("navbar_indiv", "nav_indiv_edu")
      } else if (global$data_format == "temporal_other") {
        hideTab("navbar_indiv", "nav_indiv_edu")
        hideTab("navbar", "nav_geo")
      } else if (global$data_format == "static_poll") {
        hideTab("navbar", "nav_geo")
        hideTab("navbar_positive", "nav_positive_overall")
        updateTabsetPanel(session, "navbar_positive", selected = "nav_positive_by_geo")
      } else if (global$data_format == "static_other") {
        hideTab("navbar", "nav_geo")
        hideTab("navbar_indiv", "nav_indiv_edu")
        hideTab("navbar_positive", "nav_positive_overall")
        updateTabsetPanel(session, "navbar_positive", selected = "nav_positive_by_geo")
      }
    })
    
    # Navigation event handlers
    observeEvent(global$input$navbar_analyze, {
      if(global$input$navbar_analyze == "nav_analyze_visualize") {
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
      }
    })
    
    observeEvent(input$to_upload, {
      updateTabsetPanel(global$session,
        inputId = "navbar_analyze",
        selected = "nav_analyze_upload"
      )

      removeModal(global$session)
    })
  })
}
