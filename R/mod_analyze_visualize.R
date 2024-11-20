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
  uiOutput(outputId = ns("ui"))
}

#' analyze_visualize Server Functions
#'
#' @noRd
mod_analyze_visualize_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(global$covid, {
      if(global$covid) {
        output$ui <- renderUI({
          tags$div(class = "pad_top",
            navlistPanel(widths = c(3, 9),
              tabPanel("Individual Characteristics",
                tabsetPanel(
                  tabPanel("Sex",
                    plotOutput(outputId = ns("indiv_sex"))
                  ),
                  tabPanel("Race",
                    plotOutput(outputId = ns("indiv_race"))
                  ),
                  tabPanel("Age",
                    plotOutput(outputId = ns("indiv_age"))
                  ),
                  tabPanel("County",
                    tags$div(class = "pad_top",
                       column(width = 9,
                              plotly::plotlyOutput(outputId = ns("indiv_area_map"),
                                                   height = "500px")
                       ),
                       column(width = 3,
                              DT::dataTableOutput(outputId = ns("indiv_area_table"))
                       )
                    )
                  )
                )
              ),
              tabPanel("Geographic Characteristics",
                tabsetPanel(
                  tabPanel("Education",
                    column(width = 9,
                      plotOutput(outputId = ns("geo_edu_plot"))
                    ),
                    column(width = 3,
                      DT::dataTableOutput(outputId = ns("geo_edu_table"))
                    )
                  ),
                  tabPanel("Poverty",
                    column(width = 9,
                      plotOutput(outputId = ns("geo_poverty_plot"))
                    ),
                    column(width = 3,
                      DT::dataTableOutput(outputId = ns("geo_poverty_table"))
                    )
                  ),
                  tabPanel("Employment",
                    column(width = 9,
                      plotOutput(outputId = ns("geo_employ_plot"))
                    ),
                    column(width = 3,
                      DT::dataTableOutput(outputId = ns("geo_employ_table"))
                    )
                  ),
                  tabPanel("Income",
                    column(width = 9,
                      plotOutput(outputId = ns("geo_income_plot"))
                    ),
                    column(width = 3,
                      DT::dataTableOutput(outputId = ns("geo_income_table"))
                    )
                  ),
                  tabPanel("Urbanicity",
                    column(width = 9,
                      plotOutput(outputId = ns("geo_urban_plot"))
                    ),
                    column(width = 3,
                      DT::dataTableOutput(outputId = ns("geo_urban_table"))
                    )
                  ),
                  tabPanel("ADI",
                    column(width = 9,
                      plotOutput(outputId = ns("geo_adi_plot"))
                    ),
                    column(width = 3,
                      DT::dataTableOutput(outputId = ns("geo_adi_table"))
                    )
                  )
                )
              ),
              tabPanel("Positivity",
                tabsetPanel(
                  tabPanel("Overall",
                    plotOutput(outputId = ns("prev_overall"))
                  ),
                  tabPanel("By county",
                    tags$div(class = "pad_top",
                      plotly::plotlyOutput(outputId = ns("prev_county"),
                                           height = "700px")
                    )
                  )
                )
              )
            )
          )
        })

        output$indiv_sex <- renderPlot({
          req(global$mrp$input)

            plot_individual(
              global$mrp$input |> mutate(demo = sex) |> select(demo, total),
              global$mrp$new |> filter(time == 1) |> mutate(demo = sex) |> select(demo, total),
              global$mrp$levels$sex
            )
        }, height = function() global$static$ui$plot_height)

        output$indiv_race <- renderPlot({
          req(global$mrp$input)

            plot_individual(
              global$mrp$input |> mutate(demo = race) |> select(demo, total),
              global$mrp$new |> filter(time == 1) |> mutate(demo = race) |> select(demo, total),
              global$mrp$levels$race
            )
        }, height = function() global$static$ui$plot_height)

        output$indiv_age <- renderPlot({
          req(global$mrp$input)

            plot_individual(
              global$mrp$input |> mutate(demo = age) |> select(demo, total),
              global$mrp$new |> filter(time == 1) |> mutate(demo = age) |> select(demo, total),
              global$mrp$levels$age
            )
        }, height = function() global$static$ui$plot_height)

        output$indiv_area_map <- plotly::renderPlotly({
          req(global$mrp, global$plotdata)

          global$mrp$input |>
            mutate(fips = county) |>
            get_sample_size(
              global$extdata$covid$fips,
              state = FALSE
            ) |>
            mutate(value = count) |>
            choro_map(
              global$plotdata$geojson,
              map_title = "Map of County Sample Sizes",
              colorbar_title = "Sample\nSize",
              state = FALSE
            )
        })

        output$indiv_area_table <- DT::renderDataTable({
          req(global$mrp, global$plotdata)

          global$mrp$input |>
            mutate(fips = county) |>
            group_by(fips) |>
            summarize(sample_size = sum(total)) |>
            left_join(global$extdata$covid$fips, by = "fips") |>
            mutate(county = gsub(" County", "", county)) |>
            select(county, state, sample_size) |>
            arrange(desc(sample_size)) |>
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

        output$geo_urban_table <- DT::renderDataTable({
          req(global$plotdata$raw_covariates)

          global$plotdata$raw_covariates |>
            mutate(urbanicity = round(urbanicity, 4)) |>
            select(zip, urbanicity) |>
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

        output$geo_edu_table <- DT::renderDataTable({
          req(global$plotdata$raw_covariates)

          global$plotdata$raw_covariates |>
            mutate(measure = round(college, 4)) |>
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

        output$geo_poverty_table <- DT::renderDataTable({
          req(global$plotdata$raw_covariates)

          global$plotdata$raw_covariates |>
            mutate(measure = round(poverty, 4)) |>
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

        output$geo_employ_table <- DT::renderDataTable({
          req(global$plotdata$raw_covariates)

          global$plotdata$raw_covariates |>
            mutate(rate = round(employment, 4)) |>
            select(zip, rate) |>
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


        output$geo_income_table <- DT::renderDataTable({
          req(global$plotdata$raw_covariates)

          global$plotdata$raw_covariates |>
            mutate(income = round(income)) |>
            select(zip, income) |>
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

        output$geo_adi_table <- DT::renderDataTable({
          req(global$plotdata$raw_covariates)

          global$plotdata$raw_covariates |>
            mutate(ADI = round(ADI)) |>
            select(zip, ADI) |>
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

        output$geo_urban_plot <- renderPlot({
          req(global$plotdata$raw_covariates)

          threshold <- 0.95
          count <- sum(global$plotdata$raw_covariates$urbanicity >= threshold)
          total <- nrow(global$plotdata$raw_covariates)
          perc <- count / total

          global$plotdata$raw_covariates |>
            mutate(covar = urbanicity) |>
            plot_geographic(
              breaks = seq(0, 1, 0.05),
              description = sprintf("\n%d zip codes out of %d (%.2f%%) have %d%% or more tracts classified as urban.",
                                    count, total, perc*100, threshold*100),
              definition = "Urbanicity of a zip code is defined as the percentage of covered census tracts classified as urban\nweighted by tract population counts.",
              name = "Urbanicity"
            )

        }, height = function() global$static$ui$plot_height)

        output$geo_edu_plot <- renderPlot({
          req(global$plotdata$raw_covariates)

          threshold <- 0.5
          count <- sum(global$plotdata$raw_covariates$college >= threshold)
          total <- nrow(global$plotdata$raw_covariates)
          perc <- count / total

          global$plotdata$raw_covariates |>
            mutate(covar = college) |>
            plot_geographic(
              breaks = seq(0, 1, 0.05),
              description = sprintf("\n%d zip codes out of %d (%.2f%%) have %d%% or more people who have earned an Associate's degree or higher.",
                                    count, total, perc*100, threshold*100),
              definition = "Higher education measure of a zip code is defined as the percentage of the residing population\nwho have earned an Associate's degree or higher.",
              name = "Higher education measure"
            )

        }, height = function() global$static$ui$plot_height)

        output$geo_poverty_plot <- renderPlot({
          req(global$plotdata$raw_covariates)

          threshold <- 0.2
          count <- sum(global$plotdata$raw_covariates$poverty < threshold)
          total <- nrow(global$plotdata$raw_covariates)
          perc <- count / total

          global$plotdata$raw_covariates |>
            mutate(covar = poverty) |>
            plot_geographic(
              breaks = seq(0, 1, 0.05),
              description = sprintf("%d zip codes out of %d (%.2f%%) have %d%% or less people whose ratio of income to poverty level in the past 12 months\nis below 100%%.",
                                    count, total, perc*100, threshold*100),
              definition = "Poverty measure of a zip code is defined as the percentage of the residing population\nwhose ratio of income to poverty level in the past 12 months is below 100%.",
              name = "Poverty measure"
            )

        }, height = function() global$static$ui$plot_height)

        output$geo_employ_plot <- renderPlot({
          req(global$plotdata$raw_covariates)

          threshold <- 0.5
          count <- sum(global$plotdata$raw_covariates$employment >= threshold)
          total <- nrow(global$plotdata$raw_covariates)
          perc <- count / total

          global$plotdata$raw_covariates |>
            mutate(covar = employment) |>
            plot_geographic(
              breaks = seq(0, 1, 0.05),
              description = sprintf("\n%d zip codes out of %d (%.2f%%) have %d%% or more people who is employed as a part of the civilian labor force.",
                                    count, total, perc*100, threshold*100),
              definition = "Employment rate of a zip code is defined as the percentage of the residing population\nwho are employed as a part of the civilian labor force.",
              name = "Employment rate"
            )

        }, height = function() global$static$ui$plot_height)

        output$geo_income_plot <- renderPlot({
          req(global$plotdata$raw_covariates)

          threshold <- 70784
          count <- sum(global$plotdata$raw_covariates$income >= threshold)
          total <- nrow(global$plotdata$raw_covariates)
          perc <- count / total

          global$plotdata$raw_covariates |>
            mutate(covar = income) |>
            plot_geographic(
              breaks = seq(0, 150000, 5000),
              description = sprintf("%d zip codes out of %d (%.2f%%) have average value of tract-level median household income in the past 12 months\ngreater than %d dollars (2021 US median income according to the ACS).",
                                    count, total, perc*100, threshold),
              definition = "Income measure of a zip code is defined as the average value of tract-level median household income in the past 12 months\nweighted by tract population counts.",
              name = "Average of median household income"
            )

        }, height = function() global$static$ui$plot_height)

        output$geo_adi_plot <- renderPlot({
          req(global$plotdata$raw_covariates)

          threshold <- 80
          count <- sum(global$plotdata$raw_covariates$ADI > threshold)
          total <- nrow(global$plotdata$raw_covariates)
          perc <- count / total

          global$plotdata$raw_covariates |>
            mutate(covar = ADI) |>
            plot_geographic(
              breaks = seq(0, 100, 5),
              description = sprintf("\n%d zip codes out of %d (%.2f%%) have ADI over %d.",
                                    count, total, perc*100, threshold),
              definition = "Area Deprivation Index (ADI) of a zip code is the average ADI across covered census tracts\nweighted by tract population counts.",
              name = "Area Deprivation Index"
            )

        }, height = function() global$static$ui$plot_height)

        output$prev_overall <- renderPlot({
          req(global$mrp)

          plot_prev(global$mrp$input, global$plotdata$dates)

        }, height = function() global$static$ui$plot_height)

        output$prev_county <- plotly::renderPlotly({
          req(global$mrp)

          if("county" %in% names(global$mrp$input)) {
            global$mrp$input |>
              mutate(fips = county) |>
              get_raw_weekly_prev(global$extdata$covid$fips) |>
              mutate(value = max_prev) |>
              choro_map(
                global$plotdata$geojson,
                map_title = "Positivity Across Weeks",
                colorbar_title = "Highest\nWeekly\nPositivity",
                state = FALSE
              )
          }

        })

      } else {
        output$ui <- renderUI({
          tags$div(class = "pad_top",
            navlistPanel(widths = c(3, 9),
              tabPanel("Individual Characteristics",
                tabsetPanel(
                  tabPanel("Sex",
                    plotOutput(outputId = ns("indiv_sex"))
                  ),
                  tabPanel("Race",
                    plotOutput(outputId = ns("indiv_race"))
                  ),
                  tabPanel("Age",
                    plotOutput(outputId = ns("indiv_age"))
                  ),
                  tabPanel("Education",
                    plotOutput(outputId = ns("indiv_edu"))
                  ),
                  tabPanel("State",
                    tags$div(class = "pad_top",
                       column(width = 9,
                              plotly::plotlyOutput(outputId = ns("indiv_area_map"),
                                                   height = "500px")
                       ),
                       column(width = 3,
                              DT::dataTableOutput(outputId = ns("indiv_area_table"))
                       )
                    )
                  )
                )
              ),
              tabPanel("Response",
                tags$div(class = "pad_top",
                  plotly::plotlyOutput(outputId = ns("support_map"),
                                       height = "700px")
                )
              )
            )
          )
        })

        output$indiv_sex <- renderPlot({
          req(global$mrp$input)

            plot_individual(
              global$mrp$input |> mutate(demo = sex) |> select(demo, total),
              global$mrp$new |> mutate(demo = sex) |> select(demo, total),
              global$mrp$levels$sex
            )
        }, height = function() global$static$ui$plot_height)

        output$indiv_race <- renderPlot({
          req(global$mrp$input)

            plot_individual(
              global$mrp$input |> mutate(demo = race) |> select(demo, total),
              global$mrp$new |> mutate(demo = race) |> select(demo, total),
              global$mrp$levels$race
            )
        }, height = function() global$static$ui$plot_height)

        output$indiv_age <- renderPlot({
          req(global$mrp$input)

            plot_individual(
              global$mrp$input |> mutate(demo = age) |> select(demo, total),
              global$mrp$new |> mutate(demo = age) |> select(demo, total),
              global$mrp$levels$age
            )
        }, height = function() global$static$ui$plot_height)

        output$indiv_edu <- renderPlot({
          req(global$mrp$input)

            plot_individual(
              global$mrp$input |> mutate(demo = edu) |> select(demo, total),
              global$mrp$new |> mutate(demo = edu) |> select(demo, total),
              global$mrp$levels$edu
            )
        }, height = function() global$static$ui$plot_height)

        output$indiv_area_map <- plotly::renderPlotly({
          req(global$mrp, global$plotdata)

          global$mrp$input |>
            mutate(fips = state) |>
            get_sample_size(
              global$extdata$poll$fips,
              state = TRUE
            ) |>
            mutate(value = count) |>
            choro_map(
              global$plotdata$geojson,
              map_title = "Map of state sample size",
              colorbar_title = "Sample\nSize",
              state = TRUE
            )
        })

        output$indiv_area_table <- DT::renderDataTable({
          req(global$mrp, global$plotdata)

          global$mrp$input |>
            mutate(fips = state) |>
            group_by(fips) |>
            summarize(sample_size = sum(total)) |>
            left_join(global$extdata$poll$fips, by = "fips") |>
            select(state, sample_size) |>
            arrange(desc(sample_size)) |>
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

        output$support_map <- plotly::renderPlotly({
          req(global$mrp)

          if("state" %in% names(global$mrp$input)) {
            global$mrp$input |>
              mutate(fips = state) |>
              get_raw_support(global$extdata$poll$fips) |>
              mutate(value = support) |>
              choro_map(
                global$plotdata$geojson,
                map_title = "Percentage of Positive Response",
                colorbar_title = "%",
                state = TRUE
              )
          }


        })
      }
    })

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
