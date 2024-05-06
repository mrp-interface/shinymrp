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
  uiOutput(outputId = ns("ui"))
}

#' analyze_result Server Functions
#'
#' @noRd
mod_analyze_result_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(global$input$navbar_analyze, {
      if(global$input$navbar_analyze == "nav_analyze_result") {
        if(is.null(global$mrp_input)) {
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

        if(length(global$models) == 0) {
          showModal(
            modalDialog(
              title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
              "No model detected.",
              footer = actionButton(
                inputId = ns("to_model"),
                label = "Go to model page"
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

    observeEvent(input$to_model, {
      updateTabsetPanel(global$session,
        inputId = "navbar_analyze",
        selected = "nav_analyze_model"
      )

      removeModal(global$session)
    })

    observeEvent(global$covid, {
      if(global$covid) {

        output$ui <- renderUI({
          fips_df <- global$extdata$covid$fips |> filter(fips %in% global$mrp_input$levels$county)
          counties <- sort(fips_df$county)

          tags$div(class = "pad_top",
            navlistPanel(widths = c(3, 9),
              tabPanel(
                selectInput(
                  inputId = ns("model_select"),
                  label = "Select a model",
                  choices = names(global$models)
                )
              ),
              tabPanel("Raw vs MRP",
                plotOutput(outputId = ns("est_overall"))
              ),
              tabPanel("By subgroup",
                tabsetPanel(
                  tabPanel("Sex",
                    plotOutput(outputId = ns("est_sex"))
                  ),
                  tabPanel("Race",
                    plotOutput(outputId = ns("est_race"))
                  ),
                  tabPanel("Age",
                    plotOutput(outputId = ns("est_age"))
                  ),
                  tabPanel("County",
                    tags$div(class = "pad_top",
                      plotly::plotlyOutput(outputId = ns("est_county_map"),
                                           height = "700px")
                    ),
                    tags$div(class = "pad_top",
                      selectizeInput(
                        inputId = ns("counties_select"),
                        label = "Select one or more counties (max = 5)",
                        choices = counties,
                        selected = counties[1],
                        multiple = TRUE,
                        options = list(maxItems = 5)
                      ),
                      plotOutput(outputId = ns("est_county_line"))
                    )
                  )
                )
              )
            )
          )
        })

        output$est_overall <- renderPlot({
          req(names(global$models))

          plot_prev(
            global$mrp_input$brms_input,
            global$plotdata$dates,
            global$models[[input$model_select]]$overall
          )
        }, height = function() global$static$ui$plot_height)

        output$est_sex <- renderPlot({
          req(names(global$models))

          plot_est_covid(
            global$models[[input$model_select]]$sex,
            global$plotdata$dates
          )

        }, height = function() global$static$ui$subplot_height * (length(global$mrp_input$levels$sex) + 1))

        output$est_race <- renderPlot({
          req(names(global$models))

          plot_est_covid(
            global$models[[input$model_select]]$race,
            global$plotdata$dates
          )

        }, height = function() global$static$ui$subplot_height * (length(global$mrp_input$levels$race) + 1))

        output$est_age <- renderPlot({
          req(names(global$models))

          plot_est_covid(
            global$models[[input$model_select]]$age,
            global$plotdata$dates
          )

        }, height = function() global$static$ui$subplot_height * (length(global$mrp_input$levels$age) + 1))

        output$est_county_map <- plotly::renderPlotly({
          req(names(global$models))

          global$models[[input$model_select]]$county |>
            mutate(fips = factor) |>
          get_est_weekly_prev(
            global$extdata$covid$fips,
            global$plotdata$dates
          ) |>
            mutate(value = est) |>
            choro_map(
              global$plotdata$geojson,
              map_title = "Estimated Prevalence",
              colorbar_title = "Prevalence",
              state = FALSE
            ) |> suppressWarnings()
        })

        output$est_county_line <- renderPlot({
          req(names(global$models))

          global$models[[input$model_select]]$county |>
            mutate(fips = factor) |>
            left_join(global$extdata$covid$fips, by = "fips") |>
            select(time, county, est, std) |>
            filter(county %in% input$counties_select) |>
            rename("factor" = "county") |>
            plot_est_covid(global$plotdata$dates)

        }, height = function() global$static$ui$subplot_height * (length(input$counties_select) + 1))


      } else {
        output$ui <- renderUI({
          tags$div(class = "pad_top",
            navlistPanel(widths = c(3, 9),
              tabPanel(
                selectInput(
                  inputId = ns("model_select"),
                  label = "Select a model",
                  choices = names(global$models)
                )
              ),
              tabPanel("Raw vs MRP",
                plotOutput(outputId = ns("est_overall"))
              ),
              tabPanel("By subgroup",
                tabsetPanel(
                  tabPanel("Sex",
                    plotOutput(outputId = ns("est_sex"))
                  ),
                  tabPanel("Race",
                    plotOutput(outputId = ns("est_race"))
                  ),
                  tabPanel("Age",
                    plotOutput(outputId = ns("est_age"))
                  ),
                  tabPanel("Education",
                    plotOutput(outputId = ns("est_edu"))
                  ),
                  tabPanel("State",
                    tags$div(class = "pad_top",
                      plotly::plotlyOutput(outputId = ns("est_state_map"),
                                           height = "700px")
                    ),
                    plotOutput(outputId = ns("est_state_point"))
                  )
                )
              )
            )
          )
        })


        output$est_overall <- renderPlot({
          req(names(global$models))

          global$models[[input$model_select]]$overall |>
            mutate(
              data = "Estimate",
              lower = est - std,
              median = est,
              upper = est + std
            ) |>
            select(data, lower, median, upper) |>
            plot_support(global$mrp_input$brms_input)
        })

        output$est_sex <- renderPlot({
          req(names(global$models))

          plot_est_poll(global$models[[input$model_select]]$sex)

        }, height = function() global$static$ui$plot_height)

        output$est_race <- renderPlot({
          req(names(global$models))

          plot_est_poll(global$models[[input$model_select]]$race)

        }, height = function() global$static$ui$plot_height)

        output$est_age <- renderPlot({
          req(names(global$models))

          plot_est_poll(global$models[[input$model_select]]$age)

        }, height = function() global$static$ui$plot_height)

        output$est_edu <- renderPlot({
          req(names(global$models))

          plot_est_poll(global$models[[input$model_select]]$edu)

        }, height = function() global$static$ui$plot_height)

        output$est_state_point <- renderPlot({
          req(names(global$models), global$plotdata)

          global$models[[input$model_select]]$state |>
            mutate(fips = factor) |>
            left_join(global$extdata$poll$fips, by = "fips") |>
            select(state, est, std) |>
            rename("factor" = "state") |>
            plot_est_poll()

        }, height = function() global$static$ui$plot_height)

        output$est_state_map <- plotly::renderPlotly({
          req(names(global$models), global$plotdata)


          global$models[[input$model_select]]$state |>
            mutate(fips = factor) |>
            get_est_support(global$extdata$poll$fips) |>
            mutate(value = est) |>
            choro_map(
              global$plotdata$geojson,
              map_title = "MRP Estimate of Positive Response Rate",
              colorbar_title = "%",
              state = TRUE
            )
        })
      }
    })

  })
}
