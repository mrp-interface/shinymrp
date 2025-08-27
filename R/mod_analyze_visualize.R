#' Data Visualization Module UI Function
#'
#' @description Creates the user interface for data visualization and exploratory
#' analysis in the MRP application. Provides a sidebar layout with dynamic
#' selection controls for different plot categories including individual-level
#' characteristics, geographic patterns, and outcome averages. Supports
#' both time-varying and cross-sectional data visualization with interactive maps
#' and plots.
#'
#' @param id Character string. The module's namespace identifier.
#'
#' @return A `bslib::layout_sidebar` containing the visualization interface with:
#' \itemize{
#'   \item Sidebar with dynamic plot category and subcategory selection
#'   \item Conditional panels for time-varying data options
#'   \item Main panel with dynamic plot output based on selections
#' }
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput selectInput conditionalPanel plotOutput
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
        condition = sprintf("output.is_timevar && input['%s'] == 'by_geo'", 
                            ns("plot_subcategory")),
        bslib::card(
          selectizeInput(
            inputId = ns("summary_slt"),
            label = "Select summary statistic",
            choices = .const()$ui$plot_selection$summary,
            options = list(dropdownParent = "body")
          )
        )
      )
    ),
    uiOutput(ns("plot_output"))
  )
}

#' Data Visualization Module Server Function
#'
#' @description Server logic for the data visualization module. Manages dynamic
#' UI updates based on data format and linking geography, renders various types
#' of plots including individual-level characteristics, geographic patterns,
#' and outcome averages. Handles both time-varying and cross-sectional data
#' with appropriate visualization methods including interactive maps and charts.
#'
#' @param id Character string. The module's namespace identifier.
#' @param global Reactive values object containing global application state
#'
#' @return Server function for the visualization module. Creates reactive
#' updates for plot selection, renders dynamic UI components, and generates
#' various types of plots and maps for data exploration.
#'
#' @noRd
#'
#' @importFrom shiny moduleServer observeEvent updateSelectInput renderUI renderPlot req isolate
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom shinyjs reset
mod_analyze_visualize_server <- function(id, global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Update the plot category selectInput based on the linking geography.
    observeEvent(global$mrp_ver, {
      req(global$workflow)

      # Reset the select inputs
      shinyjs::reset("summary_slt")

      choices <- .const()$ui$plot_selection$vis_main[[global$workflow$metadata()$family]]

      if (is.null(global$workflow$link_data()$link_geo)) {
        choices <- choices[!choices == "geo"]
      }

      # Update the plot category selectInput with the new choices.
      updateSelectInput(session, "plot_category", choices = c("foo")) # trigger update
      updateSelectInput(session, "plot_category", choices = choices)
    })


    # Update the subcategory selectInput based on the main category selection.
    observeEvent(input$plot_category, {
      # Define the subcategory choices for each category.
      out <- .vis_subcat_select(
        category = input$plot_category,
        metadata = global$workflow$metadata(),
        linkdata = global$workflow$link_data()
      )

      # Update the subcategory selectInput with the new choices and label.
      updateSelectInput(
        session = session,
        inputId = "plot_subcategory",
        choices = out$choices,
        label   = out$label)
    })

    # Render the UI for the selected plot.
    output$plot_output <- renderUI({
      req(input$plot_category, input$plot_subcategory)

      .vis_ui(
        ns = ns,
        cat = input$plot_category,
        subcategory = input$plot_subcategory
      )
    })

    # --------------------------------------------------------------------------
    # Module Server Calls for Individual-level Plots
    # --------------------------------------------------------------------------
    mod_indiv_plot_server(
      "indiv_sex",
      reactive({
        global$prep_ver
        global$mrp_ver
        global$workflow
      }), 
      "sex"
    )
    mod_indiv_plot_server(
      "indiv_race",
      reactive({
        global$prep_ver
        global$mrp_ver
        global$workflow
      }),
      "race"
    )
    mod_indiv_plot_server(
      "indiv_age",
      reactive({
        global$prep_ver
        global$mrp_ver
        global$workflow
      }),
      "age"
    )
    mod_indiv_plot_server(
      "indiv_edu",
      reactive({
        global$prep_ver
        global$mrp_ver
        global$workflow
      }),
      "edu"
    )

    # --------------------------------------------------------------------------
    # Sample Size Map and Table
    # --------------------------------------------------------------------------
    mod_indiv_map_server(
      "geo_sample",
      reactive({
        global$prep_ver
        global$mrp_ver
        global$workflow
      })
    )

    # --------------------------------------------------------------------------
    # Module Server Calls for Geographic-level Plots
    # --------------------------------------------------------------------------
    mod_geo_plot_server(
      "geo_college",
      reactive({
        global$prep_ver
        global$mrp_ver
        global$workflow
      }),
      "college"
    )
    
    mod_geo_plot_server(
      "geo_poverty",
      reactive({
        global$prep_ver
        global$mrp_ver
        global$workflow
      }),
      "poverty"
    )

    mod_geo_plot_server(
      "geo_employment",
      reactive({
        global$prep_ver
        global$mrp_ver
        global$workflow
      }),
      "employment"
    )

    mod_geo_plot_server(
      "geo_income",
      reactive({
        global$prep_ver
        global$mrp_ver
        global$workflow
      }),
      "income"
    )

    mod_geo_plot_server(
      "geo_urbanicity",
      reactive({
        global$prep_ver
        global$mrp_ver
        global$workflow
      }),
      "urbanicity"
    )

    mod_geo_plot_server(
      "geo_adi",
      reactive({
        global$prep_ver
        global$mrp_ver
        global$workflow
      }),
      "adi"
    )


    # --------------------------------------------------------------------------
    # Plot for Outcome Measure over Time
    # --------------------------------------------------------------------------
    output$positive_plot <- renderPlot({
      req(global$workflow)
      global$workflow$outcome_plot()
    })

    # --------------------------------------------------------------------------
    # Map for Outcome Measure
    # --------------------------------------------------------------------------
    output$positive_map <- highcharter::renderHighchart({
      req(global$workflow, global$workflow$link_data()$link_geo)

      global$workflow$outcome_map(summary_type = input$summary_slt)
    })
    
  })
}
