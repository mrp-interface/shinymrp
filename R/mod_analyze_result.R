#' analyze_result UI Function
#'
#' @description A shiny Module that uses a sidebar layout with dynamic selectInputs
#'              to choose which result (plot) to display.
#'
#' @param id A unique id for the module.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList layout_sidebar sidebar layout_sidebar sidebarPanel mainPanel
mod_analyze_result_ui <- function(id){
  ns <- NS(id)
  
  # Use layout_sidebar from bslib for a modern sidebar layout.
  layout_sidebar(
    sidebar = sidebar(
      width = 350,
      # Select a model from available models.
      selectInput(
        inputId = ns("model_select"),
        label = "Select a model",
        choices = NULL
      ),
      # Select the result category.
      selectInput(
        inputId = ns("result_category"),
        label = "Select result type",
        choices = c("Raw vs MRP", "By Subgroup")
      ),
      # If user selects "By Subgroup", show subgroup choice.
      conditionalPanel(
        condition = sprintf("input['%s'] == 'By Subgroup'", ns("result_category")),
        selectInput(
          inputId = ns("subgroup_select"),
          label = "Select subgroup",
          choices = c("Sex", "Race", "Age", "Education", "Geography")
        )
      )
    ),
    uiOutput(ns("result_output"))
  )
}

#' analyze_result Server Functions
#'
#' @noRd
mod_analyze_result_server <- function(id, global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Reactive for the selected model.
    selected_model <- reactive({
      req(input$model_select)
      global$poststratified_models[[input$model_select]]
    })
    # Buffer to preserve selection.
    model_select_buffer <- reactive(input$model_select)

    # Reactive to determine the selected geographic scale.
    selected_scale <- reactive({
      req(selected_model())

      if(selected_model()$data_format == "temporal_covid") {
        "county"
      } else if (selected_model()$data_format == "static_poll") {
        "state"
      } else {
        input$geo_scale_select
      }
    })

    # Create reactive outputs for dynamic conditionals.
    output$no_geo <- reactive(is.null(selected_model()$link_data$link_geo))
    outputOptions(output, "no_geo", suspendWhenHidden = FALSE)
    output$data_format <- reactive(selected_model()$data_format)
    outputOptions(output, "data_format", suspendWhenHidden = FALSE)

    # --------------------------------------------------------------------------
    # Initialize demographic plot modules (always called so the server is ready)
    # --------------------------------------------------------------------------
    mod_est_plot_server("est_sex", selected_model, "sex")
    mod_est_plot_server("est_race", selected_model, "race")
    mod_est_plot_server("est_age", selected_model, "age")
    mod_est_plot_server("est_edu", selected_model, "edu")
    
    
    # --------------------------------------------------------------------------
    # Overall plot for Raw vs MRP
    # --------------------------------------------------------------------------
    output$est_overall <- renderPlot({
      req(selected_model())
      if(global$data_format %in% c("temporal_covid", "temporal_other")) {
        plot_prev(
          selected_model()$mrp$input,
          selected_model()$plotdata$dates,
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
          plot_support(selected_model()$mrp$input)
      }
    }, height = function() GLOBAL$ui$plot_height)
    
    # --------------------------------------------------------------------------
    # Render UI dynamically based on the user's selection.
    # --------------------------------------------------------------------------
    output$result_output <- renderUI({
      req(input$result_category, input$subgroup_select)

      result_category <- isolate(input$result_category)
      subgroup_select <- isolate(input$subgroup_select)

      if (result_category == "Raw vs MRP") {
        plotOutput(ns("est_overall"), height = GLOBAL$ui$plot_height)
      } else if (result_category == "By Subgroup") {
        switch(subgroup_select,
          "Sex" = mod_est_plot_ui(ns("est_sex")),
          "Race" = mod_est_plot_ui(ns("est_race")),
          "Age" = mod_est_plot_ui(ns("est_age")),
          "Education" = mod_est_plot_ui(ns("est_edu")),
          "Geography" = tagList(
            # Show message if geographic data is not available.
            conditionalPanel(
              condition = sprintf("output['%s']", ns("no_geo")),
              tags$p("Map unavailable", class = "alt_text")
            ),
            conditionalPanel(
              condition = sprintf("output['%s'] == 'temporal_other' || output['%s'] == 'static_other'", ns("data_format"), ns("data_format")),
              selectInput(
                inputId = ns("geo_scale_select"),
                label = "Select geographic scale",
                choices = NULL
              )
            ),
            plotly::plotlyOutput(ns("est_geo_map"), height = "700px"),
            conditionalPanel(
              condition = sprintf("output['%s'] == 'temporal_covid' || output['%s'] == 'temporal_other'", ns("data_format"), ns("data_format")),
              selectizeInput(
                inputId = ns("geo_select"),
                label = "Select one or more counties (max = 5)",
                choices = NULL,
                multiple = TRUE,
                options = list(maxItems = 5)
              )
            ),
            plotOutput(ns("est_geo_plot"))
          )
        )
      }
    })
    
    # --------------------------------------------------------------------------
    # Plot for geographic map (MRP estimates)
    # --------------------------------------------------------------------------
    output$est_geo_map <- plotly::renderPlotly({
      req(selected_model())

      geo <- selected_scale()
      selected_model()$est[[geo]] |>
        prep_est(
          fips_codes = global$extdata$fips[[geo]],
          geo = geo,
          dates = if("dates" %in% names(selected_model()$plotdata)) selected_model()$plotdata$dates else NULL
        ) |>
        mutate(value = est) |>
        choro_map(
          selected_model()$plotdata$geojson[[geo]],
          map_title = "MRP Estimate of Positive Response Rate",
          colorbar_title = "Positive\nResponse\nRate",
          geo = geo
        ) |>
        suppressWarnings()
    })
    
    # --------------------------------------------------------------------------
    # Plot for geographic subgroup estimates (subset plots)
    # --------------------------------------------------------------------------
    output$est_geo_plot <- renderPlot({
      req(selected_model())

      geo <- selected_scale()
      fips_df <- global$extdata$fips[[geo]] |> fips_upper()

      plof_df <- selected_model()$est[[geo]] |>
        rename("fips" = "factor") |>
        left_join(fips_df, by = "fips") |>
        rename("factor" = geo)
      if(global$data_format %in% c("temporal_covid", "temporal_other")) {
        plof_df |>
          filter(factor %in% input$geo_select) |>
          plot_est_temporal(selected_model()$plotdata$dates)
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
    
    # --------------------------------------------------------------------------
    # Update model selection when user navigates to the Results page.
    # --------------------------------------------------------------------------
    observeEvent(global$input$navbar_analyze, {
      if(global$input$navbar_analyze == "nav_analyze_result") {
        if(is.null(global$mrp)) {
          showModal(modalDialog(
            title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
            "Invalid input data.",
            footer = actionButton(inputId = ns("to_upload"), label = "Go to data upload page")
          ), session = global$session)
        }
        
        # Omit pre-poststratification models.
        global$poststratified_models <- purrr::keep(global$models, ~ !is.null(.x$fit$pstrat))
        
        if(length(global$poststratified_models) == 0) {
          showModal(modalDialog(
            title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
            "No model with poststratified estimates found. Make sure to run poststratification after fitting models.",
            footer = actionButton(inputId = ns("to_model"), label = "Go to model page")
          ), session = global$session)
        } else {
          choices <- names(global$poststratified_models)
          selected <- if(model_select_buffer() %in% choices) model_select_buffer() else choices[1]
          updateSelectInput(session, inputId = "model_select", choices = choices, selected = selected)
        }
      }
    })
    
    # --------------------------------------------------------------------------
    # Update subgroup and geographic scale selection when model changes.
    # --------------------------------------------------------------------------
    observeEvent(selected_model(), {
      req(selected_model())

      if(input$model_select != "") {
        # Update the subgroup select options.
        choices <- switch(selected_model()$data_format,
          "static_poll" = c("Sex", "Race", "Age", "Education", "Geography"),
          c("Sex", "Race", "Age", "Geography")
        )
        updateSelectInput(session, inputId = "subgroup_select", choices = choices, selected = choices[1])

        # Update the geographic scale select options.
        choices <- intersect(names(selected_model()$est), GLOBAL$vars$geo)
        updateSelectInput(session, inputId = "geo_scale_select", choices = choices, selected = choices[1])

      }
    })
    

    # ---------------------------------------------------------------------------
    # Update county selection when geographic scale changes.
    # ---------------------------------------------------------------------------
    observeEvent(selected_scale(), {
      req(selected_scale())

      geo <- selected_scale()
      if(geo != "") {
        fips_df <- global$extdata$fips[[geo]] |>
          filter(fips %in% selected_model()$mrp$levels[[geo]]) |>
          fips_upper()
        choices <- sort(fips_df[[geo]])
        updateSelectInput(session, inputId = "geo_select", choices = choices, selected = choices[1])
      }
    })
    
    # --------------------------------------------------------------------------
    # Navigation modal events for data/model errors.
    # --------------------------------------------------------------------------
    observeEvent(global$input$navbar_analyze, {
      if(global$input$navbar_analyze == "nav_analyze_result" &&
         is.null(global$mrp)) {
        showModal(modalDialog(
          title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
          "Invalid input data.",
          footer = actionButton(inputId = ns("to_upload"), label = "Go to data upload page")
        ), session = global$session)
      }
    })
    
    observeEvent(input$to_upload, {
      updateTabsetPanel(global$session, inputId = "navbar_analyze", selected = "nav_analyze_upload")
      removeModal(global$session)
    })
    
    observeEvent(input$to_model, {
      updateTabsetPanel(global$session, inputId = "navbar_analyze", selected = "nav_analyze_model")
      removeModal(global$session)
    })
    
  })
}
