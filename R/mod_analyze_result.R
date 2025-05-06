#' analyze_result UI Function
#'
#' @description A shiny Module that uses a sidebar layout with dynamic selectInputs
#'              to choose which result (plot) to display.
#'
#' @param id A unique id for the module.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analyze_result_ui <- function(id){
  ns <- NS(id)
  
  # Use layout_sidebar from bslib for a modern sidebar layout.
  layout_sidebar(
    sidebar = sidebar(
      width = 375,
      # Select a model from available models.
      selectInput(
        inputId = ns("model_select"),
        label = "1. Select a model",
        choices = NULL
      ),
      # Select the result category.
      selectInput(
        inputId = ns("result_category"),
        label = "2. Select result type",
        choices = c("Raw vs MRP" = "overall", "By Subgroup" = "subgroup")
      ),
      # If user selects "By Subgroup", show subgroup choice.
      conditionalPanel(
        condition = sprintf("input['%s'] == 'subgroup'", ns("result_category")),
        selectInput(
          inputId = ns("subgroup_select"),
          label = "3. Select subgroup",
          choices = NULL
        ),
        # If user selects "Geography", show geographic scale choice.
        conditionalPanel(
          condition = sprintf("input['%s'] == 'geo'", ns("subgroup_select")),
          bslib::card(
            bslib::card_header("Options for Geographic Subgroup"),
            bslib::card_body(
              selectizeInput(
                inputId = ns("geo_scale_select"),
                label = "Select geographic scale",
                choices = NULL
              ),
              selectizeInput(
                inputId = ns("geo_view_select"),
                label = "Select plot type",
                choices = c("Map" = "map", "Line/Scatter Plot" = "line_scatter"),
                options = list(dropdownParent = "body")
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'line_scatter'", ns("geo_view_select")),
                selectizeInput(
                  inputId = ns("geo_unit_select"),
                  label = "Select one or more",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(dropdownParent = "body")
                )
              )
            )
          )
        )
      ),
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
    mod_est_map_server(
      "est_geo",
      selected_model,
      global,
      reactive(tolower(input$geo_scale_select)),
      reactive(input$geo_view_select),
      reactive(input$geo_unit_select)
    )
    
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

      if (result_category == "overall") {
        plotOutput(ns("est_overall"), height = GLOBAL$ui$plot_height)
      } else if (result_category == "subgroup") {
        switch(subgroup_select,
          "sex" = mod_est_plot_ui(ns("est_sex")),
          "race" = mod_est_plot_ui(ns("est_race")),
          "age" = mod_est_plot_ui(ns("est_age")),
          "edu" = mod_est_plot_ui(ns("est_edu")),
          "geo" = mod_est_map_ui(ns("est_geo"))
        )
      }
    })
    
    # --------------------------------------------------------------------------
    # Update model selection when user navigates to the result tab.
    # --------------------------------------------------------------------------
    observeEvent(global$input$navbar_analyze, {
      if(global$input$navbar_analyze == "nav_analyze_result") {        
        if (!is.null(global$mrp)) {
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
      }
    })
    
    # --------------------------------------------------------------------------
    # Update subgroup and geographic scale selection when model changes.
    # --------------------------------------------------------------------------
    observeEvent(selected_model(), {
      req(selected_model(), input$model_select)

      # Update the subgroup select options.
      choices <- GLOBAL$ui$plot_selection$subgroup
      if(selected_model()$data_format != "static_poll") {
        choices <- choices[!choices %in% "edu"]
      }
      if(is.null(selected_model()$link_data$link_geo)) {
        choices <- choices[!choices %in% "geo"]
      }
      updateSelectInput(session, inputId = "subgroup_select", choices = choices)

      # Update the geographic scale select options.
      choices <- intersect(names(selected_model()$est), GLOBAL$vars$geo)
      choices <- setNames(choices, tools::toTitleCase(choices))
      updateSelectInput(session, inputId = "geo_scale_select", choices = choices)
    })
  

    # --------------------------------------------------------------------------
    # Update county selection when geographic scale changes.
    # --------------------------------------------------------------------------
    observeEvent(input$geo_scale_select, {
      req(input$geo_scale_select, input$geo_view_select)
      
      geo <- isolate(input$geo_scale_select)
      fips_df <- global$extdata$fips[[geo]] |>
        filter(fips %in% selected_model()$mrp$levels[[geo]]) |>
        fips_upper()
      choices <- sort(fips_df[[geo]])
      updateSelectInput(session, inputId = "geo_unit_select", choices = choices, selected = choices[1])

    })
    
    
    # --------------------------------------------------------------------------
    # Navigation modal events for data/model errors.
    # --------------------------------------------------------------------------    
    observeEvent(input$to_model, {
      updateTabsetPanel(global$session, inputId = "navbar_analyze", selected = "nav_analyze_model")
      removeModal(global$session)
    })
    
  })
}
