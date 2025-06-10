#' Results Visualization Module UI Function
#'
#' @description Creates the user interface for visualizing MRP estimation results.
#' Provides a sidebar layout with dynamic selection controls for model choice,
#' result type (overall vs subgroup), and specific visualization options.
#' Supports both time-varying and cross-sectional data visualization with geographic
#' mapping and demographic subgroup analysis.
#'
#' @param id Character string. The module's namespace identifier.
#'
#' @return A \code{bslib::layout_sidebar} containing the results interface with:
#' \itemize{
#'   \item Sidebar with model selection and result type controls
#'   \item Conditional panels for subgroup and geographic options
#'   \item Main panel with dynamic plot output based on selections
#' }
#'
#' @noRd
#'
#' @importFrom shiny NS tagList conditionalPanel selectInput selectizeInput uiOutput plotOutput
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

#' Results Visualization Module Server Function
#'
#' @description Server logic for the results visualization module. Manages model
#' selection, generates dynamic UI based on user choices, and renders various
#' types of plots including overall estimates, demographic subgroup comparisons,
#' and geographic visualizations. Handles both time-varying and cross-sectional
#' data formats with appropriate plot types.
#'
#' @param id Character string. The module's namespace identifier.
#' @param global Reactive values object containing global application state
#'
#' @return Server function for the results module. Creates reactive values for
#' model selection, renders dynamic UI components, and generates plots for
#' MRP estimation results visualization.
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactive req isolate renderUI renderPlot observeEvent updateSelectInput updateTabsetPanel showModal modalDialog removeModal actionButton
#' @importFrom dplyr mutate select filter
#' @importFrom purrr keep map_chr
#' @importFrom shinyjs reset
#' @importFrom tools toTitleCase
#' @importFrom dplyr mutate select filter
#' @importFrom rlang .data
#' @importFrom stats setNames
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
      if(global$metadata$is_timevar) {
        plot_prev(
          selected_model()$mrp$input,
          selected_model()$plot_data$dates,
          selected_model()$est$overall,
          show_caption = TRUE
        )
      } else {
        selected_model()$est$overall %>%
          mutate(
            data = "Estimate",
            lower = .data$est - .data$std,
            median = .data$est,
            upper = .data$est + .data$std
          ) %>%
          select(.data$data, .data$lower, .data$median, .data$upper) %>%
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
          models <- purrr::keep(global$models, ~ !is.null(.x$fit$pstrat))
          global$poststratified_models <- models
          
          if(length(models) == 0) {
            showModal(modalDialog(
              title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
              "No model with poststratified estimates found. Make sure to run poststratification after fitting models.",
              footer = actionButton(inputId = ns("to_model"), label = "Go to model page")
            ), session = global$session)
          } else {
            model_names <- purrr::map_chr(models, ~ .x$name)
            model_ids <- purrr::map_chr(models, ~ .x$IDs$main)
            choices <- stats::setNames(model_ids, model_names)
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
      if(selected_model()$metadata$special_case != "poll") {
        choices <- choices[!choices =="edu"]
      }
      if(is.null(selected_model()$link_data$link_geo)) {
        choices <- choices[!choices == "geo"]
      }
      updateSelectInput(session, inputId = "subgroup_select", choices = choices)

      # Update the geographic scale select options.
      choices <- intersect(names(selected_model()$est), GLOBAL$vars$geo)
      choices <- stats::setNames(choices, tools::toTitleCase(choices))
      updateSelectInput(session, inputId = "geo_scale_select", choices = choices)
    })
  

    # --------------------------------------------------------------------------
    # Update county selection when geographic scale changes.
    # --------------------------------------------------------------------------
    observeEvent(input$geo_scale_select, {
      req(input$geo_scale_select, input$geo_view_select)
      
      geo <- isolate(input$geo_scale_select)
      fips_df <- global$extdata$fips[[geo]] %>%
        filter(.data$fips %in% selected_model()$mrp$levels[[geo]]) %>%
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

    #---------------------------------------------------------------------------
    # Reset selection when user switch version
    # --------------------------------------------------------------------------
    observeEvent(
      eventExpr = list(
        global$data,
        global$metadata,
        global$link_data
      ),
      handlerExpr = {
        shinyjs::reset("model_select")
        shinyjs::reset("result_category")
        shinyjs::reset("subgroup_select")
        shinyjs::reset("geo_scale_select")
        shinyjs::reset("geo_view_select")
        shinyjs::reset("geo_unit_select")
      }
    )
    
  })
}
