#' Data Upload Module UI Function
#'
#' @description Creates the user interface for data upload and preprocessing in the MRP
#' application. Provides a sidebar layout with accordion panels for sample data upload
#' and poststratification data configuration. Supports both individual-level and
#' aggregated data formats, with options to link to ACS data or upload custom
#' poststratification data. Includes data preview and validation feedback.
#'
#' @param id Character string. The module's namespace identifier.
#'
#' @return A `bslib::layout_sidebar` containing the upload interface with:
#' \itemize{
#'   \item Sidebar with accordion panels for sample and poststratification data
#'   \item File upload inputs with format toggles
#'   \item Example data buttons and validation feedback
#'   \item Main panel with data table preview and download options
#' }
#'
#' @noRd
#' @keywords internal
#'
#' @importFrom shiny NS tagList conditionalPanel fileInput actionButton downloadButton uiOutput selectizeInput actionLink tags
mod_analyze_upload_ui <- function(id) {
  ns <- NS(id)
  
  bslib::layout_sidebar(
    #---------------------------------------------------------------------------
    # Sidebar
    #---------------------------------------------------------------------------
    sidebar = sidebar(
      width = 375,

      bslib::accordion(
        id = ns("accordion"),
        multiple = FALSE,
        bslib::accordion_panel(
          title = "Sample",
          value = "sample",
          conditionalPanel(
            condition = "output.family != 'normal' || output.is_timevar",
            tags$div(
              actionButton(
                inputId = ns("sample_spec_popover_btn"),
                label = "Data Specification",
                icon = icon("chevron-down"),
                class = "btn btn-sm btn-secondary"
              )
            ),
            tags$div(id = ns("sample_spec_popover"),
              bslib::card(class = "mt-2 mb-0",
                bslib::card_body(
                  conditionalPanel(
                    condition = "output.family != 'normal'",
                    shinyWidgets::radioGroupButtons(
                      inputId = ns("toggle_sample"),
                      label = "Data is aggregated?",
                      choices = c( "Yes" = "agg", "No" = "indiv"),
                      selected = "agg",
                      justified = TRUE,
                      size = "sm"
                    )
                  ),
                  conditionalPanel(
                    condition = "output.is_timevar",
                    shinyWidgets::radioGroupButtons(
                      inputId = ns("freq_select"),
                      label = "Group dates by",
                      choices = c( "Week" = "week", "Month" = "month", "Year" = "year"),
                      selected = "week",
                      justified = TRUE,
                      size = "sm"
                    )
                  )
                )
              )
            )
          ),
          tags$div(class = "mt-2",
            fileInput(
              inputId = ns("sample_upload"),
              label = NULL,
              accept = .const()$ui$format$data
            )
          ),
          uiOutput(ns("sample_feedback")),
          p(class = "mt-0 small",
            "For", tags$u("input data requirements,"), "open the",
            actionLink(ns("show_upload_guide"), label = "User Guide."),
            "For a detailed description of the preprocessing procedure and examples of preprocessing code, go to the",
            actionLink(ns("to_preprocess"), label = "Preprocessing"), "page."
          ),
          # Example data label
          tags$div(class = "mt-4",
            conditionalPanel(
              condition = "output.special_case == 'covid'",
              tags$p(tags$u("Example"), ": COVID-19 hospital test records")
            ),
            conditionalPanel(
              condition = "output.special_case == 'poll'",
              tags$p(tags$u("Example"), ": 2018 Cooperative Congressional Election Study")
            ),
            conditionalPanel(
              condition = "output.special_case === null",
              tags$p(tags$u("Example data"))
            ),
            conditionalPanel(
              condition = "output.family != 'normal'",
              tags$div(
                class = "d-flex gap-2 mb-3",
                actionButton(ns("use_indiv_example"), "Individual-level", icon("table")),
                actionButton(ns("use_agg_example"), "Aggregated", icon("table"))
              )
            ),
            conditionalPanel(
              condition = "output.family == 'normal'",
              actionButton(ns("use_indiv_example"), "Individual-level", icon("table"), class = "w-100")
            ),

          )
        ),
        bslib::accordion_panel(
          title = "Poststratification Data",
          value = "pstrat",
          conditionalPanel(
            condition = sprintf("!output['%s']", ns("data_preprocessed")),
            bslib::card(
              class = "bg-warning mb-3",  # yellow background & border
              bslib::card_body(
                tags$div(
                  style = "display: flex; align-items: center;",
                  shiny::icon("exclamation-triangle", class = "me-2"),  # Bootstrap margin-end
                  tags$span("Please upload sample data first", class = "fw-semibold")
                )
              )
            )
          ),
          conditionalPanel(
            condition = "output.special_case !== null",
            tags$p("Provide information for linking the input data to the ACS data.",
              class = "small"
            ),
          ),
          conditionalPanel(
            condition = "output.special_case === null",
            tags$p("Provide information for linking the input data to the ACS data or upload poststratification data.",
              class = "small"
            ),
          ),
          tags$div(class = "mt-2",
            actionButton(
              inputId = ns("link_acs_popover_btn"),
              label = "Link to ACS Data",
              icon = icon("chevron-down"),
              class = "btn btn-sm btn-secondary"
            ),
            tags$div(id = ns("link_acs_popover"),
              bslib::card(class = "mt-2",
                bslib::card_body(
                  selectizeInput(ns("link_geo"), label = "Select geography scale for poststratification", choices = NULL),
                  selectizeInput(ns("acs_year"), label = "Select 5-year ACS data to link to", choices = NULL, options = list(dropdownParent = "body")),
                  actionButton(ns("link_acs"), label = "Link", class = "btn w-100") 
                )
              )
            )
          ),
          conditionalPanel(
            condition = "output.special_case === null",
            tags$div(class = "mt-2",
              actionButton(
                inputId = ns("pstrat_upload_popover_btn"),
                label =  "Upload poststratification data",
                icon = icon("chevron-down"),
                class = "btn btn-sm btn-secondary"
              ),
              tags$div(id = ns("pstrat_upload_popover"),
                bslib::card(class = "mt-2",
                  bslib::card_body(class = "gap-3",
                    tags$p(tags$strong("Upload individual-level or aggregated poststratification data")),
                    shinyWidgets::radioGroupButtons(
                      inputId = ns("toggle_pstrat"),
                      label = NULL,
                      choices = c("Individual-level" = "indiv", "Aggregated" = "agg"),
                      selected = "agg",
                      justified = TRUE,
                      size = "sm"
                    ),
                    fileInput(
                      inputId = ns("pstrat_upload"),
                      label = NULL,
                      accept = .const()$ui$format$data
                    ),
                    uiOutput(ns("pstrat_feedback")),
                    tags$p(class = "mt-0", tags$u("Example data")),
                    downloadButton(
                      outputId = ns("save_pstrat_example"),
                      label = "Aggregated",
                      class = "btn w-100"
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    #---------------------------------------------------------------------------
    # Main Window
    #---------------------------------------------------------------------------
    conditionalPanel(
      condition = sprintf("output['%s']", ns("file_uploaded")),
      bslib::layout_columns(
        col_widths = c(4, 8),
        conditionalPanel(
          condition = sprintf("output['%s'] == true", ns("data_preprocessed")),
          div(class = "d-flex align-items-start gap-2",
            # Toggle button for table view
            shinyWidgets::radioGroupButtons(
              inputId = ns("toggle_table"),
              label = NULL,
              size = "sm",
              choices = c("Raw" = "raw", "Preprocessed" = "prep")
            ),
            # Download button for preprocessed data 
            conditionalPanel(
              condition = sprintf("input['%s'] == 'prep'", ns("toggle_table")),
              downloadButton(
                outputId = ns("download_preprocessed"),
                label = NULL,
                class = "btn btn-secondary btn-sm"
              )
            )
          )
        ),
        # Info text
        tags$p(
          sprintf("*The preview only includes the first %d rows of the data", 
                 .const()$ui$preview_size), 
          class = "small text-muted text-end"
        )
      ),
      DT::dataTableOutput(outputId = ns("table"))
    )
  )
}


#' Data Upload Module Server Function
#'
#' @description Server logic for the data upload module. Handles file uploads,
#' data preprocessing, validation, and preparation for MRP analysis. Manages
#' both sample data and poststratification data workflows, including linking
#' to ACS data and custom data uploads. Provides real-time feedback and
#' error handling throughout the upload process.
#'
#' @param id Character string. The module's namespace identifier.
#' @param global Reactive values object containing global application state
#'
#' @return Server function for the upload module. Creates reactive values for
#' data storage and validation, handles file processing, and updates global
#' state with preprocessed data ready for analysis.
#'
#' @noRd
#' @keywords internal
#'
#' @importFrom shiny moduleServer reactiveVal reactive outputOptions observeEvent updateSelectInput updateActionButton renderUI req
#' @importFrom dplyr mutate filter
#' @importFrom dplyr mutate filter
#' @importFrom rlang .data
#' @importFrom shinyjs reset toggle show hide delay
mod_analyze_upload_server <- function(id, global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    raw_sample_rv <- reactiveVal()
    raw_pstrat_rv <- reactiveVal()    

    #---------------------------------------------------------------------------
    # Reactive outputs for conditional panels
    #---------------------------------------------------------------------------
    output$file_uploaded <- reactive(!is.null(raw_sample_rv()))
    outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)

    output$data_preprocessed <- reactive({
      req(global$workflow)
      global$prep_ver
      global$workflow$check_prep_data_exists() 
    })
    outputOptions(output, "data_preprocessed", suspendWhenHidden = FALSE)


    # --------------------------------------------------------------------------
    # Reset everything when new workflow is created
    # --------------------------------------------------------------------------
    observeEvent(global$workflow, {
      raw_sample_rv(NULL)
      raw_pstrat_rv(NULL)

      .reset_upload_pg()
    })
    
    # --------------------------------------------------------------------------
    # Popover event handlers
    # --------------------------------------------------------------------------
    observeEvent(
      eventExpr = list(
        input$pstrat_upload_popover_btn,
        input$link_acs_popover_btn
      ),
      handlerExpr = {
        shinyjs::toggle(id = "pstrat_upload_popover")
        shinyjs::toggle(id = "link_acs_popover")
      }
    )

    observeEvent(input$sample_spec_popover_btn, {
      shinyjs::toggle(id = "sample_spec_popover")
    })


    # --------------------------------------------------------------------------
    # Show feedback about input data
    # --------------------------------------------------------------------------
    output$sample_feedback <- renderUI({
      req(raw_sample_rv())

      if (global$workflow$check_prep_data_exists()) {
        tags$div(
          tagList(icon("circle-check", "fa"), "Success"),
          tags$p("All requirements are met. You may proceed to the Poststratification Data section'.", class = "small")
        )
      } else {
        tags$div(
          tagList(icon("circle-xmark", "fa"), "Error"),
          tags$p("Input data does not meet all requirements. Please check the user guide for data requirements.", class = "small"),
        )
      }
    })

    
    # Table output renderer
    output$table <- DT::renderDT({
      req(raw_sample_rv())
      
      .preview_table(
        if(identical(input$toggle_table, "prep")) {
          global$workflow$preprocessed_data()
        } else {
          raw_sample_rv()
        }
      )
    })

    # Preprocessed data download handler
    output$download_preprocessed <- downloadHandler(
      filename = function() {
        paste0("preprocessed_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(global$workflow$check_prep_data_exists())
        readr::write_csv(global$workflow$preprocessed_data(), file)
      }
    )

    # Handle sample data upload
    observeEvent(input$sample_upload, {
      .show_waiter("wait")

      # Read and store the raw sample data
      .read_data(input$sample_upload$datapath) %>%
        raw_sample_rv()

      # Overwrite default input values
      is_aggregated <- input$toggle_sample == "agg"
      if (global$metadata$family != "normal") {
        is_aggregated <- FALSE
      }

      time_freq <- input$freq_select
      if (!global$metadata$is_timevar) {
        time_freq <- NULL
      }

      global$workflow$preprocess(
        raw_sample_rv(),
        is_timevar = global$metadata$is_timevar,
        is_aggregated = is_aggregated,
        special_case = global$metadata$special_case,
        family = global$metadata$family,
        time_freq = time_freq
      )

      global$trigger_prep_change()

      waiter::waiter_hide()
    })
    
    # Use individual-level example data
    observeEvent(input$use_indiv_example, {
      .show_waiter("wait")

      .create_example_filename(global$metadata, suffix = "raw") %>%
        .fetch_data(subdir = "example/data") %>%
        raw_sample_rv()

      workflow <- global$workflow

      workflow$preprocess(
        raw_sample_rv(),
        is_timevar = global$metadata$is_timevar,
        is_aggregated = FALSE,
        special_case = global$metadata$special_case,
        family = global$metadata$family,
        time_freq = if(global$metadata$is_timevar) "week" else NULL
      )

      global$trigger_prep_change()

      waiter::waiter_hide()
    })

    # Use aggregated example data
    observeEvent(input$use_agg_example, {
      .show_waiter("wait")

      .create_example_filename(global$metadata, suffix = "prep") %>%
        .fetch_data(subdir = "example/data") %>%
        raw_sample_rv()


      global$workflow$preprocess(
        raw_sample_rv(),
        is_timevar = global$metadata$is_timevar,
        is_aggregated = TRUE,
        special_case = global$metadata$special_case,
        family = global$metadata$family,
        time_freq = NULL
      )

      global$trigger_prep_change()

      waiter::waiter_hide()
    })


    #---------------------------------------------------------------------------
    # Update select input for linking to ACS
    #---------------------------------------------------------------------------
    observeEvent(global$prep_ver, {
      req(global$workflow)

      choices <- .link_select(
        data = global$workflow$preprocessed_data(),
        use_case = global$metadata$special_case
      )

      updateSelectInput(session,
        inputId = "link_geo",
        choices = choices$link_geos
      )

      updateSelectInput(session,
        inputId = "acs_year",
        choices = choices$acs_years
      )

      # Update the accordion to show the poststratification data panel
      bslib::accordion_panel_open(
        id = "accordion",
        values = "pstrat",
        session = session
      )

      waiter::waiter_hide()
    })
    

    #---------------------------------------------------------------------------
    # Create poststratification data from ACS data
    #---------------------------------------------------------------------------
    observeEvent(input$link_acs, {
      req(global$workflow$check_prep_data_exists())

      .start_busy(
        session = session,
        id = "link_acs",
        label = "Linking..."
      )

      # delay the execution to allow the UI to update
      shinyjs::delay(10, {

        global$workflow$link_acs(
          link_geo = if(input$link_geo %in% .const()$vars$geo) input$link_geo else NULL,
          acs_year = strsplit(input$acs_year, "-")[[1]][2] %>% as.numeric()
        )

        success <- global$workflow$check_mrp_exists()

        if (success) {
          global$trigger_mrp_change()
        }

        .stop_busy(
          session = session,
          id = "link_acs",
          label = if(success) "Linking complete" else "Linking failed",
          success = success
        )
      })
    })

    #----------------------------------------------------------------------------
    # Show feedback about poststratification data
    #----------------------------------------------------------------------------
    output$pstrat_feedback <- renderUI({
      req(raw_pstrat_rv())

      if (global$workflow$check_mrp_exists()) {
        tags$div(
          tagList(icon("circle-check", "fa"), "Success"),
          tags$p("All requirements are met. You may proceed to the next page.", class = "small")
        )
      } else {
        tags$div(
          tagList(icon("circle-xmark", "fa"), "Error"),
          tags$p("Poststratification data does not meet all requirements. Please check the user guide for data requirements.", class = "small")
        )
      }
    })

    #----------------------------------------------------------------------------
    # Handle poststratification data upload
    #----------------------------------------------------------------------------
    observeEvent(input$pstrat_upload, {
      .show_waiter("wait")

      .read_data(input$pstrat_upload$datapath) %>%
        raw_pstrat_rv()

      global$workflow$load_pstrat(
        raw_pstrat_rv(),
        is_aggregated = input$toggle_pstrat == "agg"
      )

      if (global$workflow$check_mrp_exists()) {
        global$trigger_mrp_change()
      }

      waiter::waiter_hide()
    })


    #----------------------------------------------------------------------------
    # Reset link button
    #----------------------------------------------------------------------------
    observeEvent(
      eventExpr = list(
        global$workflow,
        global$prep_ver,
        input$link_geo,
        input$acs_year
      ),
      handlerExpr = {
        updateActionButton(
          session = session,
          inputId = "link_acs",
          label = "Link",
          icon = character(0)
        )
      }
    )


    # show user's guide
    observeEvent(input$show_upload_guide, {
      .show_guide("upload")
    })


    # navigate to Learn > Preprocess
    observeEvent(input$to_preprocess, {
      bslib::nav_select(
        id = "navbar",
        selected = "nav_learn_preprocess",
        session = global$session
      )
    })

    # Example poststratification data download handler
    output$save_pstrat_example <- downloadHandler(
      filename = function() {
        paste0("pstrat_example_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        # Read the example file and write it to the download location
        .fetch_data("pstrat.csv", subdir = "example/data") %>%
          readr::write_csv(file)
      }
    )
  })
}
