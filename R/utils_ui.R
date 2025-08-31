#' Create loading UI elements
#'
#' @description Creates different types of loading spinners and messages for
#' various application processes including model fitting, poststratification,
#' diagnostics, and setup operations.
#'
#' @param type Character. Type of loading UI to create. Options include:
#'   "fit" (model fitting), "pstrat" (poststratification), "loo" (diagnostics),
#'   "setup" (CmdStan installation), "wait" (general waiting), or "" (NULL)
#'
#' @return A tagList containing spinner and message elements, or NULL for empty type
#'
#' @importFrom shiny tagList tags
#'
#' @noRd
#' @keywords internal
.waiter_ui <- function(loading_type) {
  text_style <- "color: black; margin-top: 10px;"

  checkmate::assert_choice(
    loading_type, 
    .const()$ui$loading_types, 
    null.ok = TRUE
  )

  loading_type <- .replace_null(loading_type, "")

  switch(loading_type,
    "fit" = tagList(
      waiter::spin_loaders(2, color = "black"),
      tags$h4("Fitting model...", style = text_style)
    ),
    "pstrat" = tagList(
      waiter::spin_loaders(2, color = "black"),
      tags$h4("Running poststratification...", style = text_style)
    ),
    "loo" = tagList(
      waiter::spin_loaders(2, color = "black"),
      tags$h4("Running diagnostics...", style = text_style)
    ),
    "setup" = tagList(
      waiter::spin_loaders(15, color = "black"),
      tags$h4("Installing CmdStan...", style = text_style)
    ),
    "init" = tagList(
      waiter::spin_loaders(15, color = "black"),
      tags$h4("Initializing...", style = text_style)
    ),
    "wait" = tagList(
      waiter::spin_loaders(15, color = "black"),
      tags$h4("Please wait...", style = text_style)
    ),
    NULL
  )
}

#' Create user guide accordion
#'
#' @description Creates a comprehensive user guide accordion interface with
#' multiple panels covering workflow, data upload, model specification, and
#' model fitting instructions for the MRP application.
#'
#' @param open Character. Which accordion panel should be open by default.
#'   Options: "workflow", "upload", "model_spec", "model_fit"
#'
#' @return A bslib accordion object containing detailed user guide content
#'
#' @importFrom shiny tags withMathJax
#'
#' @noRd
#' @keywords internal
.create_guide <- function(open) {

  checkmate::assert_choice(
    open, 
    .const()$ui$guide_sections,
    null.ok = TRUE
  )

  open <- .replace_null(open, .const()$ui$guide_sections[1])

  bslib::accordion(
    id = "guide_accordion",
    open = open,
    class = "p-0 m-0",
    multiple = FALSE,
    # Workflow panel
    bslib::accordion_panel(
      title = tags$h4("Workflow"),
      value = "workflow",
      tags$p("The interface provides different modules tailored to various use cases, categorized into ", tags$b("time-varying"), " and ", tags$b("cross-sectional"), " analyses with ", tags$b("binary"), " or ", tags$b("continuous"), " outcomes. Each category supports specific applications, such as COVID-19 transmission tracking and public opinion polling, along with broader use cases. Regardless of the module, the analytic workflow is consistent and follows these core steps:"),
      tags$ul(
        tags$li(tags$b("Data Cleaning:"), " Preprocess and display the uploaded data."),
        tags$li(tags$b("Descriptive Statistics:"), " Visualize key summary statistics."),
        tags$li(tags$b("Model Building:"), " Specify and fit models with customizable predictors and varying or fixed effects. The application provides diagnostics to aid model evaluation and selection."),
        tags$li(tags$b("Result Visualization:"), " Generate plots to present estimates for the overall population and across demographic or geographic subgroups.")
      )
    ),

    # Uploading Data panel
    bslib::accordion_panel(
      title = tags$h4("Uploading Data"),
      value = "upload",
      tags$p("The MRP interface requires two main data components:"),
      tags$ul(
        tags$li(tags$b("Sample data:"), " Your analysis sample, which must include the outcome of interest and relevant predictors, such as COVID-19 test records or survey responses."),
        tags$li(tags$b("Poststratification data:"), " A table describing the size of population groups defined by demographic and geographic characteristics.")
      ),
      tags$p("Uploading poststratification data is optional because the interface can link sample data to the American Community Survey (ACS) using available geographic identifiers. Geographic linking is supported in all modules, and users may also upload custom poststratification data in both general time-varying and cross-sectional modules."),
      
      tags$h5("Individual-level vs. aggregated data", class = "mt-4"),
      tags$p("Data preprocessing accepts either of these formats:"),
      tags$ul(
        tags$li(tags$b("Individual-level:"), " Each row contains data for a single person."),
        tags$li(tags$b("Aggregated:"), " Each row contains data for a group (e.g., White males aged 18-30 in Michigan), summarizing demographic/geographic factors, totals, and outcome summaries.")
      ),
      tags$p("For ", tags$b("continuous outcomes"), ", only individual-level data are supported."),
      tags$p("For ", tags$b("binary outcomes"), ", the aggregated format is preferred for computational efficiency; individual-level data are aggregated automatically upon upload."),
      tags$p("Other data requirements depend on format, primarily regarding outcome measures."),
      
      tags$h5("Required columns and naming conventions", class = "mt-4"),
      tags$p("The code expects columns with specific names and values (not case-sensitive):"),
      tags$ul(
        tags$li("Sex: male, female"),
        tags$li("Race: Black, White, other"),
        tags$li("Age"),
        tags$li("Edu (education attainment): below high school (no hs), high school (hs), some college, 4-year college, post-grad"),
        tags$li("ZIP code", tags$sup("1")),
        tags$li("County", tags$sup("1")),
        tags$li("State", tags$sup("1")),
        tags$li("Time indices (time)", tags$sup("2")),
        tags$li("Date"),
        tags$li("Continuous outcome measure (outcome)", tags$sup("3")),
        tags$li("Positive response indicator or number of positive responses (positive)", tags$sup("4")),
        tags$li("Cross-tabulation cell counts (total)", tags$sup("4")),
        tags$li("Survey weights (weight)", tags$sup("5"))
      ),
      tags$p(tags$sup("1"), " Geographical columns are optional for general use. The app automatically identifies the smallest available geographic scale and infers higher levels.", class = "small text-muted"),
      tags$p(tags$sup("2"), " For individual-level data, dates are automatically converted to time indices but can be provided explicitly. Aggregated data must include a 'time' column with time indices. Optionally include a 'date' column (first day of each period) for visualization. The interface uses time-invariant poststratification data.", class = "small text-muted"),
      tags$p(tags$sup("3"), " For continuous outcomes, name your outcome column 'outcome'.", class = "small text-muted"),
      tags$p(tags$sup("4"), " For binary outcomes, the column in individual-level data must be 'positive'. For aggregated data, use 'total' (number in cell) and 'positive' (number positive in cell).", class = "small text-muted"),
      tags$p(tags$sup("5"), " Survey weights must be in a column named 'weight'. If uploaded poststratification data contain weights, they're used to estimate population counts.", class = "small text-muted"),
      
      tags$p("Input data are categorized for clear requirements and implementation, with multiple modules. The two primary categories, ", tags$b("time-varying"), " and ", tags$b("cross-sectional"), ", support specific applications as well as general cases. The following cheatsheet summarizes requirements and typical preprocessing outputs for each."),
      
      bslib::layout_columns(
        col_widths = c(6, 6),
        
        # Time-varying card
        bslib::card(
          bslib::card_header(tags$h4("TIME-VARYING")),
          bslib::card_body(
            tags$h5("COVID-19 Test Data"),
            tags$p("1. Sample data"),
            tags$ul(
              tags$li("Sex: male, female"),
              tags$li("Race: Black, White, other"),
              tags$li("Age: 0-17, 18-34, 35-64, 65-74, 75+"),
              tags$li("ZIP code: each ZIP treated as distinct"),
              tags$li("Time: Dates (yyyy-mm-dd) or sequential indices (starting at 1)")
            ),
            tags$p("2. Poststratification data"),
            tags$ul(
              tags$li("ACS linking: sex * race * age * zip")
            ),
            
            tags$h5("General", class = "mt-3"),
            tags$p("1. Sample data"),
            tags$ul(
              tags$li("Sex: male, female"),
              tags$li("Race: Black, White, other"),
              tags$li("Age: 0-17, 18-34, 35-64, 65-74, 75+"),
              tags$li("ZIP code: each ZIP treated as distinct"),
              tags$li("County: five-digit FIPS codes required"),
              tags$li("State: name, abbreviation, or FIPS code"),
              tags$li("Time: Dates or sequential indices")
            ),
            tags$p("2. Poststratification data"),
            tags$ul(
              tags$li("ACS linking: sex * race * age * (user selected geographic levels)"),
              tags$li("User upload")
            )
          )
        ),
        
        # Cross-sectional card
        bslib::card(
          bslib::card_header(tags$h4("CROSS-SECTIONAL")),
          bslib::card_body(
            tags$h5("Public Opinion Poll Data"),
            tags$p("1. Sample data"),
            tags$ul(
              tags$li("Sex: male, female"),
              tags$li("Race: Black, White, other"),
              tags$li("Age: 18-29, 30-39, 40-49, 50-59, 60-69, 70+"),
              tags$li("Education (edu): below high school, high school, some college, 4-year college, post-grad"),
              tags$li("State: name, abbreviation, or FIPS code")
            ),
            tags$p("2. Poststratification data"),
            tags$ul(
              tags$li("ACS linking: sex * race * age * edu * state")
            ),
            
            tags$h5("General", class = "mt-3"),
            tags$p("1. Sample data"),
            tags$ul(
              tags$li("Sex: male, female"),
              tags$li("Race: Black, White, other"),
              tags$li("Age: 0-17, 18-34, 35-64, 65-74, 75+"),
              tags$li("ZIP code: each ZIP treated as distinct"),
              tags$li("County: five-digit FIPS codes"),
              tags$li("State: name, abbreviation, or FIPS code")
            ),
            tags$p("2. Poststratification data"),
            tags$ul(
              tags$li("ACS linking: sex * race * age * (user selected geographic levels)"),
              tags$li("User upload")
            )
          )
        )
      ),
      
      tags$h5("Data Preprocessing", class = "mt-4"),
      tags$p("The application performs several preprocessing steps, including cleaning, variable recoding (e.g., converting numeric age to age groups, date to time index), and removing defective records. While these steps streamline analysis, they do not substitute for thorough data preparation. Users should ensure data quality before upload. Example preprocessing scripts are available for review and customization via the ", tags$b("Learn > Data Preprocessing"), " page."),
      
      tags$h5("Data Linking", class = "mt-4"),
      tags$p("To facilitate ACS data linkage, the interface recognizes the smallest geographic unit in the sample and infers corresponding higher-level geographies (e.g., ZIP to county or state). The application also enriches data with geographic covariates at the ZIP code level (urbanicity, Area Deprivation Index, etc.). More information is available on the ", tags$b("Learn > Data Preprocessing"), " page."),
      tags$p("Linking to the ACS enables estimation of approximate population counts for poststratification. Users may select geographic levels and ACS reference years, with built-in constraints for some use cases (e.g., COVID-19 or public opinion polls). Currently, COVID-19 data are linked to five-year ACS (2017-2021) at the ZIP code level, and polling data to five-year ACS (2014-2018) at the state level. General modules offer additional linking options.")
    ),

    # Plot Selection panel
    bslib::accordion_panel(
      title = tags$h4("Data Visualization"),
      value = "plot_select",
      tags$p("The interface maintains a unified, responsive layout throughout the workflow. In both the descriptive statistics and result sections, a sidebar with selection inputs allows users to:"),
      tags$ol(
        tags$li("Choose a main plot type."),
        tags$li("Select relevant sub-categories as they appear."),
        tags$li("Fine-tune geographic visualizations with options such as plot type and data subsetting.")
      ),
      tags$p("Selection options depend on the current module; for instance, histograms of geographic covariates such as urbanicity and ADI are unique to the COVID-19 workflow.")
    ),
    
    # Model Specification panel
    bslib::accordion_panel(
      title = tags$h4("Model Specification"),
      value = "model_spec",
      tags$p("Users can select predictors (including two-way interactions), and specify fixed or varying effects and prior distributions. Predictors may be at the individual or geographic level."),
      
      tags$h5("Default Priors", class = "mt-4"),
      tags$p("By default, the following priors are assigned:"),
      tags$ul(
        tags$li("Overall intercept: ", withMathJax("\\(normal(0, 3)\\)")),
        tags$li("Coefficient: ", withMathJax("\\(normal(0, 1)\\)"))
      ),
      tags$p("All varying effects use normal distributions with unknown standard deviations (to which priors are also assigned):"),
      tags$ul(
        tags$li("Main effect standard deviation: ", withMathJax("\\(normal_+(0, 1)\\)")),
        tags$li("Interaction effect standard deviation: ", withMathJax("\\(normal_+(0, 1)\\)"))
      ),
      
      tags$h5("Modifying Priors", class = "mt-4"),
      tags$p("You may modify priors (where supported). Valid choices are:"),
      tags$ul(
        tags$li("normal(mu, sigma)"),
        tags$li("student_t(nu, mu, sigma)"),
        tags$li("structured**")
      ),
      tags$p("These mimic ", tags$a("Stan distribution syntax", href = "https://mc-stan.org/docs/functions-reference/unbounded_continuous_distributions.html", target = "_blank"), ". In addition, we provide the structured prior distribution developed by ", tags$a("Si et al. (2020)", href = "https://www150.statcan.gc.ca/n1/en/pub/12-001-x/2020002/article/00003-eng.pdf?st=iF1_Fbrh", target = "_blank"), ", which can be assigned to three types of two-way interactions:"),
      tags$ul(
        tags$li("Two categorical variables (both with more than two levels)"),
        tags$li("One categorical variable (with more than two levels) and one binary variable"),
        tags$li("One categorical variable (with more than two levels) and one continuous variable")
      ),
      tags$p("This approach requires the main effect of the categorical variable with more than two levels to be included as a varying effect. Below is an example of the structured prior distribution for the two-way interaction of race and age:"),
      tags$ul(class = "list-unstyled",
        tags$li(withMathJax("Main effect of race: \\(normal(0, \\lambda_1\\sigma_{race})\\)")),
        tags$li(withMathJax("Main effect of age: \\(normal(0, \\lambda_1\\sigma_{age})\\)")),
        tags$li(withMathJax("Interaction of race and age: \\(normal(0, \\lambda_1\\lambda_2\\sigma_{age}\\sigma_{race})\\)")),
        tags$li(withMathJax("Standard deviation of main effects (\\(\\sigma_{race}, \\sigma_{age}\\)): \\(normal_+(0, 1)\\)")),
        tags$li(withMathJax("Global scale (\\(\\lambda_1\\)): \\(cauchy_+(0, 1)\\)*")),
        tags$li(withMathJax("Local scale (\\(\\lambda_2\\)): \\(normal_+(0, 1)\\)*"))
      ),
      tags$p("*The plus sign (+) indicates that the distributions are restricted to positive values. For example, ", withMathJax("\\(normal_+(0, 3)\\)"), " is a normal distribution with mean 0 and standard deviation of 3 restricted to positive values.", class = "small text-muted mt-3"),
      tags$p("**The default priors for the global scale and local scale are ", withMathJax("\\(cauchy_+(0, 1)\\)"), " and ", withMathJax("\\(normal_+(0, 1)\\)"), " respectively. These cannot be changed at the moment.", class = "small text-muted")
    ),
    
    # Model Fitting panel
    bslib::accordion_panel(
      title = tags$h4("Model Fitting"),
      value = "model_fit",
      tags$p("The interface uses Markov chain Monte Carlo (MCMC) algorithms via ", tags$a("Stan", href = "https://mc-stan.org/", target = "_blank"), " for posterior computation under a Bayesian framework. The Stan code is generated for compilation with the package ", tags$a("CmdStanR", href = "https://mc-stan.org/cmdstanr/", target = "_blank"), ". MCMC chains run in parallel, with each chain allocated to a processor core. Choose the number of chains according to your computing resources for optimal performance."),
      tags$p("Large datasets and complex models increase computational demands. The application addresses this by:"),
      tags$ul(
        tags$li(tags$b("Separated stages:"), " Multilevel Regression (MR) and Poststratification (P) are separated, allowing model diagnostics prior to poststratification. Results may be saved at any workflow stage."),
        tags$li(tags$b("Optimized computation:"), " Vectorized Stan (C++) routines are used for poststratification to enhance runtime efficiency.")
      ),
      tags$p("Detailed information and advanced topics are available on the ", tags$b("Learn > MRP"), " page.")
    ),

    # Result Visualization panel
    bslib::accordion_panel(
      title = tags$h4("Result Visualization"),
      value = "result_vis",
      tags$p("The interface visualizes estimates for the overall population, demographic groups, and geographic regions. For spatial results, users can create interactive maps. For time-varying data, the results will be time-specific.")
    )
  )
}

#' Create model result tabs
#'
#' @description Creates dynamic navigation tabs for displaying model results
#' including formula, diagnostics, parameter tables, and posterior predictive
#' checks. Includes interactive elements for model management and saving.
#'
#' @param ns Namespace function for the Shiny module
#' @param model List containing model information including IDs, formula,
#'   sampling parameters, and fitted results
#' @param last_tab_id Character. ID of the last tab for positioning the new tab
#'
#' @return No return value, called for side effect of inserting navigation tab
#'
#' @importFrom shiny tags actionButton downloadButton textOutput plotOutput tableOutput icon HTML
#'
#' @noRd
#' @keywords internal
.create_model_tab <- function(ns, model, last_tab_id) {
  
  tab_header <- tags$div(
    class = "model_tab_header",
    textOutput(
      outputId = ns(model$get_id("title")),
      inline = TRUE
    ),
    actionButton(
      inputId = ns(model$get_id("rm_btn")),
      label = NULL,
      icon = icon("remove", lib = "glyphicon"),
      class = "btn-xs remove_model"
    )
  )

  bslib::nav_panel(
    title = tab_header,
    value = model$get_id("tab"),
    tags$div(
      bslib::layout_columns(
        col_widths = c(11, 1),
        class = "mb-0",
        HTML(paste0("<h4 class='formula'>", "Formula: ", model$formula(), "</h4>")),
        tags$div(class = "d-flex align-items-end gap-2",
          bslib::tooltip(
            actionButton(
              inputId = ns(model$get_id("diagnos_btn")),
              label = NULL,
              icon = icon("sliders-h", lib = "font-awesome"),
              class = "btn btn-sm btn-secondary"
            ),
            "Please check sampler diagnostics",
            id = ns(model$get_id("diagnos_tooltip")),
            placement = "left",
            options = list(trigger = "manual")
          ),
          bslib::popover(
            actionButton(
              inputId = ns(model$get_id("save_popover_btn")),
              label = NULL,
              icon = icon("download"),
              class = "btn btn-sm btn-secondary"
            ),
            title = "Save Options",
            downloadButton(
              outputId = ns(model$get_id("save_code_btn")),
              label = "Model Code",
              icon = NULL,
              style = "width: 100%; margin-bottom: 5px;"
            ),
            downloadButton(
              outputId = ns(model$get_id("save_fit_btn")),
              label = "Estimation Result",
              icon = NULL,
              style = "width: 100%;"
            ),
            placement = "left"
          )
        )
      ),
      if (model$metadata()$family == "binomial") {
        tags$p(
          paste0(
            "A binomial model with a logit function of the positive response rate. ",
            "Samples are generated using ",
            model$metadata()$n_chains, " chains with ", model$metadata()$n_iter / 2,
            " post-warmup iterations each."
          ),
          class = "fst-italic small"
        )
      } else {
        tags$p(
          paste0(
            "A linear model of the outcome measure. ",
            "Samples are generated using ",
            model$metadata()$n_chains," chains with ", model$metadata()$n_iter / 2,
            " post-warmup iterations each."
          ),
          class = "fst-italic small"
        )
      },
      actionButton(
        inputId = ns(model$get_id("postprocess_btn")),
        label = "Run poststratification"
      ),
      tags$div(style = "margin-top: 30px",
        bslib::card(
          bslib::card_header(tags$b("Note")),
          bslib::card_body(tags$ul(
            tags$li("Large ", tags$a("R-hat", href = "https://mc-stan.org/learn-stan/diagnostics-warnings.html#r-hat", target = "_blank"), " (e.g., greater than 1.05) values indicate that the computation has not yet converged, and it is necessary to run more iterations and/or modify model and prior specifications."),
            tags$li("Low values for ", tags$a("Bulk-ESS", href = "https://mc-stan.org/learn-stan/diagnostics-warnings.html#bulk-and-tail-ess", target = "_blank"), " and ", tags$a("Tail-ESS", href = "https://mc-stan.org/learn-stan/diagnostics-warnings.html#bulk-and-tail-ess", target = "_blank"), " (ESS stands for Effective Sample Size) also suggest that more iterations are required.")
          ))
        ),
        if(nrow(model$summary()$fixed) > 0) {
          tags$div(
            tags$h4("Fixed Effects", class = "break_title"),
            tags$hr(class = "break_line"),
            tableOutput(ns(model$get_id("fixed_tbl")))
          )
        },
        if(nrow(model$summary()$varying) > 0) {
          tags$div(
            tags$h4("Standard Deviation of Varying Effects", class = "break_title"),
            tags$hr(class = "break_line"),
            tableOutput(ns(model$get_id("varying_tbl")))  
          )
        },
        if(nrow(model$summary()$other) > 0) {
          tags$div(
            tags$h4("Standard Deviation of Residuals", class = "break_title"),
            tags$hr(class = "break_line"),
            tableOutput(ns(model$get_id("other_tbl")))  
          )
        },
        tags$div(
          tags$h4("Posterior Predictive Check", class = "break_title"),
          tags$hr(class = "break_line"),
          bslib::card(
            bslib::card_header(tags$b("Note")),
            bslib::card_body(tags$p("The plot shows the outcome averages computed from the observed data and 10 sets of replicated data.")) 
          ),
          plotOutput(outputId = ns(model$get_id("ppc_plot")))
        )
      )
    )
  )
}

#' Returns UI elements for visualization of estimates for geographic areas
#'
#' @param model The model object containing post-stratification and plot data.
#' @param ns Namespace function for UI element IDs.
#' @param geo_scale The geographic scale to visualize (e.g., "county", "state").
#' @param geo_view The type of view to display (e.g., "map", "line_scatter").
#'
#' @return A list of UI elements for the specified geographic visualization.
#' 
#' @noRd
#' @keywords internal
.est_map_ui <- function(ns, model, geo_scale, geo_view) {

  checkmate::assert_choice(geo_scale, .const()$vars$geo2)
  checkmate::assert_choice(geo_view, .const()$ui$geo_view)

  time_indices <- model$poststratify()[[geo_scale]]$time
  dates <- model$plot_data()$dates

  switch(geo_view,
    "map" = tagList(
      highcharter::highchartOutput(
        outputId = ns("map"),
        height = .const()$plot$ui$map_height
      ),
      # Only show slider if we have time-varying data
      if (!is.null(time_indices)) {
        if(!is.null(dates)) {
          div(
            class = "mx-4",
            shinyWidgets::sliderTextInput(
              inputId = ns("map_slider"),
              label = NULL,
              choices = dates,
              selected = dates[1],
              width = "100%",
              grid = TRUE,
              animate = .const()$ui$animation
            )
          )
        } else {
          div(
            class = "mx-4",
            sliderInput(
              inputId = ns("map_slider"),
              label = NULL,
              min = 1,
              max = max(time_indices),
              step = 1,
              value = 1,
              width = "100%",
              animate = .const()$ui$animation
            )
          )
        }
      }
    ),
    "line_scatter" = plotOutput(ns("plot")),
    NULL
  )
}

#' Determine plot height based on number of plots and time-varying flag
#' 
#' @param n Number of plots to be displayed
#' @param is_timevar Logical flag indicating if the data is time-varying
#' 
#' @return Numeric height for the plot output
#' 
#' @noRd
#' @keywords internal
.plot_height <- function(n = 1, is_timevar = FALSE) {
  plot_height <- .const()$plot$ui$plot_height
  subplot_height <- .const()$plot$ui$subplot_height

  h <- if (n > 1 && is_timevar) {
    subplot_height * n
  } else {
    plot_height
  }

  return(h)
}

#' Determine main category choices
#' 
#' @description Returns the appropriate main category choices for the plot
#' selection input based on the metadata and link data.
#' 
#' @param metadata A list containing metadata about the current analysis, including special cases and time
#' -varying flag.
#' @param linkdata A list containing information about linked geographic data.
#' 
#' @return A character vector of main category choices for the plot selection input.
#' 
#' @noRd
#' @keywords internal
.vis_cat_select <- function(metadata, linkdata) {
  choices <- .const()$ui$plot_selection$vis_main[[metadata$family]]
  counts <- purrr::map_int(
    choices,
    ~ length(.vis_subcat_select(.x, metadata, linkdata)$choices)
  )

  choices <- choices[counts > 0]

  return(choices)
}

#' Determine sub-category label and choices
#' 
#' @description Returns the appropriate label and choices for the sub-category
#' selection input based on the selected main category, metadata, and link data.
#' 
#' @param category The main category selected by the user (e.g., "indiv", "geo", "outcome").
#' @param metadata A list containing metadata about the current analysis, including special cases and time-varying flag.
#' @param linkdata A list containing information about linked geographic data.
#' 
#' @return A list with 'label' and 'choices' for the sub-category selection input.
#' 
#' @noRd
#' @keywords internal
.vis_subcat_select <- function(category, metadata, linkdata) {
  ui_ps   <- .const()$ui$plot_selection

  # Base label/choices by category
  base <- switch(category,
    indiv   = list(label = "2. Select characteristic", choices = ui_ps$indiv),
    geo     = list(label = "2. Select characteristic", choices = ui_ps$geo),
    outcome = list(label = "2. Select plot type",      choices = ui_ps$outcome),
    NULL
  )

  if (is.null(base)) return(list(label = character(0), choices = NULL))

  choices <- base$choices

  # Category-specific adjustments
  if (identical(category, "indiv")) {
    # remove "edu" unless special_case == "poll"
    if (!identical(metadata$special_case, "poll")) {
      choices <- choices[!choices == "edu"]
    }
  }

  if (identical(category, "geo")) {
    # append geo covariates when special_case == "covid"
    if (identical(metadata$special_case, "covid")) {
      choices <- c(choices, ui_ps$geo_covar)
    }
    if (is.null(linkdata$link_geo)) {
      choices <- character(0)
    }
  }

  if (identical(category, "outcome")) {
    # hide "overall" when not time-varying
    if (!isTRUE(metadata$is_timevar)) {
      choices <- choices[!choices == "overall"]
    }
    # hide "by_geo" when link_geo is missing
    if (is.null(linkdata$link_geo)) {
      choices <- choices[!choices == "by_geo"]
    }
  }

  return(
    list(
      label = base$label,
      choices = choices
    )
  )
}

#' Generate UI for specific visualization based on category and sub-category
#' 
#' @param ns Namespace function for UI element IDs
#' @param category The main category selected by the user (e.g., "indiv", "geo", "outcome")
#' @param subcategory The sub-category selected by the user
#' 
#' @return The UI elements corresponding to the selected category and sub-category
#' 
#' @noRd
#' @keywords internal
.vis_ui <- function(ns, category, subcategory) {
  ui_ps <- .const()$ui$plot_selection

  checkmate::assert_choice(
    category,
    ui_ps$vis_main[[1]]
  )

  checkmate::assert_choice(
    subcategory,
    unlist(ui_ps[c("indiv", "geo", "geo_covar", "outcome")])
  )

  if (category == "indiv") {
    switch(subcategory,
      "sex"  = mod_indiv_plot_ui(ns("indiv_sex")),
      "race" = mod_indiv_plot_ui(ns("indiv_race")),
      "age"  = mod_indiv_plot_ui(ns("indiv_age")),
      "edu"  = mod_indiv_plot_ui(ns("indiv_edu"))
    )
  } else if (category == "geo") {
    switch(subcategory,
      "sample"     = mod_indiv_map_ui(ns("geo_sample")),
      "college"    = mod_geo_plot_ui(ns("geo_college")),
      "poverty"    = mod_geo_plot_ui(ns("geo_poverty")),
      "employment" = mod_geo_plot_ui(ns("geo_employment")),
      "income"     = mod_geo_plot_ui(ns("geo_income")),
      "urbanicity" = mod_geo_plot_ui(ns("geo_urbanicity")),
      "adi"        = mod_geo_plot_ui(ns("geo_adi"))
    )
  } else if (category == "outcome") {
    switch(subcategory,
      "overall" = plotOutput(ns("positive_plot"), height = .plot_height()),
      "by_geo" = highcharter::highchartOutput(ns("positive_map"), height = .const()$plot$ui$map_height)
    )
  }
}

#' Generate a preview table of the data
#' 
#' @param df A data frame to be previewed
#' 
#' @return A datatable preview of the data
#' 
#' @noRd
#' @keywords internal 
.preview_table <- function(df) {
  
  preview <- df %>%
    utils::head(.const()$ui$preview_size) %>%
    DT::datatable(
      options = list(
        columnDefs = list(
          list(
            className = "dt-left",
            targets = "_all"
          )
        ),
        scrollX = TRUE,
        lengthChange = FALSE,
        searching = FALSE,
        info = FALSE
      )
    )

  preview <- if ("outcome" %in% names(df)) {
    preview %>%
      DT::formatRound(
        columns = c("outcome"),
        digits = 4
      )
  } else if ("positive" %in% names(df)) {
    preview %>%
      DT::formatStyle(
        columns = c("positive"),
        `max-width` = "150px"
      )
  }

  return(preview)
}


#' Returns linking options based on use case
#' 
#' @param data The sample data to be analyzed
#' @param use_case The use case for the analysis
#' @param no_link_geo_label The label to use for the "no linking geography" option
#' 
#' @return A list containing linking geography options and ACS year options
#' 
#' @noRd
#' @keywords internal
.link_select <- function(
  data,
  use_case = NULL,
  no_link_geo_label = "Do not include geography"
) {

  use_case <- use_case %||% "general"
  
  choices <- switch(use_case,
    "covid" = list(link_geos = c("zip"), acs_years = 2021),
    "poll"  = list(link_geos = c("state"), acs_years = 2018),
    "general" = list(
      link_geos = c(
        .get_possible_geos(names(data)),
        no_link_geo_label
      ),
      acs_years = 2019:2023
    )
  )

  choices$acs_years <- paste0(choices$acs_years - 4, "-", choices$acs_years)

  return(choices)
}