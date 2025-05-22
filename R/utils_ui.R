#' ui
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
check_iter_chain <- function(n_iter, n_iter_range, n_chains, n_chains_range, seed) {
  flag <- TRUE
  msg <- c()
  
  if(is.numeric(n_iter) && is.numeric(n_chains) && is.numeric(seed)) {
    if(n_iter < n_iter_range[1] | n_iter > n_iter_range[2]) {
      msg <- c(msg, paste0("The number of iterations must be between ", n_iter_range[1], " and ", n_iter_range[2], "."))
      flag <- FALSE
    }
    
    if(n_chains < n_chains_range[1] | n_chains > n_chains_range[2]) {
      msg <- c(msg, paste0("The number of chains must be between ", n_chains_range[1], " and ", n_chains_range[2], "."))
      flag <- FALSE
    }
  } else {
    flag <- FALSE
    
    if(!is.numeric(n_iter)) {
      msg <- c(msg, "The number of iterations must be a numeric value.")
    }
    
    if(!is.numeric(n_chains)) {
      msg <- c(msg, "The number of chains must be a numeric value.")
    }
    
    if(!is.numeric(seed)) {
      msg <- c(msg, "The seed must be a numeric value.")
    }
  }
  
  return(list(
    valid = flag, 
    msg = msg
  ))
}

#' Check Model Fit Object
#'
#' Validates if the model's data format matches the expected format.
#'
#' @param model The model object to check
#' @param expected_format The expected data format (e.g., "temporal_covid", "crosssectional")
#'
#' @return A list with two elements:
#'   \item{valid}{Logical indicating if the format is valid}
#'   \item{message}{Warning message if invalid, or NULL if valid}
#' @noRd
check_fit_object <- function(model, expected_format) {
  example_model <- qs::qread(app_sys("extdata/example/fit/fit_crosssectional_other.RDS"))
  
  # Check if the model object has all the required fields
  if (!setequal(names(model), names(example_model))) {
    return("The uploaded file does not contain a valid estimation result.")
  }

  # Check if the model object has the expected data format
  if(model$data_format != expected_format) {
      return(sprintf("The uploaded file contains model estimation for %s instead of %s.",
                     data_format_label(model$data_format),
                     data_format_label(expected_format)))
  }

  return("")
}

waiter_ui <- function(type = "") {
  if(type == "fit") {
    tagList(
      waiter::spin_loaders(2, color = "black"),
      tags$h4("Fitting model...", style = "color: black")
    )
  } else if(type == "pstrat") {
    tagList(
      waiter::spin_loaders(2, color = "black"),
      tags$h4("Running poststratification...", style = "color: black")
    )
  } else if(type == "loo") {
    tagList(
      waiter::spin_loaders(2, color = "black"),
      tags$h4("Running diagnostics...", style = "color: black")
    )
  } else if (type == "setup") {
    tagList(
      waiter::spin_loaders(15, color = "black"),
      tags$h4("Installing CmdStan...", style = "color: black")
    )
  } else if (type == "wait") {
    tagList(
      waiter::spin_loaders(15, color = "black"),
      tags$h4("Please wait...", style = "color: black")
    )
  } else {
    NULL
  }
}

show_alert <- function(message, session) {
  showModal(
    modalDialog(
      title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
      message
    ),
    session = session
  )
}

show_notif <- function(message, session) {
  showModal(
    modalDialog(
      title = tagList(icon("bell", "fa"), "Notification"),
      message
    ),
    session = session
  )
}

create_guide <- function(open = c("workflow", "upload", "model_spec", "model_fit")) {
  open <- match.arg(open)

  bslib::accordion(
    id = "guide_accordion",
    open = open,
    class = "p-0 m-0",
    multiple = FALSE,
    # Workflow panel
    bslib::accordion_panel(
      title = tags$h4("Workflow"),
      value = "workflow",
      tags$p("The interface provides different modules tailored to various use cases, categorized into ", tags$b("time-varying"), " and ", tags$b("cross-sectional"), " analyses. Each category supports specific applications, such as COVID-19 transmission tracking and public opinion polling, along with broader use cases. Regardless of the chosen module, the application adheres to a standard statistical analysis workflow:"),
      tags$ul(
        tags$li(tags$b("Data Cleaning:"), " Preprocess and display the input data."),
        tags$li(tags$b("Descriptive Statistics:"), " Visualize key summary statistics."),
        tags$li(tags$b("Model Building:"), " Users can specify and fit models with various predictors and fixed or varying effects. The interface offers detailed diagnostics to evaluate and compare models, facilitating model selection."),
        tags$li(tags$b("Result Visualization:"), " Generate graphs to illustrate estimates for the target population and demographic and geographic subgroups for the selected model.")
      )
    ),
    
    # Uploading Data panel
    bslib::accordion_panel(
      title = tags$h4("Uploading Data"),
      value = "upload",
      tags$p("The MRP interface needs two major data components:"),
      tags$ul(
        tags$li(tags$b("Sample data:"), " The analysis sample that includes the outcome of interest and predictors, such as the COVID test records and survey sample results."),
        tags$li(tags$b("Poststratification data:"), " The table containing sizes of groups in the target population defined by the demographic and geographic factors.")
      ),
      tags$p("Providing poststratification data is optional since the application can utilize geographic identifiers to link the American Community Survey (ACS) and obtain the population counts residing in the catchment areas. Data linking is available across all application modules, and users can upload custom poststratification data in the time-varying and cross-sectional general cases."),
      tags$h5("Individual-level vs. Aggregated Data", class = "mt-4"),
      tags$p("The interface accepts data in two formats:",
      tags$ul(
        tags$li(tags$b("Individual-level:"), " Each row contains information for on individual."),
        tags$li(tags$b("Aggregated:"), " Each row contains information for one group (e.g., White males aged 18-30 in Michigan), with geographic-demographic factors, total numbers of individuals, and summary of outcomes.")
      )),
      tags$p("Aggregated data are preferred for computational benefits. Individual-level data will be automatically aggregated upon upload. Data requirements vary slightly between formats, mainly regarding the outcome measure."),
      tags$h5("Required Columns and Categories", class = "mt-4"),
      tags$p("The application screens input data using a specific naming convention. Here's a list of the expected columns and their values (case-insensitive):"),
      tags$ul(
        tags$li("Sex: male, female"),
        tags$li("Race: Black, White, other"),
        tags$li("Age"),
        tags$li("Education attainment (edu): below high school (no hs), high school (hs), some college, 4-year college, post-grad"),
        tags$li(withMathJax("ZIP code\\(^1\\)")),
        tags$li(withMathJax("County\\(^1\\)")),
        tags$li("State\\(^1\\)"),
        tags$li(withMathJax("Week indices (time)\\(^2\\)")),
        tags$li("Date"),
        tags$li(withMathJax("Positive response indicator or number of positive responses (positive)\\(^3\\)")),
        tags$li(withMathJax("Cross-tabulation cell counts (total)\\(^3\\)")),
        tags$li(withMathJax("Survey weights (weight)\\(^4\\)"))
      ),
      tags$p("1. For general use cases, providing geographic information is optional. The application will automatically identify the smallest geographic scale available and provide the corresponding higher levels.",
        class = "fst-italic small mb-1"),
      tags$p("2. If the input sample data are in aggregated format, there has to be a column named 'time' that contains week indices. An optional 'date' column containing the date of the first day of each week can be included for visualization purposes. For individual-level sample data, the interface will automatically convert the dates to week indices, but users can also provide the week indices directly. The interface uses time-invariant poststratification data.",
        class = "fst-italic small mb-1"),
      tags$p("3. In the individual-level data, the binary outcome column must be named 'positive'. Aggregated data require two columns to represent the outcome measures: the total count of individuals and the number of positive responses for each cross-tabulation cell, which should be named 'total' and 'positive', respectively.",
        class = "fst-italic small mb-1"),
      tags$p("4. Please name the column containing survey weights in the data 'weight'. If the uploaded poststratification data include survey weights, the interface uses weights to estimate the population counts.",
        class = "fst-italic small"),
      tags$h5("Data Preprocessing", class = "mt-4"),
      tags$p("The application performs several preprocessing steps to prepare the data for MRP, such as removing defects, converting raw values to categories (e.g., numeric age to age groups, date to week index), etc. However, exhaustive preprocessing is not guaranteed; users may need to prepare data beforehand. Preprocessing code is available for download and customization via the ", tags$b("Learn > Data Preprocessing"), " page."),
      tags$h5("Data Linking", class = "mt-4"),
      tags$p("To enhance linking capabilities, the interface identifies the smallest geographic unit in the sample data and infers corresponding larger geographic areas from the smallest units (e.g., ZIP code to county and state with most overlapping areas). Additionally, the interface supplements geographic covariates on the zip code level (e.g., urbanicity, Area Deprivation Index, etc.). More details are available on the ", tags$b("Learn > Data Preprocessing"), " page."),
      tags$p("The MRP interface facilitates linking to the ACS to obtain approximate population counts critical to poststratification. Users can select geographic factors and ACS data years, with specific restrictions for use cases of COVID-19 and public opinion polling data. Current options link COVID records to five-year ACS data (2017-2021) via ZIP codes and poll data to five-year ACS (2014-2018) via states. More options exist for general applications, including ZIP code, county, or state-based links."),
      tags$p("Input requirements differ between interface use case modules. Detailed columns for both sample and poststratification data are outlined below."),
      bslib::layout_columns(
        col_widths = c(6, 6),

        # Time-varying card
        bslib::card(
          bslib::card_header(tags$h4("Time-varying")),
          bslib::card_body(class = "gap-2",
            tags$h5("COVID Test Data"),
            tags$p("1. Sample data"),
            tags$ul(
              tags$li("Sex: male, female"),
              tags$li("Race: Black, White, other"),
              tags$li("Age: 0-17, 18-34, 35-64, 65-74, 75+"),
              tags$li("ZIP code (zip): Each ZIP code is treated as distinct"),
              tags$li("Time: Dates (yyyy-mm-dd) or week indices (starting with index 1 assigned to the earliest week in the data)")
            ),
            tags$p("2. Poststratification data"),
            tags$ul(
              tags$li("ACS linking: sex * race * age * zip")
            ),
            tags$p("3. Sensitivity and specification adjustment in the COVID test results (Check ", tags$b("Learn > MRP"), " for details)."),
            
            tags$h5("General", class = "mt-3"),
            tags$p("1. Sample data"),
            tags$ul(
              tags$li("Sex: male, female"),
              tags$li("Race: Black, White, other"),
              tags$li("Age: 0-17, 18-34, 35-64, 65-74, 75+"),
              tags$li("ZIP code: Each ZIP code is treated as distinct"),
              tags$li("County: Five-digit FIPS codes required due to duplicates in county names"),
              tags$li("State: Names, abbreviations, or FIPS accepted"),
              tags$li("Time: Dates (yyyy-mm-dd) or week indices (starting with index 1 assigned to the earliest week in the data)")
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
          bslib::card_header(tags$h4("Cross-sectional")),
          bslib::card_body(class = "gap-2",
            tags$h5("Public Opinion Poll Data"),
            tags$p("1. Sample data"),
            tags$ul(
              tags$li("Sex: male, female"),
              tags$li("Race: Black, White, other"),
              tags$li("Age: 18-29, 30-39, 40-49, 50-59, 60-69, 70+"),
              tags$li("Education level (edu): below high school (no hs), high school (hs), some college, 4-year college, post-grad"),
              tags$li("State: Names (e.g., Michigan), abbreviations (e.g., MI), or FIPS (e.g., 26) accepted")
            ),
            tags$p("2. Poststratification data"),
            tags$ul(
              tags$li("ACS linking: sex * race * age * edu * state")
            ),
            
            tags$h5("General", style = "margin-top: 50px"),
            tags$p("1. Sample data"),
            tags$ul(
              tags$li("Sex: male, female"),
              tags$li("Race: Black, White, other"),
              tags$li("Age: 0-17, 18-34, 35-64, 65-74, 75+"),
              tags$li("ZIP code: Each ZIP code is treated as distinct"),
              tags$li("County: Five-digit FIPS codes required due to duplicates in county names"),
              tags$li("State: Names, abbreviations, or FIPS accepted")
            ),
            tags$p("2. Poststratification data"),
            tags$ul(
              tags$li("ACS linking: sex * race * age * (user selected geographic levels)"),
              tags$li("User upload")
            )
          )
        )
      )
    ),

    # Plot Selection panel
    bslib::accordion_panel(
      title = tags$h4("Graph Display"),
      value = "plot_select",
      tags$p("The application maintains a uniform visual layout throughout the workflow. In both descriptive statistics and estimation result sections, a sidebar with responsive selection inputs is present for easy navigation:"),
      tags$ol(
        tags$li("Choose a main plot category."),
        tags$li("Select relevant sub-categories as they appear."),
        tags$li("Customize geographic visualizations with distinct options, including plot types and data subsetting."),
      ),
      tags$p("Selection options vary by interface module. For example, histograms of geographic covariates like the urbanicity and Area Deprivation Index (ADI) are exclusive to the COVID-19 data analysis workflow.")
    ),

    
    # Model Specification panel
    bslib::accordion_panel(
      title = tags$h4("Model Specification"),
      value = "model_spec",
      tags$p("The interface supports user selection of predictors and their two-way interactions, fixed and varying effects, alongside prior distributions for model parameters. Predictors can include individual and geographic-level measures."),
      
      tags$h5("Default Priors", class = "mt-4"),
      tags$p("Under the Bayesian framework, the following prior distributions are assigned by default:"),
      tags$ul(
        tags$li("Overall intercept: ", withMathJax(sprintf("\\(%s\\)", GLOBAL$default_priors$Intercept))),
        tags$li("Coefficient: ", withMathJax(sprintf("\\(%s\\)", GLOBAL$default_priors$fixed)))
      ),
      tags$p("The model assumes varying effects follow a normal distribution with an unknown standard deviation, which will be assigned with priors."),
      tags$ul(
        tags$li("Standard deviation (main effect): ", withMathJax(sprintf("\\(%s\\)", gsub("\\(", "_+(", GLOBAL$default_priors$varying))), "*"),
        tags$li("Standard deviation (interaction): ", withMathJax(sprintf("\\(%s\\)", gsub("\\(", "_+(", GLOBAL$default_priors$interaction))), "*")
      ),
      
      tags$h5("Available Priors", class = "mt-4"),
      tags$p("Users can modify prior specifications. Accepted distributions include:"),
      tags$ul(
        tags$li("normal(mu, sigma)"),
        tags$li("student_t(nu, mu, sigma)"),
        tags$li("structured**")
      ),
      
      tags$p("These mimic ",
            tags$a("Stan distribution syntax.",
                    href = "https://mc-stan.org/docs/functions-reference/unbounded_continuous_distributions.html",
                    target = "_blank"),
            " In addition, we provide the structured prior distribution developed by ",
            tags$a("Si et al. (2020),",
                    href = "https://arxiv.org/abs/1707.08220",
                    target = "_blank"),
            " which can be assigned to three types of two-way interactions:",
 
      ),
      tags$ul(
        tags$li("Two categorical variables (both with more than two levels)"),
        tags$li("One categorical variable (with more than two levels) and one binary variable"),
        tags$li("One categorical variable (with more than two levels) and one continuous variable")
      ),
      tags$p("This approach requires the main effect of the categorical variable with more than two levels to be included as a varying effect. Below is an example of the structured prior distribution for the two-way interaction of race and age:"),
      
      tags$ul(class = "list-unstyled mr-2",
        tags$li(withMathJax("Main effect of race: \\(normal(0, \\lambda_1\\sigma_{race})\\)")),
        tags$li(withMathJax("Main effect of age: \\(normal(0, \\lambda_1\\sigma_{age})\\)")),
        tags$li(withMathJax("Interaction of race and age: \\(normal(0, \\lambda_1\\lambda_2\\sigma_{age}\\sigma_{race}\\))")),
        tags$li(withMathJax("Standard deviation of main effects (\\(\\sigma_{race}, \\sigma_{age}\\)): \\(normal_+(0, 1)\\)")),
        tags$li(withMathJax("Global scale (\\(\\lambda_1\\)): \\(cauchy_+(0, 1)\\)"), "*"),
        tags$li(withMathJax("Local scale (\\(\\lambda_2\\)): \\(normal_+(0, 1)\\)"), "*")
      ),
      
      tags$div(class = "small mt-3",
              withMathJax("*The plus sign indicates that the distributions are restricted to positive values. For example, \\(normal_+(0, 3)\\) is a normal distribution with mean 0 and standard deviation of 3 restricted to positive values.")),
      
      tags$div(class = "small mt-2",
              withMathJax(sprintf("**The default priors for the global scale and local scale are \\(%s\\) and \\(%s\\) respectively. These cannot be changed at the moment.",
                                  gsub("\\(", "_+(", GLOBAL$default_priors$global_scale),
                                  gsub("\\(", "_+(", GLOBAL$default_priors$local_scale))))
    ),
    
    # Model Fitting panel
    bslib::accordion_panel(
      title = tags$h4("Model Fitting"),
      value = "model_fit",
      tags$p(
        "Leveraging a Bayesian framework, the application employs Markov chain Monte Carlo (MCMC) algorithms for posterior computations via ",
        tags$a("Stan.", href = "https://mc-stan.org/", target = "_blank"),
        " MCMC chains run in parallel for efficiency, each allocated a processing core. We recommend that users carefully specify the number of MCMC chains based on their available computing capacities."
      ),
      tags$p("Applying MRP to large data sets with complex models can be computation-intensive. The application mitigates this through:"),
      tags$ul(
        tags$li(tags$b("Separated stages"), ": The Multilevel Regression (MR) and Poststratification (P) steps are separated, allowing model evaluation before poststratification. Results can be saved at any stage of the process."),
        tags$li(tags$b("Optimized computation"), ": Vectorized implementation of poststratification in Stan (C++) reduce runtime significantly.")
      ),
      tags$p("Model details are available on the ", tags$b("Learn > MRP"), " page.")
    )
  )
}

show_guide <- function(open = NULL) {
  showModal(
    modalDialog(
      title = "User Guide",
      create_guide(open),
      size = "xl",
      easyClose = TRUE,
      footer = modalButton("Close")
    )
  )
}

create_model_tab <- function(ns, model, last_tab_id) {
  
  tab_header <- tags$div(
    class = "model_tab_header",
    textOutput(
      outputId = ns(model$IDs$title),
      inline = TRUE
    ),
    actionButton(
      inputId = ns(model$IDs$rm_btn),
      label = NULL,
      icon = icon("remove", lib = "glyphicon"),
      class = "btn-xs remove_model"
    )
  )
  
  bslib::nav_insert(
    id = "navbar_model",
    target = last_tab_id,
    position = "after",
    select = TRUE,
    nav = bslib::nav_panel(
      title = tab_header,
      value = model$IDs$tab,
      tags$div(
        bslib::layout_columns(
          col_widths = c(11, 1),
          class = "mb-0",
          HTML(paste0("<h4 class='formula'>", "Formula: ", model$formula, "</h4>")),
          tags$div(class = "d-flex align-items-end gap-2",
            bslib::tooltip(
              actionButton(
                inputId = ns(model$IDs$diagnos_btn),
                label = NULL,
                icon = icon("sliders-h", lib = "font-awesome"),
                class = "btn btn-sm btn-secondary"
              ),
              "Please check sampler diagnostics",
              id = ns(model$IDs$diagnos_tooltip),
              placement = "left",
              options = list(trigger = "manual")
            ),
            bslib::popover(
              actionButton(
                inputId = ns(model$IDs$save_popover_btn),
                label = NULL,
                icon = icon("download"),
                class = "btn btn-sm btn-secondary"
              ),
              title = "Save Options",
              downloadButton(
                outputId = ns(model$IDs$save_code_btn),
                label = "Model Code",
                icon = NULL,
                style = "width: 100%; margin-bottom: 5px;"
              ),
              downloadButton(
                outputId = ns(model$IDs$save_fit_btn),
                label = "Estimation Result",
                icon = NULL,
                style = "width: 100%;"
              ),
              placement = "left"
            )
          )
        ),
        tags$p(paste0("A binomial model with a logit function of the positive response rate. ",
                       "Samples are generated using ", model$sampling$n_chains, " chains with ", model$sampling$n_iter / 2, " post-warmup iterations each."), class = "fst-italic small"),
        actionButton(
          inputId = ns(model$IDs$postprocess_btn),
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
          if(nrow(model$params$fixed) > 0) {
            tags$div(
              tags$h4("Fixed Effects", class = "break_title"),
              tags$hr(class = "break_line"),
              tableOutput(ns(model$IDs$fixed_tbl))
            )
          },
          if(nrow(model$params$varying) > 0) {
            tags$div(
              tags$h4("Standard Deviation of Varying Effects", class = "break_title"),
              tags$hr(class = "break_line"),
              tableOutput(ns(model$IDs$varying_tbl))  
            )
          },
          tags$div(
            tags$h4("Posterior Predictive Check", class = "break_title"),
            tags$hr(class = "break_line"),
            bslib::card(
              bslib::card_header(tags$b("Note")),
              bslib::card_body(tags$p("The plot shows the positive response rate/proportion computed from the observed data and 10 sets of replicated data.")) 
            ),
            plotOutput(outputId = ns(model$IDs$ppc_plot))
          )
        )
      )
    )
  )
  
}

reset_inputs <- function(vars) {
  updateVirtualSelect(
    inputId = "fixed",
    choices = vars$fixed,
    selected = NULL
  )
  
  updateVirtualSelect(
    inputId = "varying",
    choices = vars$varying,
    selected = NULL
  )
  
  updateVirtualSelect(
    inputId = "interaction",
    choices = list(),
    selected = NULL
  )
  
  shinyjs::reset("predictor_select")
  shinyjs::reset("iter_select")
  shinyjs::reset("iter_kb")
  shinyjs::reset("chain_select")
  shinyjs::reset("seed_select")
  shinyjs::reset("spec_kb")
  shinyjs::reset("sens_kb")
  shinyjs::reset("fit_upload")
}

start_busy <- function(session, id, label) {
  updateActionButton(
    session = session,
    inputId = id,
    label = label,
    icon = icon("spinner", class = "fa-spin")
  )

  shinyjs::disable(id)
  waiter::waiter_show(
    html = waiter_ui(),
    color = waiter::transparent(0)
  )
}

stop_busy <- function(session, id, label, success) {
  updateActionButton(
    session = session,
    inputId = id,
    label = label,
    icon = if(success) icon("check") else icon("exclamation-triangle"),
  )

  shinyjs::enable(id)
  waiter::waiter_hide()
}
