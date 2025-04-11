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
  
  return(list(flag, msg))
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
check_fit_object <- function(model, expected_format, expected_link_geo) {
  message <- ""
  
  # Check if the model object has a data_format field
  if(!("data_format" %in% names(model))) {
    message <- "The uploaded file does not contain a model estimation result."
  } 
  # Check if the model's data_format matches the expected format
  else if(model$data_format != expected_format) {
    message <- sprintf("The uploaded file contains model estimation for %s instead of %s.",
                        data_format_label(model$data_format),
                        data_format_label(expected_format))

  }
  
  return(message)
}

create_text_box <- function(title, content) {
  tags$div(
    class = "panel panel-default",
    tags$div(
      class = "panel-heading",
      title
    ),
    tags$div(
      class = "panel-body",
      content
    )
  )
}

create_drag_item <- function(s) {
  HTML(paste0("<p><span class='glyphicon glyphicon-move'></span> <strong>", s, "</strong></p>"))
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
    waiter::spin_1()
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
      title = "Workflow",
      value = "workflow",
      tags$p("The interface offers several versions designed to handle different use cases and data types. The two main categories are ", tags$b("time-varying"), " and ", tags$b("cross-sectional"), ". Within each category, we support specific applications such as COVID data and polling data, as well as general cases."),
      tags$p("Regardless of which version you choose, the application follows a standard statistical analysis workflow:"),
      tags$ul(
        tags$li(tags$b("Data Validation"), ": The interface validates and displays the input data."),
        tags$li(tags$b("Descriptive Statistics"), ": Visual representations of descriptive statistics are presented."),
        tags$li(tags$b("Model Specification"), ": Users can specify and fit various models with different covariates and fixed/varying effects. Comprehensive diagnostics are provided to evaluate models, compare alternatives, and identify the optimal specification."),
        tags$li(tags$b("Results Visualization"), ": Interactive graphs demonstrate estimates for the target population, including demographic and geographic subpopulations for the selected model.")
      )
    ),
    
    # Uploading Data panel
    bslib::accordion_panel(
      title = "Uploading Data",
      value = "upload",
      tags$p("Example data templates are available in the 'Upload Data' tab.",
            class = "fst-italic"),
      tags$p("The interface accepts data in two formats:",
            tags$ul(
              tags$li(tags$b("Individual-level"), ": Each row contains information about a single person"),
              tags$li(tags$b("Aggregated"), ": Each row contains population counts for unique combinations of relevant geographic-demographic factors (e.g., White males between 18-30 years old living in Michigan)")
            ),
            "The aggregated format is preferred due to its computational advantages. Individual-level data uploaded to the system is automatically converted to the aggregated format. Multilevel Regression and Poststratification (MRP) requires matching categorical values between the input data and the target population data (ACS data). Given this requirement, the interface currently expects the following categorizations in the input data (values in parentheses indicate the corresponding expected values):"),
      
      tags$h5("Cross-sectional > Poll Data", class = "fw-bold mt-3"),
      tags$ul(
        tags$li("Sex: male, female"),
        tags$li("Race: Black, White, other"),
        tags$li("Age: 18-29, 30-39, 40-49, 50-59, 60-69, 70+"),
        tags$li("Education level: below high school (no hs), high school (hs), some college, 4-year college, post-grad"),
        tags$li("State: Full state names (Michigan), standard abbreviations (MI), and FIPS codes (26) are all accepted")
      ),
      
      tags$h5("Cross-sectional > General", class = "fw-bold mt-3"),
      tags$ul(
        tags$li("Sex: male, female"),
        tags$li("Race: Black, White, other"),
        tags$li("Age: 0-17, 18-34, 35-64, 65-74, 75+"),
        tags$li("ZIP code: Each ZIP code is treated as a distinct category"),
        tags$li("County: Each county is treated as a distinct category (5-digit FIPS codes required due to non-uniqueness of county names)"),
        tags$li("State: Full state names (Michigan), standard abbreviations (MI), and FIPS codes (26) are all accepted")
      ),
      
      tags$h5("Time-varying > COVID", class = "fw-bold mt-3"),
      tags$ul(
        tags$li("Sex: male, female"),
        tags$li("Race: Black, White, other"),
        tags$li("Age: 0-17, 18-34, 35-64, 65-74, 75+"),
        tags$li("ZIP code: Each ZIP code is treated as a distinct category"),
        tags$li("Week index: Test result dates are grouped into weeks, with index 1 assigned to the earliest week in the dataset. An optional column containing the date of the first day of each week can be included for visualization purposes. Currently, the only accepted date format is yyyy-mm-dd.")
      ),
      
      tags$h5("Time-varying > General", class = "fw-bold mt-3"),
      tags$ul(
        tags$li("Sex: male, female"),
        tags$li("Race: Black, White, other"),
        tags$li("Age: 0-17, 18-34, 35-64, 65-74, 75+"),
        tags$li("ZIP code: Each ZIP code is treated as a distinct category"),
        tags$li("County: Each county is treated as a distinct category (5-digit FIPS codes required due to non-uniqueness of county names)"),
        tags$li("State: Full state names (Michigan), standard abbreviations (MI), and FIPS codes (26) are all accepted"),
        tags$li("Week index: Test result dates are grouped into weeks, with index 1 assigned to the earliest week in the dataset. An optional column containing the date of the first day of each week can be included for visualization purposes. Currently, the only accepted date format is yyyy-mm-dd.")
      ),
      
      tags$p("For individual-level data, the interface attempts to convert raw values to the expected categories (e.g., numeric age to age bracket, date to week index) before grouping the data. Users may manually aggregate their raw data if the interface cannot preprocess it correctly or if they require more control over the process. Users familiar with R can download the preprocessing code from the Learn > Preprocess page and modify it as needed. The preprocessed or aggregated data must conform to the categorical values described above to be accepted by the system. Column names must also match expected formats, as the interface uses these to extract data from the input table. The application will automatically correct minor deviations such as letter case.")
    ),
    
    # Model Selection panel
    bslib::accordion_panel(
      title = "Model Selection",
      value = "model_spec",
      tags$p("The interface allows users to select predictors and their potential two-way interactions for inclusion in the model, and to specify prior distributions for model parameters. Predictors can include both individual-level and geographic-area-level measures."),
      
      tags$h5("Default Priors", class = "fw-bold mt-3"),
      tags$p("The interface treats interaction terms involving nominal measures with more than two categories as varying effects. The model assumes varying effects follow a normal distribution with an unknown standard deviation. Under the Bayesian framework, the following prior distributions are assigned by default:"),
      tags$ul(
        tags$li("Overall intercept: ", withMathJax(sprintf("\\(%s\\)", GLOBAL$default_priors$Intercept))),
        tags$li("Coefficient: ", withMathJax(sprintf("\\(%s\\)", GLOBAL$default_priors$fixed))),
        tags$li("Standard deviation (main effect): ", withMathJax(sprintf("\\(%s\\)", gsub("\\(", "_+(", GLOBAL$default_priors$varying))), "*"),
        tags$li("Standard deviation (interaction): ", withMathJax(sprintf("\\(%s\\)", gsub("\\(", "_+(", GLOBAL$default_priors$interaction))), "*")
      ),
      
      tags$h5("Available Priors", class = "fw-bold mt-3"),
      tags$p("Prior specifications can be modified by users. The interface currently accepts the following distributions:"),
      tags$ul(
        tags$li("normal(mu, sigma)"),
        tags$li("student_t(nu, mu, sigma)"),
        tags$li("structured**")
      ),
      
      tags$p("The syntax mimics the ",
            tags$a("distribution statements",
                    href = "https://mc-stan.org/docs/functions-reference/unbounded_continuous_distributions.html",
                    target = "_blank"),
            " in Stan. In addition to standard distributions, we provide the structured prior distribution developed by ",
            tags$a("Si et al. (2020)",
                    href = "https://arxiv.org/abs/1707.08220",
                    target = "_blank"),
            " which can be assigned to three types of two-way interactions:",
            tags$ol(
              tags$li("Two categorical variables (both with more than two levels)"),
              tags$li("One categorical variable (with more than two levels) and one binary variable"),
              tags$li("One categorical variable (with more than two levels) and one continuous variable")
            ),
            "This approach requires the main effect of the categorical variable with more than two levels to be included as a varying effect."),
      
      tags$ul(class = "list-unstyled",
        tags$li(withMathJax("Main effect of race: \\(normal(0, \\lambda_1\\sigma_{race})\\)")),
        tags$li(withMathJax("Main effect of age: \\(normal(0, \\lambda_1\\sigma_{age})\\)")),
        tags$li(withMathJax("Interaction of race and age: \\(normal(0, \\lambda_1\\lambda_2\\sigma_{age}\\sigma_{race}\\))")),
        tags$li(withMathJax("Standard deviation of main effects (\\(\\sigma_{race}, \\sigma_{age}\\)): \\(normal_+(0, 1)\\)")),
        tags$li(withMathJax("Global scale (\\(\\lambda_1\\)): \\(cauchy_+(0, 1)\\)"), "*"),
        tags$li(withMathJax("Local scale (\\(\\lambda_2\\)): \\(normal_+(0, 1)\\)"), "*")
      ),
      
      tags$div(class = "text-muted small mt-3",
              withMathJax("*The plus sign indicates that the distributions are restricted to positive values. For example, \\(normal_+(0, 3)\\) is a normal distribution with mean 0 and standard deviation of 3 restricted to positive values.")),
      
      tags$div(class = "text-muted small mt-2",
              withMathJax(sprintf("**The default priors for the global scale and local scale are \\(%s\\) and \\(%s\\) respectively. These cannot be changed at the moment.",
                                  gsub("\\(", "_+(", GLOBAL$default_priors$global_scale),
                                  gsub("\\(", "_+(", GLOBAL$default_priors$local_scale))))
    ),
    
    # Model Fitting panel
    bslib::accordion_panel(
      title = "Model Fitting",
      value = "model_fit",
      tags$p("The interface employs a Bayesian framework and implements Markov chain Monte Carlo (MCMC) algorithms for posterior computation via Stan. MCMC chains are run in parallel for computational efficiency, with the interface automatically allocating one processing core per chain. We recommend that users carefully specify the number of MCMC chains based on their available computing resources."),
      tags$p("Detailed model specifications for spatio-temporal data (accounting for outcome measurement sensitivity and specification) and cross-sectional data are available on the Learn > MRP page.")
    )
  )
}

show_guide <- function(open = NULL) {
  showModal(
    modalDialog(
      title = "User Guide",
      create_guide(open),
      size = "xl",
      easyClose = TRUE
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
      tags$div(class = "pad_top",
        bslib::layout_columns(
          col_widths = c(10, 2),
          HTML(paste0("<h4>", "Formula: ", model$formula, "</h4>")),
          tags$div(style = "float: right;",
            dropdown(
              label = "Save Model",
              circle = FALSE,
              block = TRUE,
              width = "100%",
              downloadButton(
                outputId = ns(model$IDs$save_code_btn),
                label = "Code",
                icon = icon("download", "fa"),
                style = "width: 100%; margin-bottom: 5px; padding: 0px auto;"
              ),
              downloadButton(
                outputId = ns(model$IDs$save_fit_btn),
                label = "Result",
                icon = icon("download", "fa"),
                style = "width: 100%; padding: 0px auto;"
              )
            )
          )
        ),
        tags$p(paste0("A binomial model with a logit function of the positive response rate. ",
                       "Samples are generated using ", model$n_chains, " chains with ", model$n_iter / 2, " post-warmup iterations each."), class = "fst-italic small"),
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
          if(nrow(model$fixed) > 0) {
            tags$div(
              tags$h4("Fixed Effects", class = "break_title"),
              tags$hr(class = "break_line"),
              tableOutput(ns(model$IDs$fixed))
            )
          },
          if(nrow(model$varying) > 0) {
            tags$div(
              tags$h4("Standard Deviation of Varying Effects", class = "break_title"),
              tags$hr(class = "break_line"),
              tableOutput(ns(model$IDs$varying))  
            )
          },
          tags$div(
            tags$h4("Posterior Predictive Check", class = "break_title"),
            tags$hr(class = "break_line"),
            bslib::card(
              bslib::card_header(tags$b("Note")),
              bslib::card_body(tags$p("The plot shows the positive response rate/proportion computed from the observed data and 10 sets of replicated data.")) 
            ),
            plotOutput(outputId = ns(model$IDs$ppc))
          )
        )
      )
    )
  )
  
}

reset_inputs <- function(vars) {
  updateVirtualSelect(
    inputId = "fixed",
    choices = vars$fixed
  )
  
  updateVirtualSelect(
    inputId = "varying",
    choices = vars$varying
  )
  
  updateVirtualSelect(
    inputId = "interaction",
    choices = list()
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
