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

show_guide <- function(section, session) {
  holder <- list(
    workflow = "",
    upload_data = "",
    model_select = "",
    model_fit = ""
  )
  
  holder[[section]] <- " open"

  showModal(
    modalDialog(
      title = tagList(icon("circle-question", "fa"), "Guide"),
      size = "l",
      easyClose = TRUE,
      tags$div(class = "guide",
        HTML(stringr::str_interp("<details${holder$workflow}><summary class='summary'>Workflow</summary>")),
        tags$div(class = "guide_section_body",
          tags$p("The interface implements a complete workflow of statistical analyses, from data description, model fitting, diagnostics, to result presentation."),
          tags$ul(
           tags$li("First, the interface reads and displays the input data."),
           tags$li("Second, it presents descriptive statistics of the patient records and geographic areas, where examples of raw data are also displayed."),
           tags$li("Third, users can specify and fit different models with various choices of covariates and fixed/varying effects. The model fitting is via Bayesian computation with Markov chain Monte Carlo algorithms in Stan. Model summaries are shown."),
           tags$li("Fourth, the interface shows a comparison between different models and presents their diagnostics results."),
           tags$li("Finally, interface graphs demonstrate the estimated infection rate over time for the targeted population and demographic and geographic subpopulations, for the selected model."),
          )         
        ),
        HTML("</details>"),
        HTML(stringr::str_interp("<details${holder$upload_data}><summary class='summary pad_top'>Uploading Data</summary>")),
        tags$div(class = "guide_section_body",
          tags$p(" Examples of expected data input are provided at data upload window.", style = "font-style: italic; font-size: 1.1em;"),
          tags$p("The interface expect the input data to be in either of the following forms:",  tags$b("individual-level"), "in which each row contains information about an individual person and",  tags$b("aggregated"), "which contains the population count for each unique combination of relevant geographic-demographic factors. The latter is preferred as it offers computational advantages over its counterpart. The statistical models generated by the interface requires categorical quantities. In the future, users will be able to specify the categories to convert raw values into, but the interface currently expects the following grouping in the input data (The values in parentheses are the corresponding values expected in the input data)."),
          tags$h5("Cross-sectional Data", class = "guide_subheader"),
          tags$ul(
          tags$li("Sex: male, female"),
          tags$li("Race: Black, White, Hispanic, other"),
          tags$li("Age (in years): 18-29, 30-39, 40-49, 50-59, 60-69, 70+"),
          tags$li("Education level: below high school (no hs), high school (hs), some college, 4-year college, post-grad"),
          tags$li("State: each state is treated as a category. Full names, abbreviations and FIPS codes are accepted.")
          ),
          tags$h5("Spatio-temporal Data", class = "guide_subheader"),
          tags$ul(
          tags$li("Sex: male, female"),
          tags$li("Race: Black, White, other"),
          tags$li("Age (in years): 0-17, 18-34, 35-64, 65-74, 75+"),
          tags$li("ZIP code: each ZIP code is treated as a category"),
          tags$li("Week index: test result dates are grouped into weeks with index 1 assigned to the earliest week in the data. An optional column containing the date of the first day of each week can be included for visualization purposes. Currently, the only accepted format is yyyy-mm-dd.")
          ),
          tags$p("Users can manually aggregate their raw data if the interface cannot preprocess it or if they want more control over the process. Users familiar with R can download the preprocessing code from the Learn > Preprocess page and modifies it as needed. The preprocessed or aggregated data must have the categorical values as described above in the right format or it will not be accepted. It also must have the right column names as the interface uses one to extract data from the input table but it can automatically fix minor deviations such as letter case.")        
        ),
        HTML("</details>"),
        HTML(stringr::str_interp("<details${holder$model_select}><summary class='summary pad_top'>Model Selection</summary>")),
        tags$div(class = "guide_section_body",
          tags$p("The interface allows users to select predictors and their potential two-way interactions to be included in the model and introduce prior distributions to model parameters. The predictors can include individual-level and geographic-area-level measures."),
          tags$h5("Default priors", class = "guide_subheader"),
          tags$p("The interface treats the interaction terms that involve nominal measures with more than two categories as varying effects. The model assumes varying effects follow a normal distribution with an unknown standard deviation. The Bayesian framework assigns the following prior distributions by default as below:"),
          tags$ul(
           tags$li("Overall intercept: ", withMathJax(sprintf("\\(%s\\)", GLOBAL$default_priors$Intercept))),
           tags$li("Coefficient: ", withMathJax(sprintf("\\(%s\\)", GLOBAL$default_priors$fixed))),
           tags$li("Standard deviation (main effect): ", withMathJax(sprintf("\\(%s\\)", gsub("\\(", "_+(", GLOBAL$default_priors$varying))), "*"),
           tags$li("Standard deviation (interaction): ", withMathJax(sprintf("\\(%s\\)", gsub("\\(", "_+(", GLOBAL$default_priors$interaction))), "*")
          ),
          tags$h5("Available priors", class = "guide_subheader"),
          tags$p("The prior specification can be modified by users. The interface currently accepts the following distributions:"),
          tags$ul(
           tags$li("normal(mu, sigma)"),
           tags$li("student_t(nu, mu, sigma)"),
           tags$li("structured**")
          ),
          tags$p("The syntax imitates the ", tags$a("distribution statement", href = "https://mc-stan.org/docs/functions-reference/unbounded_continuous_distributions.html", target = "_blank"), " in Stan. In addition to the standard distributions, we provide the structured prior distribution developed by", tags$a("Si et al. (2020)", href = "https://arxiv.org/abs/1707.08220", target = "_blank"), "which can be assigned to three types of two-way interactions: 1)  two categorical variables (both with more than two levels), 2) one categorical variable (with more than two levels) and one binary variable and 3) one categorical variable (with more than two levels) and one continuous variable. It requires the main effect of the categorical variable with more than two levels to be included as a varying effect. Below is an example of the structured prior distribution for the two-way interaction of race and age:"),
          tags$ul(style = "list-style: none",
            tags$li(withMathJax("Main effect of race: \\(normal(0, \\lambda_1\\sigma_{race})\\)")),
            tags$li(withMathJax("Main effect of age: \\(normal(0, \\lambda_1\\sigma_{age})\\)")),
            tags$li(withMathJax("Interaction of race and age: \\(normal(0, \\lambda_1\\lambda_2\\sigma_{age}\\sigma_{race}\\))")),
            tags$li(withMathJax("Standard deviation of main effects (\\(\\sigma_{race}, \\sigma_{age}\\)): \\(normal_+(0, 1)\\)")),
            tags$li(withMathJax("Global scale (\\(\\lambda_1\\)): \\(cauchy_+(0, 1)\\)"), "*"),
            tags$li(withMathJax("Local scale (\\(\\lambda_2\\)): \\(normal_+(0, 1)\\)"), "*")
          )
        ),
        tags$div(class = "guide_math", withMathJax("*The plus sign indicates that the distributions are restricted to positive values. For example, \\(normal_+(0, 3)\\) is a normal distribution with mean 0 and standard deviation of 3 restricted to positive values.")),
        tags$div(class = "guide_math", withMathJax(sprintf("**The default priors for the global scale and local scale are \\(%s\\) and \\(%s\\) respectively. These cannot be changed at the moment.", gsub("\\(", "_+(", GLOBAL$default_priors$global_scale), gsub("\\(", "_+(", GLOBAL$default_priors$local_scale)))),
        HTML("</details>"),
        HTML(stringr::str_interp("<details ${holder$model_fit}><summary class='summary pad_top'>Model Fitting</summary>")),
        tags$div(class = "guide_section_body",
          tags$p("The interface relies on a Bayesian framework and implements the Markov chain Monte Carlo (MCMC) algorithm for posterior computation via Stan. The MCMC chains are run in parallel for computational efficiency, and the interface automatically uses one core for one chain. We recommend that users carefully specify the number of MCMC chains based on the available computing resources."),
          tags$p("The model details for the spatio-temporal data (accounting for outcome measurement sensitivity and specification) and the cross-sectional data are available on the Learn > MRP page.")
        ),
        HTML("</details>") 
      )
    ),
    session = session
  )
}

create_model_tab <- function(ns, model) {
  
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
  
  appendTab("navbar_model",
    select = TRUE,
    tabPanel(title = tab_header,
      value = model$IDs$tab,
      tags$div(class = "pad_top",
        fluidRow(
          column(width = 10,
            HTML(paste0("<h4>", "Formula: ", model$formula, "</h4>"))
          ),
          column(width = 2,
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
          )
        ),
        tags$h5(paste0("A binomial model with a logit function of the positive response rate. ",
                       "Samples are generated using ", model$n_chains, " chains with ", model$n_iter / 2, " post-warmup iterations each.")),
        actionButton(
          inputId = ns(model$IDs$postprocess_btn),
          label = "Run poststratification"
        ),
        tags$div(style = "margin-top: 30px",
          create_text_box(
            title = tags$b("Note"),
            tags$ul(
              tags$li("Large ", tags$a("R-hat", href = "https://mc-stan.org/learn-stan/diagnostics-warnings.html#r-hat", target = "_blank"), " (e.g., greater than 1.05) values indicate that the computation has not yet converged, and it is necessary to run more iterations and/or modify model and prior specifications."),
              tags$li("Low values for ", tags$a("Bulk-ESS", href = "https://mc-stan.org/learn-stan/diagnostics-warnings.html#bulk-and-tail-ess", target = "_blank"), " and ", tags$a("Tail-ESS", href = "https://mc-stan.org/learn-stan/diagnostics-warnings.html#bulk-and-tail-ess", target = "_blank"), " (ESS stands for Effective Sample Size) also suggest that more iterations are required.")
            )      
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
            create_text_box(
              title = tags$b("Note"),
              tags$p("The plot shows the positive response rate/proportion computed from the observed data and 10 sets of replicated data.") 
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
