#' User Guide Interface Module UI Function
#'
#' @description Creates the user interface for the comprehensive user guide page.
#' Provides detailed documentation about the MRP application workflow, data
#' requirements, model specification, and visualization options. Includes
#' information about data formats, preprocessing steps, geographic linking,
#' and Bayesian model fitting procedures.
#'
#' @param id Character string. The module's namespace identifier.
#'
#' @return A `bslib::layout_columns` containing the user guide interface with:
#' \itemize{
#'   \item Comprehensive workflow documentation
#'   \item Data upload requirements and format specifications
#'   \item Model specification and prior distribution information
#'   \item Geographic linking and visualization guidelines
#' }
#'
#' @noRd
#' @keywords internal
mod_learn_interface_ui <- function(id){
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(-3, 6),
    tags$div(
      tags$hr(class = "header_top"),
      tags$h3("MRP Interface User Guide"),
      tags$hr(class = "header_bottom"),
      tags$h4("Workflow"),
      tags$p("The interface provides different modules tailored to various use cases, categorized into ", tags$b("time-varying"), " and ", tags$b("cross-sectional"), " analyses with ", tags$b("binary"), " or ", tags$b("continuous"), " outcomes. Each category supports specific applications, such as COVID-19 transmission tracking and public opinion polling, along with broader use cases. Regardless of the module, the analytic workflow is consistent and follows these core steps:"),
      tags$ul(
        tags$li(tags$b("Data Cleaning:"), " Preprocess and display the uploaded data."),
        tags$li(tags$b("Descriptive Statistics:"), " Visualize key summary statistics."),
        tags$li(tags$b("Model Building:"), " Specify and fit models with customizable predictors and varying or fixed effects. The application provides diagnostics to aid model evaluation and selection."),
        tags$li(tags$b("Result Visualization:"), " Generate plots to present estimates for the overall population and across demographic or geographic subgroups.")
      ),
      
      tags$h4("Uploading Data", class = "mt-5"),
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
      tags$p("Linking to the ACS enables estimation of approximate population counts for poststratification. Users may select geographic levels and ACS reference years, with built-in constraints for some use cases (e.g., COVID-19 or public opinion polls). Currently, COVID-19 data are linked to five-year ACS (2017-2021) at the ZIP code level, and polling data to five-year ACS (2014-2018) at the state level. General modules offer additional linking options."),


      tags$h4("Visualization", class = "mt-5"),
      tags$p("The interface maintains a unified, responsive layout throughout the workflow. In both the descriptive statistics and result sections, a sidebar with selection inputs allows users to:"),
      tags$ol(
        tags$li("Choose a main plot type."),
        tags$li("Select relevant sub-categories as they appear."),
        tags$li("Fine-tune geographic visualizations with options such as plot type and data subsetting.")
      ),
      tags$p("Selection options depend on the current module; for instance, histograms of geographic covariates such as urbanicity and ADI are unique to the COVID-19 workflow."),
      
      tags$h4("Model Specification", class = "mt-5"),
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
        tags$li("structured**"),
        tags$li("icar"),
        tags$li("bym2")
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
      tags$p("**The default priors for the global scale and local scale are ", withMathJax("\\(cauchy_+(0, 1)\\)"), " and ", withMathJax("\\(normal_+(0, 1)\\)"), " respectively. These cannot be changed at the moment.", class = "small text-muted"),
      
      tags$h4("Model Fitting", class = "mt-5"),
      tags$p("The interface uses Markov chain Monte Carlo (MCMC) algorithms via ", tags$a("Stan", href = "https://mc-stan.org/", target = "_blank"), " for posterior computation under a Bayesian framework. The Stan code is generated for compilation with the package ", tags$a("CmdStanR", href = "https://mc-stan.org/cmdstanr/", target = "_blank"), ". MCMC chains run in parallel, with each chain allocated to a processor core. Choose the number of chains according to your computing resources for optimal performance."),
      tags$p("Large datasets and complex models increase computational demands. The application addresses this by:"),
      tags$ul(
        tags$li(tags$b("Separated stages:"), " Multilevel Regression (MR) and Poststratification (P) are separated, allowing model diagnostics prior to poststratification. Results may be saved at any workflow stage."),
        tags$li(tags$b("Optimized computation:"), " Vectorized Stan (C++) routines are used for poststratification to enhance runtime efficiency.")
      ),
      tags$p("Detailed information and advanced topics are available on the ", tags$b("Learn > MRP"), " page."),
      
      tags$h4("Result Visualization", class = "mt-5"),
      tags$p("The interface visualizes estimates for the overall population, demographic groups, and geographic regions. For spatial results, users can create interactive maps. For time-varying data, the results will be time-specific.")
    )
  )
}

#' User Guide Interface Module Server Function
#'
#' @description Server logic for the user guide interface module. Handles
#' navigation events, renders example data tables, and provides download
#' handlers for preprocessing code and example datasets. Manages display
#' of cross-sectional and time-varying data examples with appropriate
#' formatting and file downloads.
#'
#' @param id Character string. The module's namespace identifier.
#' @param global Reactive values object containing global application state
#'
#' @return Server function for the user guide module. Creates download handlers
#' for example data and preprocessing code, renders data tables, and manages
#' navigation between guide sections.
#'
#' @noRd
#' @keywords internal
mod_learn_interface_server <- function(id, global){
  moduleServer( id, function(input, output, session){
  })
}
