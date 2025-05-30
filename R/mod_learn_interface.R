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
#' @return A \code{bslib::layout_columns} containing the user guide interface with:
#' \itemize{
#'   \item Comprehensive workflow documentation
#'   \item Data upload requirements and format specifications
#'   \item Model specification and prior distribution information
#'   \item Geographic linking and visualization guidelines
#' }
#'
#' @noRd
#'
#' @importFrom shiny NS tagList withMathJax tags
mod_learn_interface_ui <- function(id){
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(-3, 6),
    tags$div(
      tags$h4("Workflow"),
      tags$p("The interface provides different modules tailored to various use cases, categorized into ", tags$b("time-varying"), " and ", tags$b("cross-sectional"), " analyses. Each category supports specific applications, such as COVID-19 transmission tracking and public opinion polling, along with broader use cases. Regardless of the chosen module, the application adheres to a standard statistical analysis workflow:"),
      tags$ul(
        tags$li(tags$b("Data Cleaning:"), " Preprocess and display the input data."),
        tags$li(tags$b("Descriptive Statistics:"), " Visualize key summary statistics."),
        tags$li(tags$b("Model Building:"), " Users can specify and fit models with various predictors and fixed or varying effects. The interface offers detailed diagnostics to evaluate and compare models, facilitating model selection."),
        tags$li(tags$b("Result Visualization:"), " Generate graphs to illustrate estimates for the target population and demographic and geographic subgroups for the selected model.")
      ),
      
      tags$h4("Uploading Data", class = "mt-5"),
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
      ),

      tags$h4("Graph Display", class = "mt-5"),
      tags$p("The application maintains a uniform visual layout throughout the workflow. In both descriptive statistics and estimation result sections, a sidebar with responsive selection inputs is present for easy navigation:"),
      tags$ol(
        tags$li("Choose a main plot category."),
        tags$li("Select relevant sub-categories as they appear."),
        tags$li("Customize geographic visualizations with distinct options, including plot types and data subsetting."),
      ),
      tags$p("Selection options vary by interface module. For example, histograms of geographic covariates like the urbanicity and Area Deprivation Index (ADI) are exclusive to the COVID-19 data analysis workflow."),

      tags$h4("Model Specification", class = "mt-5"),
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
                                  gsub("\\(", "_+(", GLOBAL$default_priors$local_scale)))),
      

      tags$h4("Model Fitting", class = "mt-5"),
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

#' User Guide Interface Module Server Function
#'
#' @description Server logic for the user guide interface module. Handles
#' navigation events, renders example data tables, and provides download
#' handlers for preprocessing code and example datasets. Manages display
#' of cross-sectional and time-varying data examples with appropriate
#' formatting and file downloads.
#'
#' @param id Character string. The module's namespace identifier.
#' @param global Reactive values object containing global application state,
#' including session information for navigation updates.
#'
#' @return Server function for the user guide module. Creates download handlers
#' for example data and preprocessing code, renders data tables, and manages
#' navigation between guide sections.
#'
#' @noRd
#'
#' @importFrom shiny moduleServer observeEvent updateTabsetPanel downloadHandler
mod_learn_interface_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$to_mrp, {
      updateTabsetPanel(global$session,
                        inputId = "navbar",
                        selected = "nav_learn_mrp")
    })

    output$example_cs <- DT::renderDT({
      readr::read_csv(app_sys("extdata/CES_data_aggregated.csv"), show_col_types = FALSE)
    })

    output$example_st <- DT::renderDT({
      readr::read_csv(app_sys("extdata/covid_test_records_aggregated.csv"), show_col_types = FALSE)
    })

    output$save_code_cs <- downloadHandler(
      filename = function() { "preprocess.R" },
      content = function(file) {
        readLines(app_sys("extdata/preprocess_cs.R")) %>% writeLines(file)
      }
    )

    output$save_code_st <- downloadHandler(
      filename = function() { "preprocess.R" },
      content = function(file) {
        readLines(app_sys("extdata/preprocess_st.R")) %>% writeLines(file)
      }
    )

    output$save_ex_w_state <- downloadHandler(
      filename = function() { "CES_data_aggregated.csv" },
      content = function(file) {
        readr::read_csv(app_sys("extdata/CES_data_aggregated.csv"), show_col_types = FALSE) %>% readr::write_csv(file)
      }
    )

    output$save_ex_wo_state <- downloadHandler(
      filename = function() { "CES_data_aggregated_wo_state.csv" },
      content = function(file) {
        readr::read_csv(app_sys("extdata/CES_data_aggregated_wo_state.csv"), show_col_types = FALSE) %>% readr::write_csv(file)
      }
    )

    output$save_ex_st <- downloadHandler(
      filename = function() { "covid_test_records_aggregated.csv" },
      content = function(file) {
        readr::read_csv(app_sys("extdata/covid_test_records_aggregated.csv"), show_col_types = FALSE) %>% readr::write_csv(file)
      }
    )

    output$save_week_table <- downloadHandler(
      filename = function() { "week_conversion.csv" },
      content = function(file) {
        readr::read_csv(app_sys("extdata/week_conversion.csv"), show_col_types = FALSE) %>% readr::write_csv(file)
      }
    )

    outputOptions(output, "example_cs", suspendWhenHidden = FALSE)
    outputOptions(output, "example_st", suspendWhenHidden = FALSE)
  })
}
