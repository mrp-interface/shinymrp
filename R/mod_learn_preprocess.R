#' learn_preprocess UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_preprocess_ui <- function(id){
  ns <- NS(id)
  bslib::page_fillable(
    bslib::layout_columns(
      col_widths = c(-3, 6),
      tags$div(class = "pad_top",
        tags$img(src = "www/preprocess.pdf", width = "100%", style = "margin-bottom: 40px;"),
        tags$p("The input data for MRP consists of three components: the preprocessed survey/test data, the poststratification table, and geographic covariates. The survey/test data contains the geographic-demographic information about participants and their survey responses or test results. Inferring the relationship between them using regression models constitutes the first stage of MRP. Additionally, geographic covariates are included in these models to account for the structured difference among geographic areas such as states and counties. The second stage of MRP is poststratification, which involves adjusting the estimates based on the breakdown of the target population. Below are details about how these data components are prepared for both interfaces."),
        tags$h3("Data Preparation for Cross-sectional Data"),
        tags$h4("Survey/Test Data"),
        tags$p("The current interface for cross-sectional data expects the following information in the data inputted by the user:"),
        tags$ul(
          tags$li("Sex"),
          tags$li("Race"),
          tags$li("Age"),
          tags$li("Education level/Highest degree attained"),
          tags$li("State (optional)")
        ),
        tags$p("The program first cleans the data by converting column names into snake case and character values into lowercase. It then imputes missing data and groups the raw values into the following categories:"),
        tags$ul(
          tags$li("Sex: male, female"),
          tags$li("Race: White, Black, other"),
          tags$li("Age: 0-17, 18-34, 35-64, 65-74, 75+"),
          tags$li("Education: below high school, high school, some college, 4-year college, post-grad")
        ),
        tags$h4("Poststratification Table"),
        tags$p("Following this ", tags$a("guide", href = "https://bookdown.org/jl5522/MRP-case-studies/introduction-to-mister-p.html", target = "_blank"), " by Juan Lopez-Martin, we obtain the American Community Survey data through IPUMS, convert the raw values to the same categories as the input data, and then find the count for each of the subgroups corresponding to unique combinations of the demographic factors: sex, race, age, education. If the program can find states in the input data, it automatically includes them in the poststratification table and produces state-level estimates."),
        tags$h4("Geographic Covariates"),
        tags$p("Currently, we do not retrieve this information from external data sources but, if there are variables with values unique to each state in the input data, the program automatically extracts them and allows them to be included in the models. In the future, we may draw from external data relevant state-level covariates that can improve the predictive power of the models."),
        tags$p("Below, you can download the preprocessing code and an example of the expected input data."),
        tags$div(
          class = "interface_buttons",
          downloadButton(
            outputId = ns("save_code_cs"),
            label = "Preprocessing code"
          ),
          downloadButton(
            outputId = ns("save_ex_cs"),
            label = "Example data"
          )
        ),
        tags$h3("Data Preparation for Spatio-temporal Data"),
        tags$p("To account for the difference between ZIP code areas in the models, we need quantities that are defined at the ZIP code level. Because ACS data are not provided at the ZIP code level, we obtain relevant quantities at the tract level and then use the USPS crosswalk table to obtain the ZIP code level covariates. Specifically, one ZIP code can span multiple tracts so we find the overlapping tracts for each ZIP code and aggregate the values. How the values are aggregated depends on the quantities and is described in detail below."),
        tags$h4("Survey/Test Data"),
        tags$p("The preprocessing code for spatio-temporal data currently works best with test records from hospitals that adopt the Epic system. The program can automatically identify the essential columns in data frames that follow the Epic naming convention. Specifically, the input data frame must contain columns corresponding to the following geographic-demographic factors and test information:"),
        tags$ul(
          tags$li("Sex"),
          tags$li("Race"),
          tags$li("Age"),
          tags$li("ZIP code"),
          tags$li("Test result"),
          tags$li("Date of test result (optional)")
        ),
        tags$p("For raw data with individual cells, a unique ID associated with each test record is also required to identify duplicates."),
        tags$p("One of the first steps in cleaning the data is filtering out zip codes and states with small sample sizes. Specifically, we omit samples from states that account for less than one percent of the data and then zip codes with five or fewer records. The expectation is that the vast majority of test samples were obtained from people living close to the hospital and our test datasets were consistent with this expectation. Next, we impute missing and invalid values for sex, race, and age based on the frequency of occurrence in the data. Finally, we aggregate the raw values for each of these demographic variables as follows:"),
        tags$ul(
          tags$li("Sex: male, female"),
          tags$li("Race: White, Black, other"),
          tags$li("Age: 0-17, 8-34, 35-64, 65-74, 75+")
        ),
        tags$p("Future versions will allow for specification of how the values are grouped but these are the default categories for the current version. Another future feature is allowing users to aggregate result dates into either weeks or months. The current implementation only allows for the former."),
        tags$h4("Poststratification Table"),
        tags$p("From the ACS data, we obtain the population size for each tract that is broken down into subgroups based on the aforementioned factors. An example of a subgroup would be white males who are between 8 and 34 years old, reside in the same tract, and receive the test result in the same week. We sum the population size over overlapping tracts for each zip code to get the poststratification cell counts for ZIP codes."),
        tags$h4("Geographic Covariates"),
        tags$p("We obtain the following tract-level measures from the ACS and other sources:"),
        tags$ul(
          tags$li("Binary indicators of whether tracts are classified as urban or not"),
          tags$li("Population sizes based on levels of education"),
          tags$li("Population sizes based on ratios of income to poverty level in the past 12 months"),
          tags$li("Population sizes based on employment status"),
          tags$li("Median household income in the past 12 months"),
          tags$li(tags$a("Area Deprivation Index (ADI)", href = "https://www.neighborhoodatlas.medicine.wisc.edu", target = "_blank"))
        ),
        tags$p("To obtain zip-level estimates, we combine them as follows:"),
        tags$ul(
          tags$li("Urbanicity of a zip code is defined as the percentage of covered census tracts classified as urban, weighted by tract population counts."),
          tags$li("Higher education measure of a zip code is defined as the percentage of the residing population who have earned an Associate's degree or higher."),
          tags$li("Poverty measure of a zip code is defined as the percentage of the residing population whose ratio of income to poverty level in the past 12 months is below 100%."),
          tags$li("Employment rate of a zip code is defined as the percentage of the residing population who are employed as a part of the civilian labor force."),
          tags$li("Income measure of a zip code is defined as the average value of tract-level median household income in the past 12 months, weighted by tract population counts."),
          tags$li("Area Deprivation Index (ADI) of a zip code is the average ADI across covered census tracts, weighted by tract population counts")
        ),
        tags$p("Below, you can download the preprocessing code, an example of the expected input data, and a table for converting dates to week indices."),
        tags$div(
          class = "interface_buttons",
          downloadButton(
            outputId = ns("save_code_st"),
            label = "Preprocessing code"
          ),
          downloadButton(
            outputId = ns("save_ex_st"),
            label = "Example data"
          ),
          downloadButton(
            outputId = ns("save_week_table"),
            label = "Week conversion table"
          )
        )
      )
    )
  )
}

#' learn_preprocess Server Functions
#'
#' @noRd
mod_learn_preprocess_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$save_code_cs <- downloadHandler(
      filename = function() { "preprocess_crosssectional.R" },
      content = function(file) {
        readLines(app_sys("extdata/preprocess_cs.R")) |> writeLines(file)
      }
    )
    
    output$save_code_st <- downloadHandler(
      filename = function() { "preprocess_spatiotemporal.R" },
      content = function(file) {
        readLines(app_sys("extdata/preprocess_st.R")) |> writeLines(file)
      }
    )
    
    output$save_ex_cs <- downloadHandler(
      filename = function() { "CES_data_aggregated.csv" },
      content = function(file) {
        read.csv(app_sys("extdata/CES_data_aggregated.csv")) |> readr::write_csv(file)
      }
    )
    
    output$save_ex_st <- downloadHandler(
      filename = function() { "covid_test_records_aggregated.csv" },
      content = function(file) {
        read.csv(app_sys("extdata/covid_test_records_aggregated.csv")) |> readr::write_csv(file)
      }
    )
    
    output$save_week_table <- downloadHandler(
      filename = function() { "week_conversion.csv" },
      content = function(file) {
        read.csv(app_sys("extdata/week_conversion.csv")) |> readr::write_csv(file)
      }
    )

  })
}
