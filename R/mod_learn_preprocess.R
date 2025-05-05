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
  bslib::layout_columns(
    col_widths = c(-3, 6),
    tags$div(
      tags$p("The input data for MRP consists of two major components: the survey or test data and the corresponding poststratification table. The survey/test data contains the geographic-demographic information about participants and their survey responses or test results. Inferring the relationship between these variables using multilevel regression (MR) models constitutes the first stage of MRP. Additionally, geographic covariates can be included in these models to account for the structured differences among geographic areas such as states and counties. The second stage of MRP, poststratification (P), involves adjusting the estimates based on the demographic-geographic composition of the target population, which is the US population in the context of this application. Below are details about how these data components are prepared for all use cases of the MRP interface."),
      
      tags$h3("Survey/Test Data Preprocessing", class = "mt-5"),
      tags$p("The preprocessing workflow follows these general steps for both cross-sectional and spatio-temporal data:"),
      tags$ul(
        tags$li("Data cleaning: standardizing column names, converting character values to lowercase, handling missing/unknown data, standardizing ZIP and FIPS codes"),
        tags$li("Converting raw values to categorical data: recoding categorical data, converting numeric values to categories using predefined intervals, assigning week indices to dates"),
        tags$li("Data imputation: imputing missing data based on frequency distributions of converted categories in the data"),
        tags$li("Data aggregation: aggregating individual-level records to create cell counts for each unique combination of relevant demographic-geographic factors")
      ),
      tags$p("The result is a cross tabulation of the following categories:"),
      bslib::layout_columns(
        col_widths = c(6, 6),

        # Time-varying card
        bslib::card(
          bslib::card_header(tags$h4("Time-varying")),
          bslib::card_body(
            tags$h5("COVID Data"),
            tags$ul(
              tags$li("Sex: male, female"),
              tags$li("Race: Black, White, other"),
              tags$li("Age: 0-17, 18-34, 35-64, 65-74, 75+"),
              tags$li("ZIP code"),
              tags$li("Week indices")
            ),
            
            tags$h5("General", class = "mt-3"),
            tags$ul(
              tags$li("Sex: male, female"),
              tags$li("Race: Black, White, other"),
              tags$li("Age: 0-17, 18-34, 35-64, 65-74, 75+"),
              tags$li("ZIP codes (optional)"),
              tags$li("Counties (optional)"),
              tags$li("States (optional)"),
              tags$li("Week indices")
            )
          )
        ),
        
        # Cross-sectional card
        bslib::card(
          bslib::card_header(tags$h4("Cross-sectional")),
          bslib::card_body(
            tags$h5("Poll Data"),
            tags$ul(
              tags$li("Sex: male, female"),
              tags$li("Race: Black, White, other"),
              tags$li("Age: 18-29, 30-39, 40-49, 50-59, 60-69, 70+"),
              tags$li("Education level: below high school, high school, some college, 4-year college, post-grad"),
              tags$li("State")
            ),
            
            tags$h5("General", class = "mt-3"),
            tags$ul(
              tags$li("Sex: male, female"),
              tags$li("Race: Black, White, other"),
              tags$li("Age: 0-17, 18-34, 35-64, 65-74, 75+"),
              tags$li("ZIP codes (optional)"),
              tags$li("Counties (optional)"),
              tags$li("States (optional)"),
            )
          )
        )
      ),
      tags$p("Below are example scripts and data files for data preprocessing."),
      div(class = "d-flex justify-content-between",
        downloadButton(ns("save_code"), "Preprocessing Script"),
        downloadButton(ns("save_geo_conversion"), "Geography Conversion Table"),
        downloadButton(ns("save_week_conversion"), "Week Conversion Table")
      ),
      tags$h3("Geographic Identifiers & Covariates", class = "mt-5"),
      tags$p("One of the major strengths of MRP is small-area estimation. To leverage this capability, information about geographic areas and their characteristics should be included so that the model can account for related group-level variation in the data. To this end, the interface gathers as much geographic information as possible based on what is provided in the data."),
      tags$p("First, it identifies geographic units at larger scales that are not present in the data. The application automatically identifies the smallest geographic units in the data and infers corresponding units at larger scales. For example, if the data contains ZIP codes, the application will automatically find the county and state that overlaps with each ZIP code the most."),
      tags$p("In addition to geographic areas, the application also gathers quantities associated with them either from the data or external sources. For use cases other than COVID data, the app scans the data to find quantities that have a one-to-one relationship with the geographic identifier of interest. For COVID data, we have identified certain ZIP code-level quantities that are informative in modeling COVID test results, such as education level and household income. We obtain these quantities at the tract level from the ACS and other sources, then aggregate over the tracts that overlap with each ZIP code based on the USPS crosswalk table."),
      tags$p("We obtain the following tract-level measures from the ACS and other sources:"),
      tags$ul(
        tags$li("Binary indicators of whether tracts are classified as urban or not"),
        tags$li("Population sizes based on levels of education"),
        tags$li("Population sizes based on ratios of income to poverty level in the past 12 months"),
        tags$li("Population sizes based on employment status"),
        tags$li("Median household income in the past 12 months"),
        tags$li(tags$a("Area Deprivation Index (ADI)", href = "https://www.neighborhoodatlas.medicine.wisc.edu", target = "_blank"))
      ),
      
      tags$p("To obtain ZIP code-level estimates, we combine them as follows:"),
      tags$ul(
        tags$li("Urbanicity of a ZIP code is defined as the percentage of covered census tracts classified as urban, weighted by tract population counts."),
        tags$li("Higher education measure of a ZIP code is defined as the percentage of the residing population who have earned an Associate's degree or higher."),
        tags$li("Poverty measure of a ZIP code is defined as the percentage of the residing population whose ratio of income to poverty level in the past 12 months is below 100%."),
        tags$li("Employment rate of a ZIP code is defined as the percentage of the residing population who are employed as a part of the civilian labor force."),
        tags$li("Income measure of a ZIP code is defined as the average value of tract-level median household income in the past 12 months, weighted by tract population counts."),
        tags$li("Area Deprivation Index (ADI) of a ZIP code is the average ADI across covered census tracts, weighted by tract population counts.")
      ),
      
      tags$h3("Poststratification Table", class = "mt-5"),
      tags$p("We obtain ACS data through the packages tidycensus and IPUMS to compute the poststratification tables, which contain the size of each subpopulation defined by the cross-tabulation of demographic-geographic factors. Each subpopulation consists of individuals corresponding to a unique combination of these factors. We precompute and store the poststratification table at the tract level, then aggregate across tracts to derive counts at larger geographic scales. Census tracts fall perfectly within counties and states but ZIP codes have many-to-many relationships with tracts. We address this by summing over the overlapping tracts for each ZIP code to obtain ZIP code-level population counts.")
    )
  )
}


#' learn_preprocess Server Functions
#'
#' @noRd
mod_learn_preprocess_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$save_code <- downloadHandler(
      filename = function() { "preprocess.R" },
      content = function(file) {
        readLines(app_sys("extdata/preprocess.R")) |> writeLines(file)
      }
    )
    
    output$save_geo_conversion <- downloadHandler(
      filename = function() { "zip_county_state.csv" },
      content = function(file) {
        read.csv(app_sys("extdata/zip_county_state.csv")) |> readr::write_csv(file)
      }
    )
    
    output$save_week_conversion <- downloadHandler(
      filename = function() { "week_conversion.csv" },
      content = function(file) {
        read.csv(app_sys("extdata/week_conversion.csv")) |> readr::write_csv(file)
      }
    )

  })
}

