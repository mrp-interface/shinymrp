#' Data Preprocessing Guide Module UI Function
#'
#' @description Creates the user interface for the data preprocessing guide page.
#' Provides comprehensive documentation about data preprocessing workflows,
#' geographic identifier handling, and poststratification table preparation.
#' Includes detailed information about data cleaning, categorization, imputation,
#' and aggregation procedures for both time-varying and cross-sectional data.
#'
#' @param id Character string. The module's namespace identifier.
#'
#' @return A `bslib::layout_columns` containing the preprocessing guide with:
#' \itemize{
#'   \item Detailed preprocessing workflow documentation
#'   \item Geographic identifier and covariate information
#'   \item Download buttons for preprocessing scripts and conversion tables
#'   \item Poststratification table preparation guidelines
#' }
#'
#' @noRd
#' @keywords internal
mod_learn_preprocess_ui <- function(id){
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(-3, 6),
    tags$div(
      tags$hr(class = "header_top"),
      tags$h3("Data Preprocessing for MRP"),
      tags$hr(class = "header_bottom"),
      tags$p("MRP requires two main data components: the survey or test data, and a corresponding poststratification table. The workflow involves two stages:"),
      tags$ul(
        tags$li(tags$b("Multilevel regression (MR):"), " Modeling the relationship between the outcome and demographic/geographic predictors, optionally incorporating area-level covariates;"),
        tags$li(tags$b("Poststratification (P):"), " Aggregating estimates according to the population structure of the target.")
      ),
      
      tags$h4("Data requirements", class = "mt-5"),
      
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
      
      tags$h5("Data modules", class = "mt-4"),
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
      
      tags$h5("Data preprocessing steps", class = "mt-4"),
      tags$p("The preprocessing pipeline includes:"),
      tags$ul(
        tags$li(tags$b("Data cleaning"), ": Standardizes column names, converts values to lowercase, handles missing/unknown data, and standardizes ZIP/FIPS codes."),
        tags$li(tags$b("Conversion to categorical"), ": Recodes variables, applies categorization intervals, and assigns time indices to dates."),
        tags$li(tags$b("Imputation"), ": Imputes missing entries using observed frequency distributions."),
        tags$li(tags$b("Aggregation"), ": Aggregates individual-level data to produce cell counts for each combination of relevant group factors.")
      ),
      tags$p("Code reference: ", tags$a("preprocess", href = "https://github.com/mrp-interface/shinymrp/blob/main/R/fct_data.R#L1211", target = "_blank")),
      
      tags$h4("Geographic identifiers and covariates", class = "mt-5"),
      tags$p("A major strength of MRP is small area estimation, so it is recommended to include as much geographic/geocovariate information as available."),
      tags$p("First, it identifies geographic units at larger scales that are not present in the data. The application automatically identifies the smallest geographic units in the data and infers corresponding units at larger scales. For example, if the data contains ZIP codes, the application will automatically find the county and state that has the largest overlap with each ZIP code."),
      tags$p("Quantitative measures associated with geographic units are sourced from your data or external datasets. For general use cases, the app scans the data to find quantities that have a one-to-one relationship with the geographic identifier of interest."),
      tags$p("For the COVID-19 use case, we have identified certain ZIP code-level measures that are informative in modeling COVID-19 test results. We obtain these quantities at the tract level from the ACS and other sources, then aggregate over the tracts that overlap with each ZIP code based on the USPS crosswalk table."),
      tags$p("We obtain the following tract-level measures from the ACS and other sources:"),
      tags$ul(
        tags$li("Binary indicators of whether tracts are classified as urban or not"),
        tags$li("Population counts categorized by levels of education"),
        tags$li("Population counts categorized by ratios of income to poverty level in the past 12 months"),
        tags$li("Population counts categorized by employment status"),
        tags$li("Median household income in the past 12 months"),
        tags$li(tags$a("Area Deprivation Index (ADI)", href = "https://www.neighborhoodatlas.medicine.wisc.edu", target = "_blank"))
      ),
      tags$p("Code reference: ", tags$a("get_tract_data", href = "https://github.com/mrp-interface/shinymrp/blob/main/dev/scripts/get_tract_data.R#L49", target = "_blank")),
      tags$p("While the ACS reports geography at the levels of census tracts, counties, and states, ZIP codes are defined by the U.S. Postal Service (USPS). We use the ZIP code crosswalk table released by the U.S. Department of Housing and Urban Development and USPS to link ZIP codes to census tracts and calculate the ZIP-code-level measures by aggregating all available tract-level measures weighted by tract population counts. ZIP code level statistics are computed by combining the values across census tracts as follows:"),
      tags$ul(
        tags$li("Urbanicity of a ZIP code is defined as the percentage of covered census tracts classified as urban, weighted by tract population counts"),
        tags$li("Higher education measure of a ZIP code is defined as the percentage of the residing population who have earned an Associate's degree or higher"),
        tags$li("Poverty measure of a ZIP code is defined as the percentage of the residing population whose ratio of income to poverty level in the past 12 months is below 1"),
        tags$li("Employment rate of a ZIP code is defined as the percentage of the residing population who are employed as a part of the civilian labor force"),
        tags$li("Income measure of a ZIP code is defined as the average value of tract-level median household income in the past 12 months, weighted by tract population counts"),
        tags$li("ADI of a ZIP code is the average ADI across covered census tracts, weighted by tract population counts")
      ),
      tags$p("Code reference: ", tags$a("combine_tracts_covid", href = "https://github.com/mrp-interface/shinymrp/blob/main/dev/scripts/create_data.R#L97", target = "_blank")),
      
      tags$h4("Poststratification tables", class = "mt-5"),
      tags$p("Poststratification tables are computed from ACS data via the ", tags$a("tidycensus", href = "https://walker-data.com/tidycensus/", target = "_blank"), " package and ", tags$a("IPUMS", href = "https://ipums.org", target = "_blank"), " and summarize the size of every subpopulation defined by demographic and geographic cross-categories. For efficiency, tables are precomputed at the tract level and then aggregated for larger geographies. We select the county with the most-overlapping residential addresses for one ZIP code as the ZIP-linked county and sum over the overlapping tracts for each ZIP code to obtain ZIP code-level population counts."),
      tags$p("Code reference: ", tags$a("combine_tracts", href = "https://github.com/mrp-interface/shinymrp/blob/main/R/fct_data.R#L1403", target = "_blank"), " and ", tags$a("combine_tracts_covid", href = "https://github.com/mrp-interface/shinymrp/blob/main/dev/scripts/create_data.R#L97", target = "_blank"))
    )
  )
}


#' Data Preprocessing Guide Module Server Function
#'
#' @description Server logic for the data preprocessing guide module. Provides
#' download handlers for preprocessing scripts and geographic/temporal conversion
#' tables. Manages file downloads for users to access example preprocessing
#' code and reference data for their own data preparation workflows.
#'
#' @param id Character string. The module's namespace identifier.
#' @param global Reactive values object containing global application state
#'
#' @return Server function for the preprocessing guide module. Creates download
#' handlers for preprocessing scripts, geographic conversion tables, and week
#' conversion tables to support user data preparation.
#'
#' @noRd
#' @keywords internal
mod_learn_preprocess_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$save_code <- downloadHandler(
      filename = function() { "preprocess.R" },
      content = function(file) {
        .fetch_data("preprocess.R", subdir = "download") %>%
          writeLines(file)
      }
    )
    
    output$save_geo_conversion <- downloadHandler(
      filename = function() { "zip_county_state.csv" },
      content = function(file) {
        .fetch_data("zip_county_state.csv", subdir = "geo") %>%
          readr::write_csv(file)
      }
    )
    
    output$save_week_conversion <- downloadHandler(
      filename = function() { "week_conversion.csv" },
      content = function(file) {
        .fetch_data("week_conversion.csv", subdir = "download") %>%
          readr::write_csv(file)
      }
    )

  })
}

