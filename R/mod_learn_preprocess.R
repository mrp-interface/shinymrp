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
      bslib::card(
        full_screen = TRUE,
        bslib::card_body(
          bslib::card(
            bslib::card_image(src = "www/preprocess.pdf", class = "mb-3"),
            bslib::card_body(
              bslib::value_box(
                title = "MRP Data Components",
                value = NULL,
                showcase = bsicons::bs_icon("database"),
                p("The input data for MRP consists of three components:"),
                tags$ul(
                  tags$li("Preprocessed survey/test data"),
                  tags$li("Poststratification table"),
                  tags$li("Geographic covariates")
                ),
                p("The survey/test data contains geographic-demographic information about participants and their responses. Regression models infer relationships in stage one of MRP. Geographic covariates account for structured differences among areas. Stage two involves poststratification adjustment based on target population breakdown.")
              )
            )
          ),
          bslib::accordion(
            id = "preprocess_sections",
            open = TRUE,
            multiple = TRUE,
            bslib::accordion_panel(
              "Cross-sectional Data Preparation",
              bslib::card(
                bslib::card_header("Survey/Test Data"),
                bslib::card_body(
                  tags$p("The current interface expects the following information:"),
                  tags$ul(
                    tags$li("Sex"),
                    tags$li("Race"),
                    tags$li("Age"),
                    tags$li("Education level/Highest degree attained"),
                    tags$li("State (optional)")
                  ),
                  tags$p("Data cleaning process:"),
                  tags$ul(
                    tags$li("Convert column names to snake case"),
                    tags$li("Convert character values to lowercase"),
                    tags$li("Impute missing data"),
                    tags$li("Group values into categories:")
                  ),
                  tags$ul(
                    tags$li("Sex: male, female"),
                    tags$li("Race: White, Black, other"),
                    tags$li("Age: 0-17, 18-34, 35-64, 65-74, 75+"),
                    tags$li("Education: below high school, high school, some college, 4-year college, post-grad")
                  )
                )
              ),
              bslib::card(
                bslib::card_header("Poststratification Table"),
                bslib::card_body(
                  tags$p("Following this ", tags$a("guide", href = "https://bookdown.org/jl5522/MRP-case-studies/introduction-to-mister-p.html", target = "_blank"), " by Juan Lopez-Martin, we obtain the American Community Survey data through IPUMS, convert the raw values to the same categories as the input data, and then find the count for each of the subgroups corresponding to unique combinations of the demographic factors: sex, race, age, education. If the program can find states in the input data, it automatically includes them in the poststratification table and produces state-level estimates.")
                )
              ),
              bslib::card(
                bslib::card_header("Geographic Covariates"),
                bslib::card_body(
                  tags$p("Currently, we do not retrieve this information from external data sources but, if there are variables with values unique to each state in the input data, the program automatically extracts them and allows them to be included in the models. In the future, we may draw from external data relevant state-level covariates that can improve the predictive power of the models.")
                )
              ),
              bslib::card(
                bslib::card_body(
                  bslib::layout_columns(
                    col_widths = c(6, 6),
                    downloadButton(
                      outputId = ns("save_code_cs"),
                      label = "Preprocessing code",
                      class = "btn-primary w-100 mb-2"
                    ),
                    downloadButton(
                      outputId = ns("save_ex_cs"),
                      label = "Example data",
                      class = "btn-primary w-100 mb-2"
                    )
                  )
                )
              )
            ),
            bslib::accordion_panel(
              "Spatio-temporal Data Preparation",
              bslib::card(
                bslib::card_header("Overview"),
                bslib::card_body(
                  tags$p("To account for the difference between ZIP code areas in the models, we need quantities that are defined at the ZIP code level. Because ACS data are not provided at the ZIP code level, we obtain relevant quantities at the tract level and then use the USPS crosswalk table to obtain the ZIP code level covariates. Specifically, one ZIP code can span multiple tracts so we find the overlapping tracts for each ZIP code and aggregate the values.")
                )
              ),
              bslib::card(
                bslib::card_header("Survey/Test Data"),
                bslib::card_body(
                  tags$p("The preprocessing code works best with Epic system test records. Required columns:"),
                  tags$ul(
                    tags$li("Sex"),
                    tags$li("Race"),
                    tags$li("Age"),
                    tags$li("ZIP code"),
                    tags$li("Test result"),
                    tags$li("Date of test result (optional)")
                  ),
                  tags$p("Data cleaning process:"),
                  tags$ul(
                    tags$li("Filter out zip codes and states with small samples"),
                    tags$li("Impute missing values"),
                    tags$li("Aggregate demographic variables:")
                  ),
                  tags$ul(
                    tags$li("Sex: male, female"),
                    tags$li("Race: White, Black, other"),
                    tags$li("Age: 0-17, 8-34, 35-64, 65-74, 75+")
                  )
                )
              ),
              bslib::card(
                bslib::card_header("Geographic Covariates"),
                bslib::card_body(
                  tags$p("We obtain the following tract-level measures:"),
                  tags$ul(
                    tags$li("Binary indicators of whether tracts are classified as urban or not"),
                    tags$li("Population sizes based on levels of education"),
                    tags$li("Population sizes based on ratios of income to poverty level"),
                    tags$li("Population sizes based on employment status"),
                    tags$li("Median household income"),
                    tags$li(tags$a("Area Deprivation Index (ADI)", href = "https://www.neighborhoodatlas.medicine.wisc.edu", target = "_blank"))
                  ),
                  tags$p("ZIP code level aggregation:"),
                  tags$ul(
                    tags$li("Urbanicity: Percentage of urban tracts (population weighted)"),
                    tags$li("Higher education: Percentage with Associate's degree or higher"),
                    tags$li("Poverty: Percentage below 100% poverty level"),
                    tags$li("Employment: Percentage in civilian labor force"),
                    tags$li("Income: Average median household income (population weighted)"),
                    tags$li("ADI: Average across tracts (population weighted)")
                  )
                )
              ),
              bslib::card(
                bslib::card_body(
                  bslib::layout_columns(
                    col_widths = c(4, 4, 4),
                    downloadButton(
                      outputId = ns("save_code_st"),
                      label = "Preprocessing code",
                      class = "btn-primary w-100 mb-2"
                    ),
                    downloadButton(
                      outputId = ns("save_ex_st"),
                      label = "Example data",
                      class = "btn-primary w-100 mb-2"
                    ),
                    downloadButton(
                      outputId = ns("save_week_table"),
                      label = "Week conversion table",
                      class = "btn-primary w-100 mb-2"
                    )
                  )
                )
              )
            )
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
