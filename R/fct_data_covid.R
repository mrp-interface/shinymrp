#' Rename columns for COVID data processing
#'
#' @description Standardizes column names in COVID datasets by identifying and
#' renaming columns based on pattern matching. Maps columns containing ID,
#' demographic information, test results, and dates to standardized names.
#' Uses regex patterns to identify: encrypted/masked IDs, sex, race, age, zip,
#' test results (excluding IgG), and result dates/times.
#'
#' @param df A data frame containing COVID test data with various column naming conventions.
#'   Expected to contain columns matching patterns for ID, demographics, test results, and dates.
#'
#' @return A data frame with standardized column names: id, sex, race, age, zip, positive, date.
#'   Only columns matching the expected patterns are retained.
#'
#' @details The function searches for columns using these patterns:
#' \itemize{
#'   \item ID: "(encrypted|masked).*id" (case-insensitive)
#'   \item Demographics: "sex", "race", "age", "zip" (case-insensitive)
#'   \item Test results: "result|positive" but not "date|time|igg" (case-insensitive)
#'   \item Test dates: "result.*(time|date)" but not "igg" (case-insensitive)
#' }
#'
#' @importFrom dplyr select all_of
#' @importFrom magrittr %>%
#'
#' @noRd
rename_columns_covid <- function(df) {
  all_names <- names(df)
  patterns <- c("(encrypted|masked).*id", "sex", "race", "age", "zip")

  old_names <- patterns %>%
    sapply(function(s) all_names[grepl(s, all_names, ignore.case=TRUE)]) %>%
    unlist()

  old_names <- c(old_names,
                 all_names[grepl("result|positive", all_names, ignore.case=TRUE)
                               & !grepl("date|time|igg", all_names, ignore.case=TRUE)] %>% unlist(),
                 all_names[grepl("result.*(time|date)", all_names, ignore.case=TRUE)
                             & !grepl("igg", all_names, ignore.case=TRUE)]
                 )

  df <- df %>% select(all_of(old_names))
  new_names <- c("id", "sex", "race", "age", "zip",
                 "positive", "date")
  names(df) <- new_names

  return(df)
}

#' Recode COVID data values to expected levels
#'
#' @description Recodes demographic variables and test results in COVID data to
#' match expected factor levels. Converts sex to binary (male/female), race to
#' three categories (white/black/other), age to specified ranges, and test
#' results to binary (0/1).
#'
#' @param df A data frame containing COVID data with columns: sex, race, age, positive
#' @param expected_levels A list containing expected factor levels. Must include
#'   an 'age' element with age range labels (e.g., c("18-29", "30-44", "45-64", "65+")).
#'   Age ranges should start with numeric values that will be used to create cut points.
#'
#' @return A data frame with recoded demographic and test result variables:
#' \itemize{
#'   \item sex: "female" or "male" (default "male" if not matching "female")
#'   \item race: "white", "black", or "other" (default "other" if not matching white/black)
#'   \item age: Age ranges as specified in expected_levels$age
#'   \item positive: 0 (negative), 1 (positive), or NA (ambiguous results)
#' }
#'
#' @details Test results are classified as:
#' \itemize{
#'   \item Positive: matches "positive|detected|1" (case-insensitive)
#'   \item Negative: matches "not|negative|undetected|0" (case-insensitive)
#'   \item NA: all other values
#' }
#'
#' @importFrom dplyr mutate case_when if_else
#' @importFrom stringr str_detect regex
#' @importFrom rlang .data
#'
#' @noRd
recode_covid <- function(df, expected_levels) {
  ranges <- expected_levels$age
  age_bounds <- regmatches(
    ranges,
    regexpr("^\\d+", ranges)
  ) %>%
    as.numeric()
  breaks <- c(-1, age_bounds[2:length(age_bounds)] - 1, 200)
  is_pos <- grepl("positive|detected|1", df$positive, ignore.case = TRUE)
  is_neg <- grepl("not|negative|undetected|0", df$positive, ignore.case = TRUE)

  df <- df %>% mutate(
    sex = if_else(str_detect(.data$sex, regex("female", ignore_case = TRUE)), "female", "male"),
    race = case_when(
      str_detect(.data$race, regex("white", ignore_case = TRUE)) ~ "white",
      str_detect(.data$race, regex("black", ignore_case = TRUE)) ~ "black",
      TRUE ~ "other"
    ),
    age = cut(df$age, breaks, ranges) %>% as.character(),
    positive = if("positive" %in% names(df)) ifelse(is_neg, 0, ifelse(is_pos, 1, NA))
  )

  return(df)
}

#' Remove duplicate COVID test records
#'
#' @description Removes duplicate COVID test records for the same individual
#' within the same time period, keeping only one test per person per time point.
#' Uses the combination of ID and time to identify duplicates.
#'
#' @param df A data frame containing COVID test data with columns: id, time
#'
#' @return A data frame with duplicate records removed, keeping one test per
#'   individual per time period. When duplicates exist, the first occurrence
#'   is retained (based on row order).
#'
#' @importFrom dplyr distinct
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @noRd
remove_dup_covid <- function(df) {
  # remove all but one test of a df in the same week
  df <- df %>% distinct(.data$id, .data$time, .keep_all = TRUE)

  return(df)
}

#' Filter data based on state and ZIP code thresholds
#'
#' @description Filters COVID data to include only ZIP codes and states that
#' meet minimum sample size requirements. Removes areas with insufficient data
#' for reliable statistical analysis. Applies both state-level and ZIP-level
#' filtering sequentially.
#'
#' @param df A data frame containing COVID data with columns: zip, total
#' @param covariates A data frame containing geographic covariates with columns:
#'   zip, county. The county column should contain FIPS codes where the first
#'   two digits represent the state FIPS code.
#' @param zip_threshold Numeric. Minimum number of observations required per ZIP code (default: 5)
#' @param state_threshold Numeric. Minimum proportion of total sample required per state (default: 0.01)
#'
#' @return A data frame filtered to include only ZIP codes meeting both thresholds.
#'   ZIP codes are retained if: (1) their state has at least state_threshold proportion
#'   of total observations, AND (2) the ZIP code itself has at least zip_threshold observations.
#'
#' @importFrom dplyr group_by summarize mutate select distinct inner_join filter ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @noRd
filter_state_zip <- function(
    df,
    covariates,
    zip_threshold = 5,
    state_threshold = 0.01
) {
  
  zip_count <- df %>%
    group_by(.data$zip) %>%
    summarize(count = sum(.data$total))

  # create a table containing state, zip and zip count
  state_zip <- covariates %>%
    mutate(state = substr(.data$county, 1, 2)) %>%
    select(.data$zip, .data$state) %>%
    distinct(.data$zip, .keep_all = TRUE) %>%
    inner_join(zip_count, by = "zip")

  # filter based on proportion of state
  N <- sum(state_zip$count)
  state_zip <- state_zip %>%
    group_by(.data$state) %>%
    filter(sum(.data$count) > state_threshold * N) %>%
    ungroup()

  # filter based on number of zip
  state_zip <- state_zip %>%
    group_by(.data$zip) %>%
    filter(sum(.data$count) > zip_threshold) %>%
    ungroup()

  df <- df %>% filter(.data$zip %in% state_zip$zip)

  return(df)
}


#' Prepare COVID data for MRP analysis
#'
#' @description Prepares COVID data for Multilevel Regression and Poststratification
#' (MRP) analysis by filtering geographic areas, merging datasets, creating
#' poststratification frames, and organizing variable lists for modeling.
#' Applies geographic filtering, merges covariates, and creates expanded grid
#' for poststratification.
#'
#' @param input_data A data frame containing individual-level COVID test data
#' @param covariates A data frame containing geographic covariates and predictors
#' @param pstrat_data A data frame containing poststratification population counts
#'   by demographic groups and geographic areas. Should have zip and county columns
#'   followed by demographic cross-tabulation columns.
#' @param metadata A list containing metadata about expected demographic levels
#'   and variable specifications for the analysis. Used by create_expected_levels()
#'   to generate factor levels.
#'
#' @return A list containing:
#'   \item{input}{Processed individual-level data merged with covariates}
#'   \item{new}{Poststratification frame with population counts and covariates}
#'   \item{levels}{List of all factor levels for model variables (time, zip, demographics, county)}
#'   \item{vars}{Organized variable lists for model specification (created by create_variable_list())}
#'
#' @details The function performs these steps:
#' \enumerate{
#'   \item Filters data using filter_state_zip() to remove small geographic areas
#'   \item Removes duplicate columns between input_data and covariates (except zip)
#'   \item Merges input_data with covariates by zip code
#'   \item Creates factor levels using create_expected_levels()
#'   \item Builds poststratification frame using expand.grid() with proper cell ordering
#'   \item Generates variable lists using create_variable_list() with vars_global
#' }
#'
#' @importFrom dplyr filter select left_join arrange mutate
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @noRd
prepare_mrp_covid <- function(
    input_data,
    covariates,
    pstrat_data,
    metadata
) {

  # filter out state and zip codes with small sample sizes
  input_data <- filter_state_zip(input_data, covariates)
  covariates <- covariates %>% filter(.data$zip %in% input_data$zip)
  pstrat_data <- pstrat_data %>% filter(.data$zip %in% input_data$zip)
  cell_counts <- pstrat_data[-c(1, 2)] %>% t() %>% c()
  
  # # prevent duplicate columns
  dup_cols <- intersect(names(input_data), names(covariates)) %>% setdiff(c("zip"))
  input_data <- input_data %>% select(-all_of(dup_cols))

  input_data <- input_data %>%
    left_join(covariates, by = "zip")

  # create lists of all factor levels
  levels <- create_expected_levels(metadata)
  levels$time <- unique(input_data$time) %>% sort()
  levels$zip <- pstrat_data$zip

  new_data <- expand.grid(levels, stringsAsFactors = FALSE) %>%
    arrange(.data$time, .data$zip, .data$sex, .data$race, .data$age) %>%  # IMPORTANT: To match the cell order of poststratification data
    mutate(total = rep(cell_counts, length(levels$time))) %>%
    left_join(covariates, by = "zip")

  # append levels for other geographic predictors
  # NOTE: this must be done after new_data is created
  # as these levels are not used in the poststratification table
  levels$county <- pstrat_data$county %>% unique()

  # list of variables for model specification
  vars <- create_variable_list(input_data, covariates, vars_global)

  return(list(
    input = input_data,
    new = new_data,
    levels = levels,
    vars = vars
  ))
}

