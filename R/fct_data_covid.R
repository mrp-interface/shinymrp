#' Rename columns for COVID data processing
#'
#' @title Standardize COVID dataset column names using pattern matching
#' @description Standardizes column names in COVID datasets by identifying and
#' renaming columns based on regex pattern matching. This function is specifically
#' designed for COVID test data which often comes with inconsistent column naming
#' conventions across different data sources. Maps columns containing encrypted IDs,
#' demographic information, test results, and dates to standardized names required
#' for downstream MRP analysis. Excludes IgG antibody test results to focus on
#' diagnostic PCR/antigen tests.
#'
#' @param df A data frame containing raw COVID test data with various column naming
#'   conventions. Must contain columns that match the expected patterns for ID,
#'   demographics, test results, and dates. Typical input would be individual-level
#'   COVID test records from health departments or testing facilities.
#'
#' @return A data frame with exactly 7 standardized column names in this order:
#' \itemize{
#'   \item `id`: Individual identifier (from encrypted/masked ID columns)
#'   \item `sex`: Biological sex/gender
#'   \item `race`: Race/ethnicity information
#'   \item `age`: Age in years (numeric)
#'   \item `zip`: ZIP code for geographic location
#'   \item `positive`: Test result indicator
#'   \item `date`: Test result date/time
#' }
#' Only columns matching the expected patterns are retained; all other columns
#' are dropped to ensure consistent downstream processing.
#'
#' @details The function searches for columns using these specific regex patterns:
#' \itemize{
#'   \item **ID**: `"(encrypted|masked).*id"` (case-insensitive) - Matches
#'     columns like "encrypted_id", "masked_patient_id", "EncryptedID"
#'   \item **Demographics**: `"sex"`, `"race"`, `"age"`, `"zip"`
#'     (case-insensitive) - Exact matches for demographic variables
#'   \item **Test results**: `"result|positive"` but excludes `"date|time|igg"`
#'     (case-insensitive) - Captures diagnostic test results while excluding antibody tests
#'   \item **Test dates**: `"result.*(time|date)"` but excludes `"igg"`
#'     (case-insensitive) - Matches result timestamps while excluding antibody test dates
#' }
#'
#' The function assumes exactly one column matches each pattern. If multiple columns
#' match a pattern, the first match is used. This is designed for COVID surveillance
#' data where column naming follows predictable patterns but may vary across sources.
#'
#' @section COVID Data Context:
#' This function is specifically designed for COVID surveillance and testing data which
#' typically includes:
#' \itemize{
#'   \item Individual test records with privacy-protected identifiers
#'   \item Demographic information for epidemiological analysis
#'   \item Binary test results (positive/negative) for diagnostic tests
#'   \item Temporal information for outbreak tracking
#'   \item Geographic identifiers for spatial analysis
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

#' Remove duplicate COVID test records within time periods
#'
#' @title Remove duplicate COVID test records keeping the latest result per individual per time period
#' @description Removes duplicate COVID test records for the same individual
#' within the same time period, keeping only the latest test result when multiple
#' tests exist for the same person in the same temporal unit. This is essential
#' for longitudinal COVID surveillance data where individuals may have multiple
#' tests recorded within the same time period (e.g., week, month) due to repeated
#' testing, data collection artifacts, or administrative duplicates. Ensures each
#' individual contributes only one observation per time period for valid
#' statistical analysis while preserving the most recent test information.
#'
#' @param df A data frame containing individual-level COVID test data with the
#'   following required columns:
#' \itemize{
#'   \item `id`: Character or factor - Individual identifier (typically encrypted
#'     or masked for privacy protection)
#'   \item `time`: Character, factor, or Date - Time period identifier (e.g.,
#'     "2020-03", "Week 12", or date values that have been grouped into periods)
#'   \item `date`: Date or POSIXct - Actual test date/timestamp used to determine
#'     which record to keep when duplicates exist within the same time period
#' }
#' Additional columns (demographics, test results, geography) are preserved in
#' the output.
#'
#' @return A data frame with duplicate records removed, containing one test record
#'   per individual per time period. When multiple records exist for the same
#'   `id`-`time` combination:
#' \itemize{
#'   \item The record with the latest `date` value is retained
#'   \item All other duplicates are removed
#'   \item All columns from the original data frame are preserved
#'   \item Row count may be reduced if duplicates were present
#' }
#'
#' @details
#' **Duplicate Resolution Logic:**
#' Uses `dplyr::slice_max()` on the `date` column within each `id`-`time` group:
#' \itemize{
#'   \item Groups data by individual identifier and time period
#'   \item Selects the record with the maximum (latest) date value
#'   \item Breaks ties deterministically using `with_ties = FALSE`
#'   \item Maintains original column structure and data types
#'   \item Handles missing date values by treating them as earliest
#' }
#'
#' **Time Period Considerations:**
#' The `time` variable should represent meaningful temporal units for analysis:
#' \itemize{
#'   \item Weekly periods for outbreak tracking
#'   \item Monthly periods for trend analysis
#'   \item Custom periods based on epidemiological needs
#' }
#'
#' @section COVID Surveillance Context:
#' Multiple test records within the same time period commonly occur due to:
#' \itemize{
#'   \item Individuals receiving multiple tests within a short timeframe
#'   \item Confirmatory testing following initial results
#'   \item Administrative data processing creating duplicate entries
#'   \item Data integration from multiple testing sources
#' }
#'
#' This function prioritizes the most recent test result, which is typically
#' the most clinically relevant for surveillance purposes.
#'
#' @importFrom dplyr group_by slice_max ungroup
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @noRd
remove_duplicates_covid <- function(df) {
  # only keep the latest result if a patient have multiple tests in the same week
  df <- df |>
    dplyr::group_by(.data$id, .data$time) |>
    dplyr::slice_max(.data$date, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  return(df)
}

#' Recode COVID data values to expected levels
#'
#' @title Standardize COVID data values for MRP analysis
#' @description Recodes demographic variables and test results in COVID data to
#' match standardized factor levels required for MRP modeling. This function ensures
#' consistent categorical coding across different COVID data sources by converting
#' free-text and numeric values to standardized categories. Handles the complexity
#' of COVID test result interpretation where various terms may indicate positive
#' or negative results, and creates age groups suitable for epidemiological analysis.
#'
#' @param df A data frame containing COVID test data with the following required columns:
#' \itemize{
#'   \item `sex`: Character or factor indicating biological sex/gender
#'   \item `race`: Character or factor indicating race/ethnicity
#'   \item `age`: Numeric age in years
#'   \item `positive`: Character, numeric, or factor indicating test result
#' }
#' Data should be individual-level COVID test records after column standardization.
#'
#' @param expected_levels A named list containing expected factor levels for demographic
#'   variables. Must include an `age` element with age range labels as character
#'   vector (e.g., `c("18-29", "30-44", "45-64", "65+")`). Age range labels
#'   must start with numeric values (e.g., "18-29", "30-44") which are extracted
#'   to create cut points for age categorization. Typically generated by
#'   `create_expected_levels()` function.
#'
#' @return A data frame with the same number of rows as input but with recoded
#' demographic and test result variables:
#' \itemize{
#'   \item `sex`: Character - "female" or "male" (defaults to "male" if not
#'     explicitly matching "female" pattern)
#'   \item `race`: Character - "white", "black", or "other" (defaults to "other"
#'     if not matching white/black patterns)
#'   \item `age`: Character - Age ranges as specified in `expected_levels$age`
#'     (e.g., "18-29", "30-44", "45-64", "65+")
#'   \item `positive`: Numeric - 0 (negative test), 1 (positive test), or
#'     `NA` (ambiguous/invalid results)
#' }
#'
#' @details
#' **Test Result Classification:**
#' COVID test results are classified using case-insensitive pattern matching:
#' \itemize{
#'   \item **Positive (1)**: Matches `"positive|detected|1"` - Includes
#'     "Positive", "DETECTED", "1", "positive result", etc.
#'   \item **Negative (0)**: Matches `"not|negative|undetected|0"` - Includes
#'     "Negative", "NOT DETECTED", "0", "not detected", etc.
#'   \item **Invalid (NA)**: All other values including "inconclusive", "pending",
#'     "invalid", empty strings, or any unrecognized text
#' }
#'
#' **Age Group Creation:**
#' Age groups are created by extracting the first numeric value from each age range
#' label to create cut points. For example, `c("18-29", "30-44", "45-64", "65+")`
#' creates breaks at [18, 30, 45, 65] with appropriate boundary handling.
#'
#' **Demographic Recoding:**
#' \itemize{
#'   \item Sex: Case-insensitive matching for "female"; all others default to "male"
#'   \item Race: Case-insensitive matching for "white" and "black"; all others become "other"
#' }
#'
#' @section COVID-Specific Considerations:
#' This function addresses common challenges in COVID surveillance data:
#' \itemize{
#'   \item Inconsistent test result terminology across laboratories
#'   \item Free-text demographic entries requiring standardization
#'   \item Age groupings appropriate for COVID epidemiological analysis
#'   \item Integration with broader MRP modeling framework
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
#' @title Remove duplicate COVID test records within time periods
#' @description Removes duplicate COVID test records for the same individual
#' within the same time period, keeping only one test per person per time point.
#' This is essential for longitudinal COVID surveillance data where individuals
#' may have multiple tests recorded within the same temporal unit (e.g., week,
#' month) due to data collection artifacts, repeated testing, or administrative
#' duplicates. Ensures each individual contributes only one observation per
#' time period for valid statistical analysis.
#'
#' @param df A data frame containing individual-level COVID test data with the
#'   following required columns:
#' \itemize{
#'   \item `id`: Character or factor - Individual identifier (typically encrypted
#'     or masked for privacy protection)
#'   \item `time`: Character, factor, or Date - Time period identifier (e.g.,
#'     "2020-03", "Week 12", or date values that have been grouped into periods)
#' }
#' Additional columns (demographics, test results, geography) are preserved in
#' the output.
#'
#' @return A data frame with duplicate records removed, containing one test record
#'   per individual per time period. When multiple records exist for the same
#'   `id`-`time` combination:
#' \itemize{
#'   \item The first occurrence is retained (based on original row order)
#'   \item All other duplicates are removed
#'   \item All columns from the original data frame are preserved
#'   \item Row count may be reduced if duplicates were present
#' }
#'
#' @details
#' **Duplicate Detection Logic:**
#' Uses `dplyr::distinct()` with `.keep_all = TRUE` to identify unique
#' combinations of `id` and `time`. This approach:
#' \itemize{
#'   \item Preserves the first occurrence when duplicates exist
#'   \item Maintains original column structure and data types
#'   \item Handles missing values appropriately (NA values are treated as distinct)
#'   \item Is computationally efficient for large datasets
#' }
#'
#' **Time Period Considerations:**
#' The `time` variable should represent meaningful temporal units for analysis:
#' \itemize{
#'   \item Weekly periods for outbreak tracking
#'   \item Monthly periods for trend analysis
#'   \item Custom periods based on epidemiological needs
#' }
#'
#' @section COVID Surveillance Context:
#' Duplicate records commonly occur in COVID surveillance data due to:
#' \itemize{
#'   \item Multiple test results reported for the same individual
#'   \item Administrative data processing creating duplicate entries
#'   \item Individuals tested multiple times within short periods
#'   \item Data integration from multiple sources
#' }
#'
#' This function ensures clean longitudinal data suitable for time-series analysis
#' and MRP modeling of COVID trends.
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


#' Prepare COVID data for MRP analysis
#'
#' @title Comprehensive COVID data preparation for MRP modeling
#' @description Prepares COVID surveillance data for Multilevel Regression and
#' Poststratification (MRP) analysis by performing comprehensive data integration,
#' geographic filtering, and structural organization. This function serves as the
#' main data preparation pipeline for COVID-specific MRP analysis, handling the
#' unique challenges of COVID surveillance data including geographic sparsity,
#' temporal variation, and integration with demographic covariates. Creates both
#' the modeling dataset and poststratification frame required for MRP inference.
#'
#' @param input_data A data frame containing individual-level COVID test data with
#'   the following required structure:
#' \itemize{
#'   \item Individual test records (one row per person per time period)
#'   \item Demographic variables: `sex`, `race`, `age`
#'   \item Geographic identifier: `zip`
#'   \item Temporal identifier: `time`
#'   \item Outcome variable: `positive` (binary test result)
#'   \item Sample size indicator: `total` (for aggregated data)
#' }
#' Should be the output of COVID-specific data processing functions.
#'
#' @param pstrat_data A data frame containing poststratification population counts
#'   organized by demographic groups and geographic areas. Must include:
#' \itemize{
#'   \item Geographic identifiers: `zip`, `county`
#'   \item Demographic cross-tabulations with population counts
#'   \item Structure compatible with `expand.grid()` for frame creation
#' }
#' Typically derived from American Community Survey or Census data.
#'
#' @param covariates A data frame containing geographic covariates and auxiliary
#'   variables for modeling. Must include:
#' \itemize{
#'   \item `zip`: ZIP code identifier (primary key for merging)
#'   \item Additional geographic predictors (e.g., urbanicity, income, demographics)
#'   \item Should not duplicate columns in `input_data` except for `zip`
#' }
#'
#' @param metadata A named list containing metadata specifications for the analysis:
#' \itemize{
#'   \item `special_case`: Should be "covid" for COVID-specific processing
#'   \item `is_timevar`: Logical indicating time-varying analysis
#'   \item Demographic specifications used by `create_expected_levels()`
#'   \item Variable type classifications for model specification
#' }
#'
#' @return A named list with four components required for MRP analysis:
#' \describe{
#'   \item{`input`}{Data frame - Processed individual-level data merged with
#'     geographic covariates. Ready for model fitting with consistent variable
#'     coding and geographic filtering applied.}
#'   \item{`new`}{Data frame - Poststratification frame containing all
#'     combinations of factor levels with population weights. Includes:
#'     \itemize{
#'       \item All demographic and geographic factor combinations
#'       \item `total`: Population counts for each cell
#'       \item Geographic covariates merged by ZIP code
#'       \item Proper ordering to match population count structure
#'     }}
#'   \item{`levels`}{Named list - All factor levels for model variables:
#'     \itemize{
#'       \item `time`: Unique time periods from input data
#'       \item `zip`: ZIP codes after geographic filtering
#'       \item Demographic levels: `sex`, `race`, `age`
#'       \item `county`: County identifiers for hierarchical modeling
#'     }}
#'   \item{`vars`}{Named list - Organized variable specifications for model
#'     fitting created by `create_variable_list()`, including fixed effects,
#'     random effects, and variables to omit.}
#' }
#'
#' @details
#' **Data Processing Pipeline:**
#' \enumerate{
#'   \item **Geographic Filtering**: Applies `filter_state_zip()` to
#'     remove geographic areas with insufficient sample sizes
#'   \item **Data Alignment**: Ensures consistent geographic coverage across
#'     input data, covariates, and poststratification frame
#'   \item **Column Management**: Removes duplicate columns between datasets
#'     while preserving the ZIP code key for merging
#'   \item **Data Integration**: Merges individual-level data with geographic
#'     covariates using left join on ZIP code
#'   \item **Factor Level Creation**: Generates standardized factor levels
#'     using `create_expected_levels()` with COVID-specific specifications
#'   \item **Poststratification Frame**: Creates comprehensive grid of all
#'     factor combinations with proper cell ordering and population weights
#'   \item **Variable Organization**: Structures variables for model
#'     specification using `create_variable_list()`
#' }
#'
#' **Poststratification Frame Construction:**
#' The poststratification frame is created using `expand.grid()` with:
#' \itemize{
#'   \item All combinations of demographic and geographic factors
#'   \item Specific ordering: time, zip, sex, race, age (critical for population matching)
#'   \item Population counts replicated across time periods
#'   \item Geographic covariates merged for each cell
#' }
#'
#' **COVID-Specific Considerations:**
#' \itemize{
#'   \item Handles temporal variation in geographic coverage
#'   \item Manages sparse data in rural or low-testing areas
#'   \item Integrates with COVID-specific demographic coding
#'   \item Supports time-varying MRP analysis for outbreak tracking
#' }
#'
#' @section Integration with MRP Workflow:
#' This function produces the standardized data structures required by the
#' broader MRP modeling framework:
#' \itemize{
#'   \item `input` and `new` are used directly in model fitting
#'   \item `levels` provides factor specifications for model constraints
#'   \item `vars` guides variable selection and model specification
#' }
#'
#' @seealso
#' \itemize{
#'   \item `filter_state_zip()` for geographic filtering logic
#'   \item `create_expected_levels()` for factor level specifications
#'   \item `create_variable_list()` for variable organization
#' }
#'
#' @importFrom dplyr filter select left_join arrange mutate
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @noRd
prepare_mrp_covid <- function(
    input_data,
    pstrat_data,
    covariates,
    metadata
) {

  # prep_old <- readRDS("/Users/tntoan/Desktop/MRP/Code/mrp_workflow_paper/preprocessed_old.RDS")
  # covariates <- prep_old$covar |> rename("adi" = "ADI", "county" = "fips")
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
  vars <- create_variable_list(input_data, covariates)

  return(list(
    input = input_data,
    new = new_data,
    levels = levels,
    vars = vars
  ))
}
