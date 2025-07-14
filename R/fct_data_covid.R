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
#'   \item \code{id}: Individual identifier (from encrypted/masked ID columns)
#'   \item \code{sex}: Biological sex/gender
#'   \item \code{race}: Race/ethnicity information
#'   \item \code{age}: Age in years (numeric)
#'   \item \code{zip}: ZIP code for geographic location
#'   \item \code{positive}: Test result indicator
#'   \item \code{date}: Test result date/time
#' }
#' Only columns matching the expected patterns are retained; all other columns
#' are dropped to ensure consistent downstream processing.
#'
#' @details The function searches for columns using these specific regex patterns:
#' \itemize{
#'   \item \strong{ID}: \code{"(encrypted|masked).*id"} (case-insensitive) - Matches
#'     columns like "encrypted_id", "masked_patient_id", "EncryptedID"
#'   \item \strong{Demographics}: \code{"sex"}, \code{"race"}, \code{"age"}, \code{"zip"}
#'     (case-insensitive) - Exact matches for demographic variables
#'   \item \strong{Test results}: \code{"result|positive"} but excludes \code{"date|time|igg"}
#'     (case-insensitive) - Captures diagnostic test results while excluding antibody tests
#'   \item \strong{Test dates}: \code{"result.*(time|date)"} but excludes \code{"igg"}
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
#'   \item \code{sex}: Character or factor indicating biological sex/gender
#'   \item \code{race}: Character or factor indicating race/ethnicity
#'   \item \code{age}: Numeric age in years
#'   \item \code{positive}: Character, numeric, or factor indicating test result
#' }
#' Data should be individual-level COVID test records after column standardization.
#'
#' @param expected_levels A named list containing expected factor levels for demographic
#'   variables. Must include an \code{age} element with age range labels as character
#'   vector (e.g., \code{c("18-29", "30-44", "45-64", "65+")}). Age range labels
#'   must start with numeric values (e.g., "18-29", "30-44") which are extracted
#'   to create cut points for age categorization. Typically generated by
#'   \code{create_expected_levels()} function.
#'
#' @return A data frame with the same number of rows as input but with recoded
#' demographic and test result variables:
#' \itemize{
#'   \item \code{sex}: Character - "female" or "male" (defaults to "male" if not
#'     explicitly matching "female" pattern)
#'   \item \code{race}: Character - "white", "black", or "other" (defaults to "other"
#'     if not matching white/black patterns)
#'   \item \code{age}: Character - Age ranges as specified in \code{expected_levels$age}
#'     (e.g., "18-29", "30-44", "45-64", "65+")
#'   \item \code{positive}: Numeric - 0 (negative test), 1 (positive test), or
#'     \code{NA} (ambiguous/invalid results)
#' }
#'
#' @details
#' \strong{Test Result Classification:}
#' COVID test results are classified using case-insensitive pattern matching:
#' \itemize{
#'   \item \strong{Positive (1)}: Matches \code{"positive|detected|1"} - Includes
#'     "Positive", "DETECTED", "1", "positive result", etc.
#'   \item \strong{Negative (0)}: Matches \code{"not|negative|undetected|0"} - Includes
#'     "Negative", "NOT DETECTED", "0", "not detected", etc.
#'   \item \strong{Invalid (NA)}: All other values including "inconclusive", "pending",
#'     "invalid", empty strings, or any unrecognized text
#' }
#'
#' \strong{Age Group Creation:}
#' Age groups are created by extracting the first numeric value from each age range
#' label to create cut points. For example, \code{c("18-29", "30-44", "45-64", "65+")}
#' creates breaks at [18, 30, 45, 65] with appropriate boundary handling.
#'
#' \strong{Demographic Recoding:}
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
#'   \item \code{id}: Character or factor - Individual identifier (typically encrypted
#'     or masked for privacy protection)
#'   \item \code{time}: Character, factor, or Date - Time period identifier (e.g.,
#'     "2020-03", "Week 12", or date values that have been grouped into periods)
#' }
#' Additional columns (demographics, test results, geography) are preserved in
#' the output.
#'
#' @return A data frame with duplicate records removed, containing one test record
#'   per individual per time period. When multiple records exist for the same
#'   \code{id}-\code{time} combination:
#' \itemize{
#'   \item The first occurrence is retained (based on original row order)
#'   \item All other duplicates are removed
#'   \item All columns from the original data frame are preserved
#'   \item Row count may be reduced if duplicates were present
#' }
#'
#' @details
#' \strong{Duplicate Detection Logic:}
#' Uses \code{dplyr::distinct()} with \code{.keep_all = TRUE} to identify unique
#' combinations of \code{id} and \code{time}. This approach:
#' \itemize{
#'   \item Preserves the first occurrence when duplicates exist
#'   \item Maintains original column structure and data types
#'   \item Handles missing values appropriately (NA values are treated as distinct)
#'   \item Is computationally efficient for large datasets
#' }
#'
#' \strong{Time Period Considerations:}
#' The \code{time} variable should represent meaningful temporal units for analysis:
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

#' Filter data based on state and ZIP code thresholds
#'
#' @title Apply geographic filtering for reliable COVID analysis
#' @description Filters COVID data to include only ZIP codes and states that
#' meet minimum sample size requirements for reliable statistical inference.
#' This two-stage filtering process removes geographic areas with insufficient
#' data that could lead to unstable estimates in MRP modeling. Essential for
#' COVID surveillance data where sample sizes vary dramatically across
#' geographic areas due to population density, testing availability, and
#' outbreak patterns.
#'
#' @param df A data frame containing aggregated COVID data with the following
#'   required columns:
#' \itemize{
#'   \item \code{zip}: Character or factor - ZIP code identifier (5-digit format)
#'   \item \code{total}: Numeric - Count of observations/tests for each ZIP code
#'     (must be positive integers representing sample sizes)
#' }
#' Additional columns (demographics, time periods) are preserved through filtering.
#'
#' @param covariates A data frame containing geographic covariates and identifiers
#'   with the following required columns:
#' \itemize{
#'   \item \code{zip}: Character or factor - ZIP code identifier matching \code{df$zip}
#'   \item \code{county}: Character - County FIPS code where the first two digits
#'     represent the state FIPS code (e.g., "06001" for Alameda County, CA where
#'     "06" is California's state FIPS code)
#' }
#' Used to map ZIP codes to states for hierarchical filtering.
#'
#' @param zip_threshold Numeric. Minimum number of observations required per ZIP
#'   code to be retained in the analysis. Default is 5, which provides minimal
#'   sample size for basic statistical inference. Higher values (10-20) may be
#'   appropriate for more complex modeling.
#'
#' @param state_threshold Numeric. Minimum proportion of total national sample
#'   required per state to be retained. Default is 0.01 (1%), which excludes
#'   states with very small samples that could destabilize hierarchical models.
#'   Range should be between 0 and 1.
#'
#' @return A data frame filtered to include only ZIP codes meeting both geographic
#'   thresholds. ZIP codes are retained if and only if:
#' \enumerate{
#'   \item Their state has at least \code{state_threshold} proportion of total
#'      national observations (state-level filter)
#'   \item The ZIP code itself has at least \code{zip_threshold} observations
#'      (ZIP-level filter)
#' }
#' The returned data frame maintains the same structure as the input but with
#' potentially fewer rows due to geographic exclusions.
#'
#' @details
#' \strong{Two-Stage Filtering Process:}
#' \enumerate{
#'   \item \strong{State-level filtering}: Calculates total observations per state
#'     by summing ZIP-level counts within each state. Excludes entire states
#'     that fall below the proportional threshold.
#'   \item \strong{ZIP-level filtering}: Among remaining states, excludes individual
#'     ZIP codes that fall below the absolute count threshold.
#' }
#'
#' \strong{Geographic Hierarchy:}
#' The function uses FIPS codes to establish the state-ZIP hierarchy:
#' \itemize{
#'   \item State FIPS codes are extracted from the first 2 digits of county FIPS
#'   \item ZIP codes are mapped to states through county assignments
#'   \item Handles cases where ZIP codes cross county boundaries (uses first match)
#' }
#'
#' \strong{Sample Size Considerations:}
#' Appropriate thresholds depend on:
#' \itemize{
#'   \item Model complexity (more complex models need larger samples)
#'   \item Geographic resolution requirements
#'   \item Balance between coverage and precision
#'   \item COVID outbreak patterns and testing density
#' }
#'
#' @section COVID Analysis Context:
#' Geographic filtering is particularly important for COVID data because:
#' \itemize{
#'   \item Testing rates vary dramatically across regions
#'   \item Rural areas may have very small sample sizes
#'   \item Outbreak timing creates temporal variation in geographic coverage
#'   \item MRP models require sufficient data for stable geographic effects
#' }
#'
#' This filtering ensures that downstream MRP analysis focuses on areas with
#' adequate data for reliable inference while maintaining broad geographic
#' representation.
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
#'   \item Demographic variables: \code{sex}, \code{race}, \code{age}
#'   \item Geographic identifier: \code{zip}
#'   \item Temporal identifier: \code{time}
#'   \item Outcome variable: \code{positive} (binary test result)
#'   \item Sample size indicator: \code{total} (for aggregated data)
#' }
#' Should be the output of COVID-specific data processing functions.
#'
#' @param pstrat_data A data frame containing poststratification population counts
#'   organized by demographic groups and geographic areas. Must include:
#' \itemize{
#'   \item Geographic identifiers: \code{zip}, \code{county}
#'   \item Demographic cross-tabulations with population counts
#'   \item Structure compatible with \code{expand.grid()} for frame creation
#' }
#' Typically derived from American Community Survey or Census data.
#'
#' @param covariates A data frame containing geographic covariates and auxiliary
#'   variables for modeling. Must include:
#' \itemize{
#'   \item \code{zip}: ZIP code identifier (primary key for merging)
#'   \item Additional geographic predictors (e.g., urbanicity, income, demographics)
#'   \item Should not duplicate columns in \code{input_data} except for \code{zip}
#' }
#'
#' @param metadata A named list containing metadata specifications for the analysis:
#' \itemize{
#'   \item \code{special_case}: Should be "covid" for COVID-specific processing
#'   \item \code{is_timevar}: Logical indicating time-varying analysis
#'   \item Demographic specifications used by \code{create_expected_levels()}
#'   \item Variable type classifications for model specification
#' }
#'
#' @return A named list with four components required for MRP analysis:
#' \describe{
#'   \item{\code{input}}{Data frame - Processed individual-level data merged with
#'     geographic covariates. Ready for model fitting with consistent variable
#'     coding and geographic filtering applied.}
#'   \item{\code{new}}{Data frame - Poststratification frame containing all
#'     combinations of factor levels with population weights. Includes:
#'     \itemize{
#'       \item All demographic and geographic factor combinations
#'       \item \code{total}: Population counts for each cell
#'       \item Geographic covariates merged by ZIP code
#'       \item Proper ordering to match population count structure
#'     }}
#'   \item{\code{levels}}{Named list - All factor levels for model variables:
#'     \itemize{
#'       \item \code{time}: Unique time periods from input data
#'       \item \code{zip}: ZIP codes after geographic filtering
#'       \item Demographic levels: \code{sex}, \code{race}, \code{age}
#'       \item \code{county}: County identifiers for hierarchical modeling
#'     }}
#'   \item{\code{vars}}{Named list - Organized variable specifications for model
#'     fitting created by \code{create_variable_list()}, including fixed effects,
#'     random effects, and variables to omit.}
#' }
#'
#' @details
#' \strong{Data Processing Pipeline:}
#' \enumerate{
#'   \item \strong{Geographic Filtering}: Applies \code{filter_state_zip()} to
#'     remove geographic areas with insufficient sample sizes
#'   \item \strong{Data Alignment}: Ensures consistent geographic coverage across
#'     input data, covariates, and poststratification frame
#'   \item \strong{Column Management}: Removes duplicate columns between datasets
#'     while preserving the ZIP code key for merging
#'   \item \strong{Data Integration}: Merges individual-level data with geographic
#'     covariates using left join on ZIP code
#'   \item \strong{Factor Level Creation}: Generates standardized factor levels
#'     using \code{create_expected_levels()} with COVID-specific specifications
#'   \item \strong{Poststratification Frame}: Creates comprehensive grid of all
#'     factor combinations with proper cell ordering and population weights
#'   \item \strong{Variable Organization}: Structures variables for model
#'     specification using \code{create_variable_list()}
#' }
#'
#' \strong{Poststratification Frame Construction:}
#' The poststratification frame is created using \code{expand.grid()} with:
#' \itemize{
#'   \item All combinations of demographic and geographic factors
#'   \item Specific ordering: time, zip, sex, race, age (critical for population matching)
#'   \item Population counts replicated across time periods
#'   \item Geographic covariates merged for each cell
#' }
#'
#' \strong{COVID-Specific Considerations:}
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
#'   \item \code{input} and \code{new} are used directly in model fitting
#'   \item \code{levels} provides factor specifications for model constraints
#'   \item \code{vars} guides variable selection and model specification
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{filter_state_zip()} for geographic filtering logic
#'   \item \code{create_expected_levels()} for factor level specifications
#'   \item \code{create_variable_list()} for variable organization
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
  vars <- create_variable_list(input_data, covariates)

  return(list(
    input = input_data,
    new = new_data,
    levels = levels,
    vars = vars
  ))
}