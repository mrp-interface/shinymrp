#' Read data from various file formats
#'
#' @description Reads data from CSV, Excel (xlsx/xls), or SAS (sas7bdat) files
#' based on the file extension. Automatically detects the file format and uses
#' the appropriate reading function with optimized parameters for each format.
#'
#' @param file_path Character string. Path to the data file to be read. Must be
#'   a valid file path with one of the supported extensions: .csv, .xlsx, .xls,
#'   or .sas7bdat.
#'
#' @return A data frame containing the data from the specified file. Column types
#'   are automatically detected based on the file format:
#' \itemize{
#'   \item CSV files: Uses readr::read_csv() with column type detection disabled
#'   \item Excel files: Uses readxl::read_excel() with increased guess_max for better type detection
#'   \item SAS files: Uses haven::read_sas() with native SAS data types preserved
#' }
#'
#' @noRd
#'
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom haven read_sas
#' @importFrom stringr str_ends
.read_data <- function(file_path) {
  if(stringr::str_ends(file_path, "csv")) {
    readr::read_csv(file_path, show_col_types = FALSE)
  } else if (stringr::str_ends(file_path, "(xlsx|xls)")) {
    readxl::read_excel(file_path, guess_max = 5000)
  } else if (stringr::str_ends(file_path, "sas7bdat")) {
    haven::read_sas(file_path)
  }
}

#' Clean and standardize column names
#'
#' @description Converts column names to a standardized format by applying
#' multiple cleaning transformations. Creates consistent, R-friendly variable
#' names that follow best practices for data analysis and modeling.
#'
#' @param names Character vector of column names to be cleaned. Can contain
#'   mixed case, numbers, special characters, and whitespace.
#'
#' @return Character vector of cleaned column names with the following transformations:
#' \itemize{
#'   \item Converted to lowercase for consistency
#'   \item Numbers replaced with underscores
#'   \item Non-alphabetic characters (spaces, punctuation) replaced with underscores
#'   \item Multiple consecutive underscores collapsed to single underscores
#'   \item Leading and trailing underscores removed
#' }
#'
#' @noRd
.clean_names <- function(names) {
  names %>% 
    tolower() %>% 
    gsub("[0-9]", "_", .) %>%
    gsub("[^[:alpha:]]", "_", .) %>%
    gsub("_{2,}", "_", .) %>%
    gsub("^_|_$", "", .)
}


#' Clean character columns in a data frame
#'
#' @description Standardizes all character columns in a data frame by converting
#' to lowercase and removing leading/trailing whitespace. This ensures consistent
#' string formatting for categorical variables and text data used in MRP analysis.
#'
#' @param df Data frame containing character columns to be cleaned. Non-character
#'   columns are left unchanged.
#'
#' @return Data frame with cleaned character columns:
#' \itemize{
#'   \item All character columns converted to lowercase
#'   \item Leading and trailing whitespace removed from character columns
#'   \item Non-character columns (numeric, logical, etc.) remain unchanged
#'   \item Original data frame structure and row order preserved
#' }
#'
#' @noRd
#'
#' @importFrom dplyr mutate across where
#' @importFrom stringr str_trim
.clear_chr <- function(df) {
  # Convert character columns to lowercase and trim whitespace
  df %>% dplyr::mutate(
    dplyr::across(dplyr::where(is.character), ~ stringr::str_trim(tolower(.x)))
  )
}

#' Validate and clean geocodes
#'
#' @description Validates geocodes by checking if they conform to the standard
#' 5-digit format required for ZIP codes and county FIPS codes. Invalid geocodes
#' are replaced with NA to ensure data quality for geographic analysis and mapping.
#'
#' @param geocodes Vector of geocodes to be validated. Can be numeric, character,
#'   or mixed types. Common inputs include ZIP codes, county FIPS codes, or
#'   other 5-digit geographic identifiers.
#'
#' @return Character vector of validated geocodes with the same length as input:
#' \itemize{
#'   \item Valid geocodes: Exactly 5 digits, returned as character strings
#'   \item Invalid geocodes: Replaced with NA_character_
#'   \item Original NA values: Preserved as NA_character_
#' }
#'
#' @noRd
.find_bad_geocode <- function(geocodes) {
  # Coerce to character (so that NA stays NA_character_)
  geocodes <- as.character(geocodes)
  
  # Valid if exactly 5 digits
  valid <- grepl("^[0-9]{5}$", geocodes)
  
  # Replace anything not matching (including non‐character inputs) with NA
  geocodes[!valid] <- NA_character_
  
  return(geocodes)
}

#' Format geographic identifier columns
#'
#' @title Format geographic identifier columns
#' @description Formats geographic identifier columns (zip, county, state) to
#' ensure consistent formatting with proper leading zeros and validation. This
#' standardization is essential for accurate geographic linking and mapping in
#' MRP analysis.
#'
#' @param df Data frame containing geographic identifier columns. Expected columns
#'   include 'zip', 'county', and/or 'state'. Other columns are left unchanged.
#'
#' @return Data frame with properly formatted geographic identifiers:
#' \itemize{
#'   \item zip: 5-digit character strings with leading zeros (e.g., "01234")
#'   \item county: 5-digit character strings with leading zeros (FIPS format)
#'   \item state: 2-digit character strings with leading zeros (FIPS format)
#'   \item Other columns: Unchanged
#' }
#' 
#' @noRd
.format_geocode <- function(df) {
  if ("zip" %in% names(df)) {
    if (is.numeric(df$zip)) {
      df$zip <- sprintf("%05d", df$zip)
    } else {
      df$zip <- .find_bad_geocode(df$zip)
    }
  }

  if ("county" %in% names(df)) {
    if (is.numeric(df$county)) {
      df$county <- sprintf("%05d", df$county)
    } else {
      df$county <- .find_bad_geocode(df$county)
    }
  }

  if ("state" %in% names(df)) {
    if (is.numeric(df$state)) {
      df$state <- sprintf("%02d", df$state)
    }
  }

  return(df)
}


#' Main data cleaning function
#'
#' @title Comprehensive data cleaning pipeline
#' @description Performs comprehensive data cleaning through multiple standardization
#' steps. This is the primary data cleaning function used throughout the shinymrp
#' package to ensure consistent data formatting for MRP analysis and modeling.
#'
#' @param df Data frame to be cleaned. Can contain mixed data types, inconsistent
#'   formatting, and various representations of missing values.
#' @param na_strings Character vector of strings to be converted to NA values.
#'   Default includes common representations of missing values: "", "na", "n/a",
#'   "none", "null", "unknown". Case-insensitive matching is applied.
#'
#' @return Comprehensively cleaned data frame with:
#' \itemize{
#'   \item Standardized column names (lowercase, underscores, no special characters)
#'   \item Duplicate columns removed (keeping first occurrence)
#'   \item Character columns converted to lowercase and trimmed
#'   \item Common NA string representations converted to actual NA values
#'   \item Geographic identifiers properly formatted with leading zeros
#'   \item Consistent data structure ready for further processing
#' }
#'
#' @noRd
#' @importFrom dplyr mutate across everything if_else
.clean_data <- function(
    df,
    na_strings = c("", "na", "n/a", "none", "null", "unknown")
) {

  # Clean column names
  names(df) <- .clean_names(names(df))

  # Remove duplicate column names (keeping first occurrence)
  if (any(duplicated(names(df)))) {
    df <- df[, !duplicated(names(df))]
  }

  # Convert character columns to lowercase and trim whitespace
  df <- .clear_chr(df)

  # Convert common NA strings to actual NA
  df <- df %>%
    dplyr::mutate(dplyr::across(dplyr::everything(),
                 ~dplyr::if_else(.x %in% na_strings, NA, .x)))

  # Format geographic identifiers
  df <- .format_geocode(df)
  
  return(df)
}


#' Remove rows with rare values
#'
#' @description Filters out rows from a data frame where any column contains values that
#' appear less frequently than the specified threshold. Columns listed in
#' .const()$vars$ignore are excluded from this filtering process.
#'
#' @param df A data frame to filter
#' @param threshold Numeric threshold. Values appearing fewer than this many
#'   times in any column will cause the entire row to be removed
#' 
#' @return A data frame with rows containing rare values removed. The returned
#'   data frame maintains the same structure as the input but with fewer rows.
#' 
#' @noRd 
.omit_rare_rows <- function(df, threshold) {
  # For each column, compute frequencies and flag rare rows
  keep <- rep(TRUE, nrow(df))
  for (col in setdiff(names(df), .const()$vars$ignore)) {
    freqs <- table(df[[col]])
    rare_values <- names(freqs[freqs < threshold])
    keep <- keep & !(df[[col]] %in% rare_values)
  }

  return(df[keep, , drop = FALSE])
}


#' Rename columns based on expected variable names
#'
#' @description Renames columns in the data frame to match expected variable
#' names defined in the global constants. Uses pattern matching to identify
#' columns that correspond to standard demographic, geographic, and outcome
#' variables used in MRP analysis.
#'
#' @param df Data frame with columns to be renamed. Column names will be matched
#'   against expected patterns to identify standard variables.
#' @param covid_indiv Logical. If TRUE, uses COVID-specific individual data
#'   column renaming via .rename_columns_covid(). Default is FALSE for general
#'   data processing.
#'
#' @return Data frame with renamed columns matching standard variable names:
#' \itemize{
#'   \item Individual variables: sex, race, age, edu, time
#'   \item Geographic variables: zip, county, state
#'   \item Outcome variables: positive, outcome, total
#'   \item Other columns: Unchanged if no pattern match found
#' }
#'
#' @noRd
.rename_columns <- function(
    df,
    covid_indiv = FALSE
) {
  if (covid_indiv) {
    return(.rename_columns_covid(df))
  }

  target_names <- c(
    .const()$vars$indiv,
    .const()$vars$geo,
    .const()$vars$ignore
  )

  current_names <- names(df)
  rename_map <- sapply(target_names, function(target) {
    matches <- grep(target, current_names, ignore.case = TRUE, value = TRUE)
    if(length(matches) > 0) matches[1] else NULL
  })
  
  # Filter out NULLs and create rename specification
  rename_map <- rename_map[!sapply(rename_map, is.null)]
  if(length(rename_map) == 0) return(df)
  
  # Create the renaming specification (new_name = old_name)
  rename_spec <- stats::setNames(rename_map, names(rename_map))
  
  # Apply renaming
  dplyr::rename(df, !!!rename_spec)
}

#' Remove duplicate records from data
#'
#' @description Removes duplicate records from the data frame. For COVID data,
#' uses specialized duplicate removal function.
#'
#' @param data Data frame to remove duplicates from.
#' @param is_covid Logical. If TRUE, uses COVID-specific duplicate removal.
#'   Default is FALSE.
#'
#' @return Data frame with duplicate records removed.
#'
#' @noRd
.remove_duplicates <- function(data, is_covid = FALSE) {
  if (is_covid) {
    data <- .remove_duplicates_covid(data)
  }

  return(data)
}

#' Impute missing values using frequency-based sampling
#'
#' @description Imputes missing values in a vector by sampling from the
#' observed values based on their frequency distribution. This method preserves
#' the original distribution of the variable while filling in missing data,
#' making it suitable for categorical and discrete variables in MRP analysis.
#'
#' @param v Vector with potential missing values to be imputed. Can be numeric,
#'   character, factor, or any vector type. Missing values should be represented
#'   as NA.
#'
#' @return Vector with missing values imputed:
#' \itemize{
#'   \item Missing values replaced by sampling from observed values
#'   \item Sampling probabilities proportional to observed frequencies
#'   \item Original data type and non-missing values preserved
#'   \item If no missing values exist, returns original vector unchanged
#' }
#'
#' @noRd
.impute <- function(v) {
  cond <- is.na(v)
 
  if(sum(cond) == 0) {
    return(v)
  }

  tbl <- table(v[!cond])
  freqs <- as.numeric(tbl)
  lvls <- names(tbl)

  if(is.numeric(v)) {
    lvls <- as.numeric(lvls)
  }

  v[cond] <- sample(
    lvls,
    prob = freqs / sum(freqs),
    size = sum(cond),
    replace = TRUE
  )

  return(v)
}

#' Get period indices and first-of-period dates for week/month/year
#'
#' @param date_strings A character vector of dates ("YYYY-MM-DD").
#' @param time_freq         One of "week", "month", or "year".
#' @return A list with:
#'   - indices: integer vector giving the 1-based period index for each input date
#'   - timeline: Date vector of the first date of every period between the min and max
#'
#' @noRd
.get_time_indices <- function(date_strings, time_freq = c("week", "month", "year")) {
  time_freq <- base::match.arg(time_freq)
  dates <- base::as.Date(date_strings)
  
  # 1) Floor each date to the start of its period
  period_starts <- switch(time_freq,
                          week  = lubridate::floor_date(dates, unit = "week",  week_start = 1),
                          month = lubridate::floor_date(dates, unit = "month"),
                          year  = lubridate::floor_date(dates, unit = "year")
  )
  
  # 2) Build a complete sequence of period-start dates
  seq_by <- switch(time_freq,
                   week  = "1 week",
                   month = "1 month",
                   year  = "1 year"
  )
  start <- base::min(period_starts, na.rm = TRUE)
  end   <- base::max(period_starts, na.rm = TRUE)
  timeline <- base::seq(from = start, to = end, by = seq_by)
  
  # 3) Map each date’s period to its position in the full timeline
  indices <- base::match(period_starts, timeline)
  
  list(
    indices  = indices,
    timeline = timeline
  )
}


#' Convert Date Column to Character
#'
#' A helper function that converts a 'date' column in a data frame to character type
#' if the column exists. This is useful for ensuring consistent data types when
#' working with date columns that need to be treated as strings.
#'
#' @param df A data frame that may contain a column named 'date'
#'
#' @return A data frame with the 'date' column converted to character type if it exists,
#'   otherwise returns the original data frame unchanged
#'
#' @noRd
.convert_date_to_character <- function(df) {
  if ("date" %in% names(df)) {
    df <- df %>% dplyr::mutate(date = as.character(.data$date))
  }
  return(df)
}

#' Add time indices to time-varying data
#'
#' @title Add time indices to data frame
#' @description Converts date columns to cumulative time indices for time-varying
#' analysis. Creates a complete timeline of periods and adds corresponding time
#' indices to the data frame. Essential for time-varying MRP models that require
#' consistent temporal indexing.
#'
#' @param df Data frame containing time-related columns. Must contain either a
#'   'date' column with date strings or existing time indices matching
#'   .const()$vars$time specification.
#' @param time_freq Character string specifying the frequency of time indices to be
#'  added. Must be one of "week", "month", or "year". Determines how dates are
#'  grouped into time periods.
#'
#' @return Data frame with added time indices and complete timeline:
#' \itemize{
#'   \item time: Numeric column with cumulative time indices starting from 1
#'   \item date: Character column with first date of each period (if dates provided)
#'   \item Original columns preserved
#'   \item Complete timeline created via full_join to ensure no missing periods
#' }
#'
#' @noRd
#'
#' @importFrom dplyr full_join
.add_time_indices <- function(df, time_freq) {
  common <- intersect(names(df), .const()$vars$time)

  if (length(common) == 1 && "date" %in% common) {
    # convert date to time indices
    out <- .get_time_indices(df$date, time_freq)
    df$time <- out$indices

    # add the column containing first dates of the periods
    df <- df %>% dplyr::select(-"date")
    df <- df %>%
      dplyr::full_join(
        data.frame(
          time = 1:max(df$time),
          date = as.character(out$timeline)
        ),
        by = "time"
      )
  } else if (length(common) == 0) {
    stop("No dates or time indices found.")
  }

  return(df)
}

#' Extract and format unique dates from data frame
#'
#' @description Extracts unique dates from the 'date' column of a data frame,
#' removes missing values, sorts them, and formats them according to the
#' global date format setting.
#'
#' @param df Data frame containing a 'date' column.
#'
#' @return Character vector of formatted unique dates in sorted order.
#'
#' @noRd
.get_dates <- function(df) {
  df$date %>%
    stats::na.omit() %>%
    unique() %>%
    as.Date() %>%
    sort() %>%
    format(.const()$ui$format$date) %>%
    as.character()
}

#' Recode values to match expected levels
#'
#' @description Recodes demographic and response variables to match expected
#' levels. For COVID data, uses a specialized recoding function. Handles age
#' binning, categorical variable validation, and binary response coding.
#'
#' @param df Data frame with variables to be recoded.
#' @param expected_levels List containing expected levels for each variable.
#' @param is_covid Logical. If TRUE, uses COVID-specific recoding. Default is FALSE.
#'
#' @return Data frame with recoded variables matching expected levels.
#'
#' @noRd
#'
#' @importFrom dplyr mutate if_else case_match
#' @importFrom rlang .data
.recode_values <- function(df, expected_levels, is_covid=FALSE) {
  if (is_covid) {
    return(.recode_covid(df, expected_levels))
  }

  # this function assumes that strings are already lower case
  ranges <- expected_levels$age
  age_bounds <- regmatches(
    ranges,
    regexpr("^\\d+", ranges)
  ) %>%
    as.numeric()
  breaks <- c(-1, age_bounds[2:length(age_bounds)] - 1, 200)
  colnames <- names(df)

  df <- df %>% dplyr::mutate(
    sex  = if("sex" %in% colnames) dplyr::if_else(.data$sex %in% expected_levels$sex, .data$sex, NA),
    race = if("race" %in% colnames) dplyr::if_else(.data$race %in% c(expected_levels$race, NA), .data$race, "other"),
    age  = if("age" %in% colnames) cut(df$age, breaks, ranges) %>% as.character(),
    edu  = if("edu" %in% colnames) dplyr::if_else(.data$edu %in% expected_levels$edu, .data$edu, NA),
    positive = if("positive" %in% colnames) dplyr::case_match(
      as.character(.data$positive),
      c("positive", "detected", "yes", "y", "true", "1") ~ 1,
      c("negative", "undetected", "no", "n", "false", "0") ~ 0
    )
  )

  return(df)
}

#' Filter GeoJSON features by geographic identifiers
#'
#' @description Filters GeoJSON features to include only those with FIPS codes
#' that match the provided geographic identifiers.
#'
#' @param geojson GeoJSON object containing geographic features.
#' @param geoids Character vector of geographic identifiers (FIPS codes) to filter by.
#' @param omit Logical. Currently unused parameter. Default is FALSE.
#'
#' @return Filtered GeoJSON object containing only matching features, or NULL
#'   if input is NULL.
#'
#' @noRd
#'
.filter_geojson <- function(geojson, geoids, omit = FALSE) {
  if(is.null(geojson) | is.null(geoids)) {
    return(NULL)
  }

  geojson$features <- purrr::keep(
    geojson$features,
    function(f) !is.null(.nullify(f$properties$fips)) && f$properties$fips %in% geoids
  )

  return(geojson)
}

#' Filter data based on state and ZIP code thresholds
#'
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
#'   \item `zip`: Character or factor - ZIP code identifier (5-digit format)
#'   \item `total`: Numeric - Count of observations/tests for each ZIP code
#'     (must be positive integers representing sample sizes)
#' }
#' Additional columns (demographics, time periods) are preserved through filtering.
#'
#' @param zip_county_state A data frame containing geographic covariates and identifiers
#'   with the following required columns:
#' \itemize{
#'   \item `zip`: Character or factor - ZIP code identifier matching `df$zip`
#'   \item `county`: Character - County FIPS code where the first two digits
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
#'   \item Their state has at least `state_threshold` proportion of total
#'      national observations (state-level filter)
#'   \item The ZIP code itself has at least `zip_threshold` observations
#'      (ZIP-level filter)
#' }
#' The returned data frame maintains the same structure as the input but with
#' potentially fewer rows due to geographic exclusions.
#'
#' @importFrom dplyr group_by summarize mutate select distinct inner_join filter ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @noRd
.filter_state_zip <- function(
    df,
    zip_county_state,
    zip_threshold = 0,
    state_threshold = 0
) {
  
  zip_count <- df %>%
    dplyr::group_by(.data$zip) %>%
    dplyr::summarize(count = dplyr::n())

  # create a table containing state, zip and zip count
  state_zip <- zip_county_state %>%
    dplyr::mutate(state = substr(.data$fips, 1, 2)) %>%
    dplyr::select(.data$zip, .data$state) %>%
    dplyr::distinct(.data$zip, .keep_all = TRUE) %>%
    dplyr::inner_join(zip_count, by = "zip")

  # filter based on proportion of state
  N <- sum(state_zip$count)
  state_zip <- state_zip %>%
    dplyr::group_by(.data$state) %>%
    dplyr::filter(sum(.data$count) > state_threshold * N) %>%
    dplyr::ungroup()

  # filter based on number of zip
  state_zip <- state_zip %>%
    dplyr::group_by(.data$zip) %>%
    dplyr::filter(sum(.data$count) > zip_threshold) %>%
    dplyr::ungroup()

  df <- df %>% dplyr::filter(.data$zip %in% state_zip$zip)

  return(df)
}

#' Convert geographic identifiers to FIPS codes
#'
#' @description Converts geographic identifiers (names, abbreviations, or codes)
#' to standardized FIPS codes used for geographic linking in MRP analysis. Handles
#' both numeric codes that need formatting and text identifiers that require
#' lookup table matching.
#'
#' @param vec Vector of geographic identifiers to be converted. Can contain:
#'   \itemize{
#'     \item Numeric FIPS codes (will be formatted with leading zeros)
#'     \item State/county names (e.g., "California", "Los Angeles County")
#'     \item State abbreviations (e.g., "CA", "NY")
#'     \item Mixed formats within the same vector
#'   }
#' @param link_geo Character string specifying geographic level. Must be either
#'   "county" or "state" as defined in .const()$vars$geo2. Determines the lookup
#'   table and formatting used.
#'
#' @return Character vector of FIPS codes with proper formatting:
#' \itemize{
#'   \item State FIPS: 2-digit codes with leading zeros (e.g., "06" for California)
#'   \item County FIPS: 5-digit codes with leading zeros (e.g., "06037" for Los Angeles)
#'   \item NA values for identifiers that cannot be matched
#' }
#'
#' @noRd
.to_fips <- function(vec, link_geo) {
  checkmate::assert_choice(
    link_geo,
    choices = .const()$vars$geo2,
    null.ok = FALSE
  )

  lookup_df <- switch(
    link_geo,
    county = fips_$county,
    state = fips_$state
  )
  fmt <- if(link_geo == "state") "%02d" else "%05d"

  if(is.numeric(vec)) {
    # If column already contains FIPS codes, return with proper formatting
    fips <- sprintf(fmt, vec)
  } else {
    # Otherwise, find best matching column
    counts <- lookup_df %>% apply(2, function(c) sum(vec %in% c))
    colname <- names(counts)[which.max(counts)]

    fips <- lookup_df$fips[match(vec, lookup_df[[colname]])]
  }

  return(fips)
}

#' Extract geographic predictors from data
#'
#' @description Identifies columns that have constant values within each
#' geographic unit, indicating they are geographic predictors rather than
#' individual-level variables.
#'
#' @param df Data frame containing geographic and predictor variables.
#' @param geo_col Character string specifying the name of the geographic column.
#'
#' @return Data frame containing unique combinations of geographic units and
#'   their associated geographic predictors.
#'
#' @noRd
#'
#' @importFrom dplyr group_by summarize_all select distinct all_of sym n_distinct
.get_geo_predictors <- function(df, geo_col) {
  bool <- df %>%
    dplyr::group_by(!!dplyr::sym(geo_col)) %>%
    dplyr::summarize_all(dplyr::n_distinct) %>%
    lapply(function(c) all(c == 1)) %>%
    unlist()

  geo_pred_cols <- names(bool)[bool]

  geo_preds <- df %>%
    dplyr::select(dplyr::all_of(c(geo_col, geo_pred_cols))) %>%
    dplyr::distinct(!!dplyr::sym(geo_col), .keep_all = TRUE)

  return(geo_preds)
}

#' Find the smallest geographic scale in data
#'
#' @description Identifies the smallest (most granular) geographic scale
#' present in the data based on the hierarchy defined in .const()$vars$geo.
#'
#' @param col_names Character vector of column names in the data.
#'
#' @return List containing:
#'   \item{geo}{Character string of the smallest geographic scale}
#'   \item{idx}{Numeric index of the geographic scale in the hierarchy}
#'   Returns NULL if no geographic variables are found.
#'
#' @noRd
#'
.get_smallest_geo <- function(col_names) {
  geo_all <- .const()$vars$geo

  # Find the smallest geographic index
  idx <- match(col_names, geo_all) %>% stats::na.omit()
  if (length(idx) == 0) {
    return(NULL)
  }

  smallest_geo_index <- min(idx)
  smallest_geo <- geo_all[smallest_geo_index]

  return(list(
    geo = smallest_geo,
    idx = smallest_geo_index
  ))
}

#' Get possible geographic scales for data
#'
#' @description Determines all possible geographic scales that can be used with
#' the data based on the smallest geographic scale present. Returns geographic
#' variables from the smallest scale up to the largest (national) scale according
#' to the geographic hierarchy defined in .const()$vars$geo.
#'
#' @param col_names Character vector of column names in the data frame to check
#'   for geographic variables.
#'
#' @return Character vector of geographic scale names from smallest to largest
#'   that can be used with the data, or NULL if no geographic variables are found.
#'   Geographic scales follow the hierarchy: zip -> county -> state.
#'
#' @noRd
.get_possible_geos <- function(col_names) {
  smallest <- .get_smallest_geo(col_names)
  if (is.null(smallest)) {
    return(NULL)
  }

  # Return all geographic variables from the smallest to the largest scale
  return(.const()$vars$geo[smallest$idx:length(.const()$vars$geo)])
}

#' Append geographic variables at larger scales
#'
#' @description Adds geographic variables at larger scales (county, state)
#' based on the smallest geographic scale present in the data. Converts
#' geographic names to FIPS codes and joins with geographic crosswalk data.
#'
#' @param input_data Data frame containing input data with geographic variables.
#' @param zip_county_state Data frame containing ZIP code to county/state crosswalk.
#'
#' @return Data frame with additional geographic variables at larger scales.
#'
#' @noRd
#'
#' @importFrom dplyr select rename mutate distinct
#' @importFrom rlang .data
.append_geo <- function(input_data, zip_county_state) {
  # get the smallest geographic scale in the data
  smallest <- .get_smallest_geo(names(input_data))

  # Get geographic variables at current and larger scales
  geo_vars <- .get_possible_geos(names(input_data))

  if (is.null(geo_vars)) {
    return(input_data)
  }

  # Prepare geographic crosswalk
  zip_county_state <- zip_county_state %>%
    dplyr::select(.data$zip, .data$fips) %>%
    dplyr::rename(county = .data$fips) %>%
    dplyr::mutate(state = substr(.data$county, 1, 2)) %>%
    dplyr::select(dplyr::all_of(geo_vars)) %>%
    dplyr::distinct()
  
  # Convert names to FIPS for smallest geographic scale
  if (smallest$geo != "zip") { 
    input_data[[smallest$geo]] <- .to_fips(
      input_data[[smallest$geo]], 
      smallest$geo
    )
  }

  # Join geographic variables
  input_data <- .clean_left_join(input_data, zip_county_state, by = smallest$geo)

  # Convert names to GEOIDs for larger geographic scales
  for (geo in setdiff(geo_vars, smallest$geo)) {
    if (geo != "zip") {
      input_data[[geo]] <- .to_fips(
        input_data[[geo]], 
        geo
      )
    }
  }
  

  return(input_data)
}

#' Convert columns to factors with specified levels
#'
#' @description Converts specified columns in a data frame to factors using
#' predefined levels. Only processes columns that exist in both the data frame
#' and the levels specification.
#'
#' @param df Data frame with columns to be converted to factors.
#' @param levels Named list where names correspond to column names and values
#'   are character vectors of factor levels.
#'
#' @return Data frame with specified columns converted to factors.
#'
#' @noRd
.as_factor <- function(df, levels) {
  # Find columns that exist in both df and have defined levels
  cols_to_convert <- intersect(names(df), names(levels))
  
  # Apply factor conversion to each column
  for(col in cols_to_convert) {
    if(!is.null(levels[[col]])) {
      df[[col]] <- factor(df[[col]], levels = levels[[col]])
    }
  }
  
  return(df)
}

#' Find nested relationships between categorical variables
#'
#' @description Identifies pairs of categorical variables that have a bijective
#' (one-to-one) relationship, indicating potential nesting or perfect correlation.
#'
#' @param df Data frame containing categorical variables to test.
#' @param cols Character vector of column names to test for nesting relationships.
#' @param sep Character string used as separator when combining variable values.
#'   Default is "---".
#'
#' @return Character vector of variable pairs that show nesting relationships,
#'   formatted as "var1:var2".
#'
#' @noRd
#'
.find_nested <- function(df, cols, sep = "---") {
  # generate all 2-column combinations
  pairs <- utils::combn(cols, 2, simplify = FALSE)
  
  # test each pair for a bijection via approach 2
  is_bij <- vapply(pairs, function(pr) {
    x  <- df[[pr[1]]]
    y  <- df[[pr[2]]]
    ux <- unique(x)
    uy <- unique(y)
    up <- unique(paste(x, y, sep = sep))
    length(ux) == length(uy) && length(up) == length(ux)
  }, logical(1))
  
  # return only the names of the true pairs, collapsed with “:”
  vapply(pairs[is_bij], paste, collapse = ":", FUN.VALUE = "")
}


#' Perform a clean left join between data frames
#'
#' @description Performs a left join while avoiding column name conflicts by
#' removing common columns (except join keys) from the right data frame before joining.
#'
#' @param df1 Left data frame for the join.
#' @param df2 Right data frame for the join.
#' @param by Character vector specifying the join keys.
#'
#' @return Data frame resulting from the clean left join operation.
#'
#' @noRd
#'
#' @importFrom dplyr select right_join
.clean_left_join <- function(df1, df2, by) {
  common <- intersect(names(df1), names(df2))
  to_drop <- setdiff(common, by)
  df_join <- df2 %>%
    dplyr::select(-dplyr::all_of(to_drop)) %>%
    dplyr::right_join(df1, by = by)

  
  return(df_join)
}

#' Determine data type of a column
#'
#' @description Intelligently classifies a data column as binary, categorical, or
#' continuous based on its values, distribution, and uniqueness patterns. This
#' classification is crucial for determining appropriate statistical modeling
#' approaches and variable treatment in MRP analysis.
#'
#' @param col Vector representing a data column to be classified. Can be numeric,
#'   character, factor, or logical. Missing values (NA) are handled appropriately.
#' @param num Logical. If TRUE, returns numeric codes for programmatic use
#'   (1=binary, 2=categorical, 3=continuous). If FALSE, returns descriptive
#'   character labels ("bin", "cat", "cont"). Default is FALSE.
#' @param threshold Numeric between 0 and 1. Threshold for determining if numeric
#'   data should be treated as continuous based on the proportion of unique values.
#'   Higher values favor categorical classification. Default is 0.1 (10%).
#'
#' @return Data type classification:
#' \itemize{
#'   \item **Binary ("bin" or 1)**: Exactly 2 unique values (e.g., male/female, yes/no)
#'   \item **Categorical ("cat" or 2)**: Multiple discrete values, low uniqueness
#'   \item **Continuous ("cont" or 3)**: High uniqueness or non-integer numeric values
#' }
#'
#' @noRd
#'
#' @importFrom dplyr n_distinct
.data_type <- function(col, num = FALSE, threshold = 0.1) {
  if(is.numeric(col)) {
    if(!all(as.integer(col) == col) ||
       mean(table(col) == 1) > threshold) {
      dtype <- if(num) 3 else "cont"
    } else if(dplyr::n_distinct(col) == 2) {
      dtype <- if(num) 1 else "bin"
    } else {
      dtype <- if(num) 2 else "cat"
    }
  } else {
    if(dplyr::n_distinct(col, na.rm = TRUE) == 2) {
      dtype <- if(num) 1 else "bin"
    } else {
      dtype <- if(num) 2 else "cat"
    }
  }

  return(dtype)
}

#' Create expected data types for variables
#'
#' @description Creates a list of expected data types for variables based on
#' the data format and structure. Different data formats have different
#' expected variable types.
#'
#' @param is_sample Logical. Whether the data represents sample data. Default is TRUE.
#' @param is_aggregated Logical. Whether the data is aggregated. Default is FALSE.
#'
#' @return Named list where names are variable names and values are expected
#'   data types ("bin", "cat", "cont", "ignore").
#'
#' @noRd
.create_expected_types <- function(
  metadata,
  is_sample = TRUE,
  is_aggregated = FALSE
) {
  
  types <- list(
    sex  = "bin",
    race = "cat",
    age  = "cat"
  )

  if (!is.null(metadata$special_case) &&
      metadata$special_case == "covid") types$zip <- "cat"
  if (!is.null(metadata$special_case) &&
      metadata$special_case == "poll") types$edu <- "cat"

  if (is_sample) {
    if (metadata$family == "binomial") {
      types$positive <- "ignore"
    } else if (metadata$family == "normal") {
      types$outcome <- "ignore"
    }
    if (is_aggregated) {
      if (metadata$is_timevar) types$time  <- "cat"
    }
  }

  if (is_aggregated) {
    types$total <- "ignore"
  }
  
  return(types)
}

#' Create expected levels for categorical variables
#'
#' @description Creates a list of expected levels for categorical variables
#' based on the data format. Different data formats (COVID vs poll) have
#' different expected demographic categories and age groupings.
#'
#' @param metadata List containing analysis metadata. If metadata$special_case
#'   is "poll", uses poll-specific levels, otherwise uses COVID/general levels.
#'
#' @return Named list where names are variable names and values are character
#'   vectors of expected levels:
#' \itemize{
#'   \item Poll data: Includes education levels, different age ranges
#'   \item COVID/general data: Standard demographic categories
#' }
#'
#' @noRd
.create_expected_levels <- function(metadata) {
  if (!is.null(metadata$special_case) &&
      metadata$special_case == "poll") {
    list(
      sex = c("male", "female"),
      race = c("white", "black", "other"),
      age = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"),
      edu = c("no hs", "hs", "some college", "4-year college", "post-grad")
    )
  } else {
    list(
      sex = c("male", "female"),
      race = c("white", "black", "other"),
      age = c("0-17", "18-34", "35-64", "65-74", "75+")
    )
  }
}

#' Validate data against expected structure
#'
#' @description Performs comprehensive data validation including checking for
#' missing columns, validating data types, checking for excessive missing values,
#' and validating date formats for time-varying data.
#'
#' @param df Data frame to be validated.
#' @param expected_types Named list of expected data types for each variable.
#'   Types should be "bin", "cat", "cont", or "ignore".
#' @param na_threshold Numeric. Maximum allowed proportion of missing values
#'   per column. Default is 0.5 (50%).
#'
#' @return No return value. Throws errors if validation fails:
#' \itemize{
#'   \item Missing required columns
#'   \item Incorrect data types
#'   \item Excessive missing values (>na_threshold)
#' }
#' Issues warnings for date format problems in time-varying data.
#'
#' @noRd
#'
.check_data <- function(df, expected_types, is_aggregated, na_threshold = 0.5) {
  expected_columns <- names(expected_types)

  # Check for missing columns
  missing <- setdiff(expected_columns, names(df))
  if(length(missing) > 0) {
    stop(paste0("The following columns are missing: ",
                  paste(missing, collapse = ", ")))

  }
  
  # Check data types
  types <- df %>% dplyr::select(dplyr::all_of(expected_columns)) %>% lapply(.data_type) %>% unlist()
  valid <- unlist(expected_types) == types
  valid[expected_types == "ignore"] <- TRUE
  
  if(any(!valid)) {
    stop(paste0("Columns corresponding to the following variables have inappropriate data types: ",
                paste(expected_columns[!valid], collapse = ", ")))
  }
  
  # Check for too many NAs
  na_percents <- df %>%
    lapply(function(c) sum(as.numeric(is.na(c))) / length(c)) %>%
    unlist()
  
  high_na_cols <- expected_columns[na_percents[expected_columns] > na_threshold]
  if(length(high_na_cols) > 0) {
    stop(paste0("Columns corresponding to the following variables have more than ",
                na_threshold * 100, "% rows with missing data: ",
                paste(high_na_cols, collapse = ", ")))
  }
  
  # Check date format
  if("time" %in% expected_columns) {
    if("date" %in% names(df)) {
      if (anyNA(as.Date(stats::na.omit(df$date), optional = TRUE))) {
        warning("Provided dates are not in expected format. Plots will use time indices instead.")
      }
    } else {
      warning("Dates are not provided. Plots will use time indices instead.")
    }
  }

  # Check if aggregated data misspecified as individual-level
  if ("total" %in% names(df) && !is_aggregated) {
    stop("Input data may be aggregated but is specified as individual-level.")
  }
}

#' Validate poststratification data against sample data
#'
#' @description Ensures that poststratification data has the same structure
#' and categorical levels as the sample data. Checks for missing columns and
#' validates that unique values match between datasets.
#'
#' @param df Data frame containing poststratification data.
#' @param df_ref Data frame containing reference sample data.
#' @param expected_levels Named list of expected levels for categorical variables.
#'
#' @return No return value. Throws errors if validation fails:
#' \itemize{
#'   \item Missing columns in either dataset
#'   \item Mismatched unique values between datasets
#' }
#'
#' @noRd
.check_pstrat <- function(df, df_ref, expected_levels) {
  if (is.null(df_ref)) {
    stop("Sample data is not provided.")
  }
  
  # ensure columns exist
  cols <- names(expected_levels)
  missing_df  <- setdiff(cols, names(df))
  missing_ref <- setdiff(cols, names(df_ref))
  if (length(missing_df))  stop("Missing in sample data:  ", paste(missing_df, collapse = ", "))
  if (length(missing_ref)) stop("Missing in postratification data: ", paste(missing_ref, collapse = ", "))
  
  
  # compare unique values
  cond <- vapply(cols, function(col) {
    setequal(unique(df[[col]]), unique(df_ref[[col]]))
  }, logical(1))

  if (any(!cond)) {
    stop("The following columns have different unique values in sample and postratification data: ",
         paste(cols[!cond], collapse = ", "))
  }
}


#' Main preprocessing function for uploaded data
#'
#' @description The primary preprocessing function that transforms raw uploaded data
#' into a standardized format suitable for MRP analysis. This comprehensive pipeline
#' handles data cleaning, validation, transformation, and preparation across different
#' data types and analysis scenarios (COVID, poll, time-varying, cross-sectional).
#'
#' @param data Data frame containing raw uploaded data. Can be in various formats
#'   with different column names, data types, and structures.
#' @param metadata List containing analysis metadata that controls preprocessing behavior:
#'   \itemize{
#'     \item family: "binomial" or "normal" for outcome type
#'     \item is_timevar: Logical indicating time-varying analysis
#'     \item special_case: "covid" or "poll" for specialized processing
#'   }
#' @param zip_county_state Data frame containing ZIP code to county/state crosswalk.
#' @param time_freq Character string specifying the time indexing frequency or time length for grouping dates (YYYY-MM-DD) in the data.
#' @param freq_threshold Numeric. Minimum frequency threshold for omitting rare rows.
#' @param is_sample Logical. Whether the data represents sample data (TRUE) or
#'   poststratification data (FALSE). Affects validation and processing steps.
#' @param is_aggregated Logical. Whether the data is already aggregated (TRUE) or
#'   individual-level records (FALSE). Determines if aggregation step is needed.
#'
#' @return Preprocessed data frame ready for MRP analysis with:
#' \itemize{
#'   \item **Standardized structure**: Cleaned column names and consistent formatting
#'   \item **Validated data types**: Appropriate types for demographic and outcome variables
#'   \item **Recoded variables**: Demographic variables matching expected factor levels
#'   \item **Geographic integration**: Geographic variables at multiple scales with FIPS codes
#'   \item **Time handling**: Time indices and complete timelines for time-varying data
#'   \item **Missing data treatment**: Imputed demographic variables using frequency-based sampling
#'   \item **Aggregation**: Cross-tabulated data for modeling (if individual-level input)
#' }
#'
#' @noRd
#'
#' @importFrom dplyr mutate group_by summarize ungroup across any_of first n full_join
#' @importFrom tidyr drop_na
#' @importFrom rlang syms .data
.preprocess <- function(
  data,
  metadata,
  zip_county_state,
  time_freq = NULL,
  freq_threshold = NULL,
  is_sample = TRUE,
  is_aggregated = TRUE
) {
  
  is_covid <- !is.null(metadata$special_case) &&
              metadata$special_case == "covid"
  levels <- .create_expected_levels(metadata)
  indiv_vars <- names(levels)
  if (metadata$is_timevar) {
    indiv_vars <- c(indiv_vars, "time")
  }

  # Clean data
  data <- .clean_data(data)

  # Find and rename columns
  data <- .rename_columns(data, is_covid && !is_aggregated)

  # Check for common dataframe issues
  types <- .create_expected_types(
    metadata = metadata,
    is_sample = is_sample,
    is_aggregated = is_aggregated
  )
  .check_data(data, types, is_aggregated)

  # Convert date column to character
  data <- .convert_date_to_character(data)

  # Aggregate if needed
  if (!is_aggregated) {
    # remove NAs
    check_cols <- setdiff(names(data), indiv_vars)
    data <- data %>% tidyr::drop_na(dplyr::all_of(check_cols))

    # convert date to time indices if necessary
    if (metadata$is_timevar & !is.null(time_freq)) {
      data <- .add_time_indices(data, time_freq = time_freq)

      if (dplyr::n_distinct(data$time) == 1) {
        stop("Time variable has only one unique value. Please use modules for cross-sectional data instead.")
      }
    }

    # remove duplicate rows
    data <- .remove_duplicates(data, is_covid)

    # remove values with low frequency
    data <- .omit_rare_rows(data, threshold = freq_threshold)

    # recode values to expected levels
    data <- .recode_values(data, levels, is_covid)

    # .impute missing demographic data based on frequency
    data <- data %>% dplyr::mutate(dplyr::across(dplyr::all_of(indiv_vars), .impute))

    if (metadata$family != "normal") {
      # aggregate test records based on combinations of factors
      smallest <- .get_smallest_geo(names(data))
      smallest_geo <- if(!is.null(smallest)) smallest$geo else NULL
      group_vars <- c(indiv_vars, smallest_geo)
      geo_covars <- if(!is.null(smallest_geo)) names(.get_geo_predictors(data, smallest_geo)) else NULL

      # cross-tabulate data
      data <- data %>%
        dplyr::group_by(!!!rlang::syms(group_vars)) %>%
        dplyr::summarize(
          dplyr::across(dplyr::any_of(geo_covars), dplyr::first),
          date = if("date" %in% names(data)) dplyr::first(.data$date),
          total = if("weight" %in% names(data)) sum(.data$weight) else dplyr::n(),
          positive = sum(.data$positive)

        ) %>%
        dplyr::ungroup()
    }
  }

  # append geographic areas at larger scales if missing
  data <- .append_geo(data, zip_county_state)

  return(data)
}

#' Create variable lists for model specification
#'
#' @description Creates organized lists of variables for model specification,
#' categorizing them as fixed effects, varying effects, or variables to omit.
#' Identifies variables with single levels or nested relationships that should
#' be excluded from modeling.
#'
#' @param input_data Data frame containing input data with individual-level variables.
#' @param covariates Data frame containing geographic covariates.
#'
#' @return Named list containing:
#'   \item{fixed}{List of fixed effect variables by category (Individual-level, Geographic)}
#'   \item{varying}{List of varying effect variables by category (Individual-level, Geographic)}
#'   \item{omit}{List of variables to omit: one_level (single-level vars), nested (nested pairs)}
#'
#' @details Variables are categorized as:
#' \itemize{
#'   \item Fixed effects: All variables with >1 level
#'   \item Varying effects: Categorical variables with >1 level
#'   \item Omitted: Variables with only 1 level or nested relationships
#' }
#'
#' @noRd
#'
.create_variable_list <- function(input_data, covariates) {
  # list of variables for model specification
  vars <- list(
    fixed = list(
      "Individual-level Predictor" = c(),
      "Geographic Predictor" = c()
    ),
    varying = list(
      "Individual-level Predictor" = c(),
      "Geographic Predictor" = c()
    ),
    omit = list(
      one_level = c(),
      nested = c()
    )
  )

  # Helper function to process variables and add them to appropriate lists
  add_variables <- function(group_name, var_names, data_source, vars) {
    for (v in var_names) {
      if (!is.null(data_source[[v]]) && dplyr::n_distinct(data_source[[v]]) > 1) {
        if (.data_type(data_source[[v]]) == "cat") {
          vars$varying[[group_name]] <- c(vars$varying[[group_name]], v)
        }
        vars$fixed[[group_name]] <- c(vars$fixed[[group_name]], v) 
      } else {
        # if the variable has only one level, add it to the omit list
        vars$omit$one_level <- c(vars$omit$one_level, v)
      }
    }
    return(vars)
  }

  # Process individual-level predictors
  indiv_vars <- setdiff(names(input_data), c(.const()$vars$geo, .const()$vars$ignore, names(covariates)))
  vars <- add_variables("Individual-level Predictor", indiv_vars, input_data, vars)

  # Process geographic predictors
  geo_vars <- names(covariates)
  vars <- add_variables("Geographic Predictor", geo_vars, covariates, vars)

  # Check for nested variables
  if (length(vars$varying[["Geographic Predictor"]]) >= 2) {
    vars$omit$nested <- utils::combn(vars$varying[["Geographic Predictor"]], 2, simplify = FALSE) %>%
      lapply(paste, collapse = ":") %>%
      unlist()
  }

  return(vars)
}

#' Combine tract-level data to larger geographic scales
#'
#' @description Aggregates tract-level demographic data to larger geographic
#' scales (zip, county, state, or national level) by summing population counts
#' across relevant geographic units. Used to create poststratification frames
#' at different geographic resolutions.
#'
#' @param tract_data Data frame containing tract-level demographic data with
#'   GEOID column (11-digit tract codes) and demographic cross-tabulation columns.
#' @param link_geo Character string specifying target geographic scale. Must be
#'   one of the values in .const()$vars$geo: "zip", "county", "state", or NULL for national.
#'
#' @return Data frame with demographic data aggregated to the specified
#'   geographic scale:
#' \itemize{
#'   \item geocode: Geographic identifier at the specified scale
#'   \item Demographic columns: Population counts summed across tracts
#' }
#'
#' @noRd
#'
#' @importFrom dplyr select rename inner_join group_by summarise across mutate summarize_all
#' @importFrom rlang .data
.combine_tracts <- function(
    tract_data,
    zip_tract,
    link_geo = NULL
) {

  checkmate::assert_choice(
    link_geo,
    choices = .const()$vars$geo,
    null.ok = TRUE
  )

  if (is.null(link_geo)) {

    pstrat_data <- tract_data %>%
      dplyr::mutate(geocode = "place_holder") %>%
      dplyr::select(-.data$GEOID) %>%
      dplyr::group_by(.data$geocode) %>%
      dplyr::summarize_all(sum)

  } else if (link_geo == "zip") {

    # join tract-level data with zip-tract conversion table then group by zip
    by_zip <- zip_tract %>%
      dplyr::select(.data$geoid, .data$zip) %>%
      dplyr::rename("GEOID" = "geoid") %>%
      dplyr::inner_join(
        tract_data,
        by = "GEOID"
      ) %>%
      dplyr::group_by(.data$zip)
    
    # compute zip-level population size by aggregating across overlapping tracts
    all_colnames <- names(tract_data)
    pstrat_colnames <- all_colnames[grepl("male|female", all_colnames)]
    pstrat_data <- by_zip %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(pstrat_colnames), ~ sum(.x, na.rm = TRUE))
      ) %>%
      dplyr::rename("geocode" = "zip")

  } else if (link_geo == "county") {

    pstrat_data <- tract_data %>%
      dplyr::mutate(geocode = substr(.data$GEOID, 1, 5)) %>%
      dplyr::select(-.data$GEOID) %>%
      dplyr::group_by(.data$geocode) %>%
      dplyr::summarize_all(sum)

  } else if (link_geo == "state") {

    pstrat_data <- tract_data %>%
      dplyr::mutate(geocode = substr(.data$GEOID, 1, 2)) %>%
      dplyr::select(-.data$GEOID) %>%
      dplyr::group_by(.data$geocode) %>%
      dplyr::summarize_all(sum)

  }

  return(pstrat_data)
}

#' Prepare MRP data using ACS-based poststratification
#'
#' @description Prepares data for MRP analysis using American Community Survey
#' (ACS) tract-level data for poststratification. Combines tract data to the
#' appropriate geographic scale, creates complete factor level combinations,
#' and appends geographic predictors. This function creates the poststratification
#' frame from Census data.
#'
#' @param input_data Data frame containing preprocessed sample data.
#' @param tract_data Data frame containing ACS tract-level demographic data
#'   with GEOID and demographic cross-tabulation columns.
#' @param metadata List containing analysis metadata including family type
#'   and time-varying flags.
#' @param link_geo Character string specifying geographic linking variable
#'   (e.g., "zip", "county", "state"). Must match a column in input_data.
#'
#' @return Named list containing:
#'   \item{input}{Filtered input data (common geographic areas only)}
#'   \item{new}{Complete poststratification data with population weights and covariates}
#'   \item{levels}{Complete list of factor levels for all variables}
#'   \item{vars}{Variable lists for model specification (fixed, varying, omit)}
#'
#' @noRd
#'
#' @importFrom dplyr filter select mutate arrange across left_join
#' @importFrom rlang sym .data
.prepare_mrp_acs <- function(
    input_data,
    tract_data,
    zip_tract,
    metadata,
    link_geo = NULL
) {

  # compute cell counts based on given geographic scale
  pstrat_data <- .combine_tracts(tract_data, zip_tract, link_geo)

  # filter based on common GEOIDs
  shared_geocodes <- c()
  if(!is.null(link_geo)) {
    shared_geocodes <- intersect(unique(input_data[[link_geo]]), pstrat_data$geocode)
    input_data <- input_data %>% dplyr::filter(!!dplyr::sym(link_geo) %in% shared_geocodes)
    pstrat_data <- pstrat_data %>% dplyr::filter(.data$geocode %in% shared_geocodes)
  }
  cell_counts <- pstrat_data %>% dplyr::select(-.data$geocode) %>% t() %>% c()

  # create lists of all factor levels
  n_time_indices <- 1
  levels <- .create_expected_levels(metadata)
  if(metadata$is_timevar) {
    levels$time <- unique(input_data$time) %>% sort()
    n_time_indices <- length(levels$time)
  }
  if(!is.null(link_geo)) {
    levels[[link_geo]] <- shared_geocodes
  }

  # IMPORTANT: for sorting data frame to match cell order of poststratification table
  sort_vars <- c("time", link_geo, "sex", "race", "age") %>%
    intersect(names(levels))

  new_data <- expand.grid(levels, stringsAsFactors = FALSE) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(sort_vars))) %>% # IMPORTANT: To match the cell order of poststratification data
    dplyr::mutate(total = rep(cell_counts, n_time_indices))

  # append geographic predictors
  covariates <- NULL
  if(!is.null(link_geo)) {
    # find geographic covariates
    covariates <- .get_geo_predictors(input_data, link_geo)
    if(ncol(covariates) > 1) {
      new_data <- dplyr::left_join(new_data, covariates, by = link_geo)
    }
  }

  # append levels for other geographic predictors
  # NOTE: this must be done after new_data is created
  # as these levels are not used in the poststratification table
  for(v in intersect(names(new_data), .const()$vars$geo)) {
    levels[[v]] <- unique(new_data[[v]]) %>% sort()
  }

  # add 'total' column to interface with plotting functions
  if (metadata$family == "normal") {
    input_data <- input_data %>% dplyr::mutate(total = 1)
  }

  # create variable lists for model specification
  vars <- .create_variable_list(input_data, covariates)

  return(list(
    input = input_data,
    new = new_data,
    levels = levels,
    vars = vars
  ))
}

#' Prepare MRP data using custom poststratification table
#'
#' @description Prepares data for MRP analysis using a custom poststratification
#' table provided by the user. Filters data to common geographic units, creates
#' factor levels, appends geographic predictors, and handles time-varying data
#' by duplicating rows for each time period.
#'
#' @param input_data Data frame containing preprocessed sample data.
#' @param new_data Data frame containing custom poststratification data with
#'   demographic columns and population counts (total column).
#' @param metadata List containing analysis metadata including family type
#'   and time-varying flags.
#' @param link_geo Character string specifying geographic linking variable
#'   (e.g., "zip", "county", "state"). Must exist in both datasets.
#'
#' @return Named list containing:
#'   \item{input}{Filtered input data (common geographic areas only)}
#'   \item{new}{Prepared poststratification data with covariates and time indices}
#'   \item{levels}{Complete list of factor levels for all variables}
#'   \item{vars}{Variable lists for model specification (fixed, varying, omit)}
#'
#' @noRd
#'
#' @importFrom dplyr filter mutate
#' @importFrom rlang sym
.prepare_mrp_custom <- function(
    input_data,
    new_data,
    metadata,
    link_geo = NULL
) {

  # filter based on common GEOIDs
  shared_geocodes <- c()
  if(!is.null(link_geo)) {
    shared_geocodes <- intersect(unique(input_data[[link_geo]]), unique(new_data[[link_geo]]))
    input_data <- input_data %>% dplyr::filter(!!dplyr::sym(link_geo) %in% shared_geocodes)
    new_data <- new_data %>% dplyr::filter(!!dplyr::sym(link_geo) %in% shared_geocodes)
  }

  # create lists of all factor levels
  n_time_indices <- 1
  levels <- .create_expected_levels(metadata)
  if(metadata$is_timevar) {
    levels$time <- unique(input_data$time) %>% sort()
    n_time_indices <- length(levels$time)
  }
  if(!is.null(link_geo)) {
    levels[[link_geo]] <- shared_geocodes
  }

  # append geographic predictors
  covariates <- NULL
  if(!is.null(link_geo)) {
    # find geographic covariates
    covariates <- .get_geo_predictors(input_data, link_geo)
    if(ncol(covariates) > 1) {
      new_data <- .clean_left_join(new_data, covariates, by = link_geo)
    }
  }

  # append levels for other geographic predictors
  for(v in intersect(names(new_data), .const()$vars$geo)) {
    levels[[v]] <- unique(new_data[[v]]) %>% sort()
  }

  # duplicate rows for each time index
  new_data <- purrr::map_dfr(
    seq_len(n_time_indices),
    ~ new_data %>% dplyr::mutate(time = .x)
  )

  # add 'total' column to interface with plotting functions
  if (metadata$family == "normal") {
    input_data <- input_data %>% dplyr::mutate(total = 1)
  }

  vars <- .create_variable_list(input_data, covariates)

  return(list(
    input = input_data,
    new = new_data,
    levels = levels,
    vars = vars
  ))
}
