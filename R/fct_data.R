#' Read data from various file formats
#'
#' @title Read data from multiple file formats
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
#' @details The function determines the file format using string pattern matching
#' on the file extension and applies format-specific reading parameters:
#' \itemize{
#'   \item CSV: show_col_types = FALSE to suppress column type messages
#'   \item Excel: guess_max = 5000 to improve column type detection
#'   \item SAS: Default haven parameters for optimal compatibility
#' }
#'
#' @section Supported File Formats:
#' \itemize{
#'   \item .csv: Comma-separated values
#'   \item .xlsx: Excel 2007+ format
#'   \item .xls: Legacy Excel format
#'   \item .sas7bdat: SAS dataset format
#' }
#'
#' @seealso
#' * readr::read_csv() for CSV file reading
#' * readxl::read_excel() for Excel file reading
#' * haven::read_sas() for SAS file reading
#'
#' @examples
#' \dontrun{
#' # Read CSV file
#' csv_data <- read_data("data/survey.csv")
#'
#' # Read Excel file
#' excel_data <- read_data("data/survey.xlsx")
#'
#' # Read SAS file
#' sas_data <- read_data("data/survey.sas7bdat")
#' }
#'
#' @noRd
#'
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom haven read_sas
#' @importFrom stringr str_ends
read_data <- function(file_path) {
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
#' @title Clean and standardize column names
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
#' @details The cleaning process follows these steps in order:
#' \enumerate{
#'   \item Convert to lowercase using tolower()
#'   \item Replace all digits [0-9] with underscores
#'   \item Replace all non-alphabetic characters with underscores using [^[:alpha:]]
#'   \item Collapse multiple consecutive underscores to single underscores
#'   \item Remove leading and trailing underscores
#' }
#'
#' @section Cleaning Rules:
#' \itemize{
#'   \item "Age Group 1" -> "age_group"
#'   \item "Income_2023" -> "income"
#'   \item "Race/Ethnicity" -> "race_ethnicity"
#'   \item "__Test__Variable__" -> "test_variable"
#' }
#'
#' @seealso
#' * clean_data() for comprehensive data frame cleaning
#' * format_geocode() for geographic identifier formatting
#'
#' @examples
#' \dontrun{
#' # Clean messy column names
#' messy_names <- c("Age Group 1", "Income_2023", "Race/Ethnicity", "__Test__")
#' clean_names(messy_names)
#' # Returns: c("age_group", "income", "race_ethnicity", "test")
#' }
#'
#' @noRd
clean_names <- function(names) {
  names %>% 
    tolower() %>% 
    gsub("[0-9]", "_", .) %>%
    gsub("[^[:alpha:]]", "_", .) %>%
    gsub("_{2,}", "_", .) %>%
    gsub("^_|_$", "", .)
}


#' Clean character columns in a data frame
#'
#' @title Clean character columns in data frame
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
#' @details The function uses dplyr's across() with where(is.character) to
#' selectively apply transformations only to character columns. The cleaning
#' process combines:
#' \itemize{
#'   \item tolower() for case standardization
#'   \item stringr::str_trim() for whitespace removal
#' }
#'
#' This is particularly important for categorical variables that will be used
#' in factor creation and model fitting, where inconsistent capitalization
#' or whitespace can cause matching issues.
#'
#' @section Character Cleaning Examples:
#' \itemize{
#'   \item "  Male  " -> "male"
#'   \item "FEMALE" -> "female"
#'   \item "White " -> "white"
#'   \item " Other Race" -> "other race"
#' }
#'
#' @seealso
#' * clean_data() for comprehensive data cleaning including character cleaning
#' * clean_names() for column name standardization
#' * stringr::str_trim() for whitespace removal
#'
#' @examples
#' \dontrun{
#' # Clean character columns in survey data
#' survey_data <- data.frame(
#'   age = c(25, 30, 35),
#'   sex = c("  Male  ", "FEMALE", "male"),
#'   race = c("White ", "BLACK", " Other")
#' )
#' cleaned_data <- clean_chr(survey_data)
#' # Character columns now: "male", "female", "male" and "white", "black", "other"
#' }
#'
#' @noRd
#'
#' @importFrom dplyr mutate across where
#' @importFrom stringr str_trim
clean_chr <- function(df) {
  # Convert character columns to lowercase and trim whitespace
  df %>% mutate(
    across(where(is.character), ~ stringr::str_trim(tolower(.x)))
  )
}

#' Validate and clean geocodes
#'
#' @title Validate and clean geocodes
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
#' @details The validation process:
#' \enumerate{
#'   \item Converts all input to character format using as.character()
#'   \item Applies regex pattern "^[0-9]{5}$" to check for exactly 5 digits
#'   \item Replaces any non-matching values with NA_character_
#'   \item Preserves existing NA values in their character form
#' }
#'
#' @section Validation Rules:
#' Valid geocodes must:
#' \itemize{
#'   \item Contain exactly 5 digits (0-9)
#'   \item Have no leading/trailing characters
#'   \item Have no embedded spaces or special characters
#' }
#'
#' @section Examples of Valid/Invalid Geocodes:
#' \itemize{
#'   \item Valid: "12345", "00123", "90210"
#'   \item Invalid: "1234" (too short), "123456" (too long), "1234a", " 12345 "
#' }
#'
#' @seealso
#' * format_geocode() for comprehensive geographic identifier formatting
#' * to_fips() for converting geographic names to FIPS codes
#' * clean_data() for overall data cleaning including geocode validation
#'
#' @examples
#' \dontrun{
#' # Validate mixed geocode formats
#' geocodes <- c("12345", "1234", "90210", "abcde", NA, "123456")
#' validated <- find_bad_geocode(geocodes)
#' # Returns: c("12345", NA, "90210", NA, NA, NA)
#' }
#'
#' @noRd
find_bad_geocode <- function(geocodes) {
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
#' @details The formatting process handles different input types:
#'
#' **For zip and county columns:**
#' \itemize{
#'   \item Numeric inputs: Formatted using sprintf("%05d", value)
#'   \item Character inputs: Validated using find_bad_geocode()
#'   \item Invalid entries: Replaced with NA
#' }
#'
#' **For state columns:**
#' \itemize{
#'   \item Numeric inputs: Formatted using sprintf("%02d", value)
#'   \item Character inputs: Left unchanged (assumed to be state names/abbreviations)
#' }
#'
#' @section Geographic Identifier Standards:
#' \itemize{
#'   \item ZIP codes: 5-digit postal codes (e.g., "90210", "01234")
#'   \item County FIPS: 5-digit codes (2-digit state + 3-digit county)
#'   \item State FIPS: 2-digit codes (e.g., "01" for Alabama, "06" for California)
#' }
#'
#' @seealso
#' * find_bad_geocode() for geocode validation
#' * to_fips() for converting geographic names to FIPS codes
#' * clean_data() for comprehensive data cleaning including geocode formatting
#'
#' @examples
#' \dontrun{
#' # Format mixed geographic identifiers
#' geo_data <- data.frame(
#'   zip = c(90210, 1234, "12345"),
#'   county = c("6037", 6037, "invalid"),
#'   state = c(6, "CA", 1)
#' )
#' formatted <- format_geocode(geo_data)
#' # zip: c("90210", NA, "12345")
#' # county: c("06037", "06037", NA)
#' # state: c("06", "CA", "01")
#' }
#'
#' @noRd
format_geocode <- function(df) {
  if ("zip" %in% names(df)) {
    if (is.numeric(df$zip)) {
      df$zip <- sprintf("%05d", df$zip)
    } else {
      df$zip <- find_bad_geocode(df$zip)
    }
  }

  if ("county" %in% names(df)) {
    if (is.numeric(df$county)) {
      df$county <- sprintf("%05d", df$county)
    } else {
      df$county <- find_bad_geocode(df$county)
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
#' @details The cleaning pipeline executes the following steps in order:
#' \enumerate{
#'   \item **Column name cleaning**: Uses clean_names() to standardize column names
#'   \item **Duplicate removal**: Removes duplicate column names, keeping first occurrence
#'   \item **Character standardization**: Uses clean_chr() for lowercase conversion and trimming
#'   \item **NA conversion**: Converts specified strings to actual NA values across all columns
#'   \item **Geographic formatting**: Uses format_geocode() for proper FIPS code formatting
#' }
#'
#' @section Default NA Strings:
#' The following strings are converted to NA by default (case-insensitive):
#' \itemize{
#'   \item Empty string: ""
#'   \item Explicit NA representations: "na", "n/a"
#'   \item Null representations: "none", "null"
#'   \item Unknown values: "unknown"
#' }
#'
#' @section Geographic Identifier Processing:
#' If the data contains geographic columns (zip, county, state), they are
#' automatically formatted according to standard conventions:
#' \itemize{
#'   \item ZIP codes: 5-digit format with leading zeros
#'   \item County FIPS: 5-digit format with leading zeros
#'   \item State FIPS: 2-digit format with leading zeros
#' }
#'
#' @seealso
#' * clean_names() for column name standardization
#' * clean_chr() for character column cleaning
#' * format_geocode() for geographic identifier formatting
#' * preprocess() for full preprocessing pipeline including data cleaning
#'
#' @examples
#' \dontrun{
#' # Clean messy survey data
#' messy_data <- data.frame(
#'   `Age Group` = c(25, 30, "unknown"),
#'   `  Sex  ` = c("  MALE  ", "female", ""),
#'   ZIP_Code = c(90210, 1234, "n/a"),
#'   `ZIP_Code` = c("duplicate", "column", "removed")  # duplicate column
#' )
#'
#' cleaned <- clean_data(messy_data)
#' # Result has standardized names: age_group, sex, zip_code
#' # Character values cleaned: "male", "female", NA
#' # Geographic codes formatted: "90210", NA, NA
#' # Duplicate column removed
#' }
#'
#' @noRd
#' @importFrom dplyr mutate across everything if_else
clean_data <- function(
    df,
    na_strings = c("", "na", "n/a", "none", "null", "unknown")
) {

  # Clean column names
  names(df) <- clean_names(names(df))

  # Remove duplicate column names (keeping first occurrence)
  if (any(duplicated(names(df)))) {
    df <- df[, !duplicated(names(df))]
  }

  # Convert character columns to lowercase and trim whitespace
  df <- clean_chr(df)

  # Convert common NA strings to actual NA
  df <- df %>% 
    mutate(across(everything(), 
                 ~if_else(.x %in% na_strings, NA, .x)))

  # Format geographic identifiers
  df <- format_geocode(df)
  
  return(df)
}

#' Preprocess example data for demonstration purposes
#'
#' @title Preprocess example data
#' @description Applies basic data cleaning and state FIPS code conversion to
#' example datasets. This is a simplified preprocessing function used specifically
#' for example data in the shinymrp package demonstrations and tutorials.
#'
#' @param df Data frame containing example data to be preprocessed. Should contain
#'   standard demographic and geographic columns.
#'
#' @return Data frame with cleaned data and state codes converted to FIPS format:
#' \itemize{
#'   \item Column names cleaned and standardized
#'   \item Character columns converted to lowercase and trimmed
#'   \item Common NA strings converted to actual NA values
#'   \item Geographic identifiers properly formatted
#'   \item State names/codes converted to 2-digit FIPS codes
#' }
#'
#' @details This function is a wrapper around clean_data() that additionally
#' converts state identifiers to FIPS codes using to_fips(). It's designed for
#' use with the package's example datasets and may not handle all edge cases
#' present in real-world data.
#'
#' @seealso
#' * clean_data() for comprehensive data cleaning
#' * to_fips() for geographic code conversion
#' * preprocess() for full preprocessing pipeline
#'
#' @noRd
preprocess_example <- function(df) {
  df <- df %>% clean_data()
  
  if ("state" %in% names(df)) {
    df$state <- to_fips(df$state, "state")
  }

  return(df)
}

#' Rename columns based on expected variable names
#'
#' @title Rename columns to standard variable names
#' @description Renames columns in the data frame to match expected variable
#' names defined in the global constants. Uses pattern matching to identify
#' columns that correspond to standard demographic, geographic, and outcome
#' variables used in MRP analysis.
#'
#' @param df Data frame with columns to be renamed. Column names will be matched
#'   against expected patterns to identify standard variables.
#' @param covid_indiv Logical. If TRUE, uses COVID-specific individual data
#'   column renaming via rename_columns_covid(). Default is FALSE for general
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
#' @details The renaming process:
#' \enumerate{
#'   \item Checks if COVID-specific renaming is requested
#'   \item Combines target variable names from GLOBAL$vars (indiv, geo, ignore)
#'   \item Uses case-insensitive grep() to find matching column names
#'   \item Creates rename specification using first match for each target
#'   \item Applies renaming using dplyr::rename() with !!!rename_spec syntax
#' }
#'
#' @section Variable Categories:
#' The function targets variables defined in GLOBAL$vars:
#' \itemize{
#'   \item **Individual variables**: Demographic characteristics (sex, race, age, edu, time)
#'   \item **Geographic variables**: Spatial identifiers (zip, county, state)
#'   \item **Ignore variables**: Outcome and auxiliary variables (date, total, positive, outcome)
#' }
#'
#' @section Pattern Matching:
#' Uses case-insensitive pattern matching to identify columns:
#' \itemize{
#'   \item "sex" matches: "Sex", "GENDER", "sex_var"
#'   \item "age" matches: "Age", "AGE_GROUP", "age_category"
#'   \item "race" matches: "Race", "ETHNICITY", "race_eth"
#' }
#'
#' @seealso
#' * rename_columns_covid() for COVID-specific column renaming
#' * GLOBAL$vars for variable name specifications
#' * clean_data() for comprehensive data cleaning including column renaming
#'
#' @examples
#' \dontrun{
#' # Rename survey data columns
#' survey_data <- data.frame(
#'   Gender = c("Male", "Female"),
#'   Age_Group = c("25-34", "35-44"),
#'   Race_Ethnicity = c("White", "Black"),
#'   ZIP_Code = c("90210", "10001")
#' )
#'
#' renamed <- rename_columns(survey_data)
#' # Columns now: sex, age, race, zip
#' }
#'
#' @noRd
rename_columns <- function(
    df,
    covid_indiv = FALSE
) {
  if (covid_indiv) {
    return(rename_columns_covid(df))
  }

  target_names <- c(
    GLOBAL$vars$indiv,
    GLOBAL$vars$geo,
    GLOBAL$vars$ignore
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

remove_duplicates <- function(data, is_covid = FALSE) {
  if (is_covid) {
    data <- remove_duplicates_covid(data)
  }

  return(data)
}

#' Impute missing values using frequency-based sampling
#'
#' @title Impute missing values using frequency-based sampling
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
#' @details The imputation process:
#' \enumerate{
#'   \item Identifies missing values using is.na()
#'   \item If no missing values, returns original vector
#'   \item Creates frequency table of observed (non-missing) values
#'   \item Samples replacement values with probabilities proportional to frequencies
#'   \item Handles both numeric and character vectors appropriately
#'   \item Uses replacement sampling to handle cases with many missing values
#' }
#'
#' @section Imputation Method:
#' This frequency-based imputation method:
#' \itemize{
#'   \item **Preserves distribution**: Maintains the original frequency distribution
#'   \item **Handles all types**: Works with numeric, character, and factor variables
#'   \item **Probabilistic**: Uses random sampling rather than deterministic replacement
#'   \item **Conservative**: Only imputes when sufficient observed data exists
#' }
#'
#' @section Use Cases:
#' Particularly effective for:
#' \itemize{
#'   \item Categorical demographic variables (sex, race, education)
#'   \item Discrete numeric variables with limited unique values
#'   \item Variables where mode imputation would be too simplistic
#'   \item Preserving uncertainty in imputed values through randomization
#' }
#'
#' @seealso
#' * preprocess() for comprehensive preprocessing including imputation
#' * recode_values() for value recoding before imputation
#' * table() for frequency table creation
#'
#' @examples
#' \dontrun{
#' # Impute missing categorical values
#' sex_var <- c("male", "female", "male", NA, "female", NA, "male")
#' imputed_sex <- impute(sex_var)
#' # Missing values replaced with "male" or "female" based on observed frequencies
#'
#' # Impute missing numeric values
#' age_var <- c(25, 30, 25, NA, 35, NA, 30)
#' imputed_age <- impute(age_var)
#' # Missing values replaced with 25, 30, or 35 based on frequencies
#' }
#'
#' @noRd
impute <- function(v) {
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

#' Convert dates to week indices and create timeline
#'
#' @title Convert dates to cumulative week indices
#' @description Converts date strings to cumulative ISO week indices and creates
#' a complete timeline of dates. This function is essential for time-varying MRP
#' models that require consistent temporal indexing across potentially multi-year
#' datasets with irregular date spacing.
#'
#' @param strings Character vector of date strings to be converted. Should be in
#'   standard date format (YYYY-MM-DD) or any format parseable by as.Date().
#'
#' @return List containing two components:
#' \itemize{
#'   \item **indices**: Numeric vector of cumulative week indices starting from 1,
#'     with the same length as input strings
#'   \item **timeline**: Date vector containing the first day (Monday) of each week
#'     in the complete timeline from earliest to latest week
#' }
#'
#' @details The conversion process handles complex multi-year scenarios:
#' \enumerate{
#'   \item Converts dates to ISO week format using ISOweek::ISOweek()
#'   \item Extracts year and week components from ISO week strings
#'   \item For single-year data: Uses week numbers directly
#'   \item For multi-year data: Calculates cumulative weeks across years
#'   \item Accounts for varying weeks per year (52 or 53 weeks)
#'   \item Creates complete timeline filling gaps between min and max weeks
#'   \item Returns first day (Monday) of each week using ISOweek2date()
#' }
#'
#' @section ISO Week Handling:
#' The function uses ISO 8601 week numbering where:
#' \itemize{
#'   \item Week 1 contains the first Thursday of the year
#'   \item Weeks run Monday to Sunday
#'   \item Years can have 52 or 53 weeks
#'   \item Week numbers are cumulative across multiple years
#' }
#'
#' @section Multi-Year Processing:
#' For datasets spanning multiple years:
#' \itemize{
#'   \item Calculates weeks per year using December 28th (always in last week)
#'   \item Applies cumulative offsets to create continuous week indices
#'   \item Fills complete timeline including weeks with no data
#'   \item Ensures consistent indexing for time-varying models
#' }
#'
#' @seealso
#' * add_week_indices() for adding week indices to data frames
#' * ISOweek::ISOweek() for ISO week conversion
#' * ISOweek::ISOweek2date() for converting back to dates
#' * preprocess() for full preprocessing including time handling
#'
#' @examples
#' \dontrun{
#' # Single year example
#' dates_2023 <- c("2023-01-02", "2023-01-09", "2023-01-16")
#' result <- get_week_indices(dates_2023)
#' # result$indices: c(1, 2, 3)
#' # result$timeline: Mondays of weeks 1, 2, 3 in 2023
#'
#' # Multi-year example
#' dates_multi <- c("2022-12-26", "2023-01-02", "2023-01-09")
#' result <- get_week_indices(dates_multi)
#' # result$indices: cumulative across years
#' # result$timeline: complete timeline from 2022 week 52 to 2023 week 2
#' }
#'
#' @noRd
#'
#' @importFrom ISOweek ISOweek ISOweek2date
get_week_indices <- function(strings) {
  # extract week numbers, months and years from dates
  years_weeks <- ISOweek::ISOweek(strings)
  years <- years_weeks %>% sapply(substr, start = 1, stop = 4) %>% as.numeric()
  weeks <- years_weeks %>% sapply(substr, start = 7, stop = 8) %>% as.numeric()
  months <- strings %>% as.Date() %>% format("%m") %>% as.numeric()

  # find year range
  year_min <- min(years)
  year_max <- max(years)
  all_years <- year_min:year_max


  if(year_min == year_max) {
    weeks_accum <- weeks
    timeline_week <- min(weeks_accum):max(weeks_accum)
    timeline_year <- rep(year_min, length(timeline_week))
  } else {
    # add offsets to week numbers in later years
    weeks_per_year <- paste0(all_years, "-12-28") %>%
      ISOweek::ISOweek() %>%
      sapply(substr, start = 7, stop = 8) %>%
      as.numeric()

    weeks_offset <- c(0, cumsum(weeks_per_year[1:(length(weeks_per_year)-1)]))
    offsets <- years %>% sapply(function(y) weeks_offset[which(all_years == y)])

    weeks_accum <- weeks + offsets
    weeks_accum <- weeks_accum - min(weeks_accum) + 1

    # find all weeks between the earliest and most recent dates
    start <- which.min(weeks_accum)
    end <- which.max(weeks_accum)
    year_start <- which(all_years == years[start])
    year_end <- which(all_years == years[end])

    # first year
    timeline_week <- weeks[start]:weeks_per_year[year_start]
    timeline_year <- rep(all_years[year_start], length(timeline_week))

    # in-between year
    for(year_ind in (year_start+1):(year_end-1)) {
      timeline_week <- c(timeline_week, 1:weeks_per_year[year_ind])
      timeline_year <- c(timeline_year, rep(all_years[year_ind], weeks_per_year[year_ind]))
    }

    # last year
    timeline_week <- c(timeline_week, 1:weeks[end])
    timeline_year <- c(timeline_year, rep(all_years[year_end], weeks[end]))
  }

  # get the start of each week
  timeline_date <- mapply(function(y, w) sprintf("%d-W%02d-1", y, w),
                          timeline_year,
                          timeline_week) %>%
    ISOweek::ISOweek2date()


  return(list(
    indices = weeks_accum,
    timeline = timeline_date
  ))
}

#' Add week indices to time-varying data
#'
#' @title Add week indices to data frame
#' @description Converts date columns to cumulative week indices for time-varying
#' analysis. Creates a complete timeline of weeks and adds corresponding time
#' indices to the data frame. Essential for time-varying MRP models that require
#' consistent temporal indexing.
#'
#' @param df Data frame containing time-related columns. Must contain either a
#'   'date' column with date strings or existing time indices matching
#'   GLOBAL$vars$time specification.
#'
#' @return Data frame with added time indices and complete timeline:
#' \itemize{
#'   \item time: Numeric column with cumulative week indices starting from 1
#'   \item date: Character column with first date of each week (if dates provided)
#'   \item Original columns preserved
#'   \item Complete timeline created via full_join to ensure no missing weeks
#' }
#'
#' @details The function:
#' \enumerate{
#'   \item Checks for time-related columns defined in GLOBAL$vars$time
#'   \item If 'date' column exists, converts to week indices using get_week_indices()
#'   \item Creates complete timeline from week 1 to maximum week index
#'   \item Performs full_join to ensure all weeks are represented
#'   \item Throws error if no time-related columns are found
#' }
#'
#' @section Time Variable Requirements:
#' The function expects time variables as defined in GLOBAL$vars$time:
#' \itemize{
#'   \item date: Date strings in standard format (YYYY-MM-DD)
#'   \item time: Existing numeric time indices
#' }
#'
#' @seealso
#' * get_week_indices() for date to week index conversion
#' * GLOBAL$vars$time for time variable specifications
#' * preprocess() for full preprocessing pipeline including time handling
#'
#' @examples
#' \dontrun{
#' # Data with dates
#' df_with_dates <- data.frame(
#'   date = c("2023-01-01", "2023-01-08", "2023-01-15"),
#'   outcome = c(0.1, 0.2, 0.3)
#' )
#' result <- add_week_indices(df_with_dates)
#' # Result includes time column with indices 1, 2, 3
#' }
#'
#' @noRd
#'
#' @importFrom dplyr full_join
add_week_indices <- function(df) {
  common <- intersect(names(df), GLOBAL$vars$time)

  if (length(common) == 1 && "date" %in% common) {
    # convert date to week indices
    week <- get_week_indices(df$date)
    df$time <- week$indices

    # add the column containing first dates of the weeks
    df <- df %>% select(-"date")
    df <- df %>%
      full_join(
        data.frame(
          time = 1:max(df$time),
          date = as.character(week$timeline)
        ),
        by = "time"
      )
  } else if (length(common) == 0) {
    stop("No dates or week indices found.")
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
#'
get_dates <- function(df) {
  df$date %>%
    stats::na.omit() %>%
    unique() %>%
    as.Date() %>%
    sort() %>%
    format(GLOBAL$ui$format$date) %>%
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
recode_values <- function(df, expected_levels, is_covid=FALSE) {
  if (is_covid) {
    return(recode_covid(df, expected_levels))
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

  df <- df %>% mutate(
    sex  = if("sex" %in% colnames) if_else(.data$sex %in% expected_levels$sex, .data$sex, NA),
    race = if("race" %in% colnames) if_else(.data$race %in% c(expected_levels$race, NA), .data$race, "other"),
    age  = if("age" %in% colnames) cut(df$age, breaks, ranges) %>% as.character(),
    edu  = if("edu" %in% colnames) if_else(.data$edu %in% expected_levels$edu, .data$edu, NA),
    positive = if("positive" %in% colnames) case_match(
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
filter_geojson <- function(geojson, geoids, omit = FALSE) {
  if(is.null(geojson) | is.null(geoids)) {
    return(NULL)
  }

  geojson$features <- purrr::keep(
    geojson$features,
    function(f) !is.null(nullify(f$properties$fips)) && f$properties$fips %in% geoids
  )

  return(geojson)
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
#' @details
#' **Two-Stage Filtering Process:**
#' \enumerate{
#'   \item **State-level filtering**: Calculates total observations per state
#'     by summing ZIP-level counts within each state. Excludes entire states
#'     that fall below the proportional threshold.
#'   \item **ZIP-level filtering**: Among remaining states, excludes individual
#'     ZIP codes that fall below the absolute count threshold.
#' }
#'
#' **Geographic Hierarchy:**
#' The function uses FIPS codes to establish the state-ZIP hierarchy:
#' \itemize{
#'   \item State FIPS codes are extracted from the first 2 digits of county FIPS
#'   \item ZIP codes are mapped to states through county assignments
#'   \item Handles cases where ZIP codes cross county boundaries (uses first match)
#' }
#'
#' **Sample Size Considerations:**
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
    zip_county_state,
    zip_threshold = 0,
    state_threshold = 0
) {
  
  zip_count <- df %>%
    group_by(.data$zip) %>%
    summarize(count = n())

  # create a table containing state, zip and zip count
  state_zip <- zip_county_state %>%
    mutate(state = substr(.data$fips, 1, 2)) %>%
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

#' Convert geographic identifiers to FIPS codes
#'
#' @title Convert geographic identifiers to FIPS codes
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
#'   "county" or "state" as defined in GLOBAL$vars$geo2. Determines the lookup
#'   table and formatting used.
#'
#' @return Character vector of FIPS codes with proper formatting:
#' \itemize{
#'   \item State FIPS: 2-digit codes with leading zeros (e.g., "06" for California)
#'   \item County FIPS: 5-digit codes with leading zeros (e.g., "06037" for Los Angeles)
#'   \item NA values for identifiers that cannot be matched
#' }
#'
#' @details The conversion process:
#' \enumerate{
#'   \item Validates link_geo parameter against GLOBAL$vars$geo2
#'   \item Selects appropriate lookup table (fips_$county or fips_$state)
#'   \item For numeric input: Applies sprintf formatting with leading zeros
#'   \item For character input: Finds best matching column in lookup table
#'   \item Uses column with highest match count for identifier resolution
#'   \item Returns formatted FIPS codes or NA for unmatched identifiers
#' }
#'
#' @section FIPS Code Standards:
#' \itemize{
#'   \item **State FIPS**: 2-digit codes (01-56) assigned by Census Bureau
#'   \item **County FIPS**: 5-digit codes (2-digit state + 3-digit county)
#'   \item **Leading zeros**: Essential for proper geographic linking
#'   \item **Consistency**: Standardized format across all geographic scales
#' }
#'
#' @section Lookup Table Matching:
#' For text identifiers, the function:
#' \itemize{
#'   \item Tests all columns in the lookup table for matches
#'   \item Selects the column with the highest number of matches
#'   \item Handles variations in naming conventions and abbreviations
#'   \item Returns NA for identifiers not found in any column
#' }
#'
#' @seealso
#' * fips_$county and fips_$state for lookup tables
#' * format_geocode() for comprehensive geographic identifier formatting
#' * append_geo() for adding geographic variables at multiple scales
#' * GLOBAL$vars$geo2 for valid geographic level specifications
#'
#' @examples
#' \dontrun{
#' # Convert state names to FIPS codes
#' states <- c("California", "New York", "Texas")
#' state_fips <- to_fips(states, "state")
#' # Returns: c("06", "36", "48")
#'
#' # Convert numeric county codes to formatted FIPS
#' counties <- c(6037, 36061, 48201)
#' county_fips <- to_fips(counties, "county")
#' # Returns: c("06037", "36061", "48201")
#'
#' # Handle mixed formats
#' mixed_states <- c("CA", "New York", 48)
#' mixed_fips <- to_fips(mixed_states, "state")
#' # Returns: c("06", "36", "48")
#' }
#'
#' @noRd
to_fips <- function(vec, link_geo) {
  checkmate::assert_choice(
    link_geo,
    choices = GLOBAL$vars$geo2,
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
get_geo_predictors <- function(df, geo_col) {
  bool <- df %>%
    group_by(!!sym(geo_col)) %>%
    summarize_all(n_distinct) %>%
    lapply(function(c) all(c == 1)) %>%
    unlist()

  geo_pred_cols <- names(bool)[bool]

  geo_preds <- df %>%
    select(all_of(c(geo_col, geo_pred_cols))) %>%
    distinct(!!sym(geo_col), .keep_all = TRUE)

  return(geo_preds)
}

#' Find the smallest geographic scale in data
#'
#' @description Identifies the smallest (most granular) geographic scale
#' present in the data based on the hierarchy defined in GLOBAL$vars$geo.
#'
#' @param col_names Character vector of column names in the data.
#' @param geo_col Character string specifying geographic column (currently unused).
#'
#' @return List containing:
#'   \item{geo}{Character string of the smallest geographic scale}
#'   \item{idx}{Numeric index of the geographic scale in the hierarchy}
#'   Returns NULL if no geographic variables are found.
#'
#' @noRd
#'
get_smallest_geo <- function(col_names) {
  geo_all <- GLOBAL$vars$geo

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
#' @title Get possible geographic scales
#' @description Determines all possible geographic scales that can be used with
#' the data based on the smallest geographic scale present. Returns geographic
#' variables from the smallest scale up to the largest (national) scale according
#' to the geographic hierarchy defined in GLOBAL$vars$geo.
#'
#' @param col_names Character vector of column names in the data frame to check
#'   for geographic variables.
#'
#' @return Character vector of geographic scale names from smallest to largest
#'   that can be used with the data, or NULL if no geographic variables are found.
#'   Geographic scales follow the hierarchy: zip -> county -> state.
#'
#' @details The function:
#' \enumerate{
#'   \item Uses get_smallest_geo() to find the most granular geographic scale
#'   \item Returns all scales from that level up to the largest scale
#'   \item Follows the geographic hierarchy: zip (most granular) to state (least granular)
#'   \item Returns NULL if no geographic columns are detected
#' }
#'
#' @section Geographic Hierarchy:
#' The geographic hierarchy used is defined in GLOBAL$vars$geo:
#' \itemize{
#'   \item zip: ZIP code level (most granular)
#'   \item county: County level (FIPS codes)
#'   \item state: State level (FIPS codes)
#' }
#'
#' @seealso
#' * get_smallest_geo() for finding the smallest geographic scale
#' * append_geo() for adding geographic variables at larger scales
#' * GLOBAL$vars$geo for geographic variable hierarchy
#'
#' @examples
#' \dontrun{
#' # Data with county information can use county and state scales
#' col_names <- c("sex", "age", "county", "outcome")
#' possible_geos <- get_possible_geos(col_names)
#' # Returns: c("county", "state")
#'
#' # Data with ZIP codes can use all scales
#' col_names <- c("sex", "age", "zip", "outcome")
#' possible_geos <- get_possible_geos(col_names)
#' # Returns: c("zip", "county", "state")
#' }
#'
#' @noRd
get_possible_geos <- function(col_names) {
  smallest <- get_smallest_geo(col_names)
  if (is.null(smallest)) {
    return(NULL)
  }

  # Return all geographic variables from the smallest to the largest scale
  return(GLOBAL$vars$geo[smallest$idx:length(GLOBAL$vars$geo)])
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
append_geo <- function(input_data, zip_county_state) {
  # get the smallest geographic scale in the data
  smallest <- get_smallest_geo(names(input_data))

  # Get geographic variables at current and larger scales
  geo_vars <- get_possible_geos(names(input_data))

  if (is.null(geo_vars)) {
    return(input_data)
  }

  # Prepare geographic crosswalk
  zip_county_state <- zip_county_state %>%
    select(.data$zip, .data$fips) %>%
    rename(county = .data$fips) %>%
    mutate(state = substr(.data$county, 1, 2)) %>%
    select(all_of(geo_vars)) %>%
    distinct()
  
  # Convert names to FIPS for smallest geographic scale
  if (smallest$geo != "zip") { 
    input_data[[smallest$geo]] <- to_fips(
      input_data[[smallest$geo]], 
      smallest$geo
    )
  }

  # Join geographic variables
  input_data <- clean_left_join(input_data, zip_county_state, by = smallest$geo)

  # Convert names to GEOIDs for larger geographic scales
  for (geo in setdiff(geo_vars, smallest$geo)) {
    if (geo != "zip") {
      input_data[[geo]] <- to_fips(
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
as_factor <- function(df, levels) {
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
find_nested <- function(df, cols, sep = "---") {
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
clean_left_join <- function(df1, df2, by) {
  common <- intersect(names(df1), names(df2))
  to_drop <- setdiff(common, by)
  df_join <- df2 %>%
    select(-all_of(to_drop)) %>%
    right_join(df1, by = by)

  
  return(df_join)
}

#' Determine data type of a column
#'
#' @title Classify column data type for statistical modeling
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
#' @details The classification algorithm:
#'
#' **For numeric columns:**
#' \enumerate{
#'   \item Check if all values are integers AND uniqueness is below threshold
#'   \item If 2 unique values: classify as binary
#'   \item If >2 unique values: classify as categorical
#'   \item Otherwise: classify as continuous (non-integers or high uniqueness)
#' }
#'
#' **For non-numeric columns:**
#' \enumerate{
#'   \item Count distinct values (excluding NA)
#'   \item If exactly 2 unique values: classify as binary
#'   \item Otherwise: classify as categorical
#' }
#'
#' @section Classification Logic:
#' \itemize{
#'   \item **Uniqueness test**: mean(table(col) == 1) > threshold indicates high uniqueness
#'   \item **Integer test**: all(as.integer(col) == col) checks for whole numbers
#'   \item **Distinct count**: Uses dplyr::n_distinct() with NA handling
#' }
#'
#' @section Use Cases:
#' \itemize{
#'   \item **Model specification**: Determines fixed vs. varying effects treatment
#'   \item **Data validation**: Ensures variables match expected types
#'   \item **Variable selection**: Identifies appropriate predictors for MRP models
#'   \item **Factor conversion**: Guides factor level creation for categorical variables
#' }
#'
#' @seealso
#' * create_expected_types() for creating expected type specifications
#' * check_data() for data validation using type classifications
#' * create_variable_list() for organizing variables by type
#' * dplyr::n_distinct() for counting unique values
#'
#' @examples
#' \dontrun{
#' # Binary classification
#' sex_var <- c("male", "female", "male", "female")
#' data_type(sex_var)  # Returns "bin"
#'
#' # Categorical classification
#' race_var <- c("white", "black", "hispanic", "asian", "other")
#' data_type(race_var)  # Returns "cat"
#'
#' # Continuous classification
#' income_var <- c(45000, 52000, 38000, 67000, 41000)
#' data_type(income_var)  # Returns "cont"
#'
#' # Numeric codes
#' data_type(sex_var, num = TRUE)  # Returns 1
#'
#' # Custom threshold
#' age_var <- c(25, 30, 35, 40, 45)  # Low uniqueness
#' data_type(age_var, threshold = 0.3)  # May return "cat" instead of "cont"
#' }
#'
#' @noRd
#'
#' @importFrom dplyr n_distinct
data_type <- function(col, num = FALSE, threshold = 0.1) {
  if(is.numeric(col)) {
    if(!all(as.integer(col) == col) | mean(table(col) == 1) > threshold) {
      dtype <- if(num) 3 else "cont"
    } else if(n_distinct(col) == 2) {
      dtype <- if(num) 1 else "bin"
    } else {
      dtype <- if(num) 2 else "cat"
    }
  } else {
    if(n_distinct(col, na.rm = TRUE) == 2) {
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
create_expected_types <- function(
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
create_expected_levels <- function(metadata) {
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
check_data <- function(df, expected_types, na_threshold = 0.5) {
  expected_columns <- names(expected_types)

  # Check for missing columns
  missing <- setdiff(expected_columns, names(df))
  if(length(missing) > 0) {
    stop(paste0("The following columns are missing: ",
                  paste(missing, collapse = ", ")))

  }
  
  # Check data types
  types <- df %>% select(all_of(expected_columns)) %>% lapply(data_type) %>% unlist()
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
        warning("Provided dates are not in expected format. Plots will use week indices instead.")
      }
    } else {
      warning("Dates are not provided. Plots will use week indices instead.")
    }
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
check_pstrat <- function(df, df_ref, expected_levels) {
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
#' @title Comprehensive data preprocessing pipeline
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
#' @param is_sample Logical. Whether the data represents sample data (TRUE) or
#'   poststratification data (FALSE). Affects validation and processing steps.
#' @param is_aggregated Logical. Whether the data is already aggregated (TRUE) or
#'   individual-level records (FALSE). Determines if aggregation step is needed.
#' @param zip_threshold Numeric. Minimum number of records required for a ZIP code
#'  to be included in the analysis. Default is 0.
#' @param state_threshold Numeric. Minimum proportion of records required for a
#'  state to be included in the analysis. Default is 0.
#'
#' @return Preprocessed data frame ready for MRP analysis with:
#' \itemize{
#'   \item **Standardized structure**: Cleaned column names and consistent formatting
#'   \item **Validated data types**: Appropriate types for demographic and outcome variables
#'   \item **Recoded variables**: Demographic variables matching expected factor levels
#'   \item **Geographic integration**: Geographic variables at multiple scales with FIPS codes
#'   \item **Time handling**: Week indices and complete timelines for time-varying data
#'   \item **Missing data treatment**: Imputed demographic variables using frequency-based sampling
#'   \item **Aggregation**: Cross-tabulated data for modeling (if individual-level input)
#' }
#'
#' @details The preprocessing pipeline executes these steps in sequence:
#' \enumerate{
#'   \item **Setup**: Determine COVID status and create expected factor levels
#'   \item **Data cleaning**: Apply clean_data() for standardization
#'   \item **Column renaming**: Use rename_columns() for standard variable names
#'   \item **Validation**: Check data types and structure with check_data()
#'   \item **Individual-level processing** (if not aggregated):
#'     \itemize{
#'       \item Remove rows with missing outcome/geographic data
#'       \item Convert dates to week indices for time-varying data
#'       \item Recode demographic variables to expected levels
#'       \item Impute missing demographic data
#'       \item Aggregate to cell counts for non-normal outcomes
#'     }
#'   \item **Geographic enhancement**: Add geographic variables at larger scales
#' }
#'
#' @section Data Type Handling:
#' The function handles different data scenarios:
#' \itemize{
#'   \item **COVID data**: Uses specialized column renaming and value recoding
#'   \item **Poll data**: Handles education variables and different age groupings
#'   \item **Time-varying**: Converts dates to week indices and creates timelines
#'   \item **Cross-sectional**: Standard demographic processing without time components
#'   \item **Binomial outcomes**: Aggregates to positive/total counts
#'   \item **Normal outcomes**: Preserves individual-level structure
#' }
#'
#' @section Aggregation Process:
#' For individual-level data (is_aggregated = FALSE):
#' \itemize{
#'   \item Groups by demographic variables and smallest geographic scale
#'   \item Calculates total counts (with weights if available)
#'   \item Sums positive outcomes for binomial data
#'   \item Preserves geographic covariates through first() function
#' }
#'
#' @seealso
#' * clean_data() for data cleaning and standardization
#' * rename_columns() for column name standardization
#' * check_data() for data validation
#' * recode_values() for demographic variable recoding
#' * add_week_indices() for time-varying data processing
#' * append_geo() for geographic variable enhancement
#' * create_expected_levels() for factor level specifications
#'
#' @examples
#' \dontrun{
#' # Preprocess COVID time-varying data
#' covid_metadata <- list(
#'   family = "binomial",
#'   is_timevar = TRUE,
#'   special_case = "covid"
#' )
#' processed_covid <- preprocess(raw_covid_data, covid_metadata,
#'                              is_sample = TRUE, is_aggregated = FALSE)
#'
#' # Preprocess poll cross-sectional data
#' poll_metadata <- list(
#'   family = "binomial",
#'   is_timevar = FALSE,
#'   special_case = "poll"
#' )
#' processed_poll <- preprocess(raw_poll_data, poll_metadata,
#'                             is_sample = TRUE, is_aggregated = TRUE)
#' }
#' 
#' @noRd
#'
#' @importFrom dplyr mutate group_by summarize ungroup across any_of first n full_join
#' @importFrom tidyr drop_na
#' @importFrom rlang syms .data
preprocess <- function(
  data,
  metadata,
  zip_county_state,
  is_sample = TRUE,
  is_aggregated = TRUE,
  zip_threshold = 0,
  state_threshold = 0
) {
  
  is_covid <- !is.null(metadata$special_case) &&
              metadata$special_case == "covid"
  levels <- create_expected_levels(metadata)
  indiv_vars <- names(levels)
  if (metadata$is_timevar) {
    indiv_vars <- c(indiv_vars, "time")
  }

  # Clean data
  data <- clean_data(data)

  # Find and rename columns
  data <- rename_columns(data, is_covid && !is_aggregated)

  # Check for common dataframe issues
  types <- create_expected_types(
    metadata = metadata,
    is_sample = is_sample,
    is_aggregated = is_aggregated
  )
  check_data(data, types)

  # Aggregate if needed
  if (!is_aggregated) {
    # remove NAs
    check_cols <- setdiff(names(data), indiv_vars)
    data <- data %>% tidyr::drop_na(all_of(check_cols))

    # convert date to week indices if necessary
    if (metadata$is_timevar) {
      data <- add_week_indices(data)
    }

    # remove duplicate rows
    data <- remove_duplicates(data, is_covid)

    # remove ZIP codes and states with small sample sizes
    if ("zip" %in% names(data)) {
      data <- filter_state_zip(
        data,
        zip_county_state,
        zip_threshold = zip_threshold,
        state_threshold = state_threshold
      )
    }

    # recode values to expected levels
    data <- recode_values(data, levels, is_covid)

    # impute missing demographic data based on frequency
    data <- data %>% mutate(across(all_of(indiv_vars), impute))

    if (metadata$family != "normal") {
      # aggregate test records based on combinations of factors
      smallest <- get_smallest_geo(names(data))
      smallest_geo <- if(!is.null(smallest)) smallest$geo else NULL
      group_vars <- c(indiv_vars, smallest_geo)
      geo_covars <- if(!is.null(smallest_geo)) names(get_geo_predictors(data, smallest_geo)) else NULL

      # cross-tabulate data
      data <- data %>%
        group_by(!!!syms(group_vars)) %>%
        summarize(
          across(any_of(geo_covars), first),
          date = if("date" %in% names(data)) first(.data$date),
          total = if("weight" %in% names(data)) sum(.data$weight) else n(),
          positive = sum(.data$positive)

        ) %>%
        ungroup()
    }
  }

  # append geographic areas at larger scales if missing
  data <- append_geo(data, zip_county_state)

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
create_variable_list <- function(input_data, covariates) {
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
      if (!is.null(data_source[[v]]) && n_distinct(data_source[[v]]) > 1) {
        if (data_type(data_source[[v]]) == "cat") {
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
  indiv_vars <- setdiff(names(input_data), c(GLOBAL$vars$geo, GLOBAL$vars$ignore, names(covariates)))
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
#'   one of the values in GLOBAL$vars$geo: "zip", "county", "state", or NULL for national.
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
combine_tracts <- function(
    tract_data,
    link_geo = NULL
) {

  checkmate::assert_choice(
    link_geo,
    choices = GLOBAL$vars$geo,
    null.ok = TRUE
  )

  if (is.null(link_geo)) {

    pstrat_data <- tract_data %>%
      mutate(geocode = "place_holder") %>%
      select(-.data$GEOID) %>%
      group_by(.data$geocode) %>%
      summarize_all(sum)

  } else if (link_geo == "zip") {

    # join tract-level data with zip-tract conversion table then group by zip
    by_zip <- zip_$tract %>%
      select(.data$geoid, .data$zip) %>%
      rename("GEOID" = "geoid") %>%
      inner_join(
        tract_data,
        by = "GEOID"
      ) %>%
      group_by(.data$zip)
    
    # compute zip-level population size by aggregating across overlapping tracts
    all_colnames <- names(tract_data)
    pstrat_colnames <- all_colnames[grepl("male|female", all_colnames)]
    pstrat_data <- by_zip %>%
      summarise(
        across(all_of(pstrat_colnames), ~ sum(.x, na.rm = TRUE))
      ) %>%
      rename("geocode" = "zip")

  } else if (link_geo == "county") {

    pstrat_data <- tract_data %>%
      mutate(geocode = substr(.data$GEOID, 1, 5)) %>%
      select(-.data$GEOID) %>%
      group_by(.data$geocode) %>%
      summarize_all(sum)

  } else if (link_geo == "state") {

    pstrat_data <- tract_data %>%
      mutate(geocode = substr(.data$GEOID, 1, 2)) %>%
      select(-.data$GEOID) %>%
      group_by(.data$geocode) %>%
      summarize_all(sum)

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
#' @details The function:
#' \enumerate{
#'   \item Aggregates tract data to the specified geographic scale
#'   \item Filters both datasets to common geographic areas
#'   \item Creates complete demographic combinations using expand.grid()
#'   \item Adds population weights from Census data
#'   \item Appends geographic predictors from input data
#'   \item Generates variable lists for modeling
#' }
#' 
#' @noRd 
#'
#' @importFrom dplyr filter select mutate arrange across left_join
#' @importFrom rlang sym .data
prepare_mrp_acs <- function(
    input_data,
    tract_data,
    metadata,
    link_geo = NULL
) {

  # compute cell counts based on given geographic scale
  pstrat_data <- combine_tracts(tract_data, link_geo)

  # filter based on common GEOIDs
  shared_geocodes <- c()
  if(!is.null(link_geo)) {
    shared_geocodes <- intersect(unique(input_data[[link_geo]]), pstrat_data$geocode)
    input_data <- input_data %>% filter(!!sym(link_geo) %in% shared_geocodes)
    pstrat_data <- pstrat_data %>% filter(.data$geocode %in% shared_geocodes)
  }
  cell_counts <- pstrat_data %>% select(-.data$geocode) %>% t() %>% c()

  # create lists of all factor levels
  n_time_indices <- 1
  levels <- create_expected_levels(metadata)
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
    arrange(across(all_of(sort_vars))) %>% # IMPORTANT: To match the cell order of poststratification data
    mutate(total = rep(cell_counts, n_time_indices))

  # append geographic predictors
  covariates <- NULL
  if(!is.null(link_geo)) {
    # find geographic covariates
    covariates <- get_geo_predictors(input_data, link_geo)
    if(ncol(covariates) > 1) {
      new_data <- left_join(new_data, covariates, by = link_geo)
    }
  }

  # append levels for other geographic predictors
  # NOTE: this must be done after new_data is created
  # as these levels are not used in the poststratification table
  for(v in intersect(names(new_data), GLOBAL$vars$geo)) {
    levels[[v]] <- unique(new_data[[v]]) %>% sort()
  }

  # add 'total' column to interface with plotting functions
  if (metadata$family == "normal") {
    input_data <- input_data %>% mutate(total = 1)
  }

  # create variable lists for model specification
  vars <- create_variable_list(input_data, covariates)

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
#' @details The function:
#' \enumerate{
#'   \item Filters both datasets to common geographic areas
#'   \item Creates factor levels from the filtered data
#'   \item Appends geographic predictors from input data
#'   \item Duplicates poststratification rows for each time period
#'   \item Generates variable lists for modeling
#' }
#' 
#' @noRd
#'
#' @importFrom dplyr filter mutate
#' @importFrom rlang sym
prepare_mrp_custom <- function(
    input_data,
    new_data,
    metadata,
    link_geo = NULL
) {

  # filter based on common GEOIDs
  shared_geocodes <- c()
  if(!is.null(link_geo)) {
    shared_geocodes <- intersect(unique(input_data[[link_geo]]), unique(new_data[[link_geo]]))
    input_data <- input_data %>% filter(!!sym(link_geo) %in% shared_geocodes)
    new_data <- new_data %>% filter(!!sym(link_geo) %in% shared_geocodes)
  }

  # create lists of all factor levels
  n_time_indices <- 1
  levels <- create_expected_levels(metadata)
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
    covariates <- get_geo_predictors(input_data, link_geo)
    if(ncol(covariates) > 1) {
      new_data <- clean_left_join(new_data, covariates, by = link_geo)
    }
  }

  # append levels for other geographic predictors
  for(v in intersect(names(new_data), GLOBAL$vars$geo)) {
    levels[[v]] <- unique(new_data[[v]]) %>% sort()
  }

  # duplicate rows for each time index
  new_data <- purrr::map_dfr(
    seq_len(n_time_indices),
    ~ new_data %>% mutate(time = .x)
  )

  # add 'total' column to interface with plotting functions
  if (metadata$family == "normal") {
    input_data <- input_data %>% mutate(total = 1)
  }

  vars <- create_variable_list(input_data, covariates)

  return(list(
    input = input_data,
    new = new_data,
    levels = levels,
    vars = vars
  ))
}

#' Aggregate FIPS codes to state level
#'
#' @description Converts county-level FIPS codes to state-level by taking the
#' first two digits and removing county information while keeping unique
#' state-level records. Used to create state-level lookup tables from
#' county-level data.
#'
#' @param df Data frame containing FIPS codes and geographic information
#'   with columns: fips (5-digit county codes), county, state, state_name.
#'
#' @return Data frame with state-level FIPS codes (2-digit) and corresponding
#'   state information. County column is removed and duplicates eliminated.
#'
#' @noRd
#'
#' @importFrom dplyr mutate select distinct
#' @importFrom rlang .data
aggregate_fips <- function(df) {

  df <- df %>%
    mutate(fips = substr(.data$fips, 1, 2)) %>%
    select(-.data$county) %>%
    distinct(.data$fips, .keep_all = TRUE)

  return(df)
}
