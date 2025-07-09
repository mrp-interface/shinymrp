#' Read data from various file formats
#'
#' @description Reads data from CSV, Excel (xlsx/xls), or SAS (sas7bdat) files
#' based on the file extension. Automatically detects the file format and uses
#' the appropriate reading function.
#'
#' @param file_path Character string. Path to the data file to be read.
#'
#' @return A data frame containing the data from the specified file.
#'
#' @noRd
#'
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
#' @description Converts column names to lowercase, replaces numbers and
#' non-alphabetic characters with underscores, removes multiple consecutive
#' underscores, and trims leading/trailing underscores.
#'
#' @param names Character vector of column names to be cleaned.
#'
#' @return Character vector of cleaned column names.
#'
#' @noRd
#' 
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
#' @description Converts all character columns to lowercase and trims
#' leading/trailing whitespace.
#'
#' @param df Data frame with character columns to be cleaned.
#'
#' @return Data frame with cleaned character columns.
#'
#' @noRd
#'
#' @importFrom dplyr mutate across where
clean_chr <- function(df) {
  # Convert character columns to lowercase and trim whitespace
  df %>% mutate(
    across(where(is.character), ~ stringr::str_trim(tolower(.x)))
  )
}

#' Validate and clean geocodes
#'
#' @description Validates geocodes by checking if they are exactly 5 digits.
#' Invalid geocodes (including non-character inputs) are replaced with NA.
#'
#' @param geocodes Vector of geocodes to be validated. Can be numeric or character.
#'
#' @return Character vector of validated geocodes with invalid entries as NA.
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
#' @description Formats zip, county, and state columns to ensure proper
#' formatting. Zip and county codes are formatted as 5-digit strings with
#' leading zeros, state codes as 2-digit strings with leading zeros.
#'
#' @param df Data frame containing geographic identifier columns (zip, county, state).
#'
#' @return Data frame with properly formatted geographic identifiers.
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
#' @description Comprehensive data cleaning that includes: cleaning column names,
#' removing duplicate columns, converting character columns to lowercase and
#' trimming whitespace, converting common NA strings to actual NA values, and
#' formatting geographic identifiers.
#'
#' @param df Data frame to be cleaned.
#' @param na_strings Character vector of strings to be converted to NA.
#'   Default includes common representations of missing values.
#'
#' @return Cleaned data frame.
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

preprocess_example <- function(df) {
  df <- df %>% clean_data() 
  
  if ("state" %in% names(df)) {
    df$state <- to_fips(df$state, "state")
  }

  return(df)
}

#' Rename columns based on expected variable names
#'
#' @description Renames columns in the data frame to match expected variable
#' names defined in the constants. For COVID individual data, uses a specialized
#' renaming function.
#'
#' @param df Data frame with columns to be renamed.
#' @param const List containing variable name mappings and constants.
#' @param covid_indiv Logical. If TRUE, uses COVID-specific individual data
#'   column renaming. Default is FALSE.
#'
#' @return Data frame with renamed columns.
#'
#' @noRd
#'
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

#' Impute missing values using frequency-based sampling
#'
#' @description Imputes missing values in a vector by sampling from the
#' observed values based on their frequency distribution. If no missing
#' values exist, returns the original vector unchanged.
#'
#' @param v Vector with potential missing values to be imputed.
#'
#' @return Vector with missing values imputed based on frequency distribution
#'   of observed values.
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
#' @description Converts date strings to ISO week indices and creates a
#' timeline of dates. Handles single and multi-year date ranges by calculating
#' cumulative week numbers and generating a complete timeline.
#'
#' @param strings Character vector of date strings to be converted.
#'
#' @return List containing:
#'   \item{indices}{Numeric vector of cumulative week indices}
#'   \item{timeline}{Date vector of first day of each week in the timeline}
#'
#' @noRd
#'
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

add_week_indices <- function(df) {
  common <- intersect(names(df), GLOBAL$vars$time)

  if (length(common) == 1 && "date" %in% common) {
    # convert date to week indices
    week <- get_week_indices(df$date)
    df$time <- week$indices

    # add the column containing first dates of the weeks
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

#' Convert geographic identifiers to FIPS codes
#'
#' @description Converts geographic identifiers (names or codes) to standardized
#' FIPS codes. If input is already numeric, formats with appropriate leading zeros.
#' Otherwise, matches against lookup table to find corresponding FIPS codes.
#'
#' @param vec Vector of geographic identifiers to be converted.
#' @param link_geo Character string specifying geographic level. Must be either
#'   "county" or "state". Default is "county".
#'
#' @return Character vector of FIPS codes with proper formatting.
#'
#' @noRd
to_fips <- function(vec, link_geo = c("county", "state")) {
  link_geo <- match.arg(link_geo)

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
#' @param geo_all Character vector of all possible geographic scales in hierarchy.
#'
#' @return Data frame with additional geographic variables at larger scales.
#'
#' @noRd
#'
#' @importFrom dplyr select rename mutate distinct
#' @importFrom rlang .data
append_geo <- function(input_data) {
  # get the smallest geographic scale in the data
  smallest <- get_smallest_geo(names(input_data))

  # Get geographic variables at current and larger scales
  geo_vars <- get_possible_geos(names(input_data))

  if (is.null(geo_vars)) {
    return(input_data)
  }

  # Prepare geographic crosswalk
  zip_county_state <- zip_$county_state %>%
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
#' @description Classifies a column as binary, categorical, or continuous based
#' on its values and distribution. Uses uniqueness threshold to distinguish
#' between categorical and continuous numeric variables.
#'
#' @param col Vector representing a data column to be classified.
#' @param num Logical. If TRUE, returns numeric codes (1=binary, 2=categorical,
#'   3=continuous). If FALSE, returns character labels. Default is FALSE.
#' @param threshold Numeric. Threshold for determining if numeric data should be
#'   treated as continuous (proportion of unique values). Default is 0.1.
#'
#' @return Character string ("bin", "cat", "cont") or numeric code (1, 2, 3)
#'   indicating the data type.
#'
#' @noRd
#'
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
#' @description Comprehensive preprocessing pipeline that cleans data, renames
#' columns, validates structure, handles aggregation, converts dates to time
#' indices, recodes values, imputes missing data, and appends geographic variables.
#' This is the main entry point for data preprocessing in the shinymrp package.
#'
#' @param data Data frame containing raw uploaded data.
#' @param metadata List containing analysis metadata including family type,
#'   special cases, and time-varying flags.
#' @param is_sample Logical. Whether the data represents sample data. Default is TRUE.
#' @param is_aggregated Logical. Whether the data is already aggregated. Default is TRUE.
#'
#' @return Preprocessed data frame ready for MRP analysis with:
#' \itemize{
#'   \item Cleaned and standardized column names
#'   \item Validated data structure and types
#'   \item Recoded demographic variables to expected levels
#'   \item Geographic variables appended at appropriate scales
#'   \item Time indices created for time-varying data
#'   \item Missing values imputed where appropriate
#'   \item Data aggregated if needed for modeling
#' }
#'
#' @importFrom dplyr mutate group_by summarize ungroup across any_of first n full_join
#' @importFrom tidyr drop_na
#' @importFrom rlang syms .data
preprocess <- function(
  data,
  metadata,
  is_sample = TRUE,
  is_aggregated = TRUE
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
          total = if("weight" %in% names(data)) sum(.data$weight) else n(),
          positive = sum(.data$positive)
        ) %>%
        ungroup()
    }
  }

  # append geographic areas at larger scales if missing
  data <- append_geo(data)

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
