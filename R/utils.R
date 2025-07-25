#' Convert empty values to NULL
#'
#' @description Converts various types of empty or missing values to NULL,
#' including empty vectors, NA values, empty strings, and empty data frames.
#' Useful for standardizing empty inputs in data processing pipelines.
#'
#' @param x Any R object to check for emptiness
#'
#' @return NULL if the input is considered empty, otherwise returns x unchanged
#'
#' @noRd
nullify <- function(x) {
  # Check for empty vector and NULL
  if (length(x) == 0) return(NULL)

  # Check for NA
  if (length(x) == 1 && is.na(x)) return(NULL)
  
  # Check for empty string
  if (is.character(x) && x == "") return(NULL)
  
  
  # Check for empty dataframe
  if (is.data.frame(x) && nrow(x) == 0) return(NULL)
  
  # If none of above, return x unchanged
  return(x)
}

replace_null <- function(x, replacement) {
  if (is.null(x)) {
    return(replacement)
  } else {
    return(x)
  }
}

#' Create human-readable data format labels
#'
#' @description Converts internal data format codes to human-readable labels
#' for display in the user interface. Maps technical format names to
#' descriptive category labels.
#'
#' @return Character. Human-readable label corresponding to the data format,
#'   or "Unknown Data Format" if the format is not recognized
#'
#' @noRd
use_case_label <- function(metadata, labels = GLOBAL$ui$use_case_labels) {
  if (!is.null(metadata$special_case)) {
    switch(metadata$special_case,
      poll = labels$poll,
      covid = labels$covid,
      "Unknown"
    )
  } else {
    if (metadata$is_timevar) {
      labels$timevar_general
    } else {
      labels$static_general
    }
  }


}


#' Generate random IDs
#'
#' @description Generates random alphanumeric identifiers of specified length
#' using digits (0-9), lowercase letters (a-z), and uppercase letters (A-Z).
#' Useful for creating unique identifiers for UI elements or temporary objects.
#'
#' @param n Integer. Length of the ID to generate (default: 8)
#'
#' @return Character. A random alphanumeric string of length n
#'
#' @noRd
generate_id <- function(n = 8) {
  # Define the pool of characters: digits, lowercase and uppercase letters
  chars <- c(0:9, letters, LETTERS)
  
  # Sample with replacement and collapse into one string
  paste0(sample(chars, size = n, replace = TRUE), collapse = "")
}

#' Get configuration values
#'
#' @description Retrieves configuration values from the application's config.yml
#' file using the config package. Respects the R_CONFIG_ACTIVE environment
#' variable for different configuration environments.
#'
#' @param value Character. The configuration key to retrieve from config.yml
#'
#' @return The configuration value associated with the specified key
#'
#' @importFrom config get
#'
#' @noRd
get_config <- function(value) {
  config::get(
    value  = value,
    config = Sys.getenv("R_CONFIG_ACTIVE", "default"),
    file   = app_sys("config.yml")
  )
}

create_example_filename <- function(
  metadata,
  suffix = c("raw", "prep", "fit"),
  ext = ".csv",
  sep = "_",
  valid_families = GLOBAL$args$family
) {
  # Validate inputs
  suffix <- match.arg(suffix)
  if (!metadata$family %in% valid_families) {
    stop("Invalid family specified in metadata")
  }

  use_case <- if (!is.null(metadata$special_case)) {
    metadata$special_case
  } else {
    if (metadata$is_timevar) {
      "timevarying"
    } else {
      "crosssectional"
    }
  }

  # Construct file name
  family <- paste0(sep, metadata$family)
  suffix <- paste0(sep, suffix)
  file_name <- paste0(use_case, family, suffix, ext)

  return(file_name)
}

#' @noRd
check_interval <- function(interval) {
  is_ci <- TRUE
  qlower <- 0.025
  qupper <- 0.975
  n_sd <- 1

  if (is.character(interval)) {
    if (!grepl("^[1-3]+sd$", interval, ignore.case = FALSE)) {
      stop("For standard deviation of uncertainty, 'interval' must be either '1sd', '2sd', or '3sd'.")
    }
    
    is_ci <- FALSE
    n_sd <- as.numeric(gsub("sd", "", interval))

  } else if (is.numeric(interval)) {
    if (interval < 0 || interval > 1) {
      stop("For credible interval, 'interval' must be between 0 and 1.")
    } 

    is_ci <- TRUE
    qlower <- (1 - interval) / 2
    qupper <- 1 - qlower

  } else {
    stop("'interval' must be a character string or a numeric value between 0 and 1.")
  }

  return(list(
    is_ci = is_ci,
    qlower = qlower,
    qupper = qupper,
    n_sd = n_sd
  ))
}