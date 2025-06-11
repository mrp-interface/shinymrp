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
  suffix = c("individual", "aggregated", "pstrat"),
  ext = ".csv",
  valid_families = GLOBAL$family
) {
  suffix <- match.arg(suffix)
  if (!metadata$family %in% valid_families) {
    stop("Invalid family specified in metadata")
  }

  use_case <- if (!is.null(metadata$special_case)) {
    switch(metadata$special_case,
      poll = "poll",
      covid = "covid"
    )
  } else {
    if (metadata$is_timevar) {
      "timevarying"
    } else {
      "crosssectional"
    }
  }
  
  paste0(use_case, '_', metadata$family, '_', suffix, ext)
}
