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
#' @param data_format Character. Internal data format code such as
#'   "static_poll", "static_other", "temporal_covid", or "temporal_other"
#'
#' @return Character. Human-readable label corresponding to the data format,
#'   or "Unknown Data Format" if the format is not recognized
#'
#' @noRd
data_format_label <- function(data_format) {
  switch(data_format,
         static_poll = "Cross-Sectional: Poll",
         static_other = "Cross-Sectional: Other",
         temporal_covid = "Time-Varying: COVID",
         temporal_other = "Time-Varying: Other",
         "Unknown Data Format")
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
