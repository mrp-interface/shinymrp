#' utils 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
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

data_format_label <- function(data_format) {
  switch(data_format,
         static_poll = "Cross-Sectional: Poll",
         static_other = "Cross-Sectional: Other",
         temporal_covid = "Time-Varying: COVID",
         temporal_other = "Time-Varying: Other",
         "Unknown Data Format")
}


generate_id <- function(n = 8) {
  # Define the pool of characters: digits, lowercase and uppercase letters
  chars <- c(0:9, letters, LETTERS)
  
  # Sample with replacement and collapse into one string
  paste0(sample(chars, size = n, replace = TRUE), collapse = "")
}

get_config <- function(value) {
  config::get(
    value  = value,
    config = Sys.getenv("R_CONFIG_ACTIVE", "default"),
    file   = app_sys("config.yml")
  )
}
