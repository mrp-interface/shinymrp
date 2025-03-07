#' utils 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
nullify <- function(x) {
  # Return NULL if x is NULL
  if (is.null(x)) return(NULL)
  
  # Check for NA
  if (length(x) == 1 && is.na(x)) return(NULL)
  
  # Check for empty string
  if (is.character(x) && length(x) == 1 && x == "") return(NULL)
  
  # Check for empty vector
  if (length(x) == 0) return(NULL)
  
  # Check for empty dataframe
  if (is.data.frame(x) && nrow(x) == 0) return(NULL)
  
  # If none of above, return x unchanged
  return(x)
}