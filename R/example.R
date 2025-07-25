
#' Return example data
#' 
#' @description Return different .
#' 
#' @param is_timevar Logical indicating whether the data is time-varying
#' @param is_aggregated Logical indicating whether the data is aggregated
#' @param special_case Optional character string for special cases (e.g., "covid", "poll")
#' @param family Character string specifying the distribution family for outcome measures (e.g., "binomial", "normal")
#' 
#' @return A data frame containing example data for the specified use case.
#' 
#' @export
example_sample_data <- function(
  is_timevar = TRUE,
  is_aggregated = TRUE,
  special_case = NULL,
  family = "binomial"
) {

  metadata <- list(
    is_timevar = is_timevar,
    special_case = special_case,
    family = family
  )
  suffix <- if (is_aggregated) "prep" else "raw"
  filename <- create_example_filename(metadata, suffix)

  readr::read_csv(
    system.file("extdata", "example", "data", filename, package = "shinymrp"),
    show_col_types = FALSE
  )
}


#' Create example post-stratification data
#' #' @description Generates example post-stratification data for MRP analysis.
#' 
#' @return An example post-stratification table.
#' 
#' @export
example_pstrat_data <- function() {
  readr::read_csv(
    system.file("extdata", "example", "data", "pstrat.csv", package = "shinymrp"),
    show_col_types = FALSE
  )
}