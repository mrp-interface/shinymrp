
#' Return example data
#' 
#' @description Return example data based on the specified characteristics.
#' 
#' @param is_timevar Logical indicating whether the data is time-varying.
#' @param is_aggregated Logical indicating whether the data is aggregated.
#' @param special_case Optional character string for specific use cases such as COVID data.
#' Options are `NULL`, `"covid"` and `"poll"`. The default is `NULL` which indicates the
#'  data is not specific to any supported use case.
#' @param family Character string specifying the distribution family for outcome measures.
#' Options are `"binomial"` for binary outcomes and `"normal"` for continuous outcomes.
#' 
#' @return A `data.frame` object.
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

  .check_data_spec(metadata)

  suffix <- if (is_aggregated) "prep" else "raw"
  filename <- .create_example_filename(metadata, suffix)

  .fetch_data(filename, subdir = "example/data")
}


#' Return example poststratification data
#' 
#' @description Return example poststratification data accepted by
#' the `$load_pstrat()` method of an `MRPWorkflow` object.
#' 
#' @return A `data.frame` object.
#' 
#' @export
example_pstrat_data <- function() {
  .fetch_data("pstrat.csv", subdir = "example/data")
}

#' Return example `MRPModel` object with estimation results.
#'
#' @description Return an example `MRPModel` object with estimation results.
#' 
#' @param is_timevar Logical indicating whether the model is fitted
#' to time-varying data.
#'
#' @return A `MRPModel` object.
#'
#' @export
example_model <- function(is_timevar = TRUE) {
  if (is_timevar) {
    .fetch_data("timevarying_binomial_fit.qs", subdir = "example/fit")
  } else {
    .fetch_data("crosssectional_binomial_fit.qs", subdir = "example/fit")
  }
}