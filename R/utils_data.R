#' Check data specifications
#'
#' @description Validate the data specifications against expected criteria.
#'
#' @param metadata A list containing metadata about the data being processed.
#'
#' @noRd
#' @keywords internal
.check_data_spec <- function(metadata) {
  if (!is.null(metadata$special_case) && identical(metadata$family, "normal")) {
    data_name <- switch(metadata$special_case,
      "covid" = "COVID data",
      "poll" = "poll data"
    )
    stop(paste0("Binary outcome measure ('binomial' distribution family) is expected for ", data_name, "."))
  }

  if (!is.null(metadata$time_var) && !is.null(metadata$time_freq)) {
    stop("Time indexing frequency cannot be specified for static data.")
  }
}