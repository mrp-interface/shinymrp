#' data_poll
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

prepare_data_poll <- function(
  input_data,
  pstrat_data,
  fips_county_state,
  demo_levels,
  vars_global
) {

  # convert geography names to FIPS codes
  link_geo <- "state"
  input_data[[link_geo]] <- input_data[[link_geo]] |> to_geocode(fips_county_state, link_geo)
  pstrat_data[[link_geo]] <- pstrat_data[[link_geo]] |> to_geocode(fips_county_state, link_geo)

  # keep states in the input data
  levels <- demo_levels
  levels$state <- unique(input_data$state) |> sort()
  pstrat_data <- pstrat_data |> filter(state %in% levels$state)

  # convert demographic levels to factors
  new_data <- pstrat_data |> as_factor(demo_levels)

  # find geographic covariates
  covariates <- get_geo_predictors(input_data, link_geo)
  if(ncol(covariates) > 1) {
    # append geographic predictors
    new_data <- left_join(new_data, covariates, by = link_geo)
  }

  vars <- create_variable_list(input_data, covariates, vars_global)

  return(list(input_data, new_data, levels, vars))
}