#' data_poll
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
to_factor_poll <- function(df, age_bounds) {
  breaks <- c(-1, age_bounds[2:length(age_bounds)] - 1, 200)
  labels <- c(paste0(age_bounds[1:(length(age_bounds)-1)], '-', age_bounds[2:length(age_bounds)] - 1),
              paste0(age_bounds[length(age_bounds)], '+'))

  df <- df |> mutate(
    sex  = recode_values(sex, c("female"), other = "male"),
    race = recode_values(race, c("white", "black"), other = "other"),
    edu  = recode_values(edu, c("no hs", "some college", "4-year college", "post-grad"), other = "hs"),
    age  = cut(df$age, breaks, labels) |> as.character()
  )

  return(df)
}


aggregate_poll <- function(df, age_bounds, threshold = 0) {
  # identify columns
  df <- find_columns(df, expected_columns = c("sex", "race", "age", "edu", "state", "positive"))

  # impute missing demographic data based on frequency
  df <- df |> mutate(across(c(sex, race, age, edu), impute))

  # # create factors from raw values
  df <- to_factor_poll(df, age_bounds)

  # aggregate test records based on combinations of factors
  # and omit cells with small number of tests
  df <- df |>
    group_by(state, edu, age, race, sex) |>
    filter(n() >= threshold) |>
    summarize(
      total = n(),
      positive = sum(positive)
    ) |>
    ungroup()

  return(df)
}


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

  # filter  convert demographic levels to factors
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