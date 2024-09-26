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
    sex  = to_factor(sex, c("female"), other = "male"),
    race = to_factor(race, c("white", "black"), other = "other"),
    edu  = to_factor(edu, c("no hs", "some college", "4-year college", "post-grad"), other = "hs"),
    age  = cut(df$age, breaks, labels) |> as.character()
  )

  return(df)
}


aggregate_poll <- function(df, age_bounds, threshold = 0) {
  # identify columns
  df <- find_columns(df, expected_columns = c("sex", "race", "age", "edu", "state", "response"))

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
      positive = sum(response)
    ) |>
    ungroup()

  return(df)
}


check_poll_data <- function(df, expected_columns, na_threshold = 0.5) {
  errors <- list()

  df <- find_columns(df, expected_columns)
  missing <- df |> lapply(function(c) all(c == "")) |> unlist()
  missing_names <- names(missing)[missing]

  if(length(missing_names) > 1 | (length(missing_names) == 1 & !"state" %in% missing_names)) {
    errors$missing_column <- paste0("The following columns are missing: ",
                                    paste(missing_names, collapse = ", "),
                                    "(\"state\" column is optional)")
  } else {
    # check data types
    expected_types <- c("character", "character", "character", "character", "character|integer|numeric", "integer|numeric", "integer|numeric")
    types <- df |> lapply(class) |> unlist()
    valid <- mapply(grepl, types, expected_types)

    if(any(!valid)) {
      errors$data_type <- paste0("Columns corresponding to the following variables have inappropriate data types: ",
                                 paste(expected_columns[!valid], collapse = ", "))
    } else {
      na_percents <- df |>
        lapply(function(c) sum(as.numeric(is.na(c))) / length(c)) |>
        unlist()
      exceed <- na_percents > na_threshold

      if(any(exceed)) {
        errors$na <- paste0("Columns corresponding to the following variables have more than ",
                            na_threshold * 100, " percent rows with missing data: ",
                            paste(expected_columns[exceed], collapse = ", "))
      } else {
        # check if state column is missing
        if("state" %in% missing_names) {
          errors$state <- "States are not provided (\"state\" column is optional)"
        }

        # check if positive votes are less than or equal to total votes
        if(any(df$positive > df$total)) {
          errors$count <- "The number of supporting votes cannot be greater than the total number of votes"
        }
      }
    }
  }

  return(errors)
}

mod_fips <- function(df) {

  df <- df |>
    mutate(fips = substr(fips, 1, 2)) |>
    select(-county) |>
    distinct(fips, .keep_all = TRUE)

  return(df)
}

to_fips <- function(state_vec, fips_state) {
  if(is.null(state_vec)) {
    return(NULL)
  }

  counts <- fips_state |> apply(2, function(c) sum(c %in% state_vec))
  colname <- names(counts)[which.max(counts)]

  if(colname == "fips") {
    fips <- if(is.numeric(state_vec)) sprintf("%02d", state_vec) else state_vec
  } else {
    fips <- fips_state$fips[match(state_vec, fips_state[[colname]])]
  }

  return(fips)
}

stan_factor_poll <- function(df, levels) {
  df <- df |> mutate(
    sex = factor(sex, levels = levels$sex, labels = c(0, 1)) |> as.character() |> as.integer(),
    race = factor(race, levels = levels$race, labels = 1:length(levels$race)) |> as.character() |> as.integer(),
    age = factor(age, levels = levels$age, labels = 1:length(levels$age)) |> as.character() |> as.integer(),
    edu = factor(edu, levels = levels$edu, labels = 1:length(levels$edu)) |> as.character() |> as.integer(),
    state = as.factor(state) |> as.integer()
  )

  return(df)
}

get_state_predictors <- function(df) {
  df <- janitor::clean_names(df)
  all_cols <- names(df)
  state_col <- all_cols[grepl("state", all_cols, ignore.case = TRUE)]

  if(length(state_col) == 0) {
    return(data.frame())
  }

  bool <- df |>
    group_by(!!!syms(state_col)) |>
    summarize_all(n_distinct) |>
    lapply(function(c) all(c == 1)) |>
    unlist()

  state_pred_cols <- all_cols[bool]

  state_preds <- df |>
    select(all_of(c(state_col, state_pred_cols))) |>
    distinct(!!!syms(state_col), .keep_all = TRUE)

  names(state_preds)[1] <- "state"

  return(state_preds)
}

prepare_data_poll <- function(
    data,
    pstrat,
    covar,
    demo_levels
) {

  states_in_data <- unique(data$state)
  if(is.null(states_in_data)) {
    new_data <- pstrat |>
      group_by(sex, race, age, edu) |>
      summarize(total = sum(total)) |>
      ungroup()
  } else {
    new_data <- pstrat |> filter(state %in% states_in_data)
  }

  new_data <- new_data |> mutate(
    sex  = factor(sex, levels = demo_levels$sex),
    race = factor(race, levels = demo_levels$race),
    age  = factor(age, levels = demo_levels$age),
    edu  = factor(edu, levels = demo_levels$edu)
  )

  # append state-level predictors
  if(ncol(covar) > 1) {
    new_data <- left_join(new_data, covar, by = "state")
  }

  # create lists of all factor levels
  levels <- demo_levels
  if(!is.null(states_in_data)) {
    levels$state <- states_in_data
  }

  # list of variables for model specification
  vars <- list(
    fixed = list(
      "Individual-level Predictor" = list("sex")
    ),
    varying = list(
      "Individual-level Predictor" = c("race", "age", "edu")
    )
  )

  if(!is.null(states_in_data)) {
    # NOTE: current implementation only accepts continuous covariates
    vars$fixed[["Geographic Predictor"]] <- names(covar)[sapply(covar, is.numeric)]
    vars$varying[["Geographic Indicator"]] <- "state"
  }


  return(list(data, new_data, levels, vars))
}

