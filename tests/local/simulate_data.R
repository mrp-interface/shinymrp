library(dplyr)
library(zeallot)
library(stringr)
library(purrr)
library(bayesplot)
library(cmdstanr)

source("/Users/tntoan/Desktop/repos/shinymrp/R/fct_data.R")
source("/Users/tntoan/Desktop/repos/shinymrp/R/fct_model.R")

#' Add Date Information to Dataset
#'
#' Joins a dataset with week indices to corresponding date data.
#'
#' @param data Data frame containing a week index column
#' @param week_date Data frame with week-index-to-date conversion information
#'
#' @return Data frame with date column added
#' @export
add_date <- function(data, week_date) {
  if("time" %in% names(data)) {
    data <- data |>
      select(-time, time) |>
      left_join(
        week_date |> rename(time = week_index, date = start_of_week), 
        by = "time"
      )
  }

  return(data)
}

#' Add Geographic Variables to Dataset
#'
#' Adds geographic identifiers to a dataset by sampling from 
#' consecutive geographic regions.
#'
#' @param geo_vars Vector of geographic variable names to include
#' @param data Data frame to add geographic data to
#' @param zip_county_state Reference data mapping between zip, county, and state
#' @param n_geo Number of consecutive geographic units to sample
#'
#' @return Data frame with geographic variables added
#' @export
add_geo <- function(
    geo_vars,
    data,
    zip_county_state,
    n_geo = 10
) {
  zip_county_state <- zip_county_state |> mutate(county = fips)

  if(length(geo_vars) > 0) {
    # Find smallest geographic unit and select consecutive units
    all_geo_vars <- c("zip", "county", "state")
    valid_geo_vars <- intersect(geo_vars, all_geo_vars)
    smallest_geo <- all_geo_vars[min(match(valid_geo_vars, all_geo_vars))]
    
    # Select ordered unique values and generate random samples
    geo_df <- zip_county_state |>
      select(all_of(geo_vars)) |>
      distinct() |>
      arrange(!!sym(smallest_geo))

    start_idx <- sample(1:(nrow(geo_df) - n_geo + 1), size = 1)
    geo_df <- geo_df[start_idx:(start_idx + n_geo - 1), ] |>
      slice_sample(n = nrow(data), replace = TRUE)

    # Add geographic data to individual-level data
    data <- bind_cols(data, geo_df)
  }

  return(data)
}

#' Add Geographic Covariates
#'
#' Generates random covariates for each unique value in a geographic column
#' and adds them to the data.
#'
#' @param data Data frame containing geographic variables
#' @param geo_col Name of geographic column to use as basis for covariates
#' @param covars Vector of covariate names or named list with min/max ranges
#' @param range Common range for covariates
#' @param seed Random seed for reproducibility
#'
#' @return Data frame with added covariate columns
#' @export
#'
add_covariates <- function(
    data,
    geo_col,
    covars,
    range = c(0, 1),
    seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  
  # Quick validation
  if (is.null(geo_col) || !geo_col %in% names(data) ||
      is.null(covars) || length(covars) == 0) return(data)
  
  # Get unique geographic values
  unique_vals <- unique(data[[geo_col]])
  
  # Create base dataframe with geographic column
  cov_df <- tibble::tibble(!!sym(geo_col) := unique_vals)
  
  # Vectorized covariate generation
  covs <- if (is.list(covars)) {
    # Handle named list with ranges
    purrr::map2_dfc(
      names(covars),
      covars,
      ~tibble::tibble(!!.x := runif(length(unique_vals), .y[1], .y[2]))
    )
  } else {
    # Handle vector of names with common range
    purrr::map_dfc(
      covars,
      ~tibble::tibble(!!.x := runif(length(unique_vals), range[1], range[2]))
    )
  }
  
  # Join covariates with base dataframe
  cov_df <- bind_cols(cov_df, covs)
  
  # Join with original data and return
  left_join(data, cov_df, by = geo_col)
}

#' Create Base Dataset for Simulation
#'
#' Generates a dataset with individual-level variables for simulation.
#'
#' @param indiv_vars Vector of individual-level variables to include
#' @param geo_vars Vector of geographic variables to include
#' @param n_samples Number of samples to generate
#' @param n_time Number of time periods if time is included
#' @param age_bounds Vector of age boundaries for categories
#' @param seed Random seed for reproducibility
#'
#' @return Data frame with individual-level variables
#' @export
create_base_data <- function(
    indiv_vars,
    geo_vars,
    n_samples = 10000,
    n_time = 12,
    age_bounds = c(0, 18, 35, 65, 75),
    seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  # Setup levels
  levels <- list(
    sex = c("Male", "Female"),
    race = c("White", "Black", "Other"),
    age = c(paste0(age_bounds[-(length(age_bounds))], '-', age_bounds[-1] - 1),
            paste0(age_bounds[length(age_bounds)], '+')),
    edu = c("No HS", "HS", "Some College", "4-Year College", "Post-Grad"),
    time = 1:n_time
  )

  # Create individual-level data
  data <- indiv_vars |>
    map_dfc(~tibble(!!.x := sample(levels[[.x]], n_samples, replace = TRUE)))

  return(data)
}

#' Simulate Binary Outcome Data
#'
#' Generates binary outcome data using a logistic model with the specified 
#' fixed and random effects.
#'
#' @param base_dat Base data frame with predictor variables
#' @param effects List of fixed and varying effects
#' @param params List of parameter values for intercept, fixed effects, and hyperparameters
#' @param sens Sensitivity of the binary test (true positive rate)
#' @param spec Specificity of the binary test (true negative rate)
#' @param seed Random seed for reproducibility
#'
#' @return List containing simulated data, effects specification, and parameters
#' @export
simulate_data <- function(
    base_dat,
    effects,
    params,
    sens = 1,
    spec = 1,
    seed = NULL
) {

  if (!is.null(seed)) set.seed(seed)
  
  # Generate group-level parameters
  for(effect in names(effects$varying)) {
    lambda_name <- paste0("lambda_", effect)
    a_name <- paste0("a_", effect)
    if(lambda_name %in% names(params)) {
      params[[lambda_name]] <- rnorm(n_distinct(base_dat[[effect]]), 0, params[[lambda_name]])
    }
  }
  
  # Build formula terms
  formula_terms <- "params$Intercept"
  for(effect in names(effects$fixed)) {
    if(paste0("beta_", effect) %in% names(params)) {
      formula_terms <- paste0(formula_terms, " + params$beta_", effect, " * ", effect)
    }
  }
  
  # Add random effects
  for(effect in names(effects$varying)) {
    if(paste0("lambda_", effect) %in% names(params)) {
      formula_terms <- paste0(formula_terms, " + params$lambda_", effect, "[", effect, "]")
    }
  }

  # Generate data
  print(formula_terms)
  dat <- base_dat |> mutate(
    mu = !!rlang::parse_expr(formula_terms),
    ptrue = 1 / (1 + exp(-mu)),
    psample = sens*ptrue + (1-spec)*(1-ptrue),
    positive = rbinom(n(), 1, psample)
  )
  
  return(list(dat, effects, params))
}

#' Check Simulation Input
#'
#' Validates that the effects and parameters are correctly specified
#' before running a simulation.
#'
#' @param effects List of effects with fixed and varying components
#' @param params List of parameters with coefficients
#' @param covar_geo Geographic column name to use for covariates
#'
#' @return TRUE if all checks pass
#' @export
check_simulation_input <- function(effects, params, covar_geo, include_date) {
  # Check for Intercept
  if (!"Intercept" %in% names(params)) {
    stop("Intercept parameter missing from params")
  }
  
  # Check for covariates and covar_geo relationship
  covariates <- setdiff(names(effects$fixed), "sex")
  if (length(covariates) > 0) {
    if (is.null(covar_geo)) {
      stop(paste0("Area-level covariates detected (", 
                 paste(covariates, collapse=", "), 
                 ") but no covar_geo provided. Please specify a geographic unit for covariates."))
    }
    
    # Also check if the specified covar_geo exists in varying effects
    if (!covar_geo %in% names(effects$varying)) {
      stop(paste0("Geographic identifier '", covar_geo, 
               "' must be included as a varying effect when using area-level covariates."))
    }

  }

  # Check if time is included in effects when include_date is TRUE
  if (!"time" %in% names(effects$varying) && include_date) {
    stop("When include_date is set to TRUE, 'time' must be included in varying effects.")
  }
  
  # Check for unused parameters
  used_params <- c("Intercept")
  if (length(effects$fixed) > 0) {
    used_params <- c(used_params, paste0("beta_", names(effects$fixed)))
  }
  if (length(effects$varying) > 0) {
    used_params <- c(used_params, paste0("lambda_", names(effects$varying)))
    used_params <- c(used_params, paste0("a_", names(effects$varying)))
  }
  
  unused_params <- setdiff(names(params), used_params)
  if (length(unused_params) > 0) {
    warning(paste0("Unused parameters: ", paste(unused_params, collapse = ", ")))
  }
  
  return(TRUE)
}

#' Run Complete Simulation Workflow
#'
#' Generates a complete simulated dataset with individual and aggregated data
#' based on specified model effects and parameters.
#'
#' @param effects List of fixed and varying effects
#' @param params List of parameter values
#' @param covar_geo Geographic column name to use for covariates
#' @param n_samples Number of samples to generate
#' @param seed Random seed for reproducibility
#' @param include_date Whether to include date information
#' @param save_path Path to save CSV outputs (NULL to skip saving)
#'
#' @return List containing individual data, aggregated data, effects, and parameters
#' @export
run_simulation <- function(
    effects,
    params,
    covar_geo = NULL,
    n_time = 12,
    n_geo = 10,
    n_samples = 10000,
    include_date = FALSE,
    seed = sample(1:10000, 1),
    save_path = NULL
) {

  # Check simulation input
  if(!check_simulation_input(effects, params, covar_geo, include_date)) {
    return(NULL)
  }

  week_date <- readr::read_csv("/Users/tntoan/Desktop/repos/shinymrp/inst/extdata/week_conversion.csv")
  zip_county_state <- readr::read_csv("/Users/tntoan/Desktop/repos/shinymrp/inst/extdata/zip_county_state.csv")
  
  # All individual and geographic variables
  indiv_vars_all <- c("sex", "race", "age", "edu", "time")
  geo_vars_all <- c("zip", "county", "state")
  ignore_vars <- c("date", "total", "positive")

  # Extract individual and geographic variables from effect list
  vars <- c(names(effects$fixed), names(effects$varying))
  indiv_vars <- intersect(vars, indiv_vars_all)
  geo_vars <- intersect(vars, geo_vars_all)
  covar_vars <- setdiff(names(effects$fixed), "sex")

  # Create base data with individual-level variables
  base_data <- create_base_data(
    indiv_vars = indiv_vars,
    geo_vars = geo_vars,
    n_time = n_time,
    n_samples = n_samples,
    seed = seed
  )

  # Add dates
  if(include_date) {
    base_data <- add_date(base_data, week_date)
  }

  # Add geographic identifiers
  base_data <- add_geo(geo_vars, base_data, zip_county_state, n_geo = n_geo)

  # Add geographic covariates
  base_data <- add_covariates(base_data, covar_geo, covar_vars)

  # Create numeric factors for Stan
  base_data_stan <- base_data |> stan_factor(ignore_vars)

  # Simulate data
  c(sim_data, effects, params) %<-% simulate_data(
    base_data_stan,
    effects,
    params,
    spec = 1,
    sens = 1,
    seed = seed
  )

  example_data_indiv <- base_data |>
    mutate(
      age = sapply(age, function(x) {
        bounds <- if(grepl("\\+", x)) {
          c(as.numeric(sub("\\+", "", x)), 100) 
        } else 
          as.numeric(strsplit(x, "-")[[1]])
        sample(bounds[1]:bounds[2], 1)
      }),
      positive = sim_data$positive
    )

  example_data_agg <- base_data |>
    mutate(positive = sim_data$positive) |>
    group_by(across(all_of(c(indiv_vars, geo_vars)))) |>
    summarise(
      date = if(include_date) first(date),
      across(all_of(covar_vars), first),
      total = n(),
      positive = sum(positive),
      .groups = "drop"
    )

  if(!is.null(save_path)) {
    readr::write_csv(example_data_indiv, paste0(save_path, "data_individual.csv"))
    readr::write_csv(example_data_agg, paste0(save_path, "data_aggregated.csv"))
  }

  return(list(example_data_indiv, example_data_agg, effects, params))
}

#' Check Parameter Recovery in Simulation
#'
#' Runs MCMC on simulated data and checks whether the true parameter values
#' used for simulation can be recovered by comparing posterior draws to true values.
#'
#' @param sim_data Simulated data frame (typically aggregated data)
#' @param effects List of fixed and varying effects used in simulation
#' @param true_coefs True coefficient values used in data generation
#' @param ignore_vars Variables to ignore when factorizing data for Stan
#' @param return_plots Whether to return plot objects (default: FALSE)
#'
#' @return 
#' @export
check_simulation_result <- function(
    sim_data,
    effects,
    true_coefs,
    ignore_vars = c("date", "total", "positive"),
    return_plots = FALSE
) {

  # Extract parameter names from effects
  variables_true <- "Intercept"
  variables_draws <- "Intercept"
  if(length(effects$fixed) > 0) {
    variables_true <- c(variables_true, paste0("beta_", names(effects$fixed)))
    variables_draws <- c(variables_draws, "beta")
  }
  if(length(effects$varying) > 0) {
    vars_random <- paste0("lambda_", names(effects$varying))
    variables_true <- c(variables_true, vars_random)
    variables_draws <- c(variables_draws, vars_random)
  }

  # Group effects for MCMC
  effects <- effects |> group_effects(sim_data) |> ungroup_effects()
  
  # Setup generated quantities data
  gq_data <- list(
    subgroups = c("sex", "race", "age"),
    temporal = "time" %in% names(sim_data)
  )

  # Run MCMC
  c(fit, stan_data, stan_code) %<-% run_mcmc(
    input_data = sim_data |> stan_factor(ignore_vars),
    new_data = sim_data |> stan_factor(ignore_vars),
    effects = effects,
    gq_data = gq_data,
    spec = 1,
    sens = 1
  )

  # Get draws and true values
  draws <- fit$mcmc$draws(variables = variables_draws, format = "draws_matrix")
  true <- do.call(c, true_coefs[variables_true])

  # Create and print recovery plots
  interval_plot <- mcmc_recover_intervals(x = draws, true = true)
  print(interval_plot)

  hist_plot <- mcmc_recover_hist(x = draws, true = true)
  print(hist_plot)
}

effects <- list(
  Intercepts = list(
    Intercept = "normal(0, 5)"
  ),
  fixed = list(
    sex = "normal(0, 3)"
  ),
  varying = list(
    race = "normal(0, 3)",
    age = "normal(0, 3)",
    time = "normal(0, 3)",
    zip = "normal(0, 3)",
    county = "normal(0, 3)",
    state = "normal(0, 3)"
  )
)

params <- list(
  Intercept = -4.5,
  beta_sex = -0.25,
  lambda_race = 0.3,
  lambda_age = 0.4,
  lambda_time = 0.9,
  lambda_zip = 0.5
)

c(data_indiv, data_agg, effects, true_coefs) %<-% run_simulation(
  effects = effects,
  params = params,
  covar_geo = "zip",
  include_date = FALSE,
  save_path = "/Users/tntoan/Desktop/repos/shinymrp/inst/extdata/example/data/timevarying_"
)
View(data_indiv)
View(data_agg)
# check_simulation_result(
#   sim_data = data_agg,
#   effects = effects,
#   true_coefs = true_coefs
# )