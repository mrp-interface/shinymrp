library(dplyr)
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
    data <- data %>%
      select(-time, time) %>%
      left_join(
        week_date %>% rename(time = week_index, date = start_of_week), 
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
  zip_county_state <- zip_county_state %>% mutate(county = fips)

  if(length(geo_vars) > 0) {
    # Find smallest geographic unit and select consecutive units
    all_geo_vars <- c("zip", "county", "state")
    valid_geo_vars <- intersect(geo_vars, all_geo_vars)
    smallest_geo <- all_geo_vars[min(match(valid_geo_vars, all_geo_vars))]
    
    # Select ordered unique values and generate random samples
    geo_df <- zip_county_state %>%
      select(all_of(geo_vars)) %>%
      distinct() %>%
      arrange(!!sym(smallest_geo))

    start_idx <- sample(1:(nrow(geo_df) - n_geo + 1), size = 1)
    geo_df <- geo_df[start_idx:(start_idx + n_geo - 1), ] %>%
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
  data <- indiv_vars %>%
    map_dfc(~tibble(!!.x := sample(levels[[.x]], n_samples, replace = TRUE)))

  return(data)
}

#' Simulate Binary Outcome Data
#'
#' Generates binary outcome data using a logistic model with the specified 
#' fixed and random effects.
#'
#' @param base_data Base data frame with predictor variables
#' @param effects List of fixed and varying effects
#' @param params List of parameter values for intercept, fixed effects, and hyperparameters
#' @param extra Sensitivity and specificity of the binary test
#' @param seed Random seed for reproducibility
#'
#' @return List containing simulated data, effects specification, and parameters
#' @export
simulate_data <- function(
    base_data,
    effects,
    params,
    family,
    seed = NULL,
    extra = NULL
) {

  if (!is.null(seed)) set.seed(seed)
  
  # Generate group-level parameters
  for(effect in names(effects$varying)) {
    lambda_name <- paste0("lambda_", effect)
    a_name <- paste0("a_", effect)
    if(lambda_name %in% names(params)) {
      params[[lambda_name]] <- rnorm(n_distinct(base_data[[effect]]), 0, params[[lambda_name]])
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

  if (family == "binomial") {
    data <- base_data %>% mutate(
      mu = !!rlang::parse_expr(formula_terms),
      ptrue = 1 / (1 + exp(-mu)),
      psample = extra$sens*ptrue + (1-extra$spec)*(1-ptrue),
      positive = rbinom(n(), 1, psample)
    )
  } else if (family == "normal") {
    data <- base_data %>% mutate(
      mu = !!rlang::parse_expr(formula_terms),
      outcome = rnorm(n(), mu, 1)
    )
  }
  
  return(list(
    data = data,
    effects = effects,
    params = params
  ))
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

prepare_indiv_agg <- function(
  base_data,
  sim_data,
  vars,
  family,
  include_date,
  extra
) {

  # convert age categories to numeric values
  data_indiv <- base_data %>%
    mutate(
      age = sapply(age, function(x) {
        bounds <- if(grepl("\\+", x)) {
          c(as.numeric(sub("\\+", "", x)), 100) 
        } else 
          as.numeric(strsplit(x, "-")[[1]])
        sample(bounds[1]:bounds[2], 1)
      })
    )
  
  if (family == "normal") {
    data_indiv <- data_indiv %>% mutate(outcome = sim_data$outcome)
  } else if (family == "binomial") {
    data_indiv <- data_indiv %>% mutate(positive = sim_data$positive)
  }

  if (extra$covid) {
    data_indiv <- data_indiv %>% 
      rename(result_date = date) %>%
      mutate(masked_id = purrr::map_chr(seq_len(n()), ~ generate_id())) %>% 
      select(masked_id, everything())
  }

  data_agg <- NULL
  if (family == "binomial") {
    data_agg <- base_data %>%
      mutate(positive = sim_data$positive) %>%
      group_by(across(all_of(c(vars$indiv, vars$geo)))) %>%
      summarise(
        date = if(include_date) first(date),
        across(all_of(vars$covar), first),
        total = n(),
        positive = sum(positive),
        .groups = "drop"
      )
  }

  return(list(
    indiv = data_indiv,
    agg = data_agg
  ))
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
check_simulation_input <- function(effects, params, family, covar_geo, include_date) {
  # Check family argument
  family <- match.arg(family, c("binomial", "normal"))

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
    family = NULL,
    covar_geo = NULL,
    n_time = 12,
    n_geo = 10,
    n_samples = 10000,
    include_date = FALSE,
    seed = sample(1:10000, 1),
    save_path = NULL,
    extra = NULL
) {

  # Check simulation input
  if(!check_simulation_input(effects, params, family, covar_geo, include_date)) {
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
  base_data_stan <- base_data %>% stan_factor(ignore_vars)

  # Simulate data
  sim <- simulate_data(
    base_data_stan,
    effects,
    params,
    family = family,
    seed = seed,
    extra = extra
  )

  # prepare individual and aggregated data
  out <- prepare_indiv_agg(
    base_data = base_data,
    sim_data = sim$dat,
    vars = list(
      indiv = indiv_vars,
      geo = geo_vars,
      covar = covar_vars
    ),
    family = family,
    include_date = include_date,
    extra = extra
  )

  if(!is.null(save_path)) {
    readr::write_csv(out$indiv, file.path(save_path, "data_individual.csv"))
    if (!is.null(out$agg)) {
      readr::write_csv(out$agg, file.path(save_path, "data_aggregated.csv"))
    }
  }

  return(list(
    data_indiv = out$indiv,
    data_agg   = out$agg,
    effects    = sim$effects,
    true_coefs = sim$params
  ))
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
  effects <- effects %>% group_effects(sim_data) %>% ungroup_effects()
  
  # Setup generated quantities data
  metadata <- list()

  # Run MCMC
  mod <- run_mcmc(
    input_data = sim_data %>% stan_factor(ignore_vars),
    new_data = sim_data %>% stan_factor(ignore_vars),
    effects = effects,
    metadata = metadata,
    spec = 1,
    sens = 1
  )

  # Get draws and true values
  draws <- mod$fit$mcmc$draws(variables = variables_draws, format = "draws_matrix")
  true <- do.call(c, true_coefs[variables_true])

  # Create and print recovery plots
  interval_plot <- mcmc_recover_intervals(x = draws, true = true)
  print(interval_plot)

  hist_plot <- mcmc_recover_hist(x = draws, true = true)
  print(hist_plot)
}

path <- "/Users/tntoan/Desktop/repos/shinymrp/dev/data/crosssectional_normal_sim.RDS"

# qs::qsave(
#   list(
#     effects = list(
#       Intercepts = list(
#         Intercept = "normal(0, 5)"
#       ),
#       fixed = list(
#         sex = "normal(0, 3)"
#       ),
#       varying = list(
#         race = "normal(0, 3)",
#         age = "normal(0, 3)",
#         # time = "normal(0, 3)",
#         zip = "normal(0, 3)",
#         county = "normal(0, 3)",
#         state = "normal(0, 3)"
#       )
#     ),
#     params = list(
#       Intercept = 0.5,
#       beta_sex = -0.25,
#       lambda_race = 0.3,
#       lambda_age = 0.4,
#       # lambda_time = 0.2,
#       lambda_zip = 0.5
#     ),
#     family = "normal",
#     covar_geo = "zip",
#     n_geo = 10,
#     include_date = FALSE,
#     extra = list(
#       covid = FALSE,
#       spec = 1,
#       sens = 1
#     )
#   ),
#   file = path
# )

sim_inputs <- qs::qread(path)

sim <- run_simulation(
  effects = sim_inputs$effects,
  params = sim_inputs$params,
  family = sim_inputs$family,
  covar_geo = sim_inputs$covar_geo,
  n_geo = sim_inputs$n_geo,
  include_date = sim_inputs$include_date,
  save_path = "/Users/tntoan/Downloads",
  extra = sim_inputs$extra
)

View(sim$data_indiv)
View(sim$data_agg)

# check_simulation_result(
#   sim_data = sim$data_agg,
#   effects = sim$effects,
#   true_coefs = sim$true_coefs
# )