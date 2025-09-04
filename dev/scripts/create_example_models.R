create_example_model <- function(
  metadata,
  model_spec,
  n_iter = 500,
  n_chains = 2,
  seed = 1234,
  file = NULL
) {

  # Load example data
  sample_data <- example_sample_data(
    is_timevar = metadata$is_timevar,
    is_aggregated = FALSE,
    special_case = metadata$special_case,
    family = metadata$family
  )

  workflow <- mrp_workflow()

  # Preprocess the input data
  workflow$preprocess(
    sample_data,
    is_timevar    = metadata$is_timevar,
    is_aggregated = FALSE,
    special_case  = metadata$special_case,
    family        = metadata$family
  )

  # Link to ACS and obtain poststratification data (e.g., by ZIP code)
  workflow$link_acs(
    link_geo = "zip",
    acs_year = 2021
  )

  # Create the MRP model
  model <- workflow$create_model(
    intercept_prior = model_spec$intercept$intercept,
    fixed          = model_spec$fixed,
    varying        = model_spec$varying,
    interaction    = model_spec$interaction
  )

  # Fit the model
  model$fit(
    n_iter    = n_iter,
    n_chains  = n_chains,
    seed      = seed
  )

  model$summary()
  model$diagnostics()
  model$loo()
  model$log_lik()
  model$poststratify()

  model$save(file = file)
}

create_example_model(
  metadata = list(
    is_timevar = FALSE,
    special_case = NULL,
    family = "binomial"
  )
)