devtools::load_all()

# Initialize workflow
workflow <- mrp_workflow()

# Load example data
sample_data <- readr::read_csv(
  "/Users/tntoan/Downloads/simulated_data_lowpass.csv",
  show_col_types = FALSE
)

# Preprocess sample data
workflow$preprocess(
  sample_data,
  is_timevar = TRUE,
  is_aggregated = FALSE,
  special_case = NULL,
  family = "binomial"
)

# Link to ACS data at ZIP code level
workflow$link_acs(
  link_geo = "zip",
  acs_year = 2021
)

# Create and fit multiple models
model <- workflow$create_model(
  varying = list(
    zip = "bym2"
  )
)

# Run MCMC
model$fit(n_iter = 500, n_chains = 2, seed = 123, max_treedepth = 15)