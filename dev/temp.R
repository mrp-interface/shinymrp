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
model1 <- workflow$create_model(
  varying = list(
    zip = "bym2"
  )
)
model2 <- workflow$create_model(
  varying = list(
    zip = ""
  )
)

# Run MCMC
model1$fit(n_iter = 1000, n_chains = 4, seed = 123, max_treedepth = 15)
model2$fit(n_iter = 1000, n_chains = 4, seed = 123, max_treedepth = 15)

print(model1$summary())
print(model2$summary())

# Compare model results
comparison <- workflow$compare_models(model1, model2)
print(comparison)