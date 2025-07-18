devtools::load_all()

# Load the shinymrp package
raw_data <- readr::read_csv(
    system.file("extdata", "example", "data", "timevarying_binomial_raw.csv", package = "shinymrp"),
    show_col_types = FALSE
)

pstrat_data <- readr::read_csv(
    system.file("extdata", "example", "data", "pstrat.csv", package = "shinymrp"),
    show_col_types = FALSE
)

library(shinymrp)

# Load sample data
sample_data <- raw_data

# Initialize the MRP workflow
workflow <- mrp_workflow()

### DATA PREPARATION

# Preprocess sample data
workflow$preprocess(
  sample_data,
  is_timevar = TRUE,
  is_aggregated = FALSE,
  special_case = NULL,
  family = "binomial"
)

# Link data to the ACS
# and obtain post-stratification data
workflow$link_acs(
  link_geo = "zip",
  acs_year = 2021
)


### DESCRIPTIVE STATISTICS

# Visualize demographic distribution of data
workflow$demo_bars(demo = "sex", file = "/Users/tntoan/Downloads/demo_bars_sex.png")

# Visualize geographic distribution of data
workflow$sample_size_map(file = "/Users/tntoan/Downloads/sample_size_map.html")

# Visualize outcome measure
workflow$outcome_plot(file = "/Users/tntoan/Downloads/outcome_plot.png")

workflow$outcome_map(file = "/Users/tntoan/Downloads/outcome_map.html")


### MODEL BUILDING

# Create new model objects
model <- workflow$create_model(
  model_spec = list(
    Intercept = list(
      Intercept = ""
    ),
    fixed = list(
      sex = "",
      race = ""
    ),
    varying = list(
      age = "",
      time = ""
    )
  )
)

# Run MCMC
model$fit(n_iter = 1000, n_chains = 2, seed = 123)

# p <- plot_ppc_timevar_subset(
#     yrep = model$ppc(),
#     raw = model$mrp()$input,
#     dates = model$plotdata()$dates,
#     metadata = model$metadata()
# )

workflow$pp_check(model)


# model$save("/Users/tntoan/Downloads/model.RDS")

# model <- qs::qread("/Users/tntoan/Downloads/model.RDS")

# # Estimates summary and diagnostics
# model$summary()

# # Sampling diagnostics
# model$diagnostics()

# # Posterior predictive check
# p <- workflow$pp_check(model) 


# ### VISUALIZE RESULTS
# workflow$estimate_plot(model, subgroup = "sex")
# workflow$estimate_map(model, geo = "county")
