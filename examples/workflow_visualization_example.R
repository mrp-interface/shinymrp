# Example: Using MRPWorkflow Visualization Methods
# This script demonstrates how to use the new visualization methods
# added to the MRPWorkflow class

library(shinymrp)

# Create a new workflow instance
workflow <- mrp_workflow()

# Example 1: Using COVID data
# Load and preprocess COVID data
covid_data <- read.csv(system.file("extdata/example/data/covid_binomial_prep.csv", 
                                   package = "shinymrp"))

# Preprocess the data
workflow$preprocess(
  data = covid_data,
  is_timevar = TRUE,
  is_aggregated = FALSE,
  special_case = "covid",
  family = "binomial"
)

# Link to ACS data (this uses pre-loaded COVID data)
workflow$link_acs()

# Now you can use the visualization methods:

# 1. Demographic comparison bar plots
demo_plot <- workflow$demo_bars(separate = TRUE)
print(demo_plot)

# 2. Geographic covariate histogram (only for COVID data)
edu_hist <- workflow$cover_hist(covariate = "edu")
print(edu_hist)

# 3. Sample size map
sample_map <- workflow$sample_size_map(geo = "county")
print(sample_map)

# 4. Outcome line plot (for time-varying data)
outcome_line <- workflow$outcome_line()
print(outcome_line)

# 5. Outcome map showing geographic distribution
outcome_map <- workflow$outcome_map(geo = "county", summary_type = "max")
print(outcome_map)

# Example 2: Using cross-sectional data
# Create a new workflow for cross-sectional data
workflow2 <- mrp_workflow()

# Load cross-sectional data
cross_data <- read.csv(system.file("extdata/example/data/crosssectional_binomial_prep.csv", 
                                   package = "shinymrp"))

# Preprocess the data
workflow2$preprocess(
  data = cross_data,
  is_timevar = FALSE,
  is_aggregated = FALSE,
  special_case = NULL,
  family = "binomial"
)

# Link to ACS data
workflow2$link_acs(acs_year = 2023)

# Visualization methods for cross-sectional data:

# 1. Demographic comparison
demo_plot2 <- workflow2$demo_bars(separate = FALSE)
print(demo_plot2)

# 2. Sample size map
sample_map2 <- workflow2$sample_size_map(geo = "state")
print(sample_map2)

# 3. Outcome map for cross-sectional data
outcome_map2 <- workflow2$outcome_map(geo = "state")
print(outcome_map2)

# Note: For outcome_point, estimate_line, and estimate_point methods,
# you would need to first fit a model and obtain estimates.
# Here's how you would use them with model estimates:

# Example with model estimates (pseudo-code):
# estimates <- your_model_estimates_dataframe
# 
# For time-varying data:
# estimate_plot <- workflow$estimate_line(estimates, subgroup = "sex")
# 
# For cross-sectional data:
# outcome_plot <- workflow2$outcome_point(estimates)
# estimate_plot <- workflow2$estimate_point(estimates, subgroup = "race")

print("Visualization methods successfully added to MRPWorkflow class!")
print("Available methods:")
print("- demo_bars(): Demographic comparison bar plots")
print("- cover_hist(): Geographic covariate histograms (COVID only)")
print("- sample_size_map(): Sample size distribution maps")
print("- outcome_line(): Outcome time series (time-varying data)")
print("- outcome_point(): Outcome comparison (cross-sectional data)")
print("- outcome_map(): Outcome geographic maps")
print("- estimate_line(): Estimate time series (time-varying data)")
print("- estimate_point(): Estimate comparison (cross-sectional data)")