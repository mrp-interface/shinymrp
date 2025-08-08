devtools::load_all()

workflow <- mrp_workflow()

sample_data <- example_sample_data(
  is_timevar = TRUE,
  is_aggregated = TRUE,
  special_case = NULL,
  family = "binomial"
)

workflow$preprocess(
  sample_data,
  is_timevar = TRUE,
  is_aggregated = TRUE,
  special_case = NULL,
  family = "binomial"
)

workflow$link_acs(link_geo = "zip", acs_year = 2021)

model1 <- workflow$create_model(
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

model1$fit(n_iter = 500, n_chains = 2, seed = 123)
print("Fdafsdfasdfasfasfasfasfasfdasfasf")
workflow$estimate_plot(model1, group = "sex")