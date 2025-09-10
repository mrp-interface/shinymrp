path <- "/Users/tntoan/Desktop/repos/shinymrp/vignettes/data/"
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

pstrat_data <- example_pstrat_data()
workflow$load_pstrat(pstrat_data, is_aggregated = TRUE)
qs::qsave(workflow, paste0(path, "workflow.qs"))

model1 <- workflow$create_model(
  intercept_prior = "normal(0, 4)",
  fixed = list(
    sex = "normal(0, 2)",
    race = "normal(0, 2)"
  ),
  varying = list(
    age = "",
    time = ""
  )
)

model1$fit(
  n_iter = 500,
  n_chains = 2,
  seed = 123,
  show_messages = FALSE,
  show_exceptions = FALSE
)

model1$summary()
model1$diagnostics()
model1$log_lik()
model1$ppc()
model1$poststratify()

model1$save(file = paste0(path, "model.qs"))

model2 <- workflow$create_model(
  intercept_prior = "normal(0, 4)",
  fixed = list(
    sex = "normal(0, 2)",
    race = "normal(0, 2)"
  ),
  varying = list(
    age = "",
    time = ""
  ),
  interaction = list(
    `age:time` = "normal(0, 1)",
    `race:time` = "normal(0, 1)",
    `sex:time` = "normal(0, 1)"
  )
)

model2$fit(
  n_iter = 500,
  n_chains = 2,
  seed = 123,
  show_messages = FALSE,
  show_exceptions = FALSE
)

compare_df <- workflow$compare_models(model1, model2)
readr::write_csv(compare_df, paste0(path, "loo.csv"))

