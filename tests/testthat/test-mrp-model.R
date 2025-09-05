test_that("getter methods work correctly", {
  workflow <- setup_test_workflow(
    metadata = list(
      is_timevar = FALSE,
      special_case = NULL,
      family = "binomial"
    )
  )

  model <- setup_test_model(workflow)

  expect_type(model$metadata(), "list")
  expect_type(model$mrp_data(), "list")
  expect_type(model$link_data(), "list")
  expect_type(model$plot_data(), "list")
  expect_type(model$model_spec(), "list")
  expect_type(model$formula(), "character")
  expect_type(model$stan_data(), "list")
  expect_type(model$stan_code(), "character")
  expect_s3_class(model$fit_object(), "CmdStanMCMC")

})


test_that("summary works correctly", {
  skip_on_cran()

  workflow <- setup_test_workflow(
    metadata = list(
      is_timevar = FALSE,
      special_case = NULL,
      family = "binomial"
    )
  )

  model <- setup_test_model(workflow)

  summary <- model$summary()

  expect_type(summary, "list")
  expect_named(summary, c("fixed", "varying", "other"))
})

test_that("diagnostics works correctly", {
  skip_on_cran()

  workflow <- setup_test_workflow(
    metadata = list(
      is_timevar = FALSE,
      special_case = NULL,
      family = "binomial"
    )
  )

  model <- setup_test_model(workflow)

  expect_s3_class(
    model$diagnostics(summarize = TRUE),
    "data.frame"
  )
  expect_type(
    model$diagnostics(summarize = FALSE),
    "list"
  )
})

test_that("save works correctly", {
  skip_on_cran()

  workflow <- setup_test_workflow(
    metadata = list(
      is_timevar = FALSE,
      special_case = NULL,
      family = "binomial"
    )
  )

  model <- setup_test_model(workflow)

  expect_save_file(model$save, ext = ".qs")
})


