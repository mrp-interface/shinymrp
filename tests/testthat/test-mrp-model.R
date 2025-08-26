test_that("summary works correctly", {
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


