test_that("getter and setter methods work correctly", {
  workflow <- setup_test_workflow(
    metadata = list(
      is_timevar = FALSE,
      special_case = NULL,
      family = "binomial"
    )
  )

  expect_type(workflow$metadata(), "list")
  expect_s3_class(workflow$preprocessed_data(), "data.frame")
})


test_that("compare_models works correctly", {

  skip_on_cran()

  workflow <- setup_test_workflow(
    metadata = list(
      is_timevar = FALSE,
      special_case = NULL,
      family = "binomial"
    )
  )

  model1 <- setup_test_model(workflow,
    model_spec = list(
      intercept = list(
        intercept = "normal(0, 1)"
      ),
      fixed = list(
        race = "normal(0, 1)"
      )
    )
  )
  model2 <- setup_test_model(workflow,
    model_spec = list(
      intercept = list(
        intercept = "normal(0, 1)"
      ),
      varying = list(
        race = "normal(0, 1)"
      )
    )
  )

  # Compare different models
  expect_s3_class(
    workflow$compare_models(model1, model2),
    "data.frame"
  )

  # Compare the same models
  expect_s3_class(
    workflow$compare_models(model1, model1),
    "data.frame"
  )
})
