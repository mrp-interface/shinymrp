test_that("getter and setter methods work correctly", {

  workflow <- setup_test_workflow(
    metadata = list(
      is_timevar = FALSE,
      special_case = NULL,
      family = "binomial"
    ),
    constructor = ShinyMRPWorkflow$new
  )

  model <- setup_test_model(workflow, fit_model = FALSE)

  # Test methods for setting and getting model ID
  expect_null(model$get_id())
  model$assign_id()
  expect_type(model$get_id(), "character")

  # Test methods for setting and getting model name
  expect_null(model$name())
  model$set_name("Test Model")
  expect_equal(model$name(), "Test Model")

})