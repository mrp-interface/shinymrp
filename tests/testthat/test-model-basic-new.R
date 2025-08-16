# Basic tests for MRPModel class
# Simple, focused tests covering core functionality

library(testthat)
library(shinymrp)

# Helper function to create minimal test data
create_test_data <- function() {
  list(
    model_spec = list(
      Intercept = list(Intercept = "normal(0, 5)"),
      fixed = list(sex = "normal(0, 3)")
    ),
    mrp_data = list(
      input = data.frame(
        positive = c(1, 2, 3),
        total = c(5, 6, 7),
        sex = c("Male", "Female", "Male"),
        stringsAsFactors = FALSE
      ),
      new = data.frame(
        sex = c("Male", "Female"),
        pop = c(100, 120),
        stringsAsFactors = FALSE
      ),
      levels = list(sex = c("Male", "Female"))
    ),
    metadata = list(
      family = "binomial",
      is_timevar = FALSE,
      variables = c("sex"),
      outcome_var = "positive",
      total_var = "total"
    ),
    link_data = list(
      geography = "state",
      acs_year = 2020
    ),
    plot_data = list(
      dates = NULL,
      geojson = list()
    )
  )
}

# Test basic class structure and initialization
test_that("MRPModel class exists and can be instantiated", {
  # Test that the class exists
  expect_true(exists("MRPModel"))
  expect_s3_class(MRPModel, "R6ClassGenerator")
  
  # Test basic instantiation with minimal data
  test_data <- create_test_data()
  
  model <- MRPModel$new(
    model_spec = test_data$model_spec,
    mrp_data = test_data$mrp_data,
    metadata = test_data$metadata,
    link_data = test_data$link_data,
    plot_data = test_data$plot_data
  )
  
  expect_s3_class(model, "MRPModel")
  expect_s3_class(model, "R6")
})

test_that("MRPModel has expected public methods", {
  # Check that key methods exist
  expected_methods <- c(
    "initialize", "model_spec", "formula", "mrp_data", "metadata",
    "plot_data", "link_data", "fit", "check_fit_exists", "save"
  )
  
  actual_methods <- names(MRPModel$public_methods)
  
  for (method in expected_methods) {
    expect_true(method %in% actual_methods, 
                info = paste("Method", method, "should exist"))
  }
})

test_that("MRPModel data access methods work", {
  test_data <- create_test_data()
  
  model <- MRPModel$new(
    model_spec = test_data$model_spec,
    mrp_data = test_data$mrp_data,
    metadata = test_data$metadata,
    link_data = test_data$link_data,
    plot_data = test_data$plot_data
  )
  
  # Test data access methods
  # Note: model_spec gets transformed during initialization, so we test structure instead
  returned_spec <- model$model_spec()
  expect_type(returned_spec, "list")
  expect_true("Intercept" %in% names(returned_spec))
  
  expect_equal(model$mrp_data(), test_data$mrp_data)
  expect_equal(model$metadata(), test_data$metadata)
  expect_equal(model$link_data(), test_data$link_data)
  expect_equal(model$plot_data(), test_data$plot_data)
  
  # Test formula generation
  formula_result <- model$formula()
  expect_type(formula_result, "character")
  expect_true(nchar(formula_result) > 0)
})

test_that("MRPModel fit status checking works", {
  test_data <- create_test_data()
  
  model <- MRPModel$new(
    model_spec = test_data$model_spec,
    mrp_data = test_data$mrp_data,
    metadata = test_data$metadata,
    link_data = test_data$link_data,
    plot_data = test_data$plot_data
  )
  
  # Before fitting, should return FALSE
  expect_false(model$check_fit_exists())
  expect_false(model$check_estimate_exists())
  
  # Stan data and code should be NULL before fitting
  expect_null(model$stan_data())
  expect_null(model$stan_code())
})

test_that("MRPModel methods requiring fitted model error appropriately", {
  test_data <- create_test_data()
  
  model <- MRPModel$new(
    model_spec = test_data$model_spec,
    mrp_data = test_data$mrp_data,
    metadata = test_data$metadata,
    link_data = test_data$link_data,
    plot_data = test_data$plot_data
  )
  
  # Methods that require fitted model should error before fitting
  expect_error(model$code(), "Model has not been fitted yet")
  expect_error(model$summary(), "Model has not been fitted yet")
  expect_error(model$diagnostics(), "Model has not been fitted yet")
  expect_error(model$ppc(), "Model has not been fitted yet")
  expect_error(model$loo(), "Model has not been fitted yet")
  expect_error(model$poststratify(), "Model has not been fitted yet")
})

test_that("MRPModel save method works", {
  test_data <- create_test_data()
  
  model <- MRPModel$new(
    model_spec = test_data$model_spec,
    mrp_data = test_data$mrp_data,
    metadata = test_data$metadata,
    link_data = test_data$link_data,
    plot_data = test_data$plot_data
  )
  
  # Should work even before fitting (saves unfitted model)
  temp_file <- tempfile(fileext = ".qs")
  expect_no_error(model$save(temp_file))
  expect_true(file.exists(temp_file))
  
  # Clean up
  unlink(temp_file)
})