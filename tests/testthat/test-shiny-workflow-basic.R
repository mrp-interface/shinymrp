# Basic tests for ShinyMRPWorkflow class
# Simple, focused tests covering core functionality

library(testthat)
library(shinymrp)

# Test basic class structure and inheritance
test_that("ShinyMRPWorkflow class exists and inherits from MRPWorkflow", {
  # Test that the class exists
  expect_true(exists("ShinyMRPWorkflow"))
  expect_s3_class(ShinyMRPWorkflow, "R6ClassGenerator")
  
  # Test inheritance - $inherit contains the symbol, not the resolved class
  expect_true(!is.null(ShinyMRPWorkflow$inherit))
  expect_true(is.symbol(ShinyMRPWorkflow$inherit))
})

test_that("ShinyMRPWorkflow can be instantiated", {
  workflow <- ShinyMRPWorkflow$new()
  
  # Test class inheritance
  expect_s3_class(workflow, "ShinyMRPWorkflow")
  expect_s3_class(workflow, "MRPWorkflow")
  expect_s3_class(workflow, "R6")
  
  # Test that it's also an instance of MRPWorkflow
  expect_true(inherits(workflow, "MRPWorkflow"))
})

test_that("ShinyMRPWorkflow has expected public methods", {
  # Test that ShinyMRPWorkflow specific methods exist
  expected_own_methods <- c(
    "initialize", "create_model", "compare_models", 
    "check_metadata_exists", "check_data_exists", "check_mrp_exists",
    "link_data", "plot_data", "mrp_data"
  )
  
  actual_methods <- names(ShinyMRPWorkflow$public_methods)
  
  for (method in expected_own_methods) {
    expect_true(method %in% actual_methods, 
                info = paste("Method", method, "should exist in ShinyMRPWorkflow"))
  }
})

test_that("ShinyMRPWorkflow inherits parent methods", {
  workflow <- ShinyMRPWorkflow$new()
  
  # Test that inherited methods are available on instances
  inherited_methods <- c("preprocess", "link_acs", "load_pstrat", "preprocessed_data")
  
  for (method in inherited_methods) {
    expect_true(method %in% names(workflow),
                info = paste("Inherited method", method, "should be available on instances"))
  }
})

test_that("ShinyMRPWorkflow check methods work", {
  workflow <- ShinyMRPWorkflow$new()
  
  # Initially, nothing should exist
  expect_false(workflow$check_metadata_exists())
  expect_false(workflow$check_data_exists())
  expect_false(workflow$check_mrp_exists())
  
  # We can't easily test the private field setting without actual preprocessing,
  # so let's just test that the methods exist and return FALSE when no data
  expect_false(workflow$check_metadata_exists())
  expect_false(workflow$check_data_exists())
  expect_false(workflow$check_mrp_exists())
})

test_that("ShinyMRPWorkflow data access methods work", {
  workflow <- ShinyMRPWorkflow$new()
  
  # Initially should return NULL
  expect_null(workflow$link_data())
  expect_null(workflow$plot_data())
  expect_null(workflow$mrp_data())
  
  # We can't easily test the private field setting without actual preprocessing,
  # so let's just test that the methods exist and return NULL when no data
  expect_null(workflow$link_data())
  expect_null(workflow$plot_data())
  expect_null(workflow$mrp_data())
})

test_that("ShinyMRPWorkflow create_model requires MRP data", {
  workflow <- ShinyMRPWorkflow$new()
  
  model_spec <- list(
    Intercept = list(Intercept = "normal(0, 5)"),
    fixed = list(sex = "normal(0, 3)")
  )
  
  # Should error when no MRP data exists
  expect_error(
    workflow$create_model(model_spec),
    "Data for MRP is not available"
  )
})

test_that("ShinyMRPWorkflow compare_models requires multiple models", {
  workflow <- ShinyMRPWorkflow$new()
  
  # Mock model object
  mock_model <- list(
    metadata = function() list(family = "binomial"),
    loo = function() matrix(rnorm(100), nrow = 10, ncol = 10)
  )
  class(mock_model) <- c("ShinyMRPModel", "MRPModel", "R6")
  
  # Should error with less than 2 models
  expect_error(
    workflow$compare_models(list(mock_model)),
    "At least two models are required for comparison"
  )
  
  expect_error(
    workflow$compare_models(list()),
    "At least two models are required for comparison"
  )
})