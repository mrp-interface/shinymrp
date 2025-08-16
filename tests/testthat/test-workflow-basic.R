# Basic tests for MRPWorkflow class
# Simple, focused tests covering core functionality

library(testthat)
library(shinymrp)

# Test basic class structure and initialization
test_that("MRPWorkflow class exists and can be instantiated", {
  # Test that the class exists
  expect_true(exists("MRPWorkflow"))
  expect_s3_class(MRPWorkflow, "R6ClassGenerator")
  
  # Test basic instantiation
  workflow <- MRPWorkflow$new()
  expect_s3_class(workflow, "MRPWorkflow")
  expect_s3_class(workflow, "R6")
})

test_that("MRPWorkflow has expected public methods", {
  # Check that key methods exist
  expected_methods <- c(
    "initialize", "preprocess", "link_acs", "load_pstrat", "preprocessed_data"
  )
  
  actual_methods <- names(MRPWorkflow$public_methods)
  
  for (method in expected_methods) {
    expect_true(method %in% actual_methods, 
                info = paste("Method", method, "should exist"))
  }
})

test_that("MRPWorkflow factory function works", {
  # Mock cmdstanr::cmdstan_version to return a version
  mock_cmdstan_version <- function(error_on_NA = TRUE) {
    return("2.32.0")
  }
  
  with_mocked_bindings(
    cmdstan_version = mock_cmdstan_version,
    .package = "cmdstanr",
    {
      workflow <- mrp_workflow()
      expect_s3_class(workflow, "MRPWorkflow")
      expect_s3_class(workflow, "R6")
    }
  )
})

test_that("MRPWorkflow factory function fails without CmdStan", {
  # Mock cmdstanr::cmdstan_version to return NULL
  mock_cmdstan_version_null <- function(error_on_NA = TRUE) {
    return(NULL)
  }
  
  with_mocked_bindings(
    cmdstan_version = mock_cmdstan_version_null,
    .package = "cmdstanr",
    {
      expect_error(
        mrp_workflow(),
        "CmdStan is not installed"
      )
    }
  )
})

test_that("MRPWorkflow preprocessed_data method works", {
  workflow <- MRPWorkflow$new()
  
  # Before preprocessing, data should be NULL
  expect_null(workflow$preprocessed_data())
  
  # We can't easily test the private field setting without actual preprocessing,
  # so let's just test that the method exists and returns NULL when no data
  expect_null(workflow$preprocessed_data())
})