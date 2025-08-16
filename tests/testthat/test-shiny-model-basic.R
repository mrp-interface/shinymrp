# Basic tests for ShinyMRPModel class
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

# Test basic class structure and inheritance
test_that("ShinyMRPModel class exists and inherits from MRPModel", {
  # Test that the class exists
  expect_true(exists("ShinyMRPModel"))
  expect_s3_class(ShinyMRPModel, "R6ClassGenerator")
  
  # Test inheritance - $inherit contains the symbol, not the resolved class
  expect_true(!is.null(ShinyMRPModel$inherit))
  expect_true(is.symbol(ShinyMRPModel$inherit))
})

test_that("ShinyMRPModel can be instantiated", {
  test_data <- create_test_data()
  
  model <- ShinyMRPModel$new(
    model_spec = test_data$model_spec,
    mrp_data = test_data$mrp_data,
    metadata = test_data$metadata,
    link_data = test_data$link_data,
    plot_data = test_data$plot_data
  )
  
  # Test class inheritance
  expect_s3_class(model, "ShinyMRPModel")
  expect_s3_class(model, "MRPModel")
  expect_s3_class(model, "R6")
  
  # Test that it's also an instance of MRPModel
  expect_true(inherits(model, "MRPModel"))
})

test_that("ShinyMRPModel has expected public methods", {
  # ShinyMRPModel specific methods
  expected_own_methods <- c("initialize", "assign_id", "get_id", "set_name", "name")
  actual_methods <- names(ShinyMRPModel$public_methods)
  
  for (method in expected_own_methods) {
    expect_true(method %in% actual_methods,
                info = paste("Method", method, "should exist in ShinyMRPModel"))
  }
})

test_that("ShinyMRPModel inherits parent methods", {
  test_data <- create_test_data()
  
  model <- ShinyMRPModel$new(
    model_spec = test_data$model_spec,
    mrp_data = test_data$mrp_data,
    metadata = test_data$metadata,
    link_data = test_data$link_data,
    plot_data = test_data$plot_data
  )
  
  # Test that inherited methods are available
  inherited_methods <- c("model_spec", "formula", "mrp_data", "metadata", "link_data", "plot_data")
  for (method in inherited_methods) {
    expect_true(method %in% names(model),
                info = paste("Inherited method", method, "should be available on instances"))
  }
  
  # Test that parent methods work
  # Note: model_spec gets transformed during initialization, so we test structure instead
  returned_spec <- model$model_spec()
  expect_type(returned_spec, "list")
  expect_true("Intercept" %in% names(returned_spec))
  
  expect_equal(model$mrp_data(), test_data$mrp_data)
  expect_equal(model$metadata(), test_data$metadata)
  expect_equal(model$link_data(), test_data$link_data)
  expect_equal(model$plot_data(), test_data$plot_data)
})

test_that("ShinyMRPModel ID management works", {
  test_data <- create_test_data()
  
  model <- ShinyMRPModel$new(
    model_spec = test_data$model_spec,
    mrp_data = test_data$mrp_data,
    metadata = test_data$metadata,
    link_data = test_data$link_data,
    plot_data = test_data$plot_data
  )
  
  # Initially, ID should be NULL
  expect_null(model$get_id())
  
  # After assigning ID
  model$assign_id()
  expect_true(!is.null(model$get_id()))
  expect_type(model$get_id(), "character")
  
  # Test getting specific UI element IDs
  expect_no_error(model$get_id("main"))
  expect_no_error(model$get_id("tab"))
  expect_no_error(model$get_id("title"))
  
  # All should return character strings
  expect_type(model$get_id("main"), "character")
  expect_type(model$get_id("tab"), "character")
  expect_type(model$get_id("title"), "character")
})

test_that("ShinyMRPModel name management works", {
  test_data <- create_test_data()
  
  model <- ShinyMRPModel$new(
    model_spec = test_data$model_spec,
    mrp_data = test_data$mrp_data,
    metadata = test_data$metadata,
    link_data = test_data$link_data,
    plot_data = test_data$plot_data
  )
  
  # Initially, name should be NULL
  expect_null(model$name())
  
  # After setting name
  model$set_name("Test Model")
  expect_equal(model$name(), "Test Model")
  
  # Can change name
  model$set_name("Updated Model")
  expect_equal(model$name(), "Updated Model")
})

test_that("ShinyMRPModel ID generation creates proper structure", {
  test_data <- create_test_data()
  
  model <- ShinyMRPModel$new(
    model_spec = test_data$model_spec,
    mrp_data = test_data$mrp_data,
    metadata = test_data$metadata,
    link_data = test_data$link_data,
    plot_data = test_data$plot_data
  )
  
  model$assign_id()
  
  # Test that different UI elements have different IDs
  main_id <- model$get_id("main")
  tab_id <- model$get_id("tab")
  title_id <- model$get_id("title")
  
  expect_true(main_id != tab_id)
  expect_true(main_id != title_id)
  expect_true(tab_id != title_id)
  
  # Test that prefixed IDs contain the main ID
  expect_true(grepl(main_id, tab_id))
  expect_true(grepl(main_id, title_id))
})

test_that("Multiple ShinyMRPModel instances are independent", {
  test_data <- create_test_data()
  
  model1 <- ShinyMRPModel$new(
    model_spec = test_data$model_spec,
    mrp_data = test_data$mrp_data,
    metadata = test_data$metadata,
    link_data = test_data$link_data,
    plot_data = test_data$plot_data
  )
  
  model2 <- ShinyMRPModel$new(
    model_spec = test_data$model_spec,
    mrp_data = test_data$mrp_data,
    metadata = test_data$metadata,
    link_data = test_data$link_data,
    plot_data = test_data$plot_data
  )
  
  # Assign different names and IDs
  model1$set_name("Model 1")
  model2$set_name("Model 2")
  model1$assign_id()
  model2$assign_id()
  
  # Should have different names and IDs
  expect_equal(model1$name(), "Model 1")
  expect_equal(model2$name(), "Model 2")
  expect_true(model1$get_id() != model2$get_id())
})