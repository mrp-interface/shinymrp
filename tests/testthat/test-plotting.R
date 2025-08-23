# Test file for MRPWorkflow plotting methods
# Tests all plotting methods: demo_bars, covar_hist, sample_size_map,
# outcome_plot, outcome_map, estimate_plot, estimate_map, pp_check


test_that("demo_bars method works correctly for general data", {
  workflow <- setup_test_workflow(
    link_geo = "zip",
    is_timevar = FALSE,
    is_aggregated = TRUE,
    special_case = NULL,
    family = "binomial"
  )

  # Test with valid demographic variable
  for (demo in c("sex", "age", "race")) {
    p <- workflow$demo_bars(demo)
    expect_s3_class(p, "ggplot")
  }

  # Test error handling for invalid demographic variable
  expect_error(
    workflow$demo_bars("edu"),
    "Assertion on 'demo' failed"
  )

  # Test error handling for invalid demographic variable
  expect_error(
    workflow$demo_bars("invalid_demo"),
    "Assertion on 'demo' failed"
  )
  
  # Test file saving functionality (without actually saving)
  temp_file <- tempfile(fileext = ".png")
  p3 <- workflow$demo_bars("age", file = temp_file)
  expect_s3_class(p3, "ggplot")
  expect_true(file.exists(temp_file))
  unlink(temp_file)
})


test_that("demo_bars method works correctly for polling data", {
  workflow <- setup_test_workflow(
    link_geo = "state",
    is_timevar = FALSE,
    is_aggregated = TRUE,
    special_case = "poll",
    family = "binomial"
  )

  # Test with "edu"
  p <- workflow$demo_bars("edu")
  expect_s3_class(p, "ggplot")
})


test_that("covar_hist method works correctly for COVID data", {
  workflow <- setup_test_workflow(
    link_geo = "zip",
    is_timevar = TRUE,
    is_aggregated = TRUE,
    special_case = "covid",
    family = "binomial"
  )
  
  # Test with valid covariates
  for (covar in c("college", "poverty", "employment", "income", "urbanicity", "adi")) {
    p <- workflow$covar_hist(covar)
    expect_s3_class(p, "ggplot")
  }
  
  # Test error handling for invalid covariate
  expect_error(
    workflow$covar_hist("invalid_covar"),
    "Assertion on 'covar' failed"
  )

  # Test file saving functionality (without actually saving)
  temp_file <- tempfile(fileext = ".png")
  p3 <- workflow$covar_hist("college", file = temp_file)
  expect_s3_class(p3, "ggplot")
  expect_true(file.exists(temp_file))
  unlink(temp_file)
})

test_that("covar_hist fails appropriately for non-COVID data", {
  # Test error handling for non-COVID data
  workflow <- setup_test_workflow(
    link_geo = "zip",
    is_timevar = TRUE,
    is_aggregated = TRUE,
    special_case = NULL,
    family = "binomial"
  )
  
  expect_error(
    workflow$covar_hist("college"),
    "Covariate data is not available. This method is only available for COVID data."
  )
})

test_that("sample_size_map method works correctly", {
  workflow <- setup_test_workflow(
    link_geo = "zip",
    is_timevar = TRUE,
    is_aggregated = TRUE,
    special_case = NULL,
    family = "binomial"
  )
  
  # Test basic functionality
  hc <- workflow$sample_size_map()
  expect_s3_class(hc, "highchart")
  
  # Test file saving functionality
  temp_file <- tempfile(fileext = ".html")
  hc2 <- workflow$sample_size_map(file = temp_file)
  expect_s3_class(hc2, "highchart")
  expect_true(file.exists(temp_file))
  unlink(temp_file)
})


# Test outcome_plot method
test_that("outcome_plot method works correctly", {
  # For time-varying data
  workflow <- setup_test_workflow(
    link_geo = "zip",
    is_timevar = TRUE,
    is_aggregated = TRUE,
    special_case = NULL,
    family = "binomial"
  )
  
  # Test basic functionality
  p <- workflow$outcome_plot()
  expect_s3_class(p, "ggplot")

  # For cross-sectional data
  workflow <- setup_test_workflow(
    link_geo = "zip",
    is_timevar = FALSE,
    is_aggregated = TRUE,
    special_case = NULL,
    family = "binomial"
  )
  
  # Test basic functionality
  p <- workflow$outcome_plot()
  expect_s3_class(p, "ggplot")
  
  # Test file saving functionality
  temp_file <- tempfile(fileext = ".png")
  p2 <- workflow$outcome_plot(file = temp_file)
  expect_s3_class(p2, "ggplot")
  expect_true(file.exists(temp_file))
  unlink(temp_file)
})

# Test outcome_map method
test_that("outcome_map method works correctly", {
  # For time-varying data
  workflow <- setup_test_workflow(
    link_geo = "zip",
    is_timevar = TRUE,
    is_aggregated = TRUE,
    special_case = NULL,
    family = "binomial"
  )

  # Test basic functionality
  for (stype in c("max", "min")) {
    hc <- workflow$outcome_map(summary_type = stype)
    expect_s3_class(hc, "highchart")
  }

  # For cross-sectional data
  workflow <- setup_test_workflow(
    link_geo = "zip",
    is_timevar = FALSE,
    is_aggregated = TRUE,
    special_case = NULL,
    family = "binomial"
  )

  # Test basic functionality
  for (stype in c("max", "min")) {
    hc <- workflow$outcome_map(summary_type = stype)
    expect_s3_class(hc, "highchart")
  }

  # Test error handling for invalid summary_type
  expect_error(
    workflow$outcome_map(summary_type = NULL),
    "Assertion on 'summary_type' failed"
  )
  
  # Test file saving functionality
  temp_file <- tempfile(fileext = ".html")
  hc4 <- workflow$outcome_map(file = temp_file)
  expect_s3_class(hc4, "highchart")
  expect_true(file.exists(temp_file))
  unlink(temp_file)
})


test_that("map-generating methods fail without linking geography", {
  workflow <- setup_test_workflow(
    link_geo = NULL,
    is_timevar = TRUE,
    is_aggregated = TRUE,
    special_case = NULL,
    family = "binomial"
  )
  
  expect_error(
    workflow$sample_size_map(),
    "Linking geography is not available"
  )

  expect_error(
    workflow$outcome_map(),
    "Linking geography is not available"
  )
})

