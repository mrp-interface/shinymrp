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

# Test pp_check method
test_that("pp_check method works correctly", {
  capture.output({
    # For time-varying data
    workflow <- setup_test_workflow(
      link_geo = NULL,
      is_timevar = TRUE,
      is_aggregated = TRUE,
      special_case = NULL,
      family = "binomial"
    )
    model <- setup_test_model(workflow)
  
    # Test basic functionality
    p <- workflow$pp_check(model)
    expect_s3_class(p, "ggplot")

    # For time-varying data
    workflow <- setup_test_workflow(
      link_geo = NULL,
      is_timevar = FALSE,
      is_aggregated = TRUE,
      special_case = NULL,
      family = "binomial"
    )
    model <- setup_test_model(workflow)
  
    # Test basic functionality
    p <- workflow$pp_check(model)
    expect_s3_class(p, "ggplot")
    
    # Test file saving functionality
    temp_file <- tempfile(fileext = ".png")
    p <- workflow$pp_check(model, file = temp_file)
    expect_s3_class(p, "ggplot")
    expect_true(file.exists(temp_file))
    unlink(temp_file)

  }, type = "message")
})

# Test estimate_plot method
test_that("estimate_plot method works correctly", {
    ### For time-varying data
    workflow <- setup_test_workflow(
      link_geo = NULL,
      is_timevar = TRUE,
      is_aggregated = TRUE,
      special_case = NULL,
      family = "binomial"
    )
    model <- setup_test_model(workflow)
    
  capture.output({
    # Test overall estimates plot
    p <- workflow$estimate_plot(model)
    expect_s3_class(p, "ggplot")
    
    # Test different intervals
    p <- workflow$estimate_plot(model, interval = 0.9)
    expect_s3_class(p, "ggplot")

    p <- workflow$estimate_plot(model, interval = "1sd")
    expect_s3_class(p, "ggplot")

    # Test show_caption parameter
    p <- workflow$estimate_plot(model, show_caption = FALSE)
    expect_s3_class(p, "ggplot")

    # Test demographic group estimates
    for (group in c("sex", "race", "age")) {
      p <- workflow$estimate_plot(model, group = group)
      expect_s3_class(p, "ggplot")
    }
  }, type = "message")

    ### For cross-sectional data
    workflow <- setup_test_workflow(
      link_geo = NULL,
      is_timevar = FALSE,
      is_aggregated = TRUE,
      special_case = NULL,
      family = "binomial"
    )
    model <- setup_test_model(workflow)
    
  capture.output({
    # Test overall estimates plot
    p <- workflow$estimate_plot(model)
    expect_s3_class(p, "ggplot")
    
    # Test different intervals
    p <- workflow$estimate_plot(model, interval = 0.9)
    expect_s3_class(p, "ggplot")

    p <- workflow$estimate_plot(model, interval = "1sd")
    expect_s3_class(p, "ggplot")

    # Test show_caption parameter
    p <- workflow$estimate_plot(model, show_caption = FALSE)
    expect_s3_class(p, "ggplot")

    # Test demographic group estimates
    for (group in c("sex", "race", "age")) {
      p <- workflow$estimate_plot(model, group = group)
      expect_s3_class(p, "ggplot")
    }
  }, type = "message")
    
  # Test error handling for invalid group
  expect_error(
    workflow$estimate_plot(model, group = "invalid_group"),
    "Assertion on 'group' failed"
  )

  # Test file saving functionality
  temp_file <- tempfile(fileext = ".png")
  p <- workflow$estimate_plot(model, file = temp_file)
  expect_s3_class(p, "ggplot")
  expect_true(file.exists(temp_file))
  unlink(temp_file)
})

# Test estimate_map method
test_that("estimate_map method works correctly", {
  ### For time-varying data
  workflow <- setup_test_workflow(
    link_geo = "zip",
    is_timevar = TRUE,
    is_aggregated = TRUE,
    special_case = NULL,
    family = "binomial"
  )
  model <- setup_test_model(workflow)
  
  # Test basic functionality
  hc <- workflow$estimate_map(model)
  expect_s3_class(hc, "highchart")
  
  # Test with specific geography
  hc <- workflow$estimate_map(model, geo = "state")
  expect_s3_class(hc, "highchart")
  
  # Test with time index for time-varying data
  hc <- workflow$estimate_map(model, time_index = 2)
  expect_s3_class(hc, "highchart")
  
  # Test different intervals
  hc <- workflow$estimate_map(model, interval = "2sd")
  expect_s3_class(hc, "highchart")

  ### For cross-sectional data
  workflow <- setup_test_workflow(
    link_geo = "zip",
    is_timevar = FALSE,
    is_aggregated = TRUE,
    special_case = NULL,
    family = "binomial"
  )
  model <- setup_test_model(workflow)
  
  # Test basic functionality
  hc <- workflow$estimate_map(model)
  expect_s3_class(hc, "highchart")
  
  # Test with specific geography
  hc <- workflow$estimate_map(model, geo = "state")
  expect_s3_class(hc, "highchart")
  
  # Test with time index for time-varying data
  hc <- workflow$estimate_map(model, time_index = 3)
  expect_s3_class(hc, "highchart")
  
  # Test different intervals
  hc <- workflow$estimate_map(model, interval = "3sd")
  expect_s3_class(hc, "highchart")
  
  # Test file saving functionality
  temp_file <- tempfile(fileext = ".html")
  hc6 <- workflow$estimate_map(model, file = temp_file)
  expect_s3_class(hc6, "highchart")
  expect_true(file.exists(temp_file))
  unlink(temp_file)
})

test_that("methods fail appropriately without preprocessed data", {
  workflow <- mrp_workflow()
  
  # These methods should fail without preprocessing
  expect_error(
    workflow$demo_bars("age"),
    "Data for MRP is not available"
  )
  expect_error(
    workflow$covar_hist("college"),
    "Data for MRP is not available"
  )
  expect_error(
    workflow$sample_size_map(),
    "Data for MRP is not available"
  )
  expect_error(
    workflow$outcome_plot(),
    "Data for MRP is not available"
  )
  expect_error(
    workflow$outcome_map(),
    "Data for MRP is not available"
  )
})

# Test error handling for methods requiring fitted models
test_that("model-dependent methods fail appropriately without fitted models", {
  workflow <- setup_test_workflow(
    link_geo = "zip",
    is_timevar = FALSE,
    is_aggregated = TRUE,
    special_case = NULL,
    family = "binomial"
  )
  model <- setup_test_model(workflow, fit_model = FALSE)

  expect_error(
    workflow$pp_check(model),
    "Model has not been fitted"
  )
  expect_error(
    workflow$estimate_plot(model),
    "Model has not been fitted"
  )
  expect_error(
    workflow$estimate_map(model),
    "Model has not been fitted"
  )

})

test_that("map-generating methods fail without linking geography", {
  workflow <- setup_test_workflow(
    link_geo = NULL,
    is_timevar = TRUE,
    is_aggregated = TRUE,
    special_case = NULL,
    family = "binomial"
  )

  model <- setup_test_model(workflow)

  expect_error(
    workflow$sample_size_map(),
    "Linking geography is not available"
  )

  expect_error(
    workflow$outcome_map(),
    "Linking geography is not available"
  )

  expect_error(
    workflow$estimate_map(model),
    "Linking geography is not available"
  )
})