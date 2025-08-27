test_that("getter and setter methods work correctly", {  
  workflow <- ShinyMRPWorkflow$new()

  expect_false(workflow$check_metadata_exists())
  expect_false(workflow$check_prep_data_exists())
  expect_false(workflow$check_mrp_exists())

  data <- example_sample_data(
    is_timevar = FALSE,
    is_aggregated = TRUE,
    special_case = NULL,
    family = "binomial"
  )

  capture.output({
    workflow$preprocess(
      data,
      is_timevar = FALSE,
      is_aggregated = TRUE,
      special_case = NULL,
      family = "binomial",    
      time_freq = NULL
    )
  }, type = "message")

  expect_true(workflow$check_metadata_exists())
  expect_true(workflow$check_prep_data_exists())

  capture.output({
    workflow$link_acs(link_geo = "zip")
  }, type = "message")

  expect_true(workflow$check_mrp_exists())

  expect_type(workflow$mrp_data(), "list")
  expect_type(workflow$link_data(), "list")
  expect_type(workflow$plot_data(), "list")
})

test_that("methods that returns DT::datatable() work", {
  workflow <- setup_test_workflow(
    metadata = list(
      is_timevar = TRUE,
      special_case = "covid",
      family = "binomial"
    ),
    link_geo = "zip",
    constructor = ShinyMRPWorkflow$new
  )

  expect_s3_class(workflow$sample_size_table(), "datatables")
  for (covar in .const()$vars$covar) {
    expect_s3_class(workflow$covar_table(covar), "datatables")
  }
})

test_that("estimate_plot_geo works correctly", {
  workflow <- ShinyMRPWorkflow$new()

  expect_s3_class(
    workflow$estimate_plot_geo(
      example_model(is_timevar = TRUE),
      "county"
    ),
    "ggplot"
  )

  expect_s3_class(
    workflow$estimate_plot_geo(
      example_model(is_timevar = FALSE),
      "state"
    ),
    "ggplot"
  )

  # Error for invalid geography
  expect_error(
    workflow$estimate_plot_geo(model, "invalid"),
    "Assertion on 'geo' failed"
  )
})

test_that("estimate_map works correctly", {
  workflow <- ShinyMRPWorkflow$new()

  expect_s3_class(
    workflow$estimate_map(
      example_model(is_timevar = TRUE),
      "county"
    ),
    "highchart"
  )

  expect_s3_class(
    workflow$estimate_map(
      example_model(is_timevar = FALSE),
      "state"
    ),
    "highchart"
  )

  # Error for invalid geography
  expect_error(
    workflow$estimate_map(model, "invalid"),
    "Assertion on 'geo' failed"
  )
})

test_that("compare_models works correctly", {
  workflow <- setup_test_workflow(
    metadata = list(
      is_timevar = FALSE,
      special_case = NULL,
      family = "binomial"
    ),
    constructor = ShinyMRPWorkflow$new
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
  expect_type(
    workflow$compare_models(list(model1, model2)),
    "list"
  )

  # Compare the same models
  expect_type(
    workflow$compare_models(list(model1, model1)),
    "list"
  )
})