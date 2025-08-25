test_that("mrp_workflow returns valid MRPWorkflow object", {  
  # Call the function
  workflow <- mrp_workflow()
  
  # Test return type
  expect_s3_class(workflow, "MRPWorkflow")
  expect_s3_class(workflow, "R6")
  
  # Test that it's a new instance
  expect_true(R6::is.R6(workflow))
})

test_that("mrp_workflow creates independent instances", {  
  # Create two instances
  workflow1 <- mrp_workflow()
  workflow2 <- mrp_workflow()
  
  # They should be different objects
  expect_false(identical(workflow1, workflow2))
  
  # But same class
  expect_identical(class(workflow1), class(workflow2))
})

test_that("mrp_workflow fails gracefully without CmdStan", {
  # Mock cmdstanr::cmdstan_version to return NULL
  local_mocked_bindings(
    cmdstan_version = function(error_on_NA = FALSE) NULL,
    .package = "cmdstanr"
  )
  
  expect_error(
    mrp_workflow(),
    "CmdStan is not installed.*install_cmdstan"
  )
})
