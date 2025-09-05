get_test_data <- function(fit) {
  variables <- setdiff(fit$metadata()$variables, "lp__")
  fit$summary(variables = variables) %>%
    select("mean", "sd")
}

expect_equal_saved_estimates <- function(workflow, model_spec, file) {
  model <- workflow$create_model(
    intercept_prior = model_spec$intercept$intercept,
    fixed = model_spec$fixed,
    varying = model_spec$varying,
    interaction = model_spec$interaction
  )

  model$fit(
    n_iter = 1000,
    n_chains = 2,
    show_messages = FALSE,
    show_exceptions = FALSE,
    diagnostics = NULL
  )

  saved <- paste0(
    "snapshots/model_fitting/",
    file
  ) %>%
    testthat::test_path() %>%
    readr::read_csv(show_col_types = FALSE)


  expect_equal(
    get_test_data(model$fit_object()),
    saved,
    tolerance = 0.1,
    ignore_attr = TRUE
  )
}

test_that("estimated parameters match saved values", {
  workflow <- setup_test_workflow(
    metadata = list(
      is_timevar = FALSE,
      special_case = NULL,
      family = "binomial"
    ),
    link_geo = "zip"
  )

  # fixed effects of binary variables
  expect_equal_saved_estimates(
    workflow, 
    model_spec = list(
      intercept = list(
        intercept = "normal(0, 1)"
      ),
      fixed = list(
        sex = "normal(0, 1)"
      )
    ),
    file = "fix-bin.csv"
  )

  # fixed effects of categorical variables
  expect_equal_saved_estimates(
    workflow,
    model_spec = list(
      intercept = list(
        intercept = "normal(0, 1)"
      ),
      fixed = list(
        race = "normal(0, 1)"
      )
    ),
    file = "fix-cat.csv"
  )

  # varying effects of categorical variables
  expect_equal_saved_estimates(
    workflow,
    model_spec = list(
      intercept = list(
        intercept = "normal(0, 1)"
      ),
      varying = list(
        race = "normal(0, 1)"
      )
    ),
    file = "var-cat.csv"
  )

  # varying effects of categorical variables
  # and interactions
  expect_equal_saved_estimates(
    workflow,
    model_spec = list(
      intercept = list(
        intercept = "normal(0, 1)"
      ),
      varying = list(
        race = "normal(0, 1)",
        age = "normal(0, 1)"
      ),
      interaction = list(
        `race:age` = "normal(0, 1)"
      )
    ),
    file = "var-cat_int.csv"
  )

  # fixed effects of binary variables,
  # varying effects of categorical variables,
  # and interactions
  expect_equal_saved_estimates(
    workflow,
    model_spec = list(
      intercept = list(
        intercept = "normal(0, 1)"
      ),
      fixed = list(
        sex = "normal(0, 1)"
      ),
      varying = list(
        race = "normal(0, 1)"
      ),
      interaction = list(
        `sex:race` = "normal(0, 1)"
      )
    ),
    file = "fix-bin_var-cat_int.csv"
  )

  # fixed effects of categorical variables,
  # varying effects of categorical variables,
  # and interactions
  expect_equal_saved_estimates(
    workflow,
    model_spec = list(
      intercept = list(
        intercept = "normal(0, 1)"
      ),
      fixed = list(
        age = "normal(0, 1)"
      ),
      varying = list(
        race = "normal(0, 1)"
      ),
      interaction = list(
        `sex:race` = "normal(0, 1)"
      )
    ),
    file = "fix-cat_var-cat_int.csv"
  )

})
