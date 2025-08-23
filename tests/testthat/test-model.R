test_that("estimated parameters match saved values", {
  workflow <- setup_test_workflow(
    link_geo = "zip",
    is_timevar = FALSE,
    is_aggregated = TRUE,
    special_case = NULL,
    family = "binomial"
  )

  # fixed effects of binary variables
  expect_equal_saved(
    workflow, 
    list(
      intercept = list(
        intercept = "normal(0, 1)"
      ),
      fixed = list(
        sex = "normal(0, 1)"
      )
    )
  )

  # fixed effects of categorical variables
  expect_equal_saved(
    workflow,
    list(
      intercept = list(
        intercept = "normal(0, 1)"
      ),
      fixed = list(
        race = "normal(0, 1)"
      )
    )
  )

  # varying effects of categorical variables
  expect_equal_saved(
    workflow,
    list(
      intercept = list(
        intercept = "normal(0, 1)"
      ),
      varying = list(
        race = "normal(0, 1)"
      )
    )
  )

  # varying effects of categorical variables
  # and interactions
  expect_equal_saved(
    workflow,
    list(
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
    )
  )

  # fixed effects of binary variables,
  # varying effects of categorical variables,
  # and interactions
  expect_equal_saved(
    workflow,
    list(
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
    )
  )

  # fixed effects of categorical variables,
  # varying effects of categorical variables,
  # and interactions
  expect_equal_saved(
    workflow,
    list(
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
    )
  )

})
