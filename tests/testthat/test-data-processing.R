expect_equal_saved_prep <- function(
  metadata,
  is_aggregated = TRUE,
  time_freq = NULL,
  file = NULL
) {

  workflow <- setup_test_workflow(
    metadata = metadata,
    is_aggregated = is_aggregated,
    time_freq = time_freq,
    link_geo = NULL,
    link = FALSE
  )

  saved <- paste0(
    "snapshots/data_processing/",
    file
  ) %>%
    testthat::test_path() %>%
    read_saved_csv()

  expect_equal(
    workflow$preprocessed_data(),
    saved,
    tolerance = 0.01,
    ignore_attr = TRUE
  )
}

test_that("prepprocess is consistent", {
  skip_on_cran()

  set.seed(123)

  # individual-level COVID data
  expect_equal_saved_prep(
    metadata = list(
      is_timevar = TRUE,
      special_case = "covid",
      family = "binomial"
    ),
    is_aggregated = FALSE,
    time_freq = "week",
    file = "covid_binomial_indiv.csv"
  )

  # aggregated COVID data
  expect_equal_saved_prep(
    metadata = list(
      is_timevar = TRUE,
      special_case = "covid",
      family = "binomial"
    ),
    is_aggregated = TRUE,
    time_freq = NULL,
    file = "covid_binomial_agg.csv"
  )

  # individual-level general time-varying data
  # with binary outcome
  expect_equal_saved_prep(
    metadata = list(
      is_timevar = TRUE,
      special_case = NULL,
      family = "binomial"
    ),
    is_aggregated = FALSE,
    time_freq = "week",
    file = "timevar_binomial_indiv.csv"
  )

  # aggregated general time-varying data
  # with binary outcome
  expect_equal_saved_prep(
    metadata = list(
      is_timevar = TRUE,
      special_case = NULL,
      family = "binomial"
    ),
    is_aggregated = TRUE,
    time_freq = NULL,
    file = "timevar_binomial_agg.csv"
  )

  # individual-level general time-varying data
  # with continuous outcome
  expect_equal_saved_prep(
    metadata = list(
      is_timevar = TRUE,
      special_case = NULL,
      family = "normal"
    ),
    is_aggregated = FALSE,
    time_freq = "week",
    file = "timevar_normal_indiv.csv"
  )

  # individual-level polling data
  expect_equal_saved_prep(
    metadata = list(
      is_timevar = FALSE,
      special_case = "poll",
      family = "binomial"
    ),
    is_aggregated = FALSE,
    time_freq = NULL,
    file = "poll_binomial_indiv.csv"
  )

  # aggregated polling data
  expect_equal_saved_prep(
    metadata = list(
      is_timevar = FALSE,
      special_case = "poll",
      family = "binomial"
    ),
    is_aggregated = TRUE,
    time_freq = NULL,
    file = "poll_binomial_agg.csv"
  )

  # individual-level general cross-sectional data
  # with binary outcome
  expect_equal_saved_prep(
    metadata = list(
      is_timevar = FALSE,
      special_case = NULL,
      family = "binomial"
    ),
    is_aggregated = FALSE,
    time_freq =  NULL,
    file = "crosssec_binomial_indiv.csv"
  )

  # aggregated general cross-sectional data
  # with binary outcome
  expect_equal_saved_prep(
    metadata = list(
      is_timevar = FALSE,
      special_case = NULL,
      family = "binomial"
    ),
    is_aggregated = TRUE,
    time_freq = NULL,
    file = "crosssec_binomial_agg.csv"
  )

  # individual-level general cross-sectional data
  # with continuous outcome
  expect_equal_saved_prep(
    metadata = list(
      is_timevar = FALSE,
      special_case = NULL,
      family = "normal"
    ),
    is_aggregated = FALSE,
    time_freq = NULL,
    file = "crosssec_normal_indiv.csv"
  )

})

test_that("link_acs works correctly with all linking geographies", {
  skip_on_cran()

  # No linking geography
  expect_no_error(
    setup_test_workflow(
      metadata = list(
        is_timevar = FALSE,
        special_case = NULL,
        family = "binomial"
      ),
      is_aggregated = TRUE,
      time_freq = NULL,
      link_geo = NULL,
      link = TRUE
    )
  )

  # Linking through zip
  expect_no_error(
    setup_test_workflow(
      metadata = list(
        is_timevar = FALSE,
        special_case = NULL,
        family = "binomial"
      ),
      is_aggregated = TRUE,
      time_freq = NULL,
      link_geo = "zip",
      link = TRUE
    )
  )

  # Linking through county
  expect_no_error(
    setup_test_workflow(
      metadata = list(
        is_timevar = FALSE,
        special_case = NULL,
        family = "binomial"
      ),
      is_aggregated = TRUE,
      time_freq = NULL,
      link_geo = "county",
      link = TRUE
    )
  )

  # Linking through state
  expect_no_error(
    setup_test_workflow(
      metadata = list(
        is_timevar = FALSE,
        special_case = NULL,
        family = "binomial"
      ),
      is_aggregated = TRUE,
      time_freq = NULL,
      link_geo = "state",
      link = TRUE
    )
  )

})


test_that("load_pstrat works correctly", {
  skip_on_cran()

  pstrat_data <- example_pstrat_data()

  # For general time-varying data
  # with binary outcome
  workflow <- setup_test_workflow(
    metadata = list(
      is_timevar = TRUE,
      special_case = NULL,
      family = "binomial"
    ),
    is_aggregated = TRUE,
    time_freq = NULL,
    link = FALSE
  )

  capture.output({
    workflow$load_pstrat(pstrat_data)
  }, type = "message")
  expect_no_error(workflow$demo_bars("sex"))


  # For general time-varying data
  # with continuous outcome
  workflow <- setup_test_workflow(
    metadata = list(
      is_timevar = TRUE,
      special_case = NULL,
      family = "normal"
    ),
    is_aggregated = FALSE,
    time_freq = "week",
    link = FALSE
  )

  capture.output({
    workflow$load_pstrat(pstrat_data)
  }, type = "message")
  expect_no_error(workflow$demo_bars("sex"))

  # For general cross-sectional data
  # with binary outcome
  workflow <- setup_test_workflow(
    metadata = list(
      is_timevar = FALSE,
      special_case = NULL,
      family = "binomial"
    ),
    is_aggregated = TRUE,
    time_freq = NULL,
    link = FALSE
  )

  capture.output({
    workflow$load_pstrat(pstrat_data)
  }, type = "message")
  expect_no_error(workflow$demo_bars("sex"))


  # For general cross-sectional data
  # with continuous outcome
  workflow <- setup_test_workflow(
    metadata = list(
      is_timevar = FALSE,
      special_case = NULL,
      family = "normal"
    ),
    is_aggregated = FALSE,
    time_freq = NULL,
    link = FALSE
  )

  capture.output({
    workflow$load_pstrat(pstrat_data)
  }, type = "message")
  expect_no_error(workflow$demo_bars("sex"))


  # For COVID data
  workflow <- setup_test_workflow(
    metadata = list(
      is_timevar = TRUE,
      special_case = "covid",
      family = "binomial"
    ),
    is_aggregated = TRUE,
    time_freq = NULL,
    link = FALSE
  )

  expect_error(
    workflow$load_pstrat(pstrat_data),
    "Custom poststratification data is not supported for special cases"
  )

  # For polling data
  workflow <- setup_test_workflow(
    metadata = list(
      is_timevar = FALSE,
      special_case = "poll",
      family = "binomial"
    ),
    link = FALSE
  )

  expect_error(
    workflow$load_pstrat(pstrat_data),
    "Custom poststratification data is not supported for special cases"
  )
})

test_that(".impute is consistent", {
  skip_on_cran()

  set.seed(123)

  n <- 20
  cols <- c("sex", "race", "age")

  data <- example_sample_data(
    is_timevar = FALSE,
    is_aggregated = FALSE,
    special_case = NULL,
    family = "binomial"
  ) %>%
    mutate(
      across(all_of(cols),
      ~ replace(., row_number() <= n, NA))
    )

  workflow <- mrp_workflow()

  capture.output(
    workflow$preprocess(
      data,
      is_timevar = FALSE,
      is_aggregated = FALSE,
      special_case = NULL,
      family = "binomial"
    )
  , type = "message")

  saved <- testthat::test_path("snapshots/data_processing/impute.csv") %>%
    read_saved_csv()

  expect_equal(
    workflow$preprocessed_data(),
    saved
  )

})