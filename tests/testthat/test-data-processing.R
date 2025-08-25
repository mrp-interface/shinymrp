expect_equal_saved_prep <- function(
  metadata,
  is_aggregated = TRUE,
  time_freq = NULL
) {

  workflow <- setup_test_workflow(
    metadata = metadata,
    is_aggregated = is_aggregated,
    time_freq = time_freq,
    link_geo = NULL,
    link = FALSE
  )

  suppressWarnings(
    saved <- paste0(
      "snapshots/data_processing/",
      make_hashed_filename(metadata, prefix = "prep")
    ) %>%
      testthat::test_path() %>%
      readr::read_csv(
        col_types = readr::cols(
          zip = readr::col_character(),
          county = readr::col_character(),
          state = readr::col_character(),
          date = readr::col_character(),
          .default = readr::col_guess()
        ),
        show_col_types = FALSE
      )
  )

  expect_equal(
    workflow$preprocessed_data(),
    saved,
    tolerance = 0.01,
    ignore_attr = TRUE
  )
}

test_that("data preprocessing works", {
  set.seed(123)

  # individual-level COVID data
  expect_equal_saved_prep(
    metadata = list(
      is_timevar = TRUE,
      special_case = "covid",
      family = "binomial"
    ),
    is_aggregated = FALSE,
    time_freq = "week"
  )

  # aggregated COVID data
  expect_equal_saved_prep(
    metadata = list(
      is_timevar = TRUE,
      special_case = "covid",
      family = "binomial"
    ),
    is_aggregated = TRUE,
    time_freq = NULL
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
    time_freq = "week"
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
    time_freq = NULL
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
    time_freq = "week"
  )

  # individual-level polling data
  expect_equal_saved_prep(
    metadata = list(
      is_timevar = FALSE,
      special_case = "poll",
      family = "binomial"
    ),
    is_aggregated = FALSE
  )

  # aggregated polling data
  expect_equal_saved_prep(
    metadata = list(
      is_timevar = FALSE,
      special_case = "poll",
      family = "binomial"
    ),
    is_aggregated = TRUE,
    time_freq = NULL
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
    time_freq =  NULL
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
    time_freq = NULL
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
    time_freq = NULL
  )

})
