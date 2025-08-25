read_qs_github <- function(url) {
  # Create temporary file
  temp_file <- tempfile(fileext = ".qs")
  on.exit(unlink(temp_file))
  
  # Download and read
  utils::download.file(url, destfile = temp_file, mode = "wb", quiet = TRUE)
  qs::qread(temp_file)
}

test_that("example_sample_data retrieves the correct files", {
  expect_equal(
    example_sample_data(
      is_timevar = TRUE,
      is_aggregated = FALSE,
      special_case = "covid",
      family = "binomial"
    ),
    readr::read_csv("https://raw.githubusercontent.com/mrp-interface/shinymrp-data/refs/heads/main/example/data/covid_binomial_raw.csv", show_col_types = FALSE)
  )

  expect_equal(
    example_sample_data(
      is_timevar = TRUE,
      is_aggregated = TRUE,
      special_case = "covid",
      family = "binomial"
    ),
    readr::read_csv("https://raw.githubusercontent.com/mrp-interface/shinymrp-data/refs/heads/main/example/data/covid_binomial_prep.csv", show_col_types = FALSE)
  )

  expect_equal(
    example_sample_data(
      is_timevar = TRUE,
      is_aggregated = FALSE,
      special_case = NULL,
      family = "binomial"
    ),
    readr::read_csv("https://raw.githubusercontent.com/mrp-interface/shinymrp-data/refs/heads/main/example/data/timevarying_binomial_raw.csv", show_col_types = FALSE)
  )

  expect_equal(
    example_sample_data(
      is_timevar = TRUE,
      is_aggregated = TRUE,
      special_case = NULL,
      family = "binomial"
    ),
    readr::read_csv("https://raw.githubusercontent.com/mrp-interface/shinymrp-data/refs/heads/main/example/data/timevarying_binomial_prep.csv", show_col_types = FALSE)
  )

  expect_equal(
    example_sample_data(
      is_timevar = TRUE,
      is_aggregated = FALSE,
      special_case = NULL,
      family = "normal"
    ),
    readr::read_csv("https://raw.githubusercontent.com/mrp-interface/shinymrp-data/refs/heads/main/example/data/timevarying_normal_raw.csv", show_col_types = FALSE)
  )

  expect_equal(
    example_sample_data(
      is_timevar = TRUE,
      is_aggregated = TRUE,
      special_case = NULL,
      family = "normal"
    ),
    readr::read_csv("https://raw.githubusercontent.com/mrp-interface/shinymrp-data/refs/heads/main/example/data/timevarying_normal_prep.csv", show_col_types = FALSE)
  )

  expect_equal(
    example_sample_data(
      is_timevar = FALSE,
      is_aggregated = FALSE,
      special_case = "poll",
      family = "binomial"
    ),
    readr::read_csv("https://raw.githubusercontent.com/mrp-interface/shinymrp-data/refs/heads/main/example/data/poll_binomial_raw.csv", show_col_types = FALSE)
  )

  expect_equal(
    example_sample_data(
      is_timevar = FALSE,
      is_aggregated = TRUE,
      special_case = "poll",
      family = "binomial"
    ),
    readr::read_csv("https://raw.githubusercontent.com/mrp-interface/shinymrp-data/refs/heads/main/example/data/poll_binomial_prep.csv", show_col_types = FALSE)
  )

  expect_equal(
    example_sample_data(
      is_timevar = FALSE,
      is_aggregated = FALSE,
      special_case = NULL,
      family = "binomial"
    ),
    readr::read_csv("https://raw.githubusercontent.com/mrp-interface/shinymrp-data/refs/heads/main/example/data/crosssectional_binomial_raw.csv", show_col_types = FALSE)
  )

  expect_equal(
    example_sample_data(
      is_timevar = FALSE,
      is_aggregated = TRUE,
      special_case = NULL,
      family = "binomial"
    ),
    readr::read_csv("https://raw.githubusercontent.com/mrp-interface/shinymrp-data/refs/heads/main/example/data/crosssectional_binomial_prep.csv", show_col_types = FALSE)
  )

  expect_equal(
    example_sample_data(
      is_timevar = FALSE,
      is_aggregated = FALSE,
      special_case = NULL,
      family = "normal"
    ),
    readr::read_csv("https://raw.githubusercontent.com/mrp-interface/shinymrp-data/refs/heads/main/example/data/crosssectional_normal_raw.csv", show_col_types = FALSE)
  )

  expect_equal(
    example_sample_data(
      is_timevar = FALSE,
      is_aggregated = TRUE,
      special_case = NULL,
      family = "normal"
    ),
    readr::read_csv("https://raw.githubusercontent.com/mrp-interface/shinymrp-data/refs/heads/main/example/data/crosssectional_normal_prep.csv", show_col_types = FALSE)
  )
})


test_that("example_pstrat_data retrieves the correct files", {
  expect_equal(
    example_pstrat_data(),
    readr::read_csv("https://raw.githubusercontent.com/mrp-interface/shinymrp-data/refs/heads/main/example/data/pstrat.csv", show_col_types = FALSE)
  )
})

test_that("example_mrp_model retrieves the correct files", {
  expect_s3_class(
    example_mrp_model(),
    "MRPModel"
  )
})