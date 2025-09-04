setup_test_workflow <- function(
  metadata,
  is_aggregated = TRUE,
  time_freq = NULL,
  link_geo = NULL,
  link = TRUE,
  constructor = mrp_workflow
) {
  
  data <- example_sample_data(
    is_timevar = metadata$is_timevar,
    is_aggregated = is_aggregated,
    special_case = metadata$special_case,
    family = metadata$family
  )
  
  workflow <- constructor()

  capture.output({
    workflow$preprocess(
      data,
      is_timevar = metadata$is_timevar,
      is_aggregated = is_aggregated,
      special_case = metadata$special_case,
      family = metadata$family,
      time_freq = time_freq
    )

    if (link) {
      workflow$link_acs(link_geo = link_geo)
    }
  }, type = "message")

  return(workflow)
}

setup_test_model <- function(workflow, model_spec = NULL, fit_model = TRUE) {
  model_spec <- model_spec %||% list(
    intercept = list(
      intercept = "normal(0, 1)"
    ),
    fixed = list(
      sex = "normal(0, 1)"
    )
  )

  model <- workflow$create_model(
    intercept_prior = model_spec$intercept$intercept,
    fixed = model_spec$fixed,
    varying = model_spec$varying,
    interaction = model_spec$interaction
  )

  if (fit_model) {
    model$fit(
      n_iter = 100,
      n_chains = 1,
      show_messages = FALSE,
      show_exceptions = FALSE,
      diagnostics = NULL
    )
  }

  return(model)
}

expect_save_file <- function(func, ext, ...) {
  temp_file <- tempfile(fileext = ext)
  func(temp_file, ...)
  expect_true(file.exists(temp_file))
  unlink(temp_file)
}

read_saved_csv <- function(file_path) {
  suppressWarnings(
    readr::read_csv(
      file_path,
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
}