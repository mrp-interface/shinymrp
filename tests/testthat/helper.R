read_qs_github <- function(url) {
  # Create temporary file
  temp_file <- tempfile(fileext = ".qs")
  on.exit(unlink(temp_file))
  
  # Download and read
  utils::download.file(url, destfile = temp_file, mode = "wb", quiet = TRUE)
  qs::qread(temp_file)
}

make_hashed_filename <- function(
  x,
  prefix = NULL,
  ext    = ".csv",
  n      = 8
) {
  # recursively sort each sub‐list by name
  normalize <- function(z) {
    if (!is.list(z)) return(z)
    z <- z[sort(names(z))]
    lapply(z, normalize)
  }

  # normalize the object so ordering doesn't matter
  norm_obj <- normalize(x)

  # serialize to a raw vector
  raw_ser <- serialize(norm_obj, connection = NULL)

  # write to a temp file and compute its MD5 sum
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeBin(raw_ser, tmp)
  full_hash <- unname(tools::md5sum(tmp))

  # take only the first n hex chars and build the filename
  short_hash <- substr(full_hash, 1, n)
  paste0(prefix, "_", short_hash, ext)
}

get_test_data <- function(fit) {
  variables <- setdiff(fit$metadata()$variables, "lp__")
  fit$summary(variables = variables) %>% select(mean, sd)
}


setup_test_workflow <- function(link_geo = NULL, ...) {
  data <- example_sample_data(...)

  workflow <- mrp_workflow()

  capture.output({
    workflow$preprocess(
      data,
      ...
    )

    workflow$link_acs(link_geo = link_geo)
  }, type = "message")

  return(workflow)
}

setup_test_model <- function(workflow, model_spec = NULL) {
  model_spec <- .replace_null(
    model_spec,
    list(
      intercept = list(
        intercept = "normal(0, 1)"
      ),
      fixed = list(
        sex = "normal(0, 1)"
      )
    )
  )

  model <- workflow$create_model(
    intercept_prior = model_spec$intercept$intercept,
    fixed = model_spec$fixed,
    varying = model_spec$varying,
    interaction = model_spec$interaction
  )

  model$fit(
    n_iter = 100,
    n_chains = 2,
    show_messages = FALSE,
    show_exceptions = FALSE,
    diagnostics = NULL
  )

  return(model)
}

expect_equal_saved <- function(workflow, model_spec) {
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

  # readr::write_csv(
  #   get_test_data(model$fit_object()),
  #   file = paste0("/Users/tntoan/Desktop/repos/shinymrp/tests/testthat/testdata/snapshot/", make_hashed_filename(model_spec, prefix = "model"))
  # )

  saved <- paste0(
      "testdata/snapshot/",
      make_hashed_filename(model_spec, prefix = "model")
    ) %>%
    testthat::test_path() %>%
    readr::read_csv(show_col_types = FALSE)


  expect_equal(
    get_test_data(model$fit_object()),
    saved,
    tolerance = 0.05,
    ignore_attr = TRUE
  )
}