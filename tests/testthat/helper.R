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

setup_test_workflow <- function(
  metadata,
  is_aggregated = TRUE,
  time_freq = NULL,
  link_geo = NULL,
  link = TRUE
) {
  
  data <- example_sample_data(
    is_timevar = metadata$is_timevar,
    is_aggregated = is_aggregated,
    special_case = metadata$special_case,
    family = metadata$family
  )
  workflow <- mrp_workflow()

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