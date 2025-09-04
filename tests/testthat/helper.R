make_hashed_filename <- function(
  x,
  prefix = NULL,
  ext    = ".csv",
  n      = 16   # safer default than 8; feel free to set back to 8
) {
  # ------------------------
  # Helpers (base R only)
  # ------------------------
  is_named_list <- function(z) {
    is.list(z) && !is.data.frame(z) &&
      !is.null(names(z)) && any(nzchar(names(z)))
  }

  # Normalize object so that equal content ⇒ equal hash across OS/R
  canonicalize <- function(z) {
    # Normalize common atomic types
    if (inherits(z, "POSIXt")) return(as.POSIXct(z, tz = "UTC"))
    if (is.factor(z))         return(enc2utf8(as.character(z)))
    if (is.character(z))      return(enc2utf8(z))
    if (is.numeric(z))        return(signif(z, 14))   # damp tiny BLAS diffs

    # Don’t disturb data frames (row/col order is meaningful)
    if (is.data.frame(z))     return(z)

    # Strip environments from formulas/functions (session noise)
    if (inherits(z, "formula")) {
      environment(z) <- emptyenv()
      return(z)
    }
    if (is.function(z)) {
      environment(z) <- emptyenv()
      return(z)
    }

    # Recurse into named lists as maps: sort by (UTF-8) key
    if (is_named_list(z)) {
      nm  <- enc2utf8(names(z))
      ord <- order(nm, method = "radix", na.last = TRUE)
      z   <- z[ord]
      names(z) <- nm[ord]
      return(lapply(z, canonicalize))
    }

    # Recurse into other lists (unnamed arrays) without reordering
    if (is.list(z)) return(lapply(z, canonicalize))

    z
  }

  sanitize_prefix <- function(p) {
    if (is.null(p) || identical(p, "")) return(NULL)
    p <- gsub("[^A-Za-z0-9._-]+", "-", p)   # keep letters, numbers, . _ -
    sub("[-.]+$", "", p)
  }

  sanitize_ext <- function(e) {
    e <- if (is.null(e) || e == "") ".dat" else e
    if (!startsWith(e, ".")) paste0(".", e) else e
  }

  # ------------------------
  # Canonicalize + serialize (pin version for portability)
  # ------------------------
  norm_obj <- canonicalize(x)
  raw_ser  <- serialize(norm_obj, connection = NULL, version = 2)

  # ------------------------
  # Hash with base R (tools::md5sum via tempfile)
  # ------------------------
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeBin(raw_ser, tmp)
  full_hash <- unname(tools::md5sum(tmp))  # 32 hex chars (md5)

  short_hash <- substr(full_hash, 1L, as.integer(n))

  pfx <- sanitize_prefix(prefix)
  ext <- sanitize_ext(ext)

  if (is.null(pfx)) paste0(short_hash, ext) else paste0(pfx, "_", short_hash, ext)
}


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