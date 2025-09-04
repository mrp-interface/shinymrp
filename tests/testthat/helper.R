make_hashed_filename <- function(
  x,
  prefix = NULL,
  ext    = ".csv",
  n      = 16
) {
  # ------------------------
  # Helpers (base R only)
  # ------------------------
  is_named_list <- function(z) {
    is.list(z) && !is.data.frame(z) &&
      !is.null(names(z)) && any(nzchar(names(z)))
  }

  strip_attrs <- function(z, keep = character()) {
    a <- attributes(z)
    if (is.null(a)) return(z)
    a_keep <- a[intersect(names(a), keep)]
    attributes(z) <- a_keep
    z
  }

  # Canonicalize dimnames (UTF-8, drop NULL vs "" ambiguity)
  canon_dimnames <- function(dn) {
    if (is.null(dn)) return(NULL)
    lapply(dn, function(v) if (is.null(v)) NULL else enc2utf8(v))
  }

  # ------------------------
  # Canonicalize object so equal content ⇒ equal bytes across R/OS
  # ------------------------
  canonicalize <- function(z) {
    # Basic scalars & vectors
    if (inherits(z, "POSIXt")) {
      # Always serialize as UTC seconds since epoch
      z <- as.POSIXct(z, tz = "UTC")
      return(z)
    }
    if (inherits(z, "Date")) {
      # Keep as Date (days since epoch are stable)
      return(as.Date(z))
    }
    if (inherits(z, "difftime")) {
      # Normalize to seconds
      return(as.numeric(z, units = "secs"))
    }
    if (is.factor(z)) {
      # Factor → UTF-8 character (levels/order differences removed)
      return(enc2utf8(as.character(z)))
    }
    if (is.ordered(z)) {
      return(enc2utf8(as.character(z)))
    }
    if (is.character(z)) {
      return(enc2utf8(z))
    }
    if (is.integer(z)) {
      # Integer vs double can vary between code paths → use double
      return(as.numeric(z))
    }
    if (is.numeric(z)) {
      # Damp tiny BLAS/ALTREP diffs
      return(signif(z, 14))
    }
    if (is.logical(z) || is.complex(z) || is.raw(z)) {
      return(z)
    }

    # Functions / formulas: strip environments
    if (is.function(z)) {
      environment(z) <- emptyenv()
      return(z)
    }
    if (inherits(z, "formula")) {
      environment(z) <- emptyenv()
      return(z)
    }

    # Matrices / arrays
    if (is.matrix(z) || length(dim(z)) > 1L) {
      # Normalize storage mode for numeric-like
      if (is.integer(z)) z <- apply(z, seq_along(dim(z)), as.numeric)
      if (is.numeric(z)) z <- signif(z, 14)

      # Canonicalize character entries
      if (is.character(z)) z <- enc2utf8(z)

      # Normalize dimnames
      dn <- canon_dimnames(dimnames(z))
      dimnames(z) <- dn

      # Keep only essential attrs: dim + dimnames + class (matrix/array)
      z <- strip_attrs(z, keep = c("dim", "dimnames", "class"))
      return(z)
    }

    # Data frames / tibbles: preserve row/col order, but canonicalize columns
    if (is.data.frame(z)) {
      z <- as.data.frame(z, stringsAsFactors = FALSE, check.names = FALSE)
      z[] <- lapply(z, canonicalize)
      # Normalize row.names representation
      row.names(z) <- as.character(seq_len(NROW(z)))
      # Keep only essential attrs
      z <- strip_attrs(z, keep = c("names", "row.names", "class"))
      return(z)
    }

    # Named lists as maps: sort by UTF-8 key, then recurse
    if (is_named_list(z)) {
      nm  <- enc2utf8(names(z))
      ord <- order(nm, method = "radix", na.last = TRUE)
      z   <- z[ord]
      names(z) <- nm[ord]
      z <- lapply(z, canonicalize)
      # Keep only names attribute
      z <- strip_attrs(z, keep = "names")
      return(z)
    }

    # Other lists: recurse without reordering; drop stray attrs
    if (is.list(z)) {
      z <- lapply(z, canonicalize)
      z <- strip_attrs(z, keep = NULL)
      return(z)
    }

    # Fallback: drop non-essential attributes
    strip_attrs(z, keep = NULL)
  }

  sanitize_prefix <- function(p) {
    if (is.null(p) || identical(p, "")) return(NULL)
    p <- gsub("[^A-Za-z0-9._-]+", "-", p)  # keep letters, numbers, . _ -
    sub("[-.]+$", "", p)
  }

  sanitize_ext <- function(e) {
    e <- if (is.null(e) || e == "") ".dat" else e
    if (!startsWith(e, ".")) paste0(".", e) else e
  }

  # ------------------------
  # Canonicalize + serialize with pinned format
  # ------------------------
  norm_obj <- canonicalize(x)
  raw_ser  <- serialize(norm_obj, connection = NULL, version = 2)

  # ------------------------
  # Hash (base R: tools::md5sum via a tempfile)
  # ------------------------
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeBin(raw_ser, tmp)
  full_hash <- unname(tools::md5sum(tmp))  # 32 hex chars

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