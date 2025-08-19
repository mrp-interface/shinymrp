#' Convert empty values to NULL
#'
#' @description Converts various types of empty or missing values to NULL,
#' including empty vectors, NA values, empty strings, and empty data frames.
#' Useful for standardizing empty inputs in data processing pipelines.
#'
#' @param x Any R object to check for emptiness
#'
#' @return NULL if the input is considered empty, otherwise returns x unchanged
#'
#' @noRd
.nullify <- function(x) {
  # Check for empty vector and NULL
  if (length(x) == 0) return(NULL)

  # Check for NA
  if (length(x) == 1 && is.na(x)) return(NULL)
  
  # Check for empty string
  if (is.character(x) && x == "") return(NULL)
  
  
  # Check for empty dataframe
  if (is.data.frame(x) && nrow(x) == 0) return(NULL)
  
  # If none of above, return x unchanged
  return(x)
}

#' Replace NULL values with a replacement value
#'
#' @description Replaces NULL values with a specified replacement value,
#' leaving non-NULL values unchanged. Useful for providing default values
#' when NULL inputs are encountered.
#'
#' @param x Any R object to check for NULL
#' @param replacement Any R object to use as replacement when x is NULL
#'
#' @return The replacement value if x is NULL, otherwise returns x unchanged
#'
#' @noRd
.replace_null <- function(x, replacement) {
  if (is.null(x)) {
    return(replacement)
  } else {
    return(x)
  }
}

#' Create human-readable data format labels
#'
#' @description Converts internal data format codes to human-readable labels
#' for display in the user interface. Maps technical format names to
#' descriptive category labels.
#'
#' @return Character. Human-readable label corresponding to the data format,
#'   or "Unknown Data Format" if the format is not recognized
#'
#' @noRd
.use_case_label <- function(metadata, labels = GLOBAL$ui$use_case_labels) {
  if (!is.null(metadata$special_case)) {
    switch(metadata$special_case,
      poll = labels$poll,
      covid = labels$covid,
      "Unknown"
    )
  } else {
    if (metadata$is_timevar) {
      labels$timevar_general
    } else {
      labels$static_general
    }
  }


}


#' Generate random IDs
#'
#' @description Generates random alphanumeric identifiers of specified length
#' using digits (0-9), lowercase letters (a-z), and uppercase letters (A-Z).
#' Useful for creating unique identifiers for UI elements or temporary objects.
#'
#' @param n Integer. Length of the ID to generate (default: 8)
#'
#' @return Character. A random alphanumeric string of length n
#'
#' @noRd
.generate_id <- function(n = 8) {
  # Define the pool of characters: digits, lowercase and uppercase letters
  chars <- c(0:9, letters, LETTERS)
  
  # Sample with replacement and collapse into one string
  paste0(sample(chars, size = n, replace = TRUE), collapse = "")
}

#' Get configuration values
#'
#' @description Retrieves configuration values from the application's config.yml
#' file using the config package. Respects the R_CONFIG_ACTIVE environment
#' variable for different configuration environments.
#'
#' @param value Character. The configuration key to retrieve from config.yml
#'
#' @return The configuration value associated with the specified key
#'
#' @importFrom config get
#'
#' @noRd
.get_config <- function(value) {
  config::get(
    value  = value,
    config = Sys.getenv("R_CONFIG_ACTIVE", "default"),
    file   = app_sys("config.yml")
  )
}

#' Create standardized example filenames
#'
#' @description Generates standardized filenames for example data files based on
#' metadata specifications. Combines use case, family, and suffix information
#' to create consistent naming conventions for different file types.
#'
#' @param metadata List containing metadata with family and case information
#' @param suffix Character vector specifying file suffix, one of "raw", "prep", or "fit"
#' @param ext Character string specifying file extension (default: ".csv")
#' @param sep Character string used as separator in filename (default: "_")
#' @param valid_families Character vector of valid family values for validation
#'
#' @return Character string containing the constructed filename following the
#'   pattern: usecase_family_suffix.ext (e.g., "covid_binomial_raw.csv")
#'
#' @noRd
.create_example_filename <- function(
  metadata,
  suffix = c("raw", "prep", "fit"),
  ext = ".csv",
  sep = "_",
  valid_families = GLOBAL$args$family
) {
  # Validate inputs
  suffix <- match.arg(suffix)
  if (!metadata$family %in% valid_families) {
    stop("Invalid family specified in metadata")
  }

  use_case <- if (!is.null(metadata$special_case)) {
    metadata$special_case
  } else {
    if (metadata$is_timevar) {
      "timevarying"
    } else {
      "crosssectional"
    }
  }

  # Construct file name
  family <- paste0(sep, metadata$family)
  suffix <- paste0(sep, suffix)
  file_name <- paste0(use_case, family, suffix, ext)

  return(file_name)
}

#' Validate and parse interval specifications
#'
#' @description Validates and parses interval specifications for uncertainty
#' quantification, supporting both credible intervals (numeric) and standard
#' deviation multiples (character). Returns standardized parameters for
#' interval calculations.
#'
#' @param interval Numeric value between 0 and 1 for credible intervals
#'   (e.g., 0.95 for 95% CI), or character string for standard deviations
#'   ("1sd", "2sd", or "3sd")
#'
#' @return List containing interval parameters:
#'   \itemize{
#'     \item is_ci: Logical indicating if credible interval (TRUE) or standard deviation (FALSE)
#'     \item qlower: Lower quantile for credible intervals
#'     \item qupper: Upper quantile for credible intervals
#'     \item n_sd: Number of standard deviations for uncertainty bands
#'   }
#'
#' @noRd
.check_interval <- function(interval) {
  is_ci <- TRUE
  qlower <- 0.025
  qupper <- 0.975
  n_sd <- 1

  if (is.character(interval)) {
    if (!grepl("^[1-3]+sd$", interval, ignore.case = FALSE)) {
      stop("For standard deviation of uncertainty, 'interval' must be either '1sd', '2sd', or '3sd'.")
    }
    
    is_ci <- FALSE
    n_sd <- as.numeric(gsub("sd", "", interval))

  } else if (is.numeric(interval)) {
    if (interval < 0 || interval > 1) {
      stop("For credible interval, 'interval' must be between 0 and 1.")
    } 

    is_ci <- TRUE
    qlower <- (1 - interval) / 2
    qupper <- 1 - qlower

  } else {
    stop("'interval' must be a character string or a numeric value between 0 and 1.")
  }

  return(list(
    is_ci = is_ci,
    qlower = qlower,
    qupper = qupper,
    n_sd = n_sd
  ))
}

#' Fetch data files from remote repository with caching
#'
#' @description Downloads and caches data files from a GitHub repository,
#' with support for local caching and remote-change detection via GitHub API.
#' Automatically handles different file formats (CSV, QS, R) and manages cache directory creation.
#'
#' @param file Character string specifying the filename to fetch (including extension)
#' @param org Character string specifying the GitHub organization (default: "mrp-interface")
#' @param repo Character string specifying the repository name (default: "shinymrp-data")
#' @param branch Character string specifying the git branch (default: "main")
#' @param subdir Character string specifying subdirectory path within repo (default: "")
#' @param cache_dir Character string specifying local cache directory path
#' @param check_remote Logical; if TRUE, use GitHub API to detect remote updates (default: TRUE)
#'
#' @return Data frame or object loaded from the specified file, with format
#' determined by file extension (CSV files return data frames, QS files
#' return the original R object, and R files return character lines)
#' @noRd
.fetch_data <- function(
  file,
  org = "mrp-interface",
  repo = "shinymrp-data",
  branch = "main",
  subdir = "",
  cache_dir = tools::R_user_dir("shinymrp", which = "cache"),
  check_remote = TRUE
) {
  # ensure cache directory exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # construct local path and URLs
  dest <- file.path(cache_dir, file)
  file_path <- if (nzchar(subdir)) paste0(subdir, "/", file) else file
  raw_url <- sprintf(
    "https://raw.githubusercontent.com/%s/%s/%s/%s",
    org, repo, branch, file_path
  )
  
  # determine if download is needed
  should_dl <- !file.exists(dest)
  
  if (!should_dl && check_remote) {
    # use GitHub API to get file information
    api_url <- sprintf(
      "https://api.github.com/repos/%s/%s/contents/%s",
      org, repo, file_path
    )
    
    tryCatch({
      # get file info from GitHub API
      resp <- httr2::request(api_url) %>%
        httr2::req_url_query(ref = branch) %>%
        httr2::req_perform()
      
      if (httr2::resp_status(resp) == 200) {
        file_info <- httr2::resp_body_json(resp)
        
        # compare SHA hashes (more reliable than timestamps)
        remote_sha <- file_info$sha
        
        # store SHA in a companion file for comparison
        sha_file <- paste0(dest, ".sha")
        local_sha <- NULL
        
        if (file.exists(sha_file)) {
          local_sha <- readLines(sha_file, n = 1, warn = FALSE)
        }
        
        if (is.null(local_sha) || remote_sha != local_sha) {
          should_dl <- TRUE
        }
      }
    }, error = function(e) {
      # if API call fails, fall back to downloading
      warning("GitHub API check failed: ", e$message, ". Proceeding with download.")
      should_dl <- TRUE
    })
  }
  
  # download if required
  if (should_dl) {
    utils::download.file(raw_url, destfile = dest, mode = "wb")
    
    # if we successfully checked remote, store the SHA for next time
    if (check_remote && exists("remote_sha")) {
      sha_file <- paste0(dest, ".sha")
      writeLines(remote_sha, sha_file)
    }
  }
  
  # read file based on extension
  ext <- tools::file_ext(file)
  switch(ext,
    csv = readr::read_csv(dest, show_col_types = FALSE),
    qs = qs::qread(dest),
    R = readLines(dest),
    stop("Unsupported file extension: ", ext)
  )
}