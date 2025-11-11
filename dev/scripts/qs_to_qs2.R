# qs_to_qs2.R
# Convert all .qs files in a directory tree to .qs2 using qs2
# Requires: qs, qs2

convert_qs_file <- function(infile,
                            outfile = sub("\\.qs$", ".qs2", infile, ignore.case = TRUE),
                            overwrite = FALSE,
                            verify = TRUE,
                            delete_old = FALSE,
                            backup_ext = ".bak") {
  stopifnot(file.exists(infile))
  if (tolower(tools::file_ext(infile)) != "qs") {
    stop("infile must have .qs extension: ", infile)
  }

  if (file.exists(outfile) && !overwrite) {
    return(list(
      status = "skipped_exists",
      infile = infile,
      outfile = outfile,
      bytes_in = file.info(infile)$size,
      bytes_out = file.info(outfile)$size,
      verified = NA
    ))
  }

  # Read old format with qs, write new with qs2
  obj <- try(qs::qread(infile), silent = TRUE)
  if (inherits(obj, "try-error")) {
    return(list(status = "read_error", infile = infile, outfile = outfile,
                bytes_in = file.info(infile)$size, bytes_out = NA, verified = FALSE))
  }

  # Ensure target dir exists
  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)

  wr <- try(qs2::qs_save(obj, outfile), silent = TRUE)
  if (inherits(wr, "try-error")) {
    return(list(status = "write_error", infile = infile, outfile = outfile,
                bytes_in = file.info(infile)$size, bytes_out = NA, verified = FALSE))
  }

  verified <- NA
  if (verify) {
    obj2 <- try(qs2::qs_read(outfile), silent = TRUE)
    if (inherits(obj2, "try-error")) {
      file.remove(outfile)
      return(list(status = "verify_read_error", infile = infile, outfile = outfile,
                  bytes_in = file.info(infile)$size, bytes_out = NA, verified = FALSE))
    }
    # Strict compare first, then fuzzy fallback
    verified <- isTRUE(identical(obj, obj2)) || isTRUE(all.equal(obj, obj2, check.attributes = TRUE))
    if (!verified) {
      file.remove(outfile)
      return(list(status = "verify_mismatch", infile = infile, outfile = outfile,
                  bytes_in = file.info(infile)$size, bytes_out = NA, verified = FALSE))
    }
  }

  if (delete_old) {
    # Move old to backup if we might want a safety net; then remove original
    bak <- paste0(infile, backup_ext)
    ok_bak <- TRUE
    if (!file.exists(bak)) {
      ok_bak <- try(!isTRUE(file.copy(infile, bak, overwrite = FALSE)) == FALSE, silent = TRUE)
    }
    if (isTRUE(ok_bak)) {
      unlink(infile)
    } else {
      # If backup fails for any reason, keep the original around.
      return(list(status = "converted_kept_old", infile = infile, outfile = outfile,
                  bytes_in = file.info(infile)$size, bytes_out = file.info(outfile)$size,
                  verified = verified))
    }
  }

  list(status = "converted", infile = infile, outfile = outfile,
       bytes_in = file.info(infile)$size, bytes_out = file.info(outfile)$size,
       verified = verified)
}

convert_qs_tree <- function(root,
                            recursive = TRUE,
                            overwrite = FALSE,
                            verify = TRUE,
                            delete_old = FALSE,
                            backup_ext = ".bak",
                            dry_run = FALSE,
                            progress = interactive()) {
  stopifnot(dir.exists(root))
  files <- list.files(root, pattern = "\\.qs$", ignore.case = TRUE,
                      recursive = recursive, full.names = TRUE)
  if (!length(files)) {
    message("No .qs files found under: ", normalizePath(root))
    return(invisible(data.frame()))
  }

  if (dry_run) {
    out <- data.frame(
      infile  = files,
      outfile = sub("\\.qs$", ".qs2", files, ignore.case = TRUE),
      action  = ifelse(file.exists(sub("\\.qs$", ".qs2", files, ignore.case = TRUE)) & !overwrite,
                       "skip (exists)", "convert"),
      stringsAsFactors = FALSE
    )
    return(out)
  }

  if (progress) pb <- utils::txtProgressBar(min = 0, max = length(files), style = 3)
  res <- vector("list", length(files))

  for (i in seq_along(files)) {
    infile  <- files[[i]]
    outfile <- sub("\\.qs$", ".qs2", infile, ignore.case = TRUE)

    res[[i]] <- try(
      convert_qs_file(infile, outfile,
                      overwrite = overwrite,
                      verify = verify,
                      delete_old = delete_old,
                      backup_ext = backup_ext),
      silent = TRUE
    )

    if (progress) utils::setTxtProgressBar(pb, i)
  }
  if (progress) close(pb)

  # Tidy up results
  rows <- lapply(res, function(r) {
    if (inherits(r, "try-error") || is.null(r)) {
      return(data.frame(status = "unknown_error", infile = NA, outfile = NA,
                        bytes_in = NA_real_, bytes_out = NA_real_, verified = NA))
    }
    as.data.frame(as.list(r), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  class(out$bytes_in)  <- "numeric"
  class(out$bytes_out) <- "numeric"

  # Summary
  tally <- table(out$status, useNA = "ifany")
  message("\nSummary:")
  print(tally)
  if ("converted" %in% names(tally)) {
    saved <- sum(out$bytes_in - out$bytes_out, na.rm = TRUE)
    message(sprintf("Total size change: %+0.1f MB", saved / (1024^2)))
  }

  out[order(out$status, out$infile), ]
}

# ---------- Example usage ----------
dir <- "/Users/tntoan/Desktop/repos/shinymrp-data"

# Dry run: preview what will be converted
convert_qs_tree(dir, dry_run = TRUE)

# Actual conversion (keeps originals; safest):
# log <- convert_qs_tree(dir, delete_old = FALSE)

# Aggressive mode (overwrites existing .qs2 if present, verifies, and deletes old .qs after a backup copy):
log <- convert_qs_tree(dir,
                       overwrite = TRUE, verify = TRUE,
                       delete_old = TRUE, backup_ext = "")
print(log)
