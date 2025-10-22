# ===============================================================
# ICAR + MRP test-data simulator for the given Stan model
# ===============================================================
# Requires: Matrix, stats, lubridate, dplyr (only for piping/arrange, can be removed)
suppressPackageStartupMessages({
  library(Matrix)
  library(lubridate)
  library(dplyr)
})

# ---------- ICAR helpers (you provided variants; kept self-contained here) ----------
icar_make_Q <- function(stan_graph) {
  N  <- as.integer(stan_graph$N_nodes)
  e1 <- as.integer(stan_graph$node1); e2 <- as.integer(stan_graph$node2)
  A  <- sparseMatrix(i = c(e1, e2), j = c(e2, e1), x = 1, dims = c(N, N))
  d  <- Matrix::rowSums(A)
  Q  <- Diagonal(x = as.numeric(d)) - A
  Q
}

# Low-pass field (your approach): smoother, higher spatial correlation
graph_lowpass_random_field <- function(stan_graph, m = 3, coeff_decay = 1, seed = 1) {
  set.seed(seed)
  Q <- icar_make_Q(stan_graph); N <- nrow(Q)
  ev <- eigen(as.matrix(Q), symmetric = TRUE)
  o  <- order(ev$values)                       # ascending
  vals <- ev$values[o]; vecs <- ev$vectors[, o, drop = FALSE]
  # drop the constant mode (≈0 eigenvalue)
  vals <- vals[-1]
  vecs <- vecs[, -1, drop = FALSE]
  m    <- min(m, length(vals))
  coefs <- rnorm(m) / pmax(vals[1:m], 1e-12)^coeff_decay
  x <- vecs[, 1:m, drop = FALSE] %*% coefs
  x <- as.numeric(x); x <- x - mean(x); x <- x / sd(x)
  x
}

# Exact ICAR sampler via spectral decomposition of Q (samples from Q^+):
# z = sum_j (xi_j / sqrt(lambda_j)) * v_j, excluding the 0-eig constant mode
icar_sample_spectral <- function(stan_graph, seed = 1) {
  set.seed(seed)
  Q <- icar_make_Q(stan_graph); N <- nrow(Q)
  ev <- eigen(as.matrix(Q), symmetric = TRUE)
  vals <- ev$values; vecs <- ev$vectors
  # Identify/skip (near-)zero eigen (constant mode)
  ztol <- 1e-10
  keep <- which(vals > ztol)
  if (length(keep) == 0L) stop("Q appears singular beyond constant mode.")
  xi <- rnorm(length(keep))
  z  <- vecs[, keep, drop = FALSE] %*% (xi / sqrt(vals[keep]))
  z  <- as.numeric(z)
  # Enforce mean/sum-to-zero & unit sd for stability with your lambda scaling:
  z <- z - mean(z); z <- z / sd(z)
  z
}

# ---------- Simple order-preserving grid graph (fallback if you don't pass one) ----------
# Creates a 4-neighbor lattice graph with nodes indexed row-major: (r,c) -> (r-1)*nc + c
make_grid_graph <- function(nr = 4, nc = 4) {
  stopifnot(nr >= 1L, nc >= 1L)
  idx <- function(r, c) (r - 1L) * nc + c
  node1 <- integer(0); node2 <- integer(0)
  for (r in 1:nr) for (c in 1:nc) {
    u <- idx(r, c)
    if (c < nc) { v <- idx(r, c + 1L); node1 <- c(node1, u); node2 <- c(node2, v) }
    if (r < nr) { v <- idx(r + 1L, c); node1 <- c(node1, u); node2 <- c(node2, v) }
  }
  list(
    N_nodes    = as.integer(nr * nc),
    N_edges    = as.integer(length(node1)),
    node1      = as.integer(node1),
    node2      = as.integer(node2),
    N_comps    = as.integer(1),
    comp_sizes = as.integer(nr * nc),
    comp_index = matrix(as.integer(1:(nr * nc)), nrow = nr * nc, ncol = 1)
  )
}

icar_scale <- function(stan_graph) {
  Q <- icar_make_Q(stan_graph)
  ev <- eigen(as.matrix(Q), symmetric = TRUE, only.values = TRUE)$values
  vals <- ev[ev > 1e-12]
  sqrt(mean(1 / vals))  # s = sqrt(trace(Q^+)/n)
}


# ---------- Main simulator ----------
simulate_stan_data_icar <- function(
  stan_graph = NULL,
  grid_nr = 4, grid_nc = 4,
  start_date = "2024-01-01",
  n_weeks    = 6L,
  race_levels = c("white","black","other"),
  age_levels  = c("0-17","18-34","35-64","65-74","75+"),
  sex_levels  = c("male","female"),
  n_sample_per_cell = 20L,
  intercept_true = -2.0,
  beta_true      = c(sex_male = log(1.25)),
  lambda_race_true = 0.3,
  lambda_age_true  = 0.6,
  lambda_time_true = 0.4,
  lambda_zip_true  = 0.8,
  icar_mode = c("lowpass","icar"),
  lowpass_m = 3, lowpass_decay = 1,
  sens = 1.0, spec = 1.0,
  seed = sample(1:1e6, 1),
  zip_labels = NULL,
  holdout_zip_frac    = 0.0,        # e.g., 0.25 holds out 25% of ZIPs entirely
  holdout_zip_ids     = NULL,       # vector of labels or integer indices to force holdout
  within_zip_coverage = 1.0,        # fraction of cells kept in NON-holdout ZIPs (<=1)
  bym2_scale          = FALSE       # if TRUE, sim + Stan get the same s_icar scaling
) {
  set.seed(seed)
  icar_mode <- match.arg(icar_mode)

  # 1) Graph for ZIPs
  if (is.null(stan_graph)) stan_graph <- make_grid_graph(grid_nr, grid_nc)
  N_zip <- as.integer(stan_graph$N_nodes)

  # ZIP labels
  zip_lab_vec <- if (!is.null(zip_labels)) {
    as.character(zip_labels)
  } else if (!is.null(stan_graph$ids_local)) {
    as.character(stan_graph$ids_local)
  } else {
    as.character(seq_len(N_zip))
  }
  if (length(zip_lab_vec) != N_zip) stop("zip_labels length must equal N_zip")

  # 2) Levels / indices
  race_levels <- as.character(race_levels)
  age_levels  <- as.character(age_levels)
  sex_levels  <- as.character(sex_levels)
  N_race <- length(race_levels)
  N_age  <- length(age_levels)
  N_time <- as.integer(n_weeks)

  # 3) Other random effects
  z_race <- rnorm(N_race); a_race <- lambda_race_true * z_race
  z_age  <- rnorm(N_age);  a_age  <- lambda_age_true  * z_age
  z_time <- rnorm(N_time); a_time <- lambda_time_true * z_time

  # 4) ICAR over ZIPs (truth)
  if (icar_mode == "lowpass") {
    z_zip <- graph_lowpass_random_field(stan_graph, m = lowpass_m,
                                        coeff_decay = lowpass_decay, seed = seed + 11)
  } else {
    z_zip <- icar_sample_spectral(stan_graph, seed = seed + 11)
  }
  z_zip <- z_zip - mean(z_zip); z_zip <- z_zip / sd(z_zip)

  # Optional BYM2-style scaling (kept consistent for Stan + sim)
  s_icar <- 1.0
  if (isTRUE(bym2_scale)) {
    s_icar <- icar_scale(stan_graph)
    z_zip  <- z_zip / s_icar
  }
  a_zip <- lambda_zip_true * z_zip

  # 5) Crossed design (cell table)
  df <- expand.grid(
    sex  = factor(sex_levels, levels = sex_levels),
    race = factor(race_levels, levels = race_levels),
    age  = factor(age_levels,  levels = age_levels),
    time = 1:N_time,
    zip  = 1:N_zip,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  df <- df[order(df$time, df$zip, df$race, df$age, df$sex), , drop = FALSE]
  zip_str <- zip_lab_vec[df$zip]

  # Stan indices
  J_race <- as.integer(df$race)
  J_age  <- as.integer(df$age)
  J_time <- as.integer(df$time)
  J_zip  <- as.integer(df$zip)

  # 6) X (K=1: sex_male)
  X <- cbind(sex_male = as.numeric(df$sex == "male"))
  K <- ncol(X)

  # Linear predictor & probabilities
  eta <- as.numeric(
    intercept_true + drop(X %*% beta_true) +
      a_race[J_race] + a_age[J_age] + a_time[J_time] + a_zip[J_zip]
  )
  p <- plogis(eta)
  p_sample <- p * sens + (1 - p) * (1 - spec)

  # 7) Sampling plan (before holdouts)
  n_sample <- rep(as.integer(n_sample_per_cell), nrow(df))

  # -------- NEW: choose ZIP-level holdouts ----------
  set.seed(seed + 97)
  zip_holdout_idx <- integer(0)

  if (!is.null(holdout_zip_ids)) {
    # allow labels or integer indices
    if (is.character(holdout_zip_ids)) {
      zip_holdout_idx <- match(holdout_zip_ids, zip_lab_vec)
    } else {
      zip_holdout_idx <- as.integer(holdout_zip_ids)
    }
    zip_holdout_idx <- sort(unique(na.omit(zip_holdout_idx)))
    if (length(zip_holdout_idx) == 0L) warning("holdout_zip_ids did not match any ZIPs.")
  } else if (holdout_zip_frac > 0) {
    n_ho <- max(0L, round(holdout_zip_frac * N_zip))
    if (n_ho > 0) zip_holdout_idx <- sort(sample.int(N_zip, n_ho, replace = FALSE))
  }

  mask_zip_holdout <- df$zip %in% zip_holdout_idx
  n_sample[mask_zip_holdout] <- 0L

  # -------- NEW: within-ZIP sparsity on remaining ZIPs ----------
  if (within_zip_coverage < 1) {
    set.seed(seed + 98)
    idx_non <- which(!mask_zip_holdout)
    keep <- runif(length(idx_non)) < within_zip_coverage
    n_sample[idx_non[!keep]] <- 0L
  }

  # Actual coverages achieved
  mask_cell_obs <- n_sample > 0L
  cov_overall   <- mean(mask_cell_obs)
  cov_nonho     <- if (sum(!mask_zip_holdout) > 0) mean(mask_cell_obs[!mask_zip_holdout]) else 0

  # 8) Simulate outcomes with the (possibly sparse) sampling plan
  y <- rbinom(n = nrow(df), size = n_sample, prob = p_sample)

  # 9) Population table & X_pop
  pop <- expand.grid(
    race = factor(race_levels, levels = race_levels),
    age  = factor(age_levels,  levels = age_levels),
    time = 1:N_time,
    zip  = 1:N_zip,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  pop <- pop[order(pop$time, pop$zip, pop$race, pop$age), , drop = FALSE]

  J_race_pop <- as.integer(pop$race)
  J_age_pop  <- as.integer(pop$age)
  J_time_pop <- as.integer(pop$time)
  J_zip_pop  <- as.integer(pop$zip)

  X_pop <- cbind(sex_male = rep(0.5, nrow(pop)))
  K_pop <- ncol(X_pop)

  # 10) Dates for weeks
  week_dates <- as.character(as.Date(start_date) + lubridate::weeks(0:(N_time - 1)))
  df$date <- week_dates[df$time]

  # 11) RAW microdata (respects holdouts/sparsity)
  expand_row <- function(i) {
    if (n_sample[i] == 0L) return(NULL)
    data.frame(
      sex      = as.character(df$sex[i]),
      race     = as.character(df$race[i]),
      age      = as.character(df$age[i]),
      time     = as.integer(df$time[i]),
      date     = df$date[i],
      zip      = zip_str[i],
      positive = as.integer(c(rep(1L, y[i]), rep(0L, n_sample[i] - y[i]))),
      stringsAsFactors = FALSE
    )
  }
  raw_list <- lapply(seq_len(nrow(df)), expand_row)
  raw <- if (length(raw_list)) do.call(rbind, raw_list) else
    data.frame(sex=character(), race=character(), age=character(),
               time=integer(), date=character(), zip=character(),
               positive=integer(), stringsAsFactors = FALSE)

  # 12) Stan data (+ optional s_icar)
  stan_data <- list(
    N      = as.integer(nrow(df)),
    K      = as.integer(K),
    X      = matrix(as.numeric(X), nrow = nrow(X), ncol = K),
    N_pop  = as.integer(nrow(pop)),
    K_pop  = as.integer(K_pop),
    X_pop  = matrix(as.numeric(X_pop), nrow = nrow(X_pop), ncol = K_pop),

    y        = as.integer(y),
    n_sample = as.integer(n_sample),

    N_race   = as.integer(length(race_levels)),
    J_race   = as.integer(J_race),
    N_race_pop = as.integer(length(race_levels)),
    J_race_pop = as.integer(J_race_pop),

    N_age   = as.integer(length(age_levels)),
    J_age   = as.integer(J_age),
    N_age_pop = as.integer(length(age_levels)),
    J_age_pop = as.integer(J_age_pop),

    N_time   = as.integer(N_time),
    J_time   = as.integer(J_time),
    N_time_pop = as.integer(N_time),
    J_time_pop = as.integer(J_time_pop),

    N_zip    = as.integer(N_zip),
    J_zip    = as.integer(J_zip),
    N_zip_pop = as.integer(N_zip),
    J_zip_pop = as.integer(J_zip_pop),

    N_edges_zip = as.integer(stan_graph$N_edges),
    node1_zip   = as.integer(stan_graph$node1),
    node2_zip   = as.integer(stan_graph$node2),

    sens = as.numeric(sens),
    spec = as.numeric(spec)
  )
  if (isTRUE(bym2_scale)) stan_data$s_icar <- as.numeric(s_icar)

  # 13) Truth + splits (for evaluation)
  truth <- list(
    intercept = intercept_true,
    beta      = as.numeric(beta_true),
    a_race = a_race, z_race = z_race,
    a_age  = a_age,  z_age  = z_age,
    a_time = a_time, z_time = z_time,
    a_zip  = a_zip,  z_zip  = z_zip,
    p = p, p_sample = p_sample,
    race_levels = race_levels, age_levels = age_levels, sex_levels = sex_levels,
    week_dates = week_dates, zip_labels = zip_lab_vec, stan_graph = stan_graph,
    s_icar = s_icar
  )

  splits <- list(
    zip_holdout_idx    = as.integer(zip_holdout_idx),
    zip_holdout_labels = zip_lab_vec[zip_holdout_idx],
    mask_zip_holdout   = mask_zip_holdout,     # length N (cell-table)
    mask_cell_observed = mask_cell_obs,        # length N (cell-table)
    coverage_overall   = cov_overall,
    coverage_nonholdout= cov_nonho
  )

  list(stan_data = stan_data, raw = raw, cell_table = df,
       pop_table = pop, truth = truth, splits = splits)
}

