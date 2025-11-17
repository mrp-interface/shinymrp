# =========================================================
# Simulate from your Stan models (ICAR vs IID ZIP) and fit with brms
# =========================================================
library(brms)       # >= 2.22
library(cmdstanr)
library(Matrix)
library(loo)
library(spdep)
library(dplyr)
set.seed(123)

options(brms.backend = "cmdstanr")
# cmdstanr::check_cmdstan_toolchain()
# cmdstanr::install_cmdstan()

# ---------- 1) ZIP adjacency (lattice) ----------
make_lattice_adj <- function(nx = 10, ny = 10) {
  K <- nx * ny
  lab <- sprintf("Z%03d", 1:K)
  W <- Matrix(0, K, K, sparse = FALSE)
  idx <- function(x, y) (y - 1L) * nx + x
  for (x in 1:nx) for (y in 1:ny) {
    i <- idx(x, y)
    if (x > 1)  W[i, idx(x - 1, y)] <- 1
    if (x < nx) W[i, idx(x + 1, y)] <- 1
    if (y > 1)  W[i, idx(x, y - 1)] <- 1
    if (y < ny) W[i, idx(x, y + 1)] <- 1
  }
  W <- (W + t(W)) > 0
  W <- Matrix(as.numeric(W), nrow = K, ncol = K, sparse = FALSE)
  rownames(W) <- colnames(W) <- lab

  # Edge list (each undirected edge once, 1-based for Stan)
  E <- which(upper.tri(W) & (W == 1), arr.ind = TRUE)
  node1 <- E[, 1]
  node2 <- E[, 2]
  list(W = W, node1 = node1, node2 = node2, labels = lab)
}

# ---------- 2) Exact ICAR sampler (sum-to-zero subspace) ----------
# ICAR precision Q = L = D - A (singular). Sample in the subspace orthogonal to 1.
icar_sample_sum_to_zero <- function(W) {
  # Graph Laplacian
  d <- rowSums(W)
  L <- diag(d) - as.matrix(W)

  # Eigen-decompose (symmetric). The nullspace (λ≈0) corresponds to the constant vector.
  eig <- eigen(L, symmetric = TRUE)
  lam <- eig$values
  U   <- eig$vectors

  # Identify the null eigenvalue (numerically smallest)
  ord <- order(lam, decreasing = FALSE)
  lam <- lam[ord]; U <- U[, ord, drop = FALSE]

  # Drop the first eigenpair (λ ~ 0), sample in remaining directions
  lam_pos <- lam[-1]
  U_pos   <- U[, -1, drop = FALSE]

  z <- rnorm(length(lam_pos))
  # φ ~ U_pos diag(1/sqrt(λ_pos)) z
  phi <- as.numeric(U_pos %*% (z / sqrt(lam_pos)))

  # Enforce sum-to-zero (should already be ~0 numerically)
  phi <- phi - mean(phi)
  phi
}

# ---------- 3) Simulate from your Stan-equivalent models ----------
# Matches:
#   eta = intercept + X beta + a_race[J_race] + a_age[J_age] + a_time[J_time] + a_zip[J_zip]
#   a_race = lambda_race * z_race,  z_race ~ N(0, I)
#   a_age  = lambda_age  * z_age,   z_age  ~ N(0, I)
#   a_time = lambda_time * z_time,  z_time ~ N(0, I)
#   a_zip  = lambda_zip  * z_zip,   z_zip  ~ ICAR(sum-to-zero)
simulate_from_stan_generative <- function(
  N_zip  = 100,      # nx * ny
  nx = 10, ny = 10,
  N_race = 4, N_age = 5, N_time = 6,
  N      = 3000,     # number of observations
  K      = 2,        # number of fixed-effect covariates
  beta   = c(0.6, -0.5),
  intercept = 0.2,
  lambda_race = 0.35,
  lambda_age  = 0.45,
  lambda_time = 0.55,
  lambda_zip  = 1.00,
  n_trials    = 50
) {
  stopifnot(nx * ny == N_zip)
  adj <- make_lattice_adj(nx, ny)
  W <- adj$W; node1 <- adj$node1; node2 <- adj$node2; zip_labels <- adj$labels

  # Design matrix X
  X <- matrix(rnorm(N * K), N, K)
  colnames(X) <- paste0("x", seq_len(K))

  # Group indices per observation (random sampling, like a survey sample)
  J_race <- sample.int(N_race, N, replace = TRUE)
  J_age  <- sample.int(N_age,  N, replace = TRUE)
  J_time <- sample.int(N_time, N, replace = TRUE)
  J_zip  <- sample.int(N_zip,  N, replace = TRUE)

  # Random effects (Stan parameterization)
  z_race <- rnorm(N_race); a_race <- lambda_race * z_race
  z_age  <- rnorm(N_age);  a_age  <- lambda_age  * z_age
  z_time <- rnorm(N_time); a_time <- lambda_time * z_time

  z_zip  <- icar_sample_sum_to_zero(W)         # sum-to-zero ICAR draw
  a_zip  <- lambda_zip * z_zip                 # scaled as in Stan transformed parameters

  # Linear predictor and Binomial outcome
  eta <- as.numeric(intercept + X %*% beta) +
    a_race[J_race] + a_age[J_age] + a_time[J_time] + a_zip[J_zip]
  p <- plogis(eta)
  n_sample <- rep(n_trials, N)
  y <- rbinom(N, n_sample, p)

  # Data frames for brms
  dat <- data.frame(
    y = y, n_sample = n_sample,
    X, race = factor(J_race),
    age = factor(J_age),
    time = factor(J_time),
    zip = factor(zip_labels[J_zip], levels = zip_labels)
  )
  attr(dat, "true") <- list(
    intercept = intercept, beta = beta,
    a_race = a_race, a_age = a_age, a_time = a_time, a_zip = a_zip,
    z_race = z_race, z_age = z_age, z_time = z_time, z_zip = z_zip
  )
  attr(dat, "adjacency") <- list(W = W, node1 = node1, node2 = node2, zip_levels = zip_labels)
  attr(dat, "dims") <- list(N_zip = N_zip, N_race = N_race, N_age = N_age, N_time = N_time, K = K, N = N)

  # Also prepare Stan-ready lists (for your pure Stan models)
  data_stan_common <- list(
    N = N, K = K, X = X,
    y = as.integer(y),
    n_sample = as.integer(n_sample),
    N_race = N_race, J_race = as.array(as.integer(J_race)),
    N_age  = N_age,  J_age  = as.array(as.integer(J_age)),
    N_time = N_time, J_time = as.array(as.integer(J_time)),
    N_zip  = N_zip,  J_zip  = as.array(as.integer(J_zip))
  )
  data_stan_icar <- c(
    data_stan_common,
    list(
      N_edges_zip = length(node1),
      node1_zip = as.array(as.integer(node1)),
      node2_zip = as.array(as.integer(node2))
    )
  )
  data_stan_iid <- data_stan_common

  list(data = dat, W = W, stan_icar = data_stan_icar, stan_iid = data_stan_iid)
}

# ---------- 4) Simulate once ----------
sim <- simulate_from_stan_generative(
  nx = 10, ny = 10, N = 3000, K = 2,
  beta = c(0.6, -0.5),
  lambda_race = 0.35, lambda_age = 0.45, lambda_time = 0.55, lambda_zip = 1.0,
  n_trials = 50
)
dat <- sim$data
W   <- sim$W
true <- attr(dat, "true")

# ---------- 5) Fit with brms: ICAR vs IID on ZIP ----------
fit_icar <- brm(
  y | trials(n_sample) ~ 1 + x1 + x2 +
    (1 | race) + (1 | age) + (1 | time) +
    car(W, gr = zip, type = "icar"),
  data = dat,
  data2 = list(W = W),
  family = binomial(),
  seed = 123,
  chains = 4, cores = 4, iter = 2000, warmup = 1000,
  control = list(adapt_delta = 0.95, max_treedepth = 12)
)

fit_iid <- brm(
  y | trials(n_sample) ~ 1 + x1 + x2 +
    (1 | race) + (1 | age) + (1 | time) +
    (1 | zip),
  data = dat,
  family = binomial(),
  seed = 123,
  chains = 4, cores = 4, iter = 2000, warmup = 1000,
  control = list(adapt_delta = 0.95, max_treedepth = 12)
)

print(fit_icar)
print(fit_iid)

# ---------- 6) Model comparison (LOO) ----------
loo_icar <- loo(fit_icar)
loo_iid  <- loo(fit_iid)
print(loo_compare(loo_icar, loo_iid))

# ---------- 7) Residual spatial autocorrelation (ZIP-level Moran's I) ----------
# Aggregate Pearson residuals by ZIP (average) for a clean map-level test
res_zip_icar <- data.frame(
  zip = dat$zip,
  r = residuals(fit_icar, type = "pearson")[, 1]
) |>
  group_by(zip) |>
  summarise(r_bar = mean(r), .groups = "drop")

res_zip_iid <- data.frame(
  zip = dat$zip,
  r = residuals(fit_iid, type = "pearson")[, 1]
) |>
  group_by(zip) |>
  summarise(r_bar = mean(r), .groups = "drop")

# listw from W
lw <- mat2listw(as.matrix(W), style = "W")

cat("\nMoran's I (ZIP-avg residuals) — ICAR:\n")
print(moran.test(res_zip_icar$r_bar, lw))

cat("\nMoran's I (ZIP-avg residuals) — IID:\n")
print(moran.test(res_zip_iid$r_bar, lw))

# ---------- 8) Optional: sanity check recovery of ZIP spatial effect amplitude ----------
# For IID model we can read ranef(fit_iid)$zip[,,1] directly.
re_iid <- ranef(fit_iid)$zip[,,1]  # posterior mean of ZIP intercepts (N_zip x 1)
# For ICAR, the CAR term is not exposed as (1|zip). A practical proxy:
# compare ZIP-mean posterior linear predictor *differences* between models.
# (The ICAR model should show less residual spatial structure and better LOO.)

# Quick look at fixed effect recovery
fixef(fit_icar); fixef(fit_iid)
