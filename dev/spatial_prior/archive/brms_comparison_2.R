library(brms)
library(cmdstanr)
library(Matrix)
library(loo)
library(dplyr)
library(spdep)
set.seed(123)
options(brms.backend = "cmdstanr")

# ---------- Lattice adjacency ----------
make_lattice_adj <- function(nx = 20, ny = 20) {
  K <- nx * ny
  lab <- sprintf("Z%04d", 1:K)
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
  list(W = W, labels = lab)
}

# ---------- ICAR sampler with smoothness control ----------
# alpha = 1   → standard ICAR
# alpha > 1   → smoother field (emphasizes low-frequency eigenmodes)
icar_sample_sum_to_zero <- function(W, alpha = 1) {
  L <- diag(rowSums(W)) - as.matrix(W)
  eig <- eigen(L, symmetric = TRUE)
  lam <- pmax(eig$values, 1e-12)    # numerical guard
  U   <- eig$vectors
  lam_pos <- lam[-1]
  U_pos   <- U[, -1, drop = FALSE]
  z <- rnorm(length(lam_pos))
  phi <- as.numeric(U_pos %*% (z / (lam_pos^(alpha/2))))
  # enforce sum-to-zero and unit sd, then scale externally by lambda_zip
  phi <- phi - mean(phi)
  phi <- phi / sd(phi)
  phi
}

# ---------- Simulator (Stan-equivalent generator) ----------
simulate_from_stan_generative <- function(
    nx = 20, ny = 20,                # many ZIPs
    N_per_zip = 2,                   # sparse per ZIP boosts ICAR advantage
    N_race = 4, N_age = 5, N_time = 6,
    K = 2,                           # # of fixed effects
    beta = c(0.3, -0.2),             # smaller fixed effects
    intercept = 0.0,
    lambda_race = 0.10,              # reduce non-spatial variance
    lambda_age  = 0.10,
    lambda_time = 0.10,
    lambda_zip  = 2.0,               # stronger spatial signal
    alpha_icar  = 1.8,               # smoother spatial field
    n_trials    = 30
) {
  adj <- make_lattice_adj(nx, ny)
  W <- adj$W; zip_levels <- adj$labels
  N_zip <- length(zip_levels)
  
  # indices
  J_zip  <- rep(1:N_zip, each = N_per_zip)
  N      <- length(J_zip)
  J_race <- sample.int(N_race, N, TRUE)
  J_age  <- sample.int(N_age,  N, TRUE)
  J_time <- sample.int(N_time, N, TRUE)
  
  # covariates
  X <- matrix(rnorm(N * K), N, K); colnames(X) <- paste0("x", 1:K)
  
  # random effects
  a_race <- lambda_race * rnorm(N_race)
  a_age  <- lambda_age  * rnorm(N_age)
  a_time <- lambda_time * rnorm(N_time)
  z_zip  <- icar_sample_sum_to_zero(W, alpha = alpha_icar)
  a_zip  <- lambda_zip * z_zip
  
  # linear predictor
  eta <- intercept + as.numeric(X %*% beta) +
    a_race[J_race] + a_age[J_age] + a_time[J_time] + a_zip[J_zip]
  p <- plogis(eta)
  n_sample <- rep(n_trials, N)
  y <- rbinom(N, n_sample, p)
  
  dat <- data.frame(
    y = y, n_sample = n_sample, X,
    race = factor(J_race),
    age  = factor(J_age),
    time = factor(J_time),
    zip  = factor(zip_levels[J_zip], levels = zip_levels)
  )
  attr(dat, "W") <- W
  dat
}

# ---------- ZIP-blocked folds (k-fold CV by area) ----------
make_folds_by_zip <- function(dat, K = 10, seed = 123) {
  set.seed(seed)
  zips <- levels(dat$zip)
  grp  <- sample(rep(1:K, length.out = length(zips)))
  grp[as.integer(dat$zip)]
}

# ---------- Run one tuned scenario ----------
dat <- simulate_from_stan_generative(
  nx = 20, ny = 20,
  N_per_zip = 2,
  beta = c(0.3, -0.2),
  lambda_race = 0.1, lambda_age = 0.1, lambda_time = 0.1,
  lambda_zip = 2.0,
  alpha_icar = 1.8,
  n_trials = 30
)
W <- attr(dat, "W")

fit_icar <- brm(
  y | trials(n_sample) ~ 1 + x1 + x2 + (1|race) + (1|age) + (1|time) +
    car(W, gr = zip, type = "icar"),
  data = dat, data2 = list(W = W),
  family = binomial(), seed = 123,
  chains = 4, cores = 4, iter = 2000, warmup = 1000,
  control = list(adapt_delta = 0.95, max_treedepth = 12)
)

fit_iid <- brm(
  y | trials(n_sample) ~ 1 + x1 + x2 + (1|race) + (1|age) + (1|time) +
    (1|zip),
  data = dat, family = binomial(), seed = 123,
  chains = 4, cores = 4, iter = 2000, warmup = 1000,
  control = list(adapt_delta = 0.95, max_treedepth = 12)
)

# Pointwise LOO (often closer)
print(loo_compare(loo(fit_icar), loo(fit_iid)))

# ZIP-blocked k-fold (reveals spatial advantage)
# folds <- make_folds_by_zip(dat, K = 10, seed = 1)
# k_icar <- kfold(fit_icar, folds = folds)
# k_iid  <- kfold(fit_iid,  folds = folds)
# print(kfold_compare(k_icar, k_iid))

# Optional: Moran's I on ZIP-averaged residuals
# lw <- mat2listw(as.matrix(W), style = "W")
# r_icar <- residuals(fit_icar, type = "pearson")[,1]
# r_iid  <- residuals(fit_iid,  type = "pearson")[,1]
# rez_zip <- function(zip, r) aggregate(r, list(zip), mean)[,2]
# cat("\nMoran's I (ICAR resids by ZIP):\n"); print(moran.test(rez_zip(dat$zip, r_icar), lw))
# cat("\nMoran's I (IID  resids by ZIP):\n"); print(moran.test(rez_zip(dat$zip, r_iid),  lw))
