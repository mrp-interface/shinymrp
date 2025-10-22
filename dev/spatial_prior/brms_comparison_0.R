# =========================================================
# ICAR vs IID sanity-check with brms + cmdstanr
# =========================================================
# Packages
library(brms)       # >= 2.22
library(cmdstanr)
library(Matrix)
library(spdep)
library(loo)
set.seed(123)

# Use cmdstanr backend
options(brms.backend = "cmdstanr")
# cmdstanr::check_cmdstan_toolchain()  # uncomment first time
# cmdstanr::install_cmdstan()          # uncomment first time

# ---------- Helpers ----------
# 4-neighbor lattice adjacency (nx * ny grid)
make_W_lattice <- function(nx = 12, ny = 12) {
  K <- nx * ny
  W <- Matrix(0, K, K, sparse = FALSE)
  id <- function(x, y) (y - 1L) * nx + x
  for (x in 1:nx) for (y in 1:ny) {
    i <- id(x, y)
    if (x > 1)  W[i, id(x - 1, y)] <- 1
    if (x < nx) W[i, id(x + 1, y)] <- 1
    if (y > 1)  W[i, id(x, y - 1)] <- 1
    if (y < ny) W[i, id(x, y + 1)] <- 1
  }
  W <- (W + t(W)) > 0  # symmetrize logical
  W <- Matrix(as.numeric(W), sparse = FALSE, nrow = K, ncol = K)
  rownames(W) <- colnames(W) <- sprintf("C%03d", 1:K)
  W
}

# Sample a *proper* CAR field (rho < 1) as a near-ICAR proxy, then center to sum=0
sample_proper_car <- function(W, rho = 0.99, tau = 1) {
  K <- nrow(W)
  d <- rowSums(W)
  Q <- tau * (diag(d) - rho * as.matrix(W))  # positive definite for 0<rho<1
  # Draw phi ~ N(0, Q^{-1}) via dense chol (K ~ 144 by default -> fast)
  R <- chol(Q)                      # upper-triangular
  z <- rnorm(K)
  phi <- backsolve(R, z)            # R %*% phi = z  -> phi ~ N(0, Q^{-1})
  phi <- as.numeric(phi) - mean(phi)  # impose sum-to-zero
  phi
}

# Simulate binomial outcomes with spatial structure
simulate_icar_binomial <- function(nx = 12, ny = 12, rho = 0.99, tau = 1,
                                   beta = c(0.6, -0.5), ntrials = 50) {
  W   <- make_W_lattice(nx, ny)
  K   <- nrow(W)
  geo <- factor(rownames(W), levels = rownames(W))

  x1 <- rnorm(K)
  x2 <- rnorm(K)
  phi <- sample_proper_car(W, rho = rho, tau = tau)

  eta  <- 0.2 + beta[1]*scale(x1)[,1] + beta[2]*scale(x2)[,1] + phi
  p    <- plogis(eta)
  size <- rep(ntrials, K)
  y    <- rbinom(K, size, p)

  dat <- data.frame(y, size, x1, x2, geo)
  attr(dat, "W")       <- W
  attr(dat, "phi_true")<- phi
  dat
}

# ---------- Simulate ----------
dat <- simulate_icar_binomial(nx = 12, ny = 12, rho = 0.99, tau = 1, ntrials = 50)
W   <- attr(dat, "W")
phi_true <- attr(dat, "phi_true")

# ---------- Fit models ----------
# ICAR model (only the ICAR component, not BYM/BYM2)
fit_icar <- brm(
  formula = y | trials(size) ~ 1 + x1 + x2 + car(W, gr = geo, type = "icar"),
  data = dat,
  data2 = list(W = W),
  family = binomial(),
  seed = 123,
  iter = 2000, warmup = 1000, chains = 4, cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 12)
)

# IID control: same geography but independent random intercepts
fit_iid <- brm(
  formula = y | trials(size) ~ 1 + x1 + x2 + (1 | geo),
  data = dat,
  family = binomial(),
  seed = 123,
  iter = 2000, warmup = 1000, chains = 4, cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 12)
)

print(fit_icar)
print(fit_iid)

# ---------- Model comparison: out-of-sample fit ----------
loo_icar <- loo(fit_icar)
loo_iid  <- loo(fit_iid)
print(loo_compare(loo_icar, loo_iid))  # more negative elpd_diff favors the second; inspect

# ---------- Residual spatial autocorrelation (Moran's I) ----------
lw <- mat2listw(as.matrix(W), style = "W")
res_icar <- residuals(fit_icar, type = "pearson")[,1]
res_iid  <- residuals(fit_iid,  type = "pearson")[,1]

cat("\nMoran's I (ICAR residuals):\n"); print(moran.test(res_icar, lw))
cat("\nMoran's I (IID residuals):\n");  print(moran.test(res_iid,  lw))

# ---------- Did the ICAR recover the latent spatial effect? ----------
# Isolate the CAR contribution = linear predictor with all terms minus fixed-effects-only
eta_full  <- posterior_linpred(fit_icar, transform = FALSE)                 # draws x N
eta_fixed <- posterior_linpred(fit_icar, transform = FALSE, re_formula = ~0)
phi_post  <- eta_full - eta_fixed                                          # draws x N
phi_hat   <- colMeans(phi_post)                                             # posterior mean per geo

cat("\nCorrelation(true phi, ICAR-estimated spatial effect): ",
    cor(phi_true, phi_hat), "\n")
