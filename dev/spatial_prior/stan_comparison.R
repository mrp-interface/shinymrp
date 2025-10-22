# ===============================================================
suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(loo)
  library(Matrix)
  library(dplyr)
  library(spdep)
})

seed <- sample(1e6, 1)
seed <- 816291
set.seed(seed)
options(mc.cores = max(1, parallel::detectCores() - 1))

# ------- Utility: lattice adjacency + labels ------
make_lattice_adj <- function(nx = 20, ny = 20) {
  K <- nx * ny
  lab <- sprintf("Z%04d", seq_len(K))
  W <- Matrix(0, K, K, sparse = FALSE)
  idx <- function(x, y) (y - 1L) * nx + x
  for (x in 1:nx) for (y in 1:ny) {
    i <- idx(x, y)
    if (x > 1)  W[i, idx(x - 1, y)] <- 1
    if (x < nx) W[i, idx(x + 1, y)] <- 1
    if (y > 1)  W[i, idx(x, y - 1)] <- 1
    if (y < ny) W[i, idx(x, y + 1)] <- 1
  }
  W <- Matrix(as.numeric(W), nrow = K, ncol = K, sparse = FALSE)
  rownames(W) <- colnames(W) <- lab
  
  # undirected edge list (upper triangle, 1-based for Stan)
  E <- which(upper.tri(W) & (W == 1), arr.ind = TRUE)
  node1 <- as.integer(E[, 1])
  node2 <- as.integer(E[, 2])
  list(W = W, labels = lab, node1 = node1, node2 = node2)
}

# ------- ICAR sampler in sum-to-zero subspace with smoothness ------
# alpha = 1 is standard ICAR; alpha > 1 emphasizes smoother, low-frequency modes
icar_sample_sum_to_zero <- function(W, alpha = 1) {
  L <- diag(rowSums(W)) - as.matrix(W)
  eig <- eigen(L, symmetric = TRUE)
  lam <- pmax(eig$values, 1e-12)     # numerical guard
  U   <- eig$vectors
  lam_pos <- lam[-1]
  U_pos   <- U[, -1, drop = FALSE]
  z <- rnorm(length(lam_pos))
  phi <- as.numeric(U_pos %*% (z / (lam_pos^(alpha / 2))))
  phi <- phi - mean(phi)
  phi <- phi / sd(phi)               # unit sd; scale externally by lambda_zip
  print(phi[1:10])
  phi
}

# eta = intercept + X beta + a_race[J_race] + a_age[J_age] + a_time[J_time] + a_zip[J_zip]
# a_grp = lambda_grp * z_grp, with z_grp ~ N(0, I). For ZIP: z_zip ~ ICAR(sum-to-zero)
simulate_stan_equiv <- function(
    nx = 20, ny = 20, N_per_zip = 2,
    N_race = 4, N_age = 5, N_time = 6, K = 1,
    beta = c(-0.2),
    intercept = 0.0,
    lambda_race = 0.3, lambda_age = 0.5, lambda_time = 0.6,
    lambda_zip = 0.8, alpha_icar = 1.8,
    n_trials = 30
) {
  adj <- make_lattice_adj(nx, ny)
  W <- adj$W; node1 <- adj$node1; node2 <- adj$node2; zip_levels <- adj$labels
  N_zip <- length(zip_levels)
  
  J_zip  <- rep(seq_len(N_zip), each = N_per_zip)
  N      <- length(J_zip)
  J_race <- sample.int(N_race, N, TRUE)
  J_age  <- sample.int(N_age,  N, TRUE)
  J_time <- sample.int(N_time, N, TRUE)
  
  X <- matrix(rnorm(N * K), N, K); colnames(X) <- paste0("x", 1:K)
  
  a_race <- lambda_race * rnorm(N_race)
  a_age  <- lambda_age  * rnorm(N_age)
  a_time <- lambda_time * rnorm(N_time)
  z_zip  <- icar_sample_sum_to_zero(W, alpha = alpha_icar)
  a_zip  <- lambda_zip * z_zip
  
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
  
  stan_common <- list(
    N = as.integer(N),
    K = as.integer(K),
    X = X,
    y = as.array(as.integer(y)),
    n_sample = as.array(as.integer(n_sample)),
    N_race = as.integer(N_race),
    J_race = as.array(as.integer(J_race)),
    N_age  = as.integer(N_age),
    J_age  = as.array(as.integer(J_age)),
    N_time = as.integer(N_time),
    J_time = as.array(as.integer(J_time)),
    N_zip  = as.integer(N_zip),
    J_zip  = as.array(as.integer(J_zip))
  )
  
  stan_icar <- c(
    stan_common,
    list(
      N_edges_zip = as.integer(length(node1)),
      node1_zip = as.array(as.integer(node1)),
      node2_zip = as.array(as.integer(node2))
    )
  )
  
  list(
    data = dat,
    W = W,
    stan_icar = stan_icar,
    stan_iid = stan_common,   # IID model doesn't need edges
    # keep true generative pieces if you want diagnostics later
    true = list(beta = beta, intercept = intercept,
                a_race = a_race, a_age = a_age, a_time = a_time, a_zip = a_zip)
  )
}

# --- Metrics: Moran's I, Geary's C, edge correlation, energy identity ---
moran_I <- function(W, phi) {
  phi_c <- phi - mean(phi)
  S0 <- sum(W)
  N  <- length(phi)
  as.numeric((N / S0) * (t(phi_c) %*% W %*% phi_c) / (t(phi_c) %*% phi_c))
}

geary_C <- function(W, phi) {
  phi_c <- phi - mean(phi)
  S0 <- sum(W); N <- length(phi)
  num <- 0
  # only upper triangle to avoid double counting
  E <- which(upper.tri(W) & W > 0, arr.ind = TRUE)
  for (k in seq_len(nrow(E))) {
    i <- E[k,1]; j <- E[k,2]
    num <- num + W[i,j] * (phi[i] - phi[j])^2
  }
  num <- 2 * num  # account for symmetry
  as.numeric(((N - 1) / (2 * S0)) * num / sum(phi_c^2))
}

edge_corr <- function(W, phi) {
  E <- which(upper.tri(W) & W > 0, arr.ind = TRUE)
  cor(phi[E[,1]], phi[E[,2]])
}

energy_identity <- function(W, phi) {
  Q <- diag(rowSums(W)) - W
  E1 <- as.numeric(t(phi) %*% Q %*% phi)
  E <- which(upper.tri(W) & W > 0, arr.ind = TRUE)
  E2 <- sum((phi[E[,1]] - phi[E[,2]])^2)
  list(phi_Q_phi = E1, sum_edge_diffs_sq = E2, diff = E1 - E2)
}

plot_moran <- function(W, phi) {
  d <- rowSums(W); d[d == 0] <- 1
  lag_phi <- as.numeric(W %*% phi / d)
  plot(phi, lag_phi, pch = 16, cex = 0.6,
       xlab = expression(phi), ylab = expression(W*phi / degree),
       main = "Moran scatterplot")
  abline(lm(lag_phi ~ phi), lwd = 2)
  abline(0, 1, lty = 2)
}

plot_lattice_heatmap <- function(phi, nx, ny,
                                 main = "Lattice heatmap (lower-left origin)") {
  M <- matrix(phi, nrow = ny, ncol = nx, byrow = TRUE)
  # Flip vertically so row 1 (y=1) is at the bottom like a grid
  M <- M[ny:1, , drop = FALSE]
  image(t(M[nrow(M):1, ])) # base image expects y to grow upward; transpose for x-fast
  title(main = main); box()
}


# ===============================================================
# 1) Simulate data
# ===============================================================
sim <- simulate_stan_equiv(
  nx = 10, ny = 10, N_per_zip = 2,
  K = 1, beta = c(-0.2),
  intercept = 0.0,
  lambda_race = 0.2, lambda_age = 0.15, lambda_time = 0.3,
  lambda_zip = 1.8, alpha_icar = 1.0,
  n_trials = 30
)
dat <- sim$data
W   <- sim$W
stan_icar <- sim$stan_icar
stan_iid  <- sim$stan_iid

# Numbers:
phi <- sim$true$a_zip
mI  <- moran_I(W, phi)
gC  <- geary_C(W, phi)
r_e <- edge_corr(W, phi)
en  <- energy_identity(W, phi)
print(list(Moran_I = mI, Geary_C = gC, Edge_corr = r_e, Energy_identity = en))

# Plots:
plot_moran(W, phi)
plot_lattice_heatmap(phi, nx = 10, ny = 10)     # if you used a lattice

# # ===============================================================
# # 2) Stan file paths and compile
# # ===============================================================
# path <- "/Users/tntoan/Desktop/repos/shinymrp/dev/spatial_prior/"
# stan_path_icar <- paste0(path, "icar.stan")
# stan_path_iid <- paste0(path, "iid.stan")
# 
# stopifnot(file.exists(stan_path_icar), file.exists(stan_path_iid))
# cat("\nCompiling models...\n")
# mod_icar <- cmdstan_model(stan_path_icar)
# mod_iid  <- cmdstan_model(stan_path_iid)
# 
# # ===============================================================
# # 3) Sample
# # ===============================================================
# common_mcmc <- list(
#   seed = NULL,
#   chains = 4, parallel_chains = 4,
#   iter_warmup = 1000, iter_sampling = 1000,
#   refresh = 200,
#   adapt_delta = 0.95, max_treedepth = 15
# )
# 
# fit_icar <- mod_icar$sample(
#   data = stan_icar,
#   seed = common_mcmc$seed,
#   chains = common_mcmc$chains,
#   parallel_chains = common_mcmc$parallel_chains,
#   iter_warmup = common_mcmc$iter_warmup,
#   iter_sampling = common_mcmc$iter_sampling,
#   refresh = common_mcmc$refresh,
#   adapt_delta = common_mcmc$adapt_delta,
#   max_treedepth = common_mcmc$max_treedepth
# )
# 
# fit_iid <- mod_iid$sample(
#   data = stan_icar,
#   seed = common_mcmc$seed,
#   chains = common_mcmc$chains,
#   parallel_chains = common_mcmc$parallel_chains,
#   iter_warmup = common_mcmc$iter_warmup,
#   iter_sampling = common_mcmc$iter_sampling,
#   refresh = common_mcmc$refresh,
#   adapt_delta = common_mcmc$adapt_delta,
#   max_treedepth = common_mcmc$max_treedepth
# )
# 
# cat("\nICAR sampling summary:\n"); print(fit_icar$summary(c("lp__", "intercept", "lambda_zip"))[,1:9])
# cat("\nIID  sampling summary:\n"); print(fit_iid$summary(c("lp__", "intercept", "lambda_zip"))[,1:9])
# 
# # ===============================================================
# # LOO using generated-quantities: log_lik[n]
# # ===============================================================
# 
# extract_loglik_matrix <- function(fit, var = "log_lik") {
#   # Returns:
#   #   $mat   : (S x N) matrix of log-lik draws
#   #   $chain : length-S integer vector of chain IDs (for r_eff)
#   dd <- posterior::as_draws_df(fit$draws(var))
#   chain_id <- dd$.chain
#   keep <- grepl(paste0("^", var, "\\["), names(dd))
#   if (!any(keep)) stop(sprintf("Variable '%s' not found in draws.", var))
#   ll <- as.matrix(dd[, keep, drop = FALSE])
#   # Guard against non-finite (shouldn't happen with 0<p<1):
#   ll[!is.finite(ll)] <- -1e6
#   list(mat = ll, chain = chain_id)
# }
# 
# cat("\nComputing LOO from generated 'log_lik'...\n")
# ll_icar <- extract_loglik_matrix(fit_icar, "log_lik")
# ll_iid  <- extract_loglik_matrix(fit_iid,  "log_lik")
# 
# r_eff_icar <- loo::relative_eff(exp(ll_icar$mat), chain_id = ll_icar$chain)
# r_eff_iid  <- loo::relative_eff(exp(ll_iid$mat),  chain_id = ll_iid$chain)
# 
# loo_icar <- loo::loo(ll_icar$mat, r_eff = r_eff_icar)
# loo_iid  <- loo::loo(ll_iid$mat,  r_eff = r_eff_iid)
# 
# cat("\n===== PSIS-LOO comparison =====\n")
# print(loo::loo_compare(loo_icar, loo_iid))
