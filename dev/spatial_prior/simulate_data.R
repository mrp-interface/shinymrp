# ---- Imports -----------------------------------------------------------------
library(Matrix)
library(cmdstanr)
library(bayesplot)
library(posterior)

# ---- FIXED: fast grid adjacency (numeric, no deprecated coercions) ----------
.make_grid_adj <- function(nx, ny) {
  stopifnot(nx >= 1L, ny >= 1L)
  if (nx * ny == 1L) return(Matrix::Matrix(0, 1, 1, sparse = TRUE))
  Tx <- Matrix::bandSparse(nx, k = c(-1, 1),
                           diag = list(rep.int(1, nx - 1L), rep.int(1, nx - 1L)))
  Ty <- Matrix::bandSparse(ny, k = c(-1, 1),
                           diag = list(rep.int(1, ny - 1L), rep.int(1, ny - 1L)))
  W  <- Matrix::kronecker(Matrix::Diagonal(ny), Tx) +
    Matrix::kronecker(Ty, Matrix::Diagonal(nx))
  W@x[] <- 1L          # keep numeric 0/1; avoid logical sparse -> no deprecated as()
  Matrix::drop0(W)
}

# ---- FIXED: disconnected lattices + edge list (upper triangle only) ---------
make_disconnected_lattices <- function(components = list(c(20, 20)),
                                       n_isolates = 0L) {
  stopifnot(all(vapply(components, length, 1L) == 2L), n_isolates >= 0L)
  blocks <- lapply(components, function(x) .make_grid_adj(x[1], x[2]))
  W <- if (length(blocks)) Matrix::bdiag(blocks) else Matrix::Matrix(0, 0, 0, sparse = TRUE)
  if (n_isolates > 0L) {
    W <- Matrix::bdiag(W, Matrix::Matrix(0, n_isolates, n_isolates, sparse = TRUE))
  }
  # Ensure symmetric numeric sparse (dsCMatrix) and consistent dimnames
  W <- Matrix::forceSymmetric(W, uplo = "U")
  N <- nrow(W)
  lab <- sprintf("Z%04d", seq_len(N))
  dimnames(W) <- list(lab, lab)
  
  # Strict upper-triangle edge list (sparse-safe; no dense conversion)
  Up <- Matrix::triu(W, k = 1)
  E  <- Matrix::summary(Up)  # i, j, x; 1-based
  list(
    W      = W,
    labels = lab,
    node1  = as.integer(E$i),
    node2  = as.integer(E$j)
  )
}

.components_from_edges <- function(n, node1, node2) {
  if (n == 0L) return(list(comp_id = integer(), N_comps = 0L))
  adj <- vector("list", n)
  for (i in seq_len(n)) adj[[i]] <- integer(0)
  if (length(node1)) {
    for (k in seq_along(node1)) {
      a <- node1[k]; b <- node2[k]
      adj[[a]] <- c(adj[[a]], b)
      adj[[b]] <- c(adj[[b]], a)
    }
  }
  comp_id <- integer(n)
  comp <- 0L
  for (i in seq_len(n)) if (comp_id[i] == 0L) {
    comp <- comp + 1L
    q <- i
    while (length(q)) {
      v <- q[[1]]; q <- q[-1]
      if (comp_id[v] == 0L) {
        comp_id[v] <- comp
        if (length(adj[[v]])) q <- c(q, adj[[v]][comp_id[adj[[v]]] == 0L])
      }
    }
  }
  list(comp_id = as.integer(comp_id), N_comps = as.integer(comp))
}

bym2_basis <- function(node1, node2, N, alpha = 1, tol = 1e-10) {
  stopifnot(alpha > 0, N >= 0)
  
  # --- sanitize edges (1..N, no self-loops)
  node1 <- as.integer(node1); node2 <- as.integer(node2)
  keep  <- node1 >= 1L & node1 <= N & node2 >= 1L & node2 <= N & node1 != node2
  node1 <- node1[keep]; node2 <- node2[keep]
  
  # --- components from edges (works whether the helper returns a vector or a list)
  comp_res <- .components_from_edges(N, node1, node2)
  comp  <- if (is.list(comp_res)) comp_res$comp_id else comp_res
  ncomp <- if (is.list(comp_res)) comp_res$N_comps else if (length(comp)) max(comp) else 0L
  
  # --- symmetric numeric sparse adjacency and Laplacian
  Wg <- Matrix::sparseMatrix(i = c(node1, node2), j = c(node2, node1), x = 1, dims = c(N, N))
  Wg@x[] <- 1
  W <- Matrix::forceSymmetric(Wg, uplo = "U")
  d <- as.numeric(Matrix::rowSums(W))
  Q <- Matrix::Diagonal(x = d) - W
  
  # --- per-component eigen basis with BYM2 scaling: GM(diag((Q^α)^+)) = 1
  Rblocks <- list(); rows <- list()
  for (c in seq_len(ncomp)) {
    idx <- which(comp == c)
    if (length(idx) <= 1L) next  # isolates: all-zero rows in R
    Qc <- as.matrix(Q[idx, idx, drop = FALSE])
    ee <- eigen(Qc, symmetric = TRUE)
    lam <- ee$values; U <- ee$vectors
    pos <- lam > tol
    if (!any(pos)) next
    lam <- lam[pos]; U <- U[, pos, drop = FALSE]
    
    w   <- lam^(-alpha)
    v   <- rowSums(U^2 * rep(w, each = nrow(U)))  # diag((Q^α)^+)
    s_c <- exp(mean(log(v)))                      # BYM2 scale per component
    Rc  <- U %*% diag(lam^(-alpha / 2), nrow = length(lam))
    Rc  <- Rc / sqrt(s_c)
    
    Rblocks[[length(Rblocks) + 1L]] <- Rc
    rows[[length(rows) + 1L]]       <- idx
  }
  
  Npos <- if (length(Rblocks)) sum(vapply(Rblocks, ncol, 1L)) else 0L
  R <- matrix(0, N, Npos); off <- 0L
  for (k in seq_along(Rblocks)) {
    idx <- rows[[k]]; p <- ncol(Rblocks[[k]])
    R[idx, (off + 1L):(off + p)] <- Rblocks[[k]]
    off <- off + p
  }
  
  list(R = R, N_pos = ncol(R), comp_id = comp)
}



# Sample ICAR field directly from the reduced-rank basis.
icar_sample <- function(R) {
  if (ncol(R) == 0L) return(rep(0, nrow(R)))
  as.numeric(R %*% rnorm(ncol(R)))
}

# Project X to be orthogonal to the span of R[J_zip, ] (vectorized; handles matrix X).
spatial_plus <- function(X, R, J_zip) {
  Rn  <- R[J_zip, , drop = FALSE]
  if (ncol(Rn) == 0L) return(X)
  RtR <- crossprod(Rn)
  L   <- chol(RtR + 1e-9 * diag(ncol(Rn)))
  coef <- backsolve(L, forwardsolve(t(L), crossprod(Rn, as.matrix(X))))
  X - Rn %*% coef
}

# ---- Simulation with disconnected graphs + BYM2 mixture ----------------------

simulate_stan_equiv_disconnected <- function(
  components = list(c(20, 20)), n_isolates = 0L, N_per_zip = 2L,
  N_race = 3L, N_age = 5L, N_time = 6L, K = 1L,
  alpha = 1,
  beta = c(-0.2),
  intercept = 0.0,
  lambda_race = 0.3, lambda_age = 0.5, lambda_time = 0.6,
  lambda_zip = 0.8, rho_zip = 0.5,
  n_trials = 30L,
  seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  adj <- make_disconnected_lattices(components, n_isolates)
  W <- adj$W; node1 <- adj$node1; node2 <- adj$node2; zip_levels <- adj$labels
  N_zip <- length(zip_levels)

  # Reduced-rank ICAR basis (disconnected-aware) + BYM2 scaling
  basis <- bym2_basis(node1, node2, N_zip, alpha)
  R <- basis$R; N_pos <- basis$N_pos

  # Structured + iid pieces for BYM2
  phi   <- icar_sample(R)                       # GM(marg var) ≈ 1
  theta <- rnorm(N_zip)                         # iid N(0,1)
  z_zip <- sqrt(rho_zip) * phi + sqrt(1 - rho_zip) * theta
  a_zip <- lambda_zip * z_zip

  # Individuals
  J_zip  <- rep(seq_len(N_zip), each = N_per_zip)
  N      <- length(J_zip)
  J_race <- sample.int(N_race, N, TRUE)
  J_age  <- sample.int(N_age,  N, TRUE)
  J_time <- sample.int(N_time, N, TRUE)

  X <- if (K > 0) {
    m <- matrix(rnorm(N * K), N, K, dimnames = list(NULL, paste0("x", 1:K)))
    m
  } else matrix(0, N, 0)

  # Optional: remove spatial confounding (commented by default)
  # X <- spatial_plus(X, R, J_zip)

  a_race <- lambda_race * rnorm(N_race)
  a_age  <- lambda_age  * rnorm(N_age)
  a_time <- lambda_time * rnorm(N_time)

  linpred <- intercept +
    (if (K > 0) drop(X %*% beta) else 0) +
    a_race[J_race] + a_age[J_age] + a_time[J_time] + a_zip[J_zip]

  p        <- plogis(linpred)
  n_sample <- rep(as.integer(n_trials), N)
  y        <- rbinom(N, n_sample, p)

  dat <- data.frame(
    y = y, n_sample = n_sample,
    if (K > 0) X,
    race = factor(J_race),
    age  = factor(J_age),
    time = factor(J_time),
    zip  = factor(zip_levels[J_zip], levels = zip_levels)
  )

  stan_data <- list(
    N = as.integer(N),
    K = as.integer(K),
    X = if (K > 0) X else matrix(0, N, 0),
    y = as.array(as.integer(y)),
    n_sample = as.array(as.integer(n_sample)),
    N_race = as.integer(N_race),
    J_race = as.array(as.integer(J_race)),
    N_age  = as.integer(N_age),
    J_age  = as.array(as.integer(J_age)),
    N_time = as.integer(N_time),
    J_time = as.array(as.integer(J_time)),
    N_zip  = as.integer(N_zip),
    J_zip  = as.array(as.integer(J_zip)),
    N_pos  = as.integer(N_pos),
    R      = R
  )

  list(
    data = dat,
    stan_data = stan_data,
    W = W,
    basis = basis,
    true = list(
      beta = beta,
      intercept = intercept,
      a_race = a_race,
      a_age = a_age,
      a_time = a_time,
      a_zip = a_zip,
      lambda_race = lambda_race,
      lambda_age = lambda_age,
      lambda_time = lambda_time,
      lambda_zip = lambda_zip,
      rho_zip = rho_zip
    )
  )
}

# ---- Example usage -----------------------------------------------------------

sim <- simulate_stan_equiv_disconnected(
  components = list(c(20, 15), c(5, 10)),
  n_isolates = 10,
  N_per_zip = 5,
  beta = c(-0.2),
  intercept = 1.0,
  lambda_race = 0.3,
  lambda_age  = 0.4,
  lambda_time = 0.5,
  lambda_zip  = 0.8,
  rho_zip     = 0.5,
  seed = 123
)

stan_dir <- "/Users/tntoan/Desktop/repos/shinymrp/dev/spatial_prior/"
mod <- cmdstan_model(file.path(stan_dir, "bym2_multicomp.stan"),
                     cpp_options = list(stan_threads = TRUE))

fit <- mod$sample(
  data = sim$stan_data,
  chains = 4, parallel_chains = 4,
  iter_warmup = 1000, iter_sampling = 1000,
  threads_per_chain = 1,
  refresh = 200,
  max_treedepth = 12,
  adapt_delta = 0.95
)

# ---- Recovery plots (robust + aligned) --------------------------------------

params <- c("intercept", "beta[1]",
            "lambda_race", "lambda_age", "lambda_time",
            "lambda_zip", "rho_zip")
View(fit$summary(variables = params))
# Draws matrix (numeric); add derived columns for sigma_struct/iid
dm <- as_draws_matrix(fit$draws(variables = params))
dm <- cbind(
  dm,
  sigma_struct = dm[, "lambda_zip"] * sqrt(dm[, "rho_zip"]),
  sigma_iid    = dm[, "lambda_zip"] * sqrt(1 - dm[, "rho_zip"])
)

# Build TRUE vector aligned to colnames(dm)
true_vals <- c(
  intercept     = sim$true$intercept,
  `beta[1]`     = sim$true$beta[1],
  lambda_race   = sim$true$lambda_race,
  lambda_age    = sim$true$lambda_age,
  lambda_time   = sim$true$lambda_time,
  lambda_zip    = sim$true$lambda_zip,
  rho_zip       = sim$true$rho_zip,
  sigma_struct  = sim$true$lambda_zip * sqrt(sim$true$rho_zip),
  sigma_iid     = sim$true$lambda_zip * sqrt(1 - sim$true$rho_zip)
)[colnames(dm)]  # reorder to match dm

stopifnot(is.numeric(true_vals), length(true_vals) == ncol(dm))

print(mcmc_recover_hist(x = dm, true = true_vals))
print(mcmc_recover_intervals(x = dm, true = true_vals))
