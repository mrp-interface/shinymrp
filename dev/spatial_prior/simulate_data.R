library(Matrix)

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
  W <- (W + t(W)) > 0
  W <- Matrix(as.numeric(W), nrow = K, ncol = K, sparse = FALSE)
  rownames(W) <- colnames(W) <- lab
  
  # undirected edge list (upper triangle, 1-based for Stan)
  E <- which(upper.tri(W) & (W == 1), arr.ind = TRUE)
  node1 <- as.integer(E[, 1])
  node2 <- as.integer(E[, 2])
  list(W = W, labels = lab, node1 = node1, node2 = node2)
}

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
  phi
}

# bym2_scale.R
# Compute BYM2 scaling factor s from a 1-based adjacency list (connected graph).
# node1, node2: integer vectors of length E (1..N); each pair is an undirected edge.
# N: number of nodes.
#
# Returns: a single number s so that GM(diag(Q^+)) == 1 when precision is s*Q.

bym2_scale <- function(node1, node2, N) {
  stopifnot(length(node1) == length(node2))
  E <- length(node1)

  # Build sparse adjacency (binary, symmetric)
  # Note: add both (i,j) and (j,i) then coerce to 0/1.
  i <- c(node1, node2)
  j <- c(node2, node1)
  x <- rep(1, 2 * E)

  # Base R sparse via Matrix
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Please install the 'Matrix' package.")
  }
  W <- Matrix::sparseMatrix(i = i, j = j, x = x, dims = c(N, N))
  W@x[] <- 1  # ensure binary

  d <- Matrix::rowSums(W)
  Q <- Matrix::Diagonal(x = as.numeric(d)) - W  # graph Laplacian

  # Eigen-decompose Q (connected graph => one zero eigenvalue)
  # Use base eigen on dense if N is small; for larger N, consider RSpectra.
  Qd <- as.matrix(Q)
  eig <- eigen(Qd, symmetric = TRUE, only.values = FALSE)
  lam <- eig$values
  U   <- eig$vectors

  # Drop the first (near-zero) eigenvalue/eigenvector
  # Sort ascending just in case (eigen usually returns descending).
  ord <- order(lam)
  lam <- lam[ord]
  U   <- U[, ord, drop = FALSE]

  # Exclude the zero eigen (index 1 for connected graph)
  lam_pos <- lam[-1]
  U_pos   <- U[, -1, drop = FALSE]

  # Moore–Penrose inverse: Q^+ = U_pos diag(1/lam_pos) U_pos^T
  # only need the diagonal entries of Q^+.
  inv_lam <- 1 / lam_pos
  # diag(Q^+) = row-wise dot of U_pos * inv_lam with U_pos
  # (efficient computation without forming the full matrix)
  v <- rowSums((U_pos^2) %*% diag(inv_lam, nrow = length(inv_lam)))

  # Geometric-mean scaling: s = exp(mean(log(diag(Q^+))))
  s <- exp(mean(log(v)))
  return(as.numeric(s))
}

simulate_stan_equiv <- function(
    nx = 20, ny = 20, N_per_zip = 2,
    N_race = 3, N_age = 5, N_time = 6, K = 1,
    beta = c(-0.2),
    intercept = 0.0,
    lambda_race = 0.3, lambda_age = 0.5, lambda_time = 0.6,
    lambda_zip = 0.8, alpha_icar = 1.8,
    n_trials = 30
) {
  adj <- make_lattice_adj(nx, ny)
  W <- adj$W
  node1 <- adj$node1
  node2 <- adj$node2
  zip_levels <- adj$labels
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
    y = y,
    n_sample = n_sample,
    X,
    race = factor(J_race),
    age  = factor(J_age),
    time = factor(J_time),
    zip  = factor(zip_levels[J_zip], levels = zip_levels)
  )
  
  stan_data <- list(
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
    J_zip  = as.array(as.integer(J_zip)),
    N_edges_zip = as.integer(length(node1)),
    node1_zip = as.array(as.integer(node1)),
    node2_zip = as.array(as.integer(node2)),
    scale_factor = bym2_scale(as.integer(node1), as.integer(node2), N_zip)
  )
  
  list(
    data = dat,
    stan_data = stan_data,
    W = W,
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
      lambda_zip = lambda_zip
    )
  )
}

format_for_mrp <- function(sim) {
  dat <- sim$data |>
    dplyr::mutate(
      sex = factor(as.numeric(x1 > 0), labels = c("female", "male")),
      race = factor(as.numeric(race), labels = c("white", "black", "other")),
      age = factor(as.numeric(age), labels = c("18-29", "30-44", "45-59", "60-74", "75+")),
      zip = 
      time = factor
      positive = y,
      total = n_sample
    )
  dat |> select(all_of(c("sex", "race", "age", "time", "zip", "positive", "total")))
}

nx <- 10
ny <- 10
sim <- simulate_stan_equiv(
  nx = nx, ny = ny,
  N_per_zip = 2,
  K = 1, beta = c(-0.2),
  intercept = 0.0,
  lambda_race = 0.2,
  lambda_age = 0.15,
  lambda_time = 0.3,
  lambda_zip = 1.8,
  alpha_icar = 1.0,
  n_trials = 30
)

df <- format_for_mrp(sim)
View(df)

