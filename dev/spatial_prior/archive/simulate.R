# packages
library(Matrix)
library(cmdstanr)
library(posterior)
library(dplyr)
library(tidyr)
library(ggplot2)

set.seed(42)

#----- Build a graph with 2 chains + 1 isolate (C = 3)
make_components <- function(sizes = c(40, 30, 1)) {
  N <- sum(sizes)
  edges <- list()
  start <- 1
  comp_index <- vector("list", length(sizes))
  for (c in seq_along(sizes)) {
    n <- sizes[c]
    idx <- start:(start + n - 1)
    comp_index[[c]] <- idx
    # chain edges within component
    if (n >= 2) {
      e <- cbind(idx[-n], idx[-1])
      edges[[length(edges) + 1]] <- e
    }
    start <- start + n
  }
  E <- if (length(edges)) do.call(rbind, edges) else matrix(numeric(), 0, 2)
  list(N = N,
       node1 = as.integer(E[,1]),
       node2 = as.integer(E[,2]),
       comp_index = comp_index)
}

g <- make_components(c(40, 30, 1))  # tweak sizes here
N_zip <- g$N
E <- cbind(g$node1, g$node2)
N_edges <- nrow(E)

#----- Laplacian Q
Q <- Matrix(0, N_zip, N_zip, sparse = TRUE)
if (N_edges > 0) {
  for (k in 1:N_edges) {
    i <- E[k,1]; j <- E[k,2]
    Q[i,i] <- Q[i,i] + 1
    Q[j,j] <- Q[j,j] + 1
    Q[i,j] <- Q[i,j] - 1
    Q[j,i] <- Q[j,i] - 1
  }
}

#----- Sample an ICAR field with per-component sum-to-zero (for data gen)
sample_icar <- function(Q, comp_index, jitter = 1e-6) {
  N <- nrow(Q)
  phi <- numeric(N)
  for (idx in comp_index) {
    Lc <- as.matrix(Q[idx, idx])
    # properize slightly and sample MVN(0, (Lc + eps I)^-1)
    ev <- eigen(Lc + diag(jitter, nrow(Lc)), symmetric = TRUE)
    vals <- pmax(ev$values, 1e-12)
    z <- rnorm(length(vals))
    phi[idx] <- ev$vectors %*% (z / sqrt(vals))
    # hard center per component
    phi[idx] <- phi[idx] - mean(phi[idx])
  }
  as.numeric(phi)
}

phi <- sample_icar(Q, g$comp_index)
# scale to convenient marginal sd
phi <- phi / sd(phi)

#----- Simulate covariates and outcomes
N      <- N_zip
K      <- 1
X      <- matrix(rnorm(N*K), N, K)
beta   <- 0.6
alpha  <- -1.5
sigma_zip <- 1.0

eta <- alpha + X %*% beta + sigma_zip * phi
p   <- 1/(1 + exp(-eta))
n_sample <- rep(100L, N)
y <- rbinom(N, n_sample, p)

# simple factor for "race" just to match your earlier structure
N_race <- 5L
J_race <- sample.int(N_race, N, replace = TRUE)

# Build per-component indexing arrays for Stan
C <- length(g$comp_index)
comp_sizes <- sapply(g$comp_index, length)
pad <- max(comp_sizes)
comp_index_mat <- matrix(0L, nrow = C, ncol = pad)
for (c in 1:C) comp_index_mat[c, 1:comp_sizes[c]] <- g$comp_index[[c]]

stan_data_common <- list(
  N = N,
  K = K,
  X = X,
  y = as.integer(y),
  n_sample = as.integer(n_sample),
  N_race = N_race,
  J_race = as.array(J_race),
  N_zip = N_zip,
  J_zip = as.array(1:N_zip),
  N_edges = N_edges,
  node1 = if (N_edges) as.array(E[,1]) else array(0L, 0),
  node2 = if (N_edges) as.array(E[,2]) else array(0L, 0),
  C = C,
  comp_sizes = as.array(comp_sizes),
  comp_index = comp_index_mat
)
