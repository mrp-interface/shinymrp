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

g <- make_components(c(20, 20, 20, 20, 1, 1))  # tweak sizes here
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

path <- "/Users/tntoan/Desktop/repos/shinymrp/dev/spatial_prior/"

# Compile
modG <- cmdstan_model(paste0(path, "icar_global.stan"), cpp_options = list(stan_threads = TRUE))
modC <- cmdstan_model(paste0(path, "icar_percomp.stan"), cpp_options = list(stan_threads = TRUE))

# Fit
fitG <- modG$sample(data = stan_data_common, chains = 4, parallel_chains = 4, threads_per_chain = 1,
                    iter_warmup = 1000, iter_sampling = 1000,
                    adapt_delta = 0.9, max_treedepth = 12)
fitC <- modC$sample(data = stan_data_common, chains = 4, parallel_chains = 4, threads_per_chain = 1,
                    iter_warmup = 1000, iter_sampling = 1000,
                    adapt_delta = 0.9, max_treedepth = 12)

# Draws
drawsG <- as_draws_df(fitG$draws())
drawsC <- as_draws_df(fitC$draws())

# Helper: component means of phi
comp_means <- function(df, prefix = "phi_zip[", comp_index) {
  out <- lapply(seq_along(comp_index), function(c) {
    idx <- comp_index[[c]]
    cols <- paste0("phi_zip[", idx, "]")
    tibble(.chain = df$.chain, .draw = df$.draw,
           comp = paste0("C", c),
           mean_phi = rowMeans(df[, cols, drop = FALSE]))
  })
  bind_rows(out)
}

cmG <- comp_means(drawsG, comp_index = g$comp_index)
cmC <- comp_means(drawsC, comp_index = g$comp_index)

# 3.1 Posterior spread of component means (should be ~0 for per-component)
p1 <- bind_rows(
  mutate(cmG, model = "Global constraint"),
  mutate(cmC, model = "Per-component")
) %>%
  ggplot(aes(x = mean_phi, fill = model)) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 60) +
  facet_wrap(~comp, scales = "free_y") +
  labs(title = "Posterior of component means of phi",
       x = "component mean(phi_zip)", y = "frequency") +
  theme_bw()

print(p1)

# 3.2 Correlation of intercept with component means (confounding signal)
corr_df <- function(draws, cm) {
  draws %>% select(.draw, intercept) %>%
    inner_join(cm, by = ".draw") %>%
    group_by(comp) %>%
    summarise(cor_ic = cor(intercept, mean_phi))
}
corrG <- corr_df(drawsG, cmG) %>% mutate(model = "Global constraint")
corrC <- corr_df(drawsC, cmC) %>% mutate(model = "Per-component")
print(bind_rows(corrG, corrC))

# 3.3 Sampler diagnostics
diag_tbl <- function(fit) {
  summ <- fit$summary()
  summ %>%
    select(variable, rhat, ess_bulk, ess_tail) %>%
    arrange(desc(rhat)) %>%
    head(15)
}
diagG <- diag_tbl(fitG)
diagC <- diag_tbl(fitC)
print(diagG); print(diagC)

# 3.4 Divergences, treedepth
print(data.frame(
  model = c("Global", "Per-component"),
  divergences = c(sum(fitG$diagnostic_summary()$num_divergent),
                  sum(fitC$diagnostic_summary()$num_divergent)),
  max_treedepth_hits = c(sum(fitG$diagnostic_summary()$num_max_treedepth),
                         sum(fitC$diagnostic_summary()$num_max_treedepth))
))

