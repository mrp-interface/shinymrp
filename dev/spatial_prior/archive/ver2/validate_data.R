# install.packages(c("Matrix","igraph")) if needed
library(Matrix)
library(igraph)



# 1) Constraint checks -------------------------------------------------------

check_global_sum_to_zero <- function(phi, tol = 1e-8) {
  s <- sum(phi)
  list(ok = abs(s) <= tol, sum = s)
}

check_per_component_zero <- function(phi, stan_graph, tol = 1e-8) {
  comp_sizes <- stan_graph$comp_sizes
  comp_index <- stan_graph$comp_index
  stopifnot(!is.null(comp_sizes), !is.null(comp_index))
  sums <- numeric(length(comp_sizes))
  for (c in seq_along(comp_sizes)) {
    m   <- comp_sizes[c]
    idx <- comp_index[seq_len(m), c]
    idx <- idx[idx != 0L]
    if (length(idx)) sums[c] <- sum(phi[idx])
  }
  list(ok = all(abs(sums) <= tol), sums = sums)
}

# 2) Energy identity & edgewise diagnostics ---------------------------------

icar_energy_and_edges <- function(phi, stan_graph) {
  mats <- icar_graph_mats(stan_graph)
  Q <- mats$Q
  e1 <- stan_graph$node1
  e2 <- stan_graph$node2

  # Energy two ways
  energy_quadratic <- as.numeric(crossprod(phi, as.numeric(Q %*% phi)))
  diffs <- phi[e1] - phi[e2]
  energy_edges <- sum(diffs^2)

  # Edge stats
  edge_cor <- suppressWarnings(cor(phi[e1], phi[e2]))
  mean_sq_edge_diff <- mean(diffs^2)
  list(
    energy_quadratic = energy_quadratic,
    energy_edges = energy_edges,
    energy_diff = energy_quadratic - energy_edges,
    edge_correlation = edge_cor,
    mean_sq_edge_diff = mean_sq_edge_diff
  )
}

# 3) Moran's I & Geary's C ---------------------------------------------------

morans_I <- function(phi, stan_graph) {
  mats <- icar_graph_mats(stan_graph)
  A <- mats$A
  W <- A
  S0 <- sum(W)
  n  <- length(phi)
  phi_c <- phi - mean(phi)
  num <- as.numeric(t(phi_c) %*% (W %*% phi_c))
  den <- as.numeric(sum(phi_c^2))
  I <- (n / S0) * (num / den)

  # Randomization expectation under spatial independence:
  E_I <- -1 / (n - 1)
  list(I = I, E_I = E_I)
}

gearys_C <- function(phi, stan_graph) {
  mats <- icar_graph_mats(stan_graph)
  A <- mats$A
  W <- A; S0 <- sum(W); n <- length(phi)
  phi_c <- phi - mean(phi)
  num <- 0
  # sum over edges (undirected counted once): use upper triangle of W
  trW <- summary(W) # (i,j,x)
  sel <- trW$i < trW$j & trW$x != 0
  num <- sum((phi[trW$i[sel]] - phi[trW$j[sel]])^2)
  den <- 2 * S0 * sum(phi_c^2)
  C <- (n - 1) * num / den
  list(C = C)
}

# 4) Neighbor-mean regression (should be ~1 slope, small residuals) ----------

neighbor_mean_regression <- function(phi, stan_graph) {
  mats <- icar_graph_mats(stan_graph); A <- mats$A; deg <- mats$deg
  n <- length(phi)
  nei_sum <- as.numeric(A %*% phi)
  has_nbr <- deg > 0
  mbar <- nei_sum[has_nbr] / deg[has_nbr]
  fit <- lm(phi[has_nbr] ~ 0 + mbar)  # no intercept: expect slope ~ 1
  # Theoretical conditional variance ~ 1/(tau^2 * deg_i) for unit-ICAR (tau=1)
  res <- phi[has_nbr] - coef(fit)[1] * mbar
  list(
    slope = unname(coef(fit)[1]),
    r2 = summary(fit)$r.squared,
    mean_abs_resid = mean(abs(res)),
    resid_sd = sd(res),
    deg_summary = summary(deg[has_nbr])
  )
}

# 5) Graph semivariogram (rising with distance) ------------------------------

# Standalone helper (outside the report, if you use it)
graph_semivariogram <- function(phi, stan_graph, max_h = 5, sample_pairs = 50000, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  # Build full adjacency so vertex ids are 1..N and isolates kept
  N  <- as.integer(stan_graph$N_nodes)
  A  <- Matrix::sparseMatrix(i = c(stan_graph$node1, stan_graph$node2),
                             j = c(stan_graph$node2, stan_graph$node1),
                             x = 1, dims = c(N, N))
  g  <- igraph::graph_from_adjacency_matrix(A, mode = "undirected")
  M  <- min(sample_pairs, N * N)

  i  <- sample.int(N, size = M, replace = TRUE)
  j  <- sample.int(N, size = M, replace = TRUE)

  # IMPORTANT: make v/to unique for distances(), then index back
  ui <- unique(i)
  uj <- unique(j)
  D  <- igraph::distances(g, v = ui, to = uj, weights = NA)
  d  <- D[cbind(match(i, ui), match(j, uj))]

  df <- data.frame(h = as.numeric(d), gamma = 0.5 * (phi[i] - phi[j])^2)
  out <- aggregate(gamma ~ h, data = subset(df, is.finite(h) & h <= max_h), FUN = mean)
  out[order(out$h), , drop = FALSE]
}



# 6) Benchmark vs non-spatial iid noise -------------------------------------

edge_energy_ratio_vs_iid <- function(phi, stan_graph, nrep = 500, seed = 1) {
  set.seed(seed)
  mats <- icar_graph_mats(stan_graph)
  e1 <- stan_graph$node1; e2 <- stan_graph$node2
  edge_energy <- function(x) mean((x[e1] - x[e2])^2)
  target <- edge_energy(phi)

  base_sd <- sd(phi)  # match marginal scale
  null_vals <- replicate(nrep, {
    x <- rnorm(length(phi), sd = base_sd)
    edge_energy(x)
  })
  list(
    target = target,
    null_mean = mean(null_vals),
    null_q05 = quantile(null_vals, 0.05),
    null_q95 = quantile(null_vals, 0.95)
  )
}

# ICAR validation report ------------------------------------------------------
# Requires: Matrix (always), igraph (only for the semivariogram)
icar_validate_report <- function(
  stan_graph,
  phi,
  nrep_iid      = 500,      # Monte-Carlo size for IID benchmark
  ridge         = 1e-8,     # tiny ridge for numerics in Q
  max_h         = 5,        # max graph distance for semivariogram
  sample_pairs  = 50000,    # pairs sampled for semivariogram
  seed          = 1,
  print_report  = TRUE
) {
  stopifnot(is.list(stan_graph), is.numeric(phi))
  if (!requireNamespace("Matrix", quietly = TRUE)) stop("Please install 'Matrix'.")
  has_igraph <- requireNamespace("igraph", quietly = TRUE)

  N  <- as.integer(stan_graph$N_nodes)
  e1 <- as.integer(stan_graph$node1)
  e2 <- as.integer(stan_graph$node2)
  if (length(phi) != N) stop("length(phi) must equal stan_graph$N_nodes")

  # ---------- helpers (local) ----------
  make_QA <- function() {
    A <- Matrix::sparseMatrix(i = c(e1, e2), j = c(e2, e1), x = 1, dims = c(N, N))
    d <- Matrix::rowSums(A)
    Q <- Matrix::Diagonal(x = as.numeric(d)) - A
    list(Q = Q, A = A, deg = as.numeric(d), S0 = sum(A))
  }
  stats_core <- function(phi, QA) {
    pc <- phi - mean(phi)
    # energy (two ways)
    e_quadr <- as.numeric(crossprod(pc, as.numeric(QA$Q %*% pc)))
    dphi    <- pc[e1] - pc[e2]
    e_edge  <- sum(dphi^2)
    # Moran's I
    num_I <- as.numeric(t(pc) %*% (QA$A %*% pc))
    den   <- sum(pc^2)
    I     <- (N / QA$S0) * (num_I / den)
    E_I   <- -1 / (N - 1)
    # Geary's C: use full directed W (A) so numerator counts both directions
    tr <- Matrix::summary(QA$A)
    num_C <- sum((pc[tr$i] - pc[tr$j])^2 * tr$x)
    C     <- ((N - 1) / (2 * QA$S0)) * (num_C / den)
    # Edges
    edge_cor  <- suppressWarnings(stats::cor(pc[e1], pc[e2]))
    edge_mse  <- mean(dphi^2)
    list(
      energy_quadratic = e_quadr,
      energy_edges     = e_edge,
      energy_diff      = e_quadr - e_edge,
      morans_I         = I,
      morans_EI        = E_I,
      gearys_C         = C,
      edge_correlation = edge_cor,
      edge_mse         = edge_mse
    )
  }
  neighbor_reg <- function(phi, QA) {
    pc  <- phi - mean(phi)
    nei <- as.numeric(QA$A %*% pc)
    deg <- QA$deg
    has <- deg > 0
    if (!any(has)) return(list(slope = NA_real_, r2 = NA_real_, resid_sd = NA_real_, n = 0))
    mbar <- nei[has] / deg[has]
    fit  <- stats::lm(pc[has] ~ 0 + mbar)
    r2   <- stats::summary.lm(fit)$r.squared      # <- was summary(fit)$r.squared
    res  <- stats::residuals(fit)
    list(
      slope    = unname(stats::coef(fit)[1]),
      r2       = r2,
      resid_sd = stats::sd(res),
      n        = sum(has)
    )
  }
  semivariogram_tbl <- function(phi) {
    if (!has_igraph) return(NULL)
    # Full adjacency -> keeps isolates; vertex ids = 1..N
    A <- Matrix::sparseMatrix(i = c(e1, e2), j = c(e2, e1), x = 1, dims = c(N, N))
    g <- igraph::graph_from_adjacency_matrix(A, mode = "undirected")

    M <- min(sample_pairs, N * N)
    i <- sample.int(N, size = M, replace = TRUE)
    j <- sample.int(N, size = M, replace = TRUE)

    # Make v/to unique to satisfy igraph
    ui <- unique(i)
    uj <- unique(j)
    D  <- igraph::distances(g, v = ui, to = uj, weights = NA)
    d  <- D[cbind(match(i, ui), match(j, uj))]

    df <- data.frame(h = as.numeric(d), gamma = 0.5 * (phi[i] - phi[j])^2, check.names = FALSE)
    df <- df[is.finite(df$h) & df$h <= max_h, ]
    if (!nrow(df)) return(NULL)
    agg <- stats::aggregate(gamma ~ h, data = df, FUN = mean)
    agg[order(agg$h), , drop = FALSE]
  }

  comp_sums <- function(phi) {
    cs <- stan_graph$comp_sizes
    ci <- stan_graph$comp_index
    if (is.null(cs) || is.null(ci)) return(NULL)
    sums <- numeric(length(cs))
    for (c in seq_along(cs)) {
      m <- cs[c]; if (m == 0) next
      idx <- ci[seq_len(m), c]; idx <- idx[idx != 0L]
      sums[c] <- sum(phi[idx])
    }
    sums
  }
  fmt <- function(x, d = 3) ifelse(is.na(x), "NA", formatC(x, digits = d, format = "f"))

  # ---------- compute ----------
  QA <- make_QA()
  phi_c <- as.numeric(phi - mean(phi))  # enforce global centering for fair comparisons

  graph_info <- list(
    N_nodes = N,
    N_edges = length(e1),
    N_comps = if (!is.null(stan_graph$N_comps)) stan_graph$N_comps else NA_integer_
  )

  constraint <- list(
    global_sum_ok = abs(sum(phi)) <= 1e-8,
    global_sum    = sum(phi),
    per_component_sums = comp_sums(phi)
  )

  core <- stats_core(phi_c, QA)
  neigh <- neighbor_reg(phi_c, QA)
  semi  <- semivariogram_tbl(phi_c)

  # IID benchmark (matched marginal SD)
  set.seed(seed)
  sd_match <- stats::sd(phi_c)
  draw_null <- function() {
    x <- stats::rnorm(N, sd = sd_match)
    x <- x - mean(x)
    s <- stats_core(x, QA)
    c(I = s$morans_I, C = s$gearys_C, edge_cor = s$edge_correlation, edge_mse = s$edge_mse)
  }
  null_mat <- t(replicate(nrep_iid, draw_null()))
  null_q   <- apply(null_mat, 2, stats::quantile, probs = c(.05, .25, .5, .75, .95))
  null_mu  <- colMeans(null_mat)

  # ---------- print ----------
  if (print_report) {
    cat("\n==================== ICAR validation ====================\n")
    cat(sprintf("[Graph] nodes:%d | edges:%d | comps:%s\n",
                graph_info$N_nodes, graph_info$N_edges,
                ifelse(is.na(graph_info$N_comps), "?", as.character(graph_info$N_comps))))
    cat("----------------------------------------------------------\n")
    cat(sprintf("Constraint (global sum): %s  (sum = %s)\n",
                ifelse(constraint$global_sum_ok, "OK", "FAIL"), fmt(constraint$global_sum, 6)))
    if (!is.null(constraint$per_component_sums)) {
      bad <- any(abs(constraint$per_component_sums) > 1e-8)
      cat(sprintf("Per-component sums: %s\n", ifelse(bad, "non-zero (info only)", "all ~ 0")))
    }
    cat(sprintf("Energy identity: ϕᵀQϕ = %s ; Σ_edges (Δϕ)^2 = %s ; diff = %s\n",
                fmt(core$energy_quadratic, 4), fmt(core$energy_edges, 4), fmt(core$energy_diff, 3)))
    cat(sprintf("Edge correlation (neighbors): %s ; mean edge MSE: %s\n",
                fmt(core$edge_correlation, 3), fmt(core$edge_mse, 4)))
    cat(sprintf("Moran's I: %s  (randomization E[I] ≈ %s)\n",
                fmt(core$morans_I, 3), fmt(core$morans_EI, 3)))
    cat(sprintf("Geary's C: %s  (C < 1 indicates positive autocorrelation)\n",
                fmt(core$gearys_C, 3)))
    cat(sprintf("Neighbor-mean regression: slope ≈ %s ; R² = %s ; resid SD = %s (n=%d)\n",
                fmt(neigh$slope, 3), fmt(neigh$r2, 3), fmt(neigh$resid_sd, 3), neigh$n))
    if (has_igraph && !is.null(semi) && nrow(semi)) {
      cat("Semivariogram (mean γ by graph distance h):\n")
      cat(paste0("  h=", semi$h, " → γ=", sprintf("%.3f", semi$gamma), collapse = "\n"), "\n")
    } else {
      cat("Semivariogram: (skip — install 'igraph' or no finite distances within max_h)\n")
    }
    cat("----------------------------------------------------------\n")
    cat(sprintf("IID benchmark (n=%d, matched SD):\n", nrep_iid))
    cat(sprintf("  Moran's I   : obs=%s | null mean=%s | 5–95%%=[%s, %s]\n",
                fmt(core$morans_I, 3), fmt(null_mu['I'], 3),
                fmt(null_q['5%', 'I'], 3), fmt(null_q['95%', 'I'], 3)))
    cat(sprintf("  Geary's C   : obs=%s | null mean=%s | 5–95%%=[%s, %s]\n",
                fmt(core$gearys_C, 3), fmt(null_mu['C'], 3),
                fmt(null_q['5%', 'C'], 3), fmt(null_q['95%', 'C'], 3)))
    cat(sprintf("  Edge corr   : obs=%s | null mean=%s | 5–95%%=[%s, %s]\n",
                fmt(core$edge_correlation, 3), fmt(null_mu['edge_cor'], 3),
                fmt(null_q['5%', 'edge_cor'], 3), fmt(null_q['95%', 'edge_cor'], 3)))
    cat(sprintf("  Edge MSE    : obs=%s | null mean=%s | 5–95%%=[%s, %s]\n",
                fmt(core$edge_mse, 4), fmt(null_mu['edge_mse'], 4),
                fmt(null_q['5%', 'edge_mse'], 4), fmt(null_q['95%', 'edge_mse'], 4)))
    cat("==========================================================\n")
  }

  # ---------- return structured results ----------
  list(
    graph      = graph_info,
    constraint = constraint,
    core_stats = core,
    neighbor   = neigh,
    semivariogram = semi,
    iid_null = list(
      means = null_mu,
      quantiles = null_q,
      n = nrep_iid
    )
  )
}


# zips <- c("48103","48104","48105","48108","48109","48197","48198")
# zip_graph <- .build_graph(geo_units = zips, geo_scale = "zip", verbose = TRUE)

# sg <- zip_graph$stan_graph
# z <- icar_sample_global(sg)$phi

# # Suppose 'sg' is your stan_graph, and 'z' is a unit-ICAR draw (sum-to-zero)
# # from the earlier sampler (icar_sample_global(...)$phi)
# # Optionally scale to your Stan a_zip with lambda:
# lambda <- 0.8
# a_zip  <- lambda * z

# # 1) Constraint:
# check_global_sum_to_zero(z)
# # -> ok should be TRUE, sum near 0

# # 2) Energy identity & edges:
# diag2 <- icar_energy_and_edges(z, sg)
# diag2$energy_diff      # ~ 0 (floating-point noise)
# diag2$edge_correlation # high and positive (often > 0.7)
# diag2$mean_sq_edge_diff# relatively small

# # 3) Spatial autocorrelation:
# morans_I(z, sg)  # I should be >> E[I] = -1/(n-1)
# gearys_C(z, sg)  # C typically < 1 for positive autocorrelation

# # 4) Neighbor-mean regression (core ICAR identity):
# neighbor_mean_regression(z, sg)
# # slope ≈ 1, R^2 high (often > 0.7), residual SD smaller when degrees are larger

# # 5) Graph semivariogram:
# graph_semivariogram(z, sg, max_h = 5)
# # gamma(h) should increase with h (monotone-ish rising trend)

# # 6) Compare to iid noise (control):
# edge_energy_ratio_vs_iid(z, sg)
# # target << null_mean and typically below null_q05

