# ---- Imports -----------------------------------------------------------------
library(Matrix)
library(cmdstanr)
library(bayesplot)
library(posterior)

.make_grid_adj <- function(nx, ny) {
  stopifnot(nx >= 1, ny >= 1)
  if (nx * ny == 1) return(Matrix::Matrix(0, 1, 1, sparse = TRUE))
  
  Tx <- Matrix::bandSparse(
    nx,
    k = c(-1, 1),
    diag = list(
      rep.int(1, nx - 1),
      rep.int(1, nx - 1)
    )
  )
  Ty <- Matrix::bandSparse(
    ny,
    k = c(-1, 1),
    diag = list(
      rep.int(1, ny - 1),
      rep.int(1, ny - 1)
    )
  )
  
  W  <- Matrix::Diagonal(ny) %x% Tx + Ty %x% Matrix::Diagonal(nx)
  W
}

.make_disconnected_lattices <- function(
    components = list(c(20, 20)),
    n_isolates = 0
) {
  stopifnot(
    all(vapply(components, length, 1L) == 2L),
    n_isolates >= 0
  )
  
  # Build block matrix from adjacency matrices
  blocks <- lapply(components, function(x) .make_grid_adj(x[1], x[2]))
  W <- Matrix::bdiag(blocks)
  iso_block <- Matrix::Matrix(0, n_isolates, n_isolates, sparse = TRUE)
  W <- Matrix::bdiag(W, iso_block)
  
  # Ensure symmetric numeric sparse (dsCMatrix) and consistent dimnames
  W <- Matrix::forceSymmetric(W, uplo = "U")
  N <- nrow(W)
  lab <- sprintf("Z%04d", seq_len(N))
  dimnames(W) <- list(lab, lab)
  
  # Strict upper-triangle edge list (sparse-safe; no dense conversion)
  Up <- Matrix::triu(W, k = 1)
  E  <- Matrix::summary(Up)
  
  list(
    W      = W,
    labels = lab,
    node1  = as.integer(E$i),
    node2  = as.integer(E$j)
  )
}

.components_from_edges <- function(n, node1, node2) {
  # No nodes
  if (n == 0L) {
    return(list(
      comp_id    = integer(),
      N_comps    = 0L,
      comp_sizes = integer(),
      connected  = integer(),
      singleton  = integer()
    ))
  }
  
  # Ensure integers (cheap + defensive)
  node1 <- as.integer(node1)
  node2 <- as.integer(node2)
  stopifnot(length(node1) == length(node2))
  
  # Build adjacency list
  adj <- rep(list(integer(0L)), n)
  
  if (length(node1) > 0L) {
    # make edges undirected by adding both (a -> b) and (b -> a)
    all_from <- c(node1, node2)
    all_to   <- c(node2, node1)
    
    # Split neighbors by source node
    split_adj <- split(all_to, all_from)
    
    # Fill only existing entries
    # names(split_adj) are node indices as character
    idx <- as.integer(names(split_adj))
    adj[idx] <- split_adj
  }
  
  # Connected components via BFS
  comp_id <- integer(n)   # 0 = unvisited
  comp    <- 0L
  
  for (start in seq_len(n)) {
    if (comp_id[start] != 0L) next  # already assigned
    
    comp <- comp + 1L
    # BFS queue implemented as a vector with a head index
    queue <- start
    head  <- 1L
    
    while (head <= length(queue)) {
      v <- queue[head]
      head <- head + 1L
      
      if (comp_id[v] != 0L) next  # might have been assigned via another path
      
      comp_id[v] <- comp
      nbrs <- adj[[v]]
      
      if (length(nbrs)) {
        unvisited <- nbrs[comp_id[nbrs] == 0L]
        if (length(unvisited)) {
          queue <- c(queue, unvisited)
        }
      }
    }
  }
  
  # Coerce and compute sizes
  comp_id <- as.integer(comp_id)
  N_comps <- as.integer(comp)
  
  comp_sizes <- if (N_comps > 0L) {
    tabulate(comp_id, nbins = N_comps)
  } else {
    integer(0L)
  }
  comp_sizes <- as.integer(comp_sizes)
  
  # Identify singleton vs non-singleton components
  singleton_comp <- comp_sizes == 1L          # which component IDs are singletons
  singleton <- which(singleton_comp[comp_id]) # nodes whose component size == 1
  connected <- which(!singleton_comp[comp_id])# nodes whose component size > 1
  
  list(
    comp_id    = comp_id,            # length n
    N_comps    = N_comps,            # scalar
    comp_sizes = comp_sizes,         # length N_comps
    connected  = as.integer(connected),
    singleton  = as.integer(singleton)
  )
}


#' BYM2 component sizes and scaling factors (non-singleton components only)
#'
#' Given an undirected graph (node1, node2, N), this function:
#'   1. Finds connected components
#'   2. Discards singleton components (size 1)
#'   3. For each remaining component c, computes the BYM2 scaling factor s_c
#'      such that geometric_mean(diag(Q_c^+)) == 1 when the precision is s_c * Q_c
#'
#' This is the quantity your Stan model expects as `component_sizes` and
#' `scaling_factors`.
#'
#' @param node1,node2 Integer vectors of equal length with 1-based endpoints of undirected edges.
#' @param N Integer number of nodes (1..N).
#' @param tol Numeric tolerance to decide strictly-positive eigenvalues.
#'
#' @return A list with:
#'   - component_sizes: integer vector of length N_components
#'   - scaling_factors: numeric vector of length N_components

#'
#' @details
#' For each non-singleton component c, we:
#'   - form the Laplacian Q_c
#'   - eigendecompose Q_c
#'   - drop the (near)-zero eigenvalue
#'   - compute diag(Q_c^+) from the positive part of the spectrum
#'   - set s_c = exp(mean(log(diag(Q_c^+))))
#'
#' This is exactly the same scaling logic as in your \code{bym2_basis()} helper.
.bym2_component_scaling <- function(node1, node2, N, tol = 1e-10) {
  stopifnot(N >= 0L, length(node1) == length(node2))

  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Please install the 'Matrix' package.")
  }

  # ---- 1. Connected components (may include singletons)
  comp <- .components_from_edges(N, node1, node2)

  # No nodes OR no components: trivial return
  if (N == 0L || comp$N_comps == 0L) {
    return(list(
      component_sizes         = integer(0L),
      scaling_factors         = numeric(0L)
    ))
  }


  # Identify non-singleton components (size > 1)
  non_singleton_components <- which(comp$comp_sizes > 1L)
  if (length(non_singleton_components) == 0L) {
    # All components are singletons: no CAR structure at all
    return(list(
      component_sizes         = integer(0L),
      scaling_factors         = numeric(0L)
    ))
  }

  # ---- 2. Global Laplacian Q
  Wg <- Matrix::sparseMatrix(
    i    = c(node1, node2),
    j    = c(node2, node1),
    x    = 1,
    dims = c(N, N)
  )
  W  <- Matrix::forceSymmetric(Wg, uplo = "U")
  d  <- as.numeric(Matrix::rowSums(W))
  Q  <- Matrix::Diagonal(x = d) - W

  # ---- 3. Loop over non-singleton components, compute BYM2 scaling factors
  component_sizes  <- integer(length(non_singleton_components))
  scaling_factors  <- numeric(length(non_singleton_components))

  out_idx <- 1L
  for (c in non_singleton_components) {
    idx <- which(comp$comp_id == c)
    n_c <- length(idx)

    if (n_c <= 1L) next  # defensive; should be excluded already

    # Sub-Laplacian for this component
    Qc <- as.matrix(Q[idx, idx, drop = FALSE])

    ee  <- eigen(Qc, symmetric = TRUE)
    lam <- ee$values
    U   <- ee$vectors

    # Keep strictly positive eigenvalues (drop the single zero EV)
    keep <- lam > tol
    if (!any(keep)) {
      stop("Component ", c, " has no positive eigenvalues; check 'tol' and the graph.")
    }

    lam_pos <- lam[keep]
    Upos    <- U[, keep, drop = FALSE]

    # diag(Q^+) = rowSums(Upos^2 * (1/lam_pos) per column)
    lam_inv    <- 1 / lam_pos
    diag_Qpin  <- rowSums(Upos^2 * rep(lam_inv, each = nrow(Upos)))
    s_c        <- exp(mean(log(diag_Qpin)))

    component_sizes[out_idx] <- n_c
    scaling_factors[out_idx] <- s_c
    out_idx <- out_idx + 1L
  }

  # In case numeric issues caused any component to be skipped (shouldn't normally happen)
  if (out_idx <= length(component_sizes)) {
    component_sizes <- component_sizes[seq_len(out_idx - 1L)]
    scaling_factors <- scaling_factors[seq_len(out_idx - 1L)]
  }

  list(
    component_sizes          = component_sizes,
    scaling_factors          = scaling_factors
  )
}

#' BYM2 basis (scaled ICAR eigen-basis) per connected component
#'
#' Builds an orthonormal basis R such that, within each connected component,
#'   R R^T approximates the Moore–Penrose inverse of the ICAR precision (graph
#'   Laplacian) and is BYM2-scaled so that
#'   geometric_mean(diag(Q^+)) = 1 in that component.
#'
#' @param node1,node2 Integer vectors of equal length with 1-based endpoints of undirected edges.
#' @param N Integer number of nodes.
#' @param tol Numeric tolerance to decide strictly-positive eigenvalues.
#' @return A list with:
#'   - R: (N x K) numeric basis matrix, K = sum_{components} (size_c - 1)
#'   - N_pos: K
#'   - comp: integer vector of component IDs per node (1..ncomp; isolates are size 1)
#' @noRd 
#' @keywords internal
.bym2_basis <- function(node1, node2, N, tol = 1e-10) {
  stopifnot(N >= 0L, length(node1) == length(node2))
  
  # ---- components (returns comp_id, N_comps, etc.)
  comp <- .components_from_edges(N, node1, node2)
  
  if (N == 0L || comp$N_comps == 0L) {
    return(list(
      R     = matrix(numeric(), nrow = N, ncol = 0L),
      N_pos = 0L,
      comp  = comp
    ))
  }
  
  # ---- sparse symmetric adjacency and Laplacian Q
  Wg <- Matrix::sparseMatrix(
    i    = c(node1, node2),
    j    = c(node2, node1),
    x    = 1,
    dims = c(N, N)
  )
  W  <- Matrix::forceSymmetric(Wg, uplo = "U")
  d  <- as.numeric(Matrix::rowSums(W))
  Q  <- Matrix::Diagonal(x = d) - W
  
  # Precompute node indices per component
  indices_by_comp <- split(seq_len(N), comp$comp_id)
  
  # ---- loop over components, build BYM2-scaled blocks
  Rblocks <- vector("list", comp$N_comps)
  rows    <- vector("list", comp$N_comps)
  
  for (c in seq_len(comp$N_comps)) {
    idx <- indices_by_comp[[c]]
    n_c <- length(idx)
    if (n_c <= 1L) next  # isolate: contributes no columns
    
    # Dense eigendecomposition (Qc is small per component in typical spatial graphs)
    Qc <- as.matrix(Q[idx, idx, drop = FALSE])
    ee <- eigen(Qc, symmetric = TRUE)
    lam <- ee$values
    U   <- ee$vectors
    
    # Keep strictly positive eigenvalues (drop the single zero EV per component)
    keep <- lam > tol
    if (!any(keep)) next
    
    lam  <- lam[keep]
    Upos <- U[, keep, drop = FALSE]
    
    # diag(Q^+) = rowSums(Upos^2 %*% diag(1/lam)), BYM2 scale s = GM(diag(Q^+))
    lam_inv   <- 1 / lam
    diag_Qpin <- as.vector((Upos^2) %*% lam_inv)
    s_c       <- exp(mean(log(diag_Qpin)))
    
    # R_c = Upos %*% diag(1/sqrt(lam)) / sqrt(s_c)
    # use sweep instead of building an explicit diagonal matrix
    Rc <- sweep(Upos, 2L, sqrt(lam), FUN = "/")
    Rc <- Rc / sqrt(s_c)
    
    Rblocks[[c]] <- Rc
    rows[[c]]    <- idx
  }
  
  # ---- assemble R
  ncols <- sum(vapply(Rblocks, function(M) if (is.null(M)) 0L else ncol(M), integer(1)))
  R     <- matrix(0.0, nrow = N, ncol = ncols)
  
  off <- 0L
  for (c in seq_len(comp$N_comps)) {
    Rc <- Rblocks[[c]]
    if (is.null(Rc)) next
    p <- ncol(Rc)
    R[rows[[c]], (off + 1L):(off + p)] <- Rc
    off <- off + p
  }
  
  list(
    R     = R,
    N_pos = ncol(R),
    comp  = comp
  )
}


# Sample ICAR field directly from the reduced-rank basis.
icar_sample <- function(R) {
  if (ncol(R) == 0L) return(rep(0, nrow(R)))
  as.numeric(R %*% rnorm(ncol(R)))
}


# ---- Simulation with disconnected graphs + BYM2 mixture ----------------------

simulate_stan_equiv_disconnected <- function(
  components = list(c(20, 20)),
  n_isolates = 0,
  N_per_zip = 2,
  N_race = 3,
  N_age = 5,
  N_time = 6,
  K = 1,
  beta = c(-0.2),
  intercept = 0.0,
  lambda_race = 0.3,
  lambda_age = 0.5,
  lambda_time = 0.6,
  lambda_zip = 0.8,
  rho_zip = 0.5,
  n_trials = 30,
  seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  adj <- .make_disconnected_lattices(components, n_isolates)
  node1 <- adj$node1
  node2 <- adj$node2
  zip_levels <- adj$labels
  N_zip <- length(zip_levels)

  # Reduced-rank ICAR basis (disconnected-aware) + BYM2 scaling
  comp <- .components_from_edges(N_zip, node1, node2)
  start_basis <- Sys.time()
  basis <- .bym2_basis(node1, node2, N_zip)
  end_basis <- Sys.time()
  comp_scale <- .bym2_component_scaling(node1, node2, N_zip)
  
  # Structured + iid pieces for BYM2
  phi   <- icar_sample(basis$R)    # GM(marg var) ≈ 1
  theta <- rnorm(N_zip)            # iid N(0,1)
  z_zip <- sqrt(rho_zip) * phi + sqrt(1 - rho_zip) * theta
  z_zip[comp$singleton] <- theta[comp$singleton]
  a_zip <- lambda_zip * z_zip

  # Individuals
  J_zip  <- rep(seq_len(N_zip), each = N_per_zip)
  N      <- length(J_zip)
  J_race <- sample.int(N_race, N, TRUE)
  J_age  <- sample.int(N_age,  N, TRUE)
  J_time <- sample.int(N_time, N, TRUE)

  X <- if (K > 0) {
    matrix(
      rnorm(N * K), N, K,
      dimnames = list(NULL, paste0("x", 1:K))
    )
  } else {
    matrix(0, N, 0)
  }

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
    y = y,
    n_sample = n_sample,
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
    y = y,
    n_sample = n_sample,
    N_race = N_race,
    J_race = J_race,
    N_age  = N_age,
    J_age  = J_age,
    N_time = N_time,
    J_time = J_time,
    N_zip  = N_zip,
    J_zip  = J_zip,
    N_pos  = basis$N_pos,
    R      = basis$R,
    N_edges_zip  = length(node1),
    node1_zip    = node1,
    node2_zip    = node2,
    N_iso   = length(comp$singleton),
    iso_idx = comp$singleton, 
    N_components   = sum(comp$comp_sizes > 1),
    component_size = comp_scale$component_sizes,
    scaling_factor = comp_scale$scaling_factors
  )
  
  list(
    data = dat,
    stan_data = stan_data,
    W = adj$W,
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
    ),
    time = list(
      basis = end_basis - start_basis
    )
  )
}


fit_model <- function(
    data,
    stan_path,
    chains = 4, parallel_chains = 4,
    iter_warmup = 1000, iter_sampling = 1000,
    threads_per_chain = 1,
    refresh = 200,
    max_treedepth = 12,
    adapt_delta = 0.95,
    seed = NULL,
    show_messages = TRUE,
    show_exceptions = TRUE
) {
  mod <- cmdstan_model(
    stan_path,
    cpp_options = list(stan_threads = TRUE)
  )

  fit <- mod$sample(
    data = data,
    chains = chains,
    parallel_chains = parallel_chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    threads_per_chain = threads_per_chain,
    refresh = refresh,
    max_treedepth = max_treedepth,
    adapt_delta = adapt_delta,
    seed = seed,
    show_messages = show_messages,
    show_exceptions = show_exceptions
  )
  
  return(fit)
}

check_recovery <- function(fit, sim) {
  # ---- Recovery plots (robust + aligned) -------------------------------------
  params <- c("intercept", "beta[1]",
              "lambda_race", "lambda_age", "lambda_time",
              "lambda_zip", "rho_zip")
  
  # Draws matrix (numeric); add derived columns for sigma_struct/iid
  dm <- fit$draws(variables = params, format = "draws_matrix")
  
  # Build TRUE vector aligned to colnames(dm)
  true_vals <- c(
    intercept     = sim$true$intercept,
    `beta[1]`     = sim$true$beta[1],
    lambda_race   = sim$true$lambda_race,
    lambda_age    = sim$true$lambda_age,
    lambda_time   = sim$true$lambda_time,
    lambda_zip    = sim$true$lambda_zip,
    rho_zip       = sim$true$rho_zip
  )[colnames(dm)]  # reorder to match dm
  
  stopifnot(is.numeric(true_vals), length(true_vals) == ncol(dm))
  
  list(
    hist = mcmc_recover_hist(x = dm, true = true_vals),
    intervals = mcmc_recover_intervals(x = dm, true = true_vals)
  )
}

run_sim <- function(
    seed = NULL,
    components = list(c(20, 20)),
    n_isolates = 0,
    N_per_zip = 5,
    N_race = 3,
    N_age = 5,
    N_time = 6,
    K = 1,
    beta = c(-0.2),
    intercept = 0.0,
    lambda_race = 0.3,
    lambda_age = 0.5,
    lambda_time = 0.6,
    lambda_zip = 0.8,
    rho_zip = 0.8,
    n_trials = 30,
    stan_path = NULL,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 1000,
    threads_per_chain = 1,
    refresh = 200,
    max_treedepth = 12,
    adapt_delta = 0.95,
    show_messages = FALSE,
    show_exceptions = FALSE
) {
  
    
  sim_data <- simulate_stan_equiv_disconnected(
    components = components,
    n_isolates = n_isolates,
    N_per_zip = N_per_zip,
    N_race = N_race,
    N_age = N_age,
    N_time = N_time,
    K = K,
    beta = beta,
    intercept = intercept,
    lambda_race = lambda_race,
    lambda_age = lambda_age,
    lambda_time = lambda_time,
    lambda_zip = lambda_zip,
    rho_zip = rho_zip,
    n_trials = n_trials,
    seed = seed
  )
  
  fit <- fit_model(
    data = sim_data$stan_data,
    stan_path = stan_path,
    chains = chains,
    parallel_chains = parallel_chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    threads_per_chain = threads_per_chain,
    refresh = refresh,
    max_treedepth = max_treedepth,
    adapt_delta = adapt_delta,
    seed = seed,
    show_messages = show_messages,
    show_exceptions = show_exceptions
  )
  
  var_vec <- c("intercept", "beta[1]",
               "lambda_race", "lambda_age", "lambda_time",
               "lambda_zip", "rho_zip")

  list(
    sim_data = sim_data,
    fit_summary = fit$summary(variables = var_vec),
    recovery = check_recovery(fit, sim_data)
  )
}

repeat_sim <- function(
    n_rep = 10,
    seed = NULL,
    components = list(c(20, 20)),
    n_isolates = 0,
    N_per_zip = 2,
    N_race = 3,
    N_age = 5,
    N_time = 6,
    K = 1,
    beta = c(-0.2),
    intercept = 0.0,
    lambda_race = 0.3,
    lambda_age = 0.5,
    lambda_time = 0.6,
    lambda_zip = 0.8,
    rho_zip = 0.5,
    n_trials = 30,
    stan_path = NULL,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 1000,
    threads_per_chain = 1,
    refresh = 200,
    max_treedepth = 12,
    adapt_delta = 0.95,
    show_messages = FALSE,
    show_exceptions = FALSE
) {

  purrr::map(seq_len(n_rep), function(i) {
    cat("---- Simulation fit ", i, " / ", n_rep, " ----\n")

    sim <- simulate_stan_equiv_disconnected(
      components = components,
      n_isolates = n_isolates,
      N_per_zip = N_per_zip,
      N_race = N_race,
      N_age = N_age,
      N_time = N_time,
      K = K,
      beta = beta,
      intercept = intercept,
      lambda_race = lambda_race,
      lambda_age = lambda_age,
      lambda_time = lambda_time,
      lambda_zip = lambda_zip,
      rho_zip = rho_zip,
      n_trials = n_trials,
      seed = seed
    )

    fit <- fit_model(
      data = sim$stan_data,
      stan_path = stan_path,
      chains = chains,
      parallel_chains = parallel_chains,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      threads_per_chain = threads_per_chain,
      refresh = refresh,
      max_treedepth = max_treedepth,
      adapt_delta = adapt_delta,
      seed = seed,
      show_messages = show_messages,
      show_exceptions = show_exceptions
    )

    var_vec <- c("intercept", "beta[1]",
                 "lambda_race", "lambda_age", "lambda_time",
                 "lambda_zip", "rho_zip")

    list(
      time = list(
        basis = as.numeric(sim$time$basis),
        fit = fit$time()$total 
      ),
      fit_summary = fit$summary(variables = var_vec)
    )
  })
}

repeat_sim_multi_graph <- function(n_rep = 10, n_nodes = c(5), stan_path = NULL) {
  purrr::map(n_nodes, ~ repeat_sim(
    n_rep = n_rep,
    components = list(c(.x, .x)),
    stan_path = stan_path
  )) |>
    setNames(paste0(n_nodes*n_nodes, "_nodes"))
}
