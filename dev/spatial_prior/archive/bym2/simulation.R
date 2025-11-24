# Pad ZIP codes to 5 characters with leading zeros if needed
..pad5 <- function(x) {
  x <- as.character(x)
  n <- nchar(x)
  x[n < 5] <- paste0(strrep("0", 5 - n[n < 5]), x[n < 5])
  x
}

# Draw one ICAR sample on a single connected component with adjacency Wc (binary)
.draw_icar_component <- function(Wc) {
  n <- nrow(Wc)
  if (n == 1L) return(0)          # isolate: structured effect is 0 on the ICAR scale
  d <- rowSums(Wc)
  Q <- diag(d) - Wc               # graph Laplacian
  ev <- eigen(Q, symmetric = TRUE)
  lam <- ev$values
  V   <- ev$vectors
  keep <- which(lam > 1e-10)      # drop the (near-)zero eigenvalue(s)
  z <- rnorm(length(keep))
  phi <- V[, keep, drop = FALSE] %*% (z / sqrt(lam[keep]))
  drop(phi) - mean(phi)           # soft center (sum-to-zero) for numerical stability
}

# Make a full NxN binary adjacency from an edge list (1-indexed)
.edges_to_W <- function(N, node1, node2) {
  W <- matrix(0, N, N)
  if (length(node1)) {
    idx <- cbind(node1, node2)
    W[idx] <- 1
    W[cbind(node2, node1)] <- 1
  }
  W
}

# ---------- main simulator ----------
# Returns a list with:
#   $data    : list ready for Stan (binomial BYM2-like)
#   $truth   : list of true parameters/effects
simulate_bym2_binomial_data <- function(
  zip_vec,
  zcta_sf,
  xwalk,
  queen,
  verbose,
  show_map,
  races = c("black", "white", "other"),
  hp = list(
    intercept    = -1.5,  # baseline log-odds
    lambda_race  = 0.5,   # SD of race effect
    sigma_zip    = 0.6,   # overall SD of BYM2 ZIP effect
    rho          = 0.6,   # mixing weight (structured fraction)
    sens         = 1.0,   # test sensitivity
    spec         = 1.0,   # test specificity
    intensity    = 80     # sampling intensity
  )
) {
  # Merge user-supplied hp with defaults (so partial lists are fine)
  hp_default <- list(
    intercept   = -1.5,
    lambda_race = 0.5,
    sigma_zip   = 0.6,
    rho         = 0.6,
    sens        = 1.0,
    spec        = 1.0,
    intensity   = 80
  )
  hp <- utils::modifyList(hp_default, hp, keep.null = TRUE)

  # 1) Build BYM2 inputs (edges, per-node scaling, component IDs, mapping)
  bym <- build_bym2_inputs_with_isolates(
    zip_vec = ..pad5(zip_vec),
    xwalk   = xwalk,
    zcta_sf = zcta_sf,
    queen   = queen,
    verbose = verbose
  )

  if (show_map) {
    print(.plot_components(bym, zcta_sf))
  }

  N_nodes <- bym$N_nodes
  if (N_nodes < 1) stop("No nodes produced by builder.")

  # 2) Rebuild adjacency W and draw an ICAR vector per connected component
  W <- .edges_to_W(N_nodes, bym$node1, bym$node2)

  phi <- numeric(N_nodes)
  comp_ids <- sort(unique(bym$comp_id))
  for (c in comp_ids) {
    idx <- which(bym$comp_id == c)
    Wc <- W[idx, idx, drop = FALSE]
    phi[idx] <- .draw_icar_component(Wc)
  }

  # 3) BYM2 combine: scale ICAR to unit variance via inv_sqrt_scale_factor,
  #    then mix with iid ~ N(0,1) and scale by sigma_zip.
  u_struct <- phi / bym$inv_sqrt_scale_factor
  theta    <- rnorm(N_nodes)
  a_zip    <- hp$sigma_zip * ( sqrt(hp$rho) * u_struct + sqrt(1 - hp$rho) * theta )

  # 4) Race effects
  z_race <- rnorm(length(races))
  a_race <- hp$lambda_race * z_race
  names(a_race) <- races

  # 5) Complete grid of (race, zip) cells
  zips_char <- names(bym$zip_to_node)  # preserves input order
  grid <- expand.grid(
    race = races,
    zip  = zips_char,
    stringsAsFactors = FALSE
  )

  # Map to indices
  J_zip  <- bym$zip_to_node[grid$zip]
  if (any(is.na(J_zip))) stop("Some ZIPs could not be mapped to nodes. Check crosswalk/builder output.")
  J_race <- match(grid$race, races)

  # 6) True and observed probabilities (with sens/spec)
  eta    <- hp$intercept + a_race[J_race] + a_zip[J_zip]
  p_true <- plogis(eta)
  p_obs  <- p_true * hp$sens + (1 - p_true) * (1 - hp$spec)

  # 7) Simulate totals and positives per cell
  total    <- rpois(nrow(grid), lambda = hp$intensity)
  positive <- rbinom(nrow(grid), size = total, prob = p_obs)

  # 8) Pack outputs
  truth <- c(hp,
    list(
      a_race  = a_race,
      a_zip   = setNames(a_zip, bym$node_label),
      p_true_by_cell = setNames(p_true, paste(grid$race, grid$zip, sep = "|"))
    )
  )

  comp <- .make_comp_index(bym$comp_id)
  noniso_idx <- which(tabulate(c(bym$node1, bym$node2), nbins = N_nodes) > 0L)

  data <- list(
    N = length(total),
    K = 0L,
    X = matrix(0, nrow = length(total), ncol = 0),
    y = positive,
    n_sample = total,
    N_race = length(races),
    J_race = J_race,
    N_zip  = length(zips_char),
    J_zip  = J_zip,
    sens = hp$sens,
    spec = hp$spec,
    N_edges = bym$N_edges,
    node1 = bym$node1,
    node2 = bym$node2,
    inv_sqrt_scale_factor = bym$inv_sqrt_scale_factor,
    C = comp$C,
    comp_sizes = comp$comp_sizes,
    max_comp_size = comp$max_comp_size,
    comp_index = comp$comp_index,
    N_noniso = length(noniso_idx),
    noniso_idx = noniso_idx
  )

  list(
    truth = truth,
    data  = data
  )
}


.sample_zips <- function(all_zips, n_zips, seed, step) {
  seed <- seed %||% sample(1:1e6, size = 1)
  set.seed(seed)

  irow <- sample(nrow(xwalk) - n_zips, size = 1)
  rows <- seq(irow, by = step, length.out = n_zips)
  zip_vec <- xwalk$ZIP_CODE[rows]

  return(zip_vec)
}

run_sim <- function(
  zcta_sf,
  xwalk,
  stan_path,
  n_zips = 20,
  step = 1,
  seed = NULL,
  queen = TRUE,
  n_iter = 2000,
  n_chains = 4,
  show_map = TRUE,
  verbose = TRUE
) {

  zip_vec <- .sample_zips(
    all_zips = xwalk$ZIP_CODE,
    n_zips = n_zips,
    seed = seed,
    step = step
  )

  sim <- simulate_bym2_binomial_data(
    zip_vec = zip_vec,
    xwalk   = xwalk,
    zcta_sf = zcta_sf,
    queen   = queen,
    verbose = verbose,
    show_map = show_map
  )

  mod_mcmc <- cmdstanr::cmdstan_model(
    stan_file = stan_path,
    cpp_options = list(stan_threads = TRUE)
  )

  fit <- mod_mcmc$sample(
    data = sim$data,
    iter_warmup = n_iter/2,
    iter_sampling = n_iter/2,
    chains = n_chains,
    parallel_chains = n_chains,
    threads_per_chain = 1,
    refresh = n_iter / 10,
    seed = NULL
  )

  x <- fit$draws(variables = c("lambda_race", "sigma_zip", "rho"))
  true <- sim$truth[c("lambda_race", "sigma_zip", "rho")] |> unlist()

  print(mcmc_recover_intervals(x, true))
  print(mcmc_recover_hist(x, true, bins = 30))
}

