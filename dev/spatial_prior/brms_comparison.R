# ============================================================
# BRMS/Stan BYM2 comparison helpers + one-shot wrapper
# ============================================================

# ---------- Adjacency + formula + priors + brms fit ----------
make_brms_W <- function(sim) {
  stopifnot(is.data.frame(sim$data), "zip" %in% names(sim$data))
  zip_levels <- levels(sim$data$zip)
  if (is.null(zip_levels)) stop("sim$data$zip must be a factor with levels.")
  
  if (!is.null(sim$W)) {
    W <- sim$W
  } else if (!is.null(sim$stan_data$N_edges_zip)) {
    N <- length(zip_levels)
    n1 <- sim$stan_data$node1_zip
    n2 <- sim$stan_data$node2_zip
    W  <- Matrix::sparseMatrix(i = c(n1, n2), j = c(n2, n1), x = 1, dims = c(N, N))
  } else {
    stop("Could not find W or edge list in `sim`.")
  }
  
  if (inherits(W, "Matrix")) {
    W <- as.matrix(Matrix::forceSymmetric(W, uplo = "U"))
  } else {
    W <- as.matrix(W)
  }
  diag(W) <- 0
  storage.mode(W) <- "numeric"
  
  if (is.null(rownames(W)) || is.null(colnames(W))) {
    rownames(W) <- colnames(W) <- zip_levels
  } else if (!identical(rownames(W), zip_levels)) {
    W <- W[zip_levels, zip_levels, drop = FALSE]
  }
  if (!isTRUE(all.equal(W, t(W)))) stop("Adjacency matrix must be symmetric.")
  W
}

make_brms_formula <- function(sim, include_X = TRUE) {
  has_x <- include_X && any(startsWith(names(sim$data), "x"))
  rhs <- c("1",
           if (has_x) grep("^x\\d+$", names(sim$data), value = TRUE) else NULL,
           "(1|race)", "(1|age)", "(1|time)",
           "car(W, gr = zip, type = 'bym2')")
  stats::as.formula(paste("y | trials(n_sample) ~", paste(rhs, collapse = " + ")))
}

default_bym2_priors <- function() {
  brms::prior(normal(0, 5), class = "Intercept") +
    brms::prior(normal(0, 3), class = "b") +                # fixed effects xk
    brms::prior(normal(0, 3), class = "sd", group = "race") +
    brms::prior(normal(0, 3), class = "sd", group = "age")  +
    brms::prior(normal(0, 3), class = "sd", group = "time") +
    brms::prior(beta(0.5, 0.5), class = "rhocar") +         # ~ Beta(0.5,0.5)
    brms::prior(normal(0, 1), class = "sdcar")              # ~ half-normal
}

fit_brms_bym2 <- function(sim,
                          iter_warmup = 1000, iter_sampling = 1000,
                          chains = 4, cores = chains,
                          adapt_delta = 0.95, max_treedepth = 12,
                          seed = NULL,
                          priors = default_bym2_priors(),
                          stan_code_path = NULL,
                          stan_code_source = c("compiled","make")) {
  stan_code_source <- match.arg(stan_code_source)
  W <- make_brms_W(sim)
  f <- make_brms_formula(sim)
  
  # Optionally pre-generate Stan code (source = "make")
  # This guarantees the same code as compilation given the same inputs.
  pre_code <- NULL
  if (!is.null(stan_code_path) && stan_code_source == "make") {
    pre_code <- brms::make_stancode(
      formula = f,
      data    = sim$data,
      family  = stats::binomial(),
      data2   = list(W = W),
      prior   = priors
    )
    dir.create(dirname(stan_code_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(pre_code, con = stan_code_path)
  }
  
  fit <- brms::brm(
    formula = f,
    data    = sim$data,
    data2   = list(W = W),
    family  = stats::binomial(),
    prior   = priors,
    backend = "cmdstanr",
    chains  = chains,
    cores   = cores,
    iter    = iter_warmup + iter_sampling,
    warmup  = iter_warmup,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    seed    = seed
  )
  
  # Optionally save the *compiled* Stan program (matches exactly what was used)
  if (!is.null(stan_code_path) && stan_code_source == "compiled") {
    code <- brms::stancode(fit)
    dir.create(dirname(stan_code_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(code, con = stan_code_path)
  }
  
  fit
}


# -----------------------------
# Comparable posterior summaries
# -----------------------------
# ----- internal utility -------------------------------------------------------
.safe_beta_names <- function(K) {
  if (isTRUE(K > 0)) sprintf("beta[%d]", seq_len(K)) else character(0)
}

.summarise_tbl <- function(draws) {
  q05 <- function(x) posterior::quantile2(x, 0.05)
  q95 <- function(x) posterior::quantile2(x, 0.95)
  posterior::summarise_draws(draws, mean, sd, q05, median, q95, rhat, ess_bulk, ess_tail)
}

# ----- CmdStanR -> standardized table (K-aware; no grep) ---------------------
summarize_cmdstan_bym2 <- function(fit_cmdstan, K = NULL) {
  beta_names <- .safe_beta_names(K %||% 0L)  # %||% for safety; define below if you don't have it
  vars <- c("intercept",
            beta_names,
            "lambda_race","lambda_age","lambda_time",
            "lambda_zip","rho_zip",
            "sigma_struct","sigma_iid")
  
  # Keep only vars that actually exist in the fit
  dr_all <- fit_cmdstan$draws()
  have   <- colnames(posterior::as_draws_matrix(dr_all))
  vars   <- intersect(vars, have)
  
  dr <- posterior::as_draws_df(fit_cmdstan$draws(variables = vars))
  .summarise_tbl(dr)
}

# ----- brms -> standardized table (K-aware; no grep) -------------------------
summarize_brms_bym2 <- function(fit_brms, K = NULL) {
  dr <- posterior::as_draws_df(fit_brms)
  nm <- names(dr)
  
  # Rename to Stan-like names without regex:
  rename_map <- list()
  
  # Intercept
  if ("b_Intercept" %in% nm) rename_map[["b_Intercept"]] <- "intercept"
  
  # Fixed effects x1..xK  -> beta[1]..beta[K]
  if (isTRUE(K > 0)) {
    for (k in seq_len(K)) {
      old <- sprintf("b_x%d", k)
      if (old %in% nm) rename_map[[old]] <- sprintf("beta[%d]", k)
    }
  }
  
  # Group SDs -> lambdas
  if ("sd_race__Intercept" %in% nm) rename_map[["sd_race__Intercept"]] <- "lambda_race"
  if ("sd_age__Intercept"  %in% nm) rename_map[["sd_age__Intercept"]]  <- "lambda_age"
  if ("sd_time__Intercept" %in% nm) rename_map[["sd_time__Intercept"]] <- "lambda_time"
  
  # BYM2 hyperparameters
  if ("sdcar"  %in% nm) rename_map[["sdcar"]]  <- "lambda_zip"
  if ("rhocar" %in% nm) rename_map[["rhocar"]] <- "rho_zip"
  
  # Apply renaming
  for (old in names(rename_map)) {
    names(dr)[names(dr) == old] <- rename_map[[old]]
  }
  
  # Derived sigmas (match Stan generated quantities)
  if (all(c("lambda_zip","rho_zip") %in% names(dr))) {
    dr[["sigma_struct"]] <- dr[["lambda_zip"]] * sqrt(dr[["rho_zip"]])
    dr[["sigma_iid"]]    <- dr[["lambda_zip"]] * sqrt(1 - dr[["rho_zip"]])
  }
  
  keep <- c("intercept",
            .safe_beta_names(K %||% 0L),
            "lambda_race","lambda_age","lambda_time",
            "lambda_zip","rho_zip",
            "sigma_struct","sigma_iid")
  keep <- intersect(keep, names(dr))
  .summarise_tbl(dr[, keep, drop = FALSE])
}

# ----- side-by-side compare (unchanged) --------------------------------------
compare_posteriors <- function(stan_tbl, brms_tbl,
                               stan_lab = "stan", brms_lab = "brms") {
  sel <- c("variable","mean","sd","q05","q50","q95","rhat","ess_bulk","ess_tail")
  stan_tbl <- stan_tbl[, intersect(sel, names(stan_tbl))]
  brms_tbl <- brms_tbl[, intersect(sel, names(brms_tbl))]
  
  names(stan_tbl)[names(stan_tbl) != "variable"] <-
    paste0(names(stan_tbl)[names(stan_tbl) != "variable"], "_", stan_lab)
  names(brms_tbl)[names(brms_tbl) != "variable"] <-
    paste0(names(brms_tbl)[names(brms_tbl) != "variable"], "_", brms_lab)
  
  merge(stan_tbl, brms_tbl, by = "variable", all = TRUE, sort = FALSE)
}

# -----------------------------
# Optional: add simple deltas
# -----------------------------
add_diffs <- function(cmp_tbl) {
  if (!all(c("mean_stan","mean_brms","sd_stan","sd_brms") %in% names(cmp_tbl))) return(cmp_tbl)
  transform(cmp_tbl,
            mean_diff = mean_stan - mean_brms,
            sd_ratio  = sd_stan / sd_brms)
}

# ============================================================
# One-shot wrapper: simulate -> fit Stan -> fit brms -> compare
# ============================================================
run_bym2_sim_and_compare <- function(
    components = list(c(5, 5)), n_isolates = 0L, N_per_zip = 5L,
    N_race = 3L, N_age = 5L, N_time = 6L, K = 1L,
    beta = c(-0.2), intercept = 1.0,
    lambda_race = 0.3, lambda_age = 0.4, lambda_time = 0.5,
    lambda_zip = 0.8, rho_zip = 0.8, n_trials = 30L,
    seed_data = NULL, seed_model = NULL,
    stan_path,
    chains = 4, cores = chains,
    iter_warmup = 1000, iter_sampling = 1000,
    adapt_delta = 0.95, max_treedepth = 12
) {
  sim <- simulate_stan_equiv_disconnected(
    components = components, n_isolates = n_isolates, N_per_zip = N_per_zip,
    N_race = N_race, N_age = N_age, N_time = N_time, K = K,
    beta = beta, intercept = intercept,
    lambda_race = lambda_race, lambda_age = lambda_age, lambda_time = lambda_time,
    lambda_zip = lambda_zip, rho_zip = rho_zip,
    n_trials = n_trials, seed = seed_data
  )
  
  fit_stan <- fit_model(
    data = sim$stan_data, stan_path = stan_path,
    chains = chains, parallel_chains = cores,
    iter_warmup = iter_warmup, iter_sampling = iter_sampling,
    adapt_delta = adapt_delta, max_treedepth = max_treedepth, seed = seed_model
  )
  
  fit_br <- fit_brms_bym2(
    sim,
    iter_warmup = iter_warmup, iter_sampling = iter_sampling,
    chains = chains, cores = cores,
    adapt_delta = adapt_delta, max_treedepth = max_treedepth, seed = seed_model,
    stan_code_path = file.path(dirname(stan_path), "bym2_brms.stan"),
    stan_code_source = "make"
  )
  
  tab_stan <- summarize_cmdstan_bym2(fit_stan, K = sim$stan_data$K)
  tab_brms <- summarize_brms_bym2(fit_br,   K = sim$stan_data$K)
  cmp      <- add_diffs(compare_posteriors(tab_stan, tab_brms))
  
  list(sim = sim,
       fit_stan = fit_stan,
       fit_brms = fit_br,
       stan_summary = tab_stan,
       brms_summary = tab_brms,
       comparison = cmp)
}


# =====================
# Example usage
# =====================
stan_path <- "/Users/tntoan/Desktop/repos/shinymrp/dev/spatial_prior/benchmark/bym2_reparam.stan"
res <- run_bym2_sim_and_compare(
  components = list(c(10, 10)),
  n_isolates = 0,
  N_per_zip  = 5,
  K = 1,
  beta = c(-0.2),
  intercept = 1.0,
  lambda_race = 0.3, lambda_age = 0.4, lambda_time = 0.5,
  lambda_zip  = 0.8, rho_zip = 0.8,
  seed_data  = sample(1e6, 1), seed_model = sample(1e6, 1),
  stan_path  = stan_path,
  chains = 4, iter_warmup = 1000, iter_sampling = 1000
)
