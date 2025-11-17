# --- requirements ---
library(brms)       # >= 2.22
library(Matrix)
library(loo)

#' Build symmetric 0/1 adjacency matrix W with dimnames = zip labels
adjacency_from_stan_graph <- function(stan_graph, labels) {
  N <- as.integer(stan_graph$N_nodes)
  stopifnot(length(labels) == N)

  W <- sparseMatrix(i = c(stan_graph$node1, stan_graph$node2),
                    j = c(stan_graph$node2, stan_graph$node1),
                    x = 1, dims = c(N, N))
  W <- forceSymmetric(W, uplo = "U")
  W <- as.matrix(W)
  dimnames(W) <- list(labels, labels)
  W
}

#' Prepare aggregated binomial dataset for brms from your `sim` object
prepare_brms_data <- function(sim) {
  cells <- sim$cell_table
  cells$y <- sim$stan_data$y
  cells$n <- sim$stan_data$n_sample

  # harmonize factors and add ZIP labels
  cells$zip  <- factor(sim$truth$zip_labels[cells$zip], levels = sim$truth$zip_labels)
  cells$race <- factor(as.character(cells$race), levels = sim$truth$race_levels)
  cells$age  <- factor(as.character(cells$age),  levels = sim$truth$age_levels)
  cells$sex  <- factor(as.character(cells$sex),  levels = sim$truth$sex_levels)

  # random intercept for time: use factor to match "(1|time)"
  cells$time <- factor(cells$time)

  # Keep rows with any trials (n can be 0; brms will ignore those contributions)
  cells
}

#' Fit iid-vs-ICAR brms models and compare via LOO
#' @param sim your simulator output list
#' @param car_type one of "icar", "esicar", "escar", "bym2"
#' @param backend "cmdstanr" (recommended) or "rstan"
fit_brms_car_vs_iid <- function(sim,
                                car_type = "icar",
                                backend  = "cmdstanr",
                                iter = 2000, chains = 4, cores = 4, seed = 1234) {
  if (backend == "cmdstanr") options(brms.backend = "cmdstanr")

  dat <- prepare_brms_data(sim)
  W   <- adjacency_from_stan_graph(sim$truth$stan_graph, sim$truth$zip_labels)

  need_cols <- c("y","n","sex","race","age","time","zip")
  missing <- setdiff(need_cols, names(dat))
  if (length(missing)) stop("Data is missing columns: ", paste(missing, collapse = ", "))

  dat$zip <- factor(dat$zip, levels = rownames(W))
  if (!identical(rownames(W), levels(dat$zip)))
    stop("levels(dat$zip) must match rownames(W) exactly.")

  # IID ZIP model
  form_iid <- brms::bf(
    y | trials(n) ~ 1 + sex + (1 | race) + (1 | age) + (1 | time) + (1 | zip)
  )

  # Spatial ZIP model (inject literal type to avoid env lookup issues)
  f_txt <- sprintf(
    "y | trials(n) ~ 1 + sex + (1 | race) + (1 | age) + (1 | time) + car(W, gr = zip, type = '%s')",
    car_type
  )
  form_car <- brms::bf(stats::as.formula(f_txt))

  priors <- c(
    set_prior("normal(0, 2.5)", class = "b"),
    set_prior("student_t(3, 0, 2.5)", class = "sd")
  )

  fit_iid <- brm(
    formula = form_iid, data = dat, family = binomial(),
    prior = priors, iter = iter, chains = chains, cores = cores, seed = seed,
    backend = "cmdstanr", save_pars = save_pars(all = TRUE)
  )

  fit_car <- brm(
    formula = form_car, data = dat, family = binomial(),
    prior = priors, iter = iter, chains = chains, cores = cores, seed = seed + 1,
    backend = "cmdstanr", data2 = list(W = W), save_pars = save_pars(all = TRUE)
  )

  # --- Safe "posterior summary table" printing (avoids masked summary()) ---
  safe_print_summary <- function(fit, label = NULL) {
    if (!is.null(label)) cat("\n===== ", label, " =====\n", sep = "")
    s_ok <- TRUE
    out <- tryCatch(base::summary(fit), error = function(e) { s_ok <<- FALSE; e })
    if (s_ok) {
      print(out)
    } else {
      cat("(base::summary() seems masked; showing key tables instead)\n")
      cat("\n-- Fixed effects --\n"); print(brms::fixef(fit, robust = TRUE))
      cat("\n-- Group-level SDs --\n"); print(brms::VarCorr(fit, summary = TRUE))
      ps <- brms::posterior_summary(fit)
      row_has_sp <- grepl("^sdcar|^rho|^phi|^lambda", rownames(ps))
      if (any(row_has_sp)) {
        cat("\n-- Spatial / CAR parameters --\n"); print(ps[row_has_sp, , drop = FALSE])
      }
    }
  }

  safe_print_summary(fit_iid, sprintf("IID ZIP model: posterior summary"))
  safe_print_summary(fit_car, sprintf("ICAR ZIP model (%s): posterior summary", car_type))

  # LOO comparison (cmdstanr path)
  fit_iid <- add_criterion(fit_iid, "loo")
  fit_car <- add_criterion(fit_car, "loo")

  cat("\n===== LOO model comparison (lower is better) =====\n")
  cmp <- loo::loo_compare(loo::loo(fit_car), loo::loo(fit_iid))
  print(cmp)

  list(
    data = dat, W = W,
    fits = list(iid = fit_iid, car = fit_car),
    loo  = list(iid = fit_iid$criteria$loo, car = fit_car$criteria$loo),
    comparison = cmp
  )
}

# --- Example run (using your `sim` from the prompt) ---
# res <- fit_brms_car_vs_iid(sim, car_type = "icar", iter = 2000, chains = 4, cores = 4)
# res$comparison   # data.frame with elpd_diff, se_diff, etc.
# fixef(res$fits$car)  # fixed-effect estimates
