# deps: dplyr, tidyr, tibble, purrr, lubridate, Matrix
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(lubridate)
  library(Matrix)
})

.pad5 <- function(x) stringr::str_pad(as.character(x), 5, pad = "0")

# ---- ICAR helpers ------------------------------------------------------------
.laplacian_from_edges <- function(n, node1, node2) {
  if (n == 0L) stop("n must be > 0")
  if (!length(node1)) {
    return(Diagonal(n)) # isolates only → degree=0; keep a tiny ridge later when sampling
  }
  i <- c(node1, node2); j <- c(node2, node1)
  W <- sparseMatrix(i = i, j = j, x = 1, dims = c(n, n))
  d <- Matrix::rowSums(W)
  L <- Diagonal(n, d) - W
  L
}

# Draw one ICAR vector with per-component sum-to-zero using eigen decomposition.
# Returns a unit-scale intrinsic draw z (lambda multiplies it in the Stan model).
.ricar_eigen <- function(n, node1, node2) {
  L <- .laplacian_from_edges(n, node1, node2)
  # Symmetrize and convert to base matrix for eigen():
  Ld <- as.matrix((L + t(L)) * 0.5)
  ee <- eigen(Ld, symmetric = TRUE, only.values = FALSE)
  vals <- ee$values
  U    <- ee$vectors
  keep <- vals > 1e-8  # drop one zero eigen per connected component
  if (!any(keep)) return(rep(0, n))
  z <- as.numeric(U[, keep, drop = FALSE] %*% (rnorm(sum(keep)) / sqrt(vals[keep])))
  # Optional: normalize to SD 1 (helps set lambda on the same scale across graphs)
  sd_z <- sd(z)
  if (is.finite(sd_z) && sd_z > 0) z <- z / sd_z
  z
}

# ---- Main simulator -----------------------------------------------------------

#' Simulate data for the given Stan ICAR model
#'
#' @param zips Character vector of 5-digit ZIPs (order is preserved). If NULL, create synthetic.
#' @param n_weeks Integer number of weeks.
#' @param n_per_zip_week Individuals per (zip,week) cell (each row has n_sample=1).
#' @param zip_graph Optional: result of `.build_graph(geo_units=…, geo_scale="zip")`.
#' @param family "binomial" (default, with sens/spec) or "gaussian".
#' @param sens,spec Sensitivity/specificity (used only for binomial).
#' @param seed RNG seed.
#' @param start_date character "YYYY-MM-DD". First day of week 1.
#' @param prob_sex,race,age Optional probability vectors for demographics.
#' @param beta,intercept,lambdas Optional true parameters; otherwise sensible defaults are used.
#' @return list(data, stan_data, truth)
simulate_icar_dataset <- function(
  zips                   = NULL,
  n_weeks                = 12L,
  n_per_zip_week         = 20L,
  zip_graph              = NULL,
  family                 = c("binomial", "gaussian"),
  sens                   = 1,
  spec                   = 1,
  seed                   = 123,
  start_date             = "2024-01-01",
  prob_sex               = c(male = 0.48, female = 0.52),
  prob_race              = c(white = 0.6, black = 0.2, other = 0.2),
  prob_age               = c(`0-17` = 0.2, `18-34` = 0.25, `35-64` = 0.35, `65-74` = 0.12, `75+` = 0.08),
  beta                   = -0.2,
  intercept              = -1.5,
  lambdas                = list(race = 0.35, age = 0.40, time = 0.30, zip = 0.60)
) {
  set.seed(seed)
  family <- match.arg(family)

  # --- Levels
  sex_lv  <- c("male", "female")
  race_lv <- c("white", "black", "other")
  age_lv  <- c("0-17","18-34","35-64","65-74","75+")
  time_lv <- seq_len(n_weeks)

  # --- ZIPs and graph
  if (is.null(zips)) {
    # Create synthetic, ordered ZIPs if not provided
    zips <- .pad5(48101 + seq_len(50))  # 50 ZIPs by default
  } else {
    zips <- .pad5(zips)
  }
  if (is.null(zip_graph)) {
    # Fallback: simple ring graph in caller order (works w/out external data)
    nZ <- length(unique(zips))
    node1 <- if (nZ > 1) seq_len(nZ) else integer(0)
    node2 <- if (nZ > 1) c(2:nZ, 1) else integer(0)
    zip_to_node <- setNames(seq_len(nZ), unique(zips))
    zip_graph <- list(
      zip_to_node = zip_to_node,
      stan_graph = list(
        N_nodes    = as.integer(nZ),
        N_edges    = as.integer(length(node1)),
        node1      = as.integer(node1),
        node2      = as.integer(node2),
        N_comps    = as.integer(1L),
        comp_sizes = as.integer(nZ),
        comp_index = matrix(seq_len(nZ), nrow = nZ, ncol = 1)
      ),
      ids_local = names(zip_to_node)
    )
  }

  # --- True parameters
  K <- 1L  
  if (is.null(beta)) beta <- c(sex_male = 0.6)

  N_race <- length(race_lv)
  N_age  <- length(age_lv)
  N_time <- length(time_lv)
  N_zip  <- as.integer(zip_graph$stan_graph$N_nodes)

  # Random-effect standard-normals (unit), scaled by lambdas
  z_race <- rnorm(N_race)
  z_age  <- rnorm(N_age)
  z_time <- rnorm(N_time)

  # ICAR unit draw for ZIP (sum-to-zero per component)
  z_zip  <- .ricar_eigen(
    n     = N_zip,
    node1 = zip_graph$stan_graph$node1,
    node2 = zip_graph$stan_graph$node2
  )

  a_race <- lambdas$race * z_race
  a_age  <- lambdas$age  * z_age
  a_time <- lambdas$time * z_time
  a_zip  <- lambdas$zip  * z_zip

  # --- Build individuals
  dates <- as.character(as.Date(start_date) + weeks(time_lv - 1L))

  # sample (zip, week) cells
  base <- tidyr::crossing(
    zip  = unique(zips),
    time = time_lv
  ) %>%
    mutate(n = n_per_zip_week) %>%
    uncount(n)

  # draw demographics per row
  draw_levels <- function(n, probs, lv) lv[as.integer(sample.int(length(lv), n, replace = TRUE, prob = probs))]
  base <- base %>%
    mutate(
      sex  = draw_levels(n(), prob_sex,  sex_lv),
      race = draw_levels(n(), prob_race, race_lv),
      age  = draw_levels(n(), prob_age,  age_lv),
      date = dates[time]
    ) %>%
    select(sex, race, age, time, date, zip)

  # --- Convert to numeric factors for Stan
  J_race <- match(base$race, race_lv)
  J_age  <- match(base$age,  age_lv)
  J_time <- base$time
  # map ZIP → local node
  J_zip  <- as.integer(unname(zip_graph$zip_to_node[base$zip]))
  if (anyNA(J_zip)) stop("Some ZIPs not in zip_graph$zip_to_node (NA indices).")

  # Fixed effects matrix X (N x K). Here K=1: sex_male indicator
  X <- matrix(as.integer(base$sex == "male"), ncol = 1L)
  colnames(X) <- "sex_male"

  # Linear predictor
  eta <- as.numeric(
    intercept +
      X %*% beta +
      a_race[J_race] +
      a_age[J_age] +
      a_time[J_time] +
      a_zip[J_zip]
  )

  out_df <-
    if (family == "binomial") {
      p <- plogis(eta)
      p_obs <- p * sens + (1 - p) * (1 - spec)  # misclassification
      tibble(positive = rbinom(length(p_obs), size = 1L, prob = p_obs))
    } else {
      # Gaussian outcome with unit noise (you can change sigma_y)
      tibble(outcome = rnorm(length(eta), mean = eta, sd = 1))
    }

  data <- bind_cols(base, out_df)

  # --- Stan data
  # Note: Placeholder for population data (only used for poststratification)
  N      <- nrow(data)
  K_pop  <- 0L
  N_pop  <- 1L
  X_pop  <- array(0, dim = c(N_pop, K_pop))

  stan_data <- list(
    N = N,
    K = K,
    X = X,
    N_pop = N_pop,
    K_pop = K_pop,
    X_pop = X_pop,

    y = if (family == "binomial") data$positive else integer(N),
    n_sample = rep.int(1L, N),

    N_race = N_race,
    J_race = J_race,
    N_race_pop = 1L,
    J_race_pop = 1L,

    N_age = N_age,
    J_age = J_age,
    N_age_pop = 1L,
    J_age_pop = 1L,

    N_time = N_time,
    J_time = J_time,
    N_time_pop = 1L,
    J_time_pop = 1L,

    N_zip = N_zip,
    J_zip = J_zip,
    N_zip_pop = 1L,
    J_zip_pop = 1L,

    N_edges_zip = as.integer(length(zip_graph$stan_graph$node1)),
    node1_zip   = as.integer(zip_graph$stan_graph$node1),
    node2_zip   = as.integer(zip_graph$stan_graph$node2),

    sens = sens,
    spec = spec
  )

  truth <- list(
    intercept = intercept,
    beta      = beta,
    z_race    = z_race, a_race = a_race,
    z_age     = z_age,  a_age  = a_age,
    z_time    = z_time, a_time = a_time,
    z_zip     = z_zip,  a_zip  = a_zip,
    lambdas   = lambdas,
    levels    = list(sex = sex_lv, race = race_lv, age = age_lv, time = time_lv, zip = unique(zips))
  )

  list(data = data, stan_data = stan_data, truth = truth)
}

# ---- Example usage -----------------------------------------------------------
zips <- c("48103","48104","48105","48108","48109","48197","48198")
zip_graph <- .build_graph(geo_units = zips, geo_scale = "zip", verbose = TRUE)
sim <- simulate_icar_dataset(
  zips = zips,
  n_weeks = 16,
  n_per_zip_week = 25,
  zip_graph = zip_graph,
  family = "binomial",
  sens = 1,
  spec = 1,
  seed = 42,
  beta = -0.2,
  intercept = -1.5,
  lambdas = list(race = 0.65, age = 0.40, time = 0.30, zip = 0.50)
)

# Or, with the fallback ring graph (no external geo data needed):
sim <- simulate_icar_dataset(n_weeks = 12, n_per_zip_week = 20, family = "binomial")

# The dataframe you asked for:
head(sim$data)
readr::write_csv(sim$data, "/Users/tntoan/Downloads/simulated_data.csv")

