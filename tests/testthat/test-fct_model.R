create_test_data <- function(
    N = 100,
    seed = sample.int(1e4, 1),
    params = list(
      cont = list(lower = 0,  upper = 10, n = 3),
      bin  = list(n = 3),
      cat  = list(categories = 1:3, n = 3)
    ),
    family = c("binomial", "gaussian")
) {
  family <- match.arg(family)
  set.seed(seed)
  out <- list()
  
  # continuous --------------------------------------------------------------
  if (params$cont$n > 0) {
    mat <- replicate(
      params$cont$n,
      runif(N, params$cont$lower, params$cont$upper)
    )
    colnames(mat) <- paste0("cont", seq_len(ncol(mat)))
    out <- c(out, as.data.frame(mat))
  }
  
  # binary ------------------------------------------------------------------
  if (params$bin$n > 0) {
    mat <- replicate(
      params$bin$n,
      sample(0:1, N, replace = TRUE)
    )
    colnames(mat) <- paste0("bin", seq_len(ncol(mat)))
    out <- c(out, as.data.frame(mat))
  }
  
  # categorical -------------------------------------------------------------
  if (params$cat$n > 0) {
    mat <- replicate(
      params$cat$n,
      sample(params$cat$categories, N, replace = TRUE)
    )
    colnames(mat) <- paste0("cat", seq_len(ncol(mat)))
    out <- c(out, as.data.frame(mat))
  }
  
  # combine all into one data.frame
  df <- do.call(
    data.frame,
    c(out, list(stringsAsFactors = FALSE))
  )
  
  
  if (family == "binomial") {
    df %>% mutate(
      positive = sample(1:5, N, replace = TRUE),
      total = sample(6:10, N, replace = TRUE)
    )
  } else if (family == "gaussian") {
    df %>% mutate(
      value = runif(N, 0, 10)
    )
  }
}

make_hashed_filename <- function(
  effects,
  prefix = NULL,
  ext    = ".csv",
  algo   = "sha1",
  n      = 8
) {

  # 1) recursively sort each sub‐list by name
  normalize <- function(z) {
    if (!is.list(z)) return(z)
    z <- z[sort(names(z))]
    lapply(z, normalize)
  }

  # 2) compute the digest of the normalized object
  h <- digest::digest(normalize(effects), algo = algo, serialize = TRUE)

  # 3) take only the first n chars
  short <- substr(h, 1, n)

  # 4) glue together: e.g. "priors_ab12cd34.csv"
  paste0(prefix, "_", short, ext)
}

test_that("estimated parameters match saved values", {
  seed <- 1234

  intercept_prior <- "normal(0, 5)"
  effect_prior <- "normal(0, 3)"
  struct_prior <- "structured"

  data <- create_test_data(seed = seed) %>%
    stan_factor(ignore_columns = c("positive", "total"))

  # fixed effect of continuous variables only
  effects <- list(
    Intercept = list(Intercept = intercept_prior),
    fixed = list(
      cont1 = effect_prior,
      cont2 = effect_prior,
      cont3 = effect_prior
    )
  ) %>%
    group_effects(data) %>%
    ungroup_effects()

  result <- run_mcmc(
    input_data = data,
    new_data = data,
    effects = effects,
    seed = seed,
    silent = TRUE
  )

  out <- result$fit$mcmc$summary()
  ref <- paste0("testdata/", make_hashed_filename(effects, prefix = "params")) %>%
    testthat::test_path() %>%
    readr::read_csv(show_col_types = FALSE)

  expect_equal(
    result$fit$mcmc$summary(),
    ref,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )

  # fixed effects of binary variables only
  effects <- list(
    Intercept = list(Intercept = intercept_prior),
    fixed = list(
      bin1 = effect_prior,
      bin2 = effect_prior,
      bin3 = effect_prior
    )
  ) %>%
    group_effects(data) %>%
    ungroup_effects()

  result <- run_mcmc(
    input_data = data,
    new_data = data,
    effects = effects,
    seed = seed,
    silent = TRUE
  )

  ref <- paste0("testdata/", make_hashed_filename(effects, prefix = "params")) %>%
    testthat::test_path() %>%
    readr::read_csv(show_col_types = FALSE)

  expect_equal(
    result$fit$mcmc$summary(),
    ref,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )

  # fixed effects of categorical variables only
  effects <- list(
    Intercept = list(Intercept = intercept_prior),
    fixed = list(
      cat1 = effect_prior,
      cat2 = effect_prior,
      cat3 = effect_prior
    )
  ) %>%
    group_effects(data) %>%
    ungroup_effects()

  result <- run_mcmc(
    input_data = data,
    new_data = data,
    effects = effects,
    seed = seed,
    silent = TRUE
  )

  ref <- paste0("testdata/", make_hashed_filename(effects, prefix = "params")) %>%
    testthat::test_path() %>%
    readr::read_csv(show_col_types = FALSE)

  expect_equal(
    result$fit$mcmc$summary(),
    ref,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )

  # varying effects of categorical variables only
  effects <- list(
    Intercept = list(Intercept = intercept_prior),
    varying = list(
      cat1 = effect_prior,
      cat2 = effect_prior,
      cat3 = effect_prior
    )
  ) %>%
    group_effects(data) %>%
    ungroup_effects()

  result <- run_mcmc(
    input_data = data,
    new_data = data,
    effects = effects,
    seed = seed,
    silent = TRUE
  )

  ref <- paste0("testdata/", make_hashed_filename(effects, prefix = "params")) %>%
    testthat::test_path() %>%
    readr::read_csv(show_col_types = FALSE)

  expect_equal(
    result$fit$mcmc$summary(),
    ref,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )

  # interaction between fixed effects of
  # continous variables
  effects <- list(
    Intercept = list(Intercept = intercept_prior),
    fixed = list(
      cont1 = effect_prior,
      cont2 = effect_prior,
      cont3 = effect_prior
    ),
    interaction = list(
      `cont1:cont2` = effect_prior,
      `cont2:cont3` = effect_prior,
      `cont3:cont1` = effect_prior
    )
  ) %>%
    group_effects(data) %>%
    ungroup_effects()

  result <- run_mcmc(
    input_data = data,
    new_data = data,
    effects = effects,
    seed = seed,
    silent = TRUE
  )

  ref <- paste0("testdata/", make_hashed_filename(effects, prefix = "params")) %>%
    testthat::test_path() %>%
    readr::read_csv(show_col_types = FALSE)

  expect_equal(
    result$fit$mcmc$summary(),
    ref,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )

  # interaction between fixed effects of
  # binary variables
  effects <- list(
    Intercept = list(Intercept = intercept_prior),
    fixed = list(
      bin1 = effect_prior,
      bin2 = effect_prior,
      bin3 = effect_prior
    ),
    interaction = list(
      `bin1:bin2` = effect_prior,
      `bin2:bin3` = effect_prior,
      `bin3:bin1` = effect_prior
    )
  ) %>%
    group_effects(data) %>%
    ungroup_effects()

  result <- run_mcmc(
    input_data = data,
    new_data = data,
    effects = effects,
    seed = seed,
    silent = TRUE
  )

  ref <- paste0("testdata/", make_hashed_filename(effects, prefix = "params")) %>%
    testthat::test_path() %>%
    readr::read_csv(show_col_types = FALSE)

  expect_equal(
    result$fit$mcmc$summary(),
    ref,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )

  # interaction between fixed effects of
  # categorical variables
  effects <- list(
    Intercept = list(Intercept = intercept_prior),
    fixed = list(
      cat1 = effect_prior,
      cat2 = effect_prior,
      cat3 = effect_prior
    ),
    interaction = list(
      `cat1:cat2` = effect_prior,
      `cat2:cat3` = effect_prior,
      `cat3:cat1` = effect_prior
    )
  ) %>%
    group_effects(data) %>%
    ungroup_effects()

  result <- run_mcmc(
    input_data = data,
    new_data = data,
    effects = effects,
    seed = seed,
    silent = TRUE
  )

  ref <- paste0("testdata/", make_hashed_filename(effects, prefix = "params")) %>%
    testthat::test_path() %>%
    readr::read_csv(show_col_types = FALSE)

  expect_equal(
    result$fit$mcmc$summary(),
    ref,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )

  # interaction between varying effects of
  # categorical variables (without structured prior)
  effects <- list(
    Intercept = list(Intercept = intercept_prior),
    varying = list(
      cat1 = effect_prior,
      cat2 = effect_prior,
      cat3 = effect_prior
    ),
    interaction = list(
      `cat1:cat2` = effect_prior,
      `cat2:cat3` = effect_prior,
      `cat3:cat1` = effect_prior
    )
  ) %>%
    group_effects(data) %>%
    ungroup_effects()

  result <- run_mcmc(
    input_data = data,
    new_data = data,
    effects = effects,
    seed = seed,
    silent = TRUE
  )

  ref <- paste0("testdata/", make_hashed_filename(effects, prefix = "params")) %>%
    testthat::test_path() %>%
    readr::read_csv(show_col_types = FALSE)

  expect_equal(
    result$fit$mcmc$summary(),
    ref,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )

  # interaction between fixed effects of
  # binary variables and continous variables
  effects <- list(
    Intercept = list(Intercept = intercept_prior),
    fixed = list(
      bin1 = effect_prior,
      bin2 = effect_prior,
      cont1 = effect_prior,
      cont2 = effect_prior
    ),
    interaction = list(
      `bin1:cont1` = effect_prior,
      `bin1:cont2` = effect_prior,
      `bin2:cont1` = effect_prior,
      `bin2:cont2` = effect_prior
    )
  ) %>%
    group_effects(data) %>%
    ungroup_effects()

  result <- run_mcmc(
    input_data = data,
    new_data = data,
    effects = effects,
    seed = seed,
    silent = TRUE
  )

  ref <- paste0("testdata/", make_hashed_filename(effects, prefix = "params")) %>%
    testthat::test_path() %>%
    readr::read_csv(show_col_types = FALSE)

  expect_equal(
    result$fit$mcmc$summary(),
    ref,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )

  # interaction between fixed effects of
  # categorical variables and fixed effects of
  # continuous variables
  effects <- list(
    Intercept = list(Intercept = intercept_prior),
    fixed = list(
      cat1 = effect_prior,
      cat2 = effect_prior,
      cont1 = effect_prior,
      cont2 = effect_prior
    ),
    interaction = list(
      `cat1:cont1` = effect_prior,
      `cat1:cont2` = effect_prior,
      `cat2:cont1` = effect_prior,
      `cat2:cont2` = effect_prior
    )
  ) %>%
    group_effects(data) %>%
    ungroup_effects()

  result <- run_mcmc(
    input_data = data,
    new_data = data,
    effects = effects,
    seed = seed,
    silent = TRUE
  )

  ref <- paste0("testdata/", make_hashed_filename(effects, prefix = "params")) %>%
    testthat::test_path() %>%
    readr::read_csv(show_col_types = FALSE)

  expect_equal(
    result$fit$mcmc$summary(),
    ref,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )

  # interaction between varying effects of
  # categorical variables and fixed effects of
  # continuous variables (without structured prior)
  effects <- list(
    Intercept = list(Intercept = intercept_prior),
    fixed = list(
      cont1 = effect_prior,
      cont2 = effect_prior
    ),
    varying = list(
      cat1 = effect_prior,
      cat2 = effect_prior
    ),
    interaction = list(
      `cat1:cont1` = effect_prior,
      `cat1:cont2` = effect_prior,
      `cat2:cont1` = effect_prior,
      `cat2:cont2` = effect_prior
    )
  ) %>%
    group_effects(data) %>%
    ungroup_effects()

  result <- run_mcmc(
    input_data = data,
    new_data = data,
    effects = effects,
    seed = seed,
    silent = TRUE
  )

  ref <- paste0("testdata/", make_hashed_filename(effects, prefix = "params")) %>%
    testthat::test_path() %>%
    readr::read_csv(show_col_types = FALSE)

  expect_equal(
    result$fit$mcmc$summary(),
    ref,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )

  # interaction between fixed effects of
  # categorical variables and fixed effects of
  # binary variables
  effects <- list(
    Intercept = list(Intercept = intercept_prior),
    fixed = list(
      bin1 = effect_prior,
      bin2 = effect_prior,
      cat1 = effect_prior,
      cat2 = effect_prior
    ),
    interaction = list(
      `bin1:cat1` = effect_prior,
      `bin1:cat2` = effect_prior,
      `bin2:cat1` = effect_prior,
      `bin2:cat2` = effect_prior
    )
  ) %>%
    group_effects(data) %>%
    ungroup_effects()

  result <- run_mcmc(
    input_data = data,
    new_data = data,
    effects = effects,
    seed = seed,
    silent = TRUE
  )

  ref <- paste0("testdata/", make_hashed_filename(effects, prefix = "params")) %>%
    testthat::test_path() %>%
    readr::read_csv(show_col_types = FALSE)

  expect_equal(
    result$fit$mcmc$summary(),
    ref,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )

  # interaction between varying effects of
  # categorical variables and fixed effects of
  # binary variables (without structured prior)
  effects <- list(
    Intercept = list(Intercept = intercept_prior),
    fixed = list(
      bin1 = effect_prior,
      bin2 = effect_prior
    ),
    varying = list(
      cat1 = effect_prior,
      cat2 = effect_prior
    ),
    interaction = list(
      `bin1:cat1` = effect_prior,
      `bin1:cat2` = effect_prior,
      `bin2:cat1` = effect_prior,
      `bin2:cat2` = effect_prior
    )
  ) %>%
    group_effects(data) %>%
    ungroup_effects()

  result <- run_mcmc(
    input_data = data,
    new_data = data,
    effects = effects,
    seed = seed,
    silent = TRUE
  )

  ref <- paste0("testdata/", make_hashed_filename(effects, prefix = "params")) %>%
    testthat::test_path() %>%
    readr::read_csv(show_col_types = FALSE)

  expect_equal(
    result$fit$mcmc$summary(),
    ref,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )

  # interaction between varying effects of
  # categorical variables (with structured prior)
  effects <- list(
    Intercept = list(Intercept = intercept_prior),
    varying = list(
      cat1 = effect_prior,
      cat2 = effect_prior,
      cat3 = effect_prior
    ),
    interaction = list(
      `cat1:cat2` = struct_prior,
      `cat2:cat3` = struct_prior,
      `cat3:cat1` = struct_prior
    )
  ) %>%
    group_effects(data) %>%
    ungroup_effects()

  result <- run_mcmc(
    input_data = data,
    new_data = data,
    effects = effects,
    seed = seed,
    silent = TRUE
  )

  ref <- paste0("testdata/", make_hashed_filename(effects, prefix = "params")) %>%
    testthat::test_path() %>%
    readr::read_csv(show_col_types = FALSE)

  expect_equal(
    result$fit$mcmc$summary(),
    ref,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )

  # interaction between varying effects of
  # categorical variables and fixed effects of
  # continuous variables (with structured prior)
  effects <- list(
    Intercept = list(Intercept = intercept_prior),
    fixed = list(
      cont1 = effect_prior,
      cont2 = effect_prior
    ),
    varying = list(
      cat1 = effect_prior,
      cat2 = effect_prior
    ),
    interaction = list(
      `cat1:cont1` = struct_prior,
      `cat1:cont2` = struct_prior,
      `cat2:cont1` = struct_prior,
      `cat2:cont2` = struct_prior
    )
  ) %>%
    group_effects(data) %>%
    ungroup_effects()

  result <- run_mcmc(
    input_data = data,
    new_data = data,
    effects = effects,
    seed = seed,
    silent = TRUE
  )

  ref <- paste0("testdata/", make_hashed_filename(effects, prefix = "params")) %>%
    testthat::test_path() %>%
    readr::read_csv(show_col_types = FALSE)

  expect_equal(
    result$fit$mcmc$summary(),
    ref,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )

  # interaction between varying effects of
  # categorical variables and fixed effects of
  # binary variables (with structured prior)
  effects <- list(
    Intercept = list(Intercept = intercept_prior),
    fixed = list(
      bin1 = effect_prior,
      bin2 = effect_prior
    ),
    varying = list(
      cat1 = effect_prior,
      cat2 = effect_prior
    ),
    interaction = list(
      `bin1:cat1` = struct_prior,
      `bin1:cat2` = struct_prior,
      `bin2:cat1` = struct_prior,
      `bin2:cat2` = struct_prior
    )
  ) %>%
    group_effects(data) %>%
    ungroup_effects()

  result <- run_mcmc(
    input_data = data,
    new_data = data,
    effects = effects,
    seed = seed,
    silent = TRUE
  )

  ref <- paste0("testdata/", make_hashed_filename(effects, prefix = "params")) %>%
    testthat::test_path() %>%
    readr::read_csv(show_col_types = FALSE)

  expect_equal(
    result$fit$mcmc$summary(),
    ref,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )

})
