create_test_data <- function(N=100, seed=sample(1:10000, 1)) {
  set.seed(seed)
  dat <- data.frame(
    cont1 = runif(N, 0, 10),
    cont2 = runif(N, 0, 10),
    bin1 = sample(0:1, N, replace = TRUE),
    bin2 = sample(0:1, N, replace = TRUE),
    cat1 = sample(1:3, N, replace = TRUE),
    cat2 = sample(1:5, N, replace = TRUE),
    positive = sample(1:5, N, replace = TRUE),
    total = sample(6:10, N, replace = TRUE)
  )

  return(dat)
}

compile <- function(effects, dat) {
  # convert to numeric factors
  stan_dat <- stan_factor(dat)

  # placeholder because only model compilation is tested
  gq_dat <- list(
    temporal = FALSE,
    subgroup = c()
  )

  # group effects
  effects <- effects %>%
    group_effects(stan_dat) %>%
    ungroup_effects()
    
  # create stan code and compile model
  make_stancode_mcmc(effects, gq_dat) %>%
    cmdstanr::write_stan_file() %>%
    cmdstanr::cmdstan_model()
}

test_that("all models compile", {
  data <- create_test_data(seed = 123)
  intercept_prior <- "normal(0, 5)"
  effect_prior <- "normal(0, 3)"
  struct_prior <- "structured"

  # fixed effect of a continuous variable
  expect_no_error(
    compile(
      list(
        Intercept = list(Intercept = intercept_prior),
        fixed = list(cont1 = effect_prior)
      ),
      data
    )
  )

  # fixed effects of a binary variables only
  expect_no_error(
    compile(
      list(
        Intercept = list(Intercept = intercept_prior),
        fixed = list(bin1 = effect_prior)
      ),
      data
    )
  )

  # fixed effects of a categorical variables only
  expect_no_error(
    compile(
      list(
        Intercept = list(Intercept = intercept_prior),
        fixed = list(cat1 = effect_prior)
      ),
      data
    )
  )

  # varying effects of categorical variables only
  expect_no_error(
    compile(
      list(
        Intercept = list(Intercept = intercept_prior),
        varying = list(cat1 = effect_prior)
      ),
      data
    )
  )

  # interaction between fixed effects of
  # continous variables (without structured prior)
  expect_no_error(
    compile(
      list(
        Intercept = list(Intercept = intercept_prior),
        fixed = list(cont1 = effect_prior, cont2 = effect_prior),
        interaction = list(`cont1:cont2` = effect_prior)
      ),
      data
    )
  )

  # interaction between fixed effects of
  # binary variables and continous variables
  # (without structured prior)
  expect_no_error(
    compile(
      list(
        Intercept = list(Intercept = intercept_prior),
        fixed = list(bin1 = effect_prior, cont1 = effect_prior),
        interaction = list(`bin1:cont1` = effect_prior)
      ),
      data
    )
  )


  # interaction between varying effects of
  # categorical variables and fixed effects of
  # continuous variables (without structured prior)
  expect_no_error(
    compile(
      list(
        Intercept = list(Intercept = intercept_prior),
        fixed = list(cont1 = effect_prior),
        varying = list(cat1 = effect_prior),
        interaction = list(`cat1:cont1` = effect_prior)
      ),
      data
    )
  )

  # interaction between varying effects of
  # categorical variables and fixed effects of
  # binary variables (without structured prior)
  expect_no_error(
    compile(
      list(
        Intercept = list(Intercept = intercept_prior),
        fixed = list(bin1 = effect_prior),
        varying = list(cat1 = effect_prior),
        interaction = list(`bin1:cat1` = effect_prior)
      ),
      data
    )
  )

  # interaction between varying effects of
  # categorical variables (without structured prior)
  expect_no_error(
    compile(
      list(
        Intercept = list(Intercept = intercept_prior),
        varying = list(cat1 = effect_prior, cat2 = effect_prior),
        interaction = list(`cat1:cat2` = effect_prior)
      ),
      data
    )
  )

  # interaction between varying effects of
  # categorical variables and fixed effects of
  # continuous variables (with structured prior)
  expect_no_error(
    compile(
      list(
        Intercept = list(Intercept = intercept_prior),
        fixed = list(cont1 = effect_prior),
        varying = list(cat1 = effect_prior),
        interaction = list(`cat1:cont1` = struct_prior)
      ),
      data
    )
  )

  # interaction between varying effects of
  # categorical variables and fixed effects of
  # binary variables (with structured prior)
  expect_no_error(
    compile(
      list(
        Intercept = list(Intercept = intercept_prior),
        fixed = list(bin1 = effect_prior),
        varying = list(cat1 = effect_prior),
        interaction = list(`bin1:cat1` = struct_prior)
      ),
      data
    )
  )

  # interaction between varying effects of
  # categorical variables (with structured prior)
  expect_no_error(
    compile(
      list(
        Intercept = list(Intercept = intercept_prior),
        varying = list(cat1 = effect_prior, cat2 = effect_prior),
        interaction = list(`cat1:cat2` = effect_prior)
      ),
      data
    )
  )

})
