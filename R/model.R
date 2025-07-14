MRPModel <- R6::R6Class(
  "MRPModel",
  private = list(
    effects_ = NULL,
    formula_ = NULL,
    mrp_ = NULL,
    metadata_ = NULL,
    link_data_ = NULL,
    plot_data_ = NULL,
    fit_ = NULL,
    stan_data_ = NULL,
    stan_code_ = NULL,
    diagnostics_ = NULL,
    params_ = NULL,
    log_lik_ = NULL,
    yrep_ = NULL,
    est_ = NULL,

    assert_fit_exists = function() {
      if (!self$check_fit_exists()) {
        stop("Model has not been fitted yet. Please call `fit()` first.")
      }
    }
  ),
  public = list(
    initialize = function(
      effects,
      mrp,
      metadata,
      link_data,
      plot_data
    ) {

      private$effects_ <- effects %>%
        group_effects(mrp$input) %>%
        ungroup_effects()
      private$formula_ <- create_formula(effects)
      private$mrp_ <- mrp
      private$metadata_ <- metadata
      private$link_data_ <- link_data
      private$plot_data_ <- plot_data
    },

    effects = function() {
      return(private$effects_)
    },

    formula = function() {
      return(private$formula_)
    },

    mrp = function() {
      return(private$mrp_)
    },

    metadata = function() {
      return(private$metadata_)
    },

    plot_data = function() {
      return(private$plot_data_)
    },

    fit = function(
      n_iter = 2000,
      n_chains = 4,
      seed = NULL,
      extra = NULL
    ) {

      private$metadata_ <- c(
        private$metadata_,
        list(
          n_iter = n_iter,
          n_chains = n_chains,
          seed = seed,
          extra = extra,
          pstrat_vars = intersect(GLOBAL$vars$pstrat, names(private$mrp_$levels))
        )
      )

      mcmc <- run_mcmc(
        input_data  = stan_factor(private$mrp_$input),
        new_data = stan_factor(private$mrp_$new),
        effects = private$effects_,
        metadata = private$metadata_,
        n_iter = n_iter,
        n_chains = n_chains,
        seed = seed,
        extra = extra
      )

      private$fit_ <- mcmc$fit
      private$stan_data_ <- mcmc$stan_data
      private$stan_code_ <- mcmc$stan_code
    },

    check_fit_exists = function() {
      return(!is.null(private$fit_))
    },

    code = function() {
      private$assert_fit_exists()

      return(private$stan_code_$mcmc)
    },

    summary = function() {
      private$assert_fit_exists()

      if (is.null(private$params_)) {
        private$params_ <- get_parameters(
          fit = private$fit_,
          effects = private$effects_,
          input_data = private$mrp_$input,
          metadata = private$metadata_
        )
      }

      return(private$params_)
    },

    diagnostics = function() {
      private$assert_fit_exists()

      if (is.null(private$diagnostics_$mcmc)) {
        out <- get_diagnostics(
          fit = private$fit_,
          total_transitions = private$metadata_$n_iter / 2 * private$metadata_$n_chains
        )
        private$diagnostics_$mcmc <- out$summary
      }

      return(private$diagnostics_$mcmc)
    },

    ppc = function() {
      private$assert_fit_exists()

      if (is.null(private$yrep_)) {
        message("Running posterior predictive check...")

        # run standalone generated quantities for PPC
        fit_ppc <- run_gq(
          fit_mcmc = private$fit_,
          stan_code = private$stan_code_$ppc,
          stan_data = private$stan_data_,
          n_chains = private$metadata_$n_chains
        )

        # extract draws and create summary table
        private$yrep_ <- get_replicates(
          fit_ppc,
          private$mrp_$input,
          private$metadata_
        )
      }

      return(private$yrep_)
    },

    loo = function() {
      private$assert_fit_exists()

      if (is.null(private$log_lik_)) {
        message("Running leave-one-out cross-validation...")

        # run standalone generated quantities for LOO
        fit_loo <- run_gq(
          fit_mcmc  = private$fit_,
          stan_code = private$stan_code_$loo,
          stan_data = private$stan_data_,
          n_chains  = private$metadata_$n_chains
        )

        private$log_lik_ <- fit_loo$draws("log_lik")
      }

      return(private$log_lik_)
    },

    poststratify = function() {
      private$assert_fit_exists()

      if (is.null(private$est_)) {
        message("Running post-stratification...")

        # run standalone generated quantities for post-stratification
        fit_pstrat <- run_gq(
          fit_mcmc = private$fit_,
          stan_code = private$stan_code_$pstrat,
          stan_data = private$stan_data_,
          n_chains = private$metadata_$n_chains
        )

        # extract draws and create summary table
        private$est_ <- get_estimates(
          fit_pstrat,
          private$mrp_$new,
          private$metadata_
        )
      }

      return(private$est_)
    },

    postprocess = function() {
      self$ppc()
      self$loo()
      self$poststratify()
    },

    save = function(file) {
      qs::qsave(self, file)
    }
  )
)
