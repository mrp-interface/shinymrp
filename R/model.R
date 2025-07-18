#' MRPModel objects
#'
#' @description A `MRPModel` object is an [R6][R6::R6Class] object created by the
#' `$create_model()` method of a
#' [`MRPWorkflow`][MRPWorkflow] object. Each `MRPModel` object represents a
#' multilevel regression model with methods for sampling, diagnostics,
#' and post-stratification.
#'
#' @format
#'   An [R6][R6::R6Class] generator object.
#' 
#' @docType class
#' 
#' @export 
#' 
#' @importFrom R6 R6Class
MRPModel <- R6::R6Class(
  "MRPModel",
  private = list(
    effects_ = NULL,
    formula_ = NULL,
    mrp_ = NULL,
    metadata_ = NULL,
    linkdata_ = NULL,
    plotdata_ = NULL,
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
    #' @description Creates a new instance of the MRPModel class with specified effects, data, and metadata for Bayesian model fitting.
    #'
    #' @param effects List containing model effects specification including intercept, fixed effects, varying effects, and interactions
    #' @param mrp List containing the MRP data structure with input sample data and new post-stratification data
    #' @param metadata List containing metadata about the analysis including family, time variables, and special cases
    #' @param linkdata List containing information about data linking including geography and ACS year
    #' @param plotdata List containing data prepared for visualization including dates and geojson objects
    #'
    #' @return A new `MRPModel` object initialized with the provided effects, MRP data, metadata, link data, and plot data.
    initialize = function(
      effects,
      mrp,
      metadata,
      linkdata,
      plotdata
    ) {

      private$effects_ <- effects %>%
        group_effects(mrp$input) %>%
        ungroup_effects()
      private$formula_ <- create_formula(effects)
      private$mrp_ <- mrp
      private$metadata_ <- metadata
      private$linkdata_ <- linkdata
      private$plotdata_ <- plotdata
    },

    #' @description Retrieves the effects specification used in the model, including intercept, fixed effects, varying effects, and interactions.
    effects = function() {
      return(private$effects_)
    },

    #' @description Retrieves the model formula constructed from the effects specification.
    formula = function() {
      return(private$formula_)
    },

    #' @description Retrieves the MRP data structure containing input sample data and post-stratification data.
    mrp = function() {
      return(private$mrp_)
    },

    #' @description Retrieves the metadata associated with the model, including information about family, time variables, and fitting parameters.
    metadata = function() {
      return(private$metadata_)
    },

    #' @description Retrieves the data prepared for visualization, including dates and geojson objects.
    plotdata = function() {
      return(private$plotdata_)
    },

    #' @description Retrieves the data linking information including geography and ACS year.
    linkdata = function() {
      return(private$linkdata_)
    },

    #' @description Fits the MRP model using Stan for Bayesian estimation with MCMC sampling.
    #' 
    #' @param n_iter Number of MCMC iterations per chain
    #' @param n_chains Number of MCMC chains to run
    #' @param seed Random seed for reproducibility
    #' @param extra Additional parameters for model fitting
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

    #' @description Checks whether the model has been fitted and results are available.
    check_fit_exists = function() {
      return(!is.null(private$fit_))
    },

    #' @description Retrieves the Stan model code used for MCMC fitting.
    code = function() {
      private$assert_fit_exists()

      return(private$stan_code_$mcmc)
    },

    #' @description Retrieves a summary of the fitted model parameters including posterior means, credible intervals, and diagnostics.
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

    #' @description Retrieves MCMC diagnostics including convergence statistics and sampling efficiency measures.
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

    #' @description Runs posterior predictive checks to assess model fit by generating replicated data from the posterior predictive distribution.
    #'
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

    #' @description Runs leave-one-out cross-validation to assess model predictive performance and enable model comparison.
    #'
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

    #' @description Runs post-stratification using the fitted model to generate population-level estimates across different subgroups and geographies.
    #'
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

    #' @description Saves a fitted MRPModel object to a file for later use.
    #'
    #' @param model Fitted MRPModel object to save
    #' @param file File path where the model should be saved
    save = function(file) {
      checkmate::assert_file_exists(file)

      # load CmdStan output files into the fitted model object
      if (!is.null(private$fit_)) {
        private$fit_$draws()
      }

      qs::qsave(self, file = file)
    }
  )
)
