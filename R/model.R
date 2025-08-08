#' MRPModel objects
#'
#' @description A `MRPModel` object is an [R6][R6::R6Class] object created by the
#' `$create_model()` method of a
#' [`MRPWorkflow`][MRPWorkflow] object. Each `MRPModel` object represents a
#' multilevel regression model with methods for sampling, diagnostics,
#' and post-stratification.

#' @section Methods: `MRPModel` objects have the following associated
#'   methods, many of which have their own (linked) documentation pages:
#'
#'   ## Data access
#'
#'   |**Method**|**Description**|
#'   |:----------|:---------------|
#'   [`$effects()`][MRPModel-method-effects] | Return model specification. |
#'   [`$formula()`][MRPModel-method-formula] | Return model formula. |
#'   [`$mrp()`][MRPModel-method-mrp] | Return data for MRP. |
#'   [`$metadata()`][MRPModel-method-metadata] | Return model metadata. |
#'   [`$plotdata()`][MRPModel-method-plotdata] | Return data used for plotting. |
#'
#'   ## Model fitting
#'   |**Method**|**Description**|
#'   |:----------|:---------------|
#'   [`$fit()`][MRPModel-method-fit] | Fit multilevel regression model using [cmdstanr][cmdstanr]. |
#'   [`$check_fit_exists()`][MRPModel-method-check_fit_exists] | Check if model has been fitted. |
#'   [`$code()`][MRPModel-method-code] | Return Stan code. |
#'
#'   ## Model diagnostics
#'   |**Method**|**Description**|
#'   |:----------|:---------------|
#'   [`$summary()`][MRPModel-method-summary] | Return posterior summary table. |
#'   [`$diagnostics()`][MRPModel-method-diagnostics] | Return sampling diagnostics. |
#'   [`$ppc()`][MRPModel-method-ppc] | Run posterior predictive check. |
#'   [`$loo()`][MRPModel-method-loo] | Create inputs for leave-one-out cross-validation. |
#'
#'   ## Post-stratification
#'   |**Method**|**Description**|
#'   |:----------|:---------------|
#'   [`$poststratify()`][MRPModel-method-poststratify] | Run post-stratification to generate population estimates. |
#'
#'   ## Other
#'   |**Method**|**Description**|
#'   |:----------|:---------------|
#'   [`$save()`][MRPModel-method-save] | Save model object to file. |
#'
#'
#' @examples
#'   \dontrun{
#'   library(shinymrp)
#'   } 
#' 
#' @export 
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
    standata_ = NULL,
    stancode_ = NULL,
    diagnostics_ = NULL,
    params_ = NULL,
    log_lik_ = NULL,
    yrep_ = NULL,
    est_ = NULL,
    buffer = NULL,

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
        .group_effects(mrp$input) %>%
        .ungroup_effects()
      private$formula_ <- .create_formula(private$effects_)
      private$mrp_ <- mrp
      private$metadata_ <- metadata
      private$linkdata_ <- linkdata
      private$plotdata_ <- plotdata
      private$buffer <- list(
        interval = NULL
      )
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
    
    #' @description Retrieves the Stan data structure used for MCMC sampling
    standata = function() {
      return(private$standata_)
    },

    #' @description Retrieves Stan code.
    stancode = function() {
      return(private$stancode_)
    }
  )
)


#' Fit multilevel regression model using cmdstanr
#'
#' @name MRPModel-method-fit
#' @aliases fit
#' @family MRPModel methods
#'
#' @description Fits the MRP model using Stan for Bayesian estimation with MCMC sampling.
#'
#' @param n_iter Number of MCMC iterations per chain
#' @param n_chains Number of MCMC chains to run
#' @param seed Random seed for reproducibility
#' @param extra Additional parameters for model fitting
#' @param ... Additional arguments passed to CmdStanR's `sample()` method
fit <- function(
  n_iter = 2000,
  n_chains = 4,
  seed = NULL,
  extra = NULL,
  ...
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

  mcmc <- .run_mcmc(
    input_data  = .stan_factor(private$mrp_$input),
    new_data = .stan_factor(private$mrp_$new),
    effects = private$effects_,
    metadata = private$metadata_,
    n_iter = n_iter,
    n_chains = n_chains,
    seed = seed,
    extra = extra,
    ...
  )

  private$fit_ <- mcmc$fit
  private$standata_ <- mcmc$stan_data
  private$stancode_ <- mcmc$stan_code
}
MRPModel$set("public", "fit", fit)

#' Check if model has been fitted
#'
#' @name MRPModel-method-check_fit_exists
#' @aliases check_fit_exists
#' @family MRPModel methods
#'
#' @description Checks whether the model has been fitted and results are available.
check_fit_exists <- function() {
  return(!is.null(private$fit_))
}
MRPModel$set("public", "check_fit_exists", check_fit_exists)

#' Return Stan code
#'
#' @name MRPModel-method-code
#' @aliases code
#' @family MRPModel methods
#'
#' @description Retrieves the Stan model code used for MCMC fitting.
code <- function() {
  private$assert_fit_exists()

  return(private$stancode_$mcmc)
}
MRPModel$set("public", "code", code)

#' Return posterior summary table
#'
#' @name MRPModel-method-summary
#' @aliases summary
#' @family MRPModel methods
#'
#' @description Retrieves a summary of the fitted model parameters including posterior means, credible intervals, and diagnostics.
summary <- function() {
  private$assert_fit_exists()

  if (is.null(private$params_)) {
    private$params_ <- .get_parameters(
      fit = private$fit_,
      effects = private$effects_,
      input_data = private$mrp_$input,
      metadata = private$metadata_
    )
  }

  return(private$params_)
}
MRPModel$set("public", "summary", summary)

#' Return sampling diagnostics
#'
#' @name MRPModel-method-diagnostics
#' @aliases diagnostics
#' @family MRPModel methods
#'
#' @description Retrieves MCMC diagnostics including convergence statistics and sampling efficiency measures.
diagnostics <- function() {
  private$assert_fit_exists()

  if (is.null(private$diagnostics_$mcmc)) {
    out <- .get_diagnostics(
      fit = private$fit_,
      total_transitions = private$metadata_$n_iter / 2 * private$metadata_$n_chains
    )
    private$diagnostics_$mcmc <- out$summary
  }

  return(private$diagnostics_$mcmc)
}
MRPModel$set("public", "diagnostics", diagnostics)

#' Run posterior predictive check
#'
#' @name MRPModel-method-ppc
#' @aliases ppc
#' @family MRPModel methods
#'
#' @description Runs posterior predictive checks to assess model fit by generating replicated data from the posterior predictive distribution.
#'
ppc <- function() {
  private$assert_fit_exists()

  if (is.null(private$yrep_)) {
    message("Running posterior predictive check...")

    # run standalone generated quantities for PPC
    fit_ppc <- .run_gq(
      fit_mcmc = private$fit_,
      stan_code = private$stancode_$ppc,
      stan_data = private$standata_,
      n_chains = private$metadata_$n_chains
    )

    # extract draws and create summary table
    private$yrep_ <- .get_replicates(
      fit_ppc,
      private$mrp_$input,
      private$metadata_
    )
  }

  return(private$yrep_)
}
MRPModel$set("public", "ppc", ppc)

#' Create inputs for leave-one-out cross-validation
#'
#' @name MRPModel-method-loo
#' @aliases loo
#' @family MRPModel methods
#'
#' @description Runs leave-one-out cross-validation to assess model predictive performance and enable model comparison.
#'
loo <- function() {
  private$assert_fit_exists()

  if (is.null(private$log_lik_)) {
    message("Running leave-one-out cross-validation...")

    # run standalone generated quantities for LOO
    fit_loo <- .run_gq(
      fit_mcmc  = private$fit_,
      stan_code = private$stancode_$loo,
      stan_data = private$standata_,
      n_chains  = private$metadata_$n_chains
    )

    private$log_lik_ <- fit_loo$draws("log_lik")
  }

  return(private$log_lik_)
}
MRPModel$set("public", "loo", loo)

#' Run post-stratification to generate population estimates
#'
#' @name MRPModel-method-poststratify
#' @aliases poststratify
#' @family MRPModel methods
#'
#' @description Runs post-stratification using the fitted model to generate population-level estimates across different subgroups and geographies.
#'
#' @param interval Confidence interval or standard deviation for the estimates (default is 0.95)
poststratify <- function(interval = 0.95) {
  private$assert_fit_exists()

  .check_interval(interval)

  if (is.null(private$buffer$interval) || private$buffer$interval != interval) {
    message("Running post-stratification...")

    # run standalone generated quantities for post-stratification
    fit_pstrat <- .run_gq(
      fit_mcmc = private$fit_,
      stan_code = private$stancode_$pstrat,
      stan_data = private$standata_,
      n_chains = private$metadata_$n_chains
    )

    # extract draws and create summary table
    private$est_ <- .get_estimates(
      fit_pstrat,
      private$mrp_$new,
      private$metadata_,
      interval = interval
    )

    # store the interval in the buffer
    private$buffer$interval <- interval
  }

  return(private$est_)
}
MRPModel$set("public", "poststratify", poststratify)

#' Save model object to file
#'
#' @name MRPModel-method-save
#' @aliases save
#' @family MRPModel methods
#'
#' @description Saves a fitted MRPModel object to a file for later use.
#'
#' @param file File path where the model should be saved
save <- function(file) {
  checkmate::assert_path_for_output(
    file,
    overwrite = TRUE,
    extension = "qs"
  )

  # load CmdStan output files into the fitted model object
  if (!is.null(private$fit_)) {
    private$fit_$draws()
  }

  qs::qsave(self, file = file)
}
MRPModel$set("public", "save", save)
