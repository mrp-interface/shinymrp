#' MRPModel objects
#'
#' @description A `MRPModel` object is an [R6][R6::R6Class] object created by the
#' [`$create_model()`][MRPWorkflow-method-create_model] method of a
#' [`MRPWorkflow`][MRPWorkflow] object. Each `MRPModel` object represents a
#' multilevel regression model with methods for sampling, diagnostics,
#' and post-stratification.

#' @section Methods: `MRPModel` objects have the following associated
#'   methods, many of which have their own (linked) documentation pages:
#'
#'   ## Data access
#'   |**Method**|**Description**|
#'   |:----------|:---------------|
#'   [`$model_spec()`][MRPModel-method-model_spec] | Return model specification. |
#'   [`$formula()`][MRPModel-method-formula] | Return model formula. |
#'   [`$metadata()`][MRPModel-method-metadata] | Return model metadata. |
#'   [`$stan_code()`][MRPModel-method-stan_code] | Return model Stan code. |
#'
#'   ## Model fitting
#'   |**Method**|**Description**|
#'   |:----------|:---------------|
#'   [`$fit()`][MRPModel-method-fit] | Fit multilevel regression model using CmdStanR. |
#'   [`$check_fit_exists()`][MRPModel-method-check_fit_exists] | Check if model has been fitted. |
#'   [`$check_estimate_exists()`][MRPModel-method-check_estimate_exists] | Check if post-stratification has been performed. |
#'
#'   ## Posterior summary & diagnostics
#'   |**Method**|**Description**|
#'   |:----------|:---------------|
#'   [`$summary()`][MRPModel-method-summary] | Return posterior summary table. |
#'   [`$diagnostics()`][MRPModel-method-diagnostics] | Return sampling diagnostics. |
#' 
#'   ## Post-processing
#'   |**Method**|**Description**|
#'   |:----------|:---------------|
#'   [`$ppc()`][MRPModel-method-ppc] | Run posterior predictive check. |
#'   [`$loo()`][MRPModel-method-loo] | Create input for leave-one-out cross-validation. |
#'   [`$poststratify()`][MRPModel-method-poststratify] | Run post-stratification to generate population estimates. |
#'
#'   ## Saving model object
#'   |**Method**|**Description**|
#'   |:----------|:---------------|
#'   [`$save()`][MRPModel-method-save] | Save model object to file. |
#'
#'
#' @examples
#'   library(shinymrp)
#'
#'   # Initialize the MRP workflow
#'   workflow <- mrp_workflow()
#'
#'   # Load example data
#'   sample_data <- example_sample_data()
#'
#'   ### DATA PREPARATION
#'
#'   # Preprocess sample data
#'   workflow$preprocess(
#'     sample_data,
#'     is_timevar = TRUE,
#'     is_aggregated = TRUE,
#'     special_case = NULL,
#'     family = "binomial"
#'   )
#'
#'   # Link data to the ACS
#'   # and obtain poststratification data
#'   workflow$link_acs(
#'     link_geo = "zip",
#'     acs_year = 2021
#'   )
#'
#'   ### DESCRIPTIVE STATISTICS
#'
#'   # Visualize demographic distribution of data
#'   workflow$demo_bars(demo = "sex")
#'
#'   # Visualize geographic distribution of data
#'   workflow$sample_size_map()
#'
#'   # Visualize outcome measure
#'   workflow$outcome_plot()
#'
#'   ### MODEL BUILDING
#'
#'   # Create new model objects
#'   model <- workflow$create_model(
#'     model_spec = list(
#'       Intercept = list(
#'         Intercept = "normal(0, 5)"
#'       ),
#'       fixed = list(
#'         sex = "normal(0, 2)",
#'         race = "normal(0, 2)"
#'       ),
#'       varying = list(
#'         age = "normal(0, 2)",
#'         time = "normal(0, 2)"
#'       )
#'     )
#'   )
#'
#'   # Run MCMC
#'   model$fit(n_iter = 1000, n_chains = 2, seed = 123)
#'
#'   # Estimates summary and diagnostics
#'   model$summary()
#'
#'   # Sampling diagnostics
#'   model$diagnostics()
#'
#'   # Posterior predictive check
#'   workflow$pp_check(model)
#'
#'   ### VISUALIZE RESULTS
#'
#'   # Plots of overall estimates, estimates for demographic groups and geographic areas
#'   workflow$estimate_plot(model, group = "sex")
#'
#'   # Choropleth map of estimates for geographic areas
#'   workflow$estimate_map(model, geo = "county")
#' 
#' @export
#' 
#' @importFrom R6 R6Class
MRPModel <- R6::R6Class(
  "MRPModel",
  private = list(
    model_spec_ = NULL,
    formula_ = NULL,
    mrpdat_ = NULL,
    metadat_ = NULL,
    linkdat_ = NULL,
    plotdat_ = NULL,
    fit_ = NULL,
    standata_ = NULL,
    stancode_ = NULL,
    diagnostics_ = NULL,
    params_ = NULL,
    log_lik_ = NULL,
    yrep_ = NULL,
    est_ = NULL,
    buffer_ = NULL,

    assert_fit_exists = function() {
      if (!self$check_fit_exists()) {
        stop("Model has not been fitted yet. Please call `fit()` first.")
      }
    }
  ),
  public = list(
    #' @description Creates a new instance of the MRPModel class with specified effects, data, and metadata for Bayesian model fitting.
    #'
    #' @param model_spec List containing model effects specification including intercept, fixed effects, varying effects, and interactions
    #' @param mrp_data List containing the MRP data structure with input sample data and new post-stratification data
    #' @param metadata List containing metadata about the analysis including family, time variables, and special cases
    #' @param link_data List containing information about data linking including geography and ACS year
    #' @param plot_data List containing data prepared for visualization including dates and geojson objects
    #'
    #' @return A new `MRPModel` object initialized with the provided effects, MRP data, metadata, link data, and plot data.
    initialize = function(
      model_spec,
      mrp_data,
      metadata,
      link_data,
      plot_data
    ) {

      private$model_spec_ <- model_spec %>%
        .group_effects(mrp_data$input) %>%
        .ungroup_effects()
      private$formula_ <- .create_formula(private$model_spec_)
      private$mrpdat_ <- mrp_data
      private$metadat_ <- metadata
      private$linkdat_ <- link_data
      private$plotdat_ <- plot_data
      private$buffer_ <- list(
        interval = NULL,
        summarize = NULL
      )
    },

    mrp_data = function() {
      return(private$mrpdat_)
    },

    plot_data = function() {
      return(private$plotdat_)
    },

    link_data = function() {
      return(private$linkdat_)
    },
    
    stan_data = function() {
      return(private$standata_)
    }
  )
)

#' Return model specification
#' 
#' @name MRPModel-method-model_spec
#' @aliases model_spec
#'
#'
#' @description The `$model_spec()` method returns the model specification list.
#' 
#' @return A list containing the model specification including intercept, fixed effects, varying effects, and interactions.
model_spec = function() {
  return(private$model_spec_)
}
MRPModel$set("public", "model_spec", model_spec)

#' Return model formula
#' 
#' @name MRPModel-method-formula
#' @aliases formula
#'
#'
#' @description The `$formula()` method returns the lme4-style formula constructed from the model specification list.
formula = function() {
  return(private$formula_)
}
MRPModel$set("public", "formula", formula)

#' Return model metadata
#'
#' @name MRPModel-method-metadata
#' @aliases metadata
#'
#'
#' @description The `$metadata()` method returns the metadata associated with the model,
#' including metadata inherited from a workflow object and model fitting parameters.
metadata = function() {
  return(private$metadat_)
}
MRPModel$set("public", "metadata", metadata)

#' Return Stan code
#'
#' @name MRPModel-method-stan_code
#' @aliases stan_code
#'
#'
#' @description The `$stan_code()` method return Stan code.
stan_code = function() {
  return(private$stancode_$mcmc)
}
MRPModel$set("public", "stan_code", stan_code)

#' Fit multilevel regression model using cmdstanr
#'
#' @name MRPModel-method-fit
#' @aliases fit
#'
#'
#' @description The `$fit()` method fits the model using Stan.
#'
#' @param n_iter Number of MCMC iterations per chain (including warmup iterations). Default is 2000.
#' @param n_chains Number of MCMC chains to run. Default is 4.
#' @param seed Random seed for reproducibility. Default is `NULL`.
#' @param extra A list of additional parameters for model fitting, such as sensitivity and specificity
#' for COVID data. For example, `list(sens = 0.7, spec = 0.999)`.
#' @param ... Additional arguments passed to CmdStanR's `$sample()` method
fit <- function(
  n_iter = 2000,
  n_chains = 4,
  seed = NULL,
  extra = NULL,
  ...
) {

  private$metadat_ <- utils::modifyList(
    private$metadat_,
    list(
      n_iter = n_iter,
      n_chains = n_chains,
      seed = seed,
      extra = extra,
      pstrat_vars = intersect(GLOBAL$vars$pstrat, names(private$mrpdat_$levels))
    )
  )

  mcmc <- .run_mcmc(
    input_data  = .stan_factor(private$mrpdat_$input),
    new_data = .stan_factor(private$mrpdat_$new),
    effects = private$model_spec_,
    metadata = private$metadat_,
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
#'
#'
#' @description The `$check_fit_exists()` method checks whether the model has been fitted.
#' 
#' @return Logical indicating whether the model has been fitted.
check_fit_exists <- function() {
  return(!is.null(private$fit_))
}
MRPModel$set("public", "check_fit_exists", check_fit_exists)

#' Check if poststratification has been performed
#'
#' @name MRPModel-method-check_estimate_exists
#' @aliases check_estimate_exists
#'
#'
#' @description The `$check_estimate_exists()` method checks whether poststratification has been performed.
#'
#' @return Logical indicating whether poststratification has been performed.
check_estimate_exists <- function() {
  return(!is.null(private$est_))
}
MRPModel$set("public", "check_estimate_exists", check_estimate_exists)

#' Return posterior summary table
#'
#' @name MRPModel-method-summary
#' @aliases summary
#'
#'
#' @description The `$summary()` method returns a summary of posterior samples for the model parameters and diagnostics.
#'
#' @return A data frame containing the summary statistics for the posterior samples and diagnostics.
summary <- function() {
  private$assert_fit_exists()

  if (is.null(private$params_)) {
    private$params_ <- .get_parameters(
      fit = private$fit_,
      effects = private$model_spec_,
      input_data = private$mrpdat_$input,
      metadata = private$metadat_
    )
  }

  return(private$params_)
}
MRPModel$set("public", "summary", summary)

#' Return sampling diagnostics
#'
#' @name MRPModel-method-diagnostics
#' @aliases diagnostics
#'
#'
#' @description The `$diagnostics()` method returns MCMC diagnostics including
#' convergence statistics and sampling efficiency measures.
#'
#' @param summarize Logical indicating whether to return a summarized version of the diagnostics (default is TRUE)
#'
#' @return A data.frame object if `summarize` is TRUE, otherwise a list of raw diagnostics
diagnostics <- function(summarize = TRUE) {
  private$assert_fit_exists()

  if (is.null(private$buffer_$summarize) || private$buffer_$summarize != summarize) {
    private$diagnostics_ <- .get_diagnostics(
      fit = private$fit_,
      total_transitions = private$metadat_$n_iter / 2 * private$metadat_$n_chains,
      summarize = summarize
    )
  }

  private$buffer_$summarize <- summarize

  return(private$diagnostics_)
}
MRPModel$set("public", "diagnostics", diagnostics)

#' Run posterior predictive check
#'
#' @name MRPModel-method-ppc
#' @aliases ppc



#' @description The `$ppc()` method runs Stan's standalone generated quantities
#' for posterior predictive checks.
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
      n_chains = private$metadat_$n_chains
    )

    # extract draws and create summary table
    private$yrep_ <- .get_replicates(
      fit_ppc,
      private$mrpdat_$input,
      private$metadat_
    )
  }

  return(private$yrep_)
}
MRPModel$set("public", "ppc", ppc)

#' Create inputs for leave-one-out cross-validation
#'
#' @name MRPModel-method-loo
#' @aliases loo
#'
#'
#' @description The `$loo()` method runs Stan's standalone generated quantities
#' and extracts log-likelihood values for leave-one-out cross-validation.
#'
loo <- function() {
  private$assert_fit_exists()

  if (is.null(private$log_lik_)) {
    # run standalone generated quantities for LOO
    fit_loo <- .run_gq(
      fit_mcmc  = private$fit_,
      stan_code = private$stancode_$loo,
      stan_data = private$standata_,
      n_chains  = private$metadat_$n_chains
    )

    private$log_lik_ <- fit_loo$draws("log_lik")
  }

  return(private$log_lik_)
}
MRPModel$set("public", "loo", loo)

#' Run poststratification to generate population estimates
#'
#' @name MRPModel-method-poststratify
#' @aliases poststratify
#'
#'
#' @description The `$poststratify()` method runs Stan's standalone generated quantities
#' and extracts posterior samples for poststratified estimates.
#'
#' @param interval Confidence interval (a numeric value between 0 and 1) or
#' standard deviation (`"1sd"`, `"2sd"`, or `"3sd"`) for the estimates (default is 0.95).
#'
#' @return A data.frame object containing the poststratified estimates and their
#' corresponding uncertainty intervals.
poststratify <- function(interval = 0.95) {
  private$assert_fit_exists()

  .check_interval(interval)

  if (is.null(private$buffer_$interval) || private$buffer_$interval != interval) {
    message("Running post-stratification...")

    # run standalone generated quantities for post-stratification
    fit_pstrat <- .run_gq(
      fit_mcmc = private$fit_,
      stan_code = private$stancode_$pstrat,
      stan_data = private$standata_,
      n_chains = private$metadat_$n_chains
    )

    # extract draws and create summary table
    private$est_ <- .get_estimates(
      fit_pstrat,
      private$mrpdat_$new,
      private$metadat_,
      interval = interval
    )

    # store the interval in the buffer_
    private$buffer_$interval <- interval
  }

  return(private$est_)
}
MRPModel$set("public", "poststratify", poststratify)

#' Save model object to file
#'
#' @name MRPModel-method-save
#' @aliases save
#'
#'
#' @description The `$save()` method saves a fitted MRPModel object to a file for later use.
#'
#' @param file File path where the model should be saved.
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
