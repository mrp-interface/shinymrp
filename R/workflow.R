#' Create a new MRPWorkflow object
#' 
#' @description Create a new [`MRPWorkflow`][MRPWorkflow] object that implements
#' the Bayesian data analysis workflow common in applications of
#' Multilevel Regression and Post-stratification (MRP).
#'
#' @return A `MRPWorkflow` object.
#' 
#' @export
mrp_workflow <- function() {
  MRPWorkflow$new()
}

#' MRPWorkflow objects
#'
#' @description A `MRPWorkflow` object is an [R6][R6::R6Class] object created by
#' the [`mrp_workflow()`][mrp_workflow] function. This class provides methods for all steps
#' in the workflow, from data preparation and visualization to model fitting.
#'
#'
#' @section Methods: `MRPWorkflow` objects have the following associated
#' methods with their own (linked) documentation pages:
#' 
#'  ## Data preparation
#' 
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$preprocess()`][MRPWorkflow-method-preprocess] | Preprocess sample data. |
#'  [`$preprocessed_data()`][MRPWorkflow-method-preprocessed_data] | Return preprocessed sample data. |
#'  [`$link_acs()`][MRPWorkflow-method-link_acs] | Link sample data to ACS data. |
#'  [`$load_pstrat()`][MRPWorkflow-method-load_pstrat] | Load custom poststratification data. |
#'
#'  ## Model fitting & diagnostics
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$create_model()`][MRPWorkflow-method-create_model] | Create a [`MRPModel`][MRPModel] object. |
#'  [`$pp_check()`][MRPWorkflow-method-pp_check] | Perform posterior predictive check. |
#'  [`$compare_models()`][MRPWorkflow-method-compare_models] | Compare models using LOO-CV. |
#' 
#'   ## Visualization
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$demo_bars()`][MRPWorkflow-method-demo_bars] | Create demographic comparison bar plots. |
#'  [`$covar_hist()`][MRPWorkflow-method-covar_hist] | Create geographic covariate distribution histograms. |
#'  [`$sample_size_map()`][MRPWorkflow-method-sample_size_map] | Create sample size map. |
#'  [`$outcome_plot()`][MRPWorkflow-method-outcome_plot] | Create summary plots of raw outcome measure. |
#'  [`$outcome_map()`][MRPWorkflow-method-outcome_map] | Visualize raw outcome measure by geography. |
#'  [`$estimate_plot()`][MRPWorkflow-method-estimate_plot] | Visualize estimates for demographic groups. |
#'  [`$estimate_map()`][MRPWorkflow-method-estimate_map] | Visualize estimates for geographic areas. |
#'
#' @examplesIf requireNamespace("cmdstanr", quietly = TRUE)
#'  \donttest{
#'    library(shinymrp)
#'
#'    # Initialize the MRP workflow
#'    workflow <- mrp_workflow()
#'
#'    # Load example data
#'    sample_data <- example_sample_data()
#'
#'    ### DATA PREPARATION
#'
#'    # Preprocess sample data
#'    workflow$preprocess(
#'      sample_data,
#'      is_timevar = TRUE,
#'      is_aggregated = TRUE,
#'      special_case = NULL,
#'      family = "binomial"
#'    )
#'
#'    # Link data to the ACS
#'    # and obtain poststratification data
#'    workflow$link_acs(
#'      link_geo = "zip",
#'      acs_year = 2021
#'    )
#'
#'    ### DESCRIPTIVE STATISTICS
#'
#'    # Visualize demographic distribution of data
#'    sex_bars <- workflow$demo_bars(demo = "sex")
#'
#'    # Visualize geographic distribution of data
#'    ss_map <- workflow$sample_size_map()
#'
#'    # Visualize outcome measure
#'    raw_outcome_plot <- workflow$outcome_plot()
#'
#'    ### MODEL BUILDING
#'
#'    # Create new model objects
#'    model <- workflow$create_model(
#'      intercept_prior = "normal(0, 4)",
#'      fixed = list(
#'        sex = "normal(0, 2)",
#'        race = "normal(0, 2)"
#'      ),
#'      varying = list(
#'        age = "",
#'        time = ""
#'      )
#'    )
#'
#'    # Run MCMC
#'    model$fit(n_iter = 500, n_chains = 2, seed = 123)
#'
#'    # Estimates summary and diagnostics
#'    model$summary()
#'
#'    # Sampling diagnostics
#'    model$diagnostics()
#'
#'    # Posterior predictive check
#'    workflow$pp_check(model)
#'
#'    ### VISUALIZE RESULTS
#'
#'    # Plots of overall estimates, estimates for demographic groups, and geographic areas
#'    workflow$estimate_plot(model, group = "sex")
#'
#'    # Choropleth map of estimates for geographic areas
#'    workflow$estimate_map(model, geo = "county")
#'  }
#'
#' @export
MRPWorkflow <- R6::R6Class(
  "MRPWorkflow",
  private = list(
    model_class_ = MRPModel,
    prepdat_ = NULL,
    metadat_ = NULL,
    linkdat_ = NULL,
    plotdat_ = NULL,
    mrpdat_ = NULL,

    assert_metadata_exists = function() {
      if (is.null(private$metadat_)) {
        stop("Metadata is not available. Please run 'preprocess' first.")
      }
    },

    assert_data_exists = function() {
      if (is.null(private$prepdat_)) {
        stop("Sample data is not available. Please run 'preprocess' first.")
      }
    },

    assert_mrp_exists = function() {
      if (is.null(private$mrpdat_)) {
        stop("Data for MRP is not available. Please run 'link_acs' or 'load_pstrat' first.")
      }
    },

    assert_model_spec = function(model_spec) {
      private$assert_mrp_exists()

      # check if model_spec$intercept is a list with a single element
      if (!is.list(model_spec$intercept) || names(model_spec$intercept) != "intercept") {
        stop("model_spec$intercept must be a list with a single element named 'intercept'.")
      }

      # check prior syntax for all effects
      effects_w_priors <- unlist(model_spec)
      names(effects_w_priors) <- purrr::map_chr(
        names(effects_w_priors),
        ~ strsplit(.x, split = "\\.")[[1]][2]
      )

      bools <- effects_w_priors %>%
        purrr::map_lgl(function(s) .clean_prior_syntax(s) %>% .check_prior_syntax())

      if (!all(bools)) {
        stop(paste0("The following priors have invalid syntax: ",
            paste(effects_w_priors[!bools], collapse = ", ")))
      }

      # check if model_spec have corresponding variables in data
      main_vars <- c(names(model_spec$fixed), names(model_spec$varying))
      vars_in_data <- names(private$mrpdat_$input)
      invalid_vars <- setdiff(main_vars, vars_in_data)

      if (length(invalid_vars) > 0) {
        stop(paste0("The following variables are not present in the data: ",
            paste(invalid_vars, collapse = ", ")))
      }

      # check if any variables are single-level factors
      omit_vars <- private$mrpdat_$vars$omit
      if (length(intersect(main_vars, omit_vars$one_level)) > 0) {
        stop(paste0("The following variables have a single level and cannot be used: ",
            paste(omit_vars$one_level, collapse = ", ")))
      }

      # check if any interactions with nested main effects
      if (length(.pair_intersect(names(model_spec$interaction), omit_vars$nested)) > 0) {
        stop(paste0("The following interactions have nested main effects and cannot be used: ",
            paste(omit_vars$nested, collapse = ", ")))
      }

      # check if effects can be assigned structured prior
      ints_w_struct <- names(effects_w_priors[effects_w_priors == "structured"])
      valid_ints_w_struct <- .interactions_for_structured(
        interactions = ints_w_struct,
        fixed_effects = names(model_spec$fixed),
        data = private$mrpdat_$input
      )
      invalid_ints_w_struct <- setdiff(ints_w_struct, valid_ints_w_struct)
      if (length(invalid_ints_w_struct) > 0) {
        stop(paste0("The following effects cannot be assigned structured prior: ",
            paste(invalid_ints_w_struct, collapse = ", ")))
      }

      # check if effects can be assigned ICAR or BYM2 prior
      vars_w_icar <- names(effects_w_priors[effects_w_priors %in% c("icar", "bym2")])
      valid_vars_w_icar <- intersect(vars_w_icar, .const()$vars$geo) %>%
        setdiff(names(model_spec$fixed))
      invalid_vars_w_icar <- setdiff(vars_w_icar, valid_vars_w_icar)
      if (length(invalid_vars_w_icar) > 0) {
        stop(paste0("The following effects cannot be assigned ICAR or BYM2 prior: ",
            paste(invalid_vars_w_icar, collapse = ", ")))
      }
    },

    assert_model = function(model) {
      checkmate::assert_class(
        model,
        classes = "MRPModel",
        null.ok = FALSE
      )
    }
  ),
  public = list(
    initialize = function() {},

    metadata = function() {
      return(private$metadat_)
    }
  )
)

#' Return preprocessed sample data
#'
#' @name MRPWorkflow-method-preprocessed_data
#' @aliases preprocessed_data
#'
#' @description The `$preprocessed_data()` method returns the preprocessed sample data.
#' Check out the [More examples of R6 classes](https://mrp-interface.github.io/shinymrp/articles/example#preprocessed_data)
#' vignette for usage examples.
#'
#' @return A data.frame object containing the preprocessed sample data.
preprocessed_data <- function() {
  return(private$prepdat_)
}
MRPWorkflow$set("public", "preprocessed_data", preprocessed_data)


#' Preprocess sample data
#'
#' @name MRPWorkflow-method-preprocess
#' @aliases preprocess
#'
#' @description The `$preprocess()` method runs the preprocessing pipeline
#' that includes data standardization, filtering, imputation, and aggregation.
#' See the [More on data preparation](https://mrp-interface.github.io/shinymrp/articles/data-prep)
#' vignette for more information about data processing. For usage examples, refer to the
#' [More examples of R6 classes](https://mrp-interface.github.io/shinymrp/articles/example#preprocess)
#' vignette.
#'
#' @param data An object of class `data.frame` (or one that can be coerced to that class) that satisfies
#' the requirements specified in the [More on data preparation](https://mrp-interface.github.io/shinymrp/articles/data-prep) vignette.
#' @param is_timevar Logical indicating whether the data contains time-varying components.
#' @param is_aggregated Logical indicating whether the data is already aggregated.
#' @param special_case Character string specifying special case handling. Options are `NULL` (the default), `"covid"`, and `"poll"`.
#' @param family Character string specifying the distribution family for the outcome variable. Options are `"binomial"` for binary outcome measures and `"normal"` for continuous outcome measures.
#' @param time_freq Character string specifying the time indexing frequency or time length for grouping dates (YYYY-MM-DD) in the data.
#' Options are `NULL` (the default), `"week"`, `"month"`, and `"year"`. This parameter must be `NULL` for cross-sectional data
#' or time-varying data that already has time indices.
#' @param freq_threshold Numeric value specifying the minimum frequency threshold for including observations.
#' Values with lower frequency will cause the entire row to be removed. The default value is 0 (no filtering).
#' 
#' @return No return value, called for side effects.
preprocess <- function(
  data,
  is_timevar = FALSE,
  is_aggregated = FALSE,
  special_case = NULL,
  family = NULL,
  time_freq = NULL,
  freq_threshold = 0
) {

  checkmate::assert_choice(
    family,
    choices = .const()$args$family,
    null.ok = TRUE
  )
  
  private$metadat_ <- list(
    is_timevar = is_timevar,
    special_case = special_case,
    family = family,
    time_freq = time_freq
  )

  .check_data_spec(private$metadat_)

  message("Preprocessing sample data...")

  tryCatch({
    zip_county_state <- .fetch_data("zip_county_state.csv", subdir = "geo")

    private$prepdat_ <- .preprocess(
      data = data,
      metadata = private$metadat_,
      zip_county_state = zip_county_state,
      freq_threshold = freq_threshold,
      is_sample = TRUE,
      is_aggregated = is_aggregated
    )
  
  }, error = function(e) {
    # show error message
    error_message <- paste("Error processing sample data:\n", e$message)
    message(error_message)
  })

}
MRPWorkflow$set("public", "preprocess", preprocess)

#' Link sample data to ACS data
#'
#' @name MRPWorkflow-method-link_acs
#' @aliases link_acs
#'
#' @description The `$link_acs()` method obtains poststratification data by
#' linking the preprocessed sample data to the American Community Survey
#' based on given geographic granularity and year. See the
#' [More on data preparation](https://mrp-interface.github.io/shinymrp/articles/data-prep)
#' vignette for more information on data linking. For usage examples, refer to the
#' [More examples of R6 classes](https://mrp-interface.github.io/shinymrp/articles/example#link_acs)
#' vignette.
#'
#' @param link_geo Character string specifying the geographic level for linking. Options are `"zip"`, `"county"`, and `"state"`.
#' @param acs_year Numeric value specifying the last year of the data collection period for the target ACS dataset.
#'
#' @return No return value, called for side effects.
link_acs <- function(
  link_geo = NULL,
  acs_year = 2023
) {

  checkmate::assert_choice(
    link_geo,
    choices = .const()$vars$geo,
    null.ok = TRUE
  )

  checkmate::assert_choice(
    acs_year,
    choices = .const()$args$acs_years,
    null.ok = TRUE
  )

  private$assert_data_exists()

  message("Linking data to the ACS...")

  # store user's selections for data linking
  private$linkdat_ <- list(
    link_geo = link_geo,
    acs_year = acs_year
  )

  tryCatch({
    if (identical(private$metadat_$special_case, "covid")) {
      if (is.null(private$linkdat_$link_geo) ||
          private$linkdat_$link_geo != "zip") {
        private$linkdat_$link_geo <- "zip"
        warning("Linking geography is either incorrect or not specified. Using 'zip' as default for COVID data.")
      }

      pstrat_covid <- .fetch_data("pstrat_covid.csv", subdir = "acs")
      covar_covid <- .fetch_data("covar_covid.csv", subdir = "acs")

      # prepare data for MRP
      private$mrpdat_ <- .prepare_mrp_covid(
        input_data = private$prepdat_,
        pstrat_data = pstrat_covid,
        covariates = covar_covid,
        metadata   = private$metadat_
      )

      # prepare data for plotting
      private$plotdat_ <- list(
        dates = if("date" %in% names(private$prepdat_)) .get_dates(private$prepdat_) else NULL,
        geojson = list(county = .filter_geojson(
          geojson_$county,
          private$mrpdat_$levels$county
        )),
        raw_covariates = covar_covid %>%
          filter(.data$zip %in% unique(private$mrpdat_$input$zip))
      )

    } else if (identical(private$metadat_$special_case, "poll")) {

      if (is.null(private$linkdat_$link_geo) ||
          private$linkdat_$link_geo != "state") {
        private$linkdat_$link_geo <- "state"
        warning(stringr::str_interp("Linking geography is either incorrect or not specified. Using 'state' as default for polling data."))
      }

      new_data <- .fetch_data("pstrat_poll.csv", subdir = "acs") %>%
        mutate(state = .to_fips(.data$state, "state"))

      private$mrpdat_ <- .prepare_mrp_custom(
        input_data = private$prepdat_,
        new_data = new_data,
        metadata = private$metadat_,
        link_geo = "state"
      )

      # prepare data for plotting
      private$plotdat_ <- list(
        geojson = list(state = .filter_geojson(
          geojson_$state,
          private$mrpdat_$levels$state
        ))
      )

    } else {
      if (is.null(private$linkdat_$acs_year)) {
        private$linkdat_$acs_year <- .const()$args$acs_years[length(.const()$args$acs_years)]
        warning(stringr::str_interp("ACS year not specified. Using the latest year: ${private$linkdat_$acs_year}."))
      }

      # retrieve ACS data based on user's input
      acs_year <- private$linkdat_$acs_year
      tract_data <- .fetch_data(
        paste0("acs_", acs_year - 4, "-", acs_year, ".csv"),
        subdir = "acs"
      )
      zip_tract <- .fetch_data("zip_tract.csv", subdir = "geo")

      # prepare data for MRP
      private$mrpdat_ <- .prepare_mrp_acs(
        input_data = private$prepdat_,
        tract_data = tract_data,
        zip_tract = zip_tract,
        metadata = private$metadat_,
        link_geo = private$linkdat_$link_geo
      )

      # prepare data for plotting
      plotdata <- list()
      plotdata$dates <- if("date" %in% names(private$prepdat_)) .get_dates(private$prepdat_) else NULL
      plotdata$geojson <- names(geojson_) %>%
        stats::setNames(nm = .) %>%
        purrr::map(~.filter_geojson(
          geojson = geojson_[[.x]], 
          geoids = private$mrpdat_$levels[[.x]]
        ))

      private$plotdat_ <- .nullify(plotdata)
    }
  }, error = function(e) {
    message(paste("Error linking data:\n", e$message))
  })
}
MRPWorkflow$set("public", "link_acs", link_acs)

#' Load custom poststratification data
#'
#' @name MRPWorkflow-method-load_pstrat
#' @aliases load_pstrat
#'
#' @description The `$load_pstrat()` method processes and stores input poststratification data.
#' The object is subject to the same data preprocessing steps as the sample data. See the
#' [More on data preparation](https://mrp-interface.github.io/shinymrp/articles/data-prep)
#' vignette for more information on data processing. For usage examples, refer to the
#' [More examples of R6 classes](https://mrp-interface.github.io/shinymrp/articles/example#load_pstrat)
#' vignette.
#'
#' @param pstrat_data An object of class `data.frame` (or one that can be coerced to that class).
#' @param is_aggregated Logical indicating whether the poststratification data is already aggregated.
#'
#' @return No return value, called for side effects.
load_pstrat <- function(pstrat_data, is_aggregated = TRUE) {
  if (!is.null(private$metadat_$special_case)) {
    stop("Custom poststratification data is not supported for special cases like COVID or polling data. Please use the `link_acs` method instead.")
  }

  if (is.null(private$prepdat_)) {
    stop("Sample data is not available. Please provide sample data through the `preprocess` method first.")
  }

  if (is.null(pstrat_data)) {
    stop("Poststratification data is required. Please provide valid pstrat_data.")
  }

  message("Preprocessing poststratification data...")

  tryCatch({
    zip_county_state <- .fetch_data("zip_county_state.csv", subdir = "geo")
    metadata <- private$metadat_
    metadata$time_freq <- NULL

    # Process data
    new_data <- .preprocess(
      data = pstrat_data,
      metadata = metadata,
      zip_county_state = zip_county_state,
      is_sample = FALSE,
      is_aggregated = is_aggregated
    )

    # Compare to sample data
    .check_pstrat(new_data, private$prepdat_, .create_expected_levels(private$metadat_))

    # Find the smallest common geography
    link_geo <- NULL
    common <- intersect(names(private$prepdat_), names(new_data))
    smallest <- .get_smallest_geo(common)
    if (!is.null(smallest)) {
      link_geo <- smallest$geo
    }

    # Store linking geography
    private$linkdat_ <- list(
      link_geo = link_geo,
      acs_year = NULL
    )

    # Prepare data for MRP
    private$mrpdat_ <- .prepare_mrp_custom(
      input_data = private$prepdat_,
      new_data = new_data,
      metadata = private$metadat_,
      link_geo = link_geo
    )


    # prepare data for plotting
    plotdata <- list()
    plotdata$dates <- if("date" %in% names(private$prepdat_)) .get_dates(private$prepdat_) else NULL
    plotdata$geojson <- names(geojson_) %>%
      stats::setNames(nm = .) %>%
      purrr::map(~.filter_geojson(
        geojson = geojson_[[.x]], 
        geoids = private$mrpdat_$levels[[.x]]
      ))

    private$plotdat_ <- .nullify(plotdata)

  }, error = function(e) {
    # show error message
    error_message <- paste("Error processing poststratification data:\n", e$message)
    message(error_message)
    
    # reset fields
    private$linkdat_ <- NULL
    private$mrpdat_ <- NULL
    private$plotdat_ <- NULL
    
  })
}
MRPWorkflow$set("public", "load_pstrat", load_pstrat)

# --------------------------------------------------------------------------
# Visualization methods
# --------------------------------------------------------------------------

#' Create demographic comparison bar plots
#'
#' @name MRPWorkflow-method-demo_bars
#' @aliases demo_bars
#'
#'
#' @description Creates bar plots for comparing demographic distributions between
#' sample data and poststratification data. Check out the
#' [More examples of R6 classes](https://mrp-interface.github.io/shinymrp/articles/example#demo_bars)
#' vignette for usage examples.
#'
#' @param demo Character string specifying the demographic variable to plot.
#' @param file Optional file path to save the plot.
#' @param ... Additional arguments passed to [`ggsave`][ggplot2::ggsave], such as `width` and `height`.
#'
#' @return A ggplot object showing demographic comparisons
demo_bars <- function(demo, file = NULL, ...) {
  private$assert_mrp_exists()

  checkmate::assert_choice(
    demo,
    choices = intersect(
      .const()$vars$demo,
      names(private$mrpdat_$levels)
    ), 
    null.ok = FALSE
  )

  input_data <- private$mrpdat_$input %>%
    .as_factor(private$mrpdat_$levels[demo]) %>%
    mutate(demo = !!sym(demo)) %>%
    select("demo", "total")

  new_data <- private$mrpdat_$new
  if ("time" %in% names(new_data)) {
    new_data <- new_data %>% filter(.data$time == 1)
  }
  new_data <- new_data %>%
    .as_factor(private$mrpdat_$levels[demo]) %>%
    mutate(demo = !!sym(demo)) %>%
    select("demo", "total")

  p <- .plot_demographic(input_data, new_data)

  if (!is.null(file)) {
    # Set default parameters for ggsave
    dots <- utils::modifyList(.const()$plot$save, list(...))
    do.call(ggplot2::ggsave, c(list(filename = file, plot = p), dots))
  }

  return(p)
}
MRPWorkflow$set("public", "demo_bars", demo_bars)

#' Create geographic covariate distribution histogram
#'
#' @name MRPWorkflow-method-covar_hist
#' @aliases covar_hist
#'
#'
#' @description The `covar_hist()` method creates histogram plots
#' showing the distribution of geographic covariates across ZIP codes. Refer to the
#' [More on data preparation](https://mrp-interface.github.io/shinymrp/articles/data-prep#geographic-identifiers-and-covariates) for their definitions.
#' This method is only available for [COVID data](https://mrp-interface.github.io/shinymrp/articles/data-prep#data-modules).
#' Check out the [More examples of R6 classes](https://mrp-interface.github.io/shinymrp/articles/example#covar_hist)
#' vignette for usage examples.
#'
#' @param covar Character string specifying the geographic covariate. Options are
#' `"college"`, `"poverty"`, `"employment"`, `"income"`, `"urbanicity"`, and `"adi"`.
#' @param file Optional file path to save the plot.
#' @param ... Additional arguments passed to [`ggsave`][ggplot2::ggsave], such as `width` and `height`.
#'
#' @return A ggplot object showing the covariate distribution histogram.
covar_hist <- function(covar, file = NULL, ...) {
  private$assert_mrp_exists()
  
  raw_covariates <- private$plotdat_$raw_covariates
  if (is.null(raw_covariates)) {
    stop("Covariate data is not available. This method is only available for COVID data.")
  }
  
  checkmate::assert_choice(
    covar,
    choices = .const()$vars$covar,
    null.ok = FALSE
  )
  
  config <- switch(covar,
    "college" = list(
      threshold   = 0.5,
      operation   = ">=",
      breaks      = seq(0, 1, 0.05),
      description = "\n%d ZIP codes out of %d (%d%%) have %d%% or more people who have earned an Associate's degree or higher.",
      definition  = "Higher education measure of a zip code is defined as the percentage of the residing population\nwho have earned an Associate's degree or higher.",
      name        = "Higher education measure"
    ),
    "poverty" = list(
      threshold   = 0.2,
      operation   = "<=",
      breaks      = seq(0, 1, 0.05),
      description = "%d zip codes out of %d (%d%%) have %d%% or less people whose ratio of income to poverty level in the past 12 months\nis below 100%%.",
      definition  = "Poverty measure of a zip code is defined as the percentage of the residing population\nwhose ratio of income to poverty level in the past 12 months is below 100%%.",
      name        = "Poverty measure"
    ),
    "employment" = list(
      threshold   = 0.5,
      operation   = ">=",
      breaks      = seq(0, 1, 0.05),
      description = "\n%d zip codes out of %d (%d%%) have %d%% or more people who is employed as a part of the civilian labor force.",
      definition  = "Employment rate of a zip code is defined as the percentage of the residing population\nwho are employed as a part of the civilian labor force.",
      name        = "Employment rate"
    ),
    "income" = list(
      threshold   = 70784,
      operation   = ">",
      breaks      = seq(0, 150000, 5000),
      description = "%d zip codes out of %d (%d%%) have average value of tract-level median household income in the past 12 months\ngreater than %d dollars (2021 US median income according to the ACS).",
      definition  = "Income measure of a zip code is defined as the average value of tract-level median household income in the past 12 months\nweighted by tract population counts.",
      name        = "Average of median household income"
    ),
    "urbanicity" = list(
      threshold   = 0.95,
      operation   = ">=",
      breaks      = seq(0, 1, 0.05),
      description = "\n%d zip codes out of %d (%d%%) have %d%% or more tracts classified as urban.",
      definition  = "Urbanicity of a zip code is defined as the percentage of covered census tracts classified as urban\nweighted by tract population counts.",
      name        = "Urbanicity"
    ),
    "adi" = list(
      threshold   = 80,
      operation   = ">",
      breaks      = seq(0, 100, 5),
      description = "\n%d zip codes out of %d (%d%%) have ADI over %d.",
      definition  = "Area Deprivation Index (ADI) of a zip code is the average ADI across covered census tracts\nweighted by tract population counts (refer to Learn > Preprocess page for the definition of ADI).",
      name        = "Area Deprivation Index"
    )
  )
  
  # Calculate statistics for description
  count <- switch(config$operation,
    ">" = sum(raw_covariates[[covar]] > config$threshold, na.rm = TRUE),
    ">=" = sum(raw_covariates[[covar]] >= config$threshold, na.rm = TRUE),
    "<" = sum(raw_covariates[[covar]] < config$threshold, na.rm = TRUE),
    "<=" = sum(raw_covariates[[covar]] <= config$threshold, na.rm = TRUE)
  )
  total <- nrow(raw_covariates)
  perc <-  round(count / total * 100)
  threshold <- if(config$threshold > 1) config$threshold else config$threshold * 100

  # Create plot
  p <- raw_covariates %>%
    mutate(covar = !!sym(covar)) %>%
    .plot_geographic(
      breaks = config$breaks,
      description = sprintf(config$description, count, total, perc, threshold),
      definition = config$definition,
      name = config$name
    )

  if (!is.null(file)) {
    # Set default parameters for ggsave
    dots <- utils::modifyList(.const()$plot$save, list(...))
    do.call(ggplot2::ggsave, c(list(filename = file, plot = p), dots))
  }

  return(p)
}
MRPWorkflow$set("public", "covar_hist", covar_hist)

#' Create sample size map
#'
#' @name MRPWorkflow-method-sample_size_map
#' @aliases sample_size_map
#'
#' @description The `$sample_size_map()` method creates interactive choropleth maps 
#' showing data distribution with respect to geography. This method cannot be used
#' if either the sample or the poststratification data contains no geographic information.
#' Check out the [More examples of R6 classes](https://mrp-interface.github.io/shinymrp/articles/example#sample_size_map)
#' vignette for usage examples.
#'
#' @param file Optional file path with .html extension to save the interactive map.
#' Expand the hamburger menu in the top right corner of the map to access other export options.
#'
#' @return A highcharter map object showing sample size distribution.
sample_size_map <- function(file = NULL) {
  private$assert_mrp_exists()

  geo <- private$linkdat_$link_geo
  if (is.null(geo)) {
    stop("Linking geography is not available.")
  } else if (geo == "zip") {
    geo <- "county"
  }

  hc <- private$mrpdat_$input %>%
    .prep_sample_size(
      fips_codes = fips_[[geo]],
      geo = geo,
      for_map = TRUE
    ) %>%
    .choro_map(
      private$plotdat_$geojson[[geo]],
      geo = geo,
      config = list(
        main_title = "Sample Size Map",
        hover_title = "Sample Size"
      )
    ) %>%
    highcharter::hc_exporting(
      enabled = TRUE,
      filename = "sample_size_map",
      type = "image/png"
    )

  if (!is.null(file)) {
    # Set default parameters for ggsave
    htmlwidgets::saveWidget(
      hc,
      file = file,
      selfcontained = FALSE
    )
  }

  return(hc)
}
MRPWorkflow$set("public", "sample_size_map", sample_size_map)

#' Create summary plots of the outcome measure
#'
#' @name MRPWorkflow-method-outcome_plot
#' @aliases outcome_plot
#'
#' @description The `$outcome_plot()` method creates plots of the average outcome values.
#' Check out the [More examples of R6 classes](https://mrp-interface.github.io/shinymrp/articles/example#outcome_plot)
#' vignette for usage examples.
#'
#' @param file Optional file path to save the plot.
#' @param ... Additional arguments passed to [`ggsave`][ggplot2::ggsave], such as `width` and `height`.
#'
#' @return A ggplot object showing the outcome measure distribution.
outcome_plot <- function(file = NULL, ...) {
  private$assert_mrp_exists()

  p <- if (private$metadat_$is_timevar) {
    .plot_outcome_timevar(
      raw = private$mrpdat_$input,
      yrep_est = NULL,
      dates = private$plotdat_$dates,
      metadata = private$metadat_,

      show_caption = FALSE
    )
  } else {
    .plot_outcome_static(
      raw = private$mrpdat_$input,
      yrep_est = NULL,
      metadata = private$metadat_
    )
  }

  if (!is.null(file)) {
    # Set default parameters for ggsave
    dots <- utils::modifyList(.const()$plot$save, list(...))
    do.call(ggplot2::ggsave, c(list(filename = file, plot = p), dots))
  }

  return(p)
}
MRPWorkflow$set("public", "outcome_plot", outcome_plot)

#' Visualize raw outcome measure by geography
#'
#' @name MRPWorkflow-method-outcome_map
#' @aliases outcome_map
#'
#' @description The `$outcome_map()` method creates maps showing the average outcome values
#' by geography for cross-sectional data, or the highest/lowest temporal average for time-varying data.
#' The sample and poststratification data must contain geographic information for this method to work.
#' Check out the [More examples of R6 classes](https://mrp-interface.github.io/shinymrp/articles/example#outcome_map)
#' vignette for usage examples.
#'
#' @param summary_type Character string, for time-varying data, indicating whether to display the
#' highest (`"max"`) or lowest (`"min"`) temporal average. Leave as `NULL` for cross-sectional data.
#' @param file Optional file path with .html extension to save the interactive map.
#' Expand the hamburger menu in the top right corner of the map to access other export options.
#'
#' @return A highcharter map object showing average outcome measure by geography.
outcome_map <- function(summary_type = NULL, file = NULL) {
  private$assert_mrp_exists()

  checkmate::assert_choice(
    summary_type,
    choices = .const()$args$summary_types,
    null.ok = TRUE
  )

  if (private$metadat_$is_timevar && is.null(summary_type)) {
    stop("For time-varying data, please specify summary_type as either 'max' or 'min'.")
  } else if (!private$metadat_$is_timevar && !is.null(summary_type)) {
    warning("summary_type is only applicable for time-varying data. Ignoring the input.")
    summary_type <- NULL
  }

  geo <- private$linkdat_$link_geo
  if (is.null(geo)) {
    stop("Linking geography is not available.")
  } else if (geo == "zip") {
    geo <- "county"
  }

  out <- .prep_raw(
    private$mrpdat_$input,
    fips_[[geo]],
    geo = geo,
    summary_type = summary_type,
    metadata = private$metadat_
  )

  config <- list()
  if (n_distinct(out$plot_df$value) == 1 &&
      out$plot_df$value[1] == 0) {
    config <- list(minValue = 0, maxValue = 1)
  }
  config <- c(config, out$title)

  hc <- .choro_map(
    out$plot_df,
    private$plotdat_$geojson[[geo]],
    geo = geo,
    config = config
    ) %>%
    highcharter::hc_exporting(
      enabled = TRUE,
      filename = "outcome_map",
      type = "image/png"
    )

  if (!is.null(file)) {
    # Set default parameters for ggsave
    htmlwidgets::saveWidget(
      hc,
      file = file,
      selfcontained = TRUE
    )
  }

  return(hc)
}
MRPWorkflow$set("public", "outcome_map", outcome_map)

#' Visualize estimates for demographic groups
#'
#' @name MRPWorkflow-method-estimate_plot
#' @aliases estimate_plot
#'
#'
#' @description The `$estimate_plot()` method creates plots showing overall MRP estimates or
#' estimates for different demographic groups. Check out the
#' [More examples of R6 classes](https://mrp-interface.github.io/shinymrp/articles/example#estimate_plot)
#' vignette for usage examples.
#'
#' @param model Fitted MRPModel object
#' @param group Character string specifying the demographic group.
#' If left as `NULL`, overall estimates are plotted.
#' @param interval Confidence interval (a numeric value between 0 and 1) or
#' standard deviation (`"1sd"` or `"2sd"`) for the estimates (default is 0.95).
#' @param file Optional file path to save the plot.
#' @param show_caption Logical indicating whether to show the caption in the plot (default is TRUE).
#' @param ... Additional arguments passed to [`ggsave`][ggplot2::ggsave], such as `width` and `height`.
#'
#' @return A ggplot object showing MRP estimates.
estimate_plot <- function(
  model,
  group = NULL,
  interval = 0.95,
  show_caption = TRUE,
  file = NULL,
  ...
) {

  checkmate::assert_choice(
    group,
    choices = intersect(.const()$vars$pstrat, names(model$mrp_data()$levels)),
    null.ok = TRUE
  )

  est_list <- model$poststratify(interval = interval)

  if (is.null(group)) {
    est_df <- est_list$overall

    p <- if(model$metadata()$is_timevar) {
      .plot_outcome_timevar(
        raw = model$mrp_data()$input,
        yrep_est = est_df,
        dates = model$plot_data()$dates,
        metadata = model$metadata(),
        show_caption = show_caption
      )
    } else {
      .plot_outcome_static(
        raw = model$mrp_data()$input,
        yrep_est = est_df,
        metadata = model$metadata(),
        show_caption = show_caption
      )
    }
  } else {
    # Convert levels to "factor" type
    est_df <- est_list[[group]] %>%
      rename(!!group := factor) %>%
      .as_factor(model$mrp_data()$levels[group]) %>%
      rename(factor := !!sym(group))

    p <- if (model$metadata()$is_timevar) {
      .plot_est_timevar(
        plot_df = est_df,
        dates = model$plot_data()$dates,
        metadata = model$metadata(),
        interval = interval,
        show_caption = show_caption
      )
    } else {
      .plot_est_static(
        plot_df = est_df,
        metadata = model$metadata(),
        interval = interval,
        show_caption = show_caption
      )
    }
  }

  if (!is.null(file)) {
    # Set default parameters for ggsave
    settings <- if (!is.null(group)) {
      list(height = 4 * (length(model$mrp_data()$levels[[group]]) + 1))
    } else{
      list()
    }
    dots <- utils::modifyList(.const()$plot$save, settings) %>%
      utils::modifyList(list(...))
    do.call(ggplot2::ggsave, c(list(filename = file, plot = p), dots))
  }      

  return(p)
}
MRPWorkflow$set("public", "estimate_plot", estimate_plot)

#' Create a choropleth map of MRP estimates
#'
#' @name MRPWorkflow-method-estimate_map
#' @aliases estimate_map
#'
#' @description The `$estimate_map()` method creates interactive choropleth maps that show MRP estimates by geographic region.
#' This method cannot be used if either the sample or the poststratification data contains no geographic information.
#' Check out the [More examples of R6 classes](https://mrp-interface.github.io/shinymrp/articles/example#estimate_map)
#' vignette for usage examples.
#'
#' @param model Fitted MRPModel object
#' @param geo Character string specifying the geographic level for mapping.
#' Options include geography for data linking and those at larger scales.
#' A "linking" geography is required to use this method. It is either specified
#' as `geo` in the `$link_acs()` method or the smallest common geographic scale
#' between the sample data and the custom poststratification data
#' input using `$load_pstrat()`.
#'
#' @param time_index Integer specifying the time index for time-varying data.
#' @param interval Confidence interval (a numeric value between 0 and 1) or
#' standard deviation (`"1sd"` or `"2sd"`) for the estimates (default is 0.95).
#' @param file Optional file path with .html extension to save the interactive map.
#' Expand the hamburger menu in the top right corner of the map to access other export options.
#'
#' @return A highcharter map object showing MRP estimates by geography.
estimate_map <- function(
  model,
  geo = NULL,
  time_index = NULL,
  interval = 0.95,
  file = NULL
) {

  if (is.null(model$link_data()$link_geo)) {
    stop("Linking geography is not available.")
  }

  choices <- intersect(.const()$vars$geo2, names(model$mrp_data()$levels))
  checkmate::assert_choice(
    geo,
    choices = choices,
    null.ok = TRUE
  )
  geo <- geo %||% choices[1]

  time_index <- if (model$metadata()$is_timevar) {
    choices <- model$mrp_data()$levels$time
    checkmate::assert_choice(
      time_index,
      choices = choices,
      null.ok = TRUE
    )
    time_index %||% choices[1]
  } else {
    NULL
  }

  est_df <- model$poststratify(interval = interval)[[geo]]

  hc <- est_df %>% 
    .prep_est(
      fips_codes = fips_[[geo]],
      geo = geo,
      time_index = time_index,
      interval = interval
    ) %>%
    .choro_map(
      model$plot_data()$geojson[[geo]],
      geo = geo,
      config = list(
        minValue = 0,
        maxValue = max(est_df$est),
        main_title = "MRP Estimate",
        hover_title = "Estimate"
      )
    ) %>%
    highcharter::hc_exporting(
      enabled = TRUE,
      filename = "estimate_map",
      type = "image/png"
    )

  if (!is.null(file)) {
    # Set default parameters for ggsave
    htmlwidgets::saveWidget(
      hc,
      file = file,
      selfcontained = FALSE
    )
  }

  return(hc)
}
MRPWorkflow$set("public", "estimate_map", estimate_map)

#' Create a new MRPModel object
#'
#' @name MRPWorkflow-method-create_model
#' @aliases create_model
#'
#' @description The `$create_model()` method creates a new MRPModel object
#' with Stan code generated from the model specification list.
#' CmdStanR objects are used internally to interface with Stan to
#' compile the code and run its MCMC algorithm. Check out the
#' [More examples of R6 classes](https://mrp-interface.github.io/shinymrp/articles/example#create_model)
#' vignette for usage examples.
#'
#' @param intercept_prior Character string specifying the prior distribution for the overall intercept.
#' Check *Details* for more information about prior specification.
#' @param fixed List of the fixed effects in the model and their prior distributions.
#' Check *Details* for more information about prior specification.
#' @param varying List of the varying effects in the model and the prior distributions of their standard deviations.
#' Check *Details* for more information about prior specification.
#' @param interaction List of the interactions in the model and their prior distributions. Interaction names are
#' created by concatenating the names of the interacting variables with a colon (e.g., "sex:age"). Currently,
#' only two-way interactions are supported. Check *Details* for more information about prior specification.
#' @param sens Sensitivity adjustment in the COVID-19 test results. Check *Details* for more information.
#' @param spec Specificity adjustment in the COVID-19 test results. Check *Details* for more information.
#'
#' @details
#' #### Prior specification
#' The syntax for the prior distributions is similar to that of Stan. The following are currently supported:
#'
#' - normal(mu, sigma)
#' - student_t(nu, mu, sigma)
#' - structured*
#' - icar
#' - bym2
#'
#' The last one is a custom prior syntax for the structured prior distribution developed by [Si et al. (2020)](https://arxiv.org/abs/1707.08220).
#'
#' The following default prior distributions are assigned to effects with empty strings (`""`)
#' in the model specification list:
#'
#' - Overall intercept: normal(0,5)
#' - Coefficient: normal(0,3)
#'
#' The model assumes varying effects follow a normal distribution with an unknown standard deviation, which will be assigned with priors.
#'
#' - Standard deviation (main effect): normal<sub>+</sub>(0,3)
#' - Standard deviation (interaction): normal<sub>+</sub>(0,1)
#'
#' #### Testing sensitivity and specificity
#' For COVID data, we allow users to specify the PCR testing sensitivity and specificity. Let \eqn{p_k} be the probability
#' that person \eqn{i} in group \eqn{k} tests positive. The analytic incidence \eqn{p_k} is a function of the test sensitivity \eqn{\delta},
#' specificity \eqn{\gamma}, and the true incidence \eqn{\pi_k} for individuals in group \eqn{k}:
#' \deqn{p_k=(1-\gamma)(1-\pi_k )+\delta \pi_k.}
#'
#' @return A new MRPModel object.
create_model <- function(
  intercept_prior = NULL,
  fixed = NULL,
  varying = NULL,
  interaction = NULL,
  sens = 1,
  spec = 1
) {

  private$assert_mrp_exists()

  intercept_prior <- intercept_prior %||% ""
  model_spec <- list(
    intercept = list(intercept = intercept_prior),
    fixed = fixed,
    varying = varying,
    interaction = interaction
  )

  private$assert_model_spec(model_spec)

  private$model_class_$new(
    model_spec = model_spec,
    mrp_data = private$mrpdat_,
    metadata = private$metadat_,
    link_data = private$linkdat_,
    plot_data = private$plotdat_,
    extra = list(
      sens = sens,
      spec = spec
    )
  )
}
MRPWorkflow$set("public", "create_model", create_model)

#' Perform posterior predictive check
#'
#' @name MRPWorkflow-method-pp_check
#' @aliases pp_check
#' 
#' @description The `$pp_check()` method creates posterior predictive check plots
#' to assess model fit by comparing observed data to replicated data from
#' the posterior predictive distribution. Check out the
#' [More examples of R6 classes](https://mrp-interface.github.io/shinymrp/articles/example#pp_check)
#' vignette for usage examples.
#'
#' @param model Fitted MRPModel object.
#' @param file Optional file path to save the plot.
#' @param ... Additional arguments passed to [`ggsave`][ggplot2::ggsave], such as `width` and `height`.
#' 
#' @return A ggplot object showing the posterior predictive check result.
pp_check <- function(model, file = NULL, ...) {

  p <- if (model$metadata()$is_timevar) {
    .plot_ppc_timevar_subset(
      yrep = model$ppc(),
      raw = model$mrp_data()$input,
      dates = model$plot_data()$dates,
      metadata = model$metadata()
    )
  } else {
    .plot_ppc_static(
      yrep = model$ppc(),
      raw = model$mrp_data()$input,
      metadata = model$metadata()
    )
  }

  if (!is.null(file)) {
    # Set default parameters for ggsave
    dots <- utils::modifyList(.const()$plot$save, list(...))
    do.call(ggplot2::ggsave, c(list(filename = file, plot = p), dots))
  }      

  return(p)
}
MRPWorkflow$set("public", "pp_check", pp_check)

#' Compare models using LOO-CV
#'
#' @name MRPWorkflow-method-compare_models
#' @aliases compare_models
#'
#'
#' @description The `$compare_models()` method compares multiple fitted `MRPModel` objects
#' using leave-one-out cross-validation to assess relative model performance. Check out the
#' [More examples of R6 classes](https://mrp-interface.github.io/shinymrp/articles/example#compare_models)
#' vignette for usage examples.
#'
#' @param ... Multiple MRPModel objects to compare.
#' @param suppress Character string specifying output to suppress during comparison.
#'
#' @return A data.frame object containing the comparison results.
compare_models <- function(..., suppress = NULL) {

  if (length(list(...)) < 2) {
    stop("At least two models are required for comparison.")
  }

  message("Running leave-one-out cross-validation...")
  
  models <- list(...)
  lapply(models, private$assert_model)

  # Extract log-likelihood from each model
  loo_list <- purrr::map(models, function(m) {
    utils::capture.output({
      loo_output <- loo::loo(
        m$log_lik(),
        cores = m$metadata()$n_chains
      )
    }, type = suppress)

    return(loo_output)
  })

  # Compare the models using loo_compare
  compare_df <- loo_list %>%
    loo::loo_compare() %>%
    as.data.frame() %>%
    select("elpd_diff", "se_diff")


  return(compare_df)
}
MRPWorkflow$set("public", "compare_models", compare_models)