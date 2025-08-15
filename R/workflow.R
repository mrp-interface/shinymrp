#' Create a new MRPWorkflow object
#' 
#' @description Create a new [`MRPWorkflow`][MRPWorkflow] object that implements
#' the Bayesian data analysis workflow that underpins the application of
#' Multilevel Regression and Post-stratification (MRP).
#'
#' @return A `MRPWorkflow` object.
#'
#' @export
mrp_workflow <- function() {
  if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
    stop("CmdStan is not installed. Please install CmdStan using `shinymrp::install_cmdstan()`.")
  }

  MRPWorkflow$new()
}

#' MRPWorkflow objects
#'
#' @description A `MRPWorkflow` object is an [R6][R6::R6Class] object created by
#' the [`mrp_workflow()`][mrp_workflow] function. This class provides methods for all steps
#' in the workflow, from data preparation and visualization to model fittingg.
#'
#'
#' @section Methods: `MRPWorkflow` objects have the following associated
#'   methods, many of which have their own (linked) documentation pages:
#' 
#'   ## Data preparation
#' 
#'   |**Method**|**Description**|
#'   |:----------|:---------------|
#'   [`$preprocess()`][MRPWorkflow-method-preprocess] | Preprocess sample data. |
#'   [`$link_acs()`][MRPWorkflow-method-link_acs] | Link sample data to ACS data. |
#'   [`$load_pstrat()`][MRPWorkflow-method-load_pstrat] | Load custom post-stratification data. |
#'
#'   ## Model fitting & diagnostics
#'   |**Method**|**Description**|
#'   |:----------|:---------------|
#'   [`$check_effects()`][MRPWorkflow-method-check_effects] | Check model specification. |
#'   [`$create_model()`][MRPWorkflow-method-create_model] | Create a [`MRPModel`][MRPModel] object. |
#'   [`$load_model()`][MRPWorkflow-method-load_model] | Load a previously saved model. |
#'   [`$pp_check()`][MRPWorkflow-method-pp_check] | Perform posterior predictive check. |
#'   [`$compare_models()`][MRPWorkflow-method-compare_models] | Compare models using LOO-CV. |
#' 
#'   ## Visualization
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$demo_bars()`][MRPWorkflow-method-demo_bars] | Create demographic comparison bar plots. |
#'  [`$covar_hist()`][MRPWorkflow-method-covar_hist] | Create geographic covariate distribution histogram. |
#'  [`$sample_size_map()`][MRPWorkflow-method-sample_size_map] | Create sample size map. |
#'  [`$outcome_plot()`][MRPWorkflow-method-outcome_plot] | Create summary plots of the outcome measure. |
#'  [`$outcome_map()`][MRPWorkflow-method-outcome_map] | Visualize raw outcome measure by geography. |
#'  [`$estimate_plot()`][MRPWorkflow-method-estimate_plot] | Visualize estimates for demographic groups. |
#'  [`$estimate_map()`][MRPWorkflow-method-estimate_map] | Visualize estimates for geographic units. |
#' 
#'  ## Other
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$metadata()`][MRPWorkflow-method-metadata] | Return metadata information. |
#'  [`$preprocessed_data()`][MRPWorkflow-method-preprocessed_data] | Return preprocessed sample data. |
#'
#' 
#' @examples
#'   \dontrun{
#'   library(shinymrp)
#'   } 
#' 
#' @export
MRPWorkflow <- R6::R6Class(
  "MRPWorkflow",
  private = list(
    data_ = NULL,
    metadata_ = NULL,
    linkdata_ = NULL,
    plotdata_ = NULL,
    mrp_ = NULL,

    assert_metadata_exists = function() {
      if (is.null(private$metadata_)) {
        stop("Metadata is not available. Please run 'preprocess' first.")
      }
    },

    assert_data_exists = function() {
      if (is.null(private$data_)) {
        stop("Sample data is not available. Please run 'preprocess' first.")
      }
    },

    assert_mrp_exists = function() {
      if (is.null(private$mrp_)) {
        stop("Data for MRP is not available. Please run 'link_acs' or 'load_pstrat' first.")
      }
    },

    assert_model_spec = function(model_spec) {
      private$assert_mrp_exists()

      # check if model_spec$Intercept is a list with a single element
      if (!is.list(model_spec$Intercept) || names(model_spec$Intercept) != "Intercept") {
        stop("model_spec$Intercept must be a list with a single element named 'Intercept'.")
      }


      # check prior syntax for all effects
      effects_w_priors <- unlist(model_spec)
      bools <- effects_w_priors %>%
        purrr::map_lgl(function(s) .clean_prior_syntax(s) %>% .check_prior_syntax())

      if (!all(bools)) {
        stop(paste0("The following priors have invalid syntax: ",
            paste(effects_w_priors[!bools], collapse = ", ")))
      }

      # check if model_spec have corresponding variables in data
      main_vars <- c(names(model_spec$fixed), names(model_spec$varying))
      vars_in_data <- names(private$mrp_$input)
      invalid_vars <- setdiff(main_vars, vars_in_data)

      if (length(invalid_vars) > 0) {
        stop(paste0("The following variables are not present in the data: ",
            paste(invalid_vars, collapse = ", ")))
      }

      # check if any variables are single-level factors
      omit_vars <- private$mrp_$vars$omit
      if (length(intersect(main_vars, omit_vars$one_level)) > 0) {
        stop(paste0("The following variables have a single level and cannot be used: ",
            paste(omit_vars$one_level, collapse = ", ")))
      }

      # check if any interactions with nested main effects
      if (length(.pair_intersect(names(model_spec$interaction), omit_vars$nested)) > 0) {
        stop(paste0("The following interactions have nested main effects and cannot be used: ",
            paste(omit_vars$nested, collapse = ", ")))
      }

      # check if interactions can be assigned structured prior
      ints_w_struct <- names(model_spec$interaction[model_spec$interaction == "structured"])
      valid_ints_w_struct <- .filter_interactions(
        interactions = ints_w_struct,
        fixed_effects = names(model_spec$fixed),
        data = private$mrp_$input
      )
      invalid_ints_w_struct <- setdiff(ints_w_struct, valid_ints_w_struct)
      if (length(invalid_ints_w_struct) > 0) {
        stop(paste0("The following interactions cannot be assigned structured prior: ",
            paste(invalid_ints_w_struct, collapse = ", ")))
      }

      return(TRUE)
    },

    assert_model = function(model) {
      checkmate::assert_class(
        model,
        classes = "MRPModel",
        null.ok = FALSE
      )

      return(TRUE)
    }
  ),
  public = list(
    #' @description Initializes the MRPWorkflow object, setting up necessary fields for data processing and model fitting.
    #' 
    #' @return A new MRPWorkflow object.
    initialize = function() {},

    check_metadata_exists = function() {
      return(!is.null(private$metadata_))
    },

    check_data_exists = function() {
      return(!is.null(private$data_))
    },

    check_mrp_exists = function() {
      return(!is.null(private$mrp_))
    },

    mrp_data = function() {
      return(private$mrp_)
    }
  )
)

#' Return workflow metadata
#' 
#' @name MRPWorkflow-method-metadata
#' @aliases metadata
#' @family MRPWorkflow methods
#' 
#' @description Retrieves the metadata associated with the current workflow, including information about time variables, family, and special cases.
#'
metadata <- function() {
  return(private$metadata_)
}
MRPWorkflow$set("public", "metadata", metadata)


#' Return preprocessed sample data
#' 
#' @name MRPWorkflow-method-preprocessed_data
#' @aliases preprocessed_data
#' @family MRPWorkflow methods
#' 
#' @description Retrieves the preprocessed sample data that has been prepared for MRP analysis.
#'
preprocessed_data <- function() {
  return(private$data_)
}
MRPWorkflow$set("public", "preprocessed_data", preprocessed_data)


#' Preprocess sample data
#'
#' @name MRPWorkflow-method-preprocess
#' @aliases preprocess
#' @family MRPWorkflow methods
#'
#' @description Preprocesses the input sample data by cleaning, validating, and preparing it for MRP analysis. This includes handling time-varying data, aggregated data, and special cases.
#'
#' @param data Input sample data to be preprocessed
#' @param is_timevar Logical indicating whether the data contains time-varying components
#' @param is_aggregated Logical indicating whether the data is already aggregated
#' @param special_case Character string specifying special case handling (e.g., "covid", "poll")
#' @param family Character string specifying the model family (e.g., "binomial", "normal")
#' @param zip_threshold Numeric value specifying the minimum number of records required for a ZIP code to be included in the analysis (default is 0)
#' @param state_threshold Numeric value specifying the minimum number of records required for a state to be included in the analysis (default is 0)
#'
preprocess <- function(
  data,
  is_timevar = FALSE,
  is_aggregated = FALSE,
  special_case = NULL,
  family = NULL,
  freq = NULL
  zip_threshold = 0,
  state_threshold = 0
) {

  checkmate::assert_choice(
    family,
    choices = GLOBAL$args$family,
    null.ok = TRUE
  )

  if (!is_timevar && !is.null(freq)) {
    stop("Time indexing frequency cannot be specified for static data.")
  }

  if (family == "normal" && is_aggregated) {
    stop("is_aggregated cannot be TRUE for data with continuous outcome (normal family).")
  }

  private$metadata_ <- list(
    is_timevar = is_timevar,
    special_case = special_case,
    family = family
  )

  message("Preprocessing sample data...")

  tryCatch({
    zip_county_state <- .fetch_data("zip_county_state.csv", subdir = "geo")

    private$data_ <- .preprocess(
      data = data,
      metadata = private$metadata_,
      zip_county_state = zip_county_state,
      freq = freq,
      is_sample = TRUE,
      is_aggregated = is_aggregated,
      zip_threshold = zip_threshold,
      state_threshold = state_threshold
    )
  
  }, error = function(e) {
    # show error message
    error_message <- paste("Error processing data:\n", e$message)
    message(error_message)
  })

}
MRPWorkflow$set("public", "preprocess", preprocess)

#' Link sample data to ACS data
#'
#' @name MRPWorkflow-method-link_acs
#' @aliases link_acs
#' @family MRPWorkflow methods
#'
#' @description Links the preprocessed sample data to ACS poststratification data based on geographic and demographic variables.
#'
#' @param link_geo Character string specifying the geographic level for linking (e.g., "state", "county", "zip")
#' @param acs_year Numeric value specifying the ACS year to use for poststratification data
#'
link_acs <- function(
  link_geo = NULL,
  acs_year = 2023
) {

  checkmate::assert_choice(
    link_geo,
    choices = GLOBAL$vars$geo,
    null.ok = TRUE
  )

  checkmate::assert_choice(
    acs_year,
    choices = GLOBAL$args$acs_years,
    null.ok = TRUE
  )

  private$assert_data_exists()

  message("Linking data to the ACS...")

  # store user's selections for data linking
  private$linkdata_ <- list(
    link_geo = if(link_geo %in% GLOBAL$vars$geo) link_geo else NULL,
    acs_year = acs_year
  )

  tryCatch({
    if (!is.null(private$metadata_$special_case) &&
        private$metadata_$special_case == "covid") {

      if (is.null(private$linkdata_$link_geo) ||
          private$linkdata_$link_geo != "zip") {
        private$linkdata_$link_geo <- "zip"
        warning(stringr::str_interp("Linking geography is either incorrect or not specified. Using 'zip' as default for COVID data."))
      }

      pstrat_covid <- .fetch_data("pstrat_covid.csv", subdir = "acs")
      covar_covid <- .fetch_data("covar_covid.csv", subdir = "acs")

      # prepare data for MRP
      private$mrp_ <- .prepare_mrp_covid(
        input_data = private$data_,
        pstrat_data = pstrat_covid,
        covariates = covar_covid,
        metadata   = private$metadata_
      )

      # prepare data for plotting
      private$plotdata_ <- list(
        dates = if("date" %in% names(private$data_)) .get_dates(private$data_) else NULL,
        geojson = list(county = .filter_geojson(
          geojson_$county,
          private$mrp_$levels$county
        )),
        raw_covariates = covar_covid %>%
          filter(.data$zip %in% unique(private$mrp_$input$zip))
      )

    } else if (!is.null(private$metadata_$special_case) &&
                private$metadata_$special_case == "poll") {

      if (is.null(private$linkdata_$link_geo) ||
          private$linkdata_$link_geo != "state") {
        private$linkdata_$link_geo <- "state"
        warning(stringr::str_interp("Linking geography is either incorrect or not specified. Using 'state' as default for polling data."))
      }

      new_data <- .fetch_data("pstrat_poll.csv", subdir = "acs") %>%
        mutate(state = .to_fips(.data$state, "state"))

      private$mrp_ <- .prepare_mrp_custom(
        input_data = private$data_,
        new_data = new_data,
        metadata = private$metadata_,
        link_geo = "state"
      )

      # prepare data for plotting
      private$plotdata_ <- list(
        geojson = list(state = .filter_geojson(
          geojson_$state,
          private$mrp_$levels$state
        ))
      )

    } else {
      if (is.null(private$linkdata_$acs_year)) {
        private$linkdata_$acs_year <- GLOBAL$args$acs_years[length(GLOBAL$args$acs_years)]
        warning(stringr::str_interp("ACS year not specified. Using the latest year: ${private$linkdata_$acs_year}."))
      }

      # retrieve ACS data based on user's input
      acs_year <- private$linkdata_$acs_year
      tract_data <- .fetch_data(
        paste0("acs_", acs_year - 4, "-", acs_year, ".csv"),
        subdir = "acs"
      )
      zip_tract <- .fetch_data("zip_tract.csv", subdir = "geo")

      # prepare data for MRP
      private$mrp_ <- .prepare_mrp_acs(
        input_data = private$data_,
        tract_data = tract_data,
        zip_tract = zip_tract,
        metadata = private$metadata_,
        link_geo = private$linkdata_$link_geo
      )

      # prepare data for plotting
      plotdata <- list()
      plotdata$dates <- if("date" %in% names(private$data_)) .get_dates(private$data_) else NULL
      plotdata$geojson <- names(geojson_) %>%
        stats::setNames(nm = .) %>%
        purrr::map(~.filter_geojson(
          geojson = geojson_[[.x]], 
          geoids = private$mrp_$levels[[.x]]
        ))

      private$plotdata_ <- .nullify(plotdata)
    }
  }, error = function(e) {
    message(paste("Error linking data:\n", e$message))
  })
}
MRPWorkflow$set("public", "link_acs", link_acs)

#' Load custom post-stratification data
#'
#' @name MRPWorkflow-method-load_pstrat
#' @aliases load_pstrat
#' @family MRPWorkflow methods
#'
#' @description Loads and processes custom poststratification data instead of using ACS data. This method validates the data and prepares it for MRP analysis.
#'
#' @param pstrat_data Custom poststratification data to be loaded
#' @param is_aggregated Logical indicating whether the poststratification data is already aggregated
#'
load_pstrat <- function(pstrat_data, is_aggregated = FALSE) {
  if (!is.null(private$metadata_$special_case)) {
    stop("Custom poststratification data is not supported for special cases like COVID or polling data. Please use the `link_acs` method instead.")
  }

  if (is.null(private$data_)) {
    stop("Sample data is not available. Please provide sample data through the `preprocess` method first.")
  }

  if (is.null(pstrat_data)) {
    stop("Post-stratification data is required. Please provide valid pstrat_data.")
  }

  message("Preprocessing poststratification data...")

  tryCatch({
    zip_county_state <- .fetch_data("zip_county_state.csv", subdir = "geo")

    # Process data
    new_data <- .preprocess(
      data = pstrat_data,
      metadata = private$metadata_,
      zip_county_state = zip_county_state,
      is_sample = FALSE,
      is_aggregated = is_aggregated
    )

    # Compare to sample data
    .check_pstrat(new_data, private$data_, .create_expected_levels(private$metadata_))

    # Find the smallest common geography
    link_geo <- NULL
    common <- intersect(names(private$data_), names(new_data))
    smallest <- .get_smallest_geo(common)
    if (!is.null(smallest)) {
      link_geo <- smallest$geo
    }

    # Store linking geography
    private$linkdata_ <- list(
      link_geo = link_geo,
      acs_year = NULL
    )

    # Prepare data for MRP
    private$mrp_ <- .prepare_mrp_custom(
      input_data = private$data_,
      new_data = new_data,
      metadata = private$metadata_,
      link_geo = link_geo
    )


    # prepare data for plotting
    plotdata <- list()
    plotdata$dates <- if("date" %in% names(private$data_)) .get_dates(private$data_) else NULL
    plotdata$geojson <- names(geojson_) %>%
      stats::setNames(nm = .) %>%
      purrr::map(~.filter_geojson(
        geojson = geojson_[[.x]], 
        geoids = private$mrp_$levels[[.x]]
      ))

    private$plotdata_ <- .nullify(plotdata)

  }, error = function(e) {
    # show error message
    error_message <- paste("Error processing data:\n", e$message)
    message(error_message)
    
    # reset fields
    private$linkdata_ <- NULL
    private$mrp_ <- NULL
    private$plotdata_ <- NULL
    
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
#' @family MRPWorkflow methods
#'
#' @description Creates bar plots comparing demographic distributions between
#' input survey data and target population data.
#'
#' @param demo Character string specifying the demographic variable to plot
#' @param file Optional file path to save the plot
#' @param ... Additional arguments passed to ggsave
#'
#' @return A ggplot object or patchwork object showing demographic comparisons
demo_bars <- function(demo, file = NULL, ...) {
  private$assert_mrp_exists()

  checkmate::assert_choice(
    demo,
    choices = intersect(GLOBAL$vars$demo, names(private$mrp_$levels)), 
    null.ok = FALSE
  )

  input_data <- private$mrp_$input %>%
    .as_factor(private$mrp_$levels[demo]) %>%
    mutate(demo = !!sym(demo)) %>%
    select(.data$demo, .data$total)

  new_data <- private$mrp_$new
  if ("time" %in% names(new_data)) {
    new_data <- new_data %>% filter(.data$time == 1)
  }
  new_data <- new_data %>%
    .as_factor(private$mrp_$levels[demo]) %>%
    mutate(demo = !!sym(demo)) %>%
    select(.data$demo, .data$total)

  p <- .plot_demographic(input_data, new_data)

  if (!is.null(file)) {
    # Set default parameters for ggsave
    dots <- modifyList(GLOBAL$plot$save, list(...))
    do.call(ggplot2::ggsave, c(list(filename = file, plot = p), dots))
  }

  return(p)
}
MRPWorkflow$set("public", "demo_bars", demo_bars)

#' Create geographic covariate distribution histogram
#'
#' @name MRPWorkflow-method-covar_hist
#' @aliases covar_hist
#' @family MRPWorkflow methods
#'
#' @description Creates histogram plots showing the distribution of geographic
#' covariates across zip codes. Only available for COVID data.
#'
#' @param covar Character string specifying the covariate. Options: "edu",
#'   "poverty", "employ", "income", "urban", "adi"
#' @param file Optional file path to save the plot
#' @param ... Additional arguments passed to ggsave
#'
#' @return A ggplot object showing the covariate distribution histogram
covar_hist <- function(covar, file = NULL, ...) {
  private$assert_mrp_exists()
  
  raw_covariates <- private$plotdata_$raw_covariates
  if (is.null(raw_covariates)) {
    stop("Covariate data is not available. This method is only available for COVID data.")
  }
  
  checkmate::assert_choice(
    covar,
    choices = GLOBAL$vars$covar,
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
    dots <- modifyList(GLOBAL$plot$save, list(...))
    do.call(ggplot2::ggsave, c(list(filename = file, plot = p), dots))
  }

  return(p)
}
MRPWorkflow$set("public", "covar_hist", covar_hist)

#' Create sample size map
#'
#' @name MRPWorkflow-method-sample_size_map
#' @aliases sample_size_map
#' @family MRPWorkflow methods
#'
#' @description Creates interactive choropleth maps showing data distribution
#' with respect to geography.
#'
#' @param file Optional file path to save the plot
#'
#' @return A highcharter map object showing sample size distribution
sample_size_map <- function(file = NULL) {
  private$assert_mrp_exists()

  geo <- private$linkdata_$link_geo
  if (geo == "zip") {
    geo <- "county"
  } else if (is.null(geo)) {
    stop("Linking geography is not available.")
  }
  
  hc <- private$mrp_$input %>%
    .prep_sample_size(
      fips_codes = fips_[[geo]],
      geo = geo,
      for_map = TRUE
    ) %>%
    .choro_map(
      private$plotdata_$geojson[[geo]],
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
#' @family MRPWorkflow methods
#'
#' @description Creates plots showing the distribution of outcome measures over time (for time-varying data) or as static distributions (for cross-sectional data).
#'
#' @param file Optional file path to save the plot
#' @param ... Additional arguments passed to ggsave
#'
#' @return A ggplot object showing the outcome measure distribution
outcome_plot <- function(file = NULL, ...) {
  private$assert_mrp_exists()

  p <- if (private$metadata_$is_timevar) {
    .plot_outcome_timevar(
      raw = private$mrp_$input,
      yrep_est = NULL,
      dates = private$plotdata_$dates,
      metadata = private$metadata_,

      show_caption = FALSE
    )
  } else {
    .plot_outcome_static(
      raw = private$mrp_$input,
      yrep_est = NULL,
      metadata = private$metadata_
    )
  }

  if (!is.null(file)) {
    # Set default parameters for ggsave
    dots <- modifyList(GLOBAL$plot$save, list(...))
    do.call(ggplot2::ggsave, c(list(filename = file, plot = p), dots))
  }

  return(p)
}
MRPWorkflow$set("public", "outcome_plot", outcome_plot)

#' Visualize raw outcome measure by geography
#'
#' @name MRPWorkflow-method-outcome_map
#' @aliases outcome_map
#' @family MRPWorkflow methods
#'
#' @description Creates maps showing average outcome measure by geography for
#' cross-sectional data, or highest/lowest temporal average for time-varying data.
#'
#' @param summary_type Character string for time-varying data: "max" or "min"
#' @param file Optional file path to save the map
#'
#' @return A highcharter map object showing outcome measures by geography
outcome_map <- function(summary_type = NULL, file = NULL) {
  private$assert_mrp_exists()

  checkmate::assert_choice(
    summary_type,
    choices = GLOBAL$args$summary_types,
    null.ok = TRUE
  )

  geo <- private$linkdata_$link_geo
  if (geo == "zip") {
    geo <- "county"
  } else if (is.null(geo)) {
    stop("Linking geography is not available.")
  }

  out <- .prep_raw(
    private$mrp_$input,
    fips_[[geo]],
    geo = geo,
    summary_type = summary_type,
    metadata = private$metadata_
  )

  config <- list()
  if (n_distinct(out$plot_df$value) == 1 &&
      out$plot_df$value[1] == 0) {
    config <- list(minValue = 0, maxValue = 1)
  }
  config <- c(config, out$title)

  hc <- .choro_map(
    out$plot_df,
    private$plotdata_$geojson[[geo]],
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
#' @family MRPWorkflow methods
#'
#' @description Creates plots showing MRP estimates for different subgroups, either over time (for time-varying data) or as static estimates (for cross-sectional data).
#'
#' @param model Fitted MRPModel object
#' @param group Character string specifying the demographic group for plotting
#' @param interval Confidence interval or standard deviation for the estimates (default is 0.95)
#' @param file Optional file path to save the plot
#' @param show_caption Logical indicating whether to show the caption in the plot (default is TRUE)
#' @param ... Additional arguments passed to ggsave
#'
#' @return A ggplot object showing the group estimates
estimate_plot <- function(model, group = NULL, interval = 0.95, show_caption = TRUE, file = NULL, ...) {

  checkmate::assert_choice(
    group,
    choices = intersect(GLOBAL$vars$pstrat, names(model$mrp()$levels)),
    null.ok = TRUE
  )

  est_list <- model$poststratify(interval = interval)

  if (is.null(group)) {
    est_df <- est_list$overall

    p <- if(model$metadata()$is_timevar) {
      .plot_outcome_timevar(
        raw = model$mrp()$input,
        yrep_est = est_df,
        dates = model$plotdata()$dates,
        metadata = model$metadata(),
        show_caption = show_caption
      )
    } else {
      .plot_outcome_static(
        raw = model$mrp()$input,
        yrep_est = est_df,
        metadata = model$metadata(),
        show_caption = show_caption
      )
    }
  } else {
    # Convert levels to "factor" type
    est_df <- est_list[[group]] %>%
      rename(!!group := factor) %>%
      .as_factor(model$mrp()$levels[group]) %>%
      rename(factor := !!sym(group))

    p <- if (model$metadata()$is_timevar) {
      .plot_est_timevar(
        plot_df = est_df,
        dates = model$plotdata()$dates,
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
      list(height = 4 * (length(model$mrp()$levels[[group]]) + 1))
    } else{
      list()
    }
    dots <- modifyList(GLOBAL$plot$save, settings) %>%
      modifyList(list(...))
    do.call(ggplot2::ggsave, c(list(filename = file, plot = p), dots))
  }      

  return(p)
}
MRPWorkflow$set("public", "estimate_plot", estimate_plot)

#' Create a choropleth map of MRP estimates
#' 
#' @name MRPWorkflow-method-estimate_map
#' @aliases estimate_map
#' @family MRPWorkflow methods
#' 
#' @description Creates interactive choropleth maps showing MRP estimates by geographic regions.
#'
#' @param model Fitted MRPModel object
#' @param geo Character string specifying the geographic level for mapping
#' @param time_index Numeric value specifying the time index for time-varying data
#' @param interval Confidence interval or standard deviation for the estimates (default is 0.95)
#' @param file Optional file path to save the map
#' @param ... Additional arguments
#' 
#' @return A highcharter map object showing MRP estimates by geography
estimate_map <- function(
  model,
  geo = NULL,
  time_index = NULL,
  interval = 0.95,
  file = NULL,
  ...
) {

  if (is.null(model$linkdata()$link_geo)) {
    stop("Linking geography is not available.")
  }

  choices <- intersect(GLOBAL$vars$geo2, names(model$mrp()$levels))
  checkmate::assert_choice(
    geo,
    choices = choices,
    null.ok = TRUE
  )
  geo <- .replace_null(geo, choices[1])

  time_index <- if (model$metadata()$is_timevar) {
    choices <- model$mrp()$levels$time
    checkmate::assert_choice(
      time_index,
      choices = choices,
      null.ok = TRUE
    )
    .replace_null(time_index, choices[1])
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
      model$plotdata()$geojson[[geo]],
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

#' Create a MRPModel object
#'
#' @name MRPWorkflow-method-create_model
#' @aliases create_model
#' @family MRPWorkflow methods
#'
#' @description Creates a new MRPModel object with validated effects specification and prepared data for Bayesian model fitting.
#'
#' @param model_spec List containing model effects specification including intercept, fixed effects, varying effects, and interactions
#'
#' @return A new MRPModel object
create_model <- function(model_spec) {
  private$assert_mrp_exists()
  private$assert_model_spec(model_spec)

  effects <- .set_default_priors(model_spec)

  MRPModel$new(
    effects   = effects,
    mrp       = private$mrp_,
    metadata  = private$metadata_,
    linkdata = private$linkdata_,
    plotdata = private$plotdata_
  )
}
MRPWorkflow$set("public", "create_model", create_model)

#' Perform posterior predictive check
#'
#' @name MRPWorkflow-method-pp_check
#' @aliases pp_check
#' @family MRPWorkflow methods
#'
#' @description Creates posterior predictive check plots to assess model fit by comparing observed data to replicated data from the posterior predictive distribution.
#'
#' @param model Fitted MRPModel object
#' @param file Optional file path to save the plot
#' @param ... Additional arguments passed to ggsave
pp_check <- function(model, file = NULL, ...) {

  p <- if (model$metadata()$is_timevar) {
    .plot_ppc_timevar_subset(
      yrep = model$ppc(),
      raw = model$mrp()$input,
      dates = model$plotdata()$dates,
      metadata = model$metadata()
    )
  } else {
    .plot_ppc_static(
      yrep = model$ppc(),
      raw = model$mrp()$input,
      metadata = model$metadata()
    )
  }

  if (!is.null(file)) {
    # Set default parameters for ggsave
    dots <- modifyList(GLOBAL$plot$save, list(...))
    do.call(ggplot2::ggsave, c(list(filename = file, plot = p), dots))
  }      

  return(p)
}
MRPWorkflow$set("public", "pp_check", pp_check)

#' Compare models using LOO-CV
#'
#' @name MRPWorkflow-method-compare_models
#' @aliases compare_models
#' @family MRPWorkflow methods
#'
#' @description Compares multiple fitted MRP models using leave-one-out cross-validation to assess relative model performance.
#'
#' @param ... Multiple MRPModel objects to compare
#' @param suppress Character string specifying output to suppress during comparison
#'
#' @return A data frame summarizing the comparison results
compare_models <- function(..., suppress = NULL) {

  if (length(list(...)) < 2) {
    stop("At least two models are required for comparison.")
  }

  models <- list(...)
  lapply(models, private$assert_model)

  # Extract log-likelihood from each model
  loo_list <- purrr::map(models, function(m) {
    utils::capture.output({
      loo_output <- loo::loo(
        m$loo(),
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