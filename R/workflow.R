#' Create a new MRPWorkflow object
#' 
#' @description Create a new [`MRPWorkflow`][MRPWorkflow] object that implements
#' the Bayesian data analysis workflow that underpins the application of
#' Multilevel Regression and Post-stratification (MRP).
#'
#' @return A ['MRPWorkflow'] object.
#' 
#' @export
mrp_workflow <- function() {
  if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
      stop("CmdStan is not installed or not available.")
  }

  MRPWorkflow$new()
}

#' MRPWorkflow objects
#'
#' @description A `MRPWorkflow` object is an [R6][R6::R6Class] object created by
#' the [`mrp_workflow()`][mrp_workflow] function. This class provides methods for all steps
#' in the workflow, from data preparation and visualization to model fitting.
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
#'   [`$check_model()`][MRPWorkflow-method-check_model] | Perform posterior predictive check. |
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
#'  [`$data()`][MRPWorkflow-method-data] | Return preprocessed sample data. |
#'
#' @examples
#'   \dontrun{
#'   library(shinymrp)
#'   }
#' 
#' @format
#'   An [R6][R6::R6Class] generator object.
#' 
#' @docType class
#' 
#' @export
#' 
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
        stop("Sample data is not available. Please run data preparation steps first.")
      }
    },

    assert_mrp_exists = function() {
      if (is.null(private$mrp_)) {
        stop("Data for MRP is not available. Please run data preparation steps first.")
      }
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
    
    #' @description Retrieves the metadata associated with the current workflow, including information about time variables, family, and special cases.
    #'
    metadata = function() {
      private$assert_metadata_exists()
      return(private$metadata_)
    },

    #' @description Retrieves the preprocessed sample data that has been prepared for MRP analysis.
    #'
    data = function() {
      private$assert_data_exists()
      return(private$data_)
    },
    
    #' @description Preprocesses the input sample data by cleaning, validating, and preparing it for MRP analysis. This includes handling time-varying data, aggregated data, and special cases.
    #'
    #' @param data Input sample data to be preprocessed
    #' @param is_timevar Logical indicating whether the data contains time-varying components
    #' @param is_aggregated Logical indicating whether the data is already aggregated
    #' @param special_case Character string specifying special case handling (e.g., "covid", "poll")
    #' @param family Character string specifying the model family (e.g., "binomial", "normal")
    #' 
    preprocess = function(
      data,
      is_timevar = FALSE,
      is_aggregated = FALSE,
      special_case = NULL,
      family = NULL
    ) {

      checkmate::assert_choice(
        family,
        choices = GLOBAL$args$family,
        null.ok = TRUE
      )

      private$metadata_ <- list(
        is_timevar = is_timevar,
        special_case = special_case,
        family = family
      )

      message("Preprocessing sample data...")

      tryCatch({
        private$data_ <- preprocess(
          data = data,
          metadata = private$metadata_,
          is_sample = TRUE,
          is_aggregated = is_aggregated
        )
      
      }, error = function(e) {
        # show error message
        error_message <- paste("Error processing data:\n", e$message)
        message(error_message)
      })
    
    },

    #' @description Links the preprocessed sample data to ACS post-stratification data based on geographic and demographic variables.
    #'
    #' @param link_geo Character string specifying the geographic level for linking (e.g., "state", "county", "zip")
    #' @param acs_year Numeric value specifying the ACS year to use for post-stratification data
    #'
    link_acs = function(
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

          # prepare data for MRP
          private$mrp_ <- prepare_mrp_covid(
            input_data = private$data_,
            pstrat_data = acs_covid_$pstrat,
            covariates = acs_covid_$covar,
            metadata   = private$metadata_
          )

          # prepare data for plotting
          private$plotdata_ <- list(
            dates = if("date" %in% names(private$data_)) get_dates(private$data_) else NULL,
            geojson = list(county = filter_geojson(
              geojson_$county,
              private$mrp_$levels$county
            )),
            raw_covariates = acs_covid_$covar %>%  
              filter(.data$zip %in% unique(private$mrp_$input$zip))
          )

        } else if (!is.null(private$metadata_$special_case) &&
                    private$metadata_$special_case == "poll") {

          if (is.null(private$linkdata_$link_geo) ||
              private$linkdata_$link_geo != "state") {
            private$linkdata_$link_geo <- "state"
            warning(stringr::str_interp("Linking geography is either incorrect or not specified. Using 'state' as default for polling data."))
          }

          new_data <- acs_poll_$pstrat %>%
            mutate(state = to_fips(.data$state, "state"))

          private$mrp_ <- prepare_mrp_custom(
            input_data = private$data_,
            new_data = new_data,
            metadata = private$metadata_,
            link_geo = "state"
          )

          # prepare data for plotting
          private$plotdata_ <- list(
            geojson = list(state = filter_geojson(
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
          tract_data <- acs_[[as.character(private$linkdata_$acs_year)]]

          # prepare data for MRP
          private$mrp_ <- prepare_mrp_acs(
            input_data = private$data_,
            tract_data = tract_data,
            metadata = private$metadata_,
            link_geo = private$linkdata_$link_geo
          )

          # prepare data for plotting
          plotdata <- list()
          plotdata$dates <- if("date" %in% names(private$data_)) get_dates(private$data_) else NULL
          plotdata$geojson <- names(geojson_) %>%
            stats::setNames(nm = .) %>%
            purrr::map(~filter_geojson(
              geojson = geojson_[[.x]], 
              geoids = private$mrp_$levels[[.x]]
            ))

          private$plotdata_ <- nullify(plotdata)
        }


      }, error = function(e) {
        message(paste("Error linking data:\n", e$message))
      })
    },
    
    #' @description Loads and processes custom post-stratification data instead of using ACS data. This method validates the data and prepares it for MRP analysis.
    #'
    #' @param pstrat_data Custom post-stratification data to be loaded
    #' @param is_aggregated Logical indicating whether the post-stratification data is already aggregated
    #'
    load_pstrat = function(pstrat_data, is_aggregated = FALSE) {
      if (!is.null(private$metadata_$special_case)) {
        stop("Custom post-stratification data is not supported for special cases like COVID or polling data. Please use the `link_acs` method instead.")
      }

      if (is.null(private$data_)) {
        stop("Sample data is not available. Please provide sample data through the `preprocess` method first.")
      }

      if (is.null(pstrat_data)) {
        stop("Post-stratification data is required. Please provide valid pstrat_data.")
      }

      message("Preprocessing post-stratification data...")

      tryCatch({

        # Process data
        new_data <- preprocess(
          data = pstrat_data,
          metadata = private$metadata_,
          is_sample = FALSE,
          is_aggregated = is_aggregated
        )

        # Compare to sample data
        check_pstrat(new_data, private$data_, create_expected_levels(private$metadata_))

        # Find the smallest common geography
        link_geo <- NULL
        common <- intersect(names(private$data_), names(new_data))
        smallest <- get_smallest_geo(common)
        if (!is.null(smallest)) {
          link_geo <- smallest$geo
        }

        # Store linking geography
        private$linkdata_ <- list(
          link_geo = link_geo,
          acs_year = NULL
        )

        # Prepare data for MRP
        private$mrp_ <- prepare_mrp_custom(
          input_data = private$data_,
          new_data = new_data,
          metadata = private$metadata_,
          link_geo = link_geo
        )


        # prepare data for plotting
        plotdata <- list()
        plotdata$dates <- if("date" %in% names(private$data_)) get_dates(private$data_) else NULL
        plotdata$geojson <- names(geojson_) %>%
          stats::setNames(nm = .) %>%
          purrr::map(~filter_geojson(
            geojson = geojson_[[.x]], 
            geoids = private$mrp_$levels[[.x]]
          ))

        private$plotdata_ <- nullify(plotdata)

      }, error = function(e) {
        # show error message
        error_message <- paste("Error processing data:\n", e$message)
        message(error_message)
        
        # reset fields
        private$linkdata_ <- NULL
        private$mrp_ <- NULL
        private$plotdata_ <- NULL
        
      })
    },

    # --------------------------------------------------------------------------
    # Visualization methods
    # --------------------------------------------------------------------------

    #' @description Creates bar plots comparing demographic distributions between
    #' input survey data and target population data.
    #'
    #' @param demo Character string specifying the demographic variable to plot
    #' @param file Optional file path to save the plot
    #' @param ... Additional arguments passed to ggsave
    #'
    #' @return A ggplot object or patchwork object showing demographic comparisons
    demo_bars = function(demo, file = NULL, ...) {
      private$assert_mrp_exists()

      checkmate::assert_choice(
        demo,
        choices = intersect(GLOBAL$vars$demo, names(private$mrp_$levels)), 
        null.ok = FALSE
      )

      input_data <- private$mrp_$input %>%
        as_factor(private$mrp_$levels[demo]) %>%
        mutate(demo = !!sym(demo)) %>%
        select(.data$demo, .data$total)

      new_data <- private$mrp_$new
      if ("time" %in% names(new_data)) {
        new_data <- new_data %>% filter(.data$time == 1)
      }
      new_data <- new_data %>%
        as_factor(private$mrp_$levels[demo]) %>%
        mutate(demo = !!sym(demo)) %>%
        select(.data$demo, .data$total)

      p <- plot_demographic(input_data, new_data)

      if (!is.null(file)) {
        # Set default parameters for ggsave
        dots <- modifyList(GLOBAL$plot$save, list(...))
        do.call(ggplot2::ggsave, c(list(filename = file, plot = p), dots))
      }

      invisible(p)
    },
    
    #' @description Creates histogram plots showing the distribution of geographic
    #' covariates across zip codes. Only available for COVID data.
    #'
    #' @param covar Character string specifying the covariate. Options: "edu",
    #'   "poverty", "employ", "income", "urban", "adi"
    #' @param file Optional file path to save the plot
    #' @param ... Additional arguments passed to ggsave
    #'
    #' @return A ggplot object showing the covariate distribution histogram
    covar_hist = function(covar, file = NULL, ...) {
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
        "edu" = list(
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
        "employ" = list(
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
        "adi" = list(
          threshold   = 0.95,
          operation   = ">=",
          breaks      = seq(0, 1, 0.05),
          description = "\n%d zip codes out of %d (%d%%) have %d%% or more tracts classified as urban.",
          definition  = "Urbanicity of a zip code is defined as the percentage of covered census tracts classified as urban\nweighted by tract population counts.",
          name        = "Urbanicity"
        ),
        "urban" = list(
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
        plot_geographic(
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

      invisible(p)
    },
    
    #' @description Creates interactive choropleth maps showing data distribution
    #' with respect to geography.
    #'
    #' @param file Optional file path to save the plot
    #'
    #' @return A highcharter map object showing sample size distribution
    sample_size_map = function(file = NULL) {
      private$assert_mrp_exists()

      geo <- private$linkdata_$link_geo
      
      hc <- private$mrp_$input %>%
        prep_sample_size(
          fips_codes = fips_[[geo]],
          geo = geo,
          for_map = TRUE
        ) %>%
        choro_map(
          geojson_[[geo]],
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
          selfcontained = TRUE
        )
      }

      return(hc)
    },
    
    #' @description Creates plots showing the distribution of outcome measures over time (for time-varying data) or as static distributions (for cross-sectional data).
    #'
    #' @param file Optional file path to save the plot
    #' @param ... Additional arguments passed to ggsave
    #'
    #' @return A ggplot object showing the outcome measure distribution
    outcome_plot = function(file = NULL, ...) {
      private$assert_mrp_exists()

      p <- if (private$metadata_$is_timevar) {
        plot_outcome_timevar(
          raw = private$mrp_$input,
          yrep_est = NULL,
          dates = private$plotdata_$dates,
          metadata = private$metadata_,
          show_caption = FALSE
        )
      } else {
        plot_outcome_static(
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
    },
    
    #' @description Creates maps showing average outcome measure by geography for
    #' cross-sectional data, or highest/lowest weekly average for time-varying data.
    #'
    #' @param summary_type Character string for time-varying data: "max" or "min"
    #' @param file Optional file path to save the map
    #'
    #' @return A highcharter map object showing outcome measures by geography
    outcome_map = function(summary_type = NULL, file = NULL) {
      private$assert_mrp_exists()

      checkmate::assert_choice(
        summary_type,
        choices = GLOBAL$args$summary_types,
        null.ok = TRUE
      )

      geo <- private$linkdata_$link_geo
      if (geo == "zip") {
        geo <- "county"
      }

      out <- prep_raw(
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

      hc <- choro_map(
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
    },

    #' @description Creates plots showing MRP estimates for different subgroups, either over time (for time-varying data) or as static estimates (for cross-sectional data).
    #'
    #' @param model Fitted MRPModel object
    #' @param subgroup Character string specifying the subgroup variable for plotting
    #' @param file Optional file path to save the plot
    #' @param ... Additional arguments passed to ggsave
    #'
    #' @return A ggplot object showing the subgroup estimates
    estimate_plot = function(model, subgroup, file = NULL, ...) {
      private$assert_mrp_exists()
      
      checkmate::assert_choice(
        subgroup,
        choices = intersect(GLOBAL$vars$pstrat, names(model$mrp()$levels)),
        null.ok = FALSE
      )

      # Convert levels to "factor" type
      est_df <- model$poststratify()[[subgroup]] %>%
        rename(!!subgroup := factor) %>%
        as_factor(model$mrp()$levels[subgroup]) %>%
        rename(factor := !!sym(subgroup))

      p <- if (model$metadata()$is_timevar) {
        plot_est_timevar(
          plot_df = est_df,
          dates = model$plotdata()$dates,
          metadata = model$metadata()
        )
      } else {
        plot_est_static(
          plot_df = est_df,
          metadata = model$metadata()
        )
      }

      if (!is.null(file)) {
        # Set default parameters for ggsave
        dots <- modifyList(GLOBAL$plot$save, list(...))
        dots$height <- 5 * (length(model$mrp()$levels[[subgroup]]) + 1)
        do.call(ggplot2::ggsave, c(list(filename = file, plot = p), dots))
      }      

      return(p)
    },

    #' @description Creates interactive choropleth maps showing MRP estimates by geographic regions.
    #'
    #' @param model Fitted MRPModel object
    #' @param geo Character string specifying the geographic level for mapping
    #' @param time_index Numeric value specifying the time index for time-varying data
    #' @param file Optional file path to save the map
    #' @param ... Additional arguments
    #' 
    #' @return A highcharter map object showing MRP estimates by geography
    estimate_map = function(
      model,
      geo = NULL,
      time_index = NULL,
      file = NULL,
      ...
    ) {
      private$assert_mrp_exists()

      choices <- intersect(GLOBAL$vars$geo2, names(model$mrp()$levels))
      checkmate::assert_choice(
        geo,
        choices = choices,
        null.ok = TRUE
      )
      geo <- replace_null(geo, choices[1])

      time_index <- if (model$metadata()$is_timevar) {
        choices <- model$mrp()$levels$time
        checkmate::assert_choice(
          time_index,
          choices = choices,
          null.ok = TRUE
        )
        replace_null(time_index, choices[1])
      } else {
        NULL
      }


      est_df <- model$poststratify()[[geo]]

      hc <- est_df %>% 
        prep_est(
          fips_codes = fips_[[geo]],
          geo = geo,
          time_index = time_index
        ) %>%
        choro_map(
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
          filename = "sample_size_map",
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
    },

    #' @description Validates the effects specification for model fitting, checking prior syntax, variable availability, and interaction validity.
    #'
    #' @param effects List containing model effects specification including intercept, fixed effects, varying effects, and interactions
    #'
    #' @return TRUE if the effects specification is valid, otherwise throws an error
    check_effects = function(effects) {
      private$assert_mrp_exists()

      # check if effects$Intercept is a list with a single element
      if (!is.list(effects$Intercept) || names(effects$Intercept) != "Intercept") {
        stop("effects$Intercept must be a list with a single element named 'Intercept'.")
      }


      # check prior syntax for all effects
      effects_w_priors <- unlist(effects)
      bools <- effects_w_priors %>%
        purrr::map_lgl(function(s) clean_prior_syntax(s) %>% check_prior_syntax())

      if (!all(bools)) {
        stop(paste0("The following priors have invalid syntax: ",
            paste(effects_w_priors[!bools], collapse = ", ")))
      }

      # check if effects have corresponding variables in data
      main_vars <- c(names(effects$fixed), names(effects$varying))
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
      if (length(pair_intersect(names(effects$interaction), omit_vars$nested)) > 0) {
        stop(paste0("The following interactions have nested main effects and cannot be used: ",
            paste(omit_vars$nested, collapse = ", ")))
      }

      # check if interactions can be assigned structured prior
      ints_w_struct <- names(effects$interaction[effects$interaction == "structured"])
      valid_ints_w_struct <- filter_interactions(
        interactions = ints_w_struct,
        fixed_effects = names(effects$fixed),
        data = private$mrp_$input
      )
      invalid_ints_w_struct <- setdiff(ints_w_struct, valid_ints_w_struct)
      if (length(invalid_ints_w_struct) > 0) {
        stop(paste0("The following interactions cannot be assigned structured prior: ",
            paste(invalid_ints_w_struct, collapse = ", ")))
      }

      return(TRUE)
    },

    #' @description Creates a new MRPModel object with validated effects specification and prepared data for Bayesian model fitting.
    #'
    #' @param effects List containing model effects specification including intercept, fixed effects, varying effects, and interactions
    #'
    #' @return A new MRPModel object
    create_model = function(effects) {
      private$assert_mrp_exists()
      self$check_effects(effects)

      effects <- set_default_priors(effects)

      MRPModel$new(
        effects   = effects,
        mrp       = private$mrp_,
        metadata  = private$metadata_,
        linkdata = private$linkdata_,
        plotdata = private$plotdata_
      )
    },

    #' @description Loads a previously saved MRPModel object from a file.
    #'
    #' @param file File path to the saved MRPModel object
    #'
    #' @return A loaded MRPModel object
    load_model = function(file) {
      checkmate::assert_file_exists(file)

      return(qs::qread(file))
    },

    #' @description Creates posterior predictive check plots to assess model fit by comparing observed data to replicated data from the posterior predictive distribution.
    #'
    #' @param model Fitted MRPModel object
    #' @param file Optional file path to save the plot
    #' @param ... Additional arguments passed to ggsave
    check_model = function(model, file = NULL, ...) {
      private$assert_mrp_exists()

      p <- if (model$metadata()$is_timevar) {
        plot_ppc_timevar_subset(
          yrep = model$ppc(),
          raw = model$mrp()$input,
          dates = model$plotdata()$dates,
          metadata = model$metadata()
        )
      } else {
        plot_ppc_static(
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
    },

    #' @description Compares multiple fitted MRP models using leave-one-out cross-validation to assess relative model performance.
    #'
    #' @param ... Multiple MRPModel objects to compare
    #' @param suppress Character string specifying output to suppress during comparison
    #'
    #' @return A data frame summarizing the comparison results
    compare_models = function(..., suppress = NULL) {
      private$assert_mrp_exists()

      if (length(list(...)) < 2) {
        stop("At least two models are required for comparison.")
      }

      models <- list(...)
      lapply(models, private$check_model)

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
  )
)