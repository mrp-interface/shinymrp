#' @importFrom dplyr filter mutate
#' @importFrom rlang .data
#' @importFrom tools toTitleCase

mrp_workflow <- function() {
    if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
        stop("CmdStan is not installed or not available.")
    }

    MRPWorkflow$new()
}

MRPWorkflow <- R6::R6Class(
  "MRPWorkflow",
  private = list(
    data = NULL,
    metadata = NULL,
    link_data = NULL,
    plot_data = NULL,
    mrp = NULL,
    models = NULL,
    poststratified_models = NULL
  ),
  public = list(
    initialize = function() {
    },
    
    preprocess = function(
      data,
      is_timevar = FALSE,
      is_aggregated = FALSE,
      special_case = NULL,
      family = NULL
    ) {

      checkmate::assert_choice(
        family,
        choices = GLOBAL$family,
        null.ok = TRUE
      )

      private$metadata <- list(
        is_timevar = is_timevar,
        special_case = special_case,
        family = family
      )

      message("Preprocessing sample data...")

      tryCatch({
        private$data <- preprocess(
          data = data,
          metadata = private$metadata,
          is_sample = TRUE,
          is_aggregated = is_aggregated
        )
      
      }, error = function(e) {
        # show error message
        error_message <- paste("Error processing data:\n", e$message)
        message(error_message)
      })
    
    },

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
        choices = GLOBAL$acs_years,
        null.ok = TRUE
      )

      if (is.null(private$data)) {
        stop("Sample data is not available. Please provide sample data through the `preprocess` method first.")
      }

      message("Linking data to the ACS...")

      # store user's selections for data linking
      private$link_data <- list(
        link_geo = if(link_geo %in% GLOBAL$vars$geo) link_geo else NULL,
        acs_year = acs_year
      )

      tryCatch({
        if (!is.null(private$metadata$special_case) &&
            private$metadata$special_case == "covid") {

          if (is.null(private$link_data$link_geo) ||
              private$link_data$link_geo != "zip") {
            private$link_data$link_geo <- "zip"
            warning(stringr::str_interp("Linking geography is either incorrect or not specified. Using 'zip' as default for COVID data."))
          }

          # prepare data for MRP
          private$mrp <- prepare_mrp_covid(
            input_data = private$data,
            pstrat_data = acs_covid_$pstrat,
            covariates = acs_covid_$covar,
            metadata   = private$metadata
          )

          # prepare data for plotting
          private$plot_data <- list(
            dates = if("date" %in% names(private$data)) get_dates(private$data) else NULL,
            geojson = list(county = filter_geojson(
              geojson_$county,
              private$mrp$levels$county
            )),
            raw_covariates = acs_covid_$covar %>%  
              filter(.data$zip %in% unique(private$mrp$input$zip))
          )

        } else if (!is.null(private$metadata$special_case) &&
                    private$metadata$special_case == "poll") {

          if (is.null(private$link_data$link_geo) ||
              private$link_data$link_geo != "state") {
            private$link_data$link_geo <- "state"
            warning(stringr::str_interp("Linking geography is either incorrect or not specified. Using 'state' as default for polling data."))
          }

          new_data <- acs_poll_$pstrat %>%
            mutate(state = to_fips(.data$state, "state"))

          private$mrp <- prepare_mrp_custom(
            input_data = private$data,
            new_data = new_data,
            metadata = private$metadata,
            link_geo = "state"
          )

          # prepare data for plotting
          private$plot_data <- list(
            geojson = list(state = filter_geojson(
              geojson_$state,
              private$mrp$levels$state
            ))
          )

        } else {
          if (is.null(private$link_data$acs_year)) {
            private$link_data$acs_year <- GLOBAL$acs_years[length(GLOBAL$acs_years)]
            warning(stringr::str_interp("ACS year not specified. Using the latest year: ${private$link_data$acs_year}."))
          }

          # retrieve ACS data based on user's input
          tract_data <- acs_[[as.character(private$link_data$acs_year)]]

          # prepare data for MRP
          private$mrp <- prepare_mrp_acs(
            input_data = private$data,
            tract_data = tract_data,
            metadata = private$metadata,
            link_geo = private$link_data$link_geo
          )

          # prepare data for plotting
          plot_data <- list()
          plot_data$dates <- if("date" %in% names(private$data)) get_dates(private$data) else NULL
          plot_data$geojson <- names(geojson_) %>%
            stats::setNames(nm = .) %>%
            purrr::map(~filter_geojson(
              geojson = geojson_[[.x]], 
              geoids = private$mrp$levels[[.x]]
            ))

          private$plot_data <- nullify(plot_data)
        }

        # set success to TRUE if no errors occurred
        success <- TRUE

      }, error = function(e) {
        message(paste("Error linking data:\n", e$message))
      })
    },
    
    load_pstrat = function(pstrat_data, is_aggregated = FALSE) {
      if (!is.null(private$metadata$special_case)) {
        stop("Custom post-stratification data is not supported for special cases like COVID or polling data. Please use the `link_acs` method instead.")
      }

      if (is.null(private$data)) {
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
          metadata = private$metadata,
          is_sample = FALSE,
          is_aggregated = is_aggregated
        )

        # Compare to sample data
        check_pstrat(new_data, private$data, create_expected_levels(private$metadata))

        # Find the smallest common geography
        link_geo <- NULL
        common <- intersect(names(private$data), names(new_data))
        smallest <- get_smallest_geo(common)
        if (!is.null(smallest)) {
          link_geo <- smallest$geo
        }

        # Store linking geography
        private$link_data <- list(
          link_geo = link_geo,
          acs_year = NULL
        )

        # Prepare data for MRP
        private$mrp <- prepare_mrp_custom(
          input_data = private$data,
          new_data = new_data,
          metadata = private$metadata,
          link_geo = link_geo
        )


        # prepare data for plotting
        plot_data <- list()
        plot_data$dates <- if("date" %in% names(private$data)) get_dates(private$data) else NULL
        plot_data$geojson <- names(geojson_) %>%
          stats::setNames(nm = .) %>%
          purrr::map(~filter_geojson(
            geojson = geojson_[[.x]], 
            geoids = private$mrp$levels[[.x]]
          ))

        private$plot_data <- nullify(plot_data)

      }, error = function(e) {
        # show error message
        error_message <- paste("Error processing data:\n", e$message)
        message(error_message)
        
        # reset fields
        private$link_data <- NULL
        private$mrp <- NULL
        private$plot_data <- NULL
        
      })
    },
    # Visualization methods
    
    #' Create demographic comparison bar plots
    #'
    #' @description Creates bar plots comparing demographic distributions between
    #' input survey data and target population data.
    #'
    #' @param separate Logical indicating whether to create separate plots (TRUE) or
    #'   side-by-side comparison (FALSE)
    #'
    #' @return A ggplot object or patchwork object showing demographic comparisons
    demo_bars = function(demo, save_file = NULL, ...) {
      if (is.null(private$mrp)) {
        stop("Data for MRP is not available. Please run data preparation steps first.")
      }

      checkmate::assert_choice(
        demo,
        choices = GLOBAL$vars$demo,
        null.ok = FALSE
      )

      input_data <- private$mrp$input %>%
        as_factor(private$mrp$levels[demo]) %>%
        mutate(demo = !!sym(demo)) %>%
        select(.data$demo, .data$total)

      new_data <- private$mrp$new
      if ("time" %in% names(new_data)) {
        new_data <- new_data %>% filter(.data$time == 1)
      }
      new_data <- new_data %>%
        as_factor(private$mrp$levels[demo]) %>%
        mutate(demo = !!sym(demo)) %>%
        select(.data$demo, .data$total)

      p <- plot_demographic(input_data, new_data)

      if (!is.null(save_file)) {
        # Set default parameters for ggsave
        dots <- list(...)
        if (is.null(dots$dpi)) dots$dpi <- 300
        if (is.null(dots$width)) dots$width <- 14
        if (is.null(dots$height)) dots$height <- 8
        if (is.null(dots$units)) dots$units <- "in"
        do.call(ggplot2::ggsave, c(list(filename = save_file, plot = p), dots))
      }

      invisible(p)
    },
    
    #' Create geographic covariate distribution histogram
    #'
    #' @description Creates histogram plots showing the distribution of geographic
    #' covariates across zip codes. Only available for COVID data.
    #'
    #' @param covariate Character string specifying the covariate. Options: "edu",
    #'   "poverty", "employ", "income", "urban", "adi"
    #'
    #' @return A ggplot object showing the covariate distribution histogram
    covar_hist = function(covar, save_file = NULL, ...) {
      raw_covariates <- private$plot_data$raw_covariates
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

      if (!is.null(save_file)) {
        dots <- list(...)
        # Set default parameters for ggsave
        if (is.null(dots$dpi)) dots$dpi <- 300
        if (is.null(dots$width)) dots$width <- 18
        if (is.null(dots$height)) dots$height <- 8
        if (is.null(dots$units)) dots$units <- "in"
        do.call(ggplot2::ggsave, c(list(filename = save_file, plot = p), dots))
      }

      p
    },
    
    #' Create sample size map
    #'
    #' @description Creates interactive choropleth maps showing data distribution
    #' with respect to geography.
    #'
    #' @param geo Character string specifying geographic level: "county" or "state"
    #'
    #' @return A highcharter map object showing sample size distribution
    sample_size_map = function(save_file = NULL) {
      if (is.null(private$mrp)) {
        stop("Data for MRP is not available. Please run data preparation steps first.")
      }

      geo <- private$link_data$link_geo
      
      hc <- private$mrp$input %>%
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

      if (!is.null(save_file)) {
        # Set default parameters for ggsave
        htmlwidgets::saveWidget(
          hc,
          file = save_file,
          selfcontained = TRUE
        )
      }

      invisible(hc)
    },
    
    #' Create line plots of outcome measures for time-varying data
    #'
    #' @description Creates line plots showing outcome measures over time, with
    #' optional model estimates and uncertainty bands.
    #'
    #' @param estimates Optional data frame containing model estimates with time,
    #'   est, and std columns
    #'
    #' @return A ggplot object showing outcome time series
    outcome_line = function(save_file = NULL, ...) {
      if (is.null(private$mrp)) {
        stop("Data for MRP is not available. Please run data preparation steps first.")
      }
      
      if (!private$metadata$is_timevar) {
        stop("This method is only available for time-varying data. Use outcome_point() for cross-sectional data.")
      }
      
      p <- plot_outcome_timevar(
        raw = private$mrp$input,
        yrep_est = NULL,
        dates = private$plot_data$dates,
        metadata = private$metadata,
        show_caption = FALSE
      )

      if (!is.null(save_file)) {
        # Set default parameters for ggsave
        dots <- list(...)
        if (is.null(dots$dpi)) dots$dpi <- 300
        if (is.null(dots$width)) dots$width <- 18
        if (is.null(dots$height)) dots$height <- 8
        if (is.null(dots$units)) dots$units <- "in"
        do.call(ggplot2::ggsave, c(list(filename = save_file, plot = p), dots))
      }

    },
    
    #' Create point plots of outcome measures for cross-sectional data
    #'
    #' @description Creates point plots with error bars comparing raw outcome rates
    #' with model estimates and uncertainty intervals.
    #'
    #' @param estimates Data frame containing model estimates with est and std columns
    #'
    #' @return A ggplot object showing outcome comparison with error bars
    outcome_point = function(save_file = NULL, ...) {
      if (is.null(private$mrp)) {
        stop("Data for MRP is not available. Please run data preparation steps first.")
      }
      
      if (private$metadata$is_timevar) {
        stop("This method is only available for cross-sectional data. Use outcome_line() for time-varying data.")
      }

      p <- plot_outcome_static(
        raw = private$mrp$input,
        yrep_est = NULL,
        metadata = private$metadata
      )

      if (!is.null(save_file)) {
        # Set default parameters for ggsave
        dots <- list(...)
        if (is.null(dots$dpi)) dots$dpi <- 300
        if (is.null(dots$width)) dots$width <- 18
        if (is.null(dots$height)) dots$height <- 8
        if (is.null(dots$units)) dots$units <- "in"
        do.call(ggplot2::ggsave, c(list(filename = save_file, plot = p), dots))
      }

      invisible(p)
    },
    
    #' Create outcome measure maps by geography
    #'
    #' @description Creates maps showing average outcome measure by geography for
    #' cross-sectional data, or highest/lowest weekly average for time-varying data.
    #'
    #' @param geo Character string specifying geographic level: "county" or "state"
    #' @param summary_type Character string for time-varying data: "max" or "min"
    #'
    #' @return A highcharter map object showing outcome measures by geography
    outcome_map = function(geo = "county", summary_type = "max") {
      if (is.null(private$mrp)) {
        stop("MRP data is not available. Please run link_acs() or load_pstrat() first.")
      }
      
      checkmate::assert_choice(
        geo,
        choices = GLOBAL$vars$geo2,
        null.ok = FALSE
      )
      summary_type <- match.arg(summary_type, choices = c("max", "min"))
      
      # Check if geographic level is available
      if (!geo %in% names(private$mrp$levels)) {
        stop(paste("Geographic level", geo, "is not available in the data."))
      }
      
      # Get FIPS codes
      fips_codes <- switch(geo,
        "county" = fips_$county,
        "state" = fips_$state
      )
      
      # Prepare raw data
      result <- prep_raw(
        data = private$mrp$input,
        fips_codes = fips_codes,
        geo = geo,
        summary_type = summary_type,
        metadata = private$metadata
      )
      
      if (is.null(result)) {
        return(NULL)
      }
      
      # Get geojson
      geojson <- private$plot_data$geojson[[geo]]
      
      # Create map
      choro_map(
        plot_df = result$plot_df,
        geojson = geojson,
        geo = geo,
        config = result$title
      )
    },
    
    #' Create line plots of overall and subgroup estimates for time-varying data
    #'
    #' @description Creates multi-panel line plots showing model estimates over time
    #' for different factor levels with uncertainty bands.
    #'
    #' @param estimates Data frame containing estimates with factor, time, est, and std columns
    #' @param subgroup Character string specifying subgroup: "overall", "sex", "race", "age", "edu", "geo"
    #'
    #' @return A patchwork object containing multiple ggplot panels
    estimate_line = function(estimates, subgroup = "overall") {
      if (is.null(estimates)) {
        stop("Estimates are required for estimate plots.")
      }
      
      if (!private$metadata$is_timevar) {
        stop("This method is only available for time-varying data. Use estimate_point() for cross-sectional data.")
      }
      
      # Filter estimates based on subgroup
      if (subgroup == "overall") {
        plot_df <- estimates %>%
          filter(.data$factor == "overall")
      } else {
        # Check if subgroup is valid
        valid_subgroups <- c("sex", "race", "age", "edu", "geo")
        if (!subgroup %in% valid_subgroups) {
          stop(paste("Subgroup must be one of:", paste(c("overall", valid_subgroups), collapse = ", ")))
        }
        
        plot_df <- estimates %>%
          filter(grepl(paste0("^", subgroup), .data$factor))
      }
      
      if (nrow(plot_df) == 0) {
        stop(paste("No estimates found for subgroup:", subgroup))
      }
      
      plot_est_temporal(
        plot_df = plot_df,
        dates = private$plot_data$dates,
        metadata = private$metadata
      )
    },
    
    #' Create point plots of overall and subgroup estimates for cross-sectional data
    #'
    #' @description Creates point plots with error bars showing model estimates
    #' for different factor levels in cross-sectional data.
    #'
    #' @param estimates Data frame containing estimates with factor, est, and std columns
    #' @param subgroup Character string specifying subgroup: "overall", "sex", "race", "age", "edu", "geo"
    #'
    #' @return A ggplot object showing estimates with error bars
    estimate_point = function(estimates, subgroup = "overall") {
      if (is.null(estimates)) {
        stop("Estimates are required for estimate plots.")
      }
      
      if (private$metadata$is_timevar) {
        stop("This method is only available for cross-sectional data. Use estimate_line() for time-varying data.")
      }
      
      # Filter estimates based on subgroup
      if (subgroup == "overall") {
        plot_df <- estimates %>%
          filter(.data$factor == "overall")
      } else {
        # Check if subgroup is valid
        valid_subgroups <- c("sex", "race", "age", "edu", "geo")
        if (!subgroup %in% valid_subgroups) {
          stop(paste("Subgroup must be one of:", paste(c("overall", valid_subgroups), collapse = ", ")))
        }
        
        plot_df <- estimates %>%
          filter(grepl(paste0("^", subgroup), .data$factor))
      }
      
      if (nrow(plot_df) == 0) {
        stop(paste("No estimates found for subgroup:", subgroup))
      }
      
      plot_est_static(
        plot_df = plot_df,
        metadata = private$metadata
      )
    }
  )
)