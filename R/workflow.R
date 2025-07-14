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
    data_ = NULL,
    metadata_ = NULL,
    link_data_ = NULL,
    plot_data_ = NULL,
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
    }
  ),
  public = list(
    initialize = function() {
    },
    
    metadata = function() {
      private$assert_metadata_exists()
      return(private$metadata_)
    },

    data = function() {
      private$assert_data_exists()
      return(private$data_)
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
      private$link_data_ <- list(
        link_geo = if(link_geo %in% GLOBAL$vars$geo) link_geo else NULL,
        acs_year = acs_year
      )

      tryCatch({
        if (!is.null(private$metadata_$special_case) &&
            private$metadata_$special_case == "covid") {

          if (is.null(private$link_data_$link_geo) ||
              private$link_data_$link_geo != "zip") {
            private$link_data_$link_geo <- "zip"
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
          private$plot_data_ <- list(
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

          if (is.null(private$link_data_$link_geo) ||
              private$link_data_$link_geo != "state") {
            private$link_data_$link_geo <- "state"
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
          private$plot_data_ <- list(
            geojson = list(state = filter_geojson(
              geojson_$state,
              private$mrp_$levels$state
            ))
          )

        } else {
          if (is.null(private$link_data_$acs_year)) {
            private$link_data_$acs_year <- GLOBAL$args$acs_years[length(GLOBAL$args$acs_years)]
            warning(stringr::str_interp("ACS year not specified. Using the latest year: ${private$link_data_$acs_year}."))
          }

          # retrieve ACS data based on user's input
          tract_data <- acs_[[as.character(private$link_data_$acs_year)]]

          # prepare data for MRP
          private$mrp_ <- prepare_mrp_acs(
            input_data = private$data_,
            tract_data = tract_data,
            metadata = private$metadata_,
            link_geo = private$link_data_$link_geo
          )

          # prepare data for plotting
          plot_data <- list()
          plot_data$dates <- if("date" %in% names(private$data_)) get_dates(private$data_) else NULL
          plot_data$geojson <- names(geojson_) %>%
            stats::setNames(nm = .) %>%
            purrr::map(~filter_geojson(
              geojson = geojson_[[.x]], 
              geoids = private$mrp_$levels[[.x]]
            ))

          private$plot_data_ <- nullify(plot_data)
        }


      }, error = function(e) {
        message(paste("Error linking data:\n", e$message))
      })
    },
    
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
        private$link_data_ <- list(
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
        plot_data <- list()
        plot_data$dates <- if("date" %in% names(private$data_)) get_dates(private$data_) else NULL
        plot_data$geojson <- names(geojson_) %>%
          stats::setNames(nm = .) %>%
          purrr::map(~filter_geojson(
            geojson = geojson_[[.x]], 
            geoids = private$mrp_$levels[[.x]]
          ))

        private$plot_data_ <- nullify(plot_data)

      }, error = function(e) {
        # show error message
        error_message <- paste("Error processing data:\n", e$message)
        message(error_message)
        
        # reset fields
        private$link_data_ <- NULL
        private$mrp_ <- NULL
        private$plot_data_ <- NULL
        
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
    
    #' Create geographic covariate distribution histogram
    #'
    #' @description Creates histogram plots showing the distribution of geographic
    #' covariates across zip codes. Only available for COVID data.
    #'
    #' @param covariate Character string specifying the covariate. Options: "edu",
    #'   "poverty", "employ", "income", "urban", "adi"
    #'
    #' @return A ggplot object showing the covariate distribution histogram
    covar_hist = function(covar, file = NULL, ...) {
      private$assert_mrp_exists()

      raw_covariates <- private$plot_data_$raw_covariates
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
    
    #' Create sample size map
    #'
    #' @description Creates interactive choropleth maps showing data distribution
    #' with respect to geography.
    #'
    #' @param geo Character string specifying geographic level: "county" or "state"
    #'
    #' @return A highcharter map object showing sample size distribution
    sample_size_map = function(file = NULL) {
      private$assert_mrp_exists()

      geo <- private$link_data_$link_geo
      
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
    
    outcome_plot = function(file = NULL, ...) {
      private$assert_mrp_exists()

      p <- if (private$metadata_$is_timevar) {
        plot_outcome_timevar(
          raw = private$mrp_$input,
          yrep_est = NULL,
          dates = private$plot_data_$dates,
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
    
    #' Create outcome measure maps by geography
    #'
    #' @description Creates maps showing average outcome measure by geography for
    #' cross-sectional data, or highest/lowest weekly average for time-varying data.
    #'
    #' @param geo Character string specifying geographic level: "county" or "state"
    #' @param summary_type Character string for time-varying data: "max" or "min"
    #'
    #' @return A highcharter map object showing outcome measures by geography
    outcome_map = function(summary_type = NULL, file = NULL) {
      private$assert_mrp_exists()

      checkmate::assert_choice(
        summary_type,
        choices = GLOBAL$args$summary_types,
        null.ok = TRUE
      )

      geo <- private$link_data_$link_geo
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
        private$plot_data_$geojson[[geo]],
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

    create_model = function(effects) {
      private$assert_mrp_exists()
      self$check_effects(effects)

      effects <- set_default_priors(effects)

      MRPModel$new(
        effects   = effects,
        mrp       = private$mrp_,
        metadata  = private$metadata_,
        link_data = private$link_data_,
        plot_data = private$plot_data_
      )
    },

    load_model = function(file) {
      checkmate::assert_file_exists(file)

      return(qs::qread(file))
    },

    check_model = function(model) {
      checkmate::assert_class(
        model,
        classes = "MRPModel",
        null.ok = FALSE
      )

      return(TRUE)
    },

    compare_models = function(..., suppress = NULL) {
      private$assert_mrp_exists()

      if (length(list(...)) < 2) {
        stop("At least two models are required for comparison.")
      }

      models <- list(...)
      lapply(models, self$check_model)

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
    },

    pp_check = function(model, file = NULL, ...) {
      private$assert_mrp_exists()

      p <- if (model$metadata()$is_timevar) {
        plot_ppc_timevar_subset(
          yrep = model$ppc(),
          raw = model$mrp()$input,
          dates = model$plot_data()$dates,
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
          dates = model$plot_data()$dates,
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
    }
  )
)