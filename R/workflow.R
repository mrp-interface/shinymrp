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

      message("Preprocessing data...")

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
        null.ok = FALSE
      )

      if (is.null(private$data)) {
        stop("Sample data is not available. Please provide sample data through the `preprocess` method first.")
      }

      message("Linking data to the ACS...")
      
      tryCatch({
        # store user's selections for data linking
        private$link_data <- list(
          link_geo = if(link_geo %in% GLOBAL$vars$geo) link_geo else NULL,
          acs_year = acs_year
        )

        if(!is.null(private$metadata$special_case) &&
            private$metadata$special_case == "covid") {

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
    }
  )
)