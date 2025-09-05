#' ShinyMRPWorkflow objects
#'
#' @description A sub-class of MRPWorkflow for use with Shiny applications
#'
#' @noRd
#' @keywords internal
ShinyMRPWorkflow <- R6::R6Class(
  "ShinyMRPWorkflow",
  inherit = MRPWorkflow,

  private = list(
    assert_model = function(model) {
      checkmate::assert_class(
        model,
        classes = "ShinyMRPModel",
        null.ok = FALSE
      )
    }
  ),

  public = list(
    initialize = function() {
      super$initialize()
      private$model_class_ <- ShinyMRPModel
    },

    reset = function() {
      private$prepdat_ <- NULL
      private$metadat_ <- NULL
      private$linkdat_ <- NULL
      private$plotdat_ <- NULL
      private$mrpdat_ <- NULL
    },

    covar_table = function(covar) {
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

      decimal_places <- if(stats::median(raw_covariates[[covar]]) > 100) 0 else 4

      raw_covariates %>%
        mutate(measure = round(!!sym(covar), decimal_places)) %>%
        select("zip", "measure") %>%
        rename("ZIP Code" = "zip") %>%
        DT::datatable(
          options = list(
            lengthChange = FALSE,
            searching = FALSE,
            info = FALSE,
            ordering = FALSE,
            pagingType = "simple"
          )
        )
    },

    sample_size_table = function() {
      private$assert_mrp_exists()

      geo <- private$linkdat_$link_geo
      if (geo == "zip") {
        geo <- "county"
      } else if (is.null(geo)) {
        stop("Linking geography is not available.")
      }
      
      private$mrpdat_$input %>%
        .prep_sample_size(
          fips_codes = fips_[[geo]],
          geo = geo,
          for_map = FALSE
        ) %>%
        DT::datatable(
          options = list(
            lengthChange = FALSE,
            searching = FALSE,
            info = FALSE,
            ordering = FALSE,
            pagingType = "simple"
          )
        )
    },

    estimate_plot_geo = function(model, geo, subset = NULL) {
      checkmate::assert_choice(geo, .const()$vars$geo2)

      fips_df <- fips_[[geo]] %>% .fips_upper()

      plot_df <- model$poststratify()[[geo]] %>%
        rename("fips" = "factor") %>%
        left_join(fips_df, by = "fips") %>%
        rename("factor" := all_of(geo))

      subset <- subset %||% plot_df$factor[1]

      plot_df <- plot_df %>%
        filter(factor %in% subset)

      p <- if(model$metadata()$is_timevar) {
        .plot_est_timevar(
          plot_df = plot_df,
          dates = model$plot_data()$dates,
          metadata = model$metadata()
        )
      } else {
        .plot_est_static(
          plot_df = plot_df,
          metadata = model$metadata()
        )
      }

      return(p)
    },

    estimate_map = function(model, geo, slider_input = NULL) {
      checkmate::assert_choice(geo, .const()$vars$geo2)
      is_timevar <- model$metadata()$is_timevar
      dates <- model$plot_data()$dates

      time_index <- NULL
      if (is_timevar) {
        time_index <- if (is.character(slider_input)) {
          which(slider_input == dates)
        } else {
          slider_input %||% 1
        }
      }
      
      super$estimate_map(model, geo, time_index)
    },

    compare_models = function(models, suppress = "message") {

      if (length(models) < 2) {
        stop("At least two models are required for comparison.")
      }

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

      # Check for problematic Pareto k values
      flag <- FALSE
      pareto_k_dfs <- purrr::map(loo_list, ~ loo::pareto_k_table(.x))
      for (df in pareto_k_dfs) {
        if (sum(df[2:3, 1]) > 0) {
          flag <- TRUE
          break
        }
      }

      # Compare the models using loo_compare
      compare_df <- loo_list %>%
        loo::loo_compare() %>%
        as.data.frame() %>%
        select("elpd_diff", "se_diff")


      return(list(
        compare_df = compare_df,
        pareto_k_dfs = pareto_k_dfs,
        flag = flag
      ))
    },

    check_metadata_exists = function() {
      return(!is.null(private$metadat_))
    },

    check_prep_data_exists = function() {
      return(!is.null(private$prepdat_))
    },

    check_mrp_exists = function() {
      return(!is.null(private$mrpdat_))
    },

    link_data = function() {
      return(private$linkdat_)
    },

    plot_data = function() {
      return(private$plotdat_)
    },

    mrp_data = function() {
      return(private$mrpdat_)
    }
  )
)