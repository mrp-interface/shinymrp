#' ShinyMRPWorkflow objects
#'
#' @description A sub-class of MRPWorkflow for use with Shiny applications
#'
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
    },

    create_model = function(model_spec) {
      private$assert_mrp_exists()
      private$assert_model_spec(model_spec)

      model_spec <- .set_default_priors(model_spec)

      ShinyMRPModel$new(
        model_spec = model_spec,
        mrp_data   = private$mrpdat_,
        metadata   = private$metadat_,
        link_data  = private$linkdat_,
        plot_data  = private$plotdat_
      )
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
            m$loo(),
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

    check_data_exists = function() {
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