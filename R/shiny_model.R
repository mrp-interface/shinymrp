#' ShinyMRPModel objects
#'
#' @description A sub-class of MRPModel for use with Shiny applications.
#' This class extends MRPModel with additional functionality for UI integration,
#' including unique ID generation and name management for Shiny modules.
#'
#' @noRd
#' @keywords internal
ShinyMRPModel <- R6::R6Class(
  "ShinyMRPModel",
  inherit = MRPModel,
  private = list(
    id_ = NULL,
    name_ = NULL
  ),
  public = list(
    initialize = function(...) {
      super$initialize(...)
    },

    assign_id = function() {
      model_id <- .generate_id()
      private$id_ <- list(
        main = model_id,
        fixed_tbl = paste0("fixed_tbl_", model_id),
        varying_tbl = paste0("varying_tbl_", model_id),
        other_tbl = paste0("other_tbl_", model_id),
        ppc_plot = paste0("ppc_plot_", model_id),
        tab = paste0("tab_", model_id),
        title = paste0("title_", model_id),
        rm_btn = paste0("rm_btn_", model_id),
        save_popover_btn = paste0("save_popover_btn_", model_id),
        save_fit_btn = paste0("save_fit_btn_", model_id),
        save_code_btn = paste0("save_code_btn_", model_id),
        diagnos_tooltip = paste0("diagnos_tooltip_", model_id),
        diagnos_btn = paste0("diagnos_btn_", model_id),
        diagnos_tbl = paste0("diagnos_tbl_", model_id),
        postprocess_btn = paste0("postprocess_btn_", model_id)
      )
    },

    get_id = function(ui_name = NULL) {
      checkmate::assert_choice(
        ui_name,
        choices = names(private$id_),
        null.ok = TRUE
      )

      ui_name <- ui_name %||% "main"

      return(private$id_[[ui_name]])
    },

    set_name = function(name) {
      private$name_ <- name
    },

    name = function() {
      return(private$name_)
    }
  )
)