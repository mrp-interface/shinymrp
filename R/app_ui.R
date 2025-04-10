#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    bslib::page_navbar(
      title = "MRP",
      id = "navbar",
      theme = bslib::bs_theme(version = 5),
      sidebar = bslib::sidebar(
        id = "guide",
        width = "100%",
        position = "right",
        open = FALSE,
        create_guide()
      ),      
      # Rest of your UI elements continue here
      bslib::nav_panel(
        title = "Home",
        value = "nav_home",
        mod_home_ui(module_ids$home)
      ),
      bslib::nav_panel(
        title = "Analyze",
        value = "nav_analyze",
        bslib::navset_card_underline(
          id = "navbar_analyze",
          bslib::nav_panel(
            title = "Upload data",
            value = "nav_analyze_upload",
            mod_analyze_upload_ui(module_ids$analyze$upload)
          ),
          bslib::nav_panel(
            title = "Visualize data",
            value = "nav_analyze_visualize",
            mod_analyze_visualize_ui(module_ids$analyze$visualize)
          ),
          bslib::nav_panel(
            title = "Fit model",
            value = "nav_analyze_model",
            mod_analyze_model_ui(module_ids$analyze$model)
          ),
          bslib::nav_panel(
            title = "View results",
            value = "nav_analyze_result",
            mod_analyze_result_ui(module_ids$analyze$result)
          ),
          bslib::nav_spacer(),
          bslib::nav_item(
            tags$span(
              conditionalPanel(
                condition = "output.data_format == 'temporal_covid'",
                "Time-varying: COVID"
              ),
              conditionalPanel(
                condition = "output.data_format == 'temporal_other'",
                "Time-varying: General"
              ),
              conditionalPanel(
                condition = "output.data_format == 'static_poll'",
                "Cross-sectional: With Education"
              ),
              conditionalPanel(
                condition = "output.data_format == 'static_other'",
                "Cross-sectional: General"
              )
            )
          )
        )
      ),
      bslib::nav_menu(
        title = "Learn",
        value = "nav_learn",
        bslib::nav_panel(
          title = "Preprocess",
          value = "nav_learn_preprocess",
          mod_learn_preprocess_ui(module_ids$learn$preprocess)
        ),
        bslib::nav_panel(
          title = "MRP",
          value = "nav_learn_mrp",
          mod_learn_mrp_ui(module_ids$learn$mrp)
        )
      ),
      bslib::nav_panel(
        title = "About",
        value = "nav_about",
        mod_about_ui(module_ids$about)
      ),
      bslib::nav_spacer(),
      bslib::nav_item(
        tags$a(
          class = "btn",
          href = "https://docs.google.com/forms/d/e/1FAIpQLSdqjTlLsdziJNnPjGGR7vYbNxYeAGdLg5oAxEGMD1EA92g-UQ/viewform?usp=sf_link",
          target = "_blank",
          bsicons::bs_icon("chat-text-fill", size = "1.5em")
        )
      ),
      bslib::nav_item(
        tags$a(
          class = "btn",
          href = "https://github.com/mrp-interface/shinymrp",
          target = "_blank",
          bsicons::bs_icon("github", size = "1.5em")
        )
      ),
      bslib::nav_item(
        actionLink(
          inputId = "show_guide",
          label = bsicons::bs_icon("question-circle", size = "1.5em"),
          class = "btn"
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  add_resource_path(
    "sbs",
    system.file("www", package = "shinyBS")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "shinymrp"
    ),

    # enable shinyjs
    shinyjs::useShinyjs(),

    # enable waiter loading spinners
    waiter::use_waiter(),
    waiter::autoWaiter(
      html = waiter::spin_loaders(13, color = "black"),
      color = "white"
    )
  )
}
