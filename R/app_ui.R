#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    navbarPage("Multilevel Regression & Poststratification",
      theme = shinythemes::shinytheme("flatly"),
      id = "navbar",
      tabPanel("Home",
        value = "nav_home",
        icon = icon("home", lib = "glyphicon"),
        mod_home_ui(module_ids$home)
      ),
      tabPanel("Analyze",
        value = "nav_analyze",
        icon = icon("stats", lib = "glyphicon"),
        tabsetPanel(id = "navbar_analyze",
          tabPanel(tagList("Upload data", icon("chevron-right", "fa")),
            value = "nav_analyze_upload",
            mod_analyze_upload_ui(module_ids$analyze$upload)
          ),
          tabPanel(tagList("Visualize data", icon("chevron-right", "fa")),
            value = "nav_analyze_visualize",
            mod_analyze_visualize_ui(module_ids$analyze$visualize)
          ),
          tabPanel(tagList("Fit model", icon("chevron-right", "fa")),
            value = "nav_analyze_model",
            mod_analyze_model_ui(module_ids$analyze$model)
          ),
          tabPanel("View results",
            value = "nav_analyze_result",
            mod_analyze_result_ui(module_ids$analyze$result)
          )
        )
      ),
      navbarMenu("Learn",
        icon = icon("book", lib = "glyphicon"),
        # tabPanel("Interface",
        #   value = "nav_learn_interface",
        #   mod_learn_interface_ui(module_ids$learn$interface)
        # ),
        tabPanel("Preprocess",
          value = "nav_learn_preprocess",
          mod_learn_preprocess_ui(module_ids$learn$preprocess)
        ),
        tabPanel("MRP",
          value = "nav_learn_mrp",
          mod_learn_mrp_ui(module_ids$learn$mrp)
        )
      ),
      tabPanel("About",
        value = "nav_about",
        icon = icon("user", lib = "glyphicon"),
        mod_about_ui(module_ids$about)
      )
    ),
    mod_persist_ui(module_ids$persist)
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
