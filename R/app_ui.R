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
    waiter::waiterShowOnLoad(
      html = .waiter_ui("init"),
      color = waiter::transparent(1)
    ),
    bslib::page_navbar(
      title = tagList(
        tags$img(
          src = "www/favicon.svg",
          height = "50px",
          alt = "MRP"
        )
      ),
      window_title = "shinymrp",
      id = "navbar",
      theme = bslib::bs_theme(version = 5) %>% 
        bslib::bs_add_rules(GLOBAL$ui$style$global),
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
            tags$div(
              class = "d-flex align-items-center",
              tags$span(
                class = "me-2",
                style = "font-size: 1.05rem;",
                conditionalPanel(
                  condition = "output.special_case == 'covid'",
                  GLOBAL$ui$use_case_labels$covid
                ),
                conditionalPanel(
                  condition = "output.special_case == 'poll'",
                  GLOBAL$ui$use_case_labels$poll
                ),
                conditionalPanel(
                  condition = "output.special_case === null && output.is_timevar",
                  GLOBAL$ui$use_case_labels$timevar_general
                ),
                conditionalPanel(
                  condition = "output.special_case === null && !output.is_timevar",
                  GLOBAL$ui$use_case_labels$static_general
                )
              ),
              actionLink(
                inputId = ".show_guide",
                label = bsicons::bs_icon("question-circle", size = "1.5em"),
                class = "btn btn-link p-0"  # Remove padding from the button for tighter layout
              )
            )
          )
        )
      ),
      bslib::nav_menu(
        title = "Learn",
        value = "nav_learn",
        bslib::nav_panel(
          title = "User Guide",
          value = "nav_learn_interface",
          mod_learn_interface_ui(module_ids$learn$interface)
        ),
        bslib::nav_panel(
          title = "Data Preprocessing",
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
          href = "https://mrp-interface.github.io/shinymrp/index.html",
          target = "_blank",
          bsicons::bs_icon("info-circle-fill", size = "1.5em")
        )
      ),
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
