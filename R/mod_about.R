#' About Page Module UI Function
#'
#' @description Creates the user interface for the about page displaying
#' information about the development team, acknowledgements, references,
#' and support resources. Provides a centered layout with team member
#' information, funding acknowledgements, academic references, and
#' links for user feedback and issue reporting.
#'
#' @param id Character string. The module's namespace identifier.
#'
#' @return A `tagList` containing the about page interface with:
#' \itemize{
#'   \item Development team member information
#'   \item Funding and acknowledgement details
#'   \item Academic references and publications
#'   \item Links for feedback and issue reporting
#' }
#'
#' @noRd
#' @keywords internal
#'
#' @importFrom shiny NS tagList fluidRow column tags
mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6, offset = 3,
        tags$div(style = "margin-top: 40px",
          tags$h4("Development team members"),
          tags$p("Yajuan Si (University of Michigan)"),
          tags$p("Toan Tran (University of Michigan)"),
          tags$p("Jonah Gabry (Columbia University)"),
          tags$p("Andrew Gelman (Columbia University)"),
          tags$p("Mitzi Morris (Columbia University)")
        ),
        tags$div(style = "margin-top: 40px",
          tags$h4("Acknowledgements"),
          tags$p("This work is funded by the National Institute on Minority Health and Health Disparities of the National Institutes of Health (U01MD017867)."),
          tags$p("This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau.")
        ),
        tags$div(style = "margin-top: 40px",
          tags$h4("References"),
            tags$p(tags$a("Y Si (2024). On the Use of Auxiliary Variables in Multilevel Regression and Poststratification, Statistical Science (forthcoming).", href = "https://arxiv.org/abs/2011.00360", target = "_blank")),
            tags$p(tags$a("Y Si, L Covello, S Wang, T Covello, and A Gelman (2022). Beyond Vaccination Rates: A Synthetic Random Proxy Metric of Total SARS-CoV-2 Immunity Seroprevalence in the Community, Epidemiology, 33(4), 457--464.", href = "https://journals.lww.com/epidem/abstract/2022/07000/beyond_vaccination_rates__a_synthetic_random_proxy.3.aspx", target = "_blank")),
            tags$p(tags$a("L Covello, A Gelman, Y Si, and S Wang (2021). Routine Hospital-Based SARS-CoV-2 Testing Outperforms State-Based Data in Predicting Clinical Burden,  Epidemiology, 32(6), 792--799.", href = "https://journals.lww.com/epidem/abstract/2021/11000/routine_hospital_based_sars_cov_2_testing.4.aspx", target = "_blank")),
            tags$p(tags$a("A Gelman, and B Carpenter (2020), Bayesian analysis of tests with unknown specificity and sensitivity. Journal of the Royal Statistical Society C, Applied Statistics, 69, 1269--1284.", href = "https://academic.oup.com/jrsssc/article/69/5/1269/7058663", target = "_blank")),
            tags$p(tags$a("Y Si, R Trangucci, J Gabry, and A Gelman (2020). Bayesian Hierarchical Weighting Adjustment and Survey Inference, Survey Methodology, 46(2), 181--214.", href = "https://www150.statcan.gc.ca/n1/en/pub/12-001-x/2020002/article/00003-eng.pdf?st=iF1_Fbrh", target = "_blank")),
            tags$p(tags$a("J Lopez-Martin, J Phillips, and A Gelman (2022). Multilevel Regression and Poststratification Case Studies, bookdown.org/jl5522/MRP-case-studies.", href = "https://bookdown.org/jl5522/MRP-case-studies/", target = "_blank")),
            tags$p(tags$a("Y Si, T Tran, J Gabry, M Morris, and A Gelman (2024). Multilevel Regression and Poststratification Interface: Application to Track Community-level COVID-19 Viral Transmission. arXiv preprint arXiv:2405.05909.", href = "https://arxiv.org/abs/2405.05909", target = "_blank"))
        ),
        tags$div(style = "margin-top: 40px",
          tags$h4("Have issues or questions?"),
          tags$p("Provide feedback ", tags$a("here", href = "https://docs.google.com/forms/d/e/1FAIpQLSdqjTlLsdziJNnPjGGR7vYbNxYeAGdLg5oAxEGMD1EA92g-UQ/viewform?usp=sf_link", target = "_blank"), " or submit an issue on our ", tags$a("GitHub page.", href = "https://github.com/mrp-interface/shinymrp/issues", target = "_blank"))
        )
      )
    )

  )
}

#' About Page Module Server Function
#'
#' @description Server logic for the about page module. Currently contains
#' minimal server-side functionality as the about page is primarily static
#' content. Maintains the module structure for potential future enhancements
#' such as dynamic content updates or user interaction tracking.
#'
#' @param id Character string. The module's namespace identifier.
#'
#' @return Server function for the about module. Currently provides basic
#' module server structure without active reactive functionality.
#'
#' @noRd
#' @keywords internal
#'
#' @importFrom shiny moduleServer
mod_about_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
