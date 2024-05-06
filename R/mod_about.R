#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6, offset = 3,
        tags$br(),
        tags$h4("Development team members"),
        tags$p("Yajuan Si (University of Michigan)"),
        tags$p("Toan Tran (University of Michigan)"),
        tags$p("Jonah Gabry (Columbia University)"),
        tags$p("Andrew Gelman (Columbia University)"),
        tags$p("Mitzi Morris (Columbia University)"),
        tags$br(),
        tags$h4("Acknowledgements"),
        tags$p("This work is funded by the National Institute on Minority Health and Health Disparities of the National Institutes of Health (U01MD017867)."),
        tags$p("This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau."),
        tags$br(),
        tags$h4("References"),
        tags$ul(
          tags$li(tags$a("Y Si (2024). On the Use of Auxiliary Variables in Multilevel Regression and Poststratification, Statistical Science (forthcoming).", href = "https://arxiv.org/abs/2011.00360", target = "_blank")),
          tags$li(tags$a("Y Si, L Covello, S Wang, T Covello, and A Gelman (2022). Beyond Vaccination Rates: A Synthetic Random Proxy Metric of Total SARS-CoV-2 Immunity Seroprevalence in the Community, Epidemiology, 33(4), 457--464.", href = "https://journals.lww.com/epidem/abstract/2022/07000/beyond_vaccination_rates__a_synthetic_random_proxy.3.aspx", target = "_blank")),
          tags$li(tags$a("L Covello, A Gelman, Y Si, and S Wang (2021). Routine Hospital-Based SARS-CoV-2 Testing Outperforms State-Based Data in Predicting Clinical Burden,  Epidemiology, 32(6), 792--799.", href = "https://journals.lww.com/epidem/abstract/2021/11000/routine_hospital_based_sars_cov_2_testing.4.aspx", target = "_blank")),
          tags$li(tags$a("A Gelman, and B Carpenter (2020), Bayesian analysis of tests with unknown specificity and sensitivity. Journal of the Royal Statistical Society C, Applied Statistics, 69, 1269--1284.", href = "https://academic.oup.com/jrsssc/article/69/5/1269/7058663", target = "_blank")),
          tags$li(tags$a("Y Si, R Trangucci, J Gabry, and A Gelman (2020). Bayesian Hierarchical Weighting Adjustment and Survey Inference, Survey Methodology, 46(2), 181--214.", href = "https://www150.statcan.gc.ca/n1/en/pub/12-001-x/2020002/article/00003-eng.pdf?st=iF1_Fbrh", target = "_blank")),
          tags$li(tags$a("J Lopez-Martin, J Phillips, and A Gelman (2022). Multilevel Regression and Poststratification Case Studies, bookdown.org/jl5522/MRP-case-studies.", href = "https://bookdown.org/jl5522/MRP-case-studies/", target = "_blank"))
        )
      )
    )

  )
}

#' about Server Functions
#'
#' @noRd
mod_about_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
