#' MRP Theory Guide Module UI Function
#'
#' @description Creates the user interface for the MRP (Multilevel Regression
#' and Poststratification) theory and methodology guide page. Provides detailed
#' mathematical explanations of MRP for both cross-sectional and time-varying
#' data, including model specifications, Bayesian frameworks, and measurement
#' error correction for COVID-19 testing data.
#'
#' @param id Character string. The module's namespace identifier.
#'
#' @return A `bslib::layout_columns` containing the MRP theory guide with:
#' \itemize{
#'   \item Mathematical formulations for cross-sectional MRP models
#'   \item Time-varying MRP models with measurement error correction
#'   \item Bayesian hierarchical model specifications
#'   \item Poststratification procedures and small area estimation
#' }
#'
#' @noRd
#' @keywords internal
#'
#' @importFrom shiny NS tagList withMathJax tags
mod_learn_mrp_ui <- function(id){
  ns <- NS(id)

  bslib::layout_columns(
    col_widths = c(-3, 6),
    tags$div(
      tags$hr(class = "header_top"),
      tags$h3("MRP Methodological Guide"),
      tags$hr(class = "header_bottom"),
      tags$div(class = "mb-4", 
        withMathJax("MRP has two key steps: (1) fit a multilevel model for the response with the adjustment variables based on the input data; and (2) poststratify using the population distribution of the adjustment variables, yielding prevalence estimates in the target population and subgroups.")
      ),
      
      tags$h4("MRP for cross-sectional data"),
      tags$div(class = "mb-3",
        withMathJax("We use cross-sectional data to refer to the dataset with measures collected at a specific time point that does not account for temporal variation in the modeling or poststratification adjustment. We use a binary outcome of interest as an example. Let \\(y_i (=0/1)\\) be the binary response for individual \\(i\\), with \\(y_i=1\\) indicating the positive response. We employ a logistic regression with varying effects for age, race, and ZIP code, where the ZIP-code-level variation is further explained by the ZIP-code-level predictors.")
      ),
      tags$div(class = "mb-3",
        withMathJax("\\begin{align} \\label{mrp-1} \\textrm{Pr}(y_i = 1) = \\textrm{logit}^{-1}( \\beta_1+\\beta_2{\\rm male}_i + \\alpha_{\\rm a[i]}^{\\rm age} + \\alpha_{\\rm r[i]}^{\\rm race} + \\alpha_{\\rm s[i]}^{\\rm ZIP} ), \\end{align}")
      ),
      tags$div(class = "mb-3",
        withMathJax("where \\({\\rm male}_i\\) is an indicator for men, \\(\\alpha_{\\rm a}^{\\rm age}\\) is the age effect, with a value of \\(a[i]\\) for subject \\(i\\), on the log-odds function of the probability of having a positive response, \\(\\alpha_{\\rm r}^{\\rm race}\\) is the racial effect, and \\(\\alpha_{\\rm s}^{\\rm ZIP}\\) is the ZIP-code-level effect. In the Bayesian framework, we assign hierarchical priors to varying intercepts as default:")
      ),
      tags$div(class = "mb-3",
        withMathJax("\\begin{align} \\label{prior} \\nonumber &\\alpha^{\\rm age} \\sim \\mbox{normal}(0,\\sigma^{\\rm age} ), \\,\\,\\, \\sigma^{\\rm age}\\sim \\mbox{normal}_+ (0,2.5)\\\\ &\\alpha^{\\rm race} \\sim \\mbox{normal}(0,\\sigma^{\\rm race} ), \\,\\,\\, \\sigma^{\\rm race}\\sim \\mbox{normal}_+ (0,2.5). \\end{align}")
      ),
      tags$div(class = "mb-3",
        withMathJax("Here \\(\\mbox{normal}_+ (0,2.5)\\) represents a half-normal distribution with the mean \\(0\\) and standard deviation \\(2.5\\) restricted to positive values. As we have ZIP-code-level predictors \\(\\vec{Z}^{\\rm ZIP}_{s}\\), we need to build another model in which \\(\\alpha_{\\rm s}^{\\rm ZIP}\\) is the outcome of a linear regression with ZIP-code-level predictors:")
      ),
      tags$div(class = "mb-3",
        withMathJax("\\begin{align} \\label{prior-zip} \\alpha_{\\rm s}^{\\rm ZIP} =\\vec{\\alpha}\\vec{Z}^{\\rm ZIP}_{s} +  e_s, \\,\\,\\, e_s\\sim \\mbox{normal}(0,\\sigma^{\\rm ZIP} ),\\,\\,\\, \\sigma^{\\rm ZIP}\\sim \\mbox{normal}_+ (0,2.5), \\end{align}")
      ),
      tags$div(class = "mb-3",
        withMathJax("where \\(e_s\\) is a ZIP-code-level random error.")
      ),
      
      tags$p("The interface allows users to specify alternative priors, including structured priors for high-order interaction terms developed by ", tags$a("Si et al. (2020)", href = "https://www150.statcan.gc.ca/n1/en/pub/12-001-x/2020002/article/00003-eng.pdf?st=iF1_Fbrh", target = "_blank"), "."),
      
      tags$div(class = "mb-3",
        withMathJax("Because the outcome model assumes that the people in the same poststratification cell share the same response probability, we can replace the microdata with cellwise aggregates and employ a binomial model for the sum of the responses in cell \\(j\\) as \\(y^*_j \\sim \\textrm{binomial}(n_j, \\theta_j)\\), where \\(n_j\\) is the sample cell size and \\(\\theta_j=\\textrm{logit}^{-1}( \\beta_1+\\beta_2{\\rm male}_j + \\alpha_{\\rm a[j]}^{\\rm age} + \\alpha_{\\rm r[j]}^{\\rm race} + \\alpha_{\\rm s[j]}^{\\rm ZIP} ) \\) using the cellwise effects of all factors. The interface thus allows users to upload microdata or cellwise aggregates as the input data.")
      ),
      
      tags$div(class = "mb-3",
        withMathJax("To generate overall population or subgroup estimates, we combine model predictions within the poststratification cells---in the contingency table of sex, age, race, and ZIP---weighted by the population cell frequencies \\(N_j\\), which are derived from the linked ACS data in our application. Additionally, users may choose to upload custom poststratification data for specific target populations (e.g., a different country, rather than the U.S.). If we write the expected outcome in cell \\(j\\) as \\(\\hat{\\theta}_j\\) in cell \\(j\\), the population average from MRP is then:")
      ),
      tags$div(class = "mb-3",
        withMathJax("$$\\hat{\\theta}^{\\rm pop} = \\frac{\\sum_j N_j \\hat{\\theta}_j}{\\sum_j N_j}.$$")
      ),
      tags$div(class = "mb-3",
        withMathJax("The MRP estimator for county \\(c\\) aggregates over covered cells \\(j\\) in that county as,")
      ),
      tags$div(class = "mb-3",
        withMathJax("$$\\hat{\\theta}_s^{\\rm pop} = \\frac{\\sum_{j \\in \\textrm{county c}} N_j \\hat{\\theta}_j}{\\sum_{j \\in \\textrm{county c}} N_j}.$$")
      ),
      tags$div(class = "mb-4",
        withMathJax("We implement Bayesian inference for the estimates, where the variance estimates and 95% credible intervals are computed based on the posterior samples.")
      ),
      tags$div(class = "mb-4",
        withMathJax("When the outcome is continuous, we specify linear regression models and estimate residual variance with introduced prior distributions.")
      ),
      
      tags$h4("MRP for time-varying data with measurement error", class = "mt-5"),
      tags$div(class = "mb-3",
        withMathJax("As an example of time-varying data, we model weekly PCR testing results. We use a Bayesian framework to account for the PCR testing sensitivity and specificity. Here, MRP proceeds in two steps: (1) fit a multilevel model to the testing data for incidence incorporating time and covariates, and (2) poststratify using the population distribution of the adjustment variables: sex, age, race, and ZIP codes, where we assume the population distribution is the same during the study period. Hence, the poststratification cell is defined by the cross-tabulation of sex, age, race, ZIP code, and indicators of time in weeks based on the test result dates.")
      ),
      tags$div(class = "mb-3",
        withMathJax("We denote the PCR test result for individual \\(i\\) as \\(y_i\\), where \\(y_i=1\\) indicates a positive result and \\(y_i=0\\) indicates negative. Similarly, with poststratification cells, we assume that people in the same cell have the same infection rate and can directly model cellwise summaries. We obtain aggregated counts as the number of tests \\(n_j\\) and the number of positive cases \\(y^*_j\\) in cell \\(j\\). Let \\(p_j=\\textrm{Pr}(y_{j[i]}=1)\\) be the probability that person \\(i\\) in cell \\(j\\) tests positive. We account for the PCR testing sensitivity and specificity, where the positivity \\(p_j\\) is a function of the test sensitivity \\(\\delta\\), specificity \\(\\gamma\\), and the true incidence \\(\\pi_j\\) for people in cell \\(j\\):")
      ),
      tags$div(class = "mb-3",
        withMathJax("\\begin{align} \\label{positivity} p_j=(1-\\gamma)(1-\\pi_j )+\\delta \\pi_j. \\end{align}")
      ),
      
      tags$div(class = "mb-3",
        withMathJax("We fit a binomial model for \\(y^*_j\\), \\(y^*_j \\sim \\textrm{binomial}(n_j, p_j)\\) with a logistic regression for \\(\\pi_j\\) with covariates---sex, age, race, ZIP codes, and time in weeks---to allow time-varying incidence in the multilevel model.")
      ),
      tags$div(class = "mb-3",
        withMathJax("\\begin{align} \\label{pi} \\textrm{logit}(\\pi_j)=\\beta_1+\\beta_2{\\rm male}_j+\\alpha_{{\\rm a}[j]}^{\\rm age}+\\alpha_{{\\rm r}[j]}^{\\rm race}+\\alpha_{{\\rm s}[j]}^{\\rm ZIP}+\\alpha_{{\\rm t}[j]}^{\\rm time}, \\end{align}")
      ),
      tags$div(class = "mb-3",
        withMathJax("where \\({\\rm male}_j\\) is an indicator for men; \\({\\rm a}[j]\\), \\({\\rm r}[j]\\), and \\({\\rm s}[j]\\) represent age, race, and ZIP levels; and \\({\\rm t}[j]\\) denotes the time in weeks when the test result is collected for cell \\(j\\). We include ZIP-code-level predictors \\(\\vec{Z}^{\\rm ZIP}_{s}\\) for ZIP code \\(s\\),")
      ),
      tags$div(class = "mb-3",
        withMathJax("\\[\\alpha_{s}^{\\rm ZIP} =\\vec{\\alpha}\\vec{Z}^{\\rm ZIP}_{s} +  e_s.\\]")
      ),
      tags$div(class = "mb-3",
        withMathJax("We assign the same priors to those in the cross-sectional case to varying intercepts and error terms \\(e_s\\).")
      ),
      tags$div(class = "mb-3",
        withMathJax("\\begin{align} \\nonumber &\\alpha^{\\rm age} \\sim \\mbox{normal}(0,\\sigma^{\\rm age} ), \\,\\,\\, \\sigma^{\\rm age}\\sim \\mbox{normal}_+ (0,2.5)\\\\ &\\alpha^{\\rm race} \\sim \\mbox{normal}(0,\\sigma^{\\rm race} ), \\,\\,\\, \\sigma^{\\rm race}\\sim \\mbox{normal}_+ (0,2.5).\\\\ \\alpha_{\\rm s}^{\\rm ZIP} &=\\vec{\\alpha}\\vec{Z}^{\\rm ZIP}_{s} +  e_s, \\,\\,\\, e_s\\sim \\mbox{normal}(0,\\sigma^{\\rm ZIP} ),\\,\\,\\, \\sigma^{\\rm ZIP}\\sim \\mbox{normal}_+ (0,2.5). \\end{align}")
      ),
      
      tags$div(class = "mb-3",
        withMathJax("As to time-varying effects, we assume \\(\\alpha_{{\\rm t}}^{\\rm time} \\sim \\mbox{normal}(0,\\sigma^{\\rm time} )\\), with a weakly informative hyperprior, \\(\\sigma^{\\rm time}\\sim \\mbox{normal}_+ (0,5)\\).")
      ),
      
      tags$div(class = "mb-3",
        withMathJax("As an example, we assign normal priors to the ZIP-code-level and time-varying effects. The interface leverages Stan's modeling capabilities to allow alternative prior choices and can be extended with advanced modeling.")
      ),
      
      tags$div(class = "mb-3",
        withMathJax("Using the estimated incidence \\(\\hat{\\pi}_j\\), we adjust for selection bias by applying the sociodemographic distributions in the community with population cell counts \\(N_j\\) based on the ACS, yielding population-level weekly incidence estimates:")
      ),
      tags$div(class = "mb-3",
        withMathJax("\\[\\hat{\\pi}_{t} = \\frac{\\sum_{j \\in \\mbox{week,} t} N_j\\hat{\\pi}_j}{\\sum_{j \\in \\mbox{week,} t} N_j}, \\]")
      ),
      tags$div(class = "mb-4",
        withMathJax("which can be restricted to specific subgroups or regions of interest, as another key property of MRP is to yield robust estimates for small groups. We obtain the Bayesian credible intervals from the posterior samples for inference.")
      ),
      
      tags$h4("More readings", class = "mt-5"),
      tags$ol(
        tags$li(tags$a("Y Si, T Tran, J Gabry, M Morris, and A Gelman (2025), Multilevel Regression and Poststratification Interface: Application to Track Community-level COVID-19 Viral Transmission, Population Health Metrics (under review)", href = "http://arxiv.org/abs/2405.05909", target = "_blank"), "."),
        tags$li(tags$a("Y Si (2025). On the Use of Auxiliary Variables in Multilevel Regression and Poststratification, Statistical Science, 40(2), 272--288", href = "http://dx.doi.org/10.1214/24-STS932", target = "_blank"), "."),
        tags$li(tags$a("Y Si, L Covello, S Wang, T Covello, and A Gelman (2022). Beyond Vaccination Rates: A Synthetic Random Proxy Metric of Total SARS-CoV-2 Immunity Seroprevalence in the Community, Epidemiology, 33(4), 457--464", href = "https://journals.lww.com/epidem/Fulltext/2022/07000/Beyond_Vaccination_Rates__A_Synthetic_Random_Proxy.3.aspx", target = "_blank"), "."),
        tags$li(tags$a("L Covello, A Gelman, Y Si, and S Wang (2021). Routine Hospital-Based SARS-CoV-2 Testing Outperforms State-Based Data in Predicting Clinical Burden, Epidemiology, 32(6), 792--799", href = "https://journals.lww.com/epidem/Fulltext/2021/11000/Routine_Hospital_based_SARS_CoV_2_Testing.4.aspx", target = "_blank"), "."),
        tags$li(tags$a("Y Si, R Trangucci, J Gabry, and A Gelman (2020). Bayesian Hierarchical Weighting Adjustment and Survey Inference, Survey Methodology, 46(2), 181--214", href = "https://www150.statcan.gc.ca/n1/en/pub/12-001-x/2020002/article/00003-eng.pdf?st=iF1_Fbrh", target = "_blank"), ".")
      )
    )
  )
}

#' MRP Theory Guide Module Server Function
#'
#' @description Server logic for the MRP theory guide module. Currently contains
#' minimal server-side functionality as the MRP theory page is primarily static
#' mathematical content. Maintains the module structure for potential future
#' enhancements such as interactive mathematical demonstrations or dynamic
#' content updates.
#'
#' @param id Character string. The module's namespace identifier.
#'
#' @return Server function for the MRP theory guide module. Currently provides
#' basic module server structure without active reactive functionality.
#'
#' @noRd
#' @keywords internal
#'
#' @importFrom shiny moduleServer
mod_learn_mrp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
