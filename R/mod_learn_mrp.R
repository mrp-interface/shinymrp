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
#'
#' @importFrom shiny NS tagList withMathJax tags
mod_learn_mrp_ui <- function(id){
  ns <- NS(id)

  bslib::layout_columns(
    col_widths = c(-3, 6),
    tags$div(
      class = "learn_mrp",
      tags$div(class = "mb-4", 
        withMathJax("MRP has two key steps: (1) fit a multilevel model for the response with the adjustment variables based on the input data; and (2) poststratify using the population distribution of the adjustment variables, yielding prevalence estimates in the target population and subgroups.")
      ),
      
      tags$h3("MRP for cross-sectional data"),
      tags$div(class = "mb-3",
        withMathJax("We use cross-sectional data to refer to the dataset with measures collected at a specific time point that does not account for temporal variation in the modeling or poststratification adjustment.")
      ),
      tags$div(class = "mb-3",
        withMathJax("The model we use in the example data is described below. Let \\(y_i\\) be the binary indicator of the positive response (1/0) for individual \\(i\\). We consider a logistic regression and include varying intercepts for age, race/ethnicity, education, and state, where the variation for the state-varying intercepts is explained by the state-level predictors.")
      ),
      tags$div(class = "mb-3",
        withMathJax("\\begin{align} \\tag{1} \\label{mrp-1} \\textrm{Pr}(y_i = 1) = \\textrm{logit}^{-1}(\\alpha_{\\rm s[i]}^{\\rm state} + \\alpha_{\\rm a[i]}^{\\rm age} + \\alpha_{\\rm r[i]}^{\\rm eth} + \\alpha_{\\rm e[i]}^{\\rm educ} + \\beta^{\\rm sex} \\cdot {\\rm sex}_{\\rm i}), \\end{align}")
      ),
      withMathJax("where:"),
      tags$div(class = "mb-2",
        withMathJax("* \\(\\alpha_{\\rm a}^{\\rm age}\\) is the effect of subject \\(i\\)'s age on the log-odds function of the probability of having a positive response.")
      ),
      tags$div(class = "mb-2",
        withMathJax("* \\(\\alpha_{\\rm r}^{\\rm eth}\\) is the effect of subject \\(i\\)'s race/ethnicity on the probability of having a positive response.")
      ),
      tags$div(class = "mb-2",
        withMathJax("* \\(\\alpha_{\\rm e}^{\\rm educ}\\) is the effect of subject \\(i\\)'s education on the probability of having a positive response.")
      ),
      tags$div(class = "mb-2",
        withMathJax("* \\(\\alpha_{\\rm s}^{\\rm state}\\): The effect of subject \\(i\\)'s state on the probability of having a positive response. As we have state-level predictors, we need to build another model in which \\(\\alpha_{\\rm s}^{\\rm state}\\) is the outcome of a linear regression with state-level predictors. For state \\(s\\),")
      ),
      tags$div(class = "mb-2",
        withMathJax("$$\\alpha_{\\rm s}^{\\rm state} =\\vec{\\alpha}\\vec{Z}^{\\rm state}_{s} +  e_s,$$")
      ),
      tags$div(class = "mb-3",
        withMathJax("with \\(e_s\\) as the random error.")
      ),
      tags$div(class = "mb-2",
        withMathJax("In the Bayesian framework we assign hierarchical priors to varying intercepts \\(\\alpha^{\\rm name}\\) or error terms \\(e_s\\):")
      ),
      tags$div(class = "mb-2",
        withMathJax("$$\\alpha^{\\rm name} \\sim normal(0,\\sigma^{\\rm name} )\\mbox{,  } \\sigma^{\\rm name}\\sim normal_+ (a,b),$$")
      ),
      tags$div(class = "mb-3",
        withMathJax("for \\(name\\in \\{\\rm age, race\\}\\). Here, \\(normal_+ (a,b)\\) represents a half-normal distribution with the mean \\(a\\) and standard deviation \\(b\\) restricted to positive values, with pre-specified values of \\((a,b)\\).")
      ),
      tags$div(class = "mb-2",
        withMathJax("To generalize results from this model to a national or subgroup estimate, we obtain the poststratification cells in the contigency table of sex, age, race/ethnicity, education, and state and weight the model predictions by the population cell frequency \\(N_j\\)'s. Suppose the cell-wise estimate based on model \\eqref{mrp-1} is \\(\\theta_j\\) in cell \\(j\\), the MRP estimate can be expressed as:")
      ),
      tags$div(class = "mb-2",
        withMathJax("$$\\theta^{MRP} = \\frac{\\sum N_j \\theta_j}{\\sum N_j}.$$")
      ),
      tags$div(class = "mb-2",
        withMathJax("Small area estimation is one of the main applications of MRP, and the MRP estimator for state s is")
      ),
      tags$div(class = "mb-3",
        withMathJax("$$\\theta_s^{MRP} = \\frac{\\sum_{j \\in s} N_j \\theta_j}{\\sum_{j \\in s} N_j}.$$")
      ),
      tags$div(class = "mb-4",
        withMathJax("We obtain inference based on Bayesian posterior predictive samples of the estimates.")
      ),
      
      tags$h3("MRP for time-varying data with measurement error"),
      tags$div(class = "mb-3",
        withMathJax("We use a Bayesian framework to account for the PCR testing sensitivity and specificity and apply MRP to COVID testing records for population representation, here using the following adjustment variables: the biological variable of sex, age, race, and zip codes.")
      ),
      tags$div(class = "mb-3",
        withMathJax("We denote the PCR test result for individual \\(i\\) as \\(y_i\\), where \\(y_i=1\\) indicating a positive result and \\(y_i=0\\) indicating negative. With poststratification cells, we can directly model cell-wise summaries. We can obtain aggregated counts as the number of tests \\(n_k\\) and the number of positive cases \\(y^*_k\\) in group \\(k\\), defined as a cell \\(k\\) in the cross-tabulation of sex, age, race, zip code and indicators of time in weeks, months, or years based on the test result dates. We assume that individuals in the same group have the same probability of being infected.")
      ),
      tags$div(class = "mb-2",
        withMathJax("Let \\(p_k=\\textrm{Pr}(y_{k[i]}=1)\\) be the probability that person \\(i\\) in group \\(k\\) tests positive. The analytic incidence \\(p_k\\) is a function of the test sensitivity \\(\\delta\\), specificity \\(\\gamma\\), and the true incidence \\(\\pi_k\\) for individuals in group \\(k\\):")
      ),
      tags$div(class = "mb-2",
        withMathJax("$$p_k=(1-\\gamma)(1-\\pi_k )+\\delta \\pi_k.$$")
      ),
      tags$div(class = "mb-3",
        withMathJax("We will start by fitting a Binomial model for \\(y^*_k\\), \\(y^*_k \\sim \\textrm{Binomial}(n_k, p_k)\\) with a logit function for \\(\\pi_k\\) with covariates including sex, age, race, zip codes, and time indices to allow time variation of prevalence over time in the multilevel model.")
      ),
      tags$div(class = "mb-2",
        withMathJax("\\begin{align} \\tag{2} \\label{pi} \\textrm{logit}(\\pi_k)=\\beta_1+\\beta_2male_k+\\alpha_{age[k]}^{\\rm age}+\\alpha_{race[k]}^{\\rm race}+\\alpha_{zip[k]}^{\\rm zip}+\\alpha_{time[k]}^{\\rm time},\\end{align}")
      ),
      tags$div(class = "mb-2",
        withMathJax("where \\(male_k\\) is an indicator for men; age[k], race[k], and zip[k] represent age, race, and zip code levels; and time[k] indices the time in weeks, months, or years when the test result is collected for group \\(k\\). We include zip code level covariates \\(\\vec{Z}^{zip}_{j}\\) for zip code \\(j\\),")
      ),
      tags$div(class = "mb-2",
        withMathJax("$$\\alpha_{j}^{\\rm zip} =\\vec{\\alpha}\\vec{Z}^{\\rm zip}_{j} +  e_j.$$")
      ),
      tags$div(class = "mb-3",
        withMathJax("Here \\(e_j\\) denotes the zip code level error term, which can follow a normal distribution (current setting) or a spatial distribution to capture the geospatial dependency (e.g., the conditional autoregressive model).")
      ),
      tags$div(class = "mb-3",
        withMathJax("According to the test protocol, the sensitivity is unknown, and the specificity is around 100%. We solicit prior information from previous testing results and try different values of the hyper-parameters for sensitivity analysis. The current setting fixes the sensitivity at 0.7 and specificity at 1.")
      ),
      tags$div(class = "mb-2",
        withMathJax("Using the estimated incidence \\(\\hat{\\pi}_k\\) based on the Bayesian model in \\eqref{pi}, we adjust for selection bias by applying the socio-demographic distributions in the community to generate the population-level prevalence estimates, as the poststratification step in MRP. For each cell in the cross-tabulation table of sex, age, race, and zip code (40 levels), we have the cell-wise incidence estimate \\(\\hat{\\pi}_c\\) and population count \\(N_c\\), where \\(c\\) is the cell index, and calculate the temporal prevalence estimate in the population,")
      ),
      tags$div(class = "mb-2",
        withMathJax("$$\\hat{pi}_{avg} = \\sum_c N_c\\hat{\\pi}_c/\\sum_c N_c,$$")
      ),
      tags$div(class = "mb-3",
        withMathJax("which can be restricted to subdomains of interest, as another property of MRP to yield robust estimates for small areas, e.g., on the county level.")
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
#'
#' @importFrom shiny moduleServer
mod_learn_mrp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
