#' learn_mrp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_mrp_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6, offset = 3,
        tags$div(
          class = "learn_mrp",
          tags$div(class = "learn_math", withMathJax("MRP has two key steps: (1) fit a multilevel model for the response with the adjustment variables based on the input data; and (2) poststratify using the population distribution of the adjustment variables, yielding estimates in the target population and subgroups.")),
          tags$h3("MRP for cross-sectional data"),
          tags$div(class = "learn_math", class = "learn_math", withMathJax("We use cross-sectional data to refer to the dataset with measures collected at a specific time point that does not account for temporal variation in the modeling or poststratification adjustment.")),
          tags$div(class = "learn_math", withMathJax("The model we use in the example data is described below. Let \\(y_i\\) be the binary indicator of the positive response (1/0) for individual \\(i\\). We consider a logistic regression and include varying intercepts for age, race/ethnicity, education, and state, where the variation for the state-varying intercepts is explained by the state-level predictors.")),
          tags$div(class = "learn_math", withMathJax("\\begin{align} \\tag{1} \\label{mrp-1} \\textrm{Pr}(y_i = 1) = \\textrm{logit}^{-1}(\\alpha_{\\rm s[i]}^{\\rm state} + \\alpha_{\\rm a[i]}^{\\rm age} + \\alpha_{\\rm r[i]}^{\\rm eth} + \\alpha_{\\rm e[i]}^{\\rm educ} + \\beta^{\\rm sex} \\cdot {\\rm sex}_{\\rm i}), \\end{align}")),
          tags$div(class = "learn_math", withMathJax("where:")),
          tags$div(class = "learn_math", withMathJax("* \\(\\alpha_{\\rm a}^{\\rm age}\\) is the effect of subject \\(i\\)'s age on the log-odds function of the probability of having a positive response.")),
          tags$div(class = "learn_math", withMathJax("* \\(\\alpha_{\\rm r}^{\\rm eth}\\) is the effect of subject \\(i\\)'s race/ethnicity on the probability of having a positive response.")),
          tags$div(class = "learn_math", withMathJax("* \\(\\alpha_{\\rm e}^{\\rm educ}\\) is the effect of subject \\(i\\)'s education on the probability of having a positive response.")),
          tags$div(class = "learn_math", withMathJax("* \\(\\alpha_{\\rm s}^{\\rm state}\\): The effect of subject \\(i\\)'s state on the probability of having a positive response. As we have state-level predictors, we need to build another model in which \\(\\alpha_{\\rm s}^{\\rm state}\\) is the outcome of a linear regression with state-level predictors. For state \\(s\\),")),
          tags$div(class = "learn_math", withMathJax("$$\\alpha_{\\rm s}^{\\rm state} =\\vec{\\alpha}\\vec{Z}^{\\rm state}_{s} +  e_s,$$")),
          tags$div(class = "learn_math", withMathJax("with \\(e_s\\) as the random error.")),
          tags$div(class = "learn_math", withMathJax("In the Bayesian framework we assign hierarchical priors to varying intercepts \\(\\alpha^{\\rm name}\\) or error terms \\(e_s\\):")),
          tags$div(class = "learn_math", withMathJax("$$\\alpha^{\\rm name} \\sim normal(0,\\sigma^{\\rm name} )\\mbox{,  } \\sigma^{\\rm name}\\sim normal_+ (a,b),$$")),
          tags$div(class = "learn_math", withMathJax("for \\(name\\in \\{\\rm age, race\\}\\). Here, \\(normal_+ (a,b)\\) represents a half-normal distribution with the mean \\(a\\) and standard deviation \\(b\\) restricted to positive values, with pre-specified values of \\((a,b)\\).")),
          tags$div(class = "learn_math", withMathJax("To generalize results from this model to a national or subgroup estimate, we obtain the poststratification cells in the contigency table of sex, age, race/ethnicity, education, and state and weight the model predictions by the population cell frequency \\(N_j\\)'s. Suppose the cell-wise estimate based on model \\eqref{mrp-1} is \\(\\theta_j\\) in cell \\(j\\), the MRP estimate can be expressed as:")),
          tags$div(class = "learn_math", withMathJax("$$\\theta^{MRP} = \\frac{\\sum N_j \\theta_j}{\\sum N_j}.$$")),
          tags$div(class = "learn_math", withMathJax("Small area estimation is one of the main applications of MRP, and the MRP estimator for state s is")),
          tags$div(class = "learn_math", withMathJax("$$\\theta_s^{MRP} = \\frac{\\sum_{j \\in s} N_j \\theta_j}{\\sum_{j \\in s} N_j}.$$")),
          tags$div(class = "learn_math", withMathJax("We obtain inference based on Bayesian posterior predictive samples of the estimates.")),
          tags$h3("MRP for spatio-temporal data with measurement error"),
          tags$div(class = "learn_math", withMathJax("We use a Bayesian framework to account for the PCR testing sensitivity and specificity and apply MRP to COVID testing records for population representation, here using the following adjustment variables: the biological variable of sex, age, race, and zip codes.")),
          tags$div(class = "learn_math", withMathJax("We denote the PCR test result for individual \\(i\\) as \\(y_i\\), where \\(y_i=1\\) indicating a positive result and \\(y_i=0\\) indicating negative. With poststratification cells, we can directly model cell-wise summaries. We can obtain aggregated counts as the number of tests \\(n_k\\) and the number of positive cases \\(y^*_k\\) in group \\(k\\), defined as a cell \\(k\\) in the cross-tabulation of sex, age, race, zip code and indicators of time in weeks based on the test result dates. We assume that individuals in the same group have the same probability of being infected.")),
          tags$div(class = "learn_math", withMathJax("Let \\(p_k=\\textrm{Pr}(y_{k[i]}=1)\\) be the probability that person \\(i\\) in group \\(k\\) tests positive. The analytic incidence \\(p_k\\) is a function of the test sensitivity \\(\\delta\\), specificity \\(\\gamma\\), and the true incidence \\(\\pi_k\\) for individuals in group \\(k\\):")),
          tags$div(class = "learn_math", withMathJax("$$p_k=(1-\\gamma)(1-\\pi_k )+\\delta \\pi_k.$$")),
          tags$div(class = "learn_math", withMathJax("We will start by fitting a Binomial model for \\(y^*_k\\), \\(y^*_k \\sim \\textrm{Binomial}(n_k, p_k)\\) with a logit function for \\(\\pi_k\\) with covariates including sex, age, race, zip codes, and time in weeks to allow time variation of positive response rate over time in the multilevel model.")),
          tags$div(class = "learn_math", withMathJax("\\begin{align} \\tag{2} \\label{pi} \\textrm{logit}(\\pi_k)=\\beta_1+\\beta_2male_k+\\alpha_{age[k]}^{\\rm age}+\\alpha_{race[k]}^{\\rm race}+\\alpha_{zip[k]}^{\\rm zip}+\\alpha_{time[k]}^{\\rm time},\\end{align}")),
          tags$div(class = "learn_math", withMathJax("where \\(male_k\\) is an indicator for men; age[k], race[k], and zip[k] represent age, race, and zip code levels; and time[k] indices the time in weeks when the test result is collected for group \\(k\\). We include zip code level covariates \\(\\vec{Z}^{zip}_{j}\\) for zip code \\(j\\),")),
          tags$div(class = "learn_math", withMathJax("$$\\alpha_{j}^{\\rm zip} =\\vec{\\alpha}\\vec{Z}^{\\rm zip}_{j} +  e_j.$$")),
          tags$div(class = "learn_math", withMathJax("Here \\(e_j\\) denotes the zip code level error term, which can follow a normal distribution (current setting) or a spatial distribution to capture the geospatial dependency (e.g., the conditional autoregressive model).")),
          tags$div(class = "learn_math", withMathJax("According to the test protocol, the sensitivity is unknown, and the specificity is around 100%. We solicit prior information from previous testing results and try different values of the hyper-parameters for sensitivity analysis. The current setting fixes the sensitivity at 0.7 and specificity at 1.")),
          tags$div(class = "learn_math", withMathJax("Using the estimated incidence \\(\\hat{\\pi}_k\\) based on the Bayesian model in \\eqref{pi}, we adjust for selection bias by applying the socio-demographic distributions in the community to generate the population-level estimates, as the poststratification step in MRP. For each cell in the cross-tabulation table of sex, age, race, and zip code (40 levels), we have the cell-wise incidence estimate \\(\\hat{\\pi}_c\\) and population count \\(N_c\\), where \\(c\\) is the cell index, and calculate the estimate of weekly positive response rate in the population,")),
          tags$div(class = "learn_math", withMathJax("$$\\hat{pi}_{avg} = \\sum_c N_c\\hat{\\pi}_c/\\sum_c N_c,$$")),
          tags$div(class = "learn_math", withMathJax("which can be restricted to subdomains of interest, as another property of MRP to yield robust estimates for small areas, e.g., on the county level."))
        )
      )
    )
  )
}

#' learn_mrp Server Functions
#'
#' @noRd
mod_learn_mrp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
