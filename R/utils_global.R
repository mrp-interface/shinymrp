#' global
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

module_ids <- list(
  home = "home",
  analyze = list(
    upload = "analyze_upload",
    visualize = "analyze_visualize",
    model = "analyze_model",
    result = "analyze_result"
  ),
  learn = list(
    interface = "learn_interface",
    preprocess = "learn_preprocess",
    mrp = "learn_mrp"
  ),
  about = "about",
  persist = "persist"
)

GLOBAL <- list(
  vars = list(
    pstrat = c("sex", "race", "age", "edu", "county", "state"),
    indiv  = c("sex", "race", "age", "edu", "time"),
    geo    = c("zip", "county", "state"),
    time   = c("time", "date"),
    ignore = c("date", "total", "positive")
  ),
  ui = list(
    style = list(
      global = "
        .navbar-brand {
          font-size: 1.5rem;
        }
        
        .navbar-nav .nav-link {
          font-size: 1.04rem;
        }
        
        .nav-item .nav-link {
          font-size: 1.0rem;
        }"
    ),
    preview_size = 100,
    max_model = 5,
    iter_range = c(100, 5000),
    chain_range = c(1, 8),
    plot_height = 550,
    subplot_height = 300,
    map_height = 700,
    date_format = "%b%d\n%Y",
    animation = list(
      duration = 1000,
      delay = 100
    ),
    data_accept = c(".csv", ".xlsx", ".sas7bdat"),
    plot_selection = list(
      vis_main = c(
        "Individual Characteristics" = "indiv",
        "Geographic Characteristics" = "geo",
        "Positive Response Rate" = "pos_rate"
      ),
      indiv = c(
        "Sex" = "sex",
        "Race" = "race",
        "Age" = "age",
        "Education" = "edu"
      ),
      geo = c(
        "Sample Size" = "sample"
      ),
      geo_covar = c(
        "Education" = "edu",
        "Poverty" = "poverty",
        "Employment" = "employ", 
        "Income" = "income",
        "Urbanicity" = "urban",
        "ADI" = "adi"
      ),
      pos_rate = c(
        "Overall" = "overall",
        "By Geography" = "by_geo"
      ),
      subgroup = c(
        "Sex" = "sex",
        "Race" = "race",
        "Age" = "age",
        "Education" = "edu",
        "Geography" = "geo"
      )
    ),
    use_case_labels = list(
      covid = "Time-varying: COVID",
      poll = "Cross-sectional: Poll",
      timevar_general = "Time-varying: General",
      static_general = "Cross-sectional: General"
    )
  ),
  default_priors = list(
    Intercept = "normal(0, 5)",
    fixed = "normal(0, 3)",
    varying = "normal(0, 3)",
    interaction = "normal(0, 1)",
    global_scale = "cauchy(0 , 1)",
    local_scale = "normal(0, 1)"
  )
)
