#' @title Global list of shiny module IDs
#' @noRd
#' @keywords internal
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

#' @title Global Constants
#' @description A global list containing paths, UI styles, and other constants used throughout the application.
#' @noRd
#' @keywords internal
GLOBAL <- list(
  path = list(
    example_data = "extdata/example/data/",
    example_fit = "extdata/example/fit/"
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
    format = list(
      date = "%b%d\n%Y",
      data = c(".csv", ".xlsx", ".sas7bdat")
    ),
    model = list(
      max_models = 5,
      iter_range = c(100, 5000),
      chain_range = c(1, 8)
    ),
    plot = list(
      plot_height = 550,
      subplot_height = 300,
      map_height = 700,
      point_size = 3.5,
      errorbar_size = 0.8,
      errorbar_width = 0,
      raw_color = "darkblue",
      yrep_color = "darkorange",
      mrp_color = "darkorange"
    ),
    animation = list(
      duration = 1000,
      delay = 100
    ),
    plot_selection = list(
      vis_main = list(
        binomial = c(
          "Individual Characteristics" = "indiv",
          "Geographic Characteristics" = "geo",
          "Positive Response Rate" = "outcome"
        ),
        normal = c(
          "Individual Characteristics" = "indiv",
          "Geographic Characteristics" = "geo",
          "Outcome Average" = "outcome"
        )
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
      outcome = c(
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
  family = c("binomial", "normal"),
  vars = list(
    pstrat = c("sex", "race", "age", "edu", "county", "state"),
    indiv  = c("sex", "race", "age", "edu", "time"),
    geo    = c("zip", "county", "state"),
    time   = c("time", "date"),
    ignore = c("date", "total", "positive", "outcome")
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
