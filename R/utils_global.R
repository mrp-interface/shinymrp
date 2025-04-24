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
  bounds = list(
    covid = list(
      poverty = c(0, 1, 2)
    )
  ),
  levels = list(
    temporal_covid = list(
      sex = c("male", "female"),
      race = c("white", "black", "other"),
      age = c("0-17", "18-34", "35-64", "65-74", "75+"),
      time = NULL
    ),
    temporal_other = list(
      sex = c("male", "female"),
      race = c("white", "black", "other"),
      age = c("0-17", "18-34", "35-64", "65-74", "75+"),
      time = NULL
    ),
    static_poll = list(
      sex = c("male", "female"),
      race = c("white", "black", "other"),
      age = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"),
      edu = c("no hs", "hs", "some college", "4-year college", "post-grad")
    ),
    static_other = list(
      sex = c("male", "female"),
      race = c("white", "black", "other"),
      age = c("0-17", "18-34", "35-64", "65-74", "75+")
    )
  ),
  expected_types = list(
    indiv = list(
      temporal_covid = list(
        sex = "bin",
        race = "cat",
        age = "cat",
        zip = "cat",
        positive = "ignore"
      ),
      temporal_other = list(
        sex = "bin",
        race = "cat",
        age = "cat",
        positive = "ignore"
      ),
      static_poll = list(
        sex = "bin",
        race = "cat",
        age = "cat",
        edu = "cat",
        positive = "ignore"
      ),
      static_other = list(
        sex = "bin",
        race = "cat",
        age = "cat",
        positive = "ignore"
      )
    ),
    agg = list(
      temporal_covid = list(
        sex = "bin",
        race = "cat",
        age = "cat",
        zip = "cat",
        time = "cat",
        total = "ignore",
        positive = "ignore"
      ),
      temporal_other = list(
        sex = "bin",
        race = "cat",
        age = "cat",
        time = "cat",
        total = "ignore",
        positive = "ignore"
      ),
      static_poll = list(
        sex = "bin",
        race = "cat",
        age = "cat",
        edu = "cat",
        total = "ignore",
        positive = "ignore"
      ),
      static_other = list(
        sex = "bin",
        race = "cat",
        age = "cat",
        total = "ignore",
        positive = "ignore"
      )
    )
  ),
  vars = list(
    subgroups = c("sex", "race", "age", "edu", "county", "state"),
    indiv = c("sex", "race", "age", "edu", "time"),
    geo = c("zip", "county", "state"),
    ignore = c("date", "total", "positive")
  ),
  ui = list(
    preview_size = 100,
    max_model = 5,
    iter_range = c(100, 5000),
    chain_range = c(1, 8),
    plot_height = 500,
    subplot_height = 300,
    map_height = 700,
    date_format = "%b%d\n%Y",
    animation = list(
      duration = 1000,
      delay = 100
    ),
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

# set file upload size limit
options(shiny.maxRequestSize = 200*1024^2)
