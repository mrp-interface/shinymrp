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
      age = c(0, 18, 35, 65, 75)
    ),
    poll = list(
      age = c(18, 30, 40, 50, 60, 70)
    )
  ),
  levels = list(
    covid = list(
      sex = c("male", "female"),
      race = c("white", "black", "other"),
      age = c("0-17", "18-34", "35-64", "65-74", "75+")
    ),
    poll = list(
      sex = c("male", "female"),
      race = c("white", "black", "other"),
      age = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"),
      edu = c("no hs", "hs", "some college", "4-year college", "post-grad")
    )
  ),
  expected_columns = list(
    covid = c("sex", "race", "age", "zip", "time", "date", "total", "positive"),
    poll = c("sex", "race", "age", "edu", "state", "total", "positive")
  ),
  ui = list(
    preview_size = 100,
    max_model = 5,
    iter_range = c(100, 5000),
    chain_range = c(1, 8),
    plot_height = 500,
    subplot_height = 300
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


