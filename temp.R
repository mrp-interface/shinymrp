devtools::load_all("/Users/tntoan/Desktop/repos/shinymrp")
source("/Users/tntoan/Desktop/repos/shinymrp/R/workflow.R")
source("/Users/tntoan/Desktop/repos/shinymrp/R/model.R")
df <- readr::read_csv("/Users/tntoan/Desktop/repos/shinymrp/inst/extdata/example/data/timevarying_binomial_prep.csv", show_col_types = FALSE)
pstrat <- readr::read_csv("/Users/tntoan/Desktop/repos/shinymrp/inst/extdata/example/data/pstrat.csv", show_col_types = FALSE)

workflow <- mrp_workflow()

workflow$preprocess(df, is_timevar = TRUE, is_aggregated = TRUE, special_case = NULL, family = "binomial")
workflow$link_acs(link_geo = "zip", acs_year = 2021)
# workflow$load_pstrat(pstrat, is_aggregated = TRUE)


# Use visualization methods
# demo_plot <- workflow$demo_bars(demo = "sex", file = "/Users/tntoan/Downloads/demo_plot.png")
# covar_plot <- workflow$covar_hist(covar = "income", file = "/Users/tntoan/Downloads/covar_plot.png")
# outcome_plot <- workflow$outcome_plot(file = "/Users/tntoan/Downloads/outcome_plot.png")
# workflow$sample_size_map(file = "/Users/tntoan/Downloads/sample_size_map.html")
# workflow$outcome_map(summary_type = "max", file = "/Users/tntoan/Downloads/outcome_map.html")

model <- workflow$create_model(
  effects = list(
    Intercept = list(
      Intercept = NULL
    ),
    fixed = list(
      sex = NULL,
      race = NULL
    ),
    varying = list(
      age = NULL,
      time = NULL,
      zip = NULL,
      county = NULL
    ),
    interaction = list(
      `race:time` = "structured",
      `sex:time` = "structured",
      `sex:race` = "structured"
    )
  )
)

# model$fit(n_iter = 1000, n_chains = 2, seed = 123)
# workflow$pp_check(model, file = "/Users/tntoan/Downloads/pp_check.png")
# workflow$estimate_plot(model, "age", file = "/Users/tntoan/Downloads/estimate_plot.png")
# workflow$estimate_map(model, time_index = 10, file = "/Users/tntoan/Downloads/estimate_map.html")

