# ===============================================================
# EXAMPLE USAGE
# ===============================================================
# zips <- c("48103","48104","48105","48108","48109",
#           "48116", "48118", "48130", "48137", "48158",
#           "48169", "48170", "48176", "48197", "48198")
zips <- c("48118", "48130", "48137", "48189", "48178",
          "48165", "48167", "48168", "48169", "48170", 
          "48184", "48185", "48186",  "48187", "48188")
zip_graph <- .build_graph(geo_units = zips, geo_scale = "zip", verbose = TRUE)

sim <- simulate_stan_data_icar(
  stan_graph = zip_graph$stan_graph,
  zip_labels = zips,
  intercept_true = -2.0,
  beta_true      = c(sex_male = log(1.25)),  # K=1: effect for male vs female (X=1 if male, 0 if female)
  lambda_race_true = 0.0,
  lambda_age_true  = 0.0,
  lambda_time_true = 0.0,
  lambda_zip_true  = 1.2,
  icar_mode = "lowpass",
  n_sample_per_cell = 10,
  within_zip_coverage = 1.0,
  holdout_zip_frac = 0.25
)

readr::write_csv(sim$raw, "/Users/tntoan/Downloads/simulated_data_lowpass.csv")
# icar_validate_report(zip_graph$stan_graph, sim$truth$z_zip)

# res <- fit_brms_car_vs_iid(sim, car_type = "bym2", iter = 2000, chains = 4, cores = 4)