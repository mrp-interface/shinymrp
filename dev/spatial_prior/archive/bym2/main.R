source("dev/spatial_prior/simulation.R")
source("dev/spatial_prior/adjacency.R")
source("dev/spatial_prior/sf_io.R")
source("R/fct_model.R")

library(sf)
library(spdep)
library(cmdstanr)
library(bayesplot)

seed <- sample(1e6, 1)

path <- "dev/spatial_prior/"
zcta_sf <- read_sf_normalized(paste0(path, "zcta_2020_cb.gpkg"), layer = "zcta_2020_cb")
xwalk <- readr::read_csv(paste0(path, "zip_zcta_2020.csv"), show_col_types = FALSE)

stan_path <- paste0(path, "with_spatial_prior_multicomp.stan")
run_sim(zcta_sf, xwalk, stan_path, step = 1, seed = seed)