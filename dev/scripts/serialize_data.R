source("R/fct_data.R")

library(magrittr)
library(dplyr)

path <- "/Users/tntoan/Desktop/repos/shinymrp-data/geo/"


# FIPS codes for counties and states
fips_ <- list(
    county = readr::read_csv(paste0(path, "fips_county.csv"), show_col_types = FALSE) %>% clean_chr(),
    state  = readr::read_csv(paste0(path, "fips_state.csv"), show_col_types = FALSE) %>% clean_chr()
)

# geojson files for counties and states
geojson_ <- list(
    county = qs2::qs_read(paste0(path, "geojson_county.qs2")),
    state  = qs2::qs_read(paste0(path, "geojson_state.qs2"))
)


# Serialize all data objects
usethis::use_data(
    fips_,
    geojson_,
    internal = TRUE,
    overwrite = TRUE
)