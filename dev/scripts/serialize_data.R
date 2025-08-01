source("R/fct_data.R")

library(magrittr)
library(dplyr)

path <- "/Users/tntoan/Desktop/MRP/Data/shinymrp/"
alt_path <- "/Users/tntoan/Desktop/MRP/Data/mrp_workflow_paper/"

# lookup tables for ZIP codes to census tracts, counbties, and states
zip_ <- list(
    tract = readr::read_csv(paste0(alt_path, "zip_tract.csv"), show_col_types = FALSE, col_types = readr::cols(.default = "c")),
    county_state = readr::read_csv(paste0(alt_path, "zip_county_state.csv"), show_col_types = FALSE) %>% clean_chr()
)

# FIPS codes for counties and states
fips_ <- list(
    county = readr::read_csv(paste0(path, "fips_county.csv"), show_col_types = FALSE) %>% clean_chr(),
    state  = readr::read_csv(paste0(path, "fips_state.csv"), show_col_types = FALSE) %>% clean_chr()
)

# geojson files for counties and states
geojson_ <- list(
    county = qs::qread(paste0(path, "geojson_county.RDS")),
    state  = qs::qread(paste0(path, "geojson_state.RDS"))
)

# poststratification table for covid data
acs_covid_ <- list(
    pstrat = readr::read_csv(paste0(alt_path, "pstrat_covid.csv"), show_col_types = FALSE),
    covar  = readr::read_csv(paste0(alt_path, "covar_covid.csv"), show_col_types = FALSE)
)

# poststratification table for polling data
acs_poll_ <- list(
    pstrat = readr::read_csv(paste0(path, "acs/pstrat_poll.csv"), show_col_types = FALSE)
)

# poststratification table for general cases
acs_years <- 2019:2023
acs_ <- purrr::map(acs_years, function(year) {
    file_path <- paste0(path, "acs/acs_", year - 4, "-", year, ".csv")
    readr::read_csv(file_path, show_col_types = FALSE)
}) %>%
    setNames(acs_years)

# Serialize all data objects
usethis::use_data(
    zip_,
    fips_,
    geojson_,
    acs_covid_,
    acs_poll_,
    acs_,
    internal = TRUE,
    overwrite = TRUE
)