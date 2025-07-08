source("R/fct_data.R")

library(magrittr)
library(dplyr)

path <- "inst/extdata/"

# lookup tables for ZIP codes to census tracts, counbties, and states
zip_ <- list(
    tract = readr::read_csv(paste0(path, "zip_tract.csv"), show_col_types = FALSE, col_types = readr::cols(.default = "c")),
    county_state = readr::read_csv(paste0(path, "zip_county_state.csv"), show_col_types = FALSE) %>% clean_chr()
)
usethis::use_data(zip_, internal = TRUE, overwrite = TRUE)

# FIPS codes for counties and states
fips_ <- list(
    county = readr::read_csv(paste0(path, "fips_county.csv"), show_col_types = FALSE) %>% clean_chr(),
    state  = readr::read_csv(paste0(path, "fips_state.csv"), show_col_types = FALSE) %>% clean_chr()
)
usethis::use_data(fips_, internal = TRUE, overwrite = TRUE)

# geojson files for counties and states
geojson_ <- list(
    county = qs::qread(paste0(path, "geojson_county.RDS")),
    state  = qs::qread(paste0(path, "geojson_state.RDS"))
)
usethis::use_data(geojson_, internal = TRUE, overwrite = TRUE)

# post-stratification table for covid data
acs_covid_ <- list(
    pstrat = readr::read_csv(paste0(path, "acs/pstrat_covid.csv"), show_col_types = FALSE),
    covar  = readr::read_csv(paste0(path, "acs/covar_covid.csv"), show_col_types = FALSE)
)
usethis::use_data(acs_covid_, internal = TRUE, overwrite = TRUE)

# post-stratification table for polling data
acs_poll_ <- list(
    pstrat = readr::read_csv(paste0(path, "acs/pstrat_poll.csv"), show_col_types = FALSE)
)
usethis::use_data(acs_poll_, internal = TRUE, overwrite = TRUE)

# post-stratification table for general cases
acs_years <- 2019:2023
acs_ <- purrr::map(acs_years, function(year) {
    file_path <- paste0(path, "acs/acs_", year - 4, "-", year, ".csv")
    readr::read_csv(file_path, show_col_types = FALSE)
}) %>%
    setNames(acs_years)
usethis::use_data(acs_, internal = TRUE, overwrite = TRUE)