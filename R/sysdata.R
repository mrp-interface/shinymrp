# R/internal_data.R

#’ Internal package datasets
#’
#’ These objects are stored in **R/sysdata.rda** (via
#’ `use_data(..., internal = TRUE)`), and are used
#’ by the package’s functions at runtime:
#’
#’ - **`zip_`**:  A list of lookup tables mapping ZIP codes to census tracts, counties, and states.
#’ - **`fips_`**: A list of lookup tables mapping county and state names to FIPS codes.
#’ - **`geojson_`**: A list of geojson objects for counties and states.
#’ - **`acs_covid_`**: A list containing the poststratification table and covariates for COVID-19 data.
#’ - **`acs_poll_`**: A list containing the poststratification table for polling data.
#’ - **`acs_`**: A list containing the ACS data for the years 2019-2023.
#’
#’ @name internal_data
#’ @docType data
#’ @keywords internal
NULL

# Suppress R CMD check “no visible binding for global variable” notes
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
        "zip_",
        "fips_",
        "geojson_",
        "acs_covid_",
        "acs_poll_",
        "acs_"
    )
  )
}
