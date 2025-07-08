source("R/utils_global.R")
source("R/fct_data.R")

library(dplyr)
library(magrittr)

#' Create zip-to-county-state crosswalk
#'
#' @description Creates a crosswalk table linking ZIP codes to counties and
#' states by finding the most common county for each ZIP code and joining
#' with FIPS county-state information. Handles ZIP codes that span multiple
#' counties by selecting the most frequent county.
#'
#' @param fips_county_state Data frame containing FIPS county-state mappings
#'   with columns: fips, county, state, state_name.
#'
#' @return Data frame with ZIP codes linked to their most common county and state:
#' \itemize{
#'   \item zip: ZIP code
#'   \item fips: 5-digit county FIPS code
#'   \item county, state, state_name: Geographic names
#' }
#'
#' @noRd
#'
#' @importFrom dplyr mutate group_by summarise left_join
#' @importFrom rlang .data
create_zip_county_state <- function(zip_tract, fips_county_state) {
  # find the most common county for each zip code
  zip_tract %>%
    mutate(county_fips = substr(.data$geoid, 1, 5)) %>%
    group_by(.data$zip) %>%
    summarise(
      fips = names(which.max(table(.data$county_fips)))
    ) %>%
    left_join(
      fips_county_state,
      by = "fips"
    )
}

#' Create FIPS county-state lookup table
#'
#' @description Creates a lookup table of unique FIPS codes with corresponding
#' county and state information. Optionally formats names for plotting with
#' proper capitalization using tools::toTitleCase().
#'
#' @param zip_county_state Data frame containing ZIP-county-state crosswalk
#'   with columns: fips, county, state, state_name.
#' @param for_plotting Logical. If TRUE, formats names with proper capitalization
#'   for plotting (uppercase state abbreviations, title case names). Default is FALSE.
#'
#' @return Data frame with unique FIPS codes and corresponding geographic names.
#'   When for_plotting=TRUE, names are formatted for display purposes.
#'
#' @noRd
#'
#' @importFrom dplyr select distinct mutate
#' @importFrom rlang .data
create_fips_county_state <- function(zip_county_state, for_plotting = FALSE) {
  fips_county_state <- zip_county_state %>%
    select(.data$fips, .data$county, .data$state, .data$state_name) %>%
    distinct()

  if(for_plotting) {
    fips_county_state <- fips_county_state %>% mutate(
      state = toupper(.data$state),
      state_name = tools::toTitleCase(.data$state_name),
      county = tools::toTitleCase(.data$county)
    )
  }

  return(fips_county_state)
}

create_pstrat <- function(
    tract_data,
    zip_tract,
    metadata,
    link_geo = NULL,
    vars_global = GLOBAL$vars
) {
  
  # compute cell counts based on given geographic scale
  pstrat_data <- combine_tracts(tract_data, zip_tract, link_geo)
  cell_counts <- pstrat_data %>% select(-geocode) %>% t() %>% c()
  
  # create lists of all factor levels
  levels <- create_expected_levels(metadata)
  if(!is.null(link_geo)) {
    levels[[link_geo]] <- pstrat_data$geocode
  }

  # IMPORTANT: for sorting data frame to match cell order of poststratification table
  sort_vars <- c(link_geo, "sex", "race", "age")


  new_data <- expand.grid(levels, stringsAsFactors = FALSE) %>%
    arrange(across(all_of(sort_vars))) %>% # IMPORTANT: To match the cell order of poststratification data
    mutate(total = cell_counts)
}


tract_data <- readr::read_csv("inst/extdata/acs/acs_2019-2023.csv", show_col_types = FALSE)

zip_tract <- readr::read_csv(
  "inst/extdata/zip_tract.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = "c")
)

metadata <- list(
  special_case = NULL
)

pstrat <- create_pstrat(
  tract_data = tract_data,
  zip_tract = zip_tract,
  metadata = metadata,
  link_geo = "county"
)

View(pstrat)
readr::write_csv(pstrat, "inst/extdata/example/data/pstrat.csv")