source("R/global.R")
source("R/fct_data.R")

library(dplyr)
library(magrittr)


major_county <- function(zip_tract) {
  # find the most common county for each zip code
  zip_fips <- zip_tract %>%
    mutate(fips = substr(geoid, 1, 5)) %>%
    # sum ratio by (zip, county)
    group_by(zip, fips) %>%
    summarise(total_ratio = sum(res_ratio, na.rm = TRUE), .groups = "drop") %>%
    # for each zip, pick the county with the max total_ratio
    group_by(zip) %>%
    slice_max(total_ratio, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-total_ratio)

  return(zip_fips)
}

#' Create zip-to-county-state crosswalk
#'
#' @description Creates a crosswalk table linking ZIP codes to counties and
#' states by finding the most common county for each ZIP code and joining
#' with FIPS county-state information. Handles ZIP codes that span multiple
#' counties by selecting the most frequent county.
#' 
#' @param zip_tract Data frame containing ZIP code to census tract mapping
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
create_zip_county_state <- function(zip_tract) {
  # find the most common county for each zip code
  zip_fips <- major_county(zip_tract)

  fips_county_state <- tidycensus::fips_codes %>%
    mutate(fips = paste0(state_code, county_code)) %>%
    select(fips, county, state, state_name)

  
  zip_county_state <- left_join(
    zip_fips,
    fips_county_state,
    by = "fips"
  )
  
  return(zip_county_state)
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

combine_tracts_covid <- function(
    tract_data,
    zip_tract
) {
  
  # join tract-level data with zip-tract conversion table
  # then group by zip
  by_zip <- zip_tract %>%
    select(.data$geoid, .data$zip) %>%
    rename("GEOID" = "geoid") %>%
    inner_join(
      tract_data,
      by = "GEOID"
    ) %>%
    group_by(.data$zip)
  
  # compute zip-level population size by aggregating across overlapping tracts
  all_colnames <- names(tract_data)
  pstrat_colnames <- all_colnames[grepl("male|female", all_colnames)]
  pstrat_data <- by_zip %>%
    summarise(
      across(all_of(pstrat_colnames), ~ sum(.x, na.rm = TRUE))
    )

  # find most overlapping county for each zip code
  zip_fips <- major_county(zip_tract) %>%
    rename("county" = "fips")

  # omit zips with only NA then compute zip-level quantities
  covar_colnames <- setdiff(all_colnames, pstrat_colnames)
  covariates <- by_zip %>%
    filter(if_all(all_of(covar_colnames), ~ !all(is.na(.)))) %>%
    summarize(
      urbanicity = 1 - sum((pop_size / sum(pop_size, na.rm = TRUE)) * (urbanicity == "N"), na.rm = TRUE),
      college = sum(above_college, na.rm = TRUE) / (sum(below_college, na.rm = TRUE) + sum(above_college, na.rm = TRUE)),
      employment = sum(employed, na.rm = TRUE) / (sum(employed, na.rm = TRUE) + sum(unemployed, na.rm = TRUE) + sum(other, na.rm = TRUE)),
      poverty = sum(`0-0.99`, na.rm = TRUE) / (sum(`0-0.99`, na.rm = TRUE) + sum(`1-1.99`, na.rm = TRUE) + sum(`2+`, na.rm = TRUE)),
      income = sum((pop_size / sum(pop_size, na.rm = TRUE)) * household_income, na.rm = TRUE),
      adi = sum((pop_size / sum(pop_size, na.rm = TRUE)) * adi, na.rm = TRUE)
    ) %>%
    left_join(zip_fips, by = "zip")
  
  return(list(
    pstrat = pstrat_data,
    covar = covariates
  ))
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

library(magrittr)
library(dplyr)
library(readr)

save_dir <- "/Users/tntoan/Desktop/MRP/Data/shinymrp/"

# Create tract data for COVID
year <- 2021
tract_data <- get_tract_data(year = year, include_covar = TRUE)
readr::write_csv(tract_data, paste0(save_dir, "acs/acs_covid_", year-4, "-", year, ".csv"))

# Get ZIP-tract crosswalk
zip_tract <- get_zip_tract(Sys.getenv("USPS_CROSSWALK_API_KEY"))

tract_data <- readr::read_csv(
  paste0(save_dir, "acs/acs_covid_2017-2021.csv"),
  show_col_types = FALSE
)

zip_tract <- readr::read_csv(
  paste0(save_dir, "zip_tract_full.csv"),
  show_col_types = FALSE
)

# Create poststratification and covariate data
out <- combine_tracts_covid(
  tract_data = tract_data,
  zip_tract = zip_tract
)

write_csv(out$pstrat, paste0(save_dir, "acs/pstrat_covid.csv"))
write_csv(out$covar, paste0(save_dir, "acs/covar_covid.csv"))

 
# Create example poststratification data
tract_data <- readr::read_csv(paste0(save_dir, "acs/acs_2019-2023.csv"), show_col_types = FALSE)

zip_tract <- readr::read_csv(
  paste0(save_dir, "zip_tract.csv"),
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