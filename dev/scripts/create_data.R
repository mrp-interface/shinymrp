source("R/utils_global.R")
source("R/fct_data.R")

library(dplyr)
library(magrittr)

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