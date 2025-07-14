get_bounds <- function(strings) {
  bounds <- strings %>% sapply(function(s) {
    stringr::str_split_1(s, "!!") %>%
      utils::tail(1) %>%
      stringr::str_split_1(" ") %>%
      utils::head(1) %>%
      as.numeric()
  })

  return(c(0, unname(bounds)))
}

collapse <- function(
    new_bounds,
    old_bounds,
    df_in,
    offset
) {

  df_out <- data.frame(init_column = 1:nrow(df_in))
  indices <- match(new_bounds, old_bounds)
  N <- length(indices)

  if(any(is.na(indices))) {
    stop("Invalid bounds!")
  }
  else {
    for(i in 1:(N-1)) {
      i_beg <- indices[i]
      i_end <- indices[i + 1]
      colname <- paste0(old_bounds[i_beg],'-', old_bounds[i_end] - offset)
      df_out[colname] <-  df_in %>% select(all_of(i_beg:(i_end-1))) %>% rowSums()
    }

    colname <- paste0(new_bounds[N], '+')
    df_out[colname] <- df_in %>% select(all_of(indices[N]:ncol(df_in))) %>% rowSums()

  }
  df_out <- df_out %>% select(-1)

  if(!identical(rowSums(df_in), rowSums(df_out))) {
    stop("Inconsistent row sums.")
  }

  return(df_out)
}

# Retrieve ACS data using tidycensus package
get_tract_data <- function(
    state_codes = c(state.abb, "DC"),
    age_bounds = c(0, 18, 35, 65, 75),
    poverty_bounds = c(0, 1, 2),
    year = 2021,
    include_covar = FALSE,
    chunk_size = 50
) {
  
  gen_vars <- function(str, seq) seq %>% sapply(function(i) sprintf("%s_%03d", str, i))
  
  # look-up table
  # https://www.census.gov/programs-surveys/acs/data/data-tables/table-ids-explained.html
  lookup_df <- tidycensus::load_variables(year, dataset = "acs5", cache = TRUE)
  
  # the indices are chosen based on the look-up table
  # for extracting table labels from look-up table
  age_indices_one <- 4:16
  age_indices_all <- 283:304
  poverty_indices <- 26397:26402
  
  # generate variable names
  group_names <- c("male_white", "female_white",
                   "male_black", "female_black",
                   "male_all", "female_all",
                   "pop_size")
  
  group_prefixes <- c("B01001A", "B01001A",
                      "B01001B", "B01001B",
                      "B01001", "B01001",
                      "B01001")
  
  group_table_numbers <- list(3:16, 18:31,
                              3:16, 18:31,
                              3:25, 27:49,
                              1)

  if (include_covar) {
    group_names <- c(group_names, "education", "poverty", "employment", "income")
    group_prefixes <- c(group_prefixes, "B15003", "C17002", "B23025", "B19013")
    group_table_numbers <- c(group_table_numbers, list(2:25), list(2:8), list(2:7), list(1))
  }


  # build the full vector of vars
  group_vars <- c()
  for(i in 1:length(group_names)) {
    group_vars <- group_vars %>% c(gen_vars(group_prefixes[i], group_table_numbers[[i]]))
  }
  
  # retrieve ACS tables
  all_tables <- tidycensus::get_acs(
    geography = "tract",
    variables = group_vars,
    state = state_codes,
    output = "wide",
    year = year
  )
  
  geoID <- all_tables[1]
  all_tables <- all_tables[seq(3, ncol(all_tables), by = 2)]
  
  group_dfs <- list()
  ind <- 0
  for(i in 1:length(group_names)) {
    n_tables <- length(group_table_numbers[[i]])
    group_dfs[group_names[i]] <- all_tables[(ind+1):(ind+n_tables)] %>% list()
    ind <- ind + n_tables
  }
  
  
  df_all <- data.frame(GEOID = geoID)
  
  # POPULATION SIZE
  names(group_dfs$pop_size) <- "pop_size"
  df_all <- cbind(df_all, group_dfs$pop_size)
  
  ### SEX, RACE, AGE
  # aggregate columns
  age_bounds_acs_all <- get_bounds(lookup_df$label[age_indices_all])
  age_bounds_acs_one <- get_bounds(lookup_df$label[age_indices_one])
  
  for(name in c("male_all", "female_all")) {
    group_dfs[name] <- collapse(age_bounds,
                                age_bounds_acs_all,
                                group_dfs[[name]],
                                1) %>% list()
  }
  for(name in c("male_white", "female_white",
                "male_black", "female_black")) {
    group_dfs[name] <- collapse(age_bounds,
                                age_bounds_acs_one,
                                group_dfs[[name]],
                                1) %>% list()
  }
  
  # subtract white and black from total
  group_dfs$male_other <- group_dfs$male_all - (group_dfs$male_white + group_dfs$male_black)
  group_dfs$female_other <- group_dfs$female_all - (group_dfs$female_white + group_dfs$female_black)
  
  # rename columns and combine data frames
  # IMPORTANT: the order of names must follow alphabetical order
  for(name in c("female_black", "female_other", "female_white",
                "male_black", "male_other", "male_white")) {
    names(group_dfs[[name]]) <- paste0(name, '_', names(group_dfs[[name]]))
    df_all <- cbind(df_all, group_dfs[[name]])
  }
  
  if (include_covar) {
    # EDUCATION
    education_levels <- c("below_college", "above_college")
    education_indices <- list(1:19, 20:24)
    for(i in 1:length(education_indices)) {
      name <- education_levels[i]
      inds <- education_indices[[i]]
      df_all[[name]] <- group_dfs$education[inds] %>% rowSums()
    }
    
    # POVERTY
    poverty_bounds_acs <- get_bounds(lookup_df$label[poverty_indices])
    group_dfs$poverty <- collapse(poverty_bounds,
                                  poverty_bounds_acs,
                                  group_dfs$poverty,
                                  0.01) %>% list()
    
    df_all <- cbind(df_all, group_dfs$poverty)
    
    # EMPLOYMENT
    employment_levels <- c("employed", "unemployed", "other")
    employment_indices <- list(3, 4, 5:6)
    for(i in 1:length(employment_indices)) {
      name <- employment_levels[i]
      inds <- employment_indices[[i]]
      df_all[[name]] <- group_dfs$employment[inds] %>% rowSums()
    }
    
    # INCOME
    names(group_dfs$income) <- "household_income"
    df_all <- cbind(df_all, group_dfs$income)
    
    # URBANICITY
    urbanicity <- haven::read_sas("dev/data/z_us_tract_uac.sas7bdat") %>%
      rename(
        "urbanicity" = "uac_yn",
        "GEOID" = "geocode"
      ) %>%
      select(GEOID, urbanicity)
    df_all <- df_all %>% full_join(urbanicity, by = "GEOID")
    
    # ADI
    adi_data <- haven::read_sas("dev/data/z_adi_bg_v3_2019.sas7bdat") %>%
      stats::na.omit() %>%
      group_by(.data$state_cty_tract_cd) %>%
      summarize(
        adi = mean(.data$us_adi_rank_num)
      ) %>%
      rename("GEOID" = "state_cty_tract_cd")
    df_all <- df_all %>% full_join(adi_data, by = "GEOID")
  }
  
  return(df_all)
}


# Rettrieve USPS crosswalk table
get_zip_tract <- function(key) {
  url <- "https://www.huduser.gov/hudapi/public/usps"
  response <- httr::GET(url, query = list(type = 1, query = "All"), httr::add_headers(Authorization = paste("Bearer", key)))

  httr::http_error(response)
  output <- httr::content(response)

  zip_tract <- dplyr::bind_rows(output$data$results)

  return(zip_tract)
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
    mutate(county = substr(.data$GEOID, 1, 5)) %>%
    group_by(.data$zip)
  
  # compute zip-level population size by aggregating across overlapping tracts
  all_colnames <- names(tract_data)
  pstrat_colnames <- all_colnames[grepl("male|female", all_colnames)]
  pstrat_data <- by_zip %>%
    summarise(
      county = first(county),
      across(all_of(pstrat_colnames), ~ sum(.x, na.rm = TRUE))
    )
  
  # omit zips with only NA then compute zip-level quantities
  covar_colnames <- setdiff(all_colnames, pstrat_colnames)
  covariates <- by_zip %>%
    filter(if_all(all_of(covar_colnames), ~ !all(is.na(.)))) %>%
    summarize(
      county = first(county),
      urbanicity = 1 - sum((pop_size / sum(pop_size, na.rm = TRUE)) * (urbanicity == "N"), na.rm = TRUE),
      college = sum(above_college, na.rm = TRUE) / (sum(below_college, na.rm = TRUE) + sum(above_college, na.rm = TRUE)),
      employment = sum(employed, na.rm = TRUE) / (sum(employed, na.rm = TRUE) + sum(unemployed, na.rm = TRUE) + sum(other, na.rm = TRUE)),
      poverty = sum(`0-0.99`, na.rm = TRUE) / (sum(`0-0.99`, na.rm = TRUE) + sum(`1-1.99`, na.rm = TRUE) + sum(`2+`, na.rm = TRUE)),
      income = sum((pop_size / sum(pop_size, na.rm = TRUE)) * household_income, na.rm = TRUE),
      adi = sum((pop_size / sum(pop_size, na.rm = TRUE)) * adi, na.rm = TRUE)
    )
  
  return(list(
    pstrat = pstrat_data,
    covar = covariates
  ))
}

library(magrittr)
library(dplyr)

year <- 2021
df <- get_tract_data(year = year, include_covar = TRUE)
readr::write_csv(df, paste0("inst/extdata/acs/acs_covid_", year-4, "-", year, ".csv"))