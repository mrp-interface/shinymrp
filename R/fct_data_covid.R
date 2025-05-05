#' data_covid
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

rename_columns_covid <- function(df) {
  all_names <- names(df)
  patterns <- c("encrypted|masked", "sex", "race", "age", "zip")

  old_names <- patterns |>
    sapply(\(s) all_names[grepl(s, all_names, ignore.case=TRUE)]) |>
    unlist()
  old_names <- c(old_names,
                 all_names[grepl("result", all_names, ignore.case=TRUE)
                               & !grepl("date|time|igg", all_names, ignore.case=TRUE)] |> unlist(),
                 all_names[grepl("result.*(time|date)", all_names, ignore.case=TRUE)
                             & !grepl("igg", all_names, ignore.case=TRUE)]
                 )

  df <- df |> select(all_of(old_names))
  new_names <- c("id", "sex", "race", "age", "zip",
                 "positive", "date")
  names(df) <- new_names

  return(df)
}

recode_covid <- function(df, expected_levels) {
  ranges <- expected_levels$age
  age_bounds <- regmatches(
    ranges,
    regexpr("^\\d+", ranges)
  ) |>
    as.numeric()
  breaks <- c(-1, age_bounds[2:length(age_bounds)] - 1, 200)

  df <- df |> mutate(
    sex = if_else(str_detect(sex, regex("female", ignore_case = TRUE)), "female", "male"),
    race = case_when(
      str_detect(race, regex("white", ignore_case = TRUE)) ~ "white",
      str_detect(race, regex("black", ignore_case = TRUE)) ~ "black",
      TRUE ~ "other"
    ),
    age = cut(df$age, breaks, ranges) |> as.character(),
    positive = if("positive" %in% names(df)) case_match(
      as.character(positive),
      c("positive", "detected", "yes", "y", "true", "1") ~ 1,
      c("negative", "undetected", "no", "n", "false", "0") ~ 0
    )
  )

  return(df)
}

aggregate_covid <- function(
    df,
    expected_levels,
    threshold = 0
) {

  indiv_vars <- names(expected_levels)
  
  # remove NAs
  check_cols <- setdiff(names(df), indiv_vars)
  df <- df |> tidyr::drop_na(all_of(check_cols))

  # convert dates to week indices
  c(time_indices, timeline) %<-% get_week_indices(df$date)
  df$time <- time_indices

  # remove all but one test of a df in the same week
  df <- df |> distinct(id, time, .keep_all = TRUE)
  
  # create factors from raw values
  df <- recode_covid(df, expected_levels)

  # impute missing demographic data based on frequency
  df <- df |> mutate(across(c(sex, race, age), impute))
  
  # aggregate test records based on combinations of factors
  # and omit cells with small number of tests
  df <- df |>
    group_by(time, zip, sex, race, age) |>
    filter(n() >= threshold) |>
    summarize(
      total = n(),
      positive = sum(positive)
    ) |>
    ungroup()

  # reset week indices and corresponding dates
  timeline <- timeline[min(df$time):max(df$time)]
  df <- df |> mutate(time = time - min(time) + 1)

  # add the column containing first dates of the weeks
  df <- df |>
    full_join(
      data.frame(
        time = 1:max(df$time),
        date = timeline |> as.character()
      ),
      by = "time"
    )

  return(df)
}

filter_state_zip <- function(
    df,
    covariates,
    zip_threshold = 5,
    state_threshold = 0.01
) {
  
  zip_count <- df |>
    group_by(zip) |>
    summarize(count = sum(total))

  # create a table containing state, zip and zip count
  state_zip <- covariates |>
    mutate(state = substr(county, 1, 2)) |>
    select(zip, state) |>
    distinct(zip, .keep_all = TRUE) |>
    inner_join(zip_count, by = "zip")

  # filter based on proportion of state
  N <- sum(state_zip$count)
  state_zip <- state_zip |>
    group_by(state) |>
    filter(sum(count) > state_threshold * N) |>
    ungroup()

  # filter based on number of zip
  state_zip <- state_zip |>
    group_by(zip) |>
    filter(sum(count) > zip_threshold) |>
    ungroup()

  df <- df |> filter(zip %in% state_zip$zip)

  return(df)
}


prepare_data_covid <- function(
    input_data,
    pstrat_data,
    covariates,
    demo_levels,
    vars_global
) {

  # filter out state and zip codes with small sample sizes
  input_data <- filter_state_zip(input_data, covariates)
  covariates <- covariates |> filter(zip %in% input_data$zip)
  pstrat_data <- pstrat_data |> filter(zip %in% input_data$zip)
  cell_counts <- pstrat_data[-c(1, 2)] |> t() |> c()
  
  # # prevent duplicate columns
  dup_cols <- intersect(names(input_data), names(covariates)) |> setdiff(c("zip"))
  input_data <- input_data |> select(-all_of(dup_cols))

  input_data <- input_data |>
    as_factor(demo_levels) |>
    left_join(covariates, by = "zip")

  # create lists of all factor levels
  levels <- demo_levels
  levels$time <- unique(input_data$time) |> sort()
  levels$zip <- pstrat_data$zip

  new_data <- expand.grid(levels, stringsAsFactors = TRUE) |> # sex, race, age must be factors for later use in plotting
    arrange(time, zip, sex, race, age) |>  # IMPORTANT: To match the cell order of poststratification data
    mutate(total = rep(cell_counts, length(levels$time))) |>
    left_join(covariates, by = "zip")

  # append levels for other geographic predictors
  # NOTE: this must be done after new_data is created
  # as these levels are not used in the poststratification table
  levels$county <- pstrat_data$county |> unique()

  # list of variables for model specification
  vars <- create_variable_list(input_data, covariates, vars_global)

  return(list(input_data, new_data, levels, vars))
}


### FOR GENERATING STATIC DATA

get_bounds <- function(strings) {
  bounds <- strings |> sapply(function(s) str_split_1(s, "!!") |> tail(1) |> str_split_1(" ") |> head(1) |> as.numeric())

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
      df_out[colname] <-  df_in |> select(all_of(i_beg:(i_end-1))) |> rowSums()
    }

    colname <- paste0(new_bounds[N], '+')
    df_out[colname] <- df_in |> select(all_of(indices[N]:ncol(df_in))) |> rowSums()

  }
  df_out <- df_out |> select(-1)

  if(!identical(rowSums(df_in), rowSums(df_out))) {
    stop("Inconsistent row sums.")
  }

  return(df_out)
}

# Retrieve ACS data using tidycensus package
get_tract_data <- function(
    state_codes,
    age_bounds,
    poverty_bounds,
    year
) {
  
  gen_vars <- function(str, seq) seq |> sapply(\(i) sprintf("%s_%03d", str, i))
  
  # look-up table
  # https://www.census.gov/programs-surveys/acs/data/data-tables/table-ids-explained.html
  lookup_df <- load_variables(2021, dataset = "acs5", cache = TRUE)
  
  # the indices are chosen based on the look-up table
  # for extracting table labels from look-up table
  age_indices_one <- 4:16
  age_indices_all <- 283:304
  poverty_indices <- 26397:26402
  
  # for level-based aggregation of columns of get_acs output
  education_levels <- c("below_college", "above_college")
  education_indices <- list(1:19, 20:24)
  employment_levels <- c("employed", "unemployed", "other")
  employment_indices <- list(3, 4, 5:6)
  
  # generate variable names
  group_names <- c("male_white", "female_white",
                   "male_black", "female_black",
                   "male_all", "female_all",
                   "education", "poverty",
                   "employment", "income",
                   "pop_size")
  
  group_prefixes <- c("B01001A", "B01001A",
                      "B01001B", "B01001B",
                      "B01001", "B01001",
                      "B15003", "C17002",
                      "B23025", "B19013",
                      "B01001")
  
  group_table_numbers <- list(3:16, 18:31,
                              3:16, 18:31,
                              3:25, 27:49,
                              2:25, 2:8,
                              2:7, 1,
                              1)
  
  group_vars <- c()
  for(i in 1:length(group_names)) {
    group_vars <- group_vars |> c(gen_vars(group_prefixes[i], group_table_numbers[[i]]))
  }
  
  # retrieve ACS tables
  all_tables <- get_acs(
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
    group_dfs[group_names[i]] <- all_tables[(ind+1):(ind+n_tables)] |> list()
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
                                1) |> list()
  }
  for(name in c("male_white", "female_white",
                "male_black", "female_black")) {
    group_dfs[name] <- collapse(age_bounds,
                                age_bounds_acs_one,
                                group_dfs[[name]],
                                1) |> list()
  }
  
  # subtract white and black from total
  group_dfs$male_other <- group_dfs$male_all - (group_dfs$male_white + group_dfs$male_black)
  group_dfs$female_other <- group_dfs$female_all - (group_dfs$female_white + group_dfs$female_black)
  
  # rename columns and combine data frames
  for(name in c("male_white", "male_black", "male_other",
                "female_white", "female_black", "female_other")) {
    names(group_dfs[[name]]) <- paste0(name, '_', names(group_dfs[[name]]))
    df_all <- cbind(df_all, group_dfs[[name]])
  }
  
  # EDUCATION
  for(i in 1:length(education_indices)) {
    name <- education_levels[i]
    inds <- education_indices[[i]]
    df_all[[name]] <- group_dfs$education[inds] |> rowSums()
  }
  
  # POVERTY
  poverty_bounds_acs <- get_bounds(lookup_df$label[poverty_indices])
  group_dfs$poverty <- collapse(poverty_bounds,
                                poverty_bounds_acs,
                                group_dfs$poverty,
                                0.01) |> list()
  
  df_all <- cbind(df_all, group_dfs$poverty)
  
  # EMPLOYMENT
  for(i in 1:length(employment_indices)) {
    name <- employment_levels[i]
    inds <- employment_indices[[i]]
    df_all[[name]] <- group_dfs$employment[inds] |> rowSums()
  }
  
  # INCOME
  names(group_dfs$income) <- "household_income"
  df_all <- cbind(df_all, group_dfs$income)
  
  # URBANICITY
  urbanicity <- haven::read_sas("z_us_tract_uac.sas7bdat") |>
    rename(
      "urbanicity" = "uac_yn",
      "GEOID" = "geocode"
    ) |>
    select(GEOID, urbanicity)
  df_all <- df_all |> full_join(urbanicity, by = "GEOID")
  
  # ADI
  adi_data <- haven::read_sas("z_adi_bg_v3_2019.sas7bdat") |>
    na.omit() |>
    group_by(state_cty_tract_cd) |>
    summarize(
      adi = mean(us_adi_rank_num)
    ) |>
    rename("GEOID" = "state_cty_tract_cd")
  df_all <- df_all |> full_join(adi_data, by = "GEOID")
  
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

