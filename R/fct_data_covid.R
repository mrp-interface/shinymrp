#' data_covid
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

find_columns_covid <- function(df) {
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
                 "result", "date")
  names(df) <- new_names

  return(df)
}

get_week_indices <- function(strings) {
  # extract week numbers, months and years from dates
  years_weeks <- ISOweek::ISOweek(strings)
  years <- years_weeks |> sapply(substr, start = 1, stop = 4) |> as.numeric()
  weeks <- years_weeks |> sapply(substr, start = 7, stop = 8) |> as.numeric()
  months <- strings |> as.Date() |> format("%m") |> as.numeric()

  # find year range
  c(low, high) %<-% range(years)
  all_years <- low:high


  if(low == high) {
    weeks_accum <- weeks
    timeline_week <- min(weeks_accum):max(weeks_accum)
    timeline_year <- rep(low, length(timeline_week))
  } else {
    # add offsets to week numbers in later years
    weeks_per_year <- paste0(all_years, "-12-28") |>
      ISOweek::ISOweek() |>
      sapply(substr, start = 7, stop = 8) |>
      as.numeric()

    weeks_offset <- c(0, cumsum(weeks_per_year[1:(length(weeks_per_year)-1)]))
    offsets <- years |> sapply(function(y) weeks_offset[which(all_years == y)])

    weeks_accum <- weeks + offsets
    weeks_accum <- weeks_accum - min(weeks_accum) + 1

    # find all weeks between the earliest and most recent dates
    start <- which.min(weeks_accum)
    end <- which.max(weeks_accum)
    year_start <- which(all_years == years[start])
    year_end <- which(all_years == years[end])

    # first year
    timeline_week <- weeks[start]:weeks_per_year[year_start]
    timeline_year <- rep(all_years[year_start], length(timeline_week))

    # in-between year
    for(year_ind in (year_start+1):(year_end-1)) {
      timeline_week <- c(timeline_week, 1:weeks_per_year[year_ind])
      timeline_year <- c(timeline_year, rep(all_years[year_ind], weeks_per_year[year_ind]))
    }

    # last year
    timeline_week <- c(timeline_week, 1:weeks[end])
    timeline_year <- c(timeline_year, rep(all_years[year_end], weeks[end]))
  }

  # get the start of each week
  timeline_date <- mapply(function(y, w) sprintf("%d-W%02d-1", y, w),
                          timeline_year,
                          timeline_week) |>
    ISOweek::ISOweek2date()


  return(list(weeks_accum, timeline_date))
}



to_factor_covid <- function(df, age_bounds) {
  is_pos <- grepl("positive|detected", df$result, ignore.case = TRUE)
  is_neg <- grepl("not|negative|undetected", df$result, ignore.case = TRUE)
  breaks <- c(-1, age_bounds[2:length(age_bounds)] - 1, 200)
  labels <- c(paste0(age_bounds[1:(length(age_bounds)-1)], '-', age_bounds[2:length(age_bounds)] - 1),
              paste0(age_bounds[length(age_bounds)], '+'))

  df <- df |> mutate(
    sex = to_factor(sex, c("female"), other = "male"),
    race = to_factor(race, c("white", "black"), other = "other"),
    age = cut(df$age, breaks, labels) |> as.character(),
    response = ifelse(is_neg, 0,
                      ifelse(is_pos, 1, NA))
  )

  return(df)
}

aggregate_covid <- function(
    patient,
    age_bounds,
    threshold = 0
) {

  # identify columns
  patient <- find_columns_covid(patient)

  # remove rows w/ missing data
  patient <- patient |> filter(!is.na(date) & !is.na(zip))

  # convert dates to week indices
  c(time_indices, timeline) %<-% get_week_indices(patient$date)
  patient$time <- time_indices

  # remove all but one test of a patient in the same week
  # patient <- patient |> distinct(id, time, .keep_all = TRUE)

  # impute missing demographic data based on frequency
  patient <- patient |> mutate(across(c(sex, race, age), impute))

  # create factors from raw values
  patient <- to_factor_covid(patient, age_bounds)

  # aggregate test records based on combinations of factors
  # and omit cells with small number of tests
  patient <- patient |>
    group_by(time, zip, sex, race, age) |>
    filter(n() >= threshold) |>
    summarize(
      total = n(),
      positive = sum(response)
    ) |>
    ungroup()

  # reset week indices and corresponding dates
  timeline <- timeline[min(patient$time):max(patient$time)]
  patient <- patient |> mutate(time = time - min(time) + 1)

  # add the column containing first dates of the weeks
  patient <- patient |>
    full_join(
      data.frame(
        time = 1:max(patient$time),
        date = timeline |> as.character()
      ),
      by = "time"
    )

  return(patient)
}


check_covid_data <- function(df, expected_columns, na_threshold = 0.5) {
  errors <- list()

  df <- find_columns(df, expected_columns)
  missing <- df |> lapply(function(c) all(as.character(c) == "")) |> unlist()
  missing_names <- names(missing)[missing]

  # check for missing columns
  if(length(missing_names) > 1 | (length(missing_names) == 1 & !"date" %in% missing_names)) {
    errors$missing_column <- paste0("The following columns are missing: ",
                                    paste(missing_names, collapse = ", "))
  } else {
    # check data types
    expected_types <- c("character", "character", "character", "integer|numeric",
                        "integer|numeric", "character|Date", "integer|numeric", "integer|numeric")
    types <- df |> lapply(class) |> unlist()
    valid <- mapply(grepl, types, expected_types)

    if(any(!valid)) {
      errors$data_type <- paste0("Columns corresponding to the following variables have inappropriate data types: ",
                                 paste(expected_columns[!valid], collapse = ", "))
    } else {
      na_percents <- df |>
        lapply(function(c) sum(as.numeric(is.na(c))) / length(c)) |>
        unlist()
      exceed <- na_percents > na_threshold

      if(any(exceed)) {
        errors$na <- paste0("Columns corresponding to the following variables have more than ",
                            na_threshold * 100, " percent rows with missing data: ",
                            paste(expected_columns[exceed], collapse = ", "))
      } else {
        df <- df |> na.omit()

        # check if week indices start at 1
        if(min(df$time) != 1) {
          errors$week <- "The lowest week index must be 1"
        }

        # check if dates are in the right format
        if("date" %in% missing_names) {
          errors$date <- "Dates are not provided. Plots will use week indices instead."
        } else {
          if (anyNA(as.Date(df$date, optional = TRUE))) {
            errors$date <- "Provided dates are not in expected format. Plots will use week indices instead."
          }
        }

        # check if cases are less than or equal to cases
        if(any(df$positive > df$total)) {
          errors$count <- "The number of cases cannot be greater than the number of tests"
        }
      }
    }
  }

  return(errors)
}


stan_factor_covid <- function(df, levels) {
  # rename raw columns
  df <- df |>
    rename(
      "sex_raw" = "sex",
      "race_raw" = "race",
      "age_raw" = "age",
      "time_raw" = "time",
      "zip_raw" = "zip"
    )

  # add Stan-dardized columns
  df <- df |>
    mutate(
      sex = factor(sex_raw, levels = levels$sex, labels = c(0, 1)) |> as.character() |> as.integer(),
      race = factor(race_raw, levels = levels$race, labels = 1:length(levels$race)) |> as.character() |> as.integer(),
      age = factor(age_raw, levels = levels$age, labels = 1:length(levels$age)) |> as.character() |> as.integer(),
      time = as.integer(time_raw),
      zip = as.factor(zip_raw) |> as.integer()
    )

  return(df)
}

get_dates <- function(df) {
  df$date |>
    na.omit() |>
    unique() |>
    as.Date() |>
    sort() |>
    format("%d%b\n%Y") |>
    as.character()
}

filter_state_zip <- function(
    df_zip,
    zip_tract,
    zip_threshold = 5,
    state_threshold = 0.01
) {

  n_records <- sum(df_zip$zip_count, na.rm = TRUE)

  # create a table containing state, zip and zip count
  state_zip <- zip_tract |>
    mutate(state_code = substr(geoid, 1, 2)) |>
    select(zip, state, state_code) |>
    distinct(zip, .keep_all = TRUE) |>
    right_join(df_zip, by = "zip")

  # filter based on proportion of state
  state_zip <- state_zip |>
    group_by(state) |>
    filter(sum(zip_count) > state_threshold * n_records) |>
    ungroup()

  # filter based on number of zip
  state_zip <- state_zip |>
    group_by(zip) |>
    filter(sum(zip_count) > zip_threshold) |>
    ungroup()

  state_zip_select <- state_zip |>
    select(state_code, zip) |>
    sapply(unique)

  return(state_zip_select)
}

# For each zip, find tracts and aggregate data over them
combine_tracts <- function(
    patient,
    tract_data,
    zip_tract
) {

  by_zip <- zip_tract |>
    filter(zip %in% unique(patient$zip)) |>
    select(geoid, zip) |>
    rename("GEOID" = "geoid") |>
    inner_join(
      tract_data,
      by = "GEOID"
    ) |>
    mutate(county = substr(GEOID, 1, 5)) |>
    group_by(zip)

  all_colnames <- names(tract_data)
  pstrat_colnames <- all_colnames[grepl("male|female", all_colnames)]
  pstrat_data <- by_zip |>
    summarise(
      county = first(county),
      across(all_of(pstrat_colnames), sum)
    )

  covariates <- by_zip |>
    summarize(
      county = first(county),
      urbanicity  = 1 - sum((pop_size / sum(pop_size)) * (urbanicity == "N")),
      college     = sum(above_college) / (sum(below_college) + sum(above_college)),
      employment  = sum(employed) / (sum(employed) + sum(unemployed) + sum(other)),
      poverty     = sum(`0-0.99`) / (sum(`0-0.99`) + sum(`1-1.99`) + sum(`2+`)),
      income      = sum((pop_size / sum(pop_size)) * household_income),
      ADI         = sum((pop_size / sum(pop_size)) * adi)
    )

  # remove zip codes without tract in USPS crosswalk table
  patient <- patient |> filter(zip %in% pstrat_data$zip)

  return(list(patient, pstrat_data, covariates))
}

link_ACS <- function(
    patient,
    tract_data,
    zip_tract
) {

  # filter out state and zip codes with small sample sizes
  state_zip_select <- filter_state_zip(
    patient |> group_by(zip) |> summarize(zip_count = sum(total)),
    zip_tract
  )
  patient <- patient |> filter(zip %in% state_zip_select$zip)

  # compute zip-level data
  c(patient, pstrat_data, covariates) %<-% combine_tracts(
    patient,
    tract_data,
    zip_tract
  )

  # save unstandardized covariates for plotting
  raw_covariates <- covariates

  # standardize zip-level covariates
  covariates <- covariates |> mutate(
    across(!zip & !county, function(c) c |> scale() |> as.vector() |> unlist(use.names=FALSE))
  )

  return(list(patient, pstrat_data, covariates, raw_covariates))
}

prepare_data_covid <- function(
    patient,
    pstrat_data,
    covariates,
    demo_levels
) {

  cell_counts <- pstrat_data[-c(1, 2)] |> t() |> c()
  time_indices <- 1:max(patient$time)
  n_time_indices <- length(time_indices)

  input_data <- patient |>
    left_join(covariates, by = "zip")


  new_data <- tidyr::expand_grid(
    sex  = demo_levels$sex,
    race = demo_levels$race,
    age  = demo_levels$age,
    time = time_indices,
    zip  = covariates$zip
  ) |>
    mutate(
      sex  = factor(sex, levels = demo_levels$sex),
      race = factor(race, levels = demo_levels$race),
      age  = factor(age, levels = demo_levels$age)
    ) |>
    arrange(time, zip, sex, race, age) |>  # IMPORTANT: To match the cell order of poststratification data
    mutate(total = rep(cell_counts, n_time_indices)) |>
    left_join(covariates, by = "zip")


  # create lists of all factor levels
  levels <- demo_levels
  levels$county <- pstrat_data$county |> unique()

  # list of variables for model specification
  vars <- list(
    fixed = list(
      "Individual-level Predictor" = c("sex", "race", "age", "time"),
      "Geographic Indicator" = c("zip"),
      "Geographic Predictor" = names(covariates) |> setdiff(c("zip", "county"))
    ),
    varying = list(
      "Individual-level Predictor" = c("race", "age", "time"),
      "Geographic Indicator" = c("zip")
    )
  )

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
    print("Invalid bounds!")
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
    print("Inconsistent row sums.")
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

  # URBANICITY
  urbanicity <- read_sas("z_us_tract_uac.sas7bdat") |>
    rename(
      "urbanicity" = "uac_yn",
      "GEOID" = "geocode"
    )
  df_all <- df_all |> inner_join(urbanicity, by = "GEOID")


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

  # ADI
  adi_data <- read_sas("z_adi_bg_v3_2019.sas7bdat") |>
    na.omit() |>
    group_by(state_cty_tract_cd) |>
    summarize(
      adi = mean(us_adi_rank_num)
    ) |>
    rename("GEOID" = "state_cty_tract_cd")
  df_all <- df_all |> inner_join(adi_data, by = "GEOID")

  df_all <- na.omit(df_all)

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
