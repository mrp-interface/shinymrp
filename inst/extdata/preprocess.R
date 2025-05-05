clean_names <- function(names) {
  names |> 
    tolower() |> 
    gsub("[0-9]", "_", x = _) |>
    gsub("[^[:alpha:]]", "_", x = _) |>
    gsub("_{2,}", "_", x = _) |>
    gsub("^_|_$", "", x = _)
}


clean_chr <- function(df) {
  # Convert character columns to lowercase and trim whitespace
  df |> mutate(across(where(is.character), 
                ~str_trim(tolower(.x))))
}

clean_data <- function(
    df,
    na_strings = c("", "na", "n/a", "none", "null", "unknown")
) {

  # Clean column names
  names(df) <- clean_names(names(df))

  # Remove duplicate column names (keeping first occurrence)
  if (any(duplicated(names(df)))) {
    df <- df[, !duplicated(names(df))]
  }

  # Convert character columns to lowercase and trim whitespace
  df <- clean_chr(df)

  # Convert common NA strings to actual NA
  df <- df |> 
    mutate(across(everything(), 
                 ~if_else(.x %in% na_strings, NA, .x)))

  # Format geographic identifiers
  df <- fix_geocode(df)
  
  return(df)
}

rename_columns <- function(df) {
  target_names <- c(
    GLOBAL$vars$indiv,
    GLOBAL$vars$geo,
    GLOBAL$vars$ignore
  )

  current_names <- names(df)
  rename_map <- sapply(target_names, function(target) {
    matches <- grep(target, current_names, ignore.case = TRUE, value = TRUE)
    if(length(matches) > 0) matches[1] else NULL
  })
  
  # Filter out NULLs and create rename specification
  rename_map <- rename_map[!sapply(rename_map, is.null)]
  if(length(rename_map) == 0) return(df)
  
  # Create the renaming specification (new_name = old_name)
  rename_spec <- stats::setNames(rename_map, names(rename_map))
  
  # Apply renaming
  dplyr::rename(df, !!!rename_spec)
}

impute <- function(v) {
  cond <- is.na(v)
 
  if(sum(cond) == 0) {
    return(v)
  }

  tbl <- table(v[!cond])
  freqs <- as.numeric(tbl)
  lvls <- names(tbl)

  if(is.numeric(v)) {
    lvls <- as.numeric(lvls)
  }

  v[cond] <- sample(
    lvls,
    prob = freqs / sum(freqs),
    size = sum(cond),
    replace = TRUE
  )

  return(v)
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

aggregate_data <- function(
    df,
    expected_levels,
    threshold = 0
) {

  indiv_vars <- names(expected_levels)

  # remove NAs
  check_cols <- setdiff(names(df), indiv_vars)
  df <- df |> tidyr::drop_na(all_of(check_cols))

  # convert date to week indices if necessary
  convert_date <- "time" %in% indiv_vars &&
                  "date" %in% names(df) 
  
  if (convert_date) {
    # convert date to week indices
    c(time_indices, timeline) %<-% get_week_indices(df$date)
    df$time <- time_indices
  }


  # recode values to expected levels
  df <- recode_values(df, expected_levels)
  

  # impute missing demographic data based on frequency
  df <- df |> mutate(across(all_of(indiv_vars), impute))

  all_geo_vars <- GLOBAL$vars$geo
  geo_vars <- intersect(all_geo_vars, names(df))
  smallest_geo <- all_geo_vars[min(match(geo_vars, all_geo_vars))]
  group_vars <- c(indiv_vars, smallest_geo)
  geo_covars <- setdiff(geo_vars, smallest_geo)

  # aggregate test records based on combinations of factors
  df <- df |>
    group_by(!!!syms(group_vars)) |>
    filter(n() >= threshold) |>   # omit cells with small number of tests
    summarize(
      across(all_of(geo_covars), first),
      total = n(),
      positive = sum(positive)
    ) |>
    ungroup()

  if (convert_date) {
    # reset week indices and corresponding dates if cells were dropped
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
  }

  return(df)
}
