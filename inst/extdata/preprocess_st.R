library(stringr)
library(dplyr)
library(readxl)
library(haven)
library(zeallot)
library(ISOweek)

find_columns <- function(df) {
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

  # add offsets to week numbers in later years
  c(low, high) %<-% range(years)
  all_years <- low:high

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

  # get the start of each week
  timeline_date <- mapply(function(y, w) sprintf("%d-W%02d-1", y, w),
                          timeline_year,
                          timeline_week) |>
    ISOweek::ISOweek2date()


  return(list(weeks_accum, timeline_date))
}


impute <- function(v) {
  cond <- is.na(v) | grepl("unknown", v, ignore.case=TRUE)

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

to_factor <- function(values, levels, other = NA) {
  for(lvl in levels) {
    values[grepl(lvl, values, ignore.case = TRUE)] <- lvl
  }

  values[!values %in% levels] <- other

  return(values)
}

to_factor_all <- function(df, age_bounds) {
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

aggregate <- function(
    patient,
    age_bounds,
    threshold = 0
) {

  # identify columns
  patient <- find_columns(patient)

  # remove rows w/ missing data
  patient <- patient |> filter(!is.na(date) & !is.na(zip))

  # convert dates to week indices
  c(time_indices, timeline) %<-% get_week_indices(patient$date)
  patient$time <- time_indices

  # remove all but one test of a patient in the same week
  patient <- patient |> distinct(id, time, .keep_all = TRUE)

  # impute missing demographic data based on frequency
  patient <- patient |> mutate(across(c(sex, race, age), impute))

  # create factors from raw values
  patient <- to_factor_all(patient, age_bounds)

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
