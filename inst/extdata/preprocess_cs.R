find_columns <- function(df, expected_columns) {
  # find columns using string search
  df <- janitor::clean_names(df)
  all_names <- names(df)
  old_names <- expected_columns |> sapply(function(s) all_names[grepl(s, all_names, ignore.case = TRUE)]) |> unlist()

  df <- df |> select(all_of(old_names))
  names(df) <- names(old_names)
  missing <- setdiff(expected_columns, names(old_names))
  df[, missing] <- ""
  df <- df |> select(all_of(expected_columns))

  return(df)
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
  breaks <- c(-1, age_bounds[2:length(age_bounds)] - 1, 200)
  labels <- c(paste0(age_bounds[1:(length(age_bounds)-1)], '-', age_bounds[2:length(age_bounds)] - 1),
              paste0(age_bounds[length(age_bounds)], '+'))

  df <- df |> mutate(
    sex  = to_factor(sex, c("female"), other = "male"),
    race = to_factor(race, c("white", "black"), other = "other"),
    edu  = to_factor(edu, c("no hs", "some college", "4-year college", "post-grad"), other = "hs"),
    age  = cut(df$age, breaks, labels) |> as.character()
  )
}

aggregate_poll <- function(df, age_bounds, threshold = 0) {
  # identify columns
  df <- find_columns(df, expected_columns = c("sex", "race", "age", "edu", "state", "response"))

  # impute missing demographic data based on frequency
  df <- df |> mutate(across(c(sex, race, age, edu), impute))

  # # create factors from raw values
  df <- to_factor_all(df, age_bounds)

  # aggregate test records based on combinations of factors
  # and omit cells with small number of tests
  df <- df |>
    group_by(state, edu, age, race, sex) |>
    filter(n() >= threshold) |>
    summarize(
      total = n(),
      positive = sum(response)
    ) |>
    ungroup()

  return(df)
}

