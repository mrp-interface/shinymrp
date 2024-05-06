#' data
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @import dplyr
#' @import zeallot

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

prep <- function(
    df,
    expected_values,
    to_lower = NULL,
    to_char = NULL
) {

  # regularize data types and values
  if(!is.null(to_lower)) {
    df <- df |> mutate(across(all_of(to_lower), tolower))
  }

  if(!is.null(to_char)) {
    df <- df |> mutate(across(all_of(to_char), as.character))
  }

  # keep only expected values
  for(name in names(expected_values)) {
    df <- df |> filter(df[[name]] %in% expected_values[[name]])
  }

  return(df)
}

filter_geojson <- function(map_data, in_data) {
  all_areas <- map_data$features
  areas_in_data <- purrr::keep(all_areas, function(c) c$properties$GEOID %in% in_data)
  map_data$features <- areas_in_data

  return(map_data)
}
