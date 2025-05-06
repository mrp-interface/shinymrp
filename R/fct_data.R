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
read_data <- function(file_path) {
  if(stringr::str_ends(file_path, "csv")) {
    readr::read_csv(file_path, show_col_types = FALSE)
  } else if (stringr::str_ends(file_path, "(xlsx|xls)")) {
    readxl::read_excel(file_path, guess_max = 5000)
  } else if (stringr::str_ends(file_path, "sas7bdat")) {
    haven::read_sas(file_path)
  }
}

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

find_bad_geocode <- function(geocodes) {
  # Coerce to character (so that NA stays NA_character_)
  geocodes <- as.character(geocodes)
  
  # Valid if exactly 5 digits
  valid <- grepl("^[0-9]{5}$", geocodes)
  
  # Replace anything not matching (including non‐character inputs) with NA
  geocodes[!valid] <- NA_character_
  
  return(geocodes)
}

format_geocode <- function(df) {
  if ("zip" %in% names(df)) {
    if (is.numeric(df$zip)) {
      df$zip <- sprintf("%05d", df$zip)
    } else {
      df$zip <- find_bad_geocode(df$zip)
    }
  }

  if ("county" %in% names(df)) {
    if (is.numeric(df$county)) {
      df$county <- sprintf("%05d", df$county)
    } else {
      df$county <- find_bad_geocode(df$county)
    }
  }

  if ("state" %in% names(df)) {
    if (is.numeric(df$state)) {
      df$state <- sprintf("%02d", df$state)
    }
  }

  return(df)
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
  df <- format_geocode(df)
  
  return(df)
}

rename_columns <- function(df, const, covid_indiv = FALSE) {
  if (covid_indiv) {
    return(rename_columns_covid(df))
  }

  target_names <- c(
    const$vars$indiv,
    const$vars$geo,
    const$vars$ignore
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

get_dates <- function(df) {
  df$date |>
    na.omit() |>
    unique() |>
    as.Date() |>
    sort() |>
    format(GLOBAL$ui$date_format) |>
    as.character()
}

recode_values <- function(df, expected_levels, covid=FALSE) {
  if (covid) {
    return(recode_covid(df, expected_levels))
  }

  # this function assumes that strings are already lower case
  ranges <- expected_levels$age
  age_bounds <- regmatches(
    ranges,
    regexpr("^\\d+", ranges)
  ) |>
    as.numeric()
  breaks <- c(-1, age_bounds[2:length(age_bounds)] - 1, 200)
  colnames <- names(df)

  df <- df |> mutate(
    sex  = if("sex" %in% colnames) if_else(sex %in% expected_levels$sex, sex, NA),
    race = if("race" %in% colnames) if_else(race %in% c(expected_levels$race, NA), race, "other"),
    age  = if("age" %in% colnames) cut(df$age, breaks, ranges) |> as.character(),
    edu  = if("edu" %in% colnames) if_else(edu %in% expected_levels$edu, edu, NA),
    positive = if("positive" %in% colnames) case_match(
      as.character(positive),
      c("positive", "detected", "yes", "y", "true", "1") ~ 1,
      c("negative", "undetected", "no", "n", "false", "0") ~ 0
    )
  )

  return(df)
}

filter_geojson <- function(geojson, geoids, omit = FALSE) {
  if(is.null(geojson) | is.null(geoids)) {
    return(NULL)
  }

  geojson$features <- purrr::keep(
    geojson$features,
    function(f) !is.null(nullify(f$properties$fips)) && f$properties$fips %in% geoids
  )

  return(geojson)
}

to_fips <- function(vec, fips_county_state, link_geo = c("county", "state")) {
  link_geo <- match.arg(link_geo)

  lookup_df <- if(link_geo == "state") aggregate_fips(fips_county_state) else fips_county_state
  fmt <- if(link_geo == "state") "%02d" else "%05d"

  if(is.numeric(vec)) {
    # If column already contains FIPS codes, return with proper formatting
    fips <- sprintf(fmt, vec)
  } else {
    # Otherwise, find best matching column
    counts <- lookup_df |> apply(2, function(c) sum(vec %in% c))
    colname <- names(counts)[which.max(counts)]

    fips <- lookup_df$fips[match(vec, lookup_df[[colname]])]
  }

  return(fips)
}

get_geo_predictors <- function(df, geo_col) {
  bool <- df |>
    group_by(!!sym(geo_col)) |>
    summarize_all(n_distinct) |>
    lapply(function(c) all(c == 1)) |>
    unlist()

  geo_pred_cols <- names(bool)[bool]

  geo_preds <- df |>
    select(all_of(c(geo_col, geo_pred_cols))) |>
    distinct(!!sym(geo_col), .keep_all = TRUE)

  return(geo_preds)
}

get_smallest_geo <- function(col_names, geo_col) {
  # Find the smallest geographic index
  idx <- match(col_names, GLOBAL$vars$geo) |> na.omit()
  if (length(idx) == 0) {
    return(NULL)
  }

  smallest_geo_index <- min(idx)
  smallest_geo <- GLOBAL$vars$geo[smallest_geo_index]

  return(list(
    geo = smallest_geo,
    idx = smallest_geo_index
  ))
}

append_geo <- function(input_data, zip_county_state, geo_all) {
  smallest <- get_smallest_geo(names(input_data), geo_all)
  if (is.null(smallest)) {
    return(input_data)
  }

  fips_county_state <- create_fips_county_state(zip_county_state)

  # Get geographic variables at current and larger scales
  geo_vars <- geo_all[smallest$idx:length(geo_all)]

  # Prepare geographic crosswalk
  zip_county_state <- zip_county_state |>
    select(zip, fips) |>
    rename(county = fips) |>
    mutate(state = substr(county, 1, 2)) |>
    select(all_of(geo_vars)) |>
    distinct()
  
  # Convert names to FIPS for smallest geographic scale
  if (smallest$geo != "zip") { 
    input_data[[smallest$geo]] <- to_fips(
      input_data[[smallest$geo]], 
      fips_county_state, 
      smallest$geo
    )
  }

  # Join geographic variables
  input_data <- clean_left_join(input_data, zip_county_state, by = smallest$geo)

  # Convert names to GEOIDs for larger geographic scales
  for (geo in setdiff(geo_vars, smallest$geo)) {
    if (geo != "zip") {
      input_data[[geo]] <- to_fips(
        input_data[[geo]], 
        fips_county_state, 
        geo
      )
    }
  }
  

  return(input_data)
}

as_factor <- function(df, levels) {
  # Find columns that exist in both df and have defined levels
  cols_to_convert <- intersect(names(df), names(levels))
  
  # Apply factor conversion to each column
  for(col in cols_to_convert) {
    if(!is.null(levels[[col]])) {
      df[[col]] <- factor(df[[col]], levels = levels[[col]])
    }
  }
  
  return(df)
}

find_nested <- function(df, cols, sep = "---") {
  # generate all 2‑column combinations
  pairs <- combn(cols, 2, simplify = FALSE)
  
  # test each pair for a bijection via approach 2
  is_bij <- vapply(pairs, function(pr) {
    x  <- df[[pr[1]]]
    y  <- df[[pr[2]]]
    ux <- unique(x)
    uy <- unique(y)
    up <- unique(paste(x, y, sep = sep))
    length(ux) == length(uy) && length(up) == length(ux)
  }, logical(1))
  
  # return only the names of the true pairs, collapsed with “:”
  vapply(pairs[is_bij], paste, collapse = ":", FUN.VALUE = "")
}


clean_left_join <- function(df1, df2, by) {
  common <- intersect(names(df1), names(df2))
  to_drop <- setdiff(common, by)
  df_join <- df2 |>
    select(-all_of(to_drop)) |>
    right_join(df1, by = by)

  
  return(df_join)
}

data_type <- function(col, num = FALSE, threshold = 0.1) {
  if(is.numeric(col)) {
    if(!all(as.integer(col) == col) | mean(table(col) == 1) > threshold) {
      dtype <- if(num) 3 else "cont"
    } else if(n_distinct(col) == 2) {
      dtype <- if(num) 1 else "bin"
    } else {
      dtype <- if(num) 2 else "cat"
    }
  } else {
    if(n_distinct(col, na.rm = TRUE) == 2) {
      dtype <- if(num) 1 else "bin"
    } else {
      dtype <- if(num) 2 else "cat"
    }
  }

  return(dtype)
}

create_expected_types <- function(
  data_format = c("temporal_covid", "temporal_other", "static_poll", "static_other"),
  is_sample = TRUE,
  is_aggregated = FALSE
) {

  data_format <- match.arg(data_format)
  
  types <- list(
    sex  = "bin",
    race = "cat",
    age  = "cat"
  )
  
  if (data_format == "temporal_covid") types$zip <- "cat"
  if (data_format == "static_poll")   types$edu <- "cat"

  if (is_sample) {
    types$positive <- "ignore"
    if (is_aggregated) {
      if (data_format %in% c("temporal_covid", "temporal_other")) types$time  <- "cat"
    }
  }

  if (is_aggregated) {
    types$total <- "ignore"
  }
  
  return(types)
}

create_expected_levels <- function(
  data_format = c("temporal_covid", "temporal_other", "static_poll", "static_other")
) {
  
  data_format <- match.arg(data_format)

  levels <- list(
    static_poll = list(
      sex = c("male", "female"),
      race = c("white", "black", "other"),
      age = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"),
      edu = c("no hs", "hs", "some college", "4-year college", "post-grad")
    ),
    other = list(
      sex = c("male", "female"),
      race = c("white", "black", "other"),
      age = c("0-17", "18-34", "35-64", "65-74", "75+")
    )
  )

  switch(
    data_format,
    static_poll = levels$static_poll,
    levels$other
  )
}

check_data <- function(df, expected_types, na_threshold = 0.5) {
  expected_columns <- names(expected_types)
  
  # Check for missing columns
  missing <- setdiff(expected_columns, names(df))
  if(length(missing) > 0) {
    stop(paste0("The following columns are missing: ",
                  paste(missing, collapse = ", ")))

  }
  
  # Check data types
  types <- df |> select(all_of(expected_columns)) |> lapply(data_type) |> unlist()
  valid <- unlist(expected_types) == types
  valid[expected_types == "ignore"] <- TRUE
  
  if(any(!valid)) {
    stop(paste0("Columns corresponding to the following variables have inappropriate data types: ",
                paste(expected_columns[!valid], collapse = ", ")))
  }
  
  # Check for too many NAs
  na_percents <- df |>
    lapply(function(c) sum(as.numeric(is.na(c))) / length(c)) |>
    unlist()
  
  high_na_cols <- expected_columns[na_percents[expected_columns] > na_threshold]
  if(length(high_na_cols) > 0) {
    stop(paste0("Columns corresponding to the following variables have more than ",
                na_threshold * 100, "% rows with missing data: ",
                paste(high_na_cols, collapse = ", ")))
  }
  
  # Check date format
  if("time" %in% expected_columns) {
    if("date" %in% names(df)) {
      if (anyNA(as.Date(na.omit(df$date), optional = TRUE))) {
        warning("Provided dates are not in expected format. Plots will use week indices instead.")
      }
    } else {
      warning("Dates are not provided. Plots will use week indices instead.")
    }
  }
}

check_pstrat <- function(df, df_ref, expected_types) {
  if (is.null(df_ref)) {
    stop("Sample data is not provided.")
  }
  
  # ensure columns exist
  cols <- names(expected_types)
  missing_df  <- setdiff(cols, names(df))
  missing_ref <- setdiff(cols, names(df_ref))
  if (length(missing_df))  stop("Missing in sample data:  ", paste(missing_df, collapse = ", "))
  if (length(missing_ref)) stop("Missing in postratification data: ", paste(missing_ref, collapse = ", "))
  
  # compare unique values
  cond <- vapply(cols, function(col) {
    setequal(unique(df[[col]]), unique(df_ref[[col]]))
  }, logical(1))

  if (any(!cond)) {
    stop("The following columns have different unique values in sample and postratification data: ",
         paste(cols[!cond], collapse = ", "))
  }
}


# Process uploaded data 
preprocess <- function(
  data,
  data_format,
  zip_county_state,
  const,
  is_sample = TRUE,
  is_aggregated = TRUE
) {
  
  # set up flags
  covid <- data_format == "temporal_covid"
  need_time <- is_sample && data_format %in% c("temporal_covid", "temporal_other")
  levels <- create_expected_levels(data_format)
  indiv_vars <- names(levels)
  if (need_time) {
    indiv_vars <- c(indiv_vars, "time")
  }
  
  # Clean data
  data <- clean_data(data)
  
  # Find and rename columns
  data <- rename_columns(data, const, covid && !is_aggregated)
  
  # Check for common dataframe issues
  types <- create_expected_types(
    data_format = data_format,
    is_sample = is_sample,
    is_aggregated = is_aggregated
  )
  check_data(data, types)

  # Aggregate if needed
  if(!is_aggregated) {
    # remove NAs
    check_cols <- setdiff(names(data), indiv_vars)
    data <- data |> tidyr::drop_na(all_of(check_cols))

    # convert date to week indices if necessary
    if (need_time) {
      common <- intersect(names(data), c("date", "time"))
      if (length(common) == 1 && "date" %in% common) {
        # convert date to week indices
        c(time_indices, timeline) %<-% get_week_indices(data$date)
        data$time <- time_indices

        # add the column containing first dates of the weeks
        data <- data |>
          full_join(
            data.frame(
              time = 1:max(data$time),
              date = timeline |> as.character()
            ),
            by = "time"
          )
      } else if (length(common) == 0) {
        stop("No dates or week indices found.")
      }
    }

    # recode values to expected levels
    data <- recode_values(data, levels, covid)

    # impute missing demographic data based on frequency
    data <- data |> mutate(across(all_of(indiv_vars), impute))

    # aggregate test records based on combinations of factors
    smallest <- get_smallest_geo(names(data), const$vars$geo)
    smallest_geo <- if(!is.null(smallest)) smallest$geo else NULL
    group_vars <- c(indiv_vars, smallest_geo)
    geo_covars <- if(!is.null(smallest_geo)) names(get_geo_predictors(data, smallest_geo)) else NULL

    data <- data |>
      group_by(!!!syms(group_vars)) |>
      summarize(
        across(any_of(geo_covars), first),
        total = n(),
        positive = sum(positive)
      ) |>
      ungroup()
  }

  # append geographic areas at larger scales if missing
  data <- append_geo(data, zip_county_state, const$vars$geo)

  return(data)
}

create_variable_list <- function(input_data, covariates, vars_global) {
  # list of variables for model specification
  vars <- list(
    fixed = list(
      "Individual-level Predictor" = c(),
      "Geographic Predictor" = c()
    ),
    varying = list(
      "Individual-level Predictor" = c(),
      "Geographic Predictor" = c()
    ),
    omit = list(
      one_level = c(),
      nested = c()
    )
  )

  # Helper function to process variables and add them to appropriate lists
  add_variables <- function(group_name, var_names, data_source, vars) {
    for (v in var_names) {
      if (!is.null(data_source[[v]]) && n_distinct(data_source[[v]]) > 1) {
        if (data_type(data_source[[v]]) == "cat") {
          vars$varying[[group_name]] <- c(vars$varying[[group_name]], v)
        }
        vars$fixed[[group_name]] <- c(vars$fixed[[group_name]], v) 
      } else {
        # if the variable has only one level, add it to the omit list
        vars$omit$one_level <- c(vars$omit$one_level, v)
      }
    }
    return(vars)
  }

  # Process individual-level predictors
  indiv_vars <- setdiff(names(input_data), c(vars_global$geo, vars_global$ignore, names(covariates)))
  vars <- add_variables("Individual-level Predictor", indiv_vars, input_data, vars)

  # Process geographic predictors
  geo_vars <- names(covariates)
  vars <- add_variables("Geographic Predictor", geo_vars, covariates, vars)

  # Check for nested variables
  if (length(vars$varying[["Geographic Predictor"]]) >= 2) {
    vars$omit$nested <- combn(vars$varying[["Geographic Predictor"]], 2, simplify = FALSE) |>
      lapply(paste, collapse = ":") |>
      unlist()
  }

  return(vars)
}

combine_tracts <- function(
    tract_data,
    zip_tract,
    link_geo = c("", "zip", "county", "state")
) {

  link_geo <- match.arg(link_geo)

  if(link_geo == "zip") {
    # join tract-level data with zip-tract conversion table then group by zip
    by_zip <- zip_tract |>
      select(geoid, zip) |>
      rename("GEOID" = "geoid") |>
      inner_join(
        tract_data,
        by = "GEOID"
      ) |>
      group_by(zip)
    
    # compute zip-level population size by aggregating across overlapping tracts
    all_colnames <- names(tract_data)
    pstrat_colnames <- all_colnames[grepl("male|female", all_colnames)]
    pstrat_data <- by_zip |>
      summarise(
        across(all_of(pstrat_colnames), ~ sum(.x, na.rm = TRUE))
      ) |>
      rename("geocode" = "zip")
  } else if (link_geo == "county") {
    pstrat_data <- tract_data |>
      mutate(geocode = substr(GEOID, 1, 5)) |>
      select(-GEOID) |>
      group_by(geocode) |>
      summarize_all(sum)
  } else if (link_geo == "state") {
    pstrat_data <- tract_data |>
      mutate(geocode = substr(GEOID, 1, 2)) |>
      select(-GEOID) |>
      group_by(geocode) |>
      summarize_all(sum)
  } else {
    pstrat_data <- tract_data |>
      mutate(geocode = "place_holder") |>
      select(-GEOID) |>
      group_by(geocode) |>
      summarize_all(sum)
  }

  return(pstrat_data)
}

prepare_mrp_custom <- function(
  input_data,
  pstrat_data,
  fips_county_state,
  demo_levels,
  vars_global,
  link_geo = NULL,
  need_time = FALSE
) {

  # filter based on common GEOIDs
  shared_geocodes <- c()
  if(!is.null(link_geo)) {
    shared_geocodes <- intersect(unique(input_data[[link_geo]]), unique(pstrat_data[[link_geo]]))
    input_data <- input_data |> filter(!!sym(link_geo) %in% shared_geocodes)
    pstrat_data <- pstrat_data |> filter(!!sym(link_geo) %in% shared_geocodes)
  }

  # create lists of all factor levels
  n_time_indices <- 1
  levels <- demo_levels
  if(need_time) {
    levels$time <- unique(input_data$time) |> sort()
    n_time_indices <- length(levels$time)
  }
  if(!is.null(link_geo)) {
    levels[[link_geo]] <- shared_geocodes
  }

  # convert demographic levels to factors
  new_data <- pstrat_data |> as_factor(demo_levels)

  # append geographic predictors
  covariates <- NULL
  if(!is.null(link_geo)) {
    # find geographic covariates
    covariates <- get_geo_predictors(input_data, link_geo)
    if(ncol(covariates) > 1) {
      new_data <- clean_left_join(new_data, covariates, by = link_geo)
    }
  }

  # append levels for other geographic predictors
  for(v in intersect(names(new_data), vars_global$geo)) {
    levels[[v]] <- unique(new_data[[v]]) |> sort()
  }

  # duplicate rows for each time index
  new_data <- purrr::map_dfr(
    seq_len(n_time_indices),
    ~ new_data |> mutate(time = .x)
  )

  vars <- create_variable_list(input_data, covariates, vars_global)

  return(list(
    input = input_data,
    new = new_data,
    levels = levels,
    vars = vars
  ))
}

prepare_mrp_acs <- function(
    input_data,
    tract_data,
    zip_tract,
    zip_county_state,
    demo_levels,
    vars_global,
    link_geo = NULL,
    need_time = FALSE
) {

  # create poststratification table
  pstrat_data <- combine_tracts(tract_data, zip_tract, link_geo)

  # filter based on common GEOIDs
  shared_geocodes <- c()
  if(!is.null(link_geo)) {
    shared_geocodes <- intersect(unique(input_data[[link_geo]]), pstrat_data$geocode)
    input_data <- input_data |> filter(!!sym(link_geo) %in% shared_geocodes)
    pstrat_data <- pstrat_data |> filter(geocode %in% shared_geocodes)
  }
  cell_counts <- pstrat_data |> select(-geocode) |> t() |> c()

  # create lists of all factor levels
  n_time_indices <- 1
  levels <- demo_levels
  if(need_time) {
    levels$time <- unique(input_data$time) |> sort()
    n_time_indices <- length(levels$time)
  }
  if(!is.null(link_geo)) {
    levels[[link_geo]] <- shared_geocodes
  }

  # IMPORTANT: for sorting data frame to match cell order of poststratification table
  sort_vars <- c("time", link_geo, "sex", "race", "age") |>
    intersect(names(levels))

  new_data <- expand.grid(levels, stringsAsFactors = TRUE) |> # sex, race, age must be factors for later use in plotting
    arrange(across(all_of(sort_vars))) |> # IMPORTANT: To match the cell order of poststratification data
    mutate(total = rep(cell_counts, n_time_indices))

  # append geographic predictors
  covariates <- NULL
  if(!is.null(link_geo)) {
    # find geographic covariates
    covariates <- get_geo_predictors(input_data, link_geo)
    if(ncol(covariates) > 1) {
      new_data <- left_join(new_data, covariates, by = link_geo)
    }
  }

  # append levels for other geographic predictors
  # NOTE: this must be done after new_data is created
  # as these levels are not used in the poststratification table
  for(v in intersect(names(new_data), vars_global$geo)) {
    levels[[v]] <- unique(new_data[[v]]) |> sort()
  }

  vars <- create_variable_list(input_data, covariates, vars_global)

  return(list(
    input = input_data,
    new = new_data,
    levels = levels,
    vars = vars
  ))
}


stan_factor <- function(df, ignore_columns = NULL) {
  # Get column names
  col_names <- setdiff(names(df), ignore_columns)

  # Loop through columns and check data type
  for (col in col_names) {
    dtype <- data_type(df[[col]])
    if(dtype == "bin" | dtype == "cat") {
      # Rename raw column
      raw_col <- paste0(col, "_raw")
      df <- df |> rename(!!raw_col := !!col)
      
      # Create new numeric factor column
      df[[col]] <- df[[raw_col]] |> factor() |> as.numeric()

      if(dtype == "bin") {
        df[[col]] <- df[[col]] - 1
      }
    } else if(dtype == "cont") {
      # Standardize continuous data
      df[[col]] <- scale(df[[col]]) |> array()

    }
  }

  return(df)
}


create_zip_county_state <- function(zip_tract, fips_county_state) {
  # find the most common county for each zip code
  zip_county_state <- zip_tract |>
    mutate(county_fips = substr(geoid, 1, 5)) |>
    group_by(zip) |>
    summarise(
      fips = names(which.max(table(county_fips)))
    ) |>
    left_join(
      fips_county_state,
      by = "fips"
    )

  return(zip_county_state)
}

create_fips_county_state <- function(zip_county_state, for_plotting = FALSE) {
  fips_county_state <- zip_county_state |>
    select(fips, county, state, state_name) |>
    distinct()

  if(for_plotting) {
    fips_county_state <- fips_county_state |> mutate(
      state = toupper(state),
      state_name = tools::toTitleCase(state_name),
      county = tools::toTitleCase(county)
    )
  }

  return(fips_county_state)
}

aggregate_fips <- function(df) {

  df <- df |>
    mutate(fips = substr(fips, 1, 2)) |>
    select(-county) |>
    distinct(fips, .keep_all = TRUE)

  return(df)
}
