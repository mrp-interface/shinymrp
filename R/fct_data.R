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

clean_names <- function(names) {
  names |> 
    tolower() |> 
    gsub("[0-9]", "_", x = _) |>
    gsub("[^[:alpha:]]", "_", x = _) |>
    gsub("_{2,}", "_", x = _) |>
    gsub("^_|_$", "", x = _)
}

clean_data <- function(
    df,
    na_threshold = 0.5,
    na_strings = c("", "na", "n/a", "none", "null", "unknown")
) {
  
  # Clean column names
  names(df) <- clean_names(names(df))
  
  # Convert common NA strings to actual NA
  df <- df |> 
    mutate(across(everything(), 
                 ~if_else(tolower(as.character(.x)) %in% na_strings, NA, .x)))
  
  # Remove columns with too many NAs
  na_proportions <- colMeans(is.na(df))
  df <- df |> select(which(na_proportions < na_threshold))
  
  # Convert character columns to lowercase and trim whitespace
  df <- df |> 
    mutate(across(where(is.character), 
                 ~str_trim(tolower(.x))))

  # Fix ZIP codes
  df <- fix_geocode(df)
  
  return(df)
}

find_columns <- function(df, expected_columns) {
  # find columns using string search
  df <- clean_names(df)
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

to_lower_case <- function(df) {
  # Convert only character columns to lowercase
  df |> 
    mutate(
      across(
        where(is.character),
        tolower
      )
    )
}

to_factor <- function(values, levels, other = NA) {
  for(lvl in levels) {
    values[grepl(lvl, values, ignore.case = TRUE)] <- lvl
  }

  values[!values %in% levels] <- other

  return(values)
}

as_factor <- function(df, levels) {
  for(lvl in names(levels)) {
    if(lvl %in% names(df)) {
      df[[lvl]] <- factor(df[[lvl]], levels = levels[[lvl]])
    }
  }

  return(df)
}


filter_geojson <- function(geojson, geoids, omit = FALSE) {
  if(is.null(geojson) | is.null(geoids)) {
    return(NULL)
  }
  
  all_areas <- geojson$features
  geojson$features <- if(omit) {
    purrr::keep(all_areas, function(c) !c$properties$GEOID %in% geoids)
  } else {
    purrr::keep(all_areas, function(c) c$properties$GEOID %in% geoids)
  }

  return(geojson)
}

fix_geocode <- function(df) {
  if("zip" %in% names(df) & is.numeric(df$zip)) {
    df$zip <- sprintf("%05d", df$zip)
  }

  if("county" %in% names(df)) {
    if(is.numeric(df$county)) {
      df$county <- sprintf("%05d", df$county)
    }
  }

  if("state" %in% names(df)) {
    if(is.numeric(df$state)) {
      df$state <- sprintf("%02d", df$state)
    }
  }

  return(df)
}

to_geocode <- function(vec, fips_county_state, link_geo = c("state", "county", "zip")) {
  link_geo <- match.arg(link_geo)

  if(link_geo == "zip") {
    return(sprintf("%05d", as.numeric(vec)))
  }

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

append_geo <- function(input_data, zip_county_state, link_geo, vars_global) {
  # Convert geographic identifiers to standard format
  input_data[[link_geo]] <- to_geocode(
    input_data[[link_geo]], 
    create_fips_county_state(zip_county_state), 
    link_geo
  )
  
  # Get geographic variables at current and larger scales
  geo_vars <- vars_global$geo[which(vars_global$geo == link_geo):length(vars_global$geo)]

  # Prepare geographic crosswalk
  zip_county_state <- zip_county_state |>
    select(zip, fips) |>
    rename(county = fips) |>
    mutate(state = substr(county, 1, 2)) |>
    select(all_of(geo_vars)) |>
    distinct()
  
  # Join geographic variables
  input_data <- input_data |>
    select(-all_of(setdiff(geo_vars, link_geo))) |>
    left_join(zip_county_state, by = link_geo)

  return(input_data)
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
    if(n_distinct(col) == 2) {
      dtype <- if(num) 1 else "bin"
    } else {
      dtype <- if(num) 2 else "cat"
    }
  }

  return(dtype)
}

check_var_data <- function(col) {
  pass <- TRUE
  
  if(n_distinct(col) < 2) {
    pass <- FALSE
  }
  
  return(pass)
}


check_data <- function(df,
    expected_types,
    na_threshold = 0.5,
    na_strings = c("", "na", "n/a", "none", "null", "unknown")
) {
  
  errors <- list()
  warnings <- list()
  expected_columns <- names(expected_types)

  # check if all expected columns are present
  missing <- setdiff(expected_columns, names(df))
  if(length(missing) > 0) {
    errors$missing_columns <- paste0("The following columns are missing: ",
                                    paste(missing, collapse = ", "))
  } else {
    # check data types
    # Remove columns marked as "ignore" from expected_types
    types <- df |> select(all_of(expected_columns)) |> lapply(data_type) |> unlist()
    valid <- unlist(expected_types) == types
    valid[expected_types == "ignore"] <- TRUE

    if(any(!valid)) {
      errors$invalid_type <- paste0("Columns corresponding to the following variables have inappropriate data types: ",
                                 paste(expected_columns[!valid], collapse = ", "))
    } else {
      na_percents <- df |>
        lapply(function(c) sum(as.numeric(is.na(c))) / length(c)) |>
        unlist()

      if(any(na_percents > na_threshold)) {
        errors$na <- paste0("Columns corresponding to the following variables have more than ",
                            na_threshold * 100, " percent rows with missing data: ",
                            paste(expected_columns[na_percents > na_threshold], collapse = ", "))
      }
    }
  }

  if("time" %in% expected_columns) {
    if("date" %in% names(df)) {
      if (anyNA(as.Date(na.omit(df$date), optional = TRUE))) {
        warnings$date_format <- "Provided dates are not in expected format. Plots will use week indices instead."
      }
    } else {
      warnings$missing_date <- "Dates are not provided. Plots will use week indices instead."
    }
  }

  return(list(errors, warnings))
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
    )
  )

  # add individual-level predictors
  indiv_vars <- setdiff(names(input_data), c(vars_global$geo, vars_global$ignore, names(covariates)))
  for(v in indiv_vars) {
    if(check_var_data(input_data[[v]])) {
      if(data_type(input_data[[v]]) == "cat") {
        vars$varying$"Individual-level Predictor" <- c(vars$varying$"Individual-level Predictor", v)
      }
      vars$fixed$"Individual-level Predictor" <- c(vars$fixed$"Individual-level Predictor", v) 
    }
  }

  # add geographic predictors
  geo_vars <- intersect(vars_global$geo, names(input_data)) |> union(names(covariates))
  for(v in geo_vars) {
    if(check_var_data(covariates[[v]])) {
      if(data_type(covariates[[v]]) == "cat") {
        vars$varying$"Geographic Predictor" <- c(vars$varying$"Geographic Predictor", v)
      }
      vars$fixed$"Geographic Predictor" <- c(vars$fixed$"Geographic Predictor", v) 
    }
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

prepare_data <- function(
    input_data,
    tract_data,
    zip_tract,
    zip_county_state,
    demo_levels,
    vars_global,
    link_geo
) {

  if(!is.null(link_geo)) {
    # append geographic identifiers at larger scales
    input_data <- append_geo(input_data, zip_county_state, link_geo, vars_global)
  }

  # create poststratification table
  pstrat <- combine_tracts(tract_data, zip_tract, link_geo)
  shared_geocodes <- c()
  if(!is.null(link_geo)) {
    shared_geocodes <- intersect(unique(input_data[[link_geo]]), pstrat$geocode)
    input_data <- input_data |> filter(!!sym(link_geo) %in% shared_geocodes)
    pstrat <- pstrat |> filter(geocode %in% shared_geocodes)
  }
  cell_counts <- pstrat |> select(-geocode) |> t() |> c()

  # create lists of all factor levels
  n_time_indices <- 1
  levels <- demo_levels
  if("time" %in% names(input_data)) {
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

  return(list(input_data, new_data, levels, vars))
}


stan_factor <- function(df, ignore_columns) {
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

### FOR GENERATING STATIC DATA ###
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
