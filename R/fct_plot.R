#' Convert FIPS codes to uppercase format
#'
#' @description Converts state and county names in FIPS data to proper case formatting
#' for consistent display in plots and maps.
#'
#' @param fips A data frame containing FIPS codes with columns for state, state_name,
#'   and optionally county
#'
#' @return A data frame with state names converted to uppercase, state_name to title case,
#'   and county names to title case if present
#' @noRd
fips_upper <- function(fips) {
  has_county <- "county" %in% names(fips)

  fips %>% dplyr::mutate(
    state = toupper(.data$state),
    state_name = tools::toTitleCase(.data$state_name),
    county = if(has_county) tools::toTitleCase(.data$county)
  )
}

#' Prepare sample size data for visualization
#'
#' @description Aggregates input data by geographic unit and prepares it for plotting
#' as either map visualizations or tabular displays. Calculates counts and percentages
#' by geographic area.
#'
#' @param input_data A data frame containing survey data with columns for geographic
#'   identifiers and total counts
#' @param fips_codes A data frame containing FIPS codes and geographic names
#' @param geo Character string specifying geographic level, either "county" or "state"
#' @param for_map Logical indicating whether to format data for map display (TRUE) or
#'   tabular display (FALSE)
#'
#' @return A data frame formatted for visualization with geographic identifiers, counts,
#'   percentages, and hover text for maps or cleaned names for tables
#' @noRd
prep_sample_size <- function(
  input_data,
  fips_codes,
  geo,
  for_map = TRUE
) {
  
  checkmate::assert_choice(
    geo,
    choices = GLOBAL$vars$geo2,
    null.ok = FALSE
  )

  if(is.null(input_data)) {
    return(NULL)
  }

  input_data <- input_data %>% dplyr::mutate(fips = input_data[[geo]])
  fips_codes <- fips_codes %>% fips_upper()
  
  total_count <- sum(input_data$total)
  plot_df <- input_data %>%
    dplyr::group_by(.data$fips) %>%
    dplyr::summarize(
      count = sum(.data$total),
      perc = (sum(.data$total) / total_count) * 100
    ) %>%
    dplyr::left_join(fips_codes, by = "fips")

  if(for_map) {
    if(geo == "state") {
      plot_df <- plot_df %>% dplyr::mutate(
        value = .data$count,
        hover = sprintf("%s: %d (%.2f%%)",
                        .data$state, .data$count, .data$perc)
      )
    } else {
      plot_df <- plot_df %>% dplyr::mutate(
        value = .data$count,
        hover = sprintf("%s (%s): %d (%.2f%%)",
                        .data$county, .data$state, .data$count, .data$perc)
      )
    }
  } else {
    if(geo == "county") {
      plot_df <- plot_df %>% dplyr::mutate(county = gsub(" [Cc][Oo][Uu][Nn][Tt][Yy]", "", .data$county))
    }
    
    plot_df <- plot_df %>%
      dplyr::select(-c(.data$fips, .data$perc, .data$state_name)) %>%
      dplyr::select(-.data$count, .data$count) %>%
      dplyr::arrange(dplyr::desc(.data$count))
  }


  return(plot_df)
}

#' Prepare Raw Data for Plotting
#'
#' @description This function processes input data for geographic visualization by computing
#' summary statistics, joining with FIPS codes, and generating appropriate
#' titles and hover text based on the data characteristics.
#'
#' @param input_data A data frame containing the raw data to be processed.
#'   Should contain geographic identifiers and outcome variables.
#' @param fips_codes A data frame containing FIPS codes and geographic names
#'   for joining with input data.
#' @param geo Character string specifying geographic level. One of "county" or "state".
#' @param summary_type Character string specifying summary statistic for time series data.
#'   One of "max" or "min".
#' @param metadata A list containing metadata about the data structure with elements:
#'   \itemize{
#'     \item `is_timevar`: Logical indicating if data has time dimension
#'     \item `family`: Character specifying data family ("binomial" or "normal")
#'   }
#'
#' @return A list containing:
#'   \itemize{
#'     \item `plot_df`: Processed data frame ready for plotting with columns for
#'       fips, value, geographic names, and hover text
#'     \item `title`: List containing main_title and hover_title for the plot
#'   }
#'   Returns NULL if input_data is NULL.
#' @noRd
prep_raw <- function(
  input_data,
  fips_codes,
  geo,
  summary_type,
  metadata
) {

  checkmate::assert_choice(
    geo,
    choices = GLOBAL$vars$geo2,
    null.ok = FALSE
  )
  
  checkmate::assert_choice(
    summary_type,
    choices = GLOBAL$args$summary_types,
    null.ok = TRUE
  )
  
  if(is.null(input_data)) {
    return(NULL)
  }

  input_data <- input_data %>% dplyr::mutate(fips = input_data[[geo]])
  fips_codes <- fips_codes %>% fips_upper()

  # compute weekly average
  group_cols <- if(metadata$is_timevar) c("fips", "time") else c("fips")
  plot_df <- input_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarize(
      value = switch(metadata$family,
        "binomial" = sum(.data$positive) / sum(.data$total),
        "normal"   = mean(.data$outcome)
      )
    )

  if (metadata$is_timevar) {
    summary_type <- replace_null(summary_type, "max")

    summary_fn <- switch(summary_type,
      "max" = max,
      "min" = min
    )

    plot_df <- plot_df %>%
      dplyr::group_by(.data$fips) %>%
      dplyr::summarize(
        value = summary_fn(.data$value)
      )
  }
  
  # construct hover text based on geographic level
  plot_df <- plot_df %>%
    dplyr::left_join(fips_codes, by = "fips") %>%
    dplyr::mutate(
      hover = switch(geo,
        "state" = paste0(.data$state, ": "),
        "county" = paste0(.data$county, " (", .data$state, "): ")
      )
    ) %>%
    dplyr::mutate(
      hover = paste0(.data$hover, round(.data$value, 4))
    )


  # map titles
  title <- list()
  main <- switch(metadata$family,
    "binomial" = "Positive Response Rate",
    "normal" = "Outcome Average"
  )
  prefix <- if (metadata$is_timevar) "Weekly " else ""
  suffix <- " by Geography"
  title$main_title <- paste0(prefix, main, suffix)

  title$hover_title <- if (metadata$is_timevar) {
    s <- switch(metadata$family,
      "binomial" = "Rate",
      "normal" = "Average"
    )
    switch(summary_type,
      "max" = stringr::str_interp("Highest Weekly ${s}"),
      "min" = stringr::str_interp("Lowest Weekly ${s}")
    )
  } else {
    switch(metadata$family,
      "binomial" = "Positive Response Rate",
      "normal" = "Outcome Average"
    )
  }

  return(list(
    plot_df = plot_df,
    title = title
  ))
}


#' Prepare model estimates for visualization
#'
#' @description Prepares model estimates for map visualization by joining with
#' geographic data and formatting hover text. Can filter to specific time points
#' for time-varying data.
#'
#' @param est_df A data frame containing model estimates with columns for factor
#'   (geographic identifier), est (estimate), std (standard error), and optionally time
#' @param fips_codes A data frame containing FIPS codes and geographic names
#' @param geo Character string specifying geographic level, either "county" or "state"
#' @param time_index Optional integer specifying which time point to filter to
#'   for time-varying estimates
#' @param interval Confidence interval or standard deviation for the estimates (default is 0.95)
#'
#' @return A data frame with estimates, geographic information, and formatted hover
#'   text for map visualization
#' @noRd
prep_est <- function(
    est_df,
    fips_codes,
    geo,
    time_index = NULL,
    interval = 0.95
) {
  
  checkmate::assert_choice(
    geo,
    choices = GLOBAL$vars$geo2,
    null.ok = FALSE
  )

  if(is.null(est_df)) {
    return(NULL)
  }

  out <- check_interval(interval)

  fips_codes <- fips_codes %>% fips_upper()
  
  if(!is.null(time_index)) {
    est_df <- est_df %>% dplyr::filter(.data$time == time_index)
  }

  plot_df <- est_df %>%
    dplyr::rename("fips" = "factor") %>%
    dplyr::left_join(fips_codes, by = "fips")

  # construct hover text
  plot_df <- plot_df %>%
    dplyr::mutate(
      hover_pre = if (geo == "state") {
        paste0(.data$state, ": ", round(.data$est, 4))
      } else {
        paste0(.data$county, " (", .data$state, "): ", round(.data$est, 4))
      },
      hover_post = if (out$is_ci) {
        paste0("<br/>", interval * 100, "% CI: ",
               round(.data$lower, 4), " - ", round(.data$upper, 4))
      } else {
        paste0(" \u00B1 ", round((.data$est - .data$lower), 4))
      },
      hover = paste0(.data$hover_pre, .data$hover_post),
      value = .data$est
    )

  return(plot_df)
}


#' Create demographic comparison plots
#'
#' @description Creates bar plots comparing demographic distributions between
#' input survey data and target population data. Can display as separate plots
#' or side-by-side comparison.
#'
#' @param input_data A data frame containing input survey data with demo and total columns
#' @param new_data A data frame containing target population data with demo and total columns
#' @param levels Character vector of demographic levels (currently unused in function)
#' @param separate Logical indicating whether to create separate plots (TRUE) or
#'   side-by-side comparison (FALSE)
#'
#' @return A ggplot object or patchwork object showing demographic comparisons
#' @noRd
plot_demographic <- function(
    input_data,
    new_data,
    levels,
    separate = TRUE
) {

  if(is.null(input_data) || is.null(new_data)) {
    return(NULL)
  }

  total_input <- sum(input_data$total)
  input <- input_data %>%
    dplyr::group_by(.data$demo) %>%
    dplyr::summarize(perc = sum(.data$total) / total_input)

  total_new <- sum(new_data$total)
  new <- new_data %>%
    dplyr::group_by(.data$demo) %>%
    dplyr::summarize(perc = sum(.data$total) / total_new)

  datasets <- c("Input Data", "Target Population")
  plot_df <- rbind(input, new) %>% dplyr::mutate(
    dataset = rep(datasets, each = nrow(input))
  )

  if(separate) {
    p1 <- ggplot2::ggplot(
      data = plot_df %>% dplyr::filter(.data$dataset == datasets[1]),
      ggplot2::aes(x = .data$demo, y = .data$perc)
    ) +
      ggplot2::geom_bar(
        stat = "identity",
        position = "dodge"
      ) +
      ggplot2::labs(
        title = datasets[1],
        caption = sprintf("Sample size: %s", format(total_input, big.mark = ","))
      )

    p2 <- ggplot2::ggplot(
      data = plot_df %>% dplyr::filter(.data$dataset == datasets[2]),
      ggplot2::aes(x = .data$demo,
          y = .data$perc)
    ) +
      ggplot2::geom_bar(
        stat = "identity",
        position = "dodge"
      ) +
      ggplot2::labs(
        title = datasets[2],
        caption = sprintf("Sample size: %s", format(total_new, big.mark = ","))
      )

    p <- patchwork::wrap_plots(p1, p2)

  } else {
    p <- ggplot2::ggplot(
      data = plot_df,
      ggplot2::aes(
        x = .data$demo,
        y = .data$perc,
        fill = .data$dataset
      )
    ) +
      ggplot2::geom_bar(
        stat = "identity",
        position = "dodge"
      )
  }

  p <- p &
    ggplot2::scale_x_discrete(
      labels = tools::toTitleCase
    ) &
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      limits = c(0, 1),
      expand = c(0, 0)
    ) &
    ggplot2::labs(x = "", y = "")

  return(p)
}


#' Create geographic covariate distribution plots
#'
#' @description Creates histogram plots showing the distribution of geographic
#' covariates across zip codes or other geographic units.
#'
#' @param covariates A data frame containing covariate values with a covar column
#' @param breaks Numeric vector specifying histogram break points
#' @param description Character string providing a description of the covariate
#' @param definition Character string providing the definition of the covariate
#' @param name Character string specifying the name/label for the x-axis
#'
#' @return A ggplot object showing the covariate distribution histogram
#' @noRd
plot_geographic <- function(
    covariates,
    breaks,
    description,
    definition,
    name
) {

  if(is.null(covariates)) {
    return(NULL)
  }

  p <- ggplot2::ggplot(
    data = covariates,
    ggplot2::aes(x = .data$covar)
  ) +
    ggplot2::geom_histogram(breaks = breaks)
    
  # Extract bin data from ggplot object
  plotdata <- ggplot2::ggplot_build(p)
  histogram_data <- plotdata$data[[1]]

  # Set minimum break width to 1
  if (max(histogram_data$count) < 3) {
    p <- p +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, .1)),
      breaks = scales::breaks_width(1)
    )
  } else {
    p <- p +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, .1))
    )
  }
    
  p <- p +
    ggplot2::scale_x_continuous(
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = "",
      subtitle = description,
      caption = definition,
      x = name, y = "Number of zip codes"
    ) +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = 16, hjust = 0),
      plot.caption = ggplot2::element_text(size = 16, hjust = 0.5)
    )

  return(p)
}

#' Create prevalence time series plots
#'
#' @description Creates line plots showing prevalence over time, with optional
#' model estimates and uncertainty bands. Supports both raw data and MRP estimates.
#'
#' @param raw A data frame containing raw survey data with time, positive, and total columns
#' @param yrep_est Optional data frame containing model estimates with time, est, and std columns
#' @param dates Optional character vector of date labels for x-axis
#' @param metadata A list containing metadata
#' @param interval Confidence interval or standard deviation for the estimates (default is 0.95)
#' @param show_caption Logical indicating whether to show uncertainty caption
#'
#' @return A ggplot object showing prevalence time series with optional estimates
#' @noRd
plot_outcome_timevar <- function(
  raw,
  yrep_est = NULL,
  dates = NULL,
  metadata = NULL,
  interval = 0.95,
  show_caption = FALSE,
  config = GLOBAL$plot
) {

  if(is.null(raw)) {
    return(NULL)
  }

  out <- check_interval(interval)

  # compute weekly rates/averages
  plot_df <- raw %>%
    dplyr::group_by(.data$time) %>%
    dplyr::summarize(
      raw = switch(metadata$family,
        "binomial" = sum(.data$positive) / sum(.data$total),
        "normal" = mean(.data$outcome)
      )
    )

  # ensure missing time points are included
  plot_df <- plot_df %>%
    dplyr::right_join(
      data.frame(time = 1:max(raw$time, na.rm = TRUE)),
      by = "time"
    )

  if(!is.null(yrep_est)) {
    plot_df <- plot_df %>%
      dplyr::left_join(yrep_est, by = "time")

    if (metadata$family == "binomial") {
      # ensure bounds are non-negative for binomial family
      plot_df$lower[plot_df$lower < 0] <- 0
    }
  }

  p <- ggplot2::ggplot(
    data = plot_df,
    ggplot2::aes(x = .data$time)
  ) +
    ggplot2::geom_line(
      ggplot2::aes(
        y = .data$raw,
        color = "Raw"
      ),
      linewidth = 1.5
    )

  if(!is.null(yrep_est)) {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(
          y = .data$est,
          color = "MRP"
        ),
        linewidth = 1.5
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          y = .data$est,
          ymin = .data$lower,
          ymax = .data$upper
        ),
        fill = config$mrp_color,
        alpha = 0.5
      )
  }

  step <- max(1, floor(max(raw$time, na.rm = TRUE) / 15))
  xticks <- seq(1, max(raw$time, na.rm = TRUE), step)
  xticklabels <- if(!is.null(dates)) dates[xticks] else xticks

  p <- p +
    ggplot2::labs(
      title = "",
      x = if(is.null(dates)) "Week index" else "",
      y = if(is.null(yrep_est)) {
        switch(metadata$family,
          "binomial" = "Positive response rate",
          "normal" = "Outcome average"
        )
      } else {
        switch(metadata$family,
          "binomial" = "Proportion estimates",
          "normal" = "Mean estimates"
        )
      },
      caption = if(show_caption) {
        if (out$is_ci) {
          sprintf("*The shaded areas represent %s%% confidence intervals", interval * 100)
        } else {
          "*The shaded areas represent \u00B11 SD of uncertainty"
        }
      } else {
        NULL
      }
    ) +
    ggplot2::scale_x_continuous(
      breaks = xticks,
      labels = xticklabels,
      expand = c(0, 0.1)
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(5e-3, 0.1))
    ) +
    ggplot2::scale_color_manual(
      values = c("Raw" = config$raw_color, "MRP" = config$mrp_color)
    ) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = if(is.null(yrep_est)) "none" else "bottom"
    )

  return(p)

}

#' Create support comparison plots
#'
#' @description Creates point plots with error bars comparing raw support rates
#' with model estimates and uncertainty intervals.
#'
#' @param raw A data frame containing raw survey data with positive and total columns
#' @param yrep_est A data frame containing model estimates with data, lower, median,
#'   and upper columns
#' @param metadata A list containing metadata
#' @param interval Confidence interval or standard deviation for the estimates (default is 0.95)
#' @param show_caption Logical indicating whether to show uncertainty caption
#'
#' @return A ggplot object showing support comparison with error bars
#' @noRd
plot_outcome_static <- function(
    raw,
    yrep_est = NULL,
    metadata = NULL,
    interval = 0.95,
    show_caption = FALSE
) {
  if(is.null(raw)) {
    return(NULL)
  }

  out <- check_interval(interval)

  raw_mean <- switch(metadata$family,
    "binomial" = sum(raw$positive) / sum(raw$total),
    "normal" = mean(raw$outcome)
  )

  raw <- data.frame(
    data = "Raw",
    lower = raw_mean,
    median = raw_mean,
    upper = raw_mean
  )

  plot_df <- raw
  if (!is.null(yrep_est)) {
    yrep_est <- yrep_est %>%
      dplyr::mutate(
        data = "Estimate",
        lower = .data$lower,
        median = .data$est,
        upper = .data$upper
      ) %>%
      dplyr::select(.data$data, .data$lower, .data$median, .data$upper)

    plot_df <- rbind(raw, yrep_est) %>%
      dplyr::mutate(data = factor(.data$data, levels = c("Raw", "Estimate")))
  }

  p <- ggplot2::ggplot(data = plot_df) +
    ggplot2::geom_point(
      ggplot2::aes(x = .data$data, y = .data$median),
      size = GLOBAL$plot$point_size
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(x = .data$data, ymin = .data$lower, ymax = .data$upper),
      size = GLOBAL$plot$errorbar_size,
      width = GLOBAL$plot$errorbar_width
    ) +
    ggplot2::labs(
      x = "",
      y = switch(metadata$family,
        "binomial" = "Proportion estimates",
        "normal" = "Mean estimates"
      ),
      caption = if(show_caption) {
        if (out$is_ci) {
          sprintf("*The error bars represent %s%% confidence intervals", interval * 100)
        } else {
          "*The error bars represent \u00B11 SD of uncertainty"
        }
      } else {
        NULL
      }
    )

  return(p)
}

#' Create COVID posterior predictive check subset plots
#'
#' @description Creates line plots comparing raw COVID data with multiple
#' posterior predictive replications over time. Shows individual replication
#' trajectories for model validation.
#'
#' @param yrep A data frame containing posterior predictive replications with
#'   time column and multiple replication columns
#' @param raw A data frame containing raw survey data with time, positive, and total columns
#' @param dates Optional character vector of date labels for x-axis
#' @param metadata A list containing metadata
#'
#' @return A ggplot object showing posterior predictive check with multiple replications
#' @noRd
plot_ppc_timevar_subset <- function(
    yrep,
    raw,
    dates,
    metadata,
    config = GLOBAL$plot
) {
  if(is.null(yrep) || is.null(raw)) {
    return(NULL)
  }

  raw <- raw %>%
    dplyr::group_by(.data$time) %>%
    dplyr::summarize(
      prev = switch(metadata$family,
        "binomial" = sum(.data$positive) / sum(.data$total),
        "normal" = mean(.data$outcome)
      )
    )

  yrep <- yrep %>% tidyr::pivot_longer(
    cols = setdiff(names(yrep), "time"),
    names_to = "name",
    values_to = "value"
  )

  step <- max(1, floor(max(raw$time, na.rm = TRUE) / 15))
  xticks <- seq(1, max(raw$time, na.rm = TRUE), step)
  xticklabels <- if(!is.null(dates)) dates[xticks] else xticks

  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = raw,
      ggplot2::aes(
        x = .data$time,
        y = .data$prev,
        color = "Raw"
      ),
      linewidth = 2
    ) +
    ggplot2::geom_line(
      data = yrep,
      ggplot2::aes(
        x = .data$time,
        y = .data$value,
        group = .data$name,
        color = "Replicated"
      ),
      alpha = 0.8
    ) +
    ggplot2::labs(
      title = "",
      x = if(is.null(dates)) "Week index" else "",
      y = switch(metadata$family,
        "binomial" = "Positive response rate",
        "normal" = "Outcome average"
      )
    ) +
    ggplot2::scale_x_continuous(
      breaks = xticks,
      labels = xticklabels,
      expand = c(0, 0.1)
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(5e-3, 0.1))
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Raw" = config$raw_color,
        "Replicated" = config$yrep_color
      )
    ) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank()
    )

}

#' Create COVID posterior predictive check aggregate plots
#'
#' @description Creates line plots comparing raw COVID data with aggregated
#' posterior predictive statistics (median and uncertainty bands) over time.
#' Shows summary of model fit quality.
#'
#' @param yrep A data frame containing posterior predictive summary statistics
#'   with time, median, lower, and upper columns
#' @param raw A data frame containing raw survey data with time, positive, and total columns
#' @param dates Optional character vector of date labels for x-axis
#' @param metadata A list containing metadata
#'
#' @return A ggplot object showing posterior predictive check with summary statistics
#' @noRd
plot_ppc_timevar_all <- function(
    yrep,
    raw,
    dates,
    metadata,
    config = GLOBAL$plot
) {

  if(is.null(yrep) || is.null(raw)) {
    return(NULL)
  }

  plot_df <- raw %>%
    dplyr::group_by(.data$time) %>%
    dplyr::summarise(
      prev = sum(.data$positive) / sum(.data$total)
    ) %>%
    dplyr::right_join(
      data.frame(time = 1:max(raw$time, na.rm = TRUE)),
      by = "time"
    ) %>%
    dplyr::left_join(yrep, by = "time")

  plot_df$lower[plot_df$lower < 0] <- 0


  step <- max(1, floor(max(raw$time, na.rm = TRUE) / 15))
  xticks <- seq(1, max(raw$time, na.rm = TRUE), step)
  xticklabels <- if(!is.null(dates)) dates[xticks] else xticks

  p <- ggplot2::ggplot(
    data = plot_df,
    ggplot2::aes(x = .data$time)
  ) +
    ggplot2::geom_line(
      ggplot2::aes(
        y = .data$prev,
        color = "Raw"
      ),
      linewidth = 1.5
    ) +
    ggplot2::geom_line(
      ggplot2::aes(
        y = .data$median,
        color = "Replicated"
      ),
      linewidth = 1.5
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        y = .data$median,
        ymin = .data$lower,
        ymax = .data$upper
      ),
      fill = config$yrep_color,
      alpha = 0.5
    ) +
    ggplot2::labs(
      title = "",
      x = if(is.null(dates)) "Week index" else "",
      y = switch(metadata$family,
        "binomial" = "Positive response rate",
        "normal" = "Outcome average"
      )
    ) +
    ggplot2::scale_x_continuous(
      breaks = xticks,
      labels = xticklabels,
      expand = c(0, 0.1)
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(5e-3, 0.1))
    ) +
    ggplot2::scale_color_manual(
      values = c("Raw" = config$raw_color, "Replicated" = config$yrep_color)
    ) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank()
    )

  return(p)
}

#' Create poll posterior predictive check plots
#'
#' @description Creates point plots comparing raw poll support rates with
#' posterior predictive replications for cross-sectional polling data validation.
#'
#' @param yrep Numeric vector containing posterior predictive replication values
#' @param raw A data frame containing raw survey data with positive and total columns
#' @param metadata A list containing metadata
#'
#' @return A ggplot object showing posterior predictive check for polling data
#' @noRd
plot_ppc_static <- function(
    yrep,
    raw,
    metadata = NULL,
    config = GLOBAL$plot
) {
  if(is.null(yrep) || is.null(raw)) {
    return(NULL)
  }

  plot_df <- rbind(
    data.frame(
      name = "Replicated",
      value = yrep
    ),
    data.frame(
      name = "Raw",
      value = switch(metadata$family,
        "binomial" = sum(raw$positive) / sum(raw$total),
        "normal" = mean(raw$outcome)
      )
    )
  )

  ggplot2::ggplot(data = plot_df) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data$name,
        y = .data$value,
        color = .data$name,
        shape = .data$name
      ),
      size = GLOBAL$plot$point_size
    ) +
    ggplot2::labs(
      x = "",
      y = switch(metadata$family,
        "binomial" = "Positive response rate",
        "normal" = "Outcome average"
      )
    ) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank()
    )
}

#' Create time-varying estimate plots
#'
#' @description Creates multi-panel line plots showing model estimates over time
#' for different factor levels. Includes an overview plot and individual plots
#' with uncertainty bands for each factor level.
#'
#' @param plot_df A data frame containing time-varying estimates with factor, time, est, and std columns
#' @param dates Optional character vector of date labels for x-axis
#' @param metadata A list containing metadata
#' @param interval Confidence interval or standard deviation for the estimates (default is 0.95)
#' @param show_caption Logical indicating whether to show uncertainty caption
#'
#' @return A patchwork object containing multiple ggplot panels showing time-varying estimates
#' @noRd
plot_est_timevar <- function(
    plot_df,
    dates,
    metadata = NULL,
    interval = 0.95,
    show_caption = TRUE
) {
  if(is.null(nullify(plot_df))) {
    return(NULL)
  }

  out <- check_interval(interval)

  levels <- unique(plot_df$factor) %>% sort()
  labels <- levels %>% as.character() %>% tools::toTitleCase()

  colors <- RColorBrewer::brewer.pal(8, "Set1")[1:length(levels)]
  step <- max(1, floor(max(plot_df$time, na.rm = TRUE) / 15))
  xticks <- seq(1, max(plot_df$time, na.rm = TRUE), step)
  xticklabels <- if(!is.null(dates)) dates[xticks] else xticks

  if(metadata$family == "binomial") {
    # ensure bounds are non-negative for binomial family
    plot_df$lower[plot_df$lower < 0] <- 0
  }
  config <- list(
    limits = c(min(plot_df$lower), max(plot_df$upper)),
    expand = switch(metadata$family,
      "binomial" = c(5e-3, 0.1),
      c(0.1, 0.1)
    )
  )
  


  plot_list <- list()
  i = 1

  plot_list[[i]] <- ggplot2::ggplot(
    data = plot_df,
    ggplot2::aes(
      x = .data$time,
      y = .data$est,
      group = .data$factor
    )
  ) +
    ggplot2::geom_line(
      ggplot2::aes(colour = .data$factor),
      alpha = 0.8,
      linewidth = 1.5
    ) +
    ggplot2::scale_color_manual(
      values = colors,
      labels = labels
    )

  for(level in levels) {
    i <- i + 1
    plot_list[[i]] <- ggplot2::ggplot(
      data = plot_df %>% dplyr::filter(.data$factor == level),
      ggplot2::aes(
        x = .data$time,
        y = .data$est,
        group = .data$factor
      )
    ) +
      ggplot2::geom_line(
        ggplot2::aes(color = .data$factor),
        alpha = 0.8,
        linewidth = 1.5
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          fill = .data$factor,
          ymin = .data$lower,
          ymax = .data$upper
        ),
        alpha = 0.5
      ) +
      ggplot2::scale_color_manual(values = colors[i - 1], labels = labels[i -1]) +
      ggplot2::scale_fill_manual(values = colors[i - 1], labels = labels[i - 1])
  }

  for(i in 1:length(plot_list)) {
    plot_list[[i]] <- plot_list[[i]] +
      ggplot2::labs(
        title = "",
        x = if(is.null(dates)) "Week index" else "",
        y = switch(metadata$family,
          "binomial" = "Proportion estimates",
          "normal" = "Mean estimates"
        )
      ) +
      ggplot2::scale_x_continuous(
        breaks = xticks,
        labels = xticklabels,
        expand = c(0, 0.1)
      ) +
      ggplot2::scale_y_continuous(
        limits = config$limits,
        expand = ggplot2::expansion(mult = config$expand)
      ) +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        legend.position = "right",
        plot.margin = ggplot2::margin(0, 1, 0, 1, "cm")
      )
  }

  p <- patchwork::wrap_plots(
    plot_list,
    ncol = 1,
    nrow = length(levels) + 1
  ) +
    patchwork::plot_annotation(
      caption = if(show_caption) {
        if (out$is_ci) {
          sprintf("*The shaded areas represent %s%% confidence intervals", interval * 100)
        } else {
          "*The shaded areas represent \u00B11 SD of uncertainty"
        }
      } else {
        NULL
      }
    )

  return(p)
}

#' Create static estimate plots
#'
#' @description Creates point plots with error bars showing model estimates
#' for different factor levels in cross-sectional data.
#'
#' @param plot_df A data frame containing estimates with factor, est, and std columns
#' @param metadata A list containing metadata
#' @param interval Confidence interval or standard deviation for the estimates (default is 0.95)
#' @param show_caption Logical indicating whether to show uncertainty caption
#'
#' @return A ggplot object showing static estimates with error bars
#' @noRd
plot_est_static <- function(
  plot_df,
  metadata = NULL,
  interval = 0.95,
  show_caption = TRUE
) {
  if(is.null(nullify(plot_df))) {
    return(NULL)
  }

  out <- check_interval(interval)

  p <- ggplot2::ggplot(data = plot_df) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data$factor,
        y = .data$est
      ),
      size = GLOBAL$plot$point_size
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        x = .data$factor,
        ymin = .data$lower,
        ymax = .data$upper
      ),
      size = GLOBAL$plot$errorbar_size,
      width = GLOBAL$plot$errorbar_width
    ) +
    ggplot2::scale_x_discrete(
      labels = tools::toTitleCase
    ) +
    ggplot2::labs(
      x = "",
      y = switch(metadata$family,
        "binomial" = "Proportion estimates",
        "normal" = "Mean estimates"
      ),
      caption = if(show_caption) {
        if (out$is_ci) {
          sprintf("*The error bars represent %s%% confidence intervals", interval * 100)
        } else {
          "*The error bars represent \u00B11 SD of uncertainty"
        }
      } else {
        NULL
      }
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = if(dplyr::n_distinct(plot_df$factor) > 20) 90 else 0)
    )

  return(p)
}

#' Create choropleth maps
#'
#' @description Creates interactive choropleth maps using Highcharter for
#' visualizing geographic data with color-coded values and hover tooltips.
#'
#' @param plot_df A data frame containing geographic data with fips, value, and hover columns
#' @param geojson A geojson object containing geographic boundaries
#' @param geo Character string specifying geographic level (used for context)
#' @param config Optional list containing minValue and maxValue for color scale limits
#'
#' @return A highcharter map object showing choropleth visualization
#' @noRd
choro_map <- function(
    plot_df,
    geojson,
    geo,
    config 
) {
  if(is.null(plot_df) || is.null(geojson)) {
    return(NULL)
  }

  # build the chart
  hc <- highcharter::highchart(type = "map") %>%
    highcharter::hc_add_series_map(
      map        = geojson,
      df         = plot_df,
      joinBy     = c("fips", "fips"),
      value      = "value",
      name       = config$hover_title,
      dataLabels = list(enabled = FALSE, format = "{point.name}"),
      tooltip    = list(
        pointFormat = "{point.hover}"
      ),
      borderWidth= 0.1
    ) %>%
    highcharter::hc_title(
      text = config$main_title,
      align = "center",
      style = list(fontSize = "20px")
    ) %>%
    highcharter::hc_mapNavigation(enabled = TRUE) %>%
    highcharter::hc_boost(enabled = FALSE)

  if(!is.null(config$minValue) && !is.null(config$maxValue)) {
    hc <- hc %>%
      highcharter::hc_colorAxis(
        min = config$minValue,
        max = config$maxValue
      )
  }

  return(hc)
}
