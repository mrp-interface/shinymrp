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
#'
#' @noRd
#' @importFrom dplyr mutate
#' @importFrom rlang .data
fips_upper <- function(fips) {
  has_county <- "county" %in% names(fips)

  fips %>% mutate(
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
#'
#' @noRd
#' @importFrom dplyr mutate group_by summarize left_join select arrange desc
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

  input_data <- input_data %>% mutate(fips = input_data[[geo]])
  fips_codes <- fips_codes %>% fips_upper()
  
  total_count <- sum(input_data$total)
  plot_df <- input_data %>%
    group_by(.data$fips) %>%
    summarize(
      count = sum(.data$total),
      perc = (sum(.data$total) / total_count) * 100
    ) %>%
    left_join(fips_codes, by = "fips")

  if(for_map) {
    if(geo == "state") {
      plot_df <- plot_df %>% mutate(
        value = .data$count,
        hover = sprintf("%s: %d (%.2f%%)",
                        .data$state, .data$count, .data$perc)
      )
    } else {
      plot_df <- plot_df %>% mutate(
        value = .data$count,
        hover = sprintf("%s (%s): %d (%.2f%%)",
                        .data$county, .data$state, .data$count, .data$perc)
      )
    }
  } else {
    if(geo == "county") {
      plot_df <- plot_df %>% mutate(county = gsub(" [Cc][Oo][Uu][Nn][Tt][Yy]", "", .data$county))
    }
    
    plot_df <- plot_df %>%
      select(-c(.data$fips, .data$perc, .data$state_name)) %>%
      select(-.data$count, .data$count) %>%
      arrange(desc(.data$count))
  }


  return(plot_df)
}

#' Prepare Raw Data for Plotting
#'
#' This function processes input data for geographic visualization by computing 
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
#'     \item \code{is_timevar}: Logical indicating if data has time dimension
#'     \item \code{family}: Character specifying data family ("binomial" or "normal")
#'   }
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{plot_df}: Processed data frame ready for plotting with columns for 
#'       fips, value, geographic names, and hover text
#'     \item \code{title}: List containing main_title and hover_title for the plot
#'   }
#'   Returns NULL if input_data is NULL.
#'
#' @details
#' For binomial family data, the function computes rates as positive/total.
#' For normal family data, it computes means of the outcome variable.
#' When time dimension is present, it can summarize to maximum or minimum values
#' across time periods. Hover text is formatted differently for state vs county level data.
#'
#' @examples
#' \dontrun{
#' metadata <- list(is_timevar = TRUE, family = "binomial")
#' result <- prep_raw(my_data, fips_df, geo = "county", 
#'                    summary_type = "max", metadata = metadata)
#' }
#' @noRd
prep_raw <- function(
  input_data,
  fips_codes,
  geo,
  summary_type = c("max", "min"),
  metadata = NULL
) {

  checkmate::assert_choice(
    geo,
    choices = GLOBAL$vars$geo2,
    null.ok = FALSE
  )
  
  summary_type <- match.arg(summary_type)
  
  if(is.null(input_data)) {
    return(NULL)
  }

  input_data <- input_data %>% mutate(fips = input_data[[geo]])
  fips_codes <- fips_codes %>% fips_upper()

  # compute weekly average
  group_cols <- if(metadata$is_timevar) c("fips", "time") else c("fips")
  plot_df <- input_data %>%
    group_by(across(all_of(group_cols))) %>%
    summarize(
      value = switch(metadata$family,
        "binomial" = sum(.data$positive) / sum(.data$total),
        "normal"   = mean(.data$outcome)
      )
    )

  if (metadata$is_timevar) {
    if(summary_type == "max") {
      summary_fn <- max
      label <- "Highest"
      which_fn <- which.max
    } else if (summary_type == "min") {
      summary_fn <- min
      label <- "Lowest"
      which_fn <- which.min
    }

    plot_df <- plot_df %>%
      group_by(.data$fips) %>%
      summarize(
        value = summary_fn(.data$value)
      )
  }
  
  # construct hover text based on geographic level
  plot_df <- plot_df %>%
    left_join(fips_codes, by = "fips") %>%
    mutate(
      hover = switch(geo,
        "state" = paste0(.data$state, ": "),
        "county" = paste0(.data$county, " (", .data$state, "): ")
      )
    ) %>% 
    mutate(
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
#'
#' @return A data frame with estimates, geographic information, and formatted hover
#'   text for map visualization
#'
#' @noRd
#' @importFrom dplyr rename left_join mutate filter
prep_est <- function(
    est_df,
    fips_codes,
    geo,
    time_index = NULL
) {
  
  checkmate::assert_choice(
    geo,
    choices = GLOBAL$vars$geo2,
    null.ok = FALSE
  )

  if(is.null(est_df)) {
    return(NULL)
  }

  fips_codes <- fips_codes %>% fips_upper()
  
  if(!is.null(time_index)) {
    est_df <- est_df %>% filter(.data$time == time_index)
  }

  plot_df <- est_df %>%
    rename("fips" = "factor") %>%
    left_join(fips_codes, by = "fips")

  if(geo == "state") {
    plot_df <- plot_df %>% mutate(
      value = .data$est,
      hover = paste0(
        .data$state, ": ", round(.data$est, 4), ' \u00B1 ', round(.data$std, 4)
      )
    )
  } else {
    plot_df <- plot_df %>% mutate(
      value = .data$est,
      hover = paste0(
        .data$county, " (", .data$state, "): ", round(.data$est, 4), ' \u00B1 ', round(.data$std, 4)
      )
    )
  }

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
#'
#' @noRd
#' @importFrom ggplot2 ggplot aes geom_bar scale_x_discrete scale_y_continuous labs theme element_text margin
#' @importFrom scales percent
#' @importFrom tools toTitleCase
#' @importFrom patchwork wrap_plots
#' @importFrom dplyr group_by summarize mutate filter
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
    group_by(.data$demo) %>%
    summarize(perc = sum(.data$total) / total_input)

  total_new <- sum(new_data$total)
  new <- new_data %>%
    group_by(.data$demo) %>%
    summarize(perc = sum(.data$total) / total_new)

  datasets <- c("Input Data", "Target Population")
  plot_df <- rbind(input, new) %>% mutate(
    dataset = rep(datasets, each = nrow(input))
  )

  if(separate) {
    p1 <- ggplot(
      data = plot_df %>% filter(.data$dataset == datasets[1]),
      aes(x = .data$demo, y = .data$perc)
    ) +
      geom_bar(
        stat = "identity",
        position = "dodge"
      ) +
      labs(
        title = datasets[1],
        caption = sprintf("Sample size: %s", format(total_input, big.mark = ","))
      )

    p2 <- ggplot(
      data = plot_df %>% filter(.data$dataset == datasets[2]),
      aes(x = .data$demo,
          y = .data$perc)
    ) +
      geom_bar(
        stat = "identity",
        position = "dodge"
      ) +
      labs(
        title = datasets[2],
        caption = sprintf("Sample size: %s", format(total_new, big.mark = ","))
      )

    p <- patchwork::wrap_plots(p1, p2)

  } else {
    p <- ggplot(
      data = plot_df,
      aes(
        x = .data$demo,
        y = .data$perc,
        fill = .data$dataset
      )
    ) +
      geom_bar(
        stat = "identity",
        position = "dodge"
      )
  }

  p <- p &
    scale_x_discrete(
      labels = tools::toTitleCase
    ) & 
    scale_y_continuous(
      labels = scales::percent,
      limits = c(0, 1),
      expand = c(0, 0)
    ) &
    labs(x = "", y = "") &
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )


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
#'
#' @noRd
#' @importFrom ggplot2 ggplot aes geom_histogram scale_y_continuous scale_x_continuous labs theme element_text margin expansion ggplot_build
#' @importFrom scales breaks_width
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

  p <- ggplot(
    data = covariates,
    aes(x = .data$covar)
  ) +
    geom_histogram(breaks = breaks)
    
  # Extract bin data from ggplot object
  plot_data <- ggplot2::ggplot_build(p)
  histogram_data <- plot_data$data[[1]]

  # Set minimum break width to 1
  if (max(histogram_data$count) < 3) {
    p <- p +
    scale_y_continuous(
      expand = expansion(mult = c(0, .1)),
      breaks = scales::breaks_width(1)
    ) 
  } else {
    p <- p +
    scale_y_continuous(
      expand = expansion(mult = c(0, .1))
    ) 
  }
    
  p <- p +
    scale_x_continuous(
      expand = c(0, 0)
    ) +
    labs(
      title = "",
      subtitle = description,
      caption = definition,
      x = name, y = "Number of zip codes"
    ) +
    theme(
      plot.subtitle = element_text(size = 16, hjust = 0),
      plot.caption = element_text(size = 16, hjust = 0.5),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )

  return(p)
}

#' Create prevalence time series plots
#'
#' @description Creates line plots showing prevalence over time, with optional
#' model estimates and uncertainty bands. Supports both raw data and MRP estimates.
#'
#' @param raw A data frame containing raw survey data with time, positive, and total columns
#' @param dates Optional character vector of date labels for x-axis
#' @param yrep Optional data frame containing model estimates with time, est, and std columns
#' @param show_caption Logical indicating whether to show uncertainty caption
#'
#' @return A ggplot object showing prevalence time series with optional estimates
#'
#' @noRd
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon labs scale_x_continuous scale_y_continuous scale_color_manual theme element_blank element_text margin expansion
#' @importFrom dplyr group_by summarize right_join left_join mutate
plot_outcome_timevar <- function(
  raw,
  yrep_est = NULL,
  dates = NULL,
  metadata = NULL,
  show_caption = FALSE,
  config = GLOBAL$ui$plot
) {

  if(is.null(raw)) {
    return(NULL)
  }

  # compute weekly rates/averages
  plot_df <- raw %>%
    group_by(.data$time) %>%
    summarize(
      raw = switch(metadata$family,
        "binomial" = sum(.data$positive) / sum(.data$total),
        "normal" = mean(.data$outcome)
      )
    )

  # ensure missing time points are included
  plot_df <- plot_df %>%
    right_join(
      data.frame(time = 1:max(raw$time, na.rm = TRUE)),
      by = "time"
    )

  if(!is.null(yrep_est)) {
    plot_df <- plot_df %>%
      left_join(yrep_est, by = "time") %>%
      mutate(
        bound_upper = .data$est + .data$std,
        bound_lower = .data$est - .data$std
      )

    if (metadata$family == "binomial") {
      # ensure bounds are non-negative for binomial family
      plot_df$bound_lower[plot_df$bound_lower < 0] <- 0
    }
  }

  p <- ggplot(
    data = plot_df,
    aes(x = .data$time)
  ) +
    geom_line(
      aes(
        y = .data$raw,
        color = "Raw"
      ),
      linewidth = 1.5
    )

  if(!is.null(yrep_est)) {
    p <- p +
      geom_line(
        aes(
          y = .data$est,
          color = "MRP"
        ),
        linewidth = 1.5
      ) +
      geom_ribbon(
        aes(
          y = .data$est,
          ymin = .data$bound_lower,
          ymax = .data$bound_upper
        ),
        fill = config$mrp_color,
        alpha = 0.5
      )
  }

  step <- max(1, floor(max(raw$time, na.rm = TRUE) / 15))
  xticks <- seq(1, max(raw$time, na.rm = TRUE), step)
  xticklabels <- if(!is.null(dates)) dates[xticks] else xticks

  p <- p +
    labs(
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
        "*The shaded areas represent \u00B11 SD of uncertainty"
      } else {
        NULL
      }
    ) +
    scale_x_continuous(
      breaks = xticks,
      labels = xticklabels,
      expand = c(0, 0.1)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(5e-3, 0.1))
    ) +
    scale_color_manual(
      values = c("Raw" = config$raw_color, "MRP" = config$mrp_color)
    ) +
    theme(
      legend.title = element_blank(),
      legend.position = if(is.null(yrep_est)) "none" else "bottom",
      plot.caption = element_text(hjust = 0.5),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )

  return(p)

}

#' Create support comparison plots
#'
#' @description Creates point plots with error bars comparing raw support rates
#' with model estimates and uncertainty intervals.
#'
#' @param yrep_est A data frame containing model estimates with data, lower, median,
#'   and upper columns
#' @param raw A data frame containing raw survey data with positive and total columns
#'
#' @return A ggplot object showing support comparison with error bars
#'
#' @noRd
#' @importFrom ggplot2 ggplot geom_point geom_errorbar scale_y_continuous labs theme element_text margin
#' @importFrom dplyr mutate
plot_outcome_static <- function(
    raw,
    yrep_est = NULL,
    metadata = NULL,
    show_caption = FALSE
) {
  if(is.null(raw)) {
    return(NULL)
  }

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
      mutate(
        data = "Estimate",
        lower = .data$est - .data$std,
        median = .data$est,
        upper = .data$est + .data$std
      ) %>%
      select(.data$data, .data$lower, .data$median, .data$upper) 

    plot_df <- rbind(raw, yrep_est) %>%
      mutate(data = factor(.data$data, levels = c("Raw", "Estimate")))
  }

  p <- ggplot(data = plot_df) +
    geom_point(
      aes(x = .data$data, y = .data$median),
      size = GLOBAL$ui$plot$point_size
    ) +
    geom_errorbar(
      aes(x = .data$data, ymin = .data$lower, ymax = .data$upper),
      size = GLOBAL$ui$plot$errorbar_size,
      width = GLOBAL$ui$plot$errorbar_width
    ) +
    labs(
      x = "",
      y = switch(metadata$family,
        "binomial" = "Proportion estimates",
        "normal" = "Mean estimates"
      ),
      caption = if(show_caption) {
        "*The error bars represent \u00B11 SD of uncertainty"
      } else {
        NULL
      }
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      plot.margin = margin(1, 1, 1, 1, "cm")
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
#'
#' @return A ggplot object showing posterior predictive check with multiple replications
#'
#' @noRd
#' @importFrom ggplot2 ggplot geom_line aes labs scale_x_continuous scale_y_continuous scale_color_manual theme element_blank margin expansion
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr group_by summarize
plot_ppc_timevar_subset <- function(
    yrep,
    raw,
    dates,
    metadata,
    config = GLOBAL$ui$plot
) {
  if(is.null(yrep) || is.null(raw)) {
    return(NULL)
  }

  raw <- raw %>%
    group_by(.data$time) %>%
    summarize(
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

  ggplot() +
    geom_line(
      data = raw,
      aes(
        x = .data$time,
        y = .data$prev,
        color = "Raw"
      ),
      linewidth = 2
    ) +
    geom_line(
      data = yrep,
      aes(
        x = .data$time,
        y = .data$value,
        group = .data$name,
        color = "Replicated"
      ),
      alpha = 0.8
    ) +
    labs(
      title = "",
      x = if(is.null(dates)) "Week index" else "",
      y = switch(metadata$family,
        "binomial" = "Positive response rate",
        "normal" = "Outcome average"
      )
    ) +
    scale_x_continuous(
      breaks = xticks,
      labels = xticklabels,
      expand = c(0, 0.1)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(5e-3, 0.1))
    ) +
    scale_color_manual(
      values = c(
        "Raw" = config$raw_color,
        "Replicated" = config$yrep_color
      )
    ) +
    theme(
      legend.title = element_blank(),
      plot.margin = margin(1, 1, 1, 1, "cm")
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
#'
#' @return A ggplot object showing posterior predictive check with summary statistics
#'
#' @noRd
#' @importFrom ggplot2 ggplot geom_line geom_ribbon aes labs scale_x_continuous scale_y_continuous scale_color_manual theme element_blank margin expansion
#' @importFrom dplyr group_by summarise right_join left_join
plot_ppc_timevar_all <- function(
    yrep,
    raw,
    dates,
    metadata,
    config = GLOBAL$ui$plot
) {

  if(is.null(yrep) || is.null(raw)) {
    return(NULL)
  }

  plot_df <- raw %>%
    group_by(.data$time) %>%
    summarise(
      prev = sum(.data$positive) / sum(.data$total)
    ) %>%
    right_join(
      data.frame(time = 1:max(raw$time, na.rm = TRUE)),
      by = "time"
    ) %>%
    left_join(yrep, by = "time")

  plot_df$lower[plot_df$lower < 0] <- 0


  step <- max(1, floor(max(raw$time, na.rm = TRUE) / 15))
  xticks <- seq(1, max(raw$time, na.rm = TRUE), step)
  xticklabels <- if(!is.null(dates)) dates[xticks] else xticks

  p <- ggplot(
    data = plot_df,
    aes(x = .data$time)
  ) +
    geom_line(
      aes(
        y = .data$prev,
        color = "Raw"
      ),
      linewidth = 1.5
    ) +
    geom_line(
      aes(
        y = .data$median,
        color = "Replicated"
      ),
      linewidth = 1.5
    ) +
    geom_ribbon(
      aes(
        y = .data$median,
        ymin = .data$lower,
        ymax = .data$upper
      ),
      fill = config$yrep_color,
      alpha = 0.5
    ) +
    labs(
      title = "",
      x = if(is.null(dates)) "Week index" else "",
      y = switch(metadata$family,
        "binomial" = "Positive response rate",
        "normal" = "Outcome average"
      )
    ) +
    scale_x_continuous(
      breaks = xticks,
      labels = xticklabels,
      expand = c(0, 0.1)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(5e-3, 0.1))
    ) +
    scale_color_manual(
      values = c("Raw" = config$raw_color, "Replicated" = config$yrep_color)
    ) +
    theme(
      legend.title = element_blank(),
      plot.margin = margin(1, 1, 1, 1, "cm")
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
#'
#' @return A ggplot object showing posterior predictive check for polling data
#'
#' @noRd
#' @importFrom ggplot2 ggplot geom_point aes scale_y_continuous labs theme element_blank element_text margin
#' @importFrom scales percent
plot_ppc_static <- function(
    yrep,
    raw,
    metadata = NULL,
    config = GLOBAL$ui$plot
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

  ggplot(data = plot_df) +
    geom_point(
      aes(
        x = .data$name,
        y = .data$value,
        color = .data$name,
        shape = .data$name
      ),
      size = GLOBAL$ui$plot$point_size
    ) +
    labs(
      x = "",
      y = switch(metadata$family,
        "binomial" = "Positive response rate",
        "normal" = "Outcome average"
      )
    ) +
    theme(
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
}

#' Create time-varying estimate plots
#'
#' @description Creates multi-panel line plots showing model estimates over time
#' for different factor levels. Includes an overview plot and individual plots
#' with uncertainty bands for each factor level.
#'
#' @param df A data frame containing time-varying estimates with factor, time, est, and std columns
#' @param dates Optional character vector of date labels for x-axis
#'
#' @return A patchwork object containing multiple ggplot panels showing time-varying estimates
#'
#' @noRd
#' @importFrom ggplot2 ggplot geom_line geom_ribbon aes labs scale_x_continuous scale_y_continuous scale_color_manual scale_fill_manual theme element_blank margin expansion
#' @importFrom RColorBrewer brewer.pal
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom tools toTitleCase
#' @importFrom dplyr mutate filter
plot_est_temporal <- function(
    plot_df,
    dates,
    metadata = NULL
) {
  if(is.null(nullify(plot_df))) {
    return(NULL)
  }

  levels <- unique(plot_df$factor) %>% sort()
  labels <- levels %>% as.character() %>% tools::toTitleCase()

  colors <- RColorBrewer::brewer.pal(8, "Set1")[1:length(levels)]
  step <- max(1, floor(max(plot_df$time, na.rm = TRUE) / 15))
  xticks <- seq(1, max(plot_df$time, na.rm = TRUE), step)
  xticklabels <- if(!is.null(dates)) dates[xticks] else xticks

  plot_df <- plot_df %>% mutate(
    bound_lower = .data$est - .data$std,
    bound_upper = .data$est + .data$std
  )

  if(metadata$family == "binomial") {
    # ensure bounds are non-negative for binomial family
    plot_df$bound_lower[plot_df$bound_lower < 0] <- 0
  }
  config <- list(
    limits = c(min(plot_df$bound_lower), max(plot_df$bound_upper)),
    expand = switch(metadata$family,
      "binomial" = c(5e-3, 0.1),
      c(0.1, 0.1)
    )
  )
  


  plot_list <- list()
  i = 1

  plot_list[[i]] <- ggplot(
    data = plot_df,
    aes(
      x = .data$time,
      y = .data$est,
      group = .data$factor
    )
  ) +
    geom_line(
      aes(colour = .data$factor),
      alpha = 0.8,
      linewidth = 1.5
    ) +
    scale_color_manual(
      values = colors,
      labels = labels
    )

  for(level in levels) {
    i <- i + 1
    plot_list[[i]] <- ggplot(
      data = plot_df %>% filter(.data$factor == level),
      aes(
        x = .data$time,
        y = .data$est,
        group = .data$factor
      )
    ) +
      geom_line(
        aes(color = .data$factor),
        alpha = 0.8,
        linewidth = 1.5
      ) +
      geom_ribbon(
        aes(
          fill = .data$factor,
          ymin = .data$bound_lower,
          ymax = .data$bound_upper
        ),
        alpha = 0.5
      ) +
      scale_color_manual(values = colors[i - 1], labels = labels[i -1]) +
      scale_fill_manual(values = colors[i - 1], labels = labels[i - 1])
  }

  for(i in 1:length(plot_list)) {
    plot_list[[i]] <- plot_list[[i]] +
      labs(
        title = "",
        x = if(is.null(dates)) "Week index" else "",
        y = switch(metadata$family,
          "binomial" = "Proportion estimates",
          "normal" = "Mean estimates"
        )
      ) +
      scale_x_continuous(
        breaks = xticks,
        labels = xticklabels,
        expand = c(0, 0.1)
      ) +
      scale_y_continuous(
        limits = config$limits,
        expand = expansion(mult = config$expand)
      ) +
      theme(
        legend.title = element_blank(),
        legend.position = "right",
        plot.margin = margin(0, 1, 0, 1, "cm")
      )
  }

  p <- patchwork::wrap_plots(
    plot_list,
    ncol = 1,
    nrow = length(levels) + 1
  ) +
    patchwork::plot_annotation(caption = "*The shaded areas represent \u00B11 SD of uncertainty") &
    theme(plot.caption = element_text(hjust = 0.5))

  return(p)
}

#' Create static estimate plots
#'
#' @description Creates point plots with error bars showing model estimates
#' for different factor levels in cross-sectional data.
#'
#' @param plot_df A data frame containing estimates with factor, est, and std columns
#'
#' @return A ggplot object showing static estimates with error bars
#'
#' @noRd
#' @importFrom ggplot2 ggplot geom_point geom_errorbar aes scale_x_discrete scale_y_continuous labs theme element_text margin
#' @importFrom scales percent
#' @importFrom tools toTitleCase
#' @importFrom dplyr n_distinct
plot_est_static <- function(plot_df, metadata = NULL) {
  if(is.null(nullify(plot_df))) {
    return(NULL)
  }

  p <- ggplot(data = plot_df) +
    geom_point(
      aes(
        x = .data$factor,
        y = .data$est
      ),
      size = GLOBAL$ui$plot$point_size
    ) +
    geom_errorbar(
      aes(
        x = .data$factor,
        ymin = .data$est - .data$std,
        ymax = .data$est + .data$std
      ),
      size = GLOBAL$ui$plot$errorbar_size,
      width = GLOBAL$ui$plot$errorbar_width
    ) +
    scale_x_discrete(
      labels = tools::toTitleCase
    ) +
    labs(
      x = "",
      y = switch(metadata$family,
        "binomial" = "Proportion estimates",
        "normal" = "Mean estimates"
      ),
      caption = "*The error bars represent \u00B11 SD of uncertainty"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = if(n_distinct(plot_df$factor) > 20) 90 else 0),
      plot.margin = margin(1, 1, 1, 1, "cm")
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
#'
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