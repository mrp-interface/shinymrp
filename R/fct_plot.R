#' plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @import dplyr
#' @import ggplot2

fips_upper <- function(fips) {
  has_county <- "county" %in% names(fips)

  fips |> mutate(
    state = toupper(state),
    state_name = tools::toTitleCase(state_name),
    county = if(has_county) tools::toTitleCase(county)
  )
}

prep_sample_size <- function(input_data, fips_codes, geo = c("county", "state"), for_map = TRUE) {
  geo <- match.arg(geo)

  if(is.null(input_data)) {
    return(NULL)
  }

  input_data <- input_data |> mutate(fips = input_data[[geo]])
  fips_codes <- fips_codes |> fips_upper()
  
  total_count <- sum(input_data$total)
  plot_df <- input_data |>
    group_by(fips) |>
    summarize(
      count = sum(total),
      perc = (sum(total) / total_count) * 100
    ) |>
    left_join(fips_codes, by = "fips")

  if(for_map) {
    if(geo == "state") {
      plot_df <- plot_df |> mutate(
        value = count,
        hover = sprintf("%s: %d (%.2f%%)",
                        state, count, perc)
      )
    } else {
      plot_df <- plot_df |> mutate(
        value = count,
        hover = sprintf("%s (%s): %d (%.2f%%)",
                        county, state, count, perc)
      )
    }
  } else {
    if(geo == "county") {
      plot_df <- plot_df |> mutate(county = gsub(" [Cc][Oo][Uu][Nn][Tt][Yy]", "", county))
    }
    
    plot_df <- plot_df |>
      select(-c(fips, perc, state_name)) |>
      select(-count, count) |>
      arrange(desc(count))
  }


  return(plot_df)
}

prep_raw_support <- function(    
    input_data,
    fips_codes,
    geo = c("county", "state")
) {
  geo <- match.arg(geo)

  if(is.null(input_data)) {
    return(NULL)
  }

  input_data <- input_data |> mutate(fips = input_data[[geo]])
  fips_codes <- fips_codes |> fips_upper()

  plot_df <- input_data |>
    group_by(fips) |>
    summarize(
      num = sum(positive),
      denom = sum(total),
      support = sum(positive) / sum(total)
    ) |>
    left_join(fips_codes, by = "fips")

  if(geo == "state") {
    plot_df <- plot_df |> mutate(
      value = support,
      hover = paste0(state, ": ", round(support, 4), " (", num, "/", denom, ")")
    )
  } else {
    plot_df <- plot_df |> mutate(
      value = support,
      hover = paste0(county, " (", state, "): ", round(support, 4), " (", num, "/", denom, ")")
    )
  }

  return(plot_df)
}

prep_raw_prev <- function(
    input_data,
    fips_codes,
    geo = c("county", "state"),
    extreme_type = c("max", "min")
) {
  geo <- match.arg(geo)
  extreme_type <- match.arg(extreme_type)
  
  if(is.null(input_data)) {
    return(NULL)
  }

  input_data <- input_data |> mutate(fips = input_data[[geo]])
  fips_codes <- fips_codes |> fips_upper()

  # calculate weekly positive response rate and test counts for each county/state
  plot_df <- input_data |>
    group_by(fips, time) |>
    summarize(
      prev = sum(positive) / sum(total),
      tests = sum(total)
    )

  # compute only the requested extreme value (min or max)
  if(extreme_type == "max") {
    extreme_fn <- max
    label <- "Highest"
    which_fn <- which.max
  } else {
    extreme_fn <- min
    label <- "Lowest"
    which_fn <- which.min
  }
  
  # compute the extreme prevalence value
  plot_df <- plot_df |>
    group_by(fips) |>
    summarize(
      prev_value = extreme_fn(prev),
      prev_sample = tests[which_fn(prev)]
    ) |>
    left_join(fips_codes, by = "fips")

  # Create hover text
  if(geo == "state") {
    plot_df <- plot_df |> mutate(
      value = prev_value,
      hover = paste0(
        state, ": ", round(prev_value, 4),
        " (", round(prev_sample * prev_value), '/', prev_sample, ")"
      )
    )
  } else {
    plot_df <- plot_df |> mutate(
      value = prev_value,
      hover = paste0(
        county, " (", state, "): ", round(prev_value, 4),
        " (", round(prev_sample * prev_value), '/', prev_sample, ")"
      )
    )
  }

  return(plot_df)
}

prep_est <- function(
    est_df,
    fips_codes,
    geo = c("county", "state"),
    time_index = NULL
) {
  geo <- match.arg(geo)

  if(is.null(est_df)) {
    return(NULL)
  }

  fips_codes <- fips_codes |> fips_upper()
  
  if(!is.null(time_index)) {
    est_df <- est_df |> filter(time == time_index)
  }

  plot_df <- est_df |>
    rename("fips" = "factor") |>
    left_join(fips_codes, by = "fips")

  if(geo == "state") {
    plot_df <- plot_df |> mutate(
      value = est,
      hover = paste0(
        state, ": ", round(est, 4), ' ± ', round(std, 4)
      )
    )
  } else {
    plot_df <- plot_df |> mutate(
      value = est,
      hover = paste0(
        county, " (", state, "): ", round(est, 4), ' ± ', round(std, 4)
      )
    )
  }

  return(plot_df)
}


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
  input <- input_data |>
    group_by(demo) |>
    summarize(perc = sum(total) / total_input)

  total_new <- sum(new_data$total)
  new <- new_data |>
    group_by(demo) |>
    summarize(perc = sum(total) / total_new)

  datasets <- c("Input Data", "Target Population")
  plot_df <- rbind(input, new) |> mutate(
    dataset = rep(datasets, each = nrow(input))
  )

  if(separate) {
    p1 <- ggplot(
      data = plot_df |> filter(dataset == datasets[1]),
      aes(x = demo, y = perc)
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
      data = plot_df |> filter(dataset == datasets[2]),
      aes(x = demo,
          y = perc)
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
        x = demo,
        y = perc,
        fill = dataset
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
    aes(x = covar)
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

plot_prev <- function(
  raw,
  dates,
  estimate = NULL,
  show_caption = FALSE,
  raw_color = "darkblue",
  mrp_color = "darkorange"
) {

  if(is.null(raw)) {
    return(NULL)
  }

  plot_df <- raw |>
    group_by(time) |>
    summarize(prev = sum(positive) / sum(total)) |>
    right_join(
      data.frame(time = 1:max(raw$time, na.rm = TRUE)),
      by = "time"
    )

  if(!is.null(estimate)) {
    plot_df <- plot_df |>
      left_join(estimate, by = "time") |>
      mutate(
        bound_upper = est + std,
        bound_lower = est - std
      )
    plot_df$bound_lower[plot_df$bound_lower < 0] <- 0
  }

  p <- ggplot(
    data = plot_df,
    aes(x = time)
  ) +
    geom_line(
      aes(
        y = prev,
        color = "Raw"
      ),
      linewidth = 1.5
    )

  if(!is.null(estimate)) {
    p <- p +
      geom_line(
        aes(
          y = est,
          color = "MRP"
        ),
        linewidth = 1.5
      ) +
      geom_ribbon(
        aes(
          y = est,
          ymin = bound_lower,
          ymax = bound_upper
        ),
        fill = mrp_color,
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
      y = "Positive\nResponse Rate",
      caption = if(show_caption) "*The shaded areas represent ±1 SD of uncertainty" else NULL
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
      values = c("Raw" = raw_color, "MRP" = mrp_color)
    ) +
    theme(
      legend.title = element_blank(),
      legend.position = if(is.null(estimate)) "none" else "bottom",
      plot.caption = element_text(hjust = 0.5),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )

  return(p)

}

plot_support <- function(
    yrep_est,
    raw
) {
  if(is.null(yrep_est) || is.null(raw)) {
    return(NULL)
  }

  raw_mean <- sum(raw$positive) / sum(raw$total)
  plot_df <- rbind(
    data.frame(
      data = "Raw",
      lower = raw_mean,
      median = raw_mean,
      upper = raw_mean
    ),
    yrep_est
  ) |>
    mutate(data = factor(data, levels = c("Raw", "Estimate")))

  p <- ggplot(data = plot_df) +
    geom_point(
      aes(x = data, y = median)
    ) +
    geom_errorbar(
      aes(x = data, ymin = lower, ymax = upper),
      width = 0
    ) +
    scale_y_continuous(
      labels = scales::percent
    ) +
    labs(x = "", y = "Positive\nResponse Rate") +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )

  return(p)
}

plot_ppc_covid_subset <- function(
    yrep,
    raw,
    dates,
    yrep_color = "darkorange",
    raw_color = "darkblue"
) {
  if(is.null(yrep) || is.null(raw)) {
    return(NULL)
  }

  raw <- raw |>
    group_by(time) |>
    summarize(
      prev = sum(positive) / sum(total)
    )

  yrep <- yrep |> tidyr::pivot_longer(
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
        x = time,
        y = prev,
        color = "Raw"
      ),
      linewidth = 2
    ) +
    geom_line(
      data = yrep,
      aes(
        x = time,
        y = value,
        group = name,
        color = "Replicated"
      ),
      alpha = 0.8
    ) +
    labs(
      title = "",
      x = if(is.null(dates)) "Week index" else "",
      y = "Positivity"
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
      values = c("Raw" = raw_color, "Replicated" = yrep_color)
    ) +
    theme(
      legend.title = element_blank(),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )

}

plot_ppc_covid_all <- function(
    yrep,
    raw,
    dates,
    yrep_color = "darkorange",
    raw_color = "darkblue"
) {

  if(is.null(yrep) || is.null(raw)) {
    return(NULL)
  }

  plot_df <- raw |>
    group_by(time) |>
    summarise(
      prev = sum(positive) / sum(total)
    ) |>
    right_join(
      data.frame(time = 1:max(raw$time, na.rm = TRUE)),
      by = "time"
    ) |>
    left_join(yrep, by = "time")

  plot_df$lower[plot_df$lower < 0] <- 0


  step <- max(1, floor(max(raw$time, na.rm = TRUE) / 15))
  xticks <- seq(1, max(raw$time, na.rm = TRUE), step)
  xticklabels <- if(!is.null(dates)) dates[xticks] else xticks

  p <- ggplot(
    data = plot_df,
    aes(x = time)
  ) +
    geom_line(
      aes(
        y = prev,
        color = "Raw"
      ),
      linewidth = 1.5
    ) +
    geom_line(
      aes(
        y = median,
        color = "Replicated"
      ),
      linewidth = 1.5
    ) +
    geom_ribbon(
      aes(
        y = median,
        ymin = lower,
        ymax = upper
      ),
      fill = yrep_color,
      alpha = 0.5
    ) +
    labs(
      title = "",
      x = if(is.null(dates)) "Week index" else "",
      y = "Positivity"
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
      values = c("Raw" = raw_color, "Replicated" = yrep_color)
    ) +
    theme(
      legend.title = element_blank(),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )

  return(p)
}

plot_ppc_poll <- function(
    yrep,
    raw,
    yrep_color = "darkorange",
    raw_color = "darkblue"
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
      value = sum(raw$positive) / sum(raw$total)
    )
  )

  ggplot(data = plot_df) +
    geom_point(
      aes(
        x = name,
        y = value,
        color = name,
        shape = name
      ),
      size = 3
    ) +
    scale_y_continuous(
      labels = scales::percent
    ) +
    labs(x = "", y = "Positive\nResponse Rate") +
    theme(
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
}

plot_est_temporal <- function(df, dates) {
  if(is.null(nullify(df))) {
    return(NULL)
  }

  levels <- unique(df$factor) |> sort()
  labels <- levels |> as.character() |> tools::toTitleCase()

  colors <- RColorBrewer::brewer.pal(8, "Set1")[1:length(levels)]
  step <- max(1, floor(max(df$time, na.rm = TRUE) / 15))
  xticks <- seq(1, max(df$time, na.rm = TRUE), step)
  xticklabels <- if(!is.null(dates)) dates[xticks] else xticks

  df <- df |> mutate(
    bound_lower = est - std,
    bound_upper = est + std
  )
  df$bound_lower[df$bound_lower < 0] <- 0
  limits <- c(0, max(df$bound_upper, na.rm = TRUE))

  plot_list <- list()
  i = 1

  plot_list[[i]] <- ggplot(
    data = df,
    aes(
      x = time,
      y = est,
      group = factor
    )
  ) +
    geom_line(
      aes(colour = factor),
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
      data = df |> filter(factor == level),
      aes(
        x = time,
        y = est,
        group = factor
      )
    ) +
      geom_line(
        aes(color = factor),
        alpha = 0.8,
        linewidth = 1.5
      ) +
      geom_ribbon(
        aes(
          fill = factor,
          ymin = bound_lower,
          ymax = bound_upper
        ),
        alpha = 0.5
      ) +
      scale_color_manual(values = colors[i - 1], labels = labels[i -1]) +
      scale_fill_manual(values = colors[i - 1], labels = labels[i - 1])
  }

  for(i in 1:length(plot_list)) {
    plot_list[[i]] <- plot_list[[i]] +
      labs(title = "",
           x = if(is.null(dates)) "Week index" else "",
           y = "Positive\nResponse Rate") +
      scale_x_continuous(
        breaks = xticks,
        labels = xticklabels,
        expand = c(0, 0.1)
      ) +
      scale_y_continuous(
        limits = limits,
        expand = expansion(mult = c(5e-3, 0.1))
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
    patchwork::plot_annotation(caption = "*The shaded areas represent ±1 SD of uncertainty") &
    theme(plot.caption = element_text(hjust = 0.5))

  return(p)
}

plot_est_static <- function(plot_df) {
  if(is.null(nullify(plot_df))) {
    return(NULL)
  }

  p <- ggplot(data = plot_df) +
    geom_point(
      aes(
        x = factor,
        y = est
      )
    ) +
    geom_errorbar(
      aes(
        x = factor,
        ymin = est - std,
        ymax = est + std
      ),
      alpha = 0.8,
      width = 0
    ) +
    scale_x_discrete(
      labels = tools::toTitleCase
    ) +
    scale_y_continuous(
      labels = scales::percent
    ) +
    labs(x = "", y = "Positive\nResponse Rate") +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = if(n_distinct(plot_df$factor) > 20) 90 else 0),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )

  return(p)
}

choro_map <- function(
    plot_df,
    geojson,
    main_title,
    sub_title,
    geo,
    config = NULL
) {
  if(is.null(plot_df) || is.null(geojson)) {
    return(NULL)
  }

  # build the chart
  hc <- highcharter::highchart(type = "map") |>
    highcharter::hc_add_series_map(
      map        = geojson,
      df         = plot_df,
      joinBy     = c("fips", "fips"),
      value      = "value",
      name       = sub_title,
      dataLabels = list(enabled = FALSE, format = "{point.name}"),
      tooltip    = list(
        pointFormat = "{point.hover}"
      ),
      borderWidth= 0.1
    ) |>
    highcharter::hc_title(
      text = main_title,
      align = "center",
      style = list(fontSize = "20px")
    ) |>
    highcharter::hc_mapNavigation(enabled = TRUE) |>
    highcharter::hc_boost(enabled = FALSE)

  if(!is.null(config$minValue) && !is.null(config$maxValue)) {
    hc <- hc |>
      highcharter::hc_colorAxis(
        min = config$minValue,
        max = config$maxValue
      )
  }

  return(hc)
}