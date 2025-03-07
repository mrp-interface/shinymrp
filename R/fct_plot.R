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

prep_sample_size <- function(input_data, fips_codes, geo = c("county", "state"), for_map = TRUE) {
  geo <- match.arg(geo)

  input_data <- input_data |> mutate(fips = input_data[[geo]])

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
        hover = sprintf("%s: %d (%.2f%%)",
                        state, count, perc)
      )
    } else {
      plot_df <- plot_df |> mutate(
        hover = sprintf("%s\n%s\nSample size: %d (%.2f%%)",
                        state_name, county, count, perc)
      )
    }
  } else {
    if(geo == "county") {
      plot_df <- plot_df |> mutate(county = gsub(" county", "", county))
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

  input_data <- input_data |> mutate(fips = input_data[[geo]])

  plot_df <- input_data |>
    group_by(fips) |>
    summarize(
      num = sum(positive),
      denom = sum(total),
      support = round(sum(positive) / sum(total), 4)
    ) |>
    left_join(fips_codes, by = "fips")

  if(geo == "state") {
    plot_df <- plot_df |> mutate(
      hover = paste0(state, ": ", support, "\n(", num, "/", denom, ")")
    )
  } else {
    plot_df <- plot_df |> mutate(
      hover = paste0(county, " (", state, "):\n", support, " (", num, "/", denom, ")")
    )
  }

  return(plot_df)
}

prep_raw_prev <- function(
    input_data,
    fips_codes,
    geo = c("county", "state")
) {
  geo <- match.arg(geo)

  input_data <- input_data |> mutate(fips = input_data[[geo]])

  # calculate weekly positive response rate and test counts for each county/state
  plot_df <- input_data |>
    group_by(fips, time) |>
    summarize(
      prev = sum(positive) / sum(total),
      tests = sum(total)
    )

  # compute max and min weekly positive response rate for each county
  plot_df <- plot_df |>
    group_by(fips) |>
    summarize(
      max_prev = max(prev),
      min_prev = min(prev),
      max_prev_sample = tests[which.max(prev)],
      min_prev_sample = tests[which.min(prev)]
    ) |>
    left_join(fips_codes, by = "fips")

  if(geo == "state") {
    plot_df <- plot_df |> mutate(
      hover = paste0(
        state_name, '\n',
        "Highest: ", round(max_prev, 4),
        " (", round(max_prev_sample * max_prev), '/', max_prev_sample, ")\n",
        "Lowest: ", round(min_prev, 4),
        " (", round(min_prev_sample * min_prev), '/', min_prev_sample, ")\n"
      )
    )
  } else {
    plot_df <- plot_df |> mutate(hover = paste0(
      state_name, '\n',
      county, '\n',
      "Highest: ", round(max_prev, 4),
      " (", round(max_prev_sample * max_prev), '/', max_prev_sample, ")\n",
      "Lowest: ", round(min_prev, 4),
      " (", round(min_prev_sample * min_prev), '/', min_prev_sample, ")\n"
    ))
  }

  return(plot_df)
}

prep_est <- function(
    est_df,
    fips_codes,
    geo = c("county", "state"),
    dates = NULL
) {
  geo <- match.arg(geo)

  est_df <- est_df |> mutate(fips = factor)
  if("time" %in% names(est_df)) {
    sq <- 1:max(est_df$time, na.rm = TRUE)
    est_df <- est_df |> mutate(
      time = factor(
        time,
        levels = sq,
        labels = if(!is.null(dates)) dates else paste0("Week ", sq)
      )
    )
  }

  plot_df <- est_df |> left_join(fips_codes, by = "fips")

  if(geo == "state") {
    plot_df <- plot_df |> mutate(
      hover = paste0(
        state_name, '\n',
        "Estimate: ", round(est, 4), '\n',
        "Standard error: ", round(std, 4)
      )
    )
  } else {
    plot_df <- plot_df |> mutate(
      hover = paste0(
        state_name, '\n',
        county, '\n',
        "Estimate: ", round(est, 4), '\n',
        "Standard error: ", round(std, 4)
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

  total_input <- sum(input_data$total)
  input <- input_data |>
    group_by(demo) |>
    summarize(perc = sum(total) / total_input)

  total_new <- sum(new_data$total)
  new <- new_data |>
    group_by(demo) |>
    summarize(perc = sum(total) / total_new)
  
  datasets <- c("Input Data", "ACS Data")
  plot_df <- rbind(input, new) |> mutate(
    dataset = rep(datasets, each = nrow(input)),
    demo = factor(demo, levels = new$demo)
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

  p <- ggplot(
    data = covariates,
    aes(x = covar)
  ) +
    geom_histogram(breaks = breaks) +
    scale_x_continuous(
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, .1))
    ) +
    labs(
      title = "",
      subtitle = description,
      caption = definition,
      x = name, y = "Number of zip codes"
    ) +
    theme(
      plot.subtitle = element_text(size = 15, hjust = 0),
      plot.caption = element_text(size = 15, hjust = 0.5),
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
  if(is.null(df) || nrow(df) == 0) {
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
    map_geojson,
    map_title,
    colorbar_title,
    geo
) {
  
  # Validate inputs
  if (is.null(map_geojson) || is.null(plot_df)) {
    stop("Missing required geojson or plot data")
  }

  # Create the plot
  fig <- plotly::plot_ly(
    frame = if("time" %in% names(plot_df)) plot_df$time else NULL
  ) |>
    plotly::add_trace(
      type = "choropleth",
      geojson = map_geojson,
      locations = plot_df$fips,
      z = plot_df$value,
      zmin = 0,
      zmax = max(plot_df$value, na.rm = TRUE),
      featureidkey = "properties.GEOID",
      colorscale = "Electric",
      text = plot_df$hover,
      hoverinfo = "text"
    ) |>
    plotly::layout(
      geo = list(
        fitbounds = if(geo == "state") NULL else "geojson",
        scope = "usa",
        projection = list(type = 'albers usa'),
        visible = FALSE
      ),
      title = list(
        text = map_title,
        xref = "paper",
        x = 0.5
      ),
      dragmode = FALSE
    ) |>
    plotly::colorbar(
      title = colorbar_title,
      thickness = 25
    ) |>
    plotly::config(
      displayModeBar = FALSE
    )

  return(fig)
}
