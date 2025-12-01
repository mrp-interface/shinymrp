plot_grid_heatmap <- function(nx, ny, values,
                              xlab = "Column",
                              ylab = "Row",
                              title = NULL) {
  stopifnot(length(values) == nx * ny)
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }
  library(ggplot2)
  
  # Assume values are given row-by-row:
  # row 1: columns 1..nx, row 2: columns 1..nx, etc.
  mat <- matrix(values, nrow = ny, ncol = nx, byrow = TRUE)
  
  df <- as.data.frame(as.table(mat))
  names(df) <- c("row", "col", "value")
  
  # Make row/col numeric
  df$row <- as.integer(df$row)
  df$col <- as.integer(df$col)
  
  p <- ggplot(df, aes(x = col, y = row, fill = value)) +
    geom_tile() +
    scale_x_continuous(breaks = seq_len(nx)) +
    scale_y_continuous(breaks = seq_len(ny)) +
    coord_fixed() +
    labs(
      x = xlab,
      y = ylab,
      fill = "Value",
      title = title
    ) +
    theme_bw() +
    scale_fill_viridis_c()
  
  p
}


# Helper: summarize timing for all implementations
summarize_timing_by_nodes <- function(models, impl_labels = names(models)) {
  stopifnot(length(models) == length(impl_labels))
  
  summarize_impl <- function(impl_res, impl_label) {
    sizes <- names(impl_res)
    
    do.call(
      rbind,
      lapply(seq_along(impl_res), function(j) {
        size_name <- sizes[j]
        
        # extract number of nodes from names like "25_nodes"
        n_nodes <- suppressWarnings(as.integer(sub("_.*$", "", size_name)))
        
        runs <- impl_res[[j]]
        
        basis_times <- vapply(
          runs,
          function(run) run$time$basis,
          numeric(1)
        )
        fit_times <- vapply(
          runs,
          function(run) run$time$fit,
          numeric(1)
        )
        
        data.frame(
          implementation = impl_label,
          nodes = n_nodes,
          basis = mean(basis_times, na.rm = TRUE),
          fit   = mean(fit_times,   na.rm = TRUE),
          stringsAsFactors = FALSE
        )
      })
    )
  }
  
  df_list <- lapply(seq_along(models), function(i) {
    summarize_impl(models[[i]], impl_labels[i])
  })
  
  do.call(rbind, df_list)
}

plot_basis_time_by_nodes <- function(models,
                                     impl_labels = names(models),
                                     basis_title = "Average R construction time") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function.")
  }
  library(ggplot2)
  
  df <- summarize_timing_by_nodes(models, impl_labels)
  
  # Aggregate across implementations: one line only
  df_basis <- aggregate(
    basis ~ nodes,
    data = df,
    FUN = mean,
    na.rm = TRUE
  )
  
  ggplot(df_basis, aes(x = nodes, y = basis)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = sort(unique(df_basis$nodes))) +
    labs(
      x = "Number of nodes",
      y = "Time (s)",
      title = basis_title
    ) +
    theme_bw()
}

plot_fit_time_by_nodes <- function(models,
                                   impl_labels = names(models),
                                   fit_title   = "Average model fitting time") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function.")
  }
  library(ggplot2)
  
  df <- summarize_timing_by_nodes(models, impl_labels)
  
  ggplot(df, aes(x = nodes, y = fit, color = implementation)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = sort(unique(df$nodes))) +
    labs(
      x = "Number of nodes",
      y = "Time (s)",
      color = "Model implementation",
      title = fit_title
    ) +
    theme_bw()
}

plot_ess_ratio_by_nodes <- function(models,
                                    variables,
                                    base_model,
                                    ess_title = "ESS ratio vs base model") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function.")
  }
  library(ggplot2)
  
  model_names <- names(models)
  if (is.null(model_names)) {
    stop("`models` must be a named list; names should identify each implementation.")
  }
  if (!(base_model %in% model_names)) {
    stop("`base_model` must be one of: ", paste(model_names, collapse = ", "))
  }
  
  impl_labels <- model_names
  
  # ---- 1) Build long ESS data: implementation, nodes, run, variable, ess_bulk, ess_tail ----
  df_ess <- do.call(
    rbind,
    lapply(seq_along(models), function(i) {
      impl_name <- impl_labels[i]
      impl_res  <- models[[i]]
      sizes     <- names(impl_res)
      
      do.call(
        rbind,
        lapply(seq_along(impl_res), function(j) {
          size_name <- sizes[j]
          n_nodes   <- suppressWarnings(as.integer(sub("_.*$", "", size_name)))
          runs      <- impl_res[[j]]
          
          do.call(
            rbind,
            lapply(seq_along(runs), function(k) {
              fs <- runs[[k]]$fit_summary
              fs <- as.data.frame(fs)
              
              sub <- fs[fs$variable %in% variables,
                        c("variable", "ess_bulk", "ess_tail")]
              if (nrow(sub) == 0L) return(NULL)
              
              cbind(
                data.frame(
                  implementation = impl_name,
                  nodes = n_nodes,
                  run   = k,
                  stringsAsFactors = FALSE
                ),
                sub
              )
            })
          )
        })
      )
    })
  )
  
  if (is.null(df_ess) || nrow(df_ess) == 0L) {
    stop("No matching ESS entries found for the specified variables.")
  }
  
  # ---- 2) Average ESS over runs for each implementation, nodes, variable ----
  ess_means <- aggregate(
    cbind(ess_bulk, ess_tail) ~ implementation + nodes + variable,
    data = df_ess,
    FUN  = mean,
    na.rm = TRUE
  )
  
  # ---- 3) Split base vs non-base ----
  base_ess  <- ess_means[ess_means$implementation == base_model, ]
  other_ess <- ess_means[ess_means$implementation != base_model, ]
  
  if (nrow(base_ess) == 0L) {
    stop("No ESS entries found for base_model = ", base_model)
  }
  if (nrow(other_ess) == 0L) {
    stop("No ESS entries found for non-base models.")
  }
  
  base_ess <- base_ess[, c("nodes", "variable", "ess_bulk", "ess_tail")]
  names(base_ess)[names(base_ess) == "ess_bulk"] <- "ess_bulk_base"
  names(base_ess)[names(base_ess) == "ess_tail"] <- "ess_tail_base"
  
  merged <- merge(
    other_ess,
    base_ess,
    by = c("nodes", "variable"),
    all = FALSE
  )
  
  if (nrow(merged) == 0L) {
    stop("No overlapping nodes/variables between base model and other models.")
  }
  
  # ---- 4) Ratios per implementation, nodes, variable ----
  merged$ratio_bulk <- merged$ess_bulk / merged$ess_bulk_base
  merged$ratio_tail <- merged$ess_tail / merged$ess_tail_base
  
  # ---- 5) Average ratios over variables (but NOT over implementations) ----
  ratio_nodes_impl <- aggregate(
    cbind(ratio_bulk, ratio_tail) ~ implementation + nodes,
    data = merged,
    FUN  = mean,
    na.rm = TRUE
  )
  
  # ---- 6) Separate plots: bulk and tail, one line per non-base implementation ----
  p_bulk <- ggplot(ratio_nodes_impl,
                   aes(x = nodes, y = ratio_bulk, color = implementation)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 1, linetype = "dashed") +
    scale_x_continuous(breaks = sort(unique(ratio_nodes_impl$nodes))) +
    labs(
      x = "Number of nodes",
      y = "Bulk ESS ratio (model / base)",
      color = "Model",
      title = paste0(ess_title, " (bulk)"),
      subtitle = paste0(
        "Base model: ", base_model,
        "\nVariable: ", paste(variables, collapse = "\n")
      )
    ) +
    theme_bw()
  
  p_tail <- ggplot(ratio_nodes_impl,
                   aes(x = nodes, y = ratio_tail, color = implementation)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 1, linetype = "dashed") +
    scale_x_continuous(breaks = sort(unique(ratio_nodes_impl$nodes))) +
    labs(
      x = "Number of nodes",
      y = "Tail ESS ratio (model / base)",
      color = "Model",
      title = paste0(ess_title, " (tail)"),
      subtitle = paste0(
        "Base model: ", base_model,
        "\nVariable: ", paste(variables, collapse = ", ")
      )
    ) +
    theme_bw()
  
  # ---- 7) Side-by-side with patchwork if available ----
  if (requireNamespace("patchwork", quietly = TRUE)) {
    # optional: collect a single shared legend
    suppressPackageStartupMessages(library(patchwork))
    p <- p_bulk + p_tail + patchwork::plot_layout(guides = "collect")
    return(p)
  } else {
    # fallback: just print both and return them
    print(p_bulk)
    print(p_tail)
    invisible(list(bulk = p_bulk, tail = p_tail))
  }
}
