#' @noRd 
#' @keywords internal
.pad2 <- function(x) stringr::str_pad(as.character(x), 2, pad = "0")

#' @noRd 
#' @keywords internal
.pad5 <- function(x) stringr::str_pad(as.character(x), 5, pad = "0")

# ---------- Helper Functions for Order-Preserving Graph Construction ----------

#' Preprocess geographic units with order preservation
#' @param geo_units Vector of geographic units (ZIP codes, counties, or states)
#' @param geo_scale Character: "zip", "county", or "state"
#' @param pad_width Integer: padding width (5 for county, 2 for state, NULL for zip)
#' @return List with processed units in input order
#' @noRd 
#' @keywords internal
.preprocess_geo_units <- function(geo_units, geo_scale, pad_width = NULL) {
  units <- as.character(geo_units)
  
  # Apply padding based on scale
  if (!is.null(pad_width)) {
    units <- stringr::str_pad(units, pad_width, pad = "0")
  } else if (geo_scale == "zip") {
    units <- .pad5(units)
  }
  
  # Preserve input order
  units_ordered <- unique(units)
  
  list(
    units_ordered = units_ordered,
    original_units = units
  )
}

#' Map ZIP codes to ZCTAs with order preservation
#' @param zip_vec Vector of ZIP codes
#' @param xwalk Crosswalk data frame
#' @param zip_col Column name for ZIP codes in xwalk
#' @param zcta_col Column name for ZCTAs in xwalk
#' @param join_col Column name for join type in xwalk
#' @param isolate_when_join_is_na Logical: treat NA join types as isolates
#' @return List with ZCTA mapping preserving ZIP order
#' @noRd 
#' @keywords internal
.map_zip_to_zcta <- function(zip_vec, xwalk, zip_col, zcta_col, join_col, isolate_when_join_is_na) {
  zip_vec <- .pad5(zip_vec)
  zip_order <- unique(zip_vec)
  
  # Validate crosswalk columns
  req <- c(zip_col, zcta_col, join_col)
  if (!all(req %in% names(xwalk))) {
    stop("xwalk must contain: ", paste(req, collapse = ", "))
  }
  
  # Prepare crosswalk
  xw <- xwalk %>%
    dplyr::mutate(
      ZIP = .pad5(.data[[zip_col]]),
      ZCTA = .pad5(.data[[zcta_col]]),
      ZIP_JOIN = as.character(.data[[join_col]])
    ) %>%
    dplyr::filter(!is.na(.data$ZIP), !is.na(.data$ZCTA)) %>%
    dplyr::distinct() %>%
    as.data.frame()
  
  # Create mapping for each ZIP in input order
  zip_to_target <- character(length(zip_order))
  names(zip_to_target) <- zip_order
  is_isolate <- logical(length(zip_order))
  names(is_isolate) <- zip_order
  
  for (i in seq_along(zip_order)) {
    zip <- zip_order[i]
    
    # Find matching rows in crosswalk
    matches <- xw[xw$ZIP == zip, , drop = FALSE]
    
    if (nrow(matches) == 0) {
      # Unknown ZIP -> isolate
      zip_to_target[zip] <- zip  # Map to itself as isolate
      is_isolate[zip] <- TRUE
    } else {
      # Use first match
      match_row <- matches[1, ]
      is_match <- (match_row$ZIP_JOIN == "Zip matches ZCTA")
      is_match <- tidyr::replace_na(is_match, !isolate_when_join_is_na)
      
      if (is_match) {
        zip_to_target[zip] <- match_row$ZCTA
        is_isolate[zip] <- FALSE
      } else {
        zip_to_target[zip] <- zip  # Map to itself as isolate
        is_isolate[zip] <- TRUE
      }
    }
  }
  
  # Extract unique targets in order
  targets_ordered <- unique(zip_to_target[!is_isolate])
  isolates_ordered <- unique(zip_to_target[is_isolate])
  
  list(
    zip_to_target = zip_to_target,
    targets_ordered = targets_ordered,
    isolates_ordered = isolates_ordered,
    is_isolate = is_isolate
  )
}

#' Create local graph from national edges with order preservation
#' @param units_ordered Vector of units in desired order
#' @param nat National edge list with $ids, $node1, $node2
#' @param unit_to_target_map Optional named vector mapping units to targets (for ZIP->ZCTA)
#' @return List with local edges and unit-to-node mapping
#' @noRd 
#' @keywords internal
.create_local_graph <- function(units_ordered, nat, unit_to_target_map = NULL) {
  if (is.null(unit_to_target_map)) {
    # Direct mapping (county/state case)
    targets_ordered <- units_ordered
    isolates_ordered <- character(0)
    
    # Find which units exist in national data
    present_flag <- targets_ordered %in% nat$ids
    targets_present <- targets_ordered[present_flag]
    isolates_ordered <- targets_ordered[!present_flag]
  } else {
    # Mapped case (ZIP->ZCTA)
    targets_ordered <- unique(unit_to_target_map[!names(unit_to_target_map) %in% names(unit_to_target_map)[unit_to_target_map == names(unit_to_target_map)]])
    isolates_ordered <- unique(unit_to_target_map[unit_to_target_map == names(unit_to_target_map)])
    
    # Filter targets that exist in national data
    present_flag <- targets_ordered %in% nat$ids
    targets_present <- targets_ordered[present_flag]
    
    # Move missing targets to isolates
    missing_targets <- targets_ordered[!present_flag]
    if (length(missing_targets) > 0) {
      # Find original units that map to missing targets
      missing_units <- names(unit_to_target_map)[unit_to_target_map %in% missing_targets]
      isolates_ordered <- unique(c(isolates_ordered, missing_units))
      targets_present <- setdiff(targets_present, missing_targets)
    }
  }
  
  # Subset edges for present targets
  if (length(targets_present) > 0) {
    gidx <- match(targets_present, nat$ids)
    gidx <- gidx[!is.na(gidx)]
    
    if (length(gidx) > 0) {
      keep_flag <- logical(length(nat$ids))
      keep_flag[gidx] <- TRUE
      sel <- keep_flag[nat$node1] & keep_flag[nat$node2]
      
      n1g <- nat$node1[sel]
      n2g <- nat$node2[sel]
      map <- integer(length(nat$ids))
      map[gidx] <- seq_along(gidx)
      
      local_edges <- list(
        node1 = map[n1g],
        node2 = map[n2g],
        ids_local = nat$ids[gidx]
      )
    } else {
      local_edges <- list(
        node1 = integer(0),
        node2 = integer(0),
        ids_local = character(0)
      )
    }
  } else {
    local_edges <- list(
      node1 = integer(0),
      node2 = integer(0),
      ids_local = character(0)
    )
  }
  
  # Create unit-to-node mapping preserving input order
  # CRITICAL: Assign node indices based on input order, not connected/isolate grouping
  unit_to_node <- setNames(seq_along(units_ordered), units_ordered)
  n_total <- length(units_ordered)
  
  # Create reverse mapping from targets/isolates to the units that map to them
  # This will be used to update the edge indices
  if (is.null(unit_to_target_map)) {
    # Direct mapping case
    target_to_units <- setNames(as.list(units_ordered), units_ordered)
  } else {
    # ZIP->ZCTA mapping case
    target_to_units <- list()
    for (unit in units_ordered) {
      target <- unit_to_target_map[unit]
      if (is.null(target_to_units[[target]])) {
        target_to_units[[target]] <- character(0)
      }
      target_to_units[[target]] <- c(target_to_units[[target]], unit)
    }
  }
  
  # Update edge indices to reflect the input-order-based node numbering
  if (length(local_edges$node1) > 0) {
    # Map from local_edges$ids_local (which are targets) to input-order node indices
    target_to_first_unit_node <- integer(length(local_edges$ids_local))
    names(target_to_first_unit_node) <- local_edges$ids_local
    
    for (i in seq_along(local_edges$ids_local)) {
      target_id <- local_edges$ids_local[i]
      if (target_id %in% names(target_to_units)) {
        # Use the first unit that maps to this target
        first_unit <- target_to_units[[target_id]][1]
        target_to_first_unit_node[target_id] <- unit_to_node[first_unit]
      }
    }
    
    # Update edge node indices
    local_edges$node1 <- target_to_first_unit_node[local_edges$ids_local[local_edges$node1]]
    local_edges$node2 <- target_to_first_unit_node[local_edges$ids_local[local_edges$node2]]
  }
  
  list(
    local_edges = local_edges,
    unit_to_node = unit_to_node,
    n_total = n_total,
    targets_ordered = if (is.null(unit_to_target_map)) targets_present else local_edges$ids_local,
    isolates_ordered = isolates_ordered
  )
}

#' Analyze graph components (consolidated implementation)
#' @param node1 Vector of edge start nodes
#' @param node2 Vector of edge end nodes
#' @param N Total number of nodes
#' @return List with component information
#' @noRd
#' @keywords internal
.analyze_components <- function(node1, node2, N) {
  # Handle empty case
  if (N == 0L) {
    return(list(
      comp_id = integer(),
      N_comps = 0L,
      comp_sizes = integer(),
      comp_index = matrix(0L, 0, 0),
      isolate_nodes = integer(),
      connected_nodes = integer()
    ))
  }
  
  # Build adjacency list and find connected components
  adj <- vector("list", N)
  for (i in seq_len(N)) adj[[i]] <- integer(0)
  
  if (length(node1)) {
    for (k in seq_along(node1)) {
      a <- node1[k]; b <- node2[k]
      adj[[a]] <- c(adj[[a]], b)
      adj[[b]] <- c(adj[[b]], a)
    }
  }
  
  # Find connected components using DFS
  comp_id <- integer(N)
  comp <- 0L
  for (i in seq_len(N)) if (comp_id[i] == 0L) {
    comp <- comp + 1L
    q <- i
    while (length(q)) {
      v <- q[[1]]; q <- q[-1]
      if (comp_id[v] == 0L) {
        comp_id[v] <- comp
        if (length(adj[[v]])) q <- c(q, adj[[v]][comp_id[adj[[v]]] == 0L])
      }
    }
  }
  
  # Build component index structure
  if (comp == 0L) {
    comp_sizes <- integer()
    comp_index <- matrix(0L, 0, 0)
  } else {
    comp_sizes <- tabulate(comp_id, nbins = comp)
    max_sz <- max(comp_sizes)
    comp_index <- matrix(0L, nrow = max_sz, ncol = comp)
    for (c in seq_len(comp)) {
      idx <- which(comp_id == c)
      comp_index[seq_along(idx), c] <- idx
    }
  }
  
  # Identify isolates (components of size 1)
  isolate_comps <- which(comp_sizes == 1)
  isolate_nodes <- integer(0)
  if (length(isolate_comps) > 0) {
    for (ic in isolate_comps) {
      isolate_nodes <- c(isolate_nodes, comp_index[1, ic])
    }
  }
  
  connected_nodes <- setdiff(seq_len(N), isolate_nodes)
  
  list(
    comp_id = comp_id,
    N_comps = comp,
    comp_sizes = comp_sizes,
    comp_index = comp_index,
    isolate_nodes = isolate_nodes,
    connected_nodes = connected_nodes
  )
}

#' Compute BYM2 basis (simplified - no reordering needed)
#' @param node1 Vector of edge start nodes
#' @param node2 Vector of edge end nodes
#' @param N Total number of nodes
#' @param tol Tolerance for positive eigenvalues
#' @return List with R matrix
#' @noRd
#' @keywords internal
.compute_bym2_basis <- function(node1, node2, N, tol = 1e-10) {
  if (N == 0L || length(node1) == 0L) {
    return(list(
      R = matrix(numeric(), nrow = N, ncol = 0L),
      N_pos = 0L,
      comp_info = list(comp_id = integer(N), N_comps = 0L)
    ))
  }
  
  # Analyze components
  comp_info <- .analyze_components(node1, node2, N)
  
  if (comp_info$N_comps == 0L) {
    return(list(
      R = matrix(numeric(), nrow = N, ncol = 0L),
      N_pos = 0L,
      comp_info = comp_info
    ))
  }
  
  # Build sparse adjacency and Laplacian
  Wg <- Matrix::sparseMatrix(
    i = c(node1, node2),
    j = c(node2, node1),
    x = 1,
    dims = c(N, N)
  )
  W <- Matrix::forceSymmetric(Wg, uplo = "U")
  d <- as.numeric(Matrix::rowSums(W))
  Q <- Matrix::Diagonal(x = d) - W
  
  # Compute BYM2 basis for each component
  indices_by_comp <- split(seq_len(N), comp_info$comp_id)
  Rblocks <- vector("list", comp_info$N_comps)
  rows <- vector("list", comp_info$N_comps)
  
  for (c in seq_len(comp_info$N_comps)) {
    idx <- indices_by_comp[[c]]
    n_c <- length(idx)
    if (n_c <= 1L) next  # Skip isolates
    
    # Dense eigendecomposition for this component
    Qc <- as.matrix(Q[idx, idx, drop = FALSE])
    ee <- eigen(Qc, symmetric = TRUE)
    lam <- ee$values
    U <- ee$vectors
    
    # Keep positive eigenvalues
    keep <- lam > tol
    if (!any(keep)) next
    
    lam <- lam[keep]
    Upos <- U[, keep, drop = FALSE]
    
    # BYM2 scaling
    lam_inv <- 1 / lam
    diag_Qpin <- as.vector((Upos^2) %*% lam_inv)
    s_c <- exp(mean(log(diag_Qpin)))
    
    # Scaled basis
    Rc <- sweep(Upos, 2L, sqrt(lam), FUN = "/")
    Rc <- Rc / sqrt(s_c)
    
    Rblocks[[c]] <- Rc
    rows[[c]] <- idx
  }
  
  # Assemble R matrix - order is already preserved from node assignment
  ncols <- sum(vapply(Rblocks, function(M) if (is.null(M)) 0L else ncol(M), integer(1)))
  R <- matrix(0.0, nrow = N, ncol = ncols)
  
  off <- 0L
  for (c in seq_len(comp_info$N_comps)) {
    Rc <- Rblocks[[c]]
    if (is.null(Rc)) next
    p <- ncol(Rc)
    R[rows[[c]], (off + 1L):(off + p)] <- Rc
    off <- off + p
  }
  
  list(
    R = R,
    N_pos = ncols,
    comp_info = comp_info
  )
}

#' Build graph structure for geographic units
#' @param geo_units Vector of geographic units
#' @param geo_scale Character: "zip", "county", or "state"
#' @param nat National edge data
#' @param xwalk Crosswalk data (for ZIP scale only)
#' @param zip_col ZIP column name (for ZIP scale)
#' @param zcta_col ZCTA column name (for ZIP scale)
#' @param zip_join_type_col Join type column name (for ZIP scale)
#' @param isolate_when_join_is_na Logical (for ZIP scale)
#' @param verbose Logical: print progress
#' @return List with graph structure and mappings
#' @noRd 
#' @keywords internal
.build_graph_unified <- function(
    geo_units,
    geo_scale,
    nat,
    xwalk = NULL,
    zip_col = "ZIP_CODE",
    zcta_col = "ZCTA", 
    zip_join_type_col = "zip_join_type",
    isolate_when_join_is_na = TRUE,
    verbose = FALSE
) {
  # Preprocess units
  pad_width <- switch(geo_scale,
    "zip" = NULL,
    "county" = 5L,
    "state" = 2L
  )
  
  processed <- .preprocess_geo_units(geo_units, geo_scale, pad_width)
  units_ordered <- processed$units_ordered
  
  # Handle ZIP->ZCTA mapping if needed
  if (geo_scale == "zip") {
    if (is.null(xwalk)) stop("xwalk required for ZIP scale")
    
    zip_mapping <- .map_zip_to_zcta(
      units_ordered, xwalk, zip_col, zcta_col, 
      zip_join_type_col, isolate_when_join_is_na
    )
    unit_to_target_map <- zip_mapping$zip_to_target
  } else {
    unit_to_target_map <- NULL
  }
  
  # Create local graph
  local_graph <- .create_local_graph(units_ordered, nat, unit_to_target_map)
  
  # Compute BYM2 basis (order already preserved in node assignment)
  basis_result <- .compute_bym2_basis(
    local_graph$local_edges$node1,
    local_graph$local_edges$node2,
    local_graph$n_total
  )
  
  if (verbose) {
    cat(sprintf("[%s] nodes:%d | edges:%d | comps:%d | N_pos:%d\n",
                toupper(geo_scale), local_graph$n_total, 
                length(local_graph$local_edges$node1),
                basis_result$comp_info$N_comps, basis_result$N_pos))
  }
  
  result <- list(
    stan_graph = list(
      N_nodes = local_graph$n_total,
      N_edges = length(local_graph$local_edges$node1),
      node1 = if (length(local_graph$local_edges$node1) > 0) {
        local_graph$local_edges$node1
      } else {
        integer(0)
      },
      node2 = if (length(local_graph$local_edges$node2) > 0) {
        local_graph$local_edges$node2
      } else {
        integer(0)
      },
      N_comps = basis_result$comp_info$N_comps,
      comp_sizes = basis_result$comp_info$comp_sizes,
      comp_index = basis_result$comp_info$comp_index,
      N_pos = basis_result$N_pos,
      R = basis_result$R,
      N_iso = length(basis_result$comp_info$isolate_nodes),
      iso_idx = basis_result$comp_info$isolate_nodes
    ),
    ids_local = c(local_graph$targets_ordered, local_graph$isolates_ordered)
  )
  
  # Add appropriate mapping
  if (geo_scale == "zip") {
    result$zip_to_node <- local_graph$unit_to_node
  } else {
    result$id_to_node <- local_graph$unit_to_node
  }
  
  result
}


# ---------- Main Interface Function ----------

.build_graph <- function(geo_units, geo_scale, ...) {
  checkmate::assert_choice(
    geo_scale,
    choices = .const()$vars$geo,
    null.ok = FALSE
  )

  nat <- switch(geo_scale,
    "zip" = .fetch_data("zcta_adj.qs", subdir = "geo"),
    "county" = .fetch_data("county_adj.qs", subdir = "geo"),
    "state" = .fetch_data("state_adj.qs", subdir = "geo")
  )
  
  if (geo_scale == "zip") {
    xwalk <- .fetch_data("zip_zcta_2020.csv", subdir = "geo")
    .build_graph_unified(
      geo_units = geo_units,
      geo_scale = geo_scale,
      nat = nat,
      xwalk = xwalk,
      ...
    )
  } else {
    .build_graph_unified(
      geo_units = geo_units,
      geo_scale = geo_scale,
      nat = nat,
      ...
    )
  }
}
