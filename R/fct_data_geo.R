#' @noRd 
#' @keywords internal
.pad2 <- function(x) stringr::str_pad(as.character(x), 2, pad = "0")

#' @noRd 
#' @keywords internal
.pad5 <- function(x) stringr::str_pad(as.character(x), 5, pad = "0")

#' Create mapping from ZIP to ZCTA with ordering preserved
#' @param zip_vec Vector of ZIP codes to map
#' @param xwalk Data frame containing ZIP to ZCTA crosswalk
#' @param zip_col Name of ZIP code column in xwalk
#' @param zcta_col Name of ZCTA column in xwalk
#' @param join_type_col Name of join type column in xwalk
#' @param na_value Value to assign when no valid mapping exists
#' 
#' @noRd 
#' @keywords internal
.map_zip_to_zcta <- function(
    zip_vec,
    xwalk,
    zip_col = "ZIP_CODE",
    zcta_col = "ZCTA", 
    join_type_col = "zip_join_type",
    na_value = "00000"
) {
  
  # Prepare crosswalk
  xwalk <- xwalk %>%
    dplyr::mutate(
      ZIP = .pad5(.data[[zip_col]]),
      ZCTA = .pad5(.data[[zcta_col]]),
      ZIP_JOIN = as.character(.data[[join_type_col]])
    ) %>%
    dplyr::filter(!is.na(.data$ZIP), !is.na(.data$ZCTA)) %>%
    dplyr::distinct()
  
  # Map ZIPs to ZCTAs
  xwalk_subset <- data.frame(ZIP_CODE = zip_vec) %>%
    left_join(xwalk, by = "ZIP_CODE")
  zcta_vec <- xwalk_subset %>% dplyr::pull("ZCTA")
  zcta_vec[xwalk_subset$ZIP_JOIN != "Zip matches ZCTA"] <- na_value
  
  zcta_vec
}

#' Create local graph for specified units
#' @param units Vector of geographic units
#' @param full_graph Full graph data frame with columns: ids, node1, node2
#' 
#' @noRd 
#' @keywords internal
.create_local_graph <- function(units, full_graph) {
  # Create index mapping
  idx <- purrr::map_int(
    units, 
    ~ if (.x %in% full_graph$ids)
      which(full_graph$ids == .x)
    else
      0
  )
  idx_map <- data.frame(
    full_id = idx,
    local_id = seq_along(idx)
  )
  
  # Filter edges to local graph
  local_graph <- full_graph[c("node1", "node2")] %>%
    as.data.frame() %>%
    filter(
      .data$node1 %in% idx &
      .data$node2 %in% idx
    )
  
  # Map to local indices
  node1_map <- local_graph %>%
    mutate(full_id = .data$node1) %>%
    select("full_id") %>%
    left_join(idx_map, by = "full_id")
    
  node2_map <- local_graph %>%
    mutate(full_id = .data$node2) %>%
    select("full_id") %>%
    left_join(idx_map, by = "full_id")
  
  list(
    node1 = node1_map$local_id,
    node2 = node2_map$local_id
  )
}

#' Analyze graph components (consolidated implementation)
#' @param node1 Vector of edge start nodes
#' @param node2 Vector of edge end nodes
#' @param n_nodes Total number of nodes
#' 
#' @noRd
#' @keywords internal
.analyze_components <- function(node1, node2, n_nodes) {
  # Handle empty case
  if (n_nodes == 0L) {
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
  adj <- vector("list", n_nodes)
  for (i in seq_len(n_nodes)) adj[[i]] <- integer(0)
  
  if (length(node1)) {
    for (k in seq_along(node1)) {
      a <- node1[k]; b <- node2[k]
      adj[[a]] <- c(adj[[a]], b)
      adj[[b]] <- c(adj[[b]], a)
    }
  }
  
  # Find connected components using DFS
  comp_id <- integer(n_nodes)
  comp <- 0L
  for (i in seq_len(n_nodes)) if (comp_id[i] == 0L) {
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
  
  connected_nodes <- setdiff(seq_len(n_nodes), isolate_nodes)
  
  list(
    comp_id = comp_id,
    N_comps = comp,
    comp_sizes = comp_sizes,
    comp_index = comp_index,
    isolate_nodes = isolate_nodes,
    connected_nodes = connected_nodes
  )
}

#' Compute ICAR basis
#' @param node1 Vector of edge start nodes
#' @param node2 Vector of edge end nodes
#' @param n_nodes Total number of nodes
#' @param tol Tolerance for positive eigenvalues
#' @return List with R matrix
#' 
#' @noRd
#' @keywords internal
.compute_icar_basis <- function(node1, node2, n_nodes, tol = 1e-10) {
  if (n_nodes == 0 || length(node1) == 0) {
    return(list(
      R = matrix(numeric(), nrow = n_nodes, ncol = 0),
      N_pos = 0,
      comp_info = list(comp_id = integer(n_nodes), N_comps = 0)
    ))
  }
  
  # Analyze components
  comp_info <- .analyze_components(node1, node2, n_nodes)
  
  if (comp_info$N_comps == 0) {
    return(list(
      R = matrix(numeric(), nrow = n_nodes, ncol = 0),
      N_pos = 0,
      comp_info = comp_info
    ))
  }
  
  # Build sparse adjacency and Laplacian
  Wg <- Matrix::sparseMatrix(
    i = c(node1, node2),
    j = c(node2, node1),
    x = 1,
    dims = c(n_nodes, n_nodes)
  )
  W <- Matrix::forceSymmetric(Wg, uplo = "U")
  d <- as.numeric(Matrix::rowSums(W))
  Q <- Matrix::Diagonal(x = d) - W
  
  # Compute BYM2 basis for each component
  indices_by_comp <- split(seq_len(n_nodes), comp_info$comp_id)
  Rblocks <- vector("list", comp_info$N_comps)
  rows <- vector("list", comp_info$N_comps)

  for (c in seq_len(comp_info$N_comps)) {
    idx <- indices_by_comp[[c]]
    n_c <- length(idx)
    if (n_c <= 1) next  # Skip isolates
    
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
    Rc <- sweep(Upos, 2, sqrt(lam), FUN = "/")
    Rc <- Rc / sqrt(s_c)
    
    Rblocks[[c]] <- Rc
    rows[[c]] <- idx
  }

  # Assemble R matrix - order is already preserved from node assignment
  ncols <- sum(vapply(Rblocks, function(M) if (is.null(M)) 0L else ncol(M), integer(1)))
  R <- matrix(0.0, nrow = n_nodes, ncol = ncols)
  
  off <- 0
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

#' Create neighborhood graph and compute ICAR basis
#' @param geo_units Vector of geographic units
#' @param geo_scale Geographic scale ("zip", "county", "state")
#' @param verbose Logical indicating whether to print summary information
#' 
#' @noRd
#' @keywords internal
.build_graph <- function(geo_units, geo_scale, verbose = FALSE) {
  checkmate::assert_choice(
    geo_scale,
    choices = .const()$vars$geo,
    null.ok = FALSE
  )

  # Fetch full graph data based on geographic scale
  full_graph <- switch(geo_scale,
    "zip" = .fetch_data("zcta_adj.qs", subdir = "geo"),
    "county" = .fetch_data("county_adj.qs", subdir = "geo"),
    "state" = .fetch_data("state_adj.qs", subdir = "geo")
  )

  geo_units <- unique(geo_units)
  n_nodes <- length(geo_units)
  
  # Map ZIP to ZCTA if needed
  if (geo_scale == "zip") {
    xwalk <- .fetch_data("zip_zcta_2020.csv", subdir = "geo")
    geo_units <- .map_zip_to_zcta(geo_units, xwalk)
  }

  # Create local graph
  local_graph <- .create_local_graph(geo_units, full_graph)
  n_edges <- length(local_graph$node1)

  # Compute BYM2 basis matrix
  icar_basis <- .compute_icar_basis(
    local_graph$node1,
    local_graph$node2,
    n_nodes
  )

  if (verbose) {
    cat(sprintf(
      "[%s] | nodes:%d | edges:%d | comps:%d | N_pos:%d\n",
      toupper(geo_scale), n_nodes, n_edges,
      icar_basis$comp_info$N_comps, icar_basis$N_pos
    ))
  }

  list(
    N_nodes = n_nodes,
    N_edges = n_edges,
    node1 = if (n_edges > 0) local_graph$node1 else integer(0),
    node2 = if (n_edges > 0) local_graph$node2 else integer(0),
    N_pos = icar_basis$N_pos,
    R = icar_basis$R,
    N_iso = length(icar_basis$comp_info$isolate_nodes),
    iso_idx = icar_basis$comp_info$isolate_nodes
  )
}
