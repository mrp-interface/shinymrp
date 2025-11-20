#' @noRd 
#' @keywords internal
.pad2 <- function(x) stringr::str_pad(as.character(x), 2, pad = "0")

#' @noRd 
#' @keywords internal
.pad5 <- function(x) stringr::str_pad(as.character(x), 5, pad = "0")

# ---------- shared utilities (no sf/spdep required) ----------

#' @noRd 
#' @keywords internal
.subset_edges_for_ids <- function(ids_keep, nat) {
  # ids_keep must be character and comparable to nat$ids (already zero-padded by Step 1)
  gidx <- match(ids_keep, nat$ids)
  gidx <- gidx[!is.na(gidx)]
  if (!length(gidx)) {
    return(list(node1 = integer(), node2 = integer(), ids_local = character()))
  }
  keep_flag <- logical(length(nat$ids)); keep_flag[gidx] <- TRUE
  sel <- keep_flag[nat$node1] & keep_flag[nat$node2]
  n1g <- nat$node1[sel]; n2g <- nat$node2[sel]
  map <- integer(length(nat$ids)); map[gidx] <- seq_along(gidx)
  list(
    node1     = map[n1g],
    node2     = map[n2g],
    ids_local = nat$ids[gidx]
  )
}

#' @noRd 
#' @keywords internal
.components_from_edges <- function(n, node1, node2) {
  if (n == 0L) return(list(comp_id = integer(), N_comps = 0L))
  adj <- vector("list", n)
  for (i in seq_len(n)) adj[[i]] <- integer(0)
  if (length(node1)) {
    for (k in seq_along(node1)) {
      a <- node1[k]; b <- node2[k]
      adj[[a]] <- c(adj[[a]], b)
      adj[[b]] <- c(adj[[b]], a)
    }
  }
  comp_id <- integer(n)
  comp <- 0L
  for (i in seq_len(n)) if (comp_id[i] == 0L) {
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
  list(comp_id = as.integer(comp_id), N_comps = as.integer(comp))
}

#' @noRd 
#' @keywords internal
.comp_index_from_comp_id <- function(comp_id_all) {
  if (!length(comp_id_all)) {
    return(list(N_comps = 0L, comp_sizes = integer(), comp_index = matrix(0L, 0, 0)))
  }
  C <- max(comp_id_all)
  comp_sizes <- tabulate(comp_id_all, nbins = C)
  max_sz <- max(comp_sizes)
  M <- matrix(0L, nrow = max_sz, ncol = C)
  for (c in seq_len(C)) {
    idx <- which(comp_id_all == c)
    M[seq_along(idx), c] <- as.integer(idx)
  }
  list(N_comps = as.integer(C), comp_sizes = as.integer(comp_sizes), comp_index = M)
}

# ---------- ZCTA-specific helper (ORDER-PRESERVING) ----------
.derive_targets_zcta <- function(zip_vec, xwalk, zip_col, zcta_col, join_col, isolate_when_join_is_na) {
  zip_vec <- .pad5(zip_vec)
  zip_order <- unique(zip_vec)  # <- preserve caller order
  
  req <- c(zip_col, zcta_col, join_col)
  if (!all(req %in% names(xwalk))) stop("xwalk must contain: ", paste(req, collapse = ", "))
  
  xw <- xwalk %>%
    mutate(
      ZIP      = .pad5(.data[[zip_col]]),
      ZCTA     = .pad5(.data[[zcta_col]]),
      ZIP_JOIN = as.character(.data[[join_col]])
    ) %>%
    filter(!is.na(ZIP), !is.na(ZCTA)) %>%
    distinct() %>%
    as.data.frame()
  
  # rows matching requested ZIPs, then reorder rows by first occurrence in input
  need <- xw %>% filter(.data$ZIP %in% zip_order) %>% as.data.frame()
  if (nrow(need)) {
    need$..ord <- match(need$ZIP, zip_order)
    need <- need[order(need$..ord), , drop = FALSE]
    need$..ord <- NULL
  }
  
  # Unknown ZIPs (not in crosswalk) → isolates (in input order)
  known_zip <- unique(need$ZIP)
  unknown_zip <- setdiff(zip_order, known_zip)
  if (length(unknown_zip)) {
    need <- dplyr::bind_rows(
      need,
      data.frame(ZIP = unknown_zip, ZCTA = NA_character_, ZIP_JOIN = "Not in crosswalk", stringsAsFactors = FALSE)
    )
  }
  # Reorder again to input order after bind_rows
  need$..ord <- match(need$ZIP, zip_order)
  need <- need[order(need$..ord), , drop = FALSE]
  need$..ord <- NULL
  
  is_match   <- (need$ZIP_JOIN == "Zip matches ZCTA")
  is_match   <- tidyr::replace_na(is_match, !isolate_when_join_is_na)
  is_unknown <- (need$ZIP_JOIN == "Not in crosswalk")
  need$.is_isolate <- (!is_match) | is_unknown
  
  list(
    zctas_keep   = unique(need$ZCTA[!need$.is_isolate & !is.na(need$ZCTA)]),  # <- order from input
    zips_isolate = unique(need$ZIP[need$.is_isolate]),                        # <- order from input
    need_df      = need,
    zip_order    = zip_order
  )
}

#' BYM2 basis (scaled ICAR eigen-basis) per connected component
#'
#' Builds an orthonormal basis R such that, within each connected component,
#'   R R^T approximates the Moore–Penrose inverse of the ICAR precision (graph
#'   Laplacian) and is BYM2-scaled so that
#'   geometric_mean(diag(Q^+)) = 1 in that component.
#'
#' @param node1,node2 Integer vectors of equal length with 1-based endpoints of undirected edges.
#' @param N Integer number of nodes.
#' @param tol Numeric tolerance to decide strictly-positive eigenvalues.
#' @noRd 
#' @keywords internal
.bym2_basis <- function(node1, node2, N, tol = 1e-10) {
  stopifnot(N >= 0L, length(node1) == length(node2))
  
  # ---- components (returns comp_id, N_comps, etc.)
  comp <- .components_from_edges(N, node1, node2)
  
  if (N == 0L || comp$N_comps == 0L) {
    return(list(
      R     = matrix(numeric(), nrow = N, ncol = 0L),
      N_pos = 0L,
      comp  = comp
    ))
  }
  
  # ---- sparse symmetric adjacency and Laplacian Q
  Wg <- Matrix::sparseMatrix(
    i    = c(node1, node2),
    j    = c(node2, node1),
    x    = 1,
    dims = c(N, N)
  )
  W  <- Matrix::forceSymmetric(Wg, uplo = "U")
  d  <- as.numeric(Matrix::rowSums(W))
  Q  <- Matrix::Diagonal(x = d) - W
  
  # Precompute node indices per component
  indices_by_comp <- split(seq_len(N), comp$comp_id)
  
  # ---- loop over components, build BYM2-scaled blocks
  Rblocks <- vector("list", comp$N_comps)
  rows    <- vector("list", comp$N_comps)
  
  for (c in seq_len(comp$N_comps)) {
    idx <- indices_by_comp[[c]]
    n_c <- length(idx)
    if (n_c <= 1L) next  # isolate: contributes no columns
    
    # Dense eigendecomposition (Qc is small per component in typical spatial graphs)
    Qc <- as.matrix(Q[idx, idx, drop = FALSE])
    ee <- eigen(Qc, symmetric = TRUE)
    lam <- ee$values
    U   <- ee$vectors
    
    # Keep strictly positive eigenvalues (drop the single zero EV per component)
    keep <- lam > tol
    if (!any(keep)) next
    
    lam  <- lam[keep]
    Upos <- U[, keep, drop = FALSE]
    
    # diag(Q^+) = rowSums(Upos^2 %*% diag(1/lam)), BYM2 scale s = GM(diag(Q^+))
    lam_inv   <- 1 / lam
    diag_Qpin <- as.vector((Upos^2) %*% lam_inv)
    s_c       <- exp(mean(log(diag_Qpin)))
    
    # R_c = Upos %*% diag(1/sqrt(lam)) / sqrt(s_c)
    # use sweep instead of building an explicit diagonal matrix
    Rc <- sweep(Upos, 2L, sqrt(lam), FUN = "/")
    Rc <- Rc / sqrt(s_c)
    
    Rblocks[[c]] <- Rc
    rows[[c]]    <- idx
  }
  
  # ---- assemble R
  ncols <- sum(vapply(Rblocks, function(M) if (is.null(M)) 0L else ncol(M), integer(1)))
  R     <- matrix(0.0, nrow = N, ncol = ncols)
  
  off <- 0L
  for (c in seq_len(comp$N_comps)) {
    Rc <- Rblocks[[c]]
    if (is.null(Rc)) next
    p <- ncol(Rc)
    R[rows[[c]], (off + 1L):(off + p)] <- Rc
    off <- off + p
  }
  
  list(
    R     = R,
    N_pos = ncol(R),
    comp  = comp
  )
}

# ---------- Generic (county/state) builder — ORDER-PRESERVING + BYM2 ----------
.build_graph_other <- function(
    ids_vec,
    nat,                   # from create_static_edges_one(geo="county"/"state")
    pad_width = NULL,      # 5 for county, 2 for state, NULL if already clean
    verbose   = FALSE
) {
  ids_vec <- as.character(ids_vec)
  if (!is.null(pad_width)) ids_vec <- stringr::str_pad(ids_vec, pad_width, pad = "0")
  ids_vec_u <- unique(ids_vec)   # keep caller order

  present_flag <- ids_vec_u %in% nat$ids
  ids_present  <- ids_vec_u[present_flag]       # in caller order
  ids_isolate  <- ids_vec_u[!present_flag]      # in caller order

  # Subset static edges to the present IDs (preserves the order it receives)
  sub   <- .subset_edges_for_ids(ids_present, nat)
  n_sel <- length(sub$ids_local)
  node1 <- sub$node1
  node2 <- sub$node2

  # Local node order: present first (exact input order), then isolates (exact input order)
  N_iso   <- length(ids_isolate)
  N_total <- n_sel + N_iso

  # Build BYM2 reduced-rank basis in this exact node order.
  # Edges reference only the first n_sel nodes; isolates occupy indices n_sel+1..N_total.
  basis <- .bym2_basis(node1, node2, N = N_total)

  # Components summary derived from the basis' comp_id (keeps everything in-sync)
  cm <- .comp_index_from_comp_id(basis$comp_id)

  # ID -> local node index (present first, then isolates), all in caller order
  id_to_index_present <- setNames(seq_len(n_sel), sub$ids_local)
  id_to_index_isol    <- if (N_iso)
    setNames(n_sel + seq_len(N_iso), ids_isolate)
  else setNames(integer(0), character(0))
  id_to_node <- c(id_to_index_present, id_to_index_isol)

  if (verbose) {
    cat(sprintf("[%s] nodes:%d (present:%d, isolates:%d) | edges:%d | comps:%d | N_pos:%d\n",
                toupper(nat$geo), N_total, n_sel, N_iso, length(node1), cm$N_comps, basis$N_pos))
  }

  list(
    id_to_node = id_to_node,                      # named int: ID -> local node (builder order)
    stan_graph = list(
      N_nodes    = N_total,
      N_edges    = length(node1),
      node1      = if (length(node1) > 0) node1 else integer(0),
      node2      = if (length(node2) > 0) node2 else integer(0),
      N_pos      = basis$N_pos,
      R          = basis$R
    ),
    ids_local = c(sub$ids_local, ids_isolate)     # local node order (present then isolates)
  )
}

# ---------- ZCTA wrapper — ORDER-PRESERVING + BYM2 ----------------------------
.build_graph_zcta <- function(
    zip_vec,
    xwalk,
    nat,                    # from create_static_edges_one(geo="zcta", ...)
    zip_col           = "ZIP_CODE",
    zcta_col          = "ZCTA",
    zip_join_type_col = "zip_join_type",
    isolate_when_join_is_na = TRUE,
    alpha = 1,              # BYM2 order (1 = standard ICAR/BYM2)
    verbose = FALSE
) {
  # 1) ZIP→ZCTA targets & isolates (unknown ZIPs ⇒ isolates), preserving caller order
  tgt <- .derive_targets_zcta(zip_vec, xwalk, zip_col, zcta_col, zip_join_type_col, isolate_when_join_is_na)
  zip_order <- tgt$zip_order

  # 2) Ensure non-isolate ZCTAs actually exist in the national set; else treat those ZIPs as isolates
  missing_zcta <- setdiff(tgt$zctas_keep, nat$ids)
  if (length(missing_zcta)) {
    move <- unique(tgt$need_df$ZIP[tgt$need_df$ZCTA %in% missing_zcta & !tgt$need_df$.is_isolate])
    if (length(move)) {
      tgt$zips_isolate <- unique(c(tgt$zips_isolate, move))
      tgt$zctas_keep   <- setdiff(tgt$zctas_keep, missing_zcta)
      tgt$need_df$.is_isolate[tgt$need_df$ZIP %in% move] <- TRUE
    }
  }

  # 3) Subset static edges to ZCTAs we need (in the order of zctas_keep)
  sub   <- .subset_edges_for_ids(tgt$zctas_keep, nat)
  n_sel <- length(sub$ids_local)
  node1 <- sub$node1
  node2 <- sub$node2

  # 4) Local node order for the graph: ZCTAs first (input order), then ZIP isolates (input order)
  N_iso   <- length(tgt$zips_isolate)
  N_total <- n_sel + N_iso

  # 5) BYM2 reduced-rank basis in this exact node order
  basis <- .bym2_basis(node1, node2, N = N_total, alpha = alpha)

  # 6) Components summary from basis (ensures consistency with R)
  cm <- .comp_index_from_comp_id(basis$comp_id)

  # 7) Named ZIP -> node lookup (J_zip) preserving caller order
  zcta_to_index <- setNames(seq_len(n_sel), sub$ids_local)
  iso_to_index  <- if (N_iso)
    setNames(n_sel + seq_len(N_iso), tgt$zips_isolate)
  else setNames(integer(0), character(0))

  uniq_zips <- unique(.pad5(zip_vec))
  zip_to_node <- setNames(rep.int(NA_integer_, length(uniq_zips)), uniq_zips)
  for (z in uniq_zips) {
    if (z %in% names(iso_to_index)) {
      zip_to_node[[z]] <- iso_to_index[[z]]
    } else {
      rows <- which(tgt$need_df$ZIP == z & !tgt$need_df$.is_isolate)
      if (length(rows)) {
        zcands <- unique(tgt$need_df$ZCTA[rows])
        zhit   <- zcands[zcands %in% names(zcta_to_index)]
        if (length(zhit)) zip_to_node[[z]] <- zcta_to_index[[zhit[1]]]
      }
    }
  }

  if (verbose) {
    cat(sprintf("[ZCTA] nodes:%d (ZCTAs:%d, ZIP isolates:%d) | edges:%d | comps:%d | N_pos:%d\n",
                N_total, n_sel, N_iso, length(node1), cm$N_comps, basis$N_pos))
  }

  list(
    zip_to_node = zip_to_node,  # named int (ZIP -> local node) in caller order
    stan_graph  = list(
      N_nodes    = N_total,
      N_edges    = length(node1),
      node1      = if (length(node1) > 0) node1 else integer(0),
      node2      = if (length(node2) > 0) node2 else integer(0),
      N_pos      = basis$N_pos,
      R          = basis$R
    ),
    ids_local = c(sub$ids_local, tgt$zips_isolate)  # local node order (ZCTAs then ZIP isolates)
  )
}

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
  
  switch(geo_scale,
    "zip" = .build_graph_zcta(
      zip_vec = geo_units,
      xwalk = .fetch_data("zip_zcta_2020.csv", subdir = "geo"),
      nat = nat,
      ...
    ),
    .build_graph_other(
      ids_vec = geo_units,
      nat = nat,
      ...
    )
  )
  
}
