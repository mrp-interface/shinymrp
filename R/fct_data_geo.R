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
.append_isolates <- function(n_sel, comp_id_sel, N_comps_sel, isolate_ids) {
  m_iso <- length(isolate_ids)
  if (m_iso == 0L) {
    return(list(
      comp_id_all   = comp_id_sel,
      N_comps_all   = N_comps_sel,
      N_nodes_total = n_sel
    ))
  }
  comp_id_iso <- seq.int(from = N_comps_sel + 1L, length.out = m_iso)
  list(
    comp_id_all   = c(comp_id_sel, comp_id_iso),
    N_comps_all   = N_comps_sel + m_iso,
    N_nodes_total = n_sel + m_iso
  )
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

.bym2_scale <- function(node1, node2, N) {
  stopifnot(length(node1) == length(node2))
  E <- length(node1)

  # Build sparse adjacency (binary, symmetric)
  # Note: add both (i,j) and (j,i) then coerce to 0/1.
  i <- c(node1, node2)
  j <- c(node2, node1)
  x <- rep(1, 2 * E)

  # Base R sparse via Matrix
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Please install the 'Matrix' package.")
  }
  W <- Matrix::sparseMatrix(i = i, j = j, x = x, dims = c(N, N))
  W@x[] <- 1  # ensure binary

  d <- Matrix::rowSums(W)
  Q <- Matrix::Diagonal(x = as.numeric(d)) - W  # graph Laplacian

  # Eigen-decompose Q (connected graph => one zero eigenvalue)
  # Use base eigen on dense if N is small; for larger N, consider RSpectra.
  Qd <- as.matrix(Q)
  eig <- eigen(Qd, symmetric = TRUE, only.values = FALSE)
  lam <- eig$values
  U   <- eig$vectors

  # Exclude the zero eigen (index 1 for connected graph)
  ord <- order(lam)
  lam <- lam[ord]
  U <- U[, ord, drop = FALSE]
  keep <- lam > 1e-12 * max(lam)         # drop all zeros/near-zeros
  lam_pos <- lam[keep]
  U_pos  <- U[, keep, drop = FALSE]

  # Moore–Penrose inverse: Q^+ = U_pos diag(1/lam_pos) U_pos^T
  # only need the diagonal entries of Q^+.
  inv_lam <- 1 / lam_pos
  # diag(Q^+) = row-wise dot of U_pos * inv_lam with U_pos
  # (efficient computation without forming the full matrix)
  v <- rowSums((U_pos^2) %*% diag(inv_lam, nrow = length(inv_lam)))

  # Geometric-mean scaling: s = exp(mean(log(diag(Q^+))))
  s <- exp(mean(log(v)))

  return(as.numeric(s))
}

# ---------- Generic (county/state) builder — ORDER-PRESERVING ----------
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
  
  # Subset the static edges to the present IDs (.subset_edges_for_ids preserves the order it receives)
  sub <- .subset_edges_for_ids(ids_present, nat)
  n_sel  <- length(sub$ids_local)
  node1  <- sub$node1
  node2  <- sub$node2
  
  # Components on the selected block
  cc <- .components_from_edges(n_sel, node1, node2)
  
  # Append isolates in caller order
  ext <- .append_isolates(n_sel, cc$comp_id, cc$N_comps, ids_isolate)
  
  # Components -> (sizes, index)
  cm <- .comp_index_from_comp_id(ext$comp_id_all)
  
  # Build lookup (present first, then isolates), all in caller order
  id_to_index_present <- setNames(seq_len(n_sel), sub$ids_local)
  id_to_index_isol    <- if (length(ids_isolate))
    setNames(n_sel + seq_along(ids_isolate), ids_isolate)
  else setNames(integer(0), character(0))
  id_to_node <- c(id_to_index_present, id_to_index_isol)
  
  if (verbose) {
    cat(sprintf("[%s] nodes:%d (present:%d, isolates:%d) | edges:%d | comps:%d\n",
                toupper(nat$geo), ext$N_nodes_total, n_sel, length(ids_isolate), length(node1), cm$N_comps))
  }
  
  list(
    id_to_node = id_to_node,   # named integer vector: ID -> local node (caller order)
    stan_graph = list(
      N_nodes      = as.integer(ext$N_nodes_total),
      N_edges      = as.integer(length(node1)),
      node1        = if (length(node1)) as.integer(node1) else integer(0),
      node2        = if (length(node2)) as.integer(node2) else integer(0),
      N_comps      = as.integer(cm$N_comps),
      comp_sizes   = as.integer(cm$comp_sizes),
      comp_index   = cm$comp_index,
      bym2_scale   = .bym2_scale(node1, node2, ext$N_nodes_total)
    ),
    ids_local = c(sub$ids_local, ids_isolate)   # local node order (present then isolates) in caller order
  )
}

# ---------- ZCTA wrapper — ORDER-PRESERVING ----------
.build_graph_zcta <- function(
    zip_vec,
    xwalk,
    nat,                    # from create_static_edges_one(geo="zcta", ...)
    zip_col           = "ZIP_CODE",
    zcta_col          = "ZCTA",
    zip_join_type_col = "zip_join_type",
    isolate_when_join_is_na = TRUE,
    verbose = FALSE
) {
  # 1) ZIP→ZCTA targets & isolates (unknown ZIPs ⇒ isolates), preserving caller order
  tgt <- .derive_targets_zcta(zip_vec, xwalk, zip_col, zcta_col, zip_join_type_col, isolate_when_join_is_na)
  zip_order <- tgt$zip_order
  
  # 2) Ensure non-isolate ZCTAs actually exist in the national set; else treat their ZIPs as isolates (preserve order)
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
  sub <- .subset_edges_for_ids(tgt$zctas_keep, nat)
  n_sel  <- length(sub$ids_local)
  node1  <- sub$node1
  node2  <- sub$node2
  
  # 4) Components on ZCTA block
  cc <- .components_from_edges(n_sel, node1, node2)
  
  # 5) Append ZIP isolates as singleton components (in caller order)
  ext <- .append_isolates(n_sel, cc$comp_id, cc$N_comps, tgt$zips_isolate)
  
  # 6) Components -> (sizes, index)
  cm <- .comp_index_from_comp_id(ext$comp_id_all)
  
  # 7) Build named ZIP -> node lookup (for J_zip) preserving input order
  zcta_to_index <- setNames(seq_len(n_sel), sub$ids_local)
  iso_to_index  <- if (length(tgt$zips_isolate))
    setNames(n_sel + seq_along(tgt$zips_isolate), tgt$zips_isolate)
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
    cat(sprintf("[ZCTA] nodes:%d (ZCTAs:%d, ZIP isolates:%d) | edges:%d | comps:%d\n",
                ext$N_nodes_total, n_sel, length(tgt$zips_isolate), length(node1), cm$N_comps))
  }
  
  list(
    zip_to_node = zip_to_node,  # named integer vector (ZIP -> local node) in caller order
    stan_graph  = list(
      N_nodes      = as.integer(ext$N_nodes_total),
      N_edges      = as.integer(length(node1)),
      node1        = if (length(node1)) as.integer(node1) else integer(0),
      node2        = if (length(node2)) as.integer(node2) else integer(0),
      N_comps      = as.integer(cm$N_comps),
      comp_sizes   = as.integer(cm$comp_sizes),
      comp_index   = cm$comp_index,
      bym2_scale   = .bym2_scale(node1, node2, ext$N_nodes_total)
    ),
    ids_local = c(sub$ids_local, tgt$zips_isolate)  # local node order (ZCTAs then ZIP isolates) in caller order
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
