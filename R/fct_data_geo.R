# ---------- order helper ----------
unique_in_order <- function(x) x[!duplicated(x)]

# ---------- unchanged: subset_edges_for_ids / components_from_edges / append_isolates / comp_index_from_comp_id ----------
# (your originals work as long as we feed them ids in the desired order)

# ---------- ZCTA-specific helper (ORDER-PRESERVING) ----------
derive_targets_zcta <- function(zip_vec, xwalk, zip_col, zcta_col, join_col, isolate_when_join_is_na) {
  zip_vec <- pad5(zip_vec)
  zip_order <- unique_in_order(zip_vec)  # <- preserve caller order
  
  req <- c(zip_col, zcta_col, join_col)
  if (!all(req %in% names(xwalk))) stop("xwalk must contain: ", paste(req, collapse = ", "))
  
  xw <- xwalk %>%
    transmute(
      ZIP      = pad5(.data[[zip_col]]),
      ZCTA     = pad5(.data[[zcta_col]]),
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
    zctas_keep   = unique_in_order(need$ZCTA[!need$.is_isolate & !is.na(need$ZCTA)]),  # <- order from input
    zips_isolate = unique_in_order(need$ZIP[need$.is_isolate]),                        # <- order from input
    need_df      = need,
    zip_order    = zip_order
  )
}

# ---------- Generic (county/state) builder — ORDER-PRESERVING ----------
build_graph_data_for_stan_other <- function(
    ids_vec,
    nat,                   # from create_static_edges_one(geo="county"/"state")
    pad_width = NULL,      # 5 for county, 2 for state, NULL if already clean
    verbose   = TRUE
) {
  ids_vec <- as.character(ids_vec)
  if (!is.null(pad_width)) ids_vec <- str_pad(ids_vec, pad_width, pad = "0")
  ids_vec_u <- unique_in_order(ids_vec)   # keep caller order
  
  present_flag <- ids_vec_u %in% nat$ids
  ids_present  <- ids_vec_u[present_flag]       # in caller order
  ids_isolate  <- ids_vec_u[!present_flag]      # in caller order
  
  # Subset the static edges to the present IDs (subset_edges_for_ids preserves the order it receives)
  sub <- subset_edges_for_ids(ids_present, nat)
  n_sel  <- length(sub$ids_local)
  node1  <- sub$node1
  node2  <- sub$node2
  
  # Components on the selected block
  cc <- components_from_edges(n_sel, node1, node2)
  
  # Append isolates in caller order
  ext <- append_isolates(n_sel, cc$comp_id, cc$N_comps, ids_isolate)
  
  # Components -> (sizes, index)
  cm <- comp_index_from_comp_id(ext$comp_id_all)
  
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
      comp_index   = cm$comp_index
    ),
    ids_local = c(sub$ids_local, ids_isolate)   # local node order (present then isolates) in caller order
  )
}

# ---------- ZCTA wrapper — ORDER-PRESERVING ----------
build_graph_data_for_stan_zcta <- function(
    zip_vec,
    xwalk,
    nat,                    # from create_static_edges_one(geo="zcta", ...)
    zip_col           = "ZIP_CODE",
    zcta_col          = "ZCTA",
    zip_join_type_col = "zip_join_type",
    isolate_when_join_is_na = TRUE,
    verbose = TRUE
) {
  # 1) ZIP→ZCTA targets & isolates (unknown ZIPs ⇒ isolates), preserving caller order
  tgt <- derive_targets_zcta(zip_vec, xwalk, zip_col, zcta_col, zip_join_type_col, isolate_when_join_is_na)
  zip_order <- tgt$zip_order
  
  # 2) Ensure non-isolate ZCTAs actually exist in the national set; else treat their ZIPs as isolates (preserve order)
  missing_zcta <- setdiff(tgt$zctas_keep, nat$ids)
  if (length(missing_zcta)) {
    move <- unique_in_order(tgt$need_df$ZIP[tgt$need_df$ZCTA %in% missing_zcta & !tgt$need_df$.is_isolate])
    if (length(move)) {
      tgt$zips_isolate <- unique_in_order(c(tgt$zips_isolate, move))
      tgt$zctas_keep   <- setdiff(tgt$zctas_keep, missing_zcta)
      tgt$need_df$.is_isolate[tgt$need_df$ZIP %in% move] <- TRUE
    }
  }
  
  # 3) Subset static edges to ZCTAs we need (in the order of zctas_keep)
  sub <- subset_edges_for_ids(tgt$zctas_keep, nat)
  n_sel  <- length(sub$ids_local)
  node1  <- sub$node1
  node2  <- sub$node2
  
  # 4) Components on ZCTA block
  cc <- components_from_edges(n_sel, node1, node2)
  
  # 5) Append ZIP isolates as singleton components (in caller order)
  ext <- append_isolates(n_sel, cc$comp_id, cc$N_comps, tgt$zips_isolate)
  
  # 6) Components -> (sizes, index)
  cm <- comp_index_from_comp_id(ext$comp_id_all)
  
  # 7) Build named ZIP -> node lookup (for J_zip) preserving input order
  zcta_to_index <- setNames(seq_len(n_sel), sub$ids_local)
  iso_to_index  <- if (length(tgt$zips_isolate))
    setNames(n_sel + seq_along(tgt$zips_isolate), tgt$zips_isolate)
  else setNames(integer(0), character(0))
  
  uniq_zips <- unique_in_order(pad5(zip_vec))
  zip_to_node <- setNames(rep.int(NA_integer_, length(uniq_zips)), uniq_zips)
  for (z in uniq_zips) {
    if (z %in% names(iso_to_index)) {
      zip_to_node[[z]] <- iso_to_index[[z]]
    } else {
      rows <- which(tgt$need_df$ZIP == z & !tgt$need_df$.is_isolate)
      if (length(rows)) {
        zcands <- unique_in_order(tgt$need_df$ZCTA[rows])
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
      comp_index   = cm$comp_index
    ),
    ids_local = c(sub$ids_local, tgt$zips_isolate)  # local node order (ZCTAs then ZIP isolates) in caller order
  )
}

build_graph_data_for_stan <- function(geo_units, geo_scale) {
  nat <- switch(geo_scale,
    "zip" = .fetch_data("zcta_adj.qs", subdir = "geo"),
    "county" = .fetch_data("county_adj.qs", subdir = "geo"),
    "state" = .fetch_data("state_adj.qs", subdir = "geo")
  )
  
  switch(geo_scale,
    "zip" = build_graph_data_for_stan_zcta(
      zip_vec = geo_units,
      xwalk = .fetch_data("zip_zcta_2020.csv", subdir = "geo"),
      nat = nat
    ),
    build_graph_data_for_stan_other(
      ids_vec = geo_units,
      nat = nat
    )
  )
  
}