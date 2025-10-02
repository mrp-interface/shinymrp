# deps: dplyr, tidyr, stringr
library(dplyr)
library(tidyr)
library(stringr)

pad2 <- function(x) str_pad(as.character(x), 2, pad = "0")
pad5 <- function(x) str_pad(as.character(x), 5, pad = "0")

# ---------- shared utilities (no sf/spdep required) ----------

subset_edges_for_ids <- function(ids_keep, nat) {
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

components_from_edges <- function(n, node1, node2) {
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

append_isolates <- function(n_sel, comp_id_sel, N_comps_sel, isolate_ids) {
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

comp_index_from_comp_id <- function(comp_id_all) {
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

# ---------- ZCTA-specific helper (uses ZIP→ZCTA crosswalk) ----------

derive_targets_zcta <- function(zip_vec, xwalk, zip_col, zcta_col, join_col, isolate_when_join_is_na) {
  zip_vec <- pad5(zip_vec)
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
  
  need <- xw %>% filter(.data$ZIP %in% zip_vec) %>% as.data.frame()
  
  # Unknown ZIPs (not in crosswalk) → isolates
  unknown_zip <- setdiff(unique(zip_vec), need$ZIP)
  if (length(unknown_zip)) {
    need <- dplyr::bind_rows(
      need,
      data.frame(ZIP = unknown_zip, ZCTA = NA_character_, ZIP_JOIN = "Not in crosswalk", stringsAsFactors = FALSE)
    )
  }
  
  is_match   <- (need$ZIP_JOIN == "Zip matches ZCTA")
  is_match   <- tidyr::replace_na(is_match, !isolate_when_join_is_na)
  is_unknown <- (need$ZIP_JOIN == "Not in crosswalk")
  need$.is_isolate <- (!is_match) | is_unknown
  
  list(
    zctas_keep   = unique(need$ZCTA[!need$.is_isolate & !is.na(need$ZCTA)]),
    zips_isolate = sort(unique(need$ZIP[need$.is_isolate])),
    need_df      = need
  )
}

# ---------- Generic (county/state) builder from requested IDs ----------

# ids_vec: requested IDs in the SAME coding as nat$ids
# - county: 5-digit FIPS ("06037"), state: 2-digit FIPS ("06")
# - they'll be padded if pad_width is supplied
build_graph_data_for_stan <- function(
    ids_vec,
    nat,                   # from create_static_edges_one(geo="county"/"state")
    pad_width = NULL,      # 5 for county, 2 for state, NULL if already clean
    verbose   = TRUE
) {
  ids_vec <- as.character(ids_vec)
  if (!is.null(pad_width)) ids_vec <- str_pad(ids_vec, pad_width, pad = "0")
  
  # Split requested IDs into those present in nat vs. not (isolates)
  ids_present  <- intersect(unique(ids_vec), nat$ids)
  ids_isolate  <- setdiff(unique(ids_vec), ids_present)
  
  # Subset the static edges to the present IDs
  sub <- subset_edges_for_ids(ids_present, nat)
  n_sel  <- length(sub$ids_local)
  node1  <- sub$node1
  node2  <- sub$node2
  
  # Components on the selected block
  cc <- components_from_edges(n_sel, node1, node2)
  
  # Append isolates (requested IDs that don't exist in nat$ids)
  ext <- append_isolates(n_sel, cc$comp_id, cc$N_comps, ids_isolate)
  
  # Build Stan component structures
  cm <- comp_index_from_comp_id(ext$comp_id_all)
  
  # Build a NAMED lookup: requested ID -> local node index
  # present IDs map to 1..n_sel in the order of sub$ids_local; isolates follow after
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
    id_to_node = id_to_node,   # named integer vector: ID -> local node
    stan_graph = list(
      N_nodes      = as.integer(ext$N_nodes_total),
      N_edges      = as.integer(length(node1)),
      node1        = if (length(node1)) as.integer(node1) else integer(0),
      node2        = if (length(node2)) as.integer(node2) else integer(0),
      N_comps      = as.integer(cm$N_comps),
      comp_sizes   = as.integer(cm$comp_sizes),
      comp_index   = cm$comp_index
    ),
    ids_local = c(sub$ids_local, ids_isolate)  # local node order, length = N_nodes
  )
}

# ---------- ZCTA wrapper that keeps ZIP↔ZCTA crosswalk semantics ----------

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
  # 1) ZIP→ZCTA targets & isolates (unknown ZIPs ⇒ isolates)
  tgt <- derive_targets_zcta(zip_vec, xwalk, zip_col, zcta_col, zip_join_type_col, isolate_when_join_is_na)
  
  # 2) Ensure non-isolate ZCTAs actually exist in the national set; otherwise treat their ZIPs as isolates
  missing_zcta <- setdiff(tgt$zctas_keep, nat$ids)
  if (length(missing_zcta)) {
    move <- unique(tgt$need_df$ZIP[tgt$need_df$ZCTA %in% missing_zcta & !tgt$need_df$.is_isolate])
    if (length(move)) {
      tgt$zips_isolate <- sort(unique(c(tgt$zips_isolate, move)))
      tgt$zctas_keep   <- setdiff(tgt$zctas_keep, missing_zcta)
      tgt$need_df$.is_isolate[tgt$need_df$ZIP %in% move] <- TRUE
    }
  }
  
  # 3) Subset static edges to the ZCTAs we actually need
  sub <- subset_edges_for_ids(tgt$zctas_keep, nat)
  n_sel  <- length(sub$ids_local)
  node1  <- sub$node1
  node2  <- sub$node2
  
  # 4) Components on ZCTA block
  cc <- components_from_edges(n_sel, node1, node2)
  
  # 5) Append ZIP isolates as singleton components
  ext <- append_isolates(n_sel, cc$comp_id, cc$N_comps, tgt$zips_isolate)
  
  # 6) Components -> (sizes, index)
  cm <- comp_index_from_comp_id(ext$comp_id_all)
  
  # 7) Build named ZIP -> node lookup (for J_zip)
  #    ZCTAs map using sub$ids_local, ZIP isolates are appended after
  zcta_to_index <- setNames(seq_len(n_sel), sub$ids_local)
  iso_to_index  <- if (length(tgt$zips_isolate))
    setNames(n_sel + seq_along(tgt$zips_isolate), tgt$zips_isolate)
  else setNames(integer(0), character(0))
  
  # one value per UNIQUE ZIP, named by ZIP
  uniq_zips <- unique(pad5(zip_vec))
  zip_to_node <- setNames(rep.int(NA_integer_, length(uniq_zips)), uniq_zips)
  for (z in uniq_zips) {
    if (z %in% names(iso_to_index)) {
      zip_to_node[[z]] <- iso_to_index[[z]]
    } else {
      rows <- which(tgt$need_df$ZIP == z & !tgt$need_df$.is_isolate)
      if (length(rows)) {
        zcands <- sort(unique(tgt$need_df$ZCTA[rows]))
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
    zip_to_node = zip_to_node,  # named integer vector (ZIP -> local node)
    stan_graph  = list(
      N_nodes      = as.integer(ext$N_nodes_total),
      N_edges      = as.integer(length(node1)),
      node1        = if (length(node1)) as.integer(node1) else integer(0),
      node2        = if (length(node2)) as.integer(node2) else integer(0),
      N_comps      = as.integer(cm$N_comps),
      comp_sizes   = as.integer(cm$comp_sizes),
      comp_index   = cm$comp_index
    ),
    ids_local = c(sub$ids_local, tgt$zips_isolate)  # local node order (ZCTAs then ZIP isolates)
  )
}
