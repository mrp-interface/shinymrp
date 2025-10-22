# zero-pad to 5 characters
..pad5 <- function(x) {
  x <- as.character(x)
  n <- nchar(x)
  x[n < 5] <- paste0(strrep("0", 5 - n[n < 5]), x[n < 5])
  x
}

# build edge list (i < j) from 'nb'
.nb_to_edges <- function(nb) {
  if (!length(nb)) return(list(node1 = integer(0), node2 = integer(0)))
  n <- length(nb)
  i_idx <- integer(0); j_idx <- integer(0)
  for (i in seq_len(n)) {
    if (length(nb[[i]])) {
      keep <- nb[[i]][nb[[i]] > i]
      if (length(keep)) {
        i_idx <- c(i_idx, rep.int(i, length(keep)))
        j_idx <- c(j_idx, keep)
      }
    }
  }
  list(node1 = i_idx, node2 = j_idx)
}

# BYM2 ICAR scaling per connected component (geometric-mean var = 1)
# Input: binary adjacency matrix W (component only). Output: single scalar inv_sqrt_scale.
.component_inv_sqrt_scale <- function(W) {
  n <- nrow(W)
  if (n == 1L) return(1.0)      # singleton: no ICAR variance to scale
  d <- rowSums(W)
  Q <- diag(d) - W              # graph Laplacian
  ev <- eigen(Q, symmetric = TRUE, only.values = FALSE)
  lam <- ev$values
  V   <- ev$vectors
  keep <- which(lam > 1e-10)    # drop zero eigenvalues (ICAR)
  if (!length(keep)) return(1.0)
  Vi2 <- V[, keep, drop = FALSE]^2
  var_i <- drop(Vi2 %*% (1/lam[keep]))
  gmean_var <- exp(mean(log(pmax(var_i, 1e-12))))
  1 / sqrt(gmean_var)
}

build_bym2_inputs_with_isolates <- function(
    zip_vec,
    xwalk,
    zcta_sf,
    zip_col             = "ZIP_CODE",
    zcta_col            = "ZCTA",
    zip_join_type_col   = "zip_join_type",
    queen               = FALSE,   # FALSE = rook (shared border), TRUE = queen (corner OR edge)
    crs_projected       = 5070,    # NAD83 / Conus Albers (meters)
    snap_tol_m          = 30,      # snap tiny gaps to improve rook detection
    isolate_when_join_is_na = TRUE,
    verbose             = TRUE
) {
  # normalize inputs (no dplyr)
  zip_vec <- ..pad5(zip_vec)
  if (!all(c(zip_col, zcta_col, zip_join_type_col) %in% names(xwalk))) {
    stop("xwalk must contain columns: ", paste(c(zip_col, zcta_col, zip_join_type_col), collapse=", "))
  }
  xw <- data.frame(
    ZIP  = ..pad5(xwalk[[zip_col]]),
    ZCTA = ..pad5(xwalk[[zcta_col]]),
    ZIP_JOIN = as.character(xwalk[[zip_join_type_col]]),
    stringsAsFactors = FALSE
  )
  xw <- xw[!is.na(xw$ZIP) & !is.na(xw$ZCTA), , drop = FALSE]
  xw <- unique(xw)
  xw_need <- xw[xw$ZIP %in% zip_vec, , drop = FALSE]
  
  is_match <- xw_need$ZIP_JOIN == "Zip matches ZCTA"
  if (isolate_when_join_is_na) {
    is_match[is.na(is_match)] <- FALSE
  } else {
    is_match[is.na(is_match)] <- TRUE
  }
  xw_need$.is_isolate <- !is_match
  
  # ZCTAs to include in contiguity; ZIPs to include as isolates
  zctas_keep   <- unique(xw_need$ZCTA[!xw_need$.is_isolate])
  zips_isolate <- sort(unique(xw_need$ZIP[xw_need$.is_isolate]))
  
  # ensure a ZCTA id field in polygon sf (no dplyr)
  if (!("ZCTA" %in% names(zcta_sf))) {
    geocol <- grep("^GEOID", names(zcta_sf), value = TRUE)
    if (!length(geocol)) stop("zcta_sf must have a GEOID* column or a ZCTA column.")
    zcta_sf$ZCTA <- zcta_sf[[geocol[1]]]
  }

  # geometry hygiene: valid -> projected -> snap
  zcta_sf <- st_make_valid(zcta_sf)
  zcta_sf <- st_transform(zcta_sf, crs_projected)

  zcta_sf_keep <- zcta_sf[zcta_sf$ZCTA %in% zctas_keep, ]
  zcta_sf_keep <- zcta_sf_keep[order(zcta_sf_keep$ZCTA), ]
  
  if (nrow(zcta_sf_keep) == 0 && length(zips_isolate) == 0) {
    stop("Nothing to build: no ZCTAs matched and no isolates requested.")
  }
  
  if (nrow(zcta_sf_keep) > 0 && snap_tol_m > 0) {
    uni <- st_union(zcta_sf_keep)
    zcta_sf_keep$geometry <- st_snap(zcta_sf_keep$geometry, uni, tolerance = snap_tol_m)
  }
  
  # build contiguity on ZCTAs only
  if (nrow(zcta_sf_keep) > 0) {
    nb_zcta <- poly2nb(zcta_sf_keep, queen = queen, snap = snap_tol_m)
    A_zcta  <- nb2mat(nb_zcta, style = "B", zero.policy = TRUE)
  } else {
    nb_zcta <- structure(list(), class = "nb")
    A_zcta  <- matrix(0, 0, 0)
  }
  n_zcta <- nrow(A_zcta)
  
  # BYM2 ICAR scaling from ZCTA block (per component)
  if (n_zcta > 0) {
    comp <- n.comp.nb(nb_zcta)         # list: $nc, $comp.id
    comp_id <- comp$comp.id
    inv_sqrt_scale_factor <- numeric(n_zcta)
    if (n_zcta > 0) {
      # component-wise scaling: same scalar within each component
      for (c in seq_len(max(comp_id))) {
        idx <- which(comp_id == c)
        Wc  <- A_zcta[idx, idx, drop = FALSE]
        inv_sqrt_scale_factor[idx] <- .component_inv_sqrt_scale(Wc)
      }
    }
    # edges from ZCTA block
    el <- .nb_to_edges(nb_zcta)
    node1 <- el$node1
    node2 <- el$node2
    C_max <- if (length(comp_id)) max(comp_id) else 0L
  } else {
    comp_id <- integer(0)
    inv_sqrt_scale_factor <- numeric(0)
    node1 <- integer(0); node2 <- integer(0)
    C_max <- 0L
  }
  
  # append ZIP isolates AFTER scaling
  m_iso <- length(zips_isolate)
  if (m_iso > 0) {
    inv_sqrt_scale_factor <- c(inv_sqrt_scale_factor, rep(1.0, m_iso))
    comp_id <- c(comp_id, seq.int(from = C_max + 1L, length.out = m_iso))
  }
  
  N_nodes <- n_zcta + m_iso
  N_edges <- length(node1)
  
  # labels & mapping (no tidyverse)
  node_label <- character(N_nodes)
  if (n_zcta > 0) node_label[seq_len(n_zcta)] <- paste0("ZCTA:", zcta_sf_keep$ZCTA)
  if (m_iso > 0)  node_label[n_zcta + seq_len(m_iso)] <- paste0("ZIP_ISO:", zips_isolate)
  
  zcta_to_index <- if (n_zcta > 0) setNames(seq_len(n_zcta), zcta_sf_keep$ZCTA) else setNames(integer(0), character(0))
  iso_to_index  <- if (m_iso  > 0) setNames(n_zcta + seq_len(m_iso), zips_isolate) else setNames(integer(0), character(0))
  
  zip_to_node <- integer(length(zip_vec))
  names(zip_to_node) <- zip_vec
  for (i in seq_along(zip_vec)) {
    z <- zip_vec[i]
    if (length(zips_isolate) && z %in% zips_isolate) {
      zip_to_node[i] <- iso_to_index[[z]]
    } else {
      # choose the first ZCTA present in lattice (deterministic)
      rows <- which(xw_need$ZIP == z & !xw_need$.is_isolate)
      if (length(rows)) {
        zcands <- sort(unique(xw_need$ZCTA[rows]))
        zhit <- zcands[zcands %in% names(zcta_to_index)]
        zip_to_node[i] <- if (length(zhit)) zcta_to_index[[zhit[1]]] else NA_integer_
      } else {
        zip_to_node[i] <- NA_integer_
      }
    }
  }
  
  if (verbose) {
    if (n_zcta > 0) {
      cat("ZCTA nodes:", n_zcta, "| isolates:", m_iso, "| edges:", N_edges, "\n")
      dg <- if (length(nb_zcta)) card(nb_zcta) else integer(0)
      if (length(dg)) {
        cat("ZCTA degree table:\n"); print(table(dg))
        cat("ZCTA connected components:", n.comp.nb(nb_zcta)$nc, "\n")
      }
    } else {
      cat("No ZCTA nodes kept. All included ZIPs are isolates.\n")
    }
    if (any(is.na(zip_to_node))) {
      bad <- names(zip_to_node)[is.na(zip_to_node)]
      message("ZIPs not placed (no matching ZCTA among kept polygons): ",
              paste(unique(bad), collapse = ", "))
    }
  }
  

  list(
    N_nodes = N_nodes,
    N_edges = N_edges,
    node1 = as.integer(node1),
    node2 = as.integer(node2),
    inv_sqrt_scale_factor = as.numeric(inv_sqrt_scale_factor),
    comp_id = as.integer(comp_id),                        
    node_label = node_label,            
    zcta_nodes = if (n_zcta > 0) zcta_sf_keep$ZCTA else character(0),
    iso_zip_nodes = zips_isolate,
    zip_to_node = zip_to_node,
    nb = if (n_zcta > 0) nb_zcta else structure(list(), class = "nb")
  )
}

# build Stan’s component arrays: comp_sizes, max_comp_size, comp_index (0-padded)
.make_comp_index <- function(comp_id) {
  C <- max(comp_id)
  comp_sizes <- tabulate(comp_id, nbins = C)
  max_comp_size <- max(comp_sizes)
  comp_index <- matrix(0L, nrow = C, ncol = max_comp_size)
  for (c in seq_len(C)) {
    idx <- which(comp_id == c)
    comp_index[c, seq_along(idx)] <- idx
  }
  list(C = C,
       comp_sizes = as.integer(comp_sizes),
       max_comp_size = as.integer(max_comp_size),
       comp_index = comp_index)
}

# plot connected components (for visual check)
.plot_components <- function(bym, zcta_sf) {
  # Keep only ZCTAs that appear in the lattice (no isolates here)
  zcta_keep <- zcta_sf[zcta_sf$ZCTA %in% bym$zcta_nodes, , drop = FALSE]

  # Build a key-based map from the builder outputs
  comp_map <- data.frame(
    ZCTA = bym$zcta_nodes,
    comp = bym$comp_id[seq_along(bym$zcta_nodes)],  # first |zcta_nodes| entries are ZCTAs
    stringsAsFactors = FALSE
  )

  # Join by ZCTA (don’t rely on row order)
  zcta_keep <- merge(zcta_keep, comp_map, by = "ZCTA", all.x = TRUE, sort = FALSE)

  # Optional: ensure a stable component factor for nice legend order
  zcta_keep$comp <- factor(zcta_keep$comp, levels = sort(unique(zcta_keep$comp)))

  op <- par(mar = c(0,0,0,0))
  plot(sf::st_geometry(zcta_keep),
       col = grDevices::hcl.colors(length(levels(zcta_keep$comp)), "Dark 3")[zcta_keep$comp],
       border = "white")
  par(op)
  legend("bottomleft", inset = 0.02,
         fill = grDevices::hcl.colors(length(levels(zcta_keep$comp)), "Dark 3"),
         legend = paste("comp", levels(zcta_keep$comp)),
         cex = 0.8, bty = "n")
}