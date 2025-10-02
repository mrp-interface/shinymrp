# deps: tigris, sf, spdep, dplyr, stringr, qs (optional for saving)
library(tigris)
library(sf)
library(spdep)
library(dplyr)
library(stringr)

padN <- function(x, n) str_pad(as.character(x), n, pad = "0")

nb_to_edges <- function(nb) {
  if (!length(nb)) return(list(node1 = integer(), node2 = integer()))
  lens <- vapply(nb, length, integer(1))
  if (!sum(lens)) return(list(node1 = integer(), node2 = integer()))
  i_idx <- rep.int(seq_along(nb), lens)
  j_idx <- unlist(nb, use.names = FALSE)
  keep  <- j_idx > i_idx
  list(node1 = as.integer(i_idx[keep]), node2 = as.integer(j_idx[keep]))
}

# Normalize IDs and sort for deterministic node order
normalize_and_prepare <- function(sf_obj, geo, crs_projected, include_territories) {
  nm <- names(sf_obj)

  if (geo == "zcta") {
    # prefer GEOID20/GEOID10, else first GEOID*
    cand <- c("GEOID20", "GEOID10", grep("^GEOID", nm, value = TRUE))
    id_col <- cand[cand %in% nm][1]
    stopifnot(length(id_col) == 1)
    ids <- padN(sf_obj[[id_col]], 5)

  } else if (geo == "county") {
    # 5-digit county FIPS
    id_col <- "GEOID"; stopifnot(id_col %in% nm)
    if (!include_territories) {
      sf_obj <- sf_obj |> filter(!STATEFP %in% c("60","66","69","72","78"))  # AS, GU, MP, PR, VI
    }
    ids <- padN(sf_obj[[id_col]], 5)

  } else if (geo == "state") {
    # 2-digit state FIPS
    id_col <- "GEOID"; stopifnot(id_col %in% nm)
    if (!include_territories) {
      sf_obj <- sf_obj |> filter(!STATEFP %in% c("60","66","69","72","78"))
    }
    ids <- padN(sf_obj[[id_col]], 2)

  } else stop("Unknown geo: ", geo)

  sf_obj |>
    mutate(.ID_STD = ids) |>
    st_make_valid() |>
    st_transform(crs_projected) |>
    arrange(.ID_STD)
}

# ---- MAIN: create & (optionally) save edges for ONE geography ----
create_static_edges <- function(
  geo               = c("zcta","county","state"),
  year              = 2020,
  cb                = TRUE,
  queen             = TRUE,
  snap_tol_m        = 0,
  crs_projected     = 5070,
  include_territories = TRUE,
  save_qs           = NULL,     # e.g. "edges_zcta_2020.qs"
  qs_preset         = "high"
) {
  geo <- match.arg(geo)

  # fetch geometry from tigris
  sf_obj <- switch(
    geo,
    zcta   = tigris::zctas(cb = cb, year = year) |> st_as_sf(),
    county = tigris::counties(cb = cb, year = year) |> st_as_sf(),
    state  = tigris::states(cb = cb, year = year) |> st_as_sf()
  )

  # normalize IDs, transform, sort
  sf_obj <- normalize_and_prepare(sf_obj, geo, crs_projected, include_territories)
  ids    <- sf_obj$.ID_STD

  # contiguity & edges
  nb <- spdep::poly2nb(sf_obj, queen = queen, snap = snap_tol_m)
  ed <- nb_to_edges(nb)

  res <- list(
    geo   = geo,
    ids   = ids,              # node order (character)
    node1 = ed$node1,         # integer endpoints (1-based indices into ids)
    node2 = ed$node2,
    meta  = list(
      year = year, cb = cb, queen = queen, snap_tol_m = snap_tol_m,
      crs = crs_projected, include_territories = include_territories,
      n_nodes = length(ids), n_edges = length(ed$node1)
    )
  )

  # Back-compat for your Step-2 (expects nat$zcta_ids)
  if (geo == "zcta") res$zcta_ids <- res$ids

  if (!is.null(save_qs)) {
    qs::qsave(res, save_qs, preset = qs_preset)
  }
  res
}
