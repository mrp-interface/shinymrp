library(sf)
library(tigris)

save_sf <- function(year = 2020) {
  # 1) Get ZCTAs (cartographic boundaries are lighter & fine for adjacency)
  zcta_sf <- tigris::zctas(cb = TRUE, year = year)
  zcta_sf <- sf::st_as_sf(zcta_sf)

  # 2) Ensure a stable "ZCTA" id column (covers 2010/2020/2023 variants)
  zcol <- intersect(names(zcta_sf), c("GEOID10","GEOID20","GEOID","ZCTA"))
  stopifnot(length(zcol) >= 1)
  zcta_sf$ZCTA <- as.character(zcta_sf[[zcol[1]]])

  # (optional) Keep just what you need; avoids bloating files
  zcta_sf <- zcta_sf[, c("ZCTA", "geometry")]

  # (optional) Geometry hygiene now so you don’t need to re-run later
  zcta_sf <- sf::st_make_valid(zcta_sf)

  # 3) Save to disk
  sf::write_sf(zcta_sf, "dev/spatial_prior/zcta_2020_cb.gpkg", layer = "zcta_2020_cb")
}

read_sf_normalized <- function(..., geom_name = "geometry") {
  x <- sf::read_sf(...)
  old <- attr(x, "sf_column")
  if (old != geom_name) {
    names(x)[names(x) == old] <- geom_name
    attr(x, "sf_column") <- geom_name
  }
  x
}

