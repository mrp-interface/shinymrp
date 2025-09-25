library(sf)
library(tigris)

save_sf <- function(year = 2020, file) {
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
  sf::write_sf(zcta_sf, file, layer = "zcta_2020_cb")
}