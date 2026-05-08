# 1 ------------------------------------------------------------------------
# 04_scratch_lga_predicate_debug.R
# Purpose: Debug point/LGA spatial predicates for random_100 only

library(arcgisbinding)
library(sf)
library(dplyr)
library(janitor)
library(here)

arc.check_product()

source(here::here("dev", "01_set_project_paths.R"))


# 2 ------------------------------------------------------------------------
# Helper: read ArcGIS feature class as sf

read_arcgis_fc <- function(fc_path) {
  arc_obj <- arc.open(fc_path)

  arc_df <- arc.select(
    arc_obj,
    fields = "*"
  )

  arc.data2sf(arc_df) |>
    janitor::clean_names()
}


# 3 ------------------------------------------------------------------------
# Read data

lga <- read_arcgis_fc(fc_paths$lga_aust_vic_2025)
pts <- read_arcgis_fc(fc_paths$random_100)


# 4 ------------------------------------------------------------------------
# Set geometry and CRS

sf::st_geometry(lga) <- "geom"
sf::st_geometry(pts) <- "geom"

lga <- sf::st_transform(lga, 7899)
pts <- sf::st_transform(pts, 7899)

pts <- pts |>
  mutate(point_id = row_number())


# 5 ------------------------------------------------------------------------
# Check validity before repair

validity_before <- lga |>
  st_drop_geometry() |>
  mutate(is_valid = sf::st_is_valid(lga)) |>
  count(is_valid)

print(validity_before)


# 6 ------------------------------------------------------------------------
# Repair LGA geometry

lga_repaired <- lga |>
  mutate(geom = sf::st_make_valid(geom))

sf::st_geometry(lga_repaired) <- "geom"

validity_after <- lga_repaired |>
  st_drop_geometry() |>
  mutate(is_valid = sf::st_is_valid(lga_repaired)) |>
  count(is_valid)

print(validity_after)


# 7 ------------------------------------------------------------------------
# Extract East Gippsland after repair

east_gippsland_code <- "22110"

east <- lga_repaired |>
  filter(lga_code_2025 == east_gippsland_code)


# 8 ------------------------------------------------------------------------
# Direct predicate against East Gippsland

intersects_east <- lengths(sf::st_intersects(pts, east)) > 0
within_east <- lengths(sf::st_within(pts, east)) > 0
covered_by_east <- lengths(sf::st_covered_by(pts, east)) > 0

predicate_check <- tibble::tibble(
  predicate = c("intersects", "within", "covered_by"),
  n = c(
    sum(intersects_east),
    sum(within_east),
    sum(covered_by_east)
  )
)

print(predicate_check)


# 9 ------------------------------------------------------------------------
# List points that directly intersect East Gippsland

east_intersect_sf <- pts |>
  filter(intersects_east)

if (nrow(east_intersect_sf) > 0) {
  coords <- sf::st_coordinates(east_intersect_sf)

  east_intersect_points <- east_intersect_sf |>
    mutate(
      x = coords[, 1],
      y = coords[, 2]
    ) |>
    st_drop_geometry() |>
    select(point_id, x, y)

  print(east_intersect_points)
} else {
  cat("\nNo points directly intersect East Gippsland.\n")
}


# 10 -----------------------------------------------------------------------
# Nearest LGA to every point using repaired polygons

nearest_idx <- sf::st_nearest_feature(pts, lga_repaired)

nearest_dist <- sf::st_distance(
  pts,
  lga_repaired[nearest_idx, ],
  by_element = TRUE
)

nearest_check <- pts |>
  st_drop_geometry() |>
  select(point_id) |>
  mutate(
    nearest_lga_code = lga_repaired$lga_code_2025[nearest_idx],
    nearest_lga_name = lga_repaired$lga_name_2025[nearest_idx],
    nearest_distance_m = as.numeric(nearest_dist),
    intersects_east = intersects_east
  )

east_nearest_points <- nearest_check |>
  filter(nearest_lga_code == east_gippsland_code) |>
  arrange(desc(nearest_distance_m))

print(east_nearest_points)


# 11 -----------------------------------------------------------------------
# Core comparison

cat("\nEast Gippsland direct intersect count:\n")
print(sum(intersects_east))

cat("\nEast Gippsland nearest-all count:\n")
print(sum(nearest_check$nearest_lga_code == east_gippsland_code))

cat("\nDone.\n")