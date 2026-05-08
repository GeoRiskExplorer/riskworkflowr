# 1 ------------------------------------------------------------------------
# 07_create_random_outside_test_points.R
# Purpose: Create random points (inside + outside) and write to GDB

library(arcgisbinding)
library(sf)
library(dplyr)
library(janitor)
library(here)

arc.check_product()

source(here::here("dev", "01_set_project_paths.R"))

# 2 ------------------------------------------------------------------------
# Helper: read ArcGIS feature class

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
# Read and repair LGA geometry

lga <- read_arcgis_fc(fc_paths$lga_aust_vic_2025)

sf::st_geometry(lga) <- "geom"
lga <- sf::st_transform(lga, 7899)

source(here::here("R", "geom_repair.R"))

lga <- geom_repair(lga, quiet = FALSE)

# Dissolve to one valid Victoria polygon
lga_union <- lga |>
  sf::st_make_valid() |>
  sf::st_union() |>
  sf::st_make_valid()

# 4 ------------------------------------------------------------------------
# Create points INSIDE Victoria

set.seed(42)

pts_inside <- sf::st_sample(
  lga_union,
  size = 80,
  type = "random"
) |>
  sf::st_as_sf() |>
  mutate(point_type = "inside")

# 5 ------------------------------------------------------------------------
# Create points OUTSIDE Victoria

outer_buffer <- sf::st_buffer(lga_union, dist = 50000) |>
  sf::st_make_valid()

outside_area <- sf::st_difference(
  outer_buffer,
  lga_union
) |>
  sf::st_make_valid()

pts_outside <- sf::st_sample(
  outside_area,
  size = 20,
  type = "random"
) |>
  sf::st_as_sf() |>
  dplyr::mutate(point_type = "outside")

# 6 ------------------------------------------------------------------------
# Combine dataset

pts_all <- bind_rows(pts_inside, pts_outside) |>
  mutate(point_id = row_number())

# Ensure geometry column is named "geom"
names(pts_all)[names(pts_all) == attr(pts_all, "sf_column")] <- "geom"
sf::st_geometry(pts_all) <- "geom"

# 7 ------------------------------------------------------------------------
# Define output path in GDB

out_fc <- file.path(
  gdb_path,
  "events",
  "random_100_outside_test"
)

# 8 ------------------------------------------------------------------------
# Write to GDB

arc.write(
  path = out_fc,
  data = pts_all
)

cat("\nCreated feature class:\n", out_fc, "\n")

# 9 ------------------------------------------------------------------------
# Quick QA

qa <- pts_all |>
  st_drop_geometry() |>
  count(point_type)

print(qa)