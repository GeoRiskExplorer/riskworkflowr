# 1 ------------------------------------------------------------------------
# 18_create_hex_test_data.R
# Purpose: Create 200 random test points inside H3 hexes and write to GDB

library(arcgisbinding)
library(sf)
library(dplyr)
library(janitor)
library(here)

arc.check_product()

source(here::here("dev", "01_set_project_paths.R"))
source(here::here("R", "geom_repair.R"))

# 2 ------------------------------------------------------------------------
# Helper

read_arcgis_fc <- function(fc_path) {
  arc_obj <- arc.open(fc_path)
  arc_df <- arc.select(arc_obj, fields = "*")

  arc.data2sf(arc_df) |>
    janitor::clean_names()
}

# 3 ------------------------------------------------------------------------
# Read H3 layer

h3 <- read_arcgis_fc(
  file.path(gdb_path, "h3", "h3_r12")
)

sf::st_geometry(h3) <- "geom"

h3 <- h3 |>
  sf::st_transform(7899) |>
  geom_repair(quiet = TRUE)

cat("\nH3 fields:\n")
print(names(h3))

# 4 ------------------------------------------------------------------------
# Sample 200 points inside H3 polygons

set.seed(123)

h3_union <- sf::st_union(h3) |>
  sf::st_make_valid()

sample_points <- sf::st_sample(
  h3_union,
  size = 200,
  type = "random"
)

pts_sf <- sf::st_as_sf(
  tibble::tibble(point_id = seq_along(sample_points)),
  geometry = sample_points,
  crs = 7899
)

coords <- sf::st_coordinates(pts_sf)

pts_table <- pts_sf |>
  sf::st_drop_geometry() |>
  dplyr::mutate(
    x = coords[, 1],
    y = coords[, 2],
    point_type = "hex_test"
  )

# 5 ------------------------------------------------------------------------
# Write point feature class to GDB

out_fc <- file.path(
  gdb_path,
  "events",
  "hex_test_points_200"
)

arc.write(
  path = out_fc,
  data = pts_table,
  coords = c("x", "y"),
  shape_info = list(
    type = "Point",
    WKID = 7899
  ),
  overwrite = TRUE
)

cat("\nCreated feature class:\n", out_fc, "\n")

# 6 ------------------------------------------------------------------------
# Confirm it exists by reading it back

pts_check <- read_arcgis_fc(out_fc)

cat("\n--- QA: Hex Test Points ---\n")
cat("Feature Class:", out_fc, "\n")
cat("Point Count:", nrow(pts_check), "\n")
cat("Geometry Column:", attr(pts_check, "sf_column"), "\n")

stopifnot(nrow(pts_check) == 200)

cat("\nhex_test_points_200 created and verified.\n")