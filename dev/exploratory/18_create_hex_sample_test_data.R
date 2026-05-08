# 1 ------------------------------------------------------------------------
# 18_create_hex_sample_test_data.R
# Purpose: Create 200 sampled H3 hex cells and test points in/around them

library(arcgisbinding)
library(sf)
library(dplyr)
library(janitor)
library(here)

arc.check_product()

source(here::here("dev", "01_set_project_paths.R"))
source(here::here("R", "geom_repair.R"))

# 2 ------------------------------------------------------------------------
# Helper: read ArcGIS feature class

read_arcgis_fc <- function(fc_path) {
  arc_obj <- arc.open(fc_path)
  arc_df <- arc.select(arc_obj, fields = "*")

  arc.data2sf(arc_df) |>
    janitor::clean_names()
}

# 3 ------------------------------------------------------------------------
# Read full H3 layer

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
# Create stable hex id if needed

if (!"hex_id" %in% names(h3)) {
  h3 <- h3 |>
    mutate(hex_id = paste0("hex_", row_number()))
}

# 5 ------------------------------------------------------------------------
# Create clustered sample of 200 neighbouring hex cells

set.seed(123)

# Pick one seed hex
seed_hex <- h3 |>
  dplyr::slice_sample(n = 1)

seed_centroid <- sf::st_centroid(seed_hex)

# Calculate distance from seed to all hex centroids
h3_centroids <- sf::st_centroid(h3)

dist_to_seed <- sf::st_distance(
  h3_centroids,
  seed_centroid,
  by_element = FALSE
)

h3_sample_200 <- h3 |>
  dplyr::mutate(
    dist_to_seed_m = as.numeric(dist_to_seed[, 1])
  ) |>
  dplyr::arrange(dist_to_seed_m) |>
  dplyr::slice_head(n = 200) |>
  dplyr::mutate(
    sample_group = "h3_cluster_sample_200"
  )

# 6 ------------------------------------------------------------------------
# Create points INSIDE sampled hexes

set.seed(456)

inside_points_geom <- sf::st_sample(
  h3_sample_200,
  size = 200,
  type = "random"
)

inside_points <- sf::st_as_sf(
  tibble::tibble(
    point_id = seq_along(inside_points_geom),
    point_type = "inside_sample_hex"
  ),
  geometry = inside_points_geom,
  crs = 7899
)

# 7 ------------------------------------------------------------------------
# Create points AROUND sampled hexes

sample_union <- sf::st_union(h3_sample_200) |>
  sf::st_make_valid()

sample_buffer <- sf::st_buffer(sample_union, dist = 5000) |>
  sf::st_make_valid()

around_area <- sf::st_difference(sample_buffer, sample_union) |>
  sf::st_make_valid()

set.seed(789)

around_points_geom <- sf::st_sample(
  around_area,
  size = 50,
  type = "random"
)

around_points <- sf::st_as_sf(
  tibble::tibble(
    point_id = seq_along(around_points_geom) + nrow(inside_points),
    point_type = "around_sample_hex"
  ),
  geometry = around_points_geom,
  crs = 7899
)

# 8 ------------------------------------------------------------------------
# Combine point set

hex_test_points_250 <- bind_rows(
  inside_points,
  around_points
)

sf::st_geometry(hex_test_points_250) <- "geometry"

coords <- sf::st_coordinates(hex_test_points_250)

hex_test_points_table <- hex_test_points_250 |>
  sf::st_drop_geometry() |>
  mutate(
    x = coords[, 1],
    y = coords[, 2]
  )

# 9 ------------------------------------------------------------------------
# Write sampled hex cells to GDB

out_hex_fc <- file.path(
  gdb_path,
  "h3",
  "h3_r12_cluster_sample_200"
)

arc.write(
  path = out_hex_fc,
  data = h3_sample_200,
  overwrite = TRUE
)

# 10 -----------------------------------------------------------------------
# Write test points to GDB

out_points_fc <- file.path(
  gdb_path,
  "events",
  "hex_cluster_test_points_250"
)

arc.write(
  path = out_points_fc,
  data = hex_test_points_table,
  coords = c("x", "y"),
  shape_info = list(
    type = "Point",
    WKID = 7899
  ),
  overwrite = TRUE
)

# 11 -----------------------------------------------------------------------
# Verify outputs

h3_check <- read_arcgis_fc(out_hex_fc)
pts_check <- read_arcgis_fc(out_points_fc)

cat("\n--- QA: Hex Sample Test Data ---\n")
cat("Sample Hex Feature Class:", out_hex_fc, "\n")
cat("Sample Hex Count:", nrow(h3_check), "\n")
cat("Point Feature Class:", out_points_fc, "\n")
cat("Point Count:", nrow(pts_check), "\n")

point_type_qa <- pts_check |>
  st_drop_geometry() |>
  count(point_type)

print(point_type_qa)

stopifnot(nrow(h3_check) == 200)
stopifnot(nrow(pts_check) == 250)

cat("\nHex sample test data created and verified.\n")