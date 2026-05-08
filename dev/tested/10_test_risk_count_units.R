# 1 ------------------------------------------------------------------------
# 10_test_risk_count_units.R
# Purpose: Test risk_count_units() after sj_join()

library(arcgisbinding)
library(sf)
library(dplyr)
library(janitor)
library(here)

arc.check_product()

source(here::here("dev", "01_set_project_paths.R"))
source(here::here("R", "geom_repair.R"))
source(here::here("R", "sj_join.R"))
source(here::here("R", "risk_count_units.R"))


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
# Read data

lga <- read_arcgis_fc(fc_paths$lga_aust_vic_2025)

pts <- read_arcgis_fc(
  file.path(gdb_path, "events", "random_100_outside_test")
)

sf::st_geometry(lga) <- "geom"
sf::st_geometry(pts) <- "geom"

lga <- sf::st_transform(lga, 7899)
pts <- sf::st_transform(pts, 7899)

pts <- pts |>
  dplyr::mutate(point_id = dplyr::row_number())


# 4 ------------------------------------------------------------------------
# Spatially assign points to LGA

events_lga <- sj_join(
  points = pts,
  polygons = lga,
  id_col = "lga_code_2025",
  name_col = "lga_name_2025",
  join_mode = "intersect_nearest",
  repair_polygons = TRUE
)


# 5 ------------------------------------------------------------------------
# Count events by LGA

lga_counts <- risk_count_units(
  data = events_lga,
  unit_id_col = "lga_code_2025",
  unit_name_col = "lga_name_2025",
  count_col = "event_count"
)

print(lga_counts)


# 6 ------------------------------------------------------------------------
# Validation

total_count <- sum(lga_counts$event_count)

n_lgas_with_events <- nrow(lga_counts)

cat("\nTotal counted events:", total_count, "\n")
cat("LGAs with events:", n_lgas_with_events, "\n")

stopifnot(total_count == 100)
stopifnot(n_lgas_with_events > 0)

cat("\nrisk_count_units() test passed.\n")