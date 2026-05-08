# 1 ------------------------------------------------------------------------
# 11_test_risk_join_counts.R

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
source(here::here("R", "risk_join_counts.R"))

read_arcgis_fc <- function(fc_path) {
  arc_obj <- arc.open(fc_path)
  arc_df <- arc.select(arc_obj, fields = "*")
  arc.data2sf(arc_df) |>
    janitor::clean_names()
}

lga <- read_arcgis_fc(fc_paths$lga_aust_vic_2025)

pts <- read_arcgis_fc(
  file.path(gdb_path, "events", "random_100_outside_test")
)

sf::st_geometry(lga) <- "geom"
sf::st_geometry(pts) <- "geom"

lga <- sf::st_transform(lga, 7899)
pts <- sf::st_transform(pts, 7899)

events_lga <- sj_join(
  points = pts,
  polygons = lga,
  id_col = "lga_code_2025",
  name_col = "lga_name_2025",
  join_mode = "intersect_nearest",
  repair_polygons = TRUE
)

lga_counts <- risk_count_units(
  data = events_lga,
  unit_id_col = "lga_code_2025",
  unit_name_col = "lga_name_2025",
  count_col = "event_count"
)

lga_surface <- risk_join_counts(
  units = geom_repair(lga, quiet = TRUE),
  counts = lga_counts,
  unit_id_col = "lga_code_2025",
  unit_name_col = "lga_name_2025",
  count_col = "event_count",
  missing_count_value = 0
)

qa <- lga_surface |>
  st_drop_geometry() |>
  summarise(
    total_units = n(),
    units_with_events = sum(event_count > 0),
    units_without_events = sum(event_count == 0),
    total_events = sum(event_count),
    missing_event_count = sum(is.na(event_count))
  )

print(qa)

stopifnot(qa$total_units == 80)
stopifnot(qa$total_events == 100)
stopifnot(qa$missing_event_count == 0)

cat("\nrisk_join_counts() test passed.\n")