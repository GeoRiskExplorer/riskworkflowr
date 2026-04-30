# 1 ------------------------------------------------------------------------
# 12_test_risk_build_counts.R

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
source(here::here("R", "risk_build_counts.R"))

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

result <- risk_build_counts(
  points = pts,
  units = lga,
  unit_id_col = "lga_code_2025",
  unit_name_col = "lga_name_2025",
  join_mode = "intersect_nearest",
  count_col = "event_count",
  missing_count_value = 0,
  repair_units = TRUE,
  return = "all"
)

qa_print_areal_counts(result$units)
qa_print_join_methods(result$joined_points)

cat("\nrisk_build_counts() test passed.\n")