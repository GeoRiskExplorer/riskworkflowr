# 1 ------------------------------------------------------------------------
# 05_test_geom_repair.R

library(arcgisbinding)
library(sf)
library(dplyr)
library(janitor)
library(here)

arc.check_product()

source(here::here("dev", "01_set_project_paths.R"))
source(here::here("R", "geom_repair.R"))


# 2 ------------------------------------------------------------------------
# Read ArcGIS feature class

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
# Read LGA and random_100

lga <- read_arcgis_fc(fc_paths$lga_aust_vic_2025)
pts <- read_arcgis_fc(fc_paths$random_100)

sf::st_geometry(lga) <- "geom"
sf::st_geometry(pts) <- "geom"

lga <- sf::st_transform(lga, 7899)
pts <- sf::st_transform(pts, 7899)


# 4 ------------------------------------------------------------------------
# Test before repair

east <- lga |>
  filter(lga_code_2025 == "22110")

before_count <- lengths(sf::st_intersects(pts, east)) |>
  sum()

cat("\nEast Gippsland count before repair:", before_count, "\n")


# 5 ------------------------------------------------------------------------
# Repair geometry

lga_repaired <- geom_repair(lga)

repair_summary <- attr(lga_repaired, "geom_repair_summary")
print(repair_summary)


# 6 ------------------------------------------------------------------------
# Test after repair

east_repaired <- lga_repaired |>
  filter(lga_code_2025 == "22110")

after_count <- lengths(sf::st_intersects(pts, east_repaired)) |>
  sum()

cat("\nEast Gippsland count after repair:", after_count, "\n")


# 7 ------------------------------------------------------------------------
# Simple pass/fail check

stopifnot(before_count == 0)
stopifnot(after_count == 12)

cat("\ngeom_repair() test passed.\n")