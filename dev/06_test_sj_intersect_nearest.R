# 1 ------------------------------------------------------------------------
# 06_test_sj_intersect_nearest.R
# Purpose: Test sj_intersect_nearest() against LGA + random_100
# Goal: Confirm correct counts (East Gippsland = 12) and 100% assignment

library(arcgisbinding)
library(sf)
library(dplyr)
library(janitor)
library(here)
library(ggplot2)

arc.check_product()

source(here::here("dev", "01_set_project_paths.R"))
source(here::here("R", "geom_repair.R"))
source(here::here("R", "sj_intersect_nearest.R"))


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
pts <- read_arcgis_fc(fc_paths$random_100)

sf::st_geometry(lga) <- "geom"
sf::st_geometry(pts) <- "geom"

lga <- sf::st_transform(lga, 7899)
pts <- sf::st_transform(pts, 7899)

pts <- pts |>
  mutate(point_id = row_number())


# 4 ------------------------------------------------------------------------
# Run function

events_lga <- sj_intersect_nearest(
  points = pts,
  polygons = lga,
  id_col = "lga_code_2025",
  name_col = "lga_name_2025",
  repair_polygons = TRUE
)


# 5 ------------------------------------------------------------------------
# Basic QA

qa_summary <- events_lga |>
  st_drop_geometry() |>
  summarise(
    total_points = n(),
    assigned = sum(!is.na(lga_code_2025)),
    unassigned = sum(is.na(lga_code_2025)),
    intersect_count = sum(join_method == "intersect"),
    nearest_count = sum(join_method == "nearest")
  )

print(qa_summary)


# 6 ------------------------------------------------------------------------
# LGA counts

lga_counts <- events_lga |>
  st_drop_geometry() |>
  count(
    lga_code_2025,
    lga_name_2025,
    name = "event_count"
  ) |>
  arrange(desc(event_count))

print(lga_counts)


# 7 ------------------------------------------------------------------------
# Check East Gippsland

east_count <- lga_counts |>
  filter(lga_code_2025 == "22110") |>
  pull(event_count)

if (length(east_count) == 0) east_count <- 0

cat("\nEast Gippsland count:", east_count, "\n")


# 8 ------------------------------------------------------------------------
# Hard validation (this should PASS)

stopifnot(qa_summary$total_points == 100)
stopifnot(qa_summary$assigned == 100)
stopifnot(east_count == 12)

cat("\nAll validation checks passed.\n")


# 9 ------------------------------------------------------------------------
# Inspect nearest assignments

nearest_points <- events_lga |>
  filter(join_method == "nearest") |>
  st_drop_geometry() |>
  select(
    point_id,
    lga_code_2025,
    lga_name_2025,
    join_distance_m
  ) |>
  arrange(desc(join_distance_m))

print(nearest_points)


# 10 -----------------------------------------------------------------------
# Optional: quick plot

p <- ggplot() +
  geom_sf(data = lga, fill = "grey95", colour = "grey70", linewidth = 0.2) +
  geom_sf(
    data = events_lga,
    aes(colour = join_method),
    size = 2,
    alpha = 0.7
  ) +
  labs(
    title = "sj_intersect_nearest() test",
    subtitle = "random_100 assignment to LGA",
    colour = "Join method"
  ) +
  theme_minimal()

print(p)