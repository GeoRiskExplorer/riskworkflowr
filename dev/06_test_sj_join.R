# 1 ------------------------------------------------------------------------
# 06_test_sj_join.R
# Purpose: Test sj_join() using random_100 and LGA polygons

library(arcgisbinding)
library(sf)
library(dplyr)
library(ggplot2)
library(janitor)
library(here)

arc.check_product()

source(here::here("dev", "01_set_project_paths.R"))
source(here::here("R", "geom_repair.R"))
source(here::here("R", "sj_join.R"))


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
  mutate(point_id = row_number())


# 4 ------------------------------------------------------------------------
# Run all three join modes

join_intersect <- sj_join(
  points = pts,
  polygons = lga,
  id_col = "lga_code_2025",
  name_col = "lga_name_2025",
  join_mode = "intersect",
  repair_polygons = TRUE
)

join_nearest <- sj_join(
  points = pts,
  polygons = lga,
  id_col = "lga_code_2025",
  name_col = "lga_name_2025",
  join_mode = "nearest",
  repair_polygons = TRUE
)

join_intersect_nearest <- sj_join(
  points = pts,
  polygons = lga,
  id_col = "lga_code_2025",
  name_col = "lga_name_2025",
  join_mode = "intersect_nearest",
  repair_polygons = TRUE
)


# 5 ------------------------------------------------------------------------
# QA summary

qa_summary <- dplyr::bind_rows(
  join_intersect |>
    st_drop_geometry() |>
    summarise(
      join_mode = "intersect",
      total_points = n(),
      assigned = sum(!is.na(lga_code_2025)),
      unassigned = sum(is.na(lga_code_2025)),
      intersect_count = sum(join_method == "intersect"),
      nearest_count = sum(join_method == "nearest"),
      max_distance_m = max(join_distance_m, na.rm = TRUE)
    ),

  join_nearest |>
    st_drop_geometry() |>
    summarise(
      join_mode = "nearest",
      total_points = n(),
      assigned = sum(!is.na(lga_code_2025)),
      unassigned = sum(is.na(lga_code_2025)),
      intersect_count = sum(join_method == "intersect"),
      nearest_count = sum(join_method == "nearest"),
      max_distance_m = max(join_distance_m, na.rm = TRUE)
    ),

  join_intersect_nearest |>
    st_drop_geometry() |>
    summarise(
      join_mode = "intersect_nearest",
      total_points = n(),
      assigned = sum(!is.na(lga_code_2025)),
      unassigned = sum(is.na(lga_code_2025)),
      intersect_count = sum(join_method == "intersect"),
      nearest_count = sum(join_method == "nearest"),
      max_distance_m = max(join_distance_m, na.rm = TRUE)
    )
)

print(qa_summary)


# 6 ------------------------------------------------------------------------
# East Gippsland check

east_gippsland_code <- "22110"

east_counts <- dplyr::bind_rows(
  join_intersect |>
    st_drop_geometry() |>
    filter(lga_code_2025 == east_gippsland_code) |>
    summarise(join_mode = "intersect", east_gippsland_count = n()),

  join_nearest |>
    st_drop_geometry() |>
    filter(lga_code_2025 == east_gippsland_code) |>
    summarise(join_mode = "nearest", east_gippsland_count = n()),

  join_intersect_nearest |>
    st_drop_geometry() |>
    filter(lga_code_2025 == east_gippsland_code) |>
    summarise(join_mode = "intersect_nearest", east_gippsland_count = n())
)

print(east_counts)


# 7 ------------------------------------------------------------------------
# Hard checks for random_100_outside_test

intersect_assigned <- qa_summary |>
  filter(join_mode == "intersect") |>
  pull(assigned)

intersect_unassigned <- qa_summary |>
  filter(join_mode == "intersect") |>
  pull(unassigned)

nearest_assigned <- qa_summary |>
  filter(join_mode == "nearest") |>
  pull(assigned)

intersect_nearest_assigned <- qa_summary |>
  filter(join_mode == "intersect_nearest") |>
  pull(assigned)

intersect_nearest_fallback <- qa_summary |>
  filter(join_mode == "intersect_nearest") |>
  pull(nearest_count)

stopifnot(intersect_assigned == 80)
stopifnot(intersect_unassigned == 20)
stopifnot(nearest_assigned == 100)
stopifnot(intersect_nearest_assigned == 100)
stopifnot(intersect_nearest_fallback == 20)

cat("\nsj_join() tests passed for outside fallback dataset.\n")


# 8 ------------------------------------------------------------------------
# Optional plot

p <- ggplot() +
  geom_sf(data = geom_repair(lga, quiet = TRUE), fill = "grey95", colour = "grey70", linewidth = 0.2) +
  geom_sf(
    data = join_intersect_nearest,
    aes(colour = join_method),
    size = 2,
    alpha = 0.7
  ) +
  labs(
    title = "sj_join() test",
    subtitle = "random_100 assigned to LGA using intersect_nearest",
    colour = "Join method"
  ) +
  theme_minimal()

print(p)