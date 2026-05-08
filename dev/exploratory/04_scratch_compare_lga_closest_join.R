# 1 ------------------------------------------------------------------------
# 04_scratch_compare_lga_closest_join.R
# Purpose: Diagnose LGA closest/intersect assignment mismatch vs ArcGIS Pro
# Focus: random_100 only

library(arcgisbinding)
library(sf)
library(dplyr)
library(janitor)
library(ggplot2)
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
# Read only what we need

lga <- read_arcgis_fc(fc_paths$lga_aust_vic_2025)

pts <- read_arcgis_fc(fc_paths$random_100)


# 4 ------------------------------------------------------------------------
# Confirm geometry + CRS

cat("\nLGA geometry column:", attr(lga, "sf_column"), "\n")
cat("Points geometry column:", attr(pts, "sf_column"), "\n")

print(sf::st_crs(lga))
print(sf::st_crs(pts))

# Do not transform if already same projected CRS
# But force CRS object consistency if needed
lga <- sf::st_transform(lga, 7899)
pts <- sf::st_transform(pts, 7899)


# 5 ------------------------------------------------------------------------
# Prepare data

lga_lu <- lga |>
  select(
    lga_code = lga_code_2025,
    lga_name = lga_name_2025,
    geom
  )

pts <- pts |>
  mutate(
    point_id = row_number()
  )


# 6 ------------------------------------------------------------------------
# METHOD A: strict intersect

join_intersect <- sf::st_join(
  pts,
  lga_lu,
  join = sf::st_intersects,
  left = TRUE
) |>
  mutate(join_method = "intersect")


counts_intersect <- join_intersect |>
  st_drop_geometry() |>
  count(lga_code, lga_name, name = "n_intersect") |>
  arrange(desc(n_intersect))

print(counts_intersect)


# 7 ------------------------------------------------------------------------
# METHOD B: closest only for every point
# This is closer to ArcGIS Pro CLOSEST behaviour

nearest_idx <- sf::st_nearest_feature(
  pts,
  lga_lu
)

nearest_lga <- lga_lu[nearest_idx, ]

nearest_dist <- sf::st_distance(
  pts,
  nearest_lga,
  by_element = TRUE
)

join_nearest_all <- pts |>
  mutate(
    lga_code = nearest_lga$lga_code,
    lga_name = nearest_lga$lga_name,
    join_method = "nearest_all",
    join_distance_m = as.numeric(nearest_dist)
  )

counts_nearest_all <- join_nearest_all |>
  st_drop_geometry() |>
  count(lga_code, lga_name, name = "n_nearest_all") |>
  arrange(desc(n_nearest_all))

print(counts_nearest_all)


# 8 ------------------------------------------------------------------------
# METHOD C: intersect first, nearest only for unmatched

join_intersect_first <- join_intersect |>
  mutate(
    join_distance_m = 0
  )

unmatched_idx <- which(is.na(join_intersect_first$lga_code))

if (length(unmatched_idx) > 0) {

  nearest_idx_unmatched <- sf::st_nearest_feature(
    join_intersect_first[unmatched_idx, ],
    lga_lu
  )

  nearest_lga_unmatched <- lga_lu[nearest_idx_unmatched, ]

  nearest_dist_unmatched <- sf::st_distance(
    join_intersect_first[unmatched_idx, ],
    nearest_lga_unmatched,
    by_element = TRUE
  )

  join_intersect_first$lga_code[unmatched_idx] <- nearest_lga_unmatched$lga_code
  join_intersect_first$lga_name[unmatched_idx] <- nearest_lga_unmatched$lga_name
  join_intersect_first$join_method[unmatched_idx] <- "nearest_fallback"
  join_intersect_first$join_distance_m[unmatched_idx] <- as.numeric(nearest_dist_unmatched)
}

counts_intersect_nearest <- join_intersect_first |>
  st_drop_geometry() |>
  count(lga_code, lga_name, name = "n_intersect_nearest") |>
  arrange(desc(n_intersect_nearest))

print(counts_intersect_nearest)


# 9 ------------------------------------------------------------------------
# Compare East Gippsland specifically

east_gippsland_code <- "22110"

get_lga_count <- function(count_table, code_col, count_col, target_code) {
  out <- count_table |>
    dplyr::filter(.data[[code_col]] == target_code) |>
    dplyr::pull(.data[[count_col]])

  if (length(out) == 0) {
    return(0L)
  }

  as.integer(out[1])
}

east_compare <- tibble::tibble(
  method = c(
    "strict_intersect",
    "nearest_all",
    "intersect_then_nearest"
  ),
  east_gippsland_count = c(
    get_lga_count(counts_intersect, "lga_code", "n_intersect", east_gippsland_code),
    get_lga_count(counts_nearest_all, "lga_code", "n_nearest_all", east_gippsland_code),
    get_lga_count(counts_intersect_nearest, "lga_code", "n_intersect_nearest", east_gippsland_code)
  )
)

print(east_compare)


# 10 -----------------------------------------------------------------------
# Point-level comparison between methods

point_compare <- pts |>
  st_drop_geometry() |>
  select(point_id) |>
  left_join(
    join_intersect |>
      st_drop_geometry() |>
      select(point_id, intersect_code = lga_code, intersect_name = lga_name),
    by = "point_id"
  ) |>
  left_join(
    join_nearest_all |>
      st_drop_geometry() |>
      select(
        point_id,
        nearest_code = lga_code,
        nearest_name = lga_name,
        nearest_distance_m = join_distance_m
      ),
    by = "point_id"
  ) |>
  mutate(
    differs = intersect_code != nearest_code |
      is.na(intersect_code) != is.na(nearest_code)
  )

print(point_compare |> filter(differs))


# 11 -----------------------------------------------------------------------
# East Gippsland point list

east_sf <- join_intersect_first |>
  filter(lga_code == east_gippsland_code)

coords <- sf::st_coordinates(east_sf)

east_points <- east_sf |>
  mutate(
    x = coords[, 1],
    y = coords[, 2]
  ) |>
  st_drop_geometry() |>
  select(
    point_id,
    lga_code,
    lga_name,
    join_method,
    join_distance_m,
    x,
    y
  ) |>
  arrange(desc(join_distance_m))

print(east_points)


# 12 -----------------------------------------------------------------------
# Map diagnostic

east_lga <- lga_lu |>
  filter(lga_code == east_gippsland_code)

p <- ggplot() +
  geom_sf(data = lga_lu, fill = "grey95", colour = "grey75", linewidth = 0.2) +
  geom_sf(data = east_lga, fill = "lightyellow", colour = "black", linewidth = 0.5) +
  geom_sf(
    data = join_intersect_first |> filter(lga_code == east_gippsland_code),
    aes(shape = join_method),
    size = 2
  ) +
  labs(
    title = "East Gippsland LGA assignment diagnostic",
    subtitle = "random_100 only: intersect vs nearest fallback",
    shape = "Join method"
  ) +
  theme_minimal()

print(p)


# 13 -----------------------------------------------------------------------
# Save diagnostic outputs

readr::write_csv(
  point_compare,
  here::here("outputs", "qa", "scratch_random_100_point_method_compare.csv")
)

readr::write_csv(
  east_points,
  here::here("outputs", "qa", "scratch_random_100_east_gippsland_points.csv")
)

ggsave(
  here::here("outputs", "maps", "scratch_random_100_east_gippsland_diagnostic.png"),
  p,
  width = 9,
  height = 6,
  dpi = 300
)