# 1 ------------------------------------------------------------------------
# 03_test_lga_assignment.R
# Purpose: Test random point assignment to Victorian LGAs
# Project: riskworkflowr

library(arcgisbinding)
library(sf)
library(dplyr)
library(ggplot2)
library(janitor)
library(readr)
library(here)

arc.check_product()

source(here::here("dev", "01_set_project_paths.R"))


# 2 ------------------------------------------------------------------------
# Helper: read ArcGIS feature class as sf

read_arcgis_fc <- function(fc_path) {
  arc_obj <- arc.open(fc_path)

  arc_df <- arc.select(
    arc_obj,
    fields = "*"
  )

  arc.data2sf(arc_df) |>
    janitor::clean_names()
}
# 2b -----------------------------------------------------------------------
# Helper: intersect first, nearest fallback second

sj_intersect_else_nearest <- function(points, polygons, id_col, name_col = NULL) {

  geom_col <- attr(polygons, "sf_column")

  polygon_lookup <- polygons |>
    dplyr::select(
      dplyr::all_of(c(id_col, name_col)),
      dplyr::all_of(geom_col)
    )

  joined_intersect <- sf::st_join(
    points,
    polygon_lookup,
    join = sf::st_intersects,
    left = TRUE
  )

  id_joined_col <- id_col

  unmatched_idx <- which(is.na(joined_intersect[[id_joined_col]]))

  joined_intersect$join_method <- dplyr::if_else(
    is.na(joined_intersect[[id_joined_col]]),
    "unmatched",
    "intersect"
  )

  joined_intersect$join_distance_m <- 0

  if (length(unmatched_idx) > 0) {

    nearest_idx <- sf::st_nearest_feature(
      joined_intersect[unmatched_idx, ],
      polygon_lookup
    )

    nearest_polygons <- polygon_lookup[nearest_idx, ]

    nearest_dist <- sf::st_distance(
      joined_intersect[unmatched_idx, ],
      nearest_polygons,
      by_element = TRUE
    )

    joined_intersect[unmatched_idx, id_col] <- nearest_polygons[[id_col]]

    if (!is.null(name_col)) {
      joined_intersect[unmatched_idx, name_col] <- nearest_polygons[[name_col]]
    }

    joined_intersect$join_method[unmatched_idx] <- "nearest"
    joined_intersect$join_distance_m[unmatched_idx] <- as.numeric(nearest_dist)
  }

  joined_intersect
}

# 3 ------------------------------------------------------------------------
# Read test data

lga <- read_arcgis_fc(fc_paths$lga_aust_vic_2025)

random_100 <- read_arcgis_fc(fc_paths$random_100)

random_1000 <- read_arcgis_fc(fc_paths$random_1000)


# 4 ------------------------------------------------------------------------
# CRS validation

crs_check <- tibble::tibble(
  layer = c("lga", "random_100", "random_1000"),
  epsg = c(
    sf::st_crs(lga)$epsg,
    sf::st_crs(random_100)$epsg,
    sf::st_crs(random_1000)$epsg
  ),
  crs_name = c(
    sf::st_crs(lga)$Name,
    sf::st_crs(random_100)$Name,
    sf::st_crs(random_1000)$Name
  )
)

print(crs_check)


# 5 ------------------------------------------------------------------------
# Transform to standard CRS

lga <- sf::st_transform(lga, standard_crs$epsg)

random_100 <- sf::st_transform(random_100, standard_crs$epsg)

random_1000 <- sf::st_transform(random_1000, standard_crs$epsg)


# 6 ------------------------------------------------------------------------
# Combine random event datasets

events_all <- dplyr::bind_rows(
  random_100 |>
    dplyr::mutate(test_dataset = "random_100"),
  random_1000 |>
    dplyr::mutate(test_dataset = "random_1000")
) |>
  dplyr::mutate(event_id = dplyr::row_number())


# 7 ------------------------------------------------------------------------
# Prepare LGA lookup fields

cat("\nLGA fields available:\n")
print(names(lga))

geom_col <- attr(lga, "sf_column")

lga_lookup <- lga |>
  dplyr::select(
    lga_code = dplyr::any_of(
      c(
        "lga_code_2025",
        "lga_code",
        "lga_code21",
        "lga_code_2021"
      )
    ),
    lga_name = dplyr::any_of(
      c(
        "lga_name_2025",
        "lga_name",
        "lga_name21",
        "lga_name_2021"
      )
    ),
    dplyr::all_of(geom_col)
  )


# 8 ------------------------------------------------------------------------
# Validate that LGA fields were found

required_lga_fields <- c("lga_code", "lga_name")

missing_lga_fields <- setdiff(
  required_lga_fields,
  names(lga_lookup)
)

if (length(missing_lga_fields) > 0) {
  stop(
    "Missing required LGA fields after selection: ",
    paste(missing_lga_fields, collapse = ", "),
    "\nCheck names(lga) and update Section 7 field aliases."
  )
}


# 9 ------------------------------------------------------------------------
# Assign points to LGAs using intersect first, nearest fallback second

events_lga <- sj_intersect_else_nearest(
  points = events_all,
  polygons = lga_lookup,
  id_col = "lga_code",
  name_col = "lga_name"
)


# 10 -----------------------------------------------------------------------
# Validation summary

validation_summary <- events_lga |>
  sf::st_drop_geometry() |>
  dplyr::group_by(test_dataset) |>
  dplyr::summarise(
    total_events = dplyr::n(),
    assigned_to_lga = sum(!is.na(lga_name)),
    intersect_assigned = sum(join_method == "intersect"),
    nearest_assigned = sum(join_method == "nearest"),
    unassigned_to_lga = sum(is.na(lga_name)),
    pct_assigned = round(assigned_to_lga / total_events * 100, 1),
    max_nearest_distance_m = round(max(join_distance_m, na.rm = TRUE), 1),
    .groups = "drop"
  )

print(validation_summary)


# 11 -----------------------------------------------------------------------
# LGA event counts

lga_event_counts <- events_lga |>
  sf::st_drop_geometry() |>
  dplyr::filter(!is.na(lga_name)) |>
  dplyr::count(
    test_dataset,
    lga_code,
    lga_name,
    name = "event_count"
  ) |>
  dplyr::arrange(
    test_dataset,
    dplyr::desc(event_count)
  )

print(lga_event_counts)


# 12 -----------------------------------------------------------------------
# Unmatched events

unmatched_events <- events_lga |>
  dplyr::filter(is.na(lga_name))

unmatched_summary <- unmatched_events |>
  sf::st_drop_geometry() |>
  dplyr::count(test_dataset, name = "unmatched_count")

print(unmatched_summary)


# 13 -----------------------------------------------------------------------
# Basic map

p_lga_assignment <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = lga_lookup,
    fill = "grey95",
    colour = "grey75",
    linewidth = 0.2
  ) +
  ggplot2::geom_sf(
    data = events_lga,
    ggplot2::aes(shape = test_dataset),
    size = 1.2,
    alpha = 0.6
  ) +
  ggplot2::labs(
    title = "Random event point assignment to Victorian LGAs",
    subtitle = "Testing random_100 and random_1000 against lga_aust_vic_2025",
    shape = "Test dataset"
  ) +
  ggplot2::theme_minimal()

print(p_lga_assignment)


# 14 -----------------------------------------------------------------------
# Save outputs

dir.create(output_paths$qa, recursive = TRUE, showWarnings = FALSE)
dir.create(output_paths$tables, recursive = TRUE, showWarnings = FALSE)
dir.create(output_paths$maps, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(
  validation_summary,
  file.path(output_paths$qa, "lga_assignment_validation_summary.csv")
)

readr::write_csv(
  lga_event_counts,
  file.path(output_paths$tables, "lga_event_counts_random_tests.csv")
)

readr::write_csv(
  unmatched_summary,
  file.path(output_paths$qa, "lga_assignment_unmatched_summary.csv")
)

ggplot2::ggsave(
  filename = file.path(output_paths$maps, "lga_assignment_random_tests.png"),
  plot = p_lga_assignment,
  width = 10,
  height = 7,
  dpi = 300
)


# 15 -----------------------------------------------------------------------
# Final console message

cat("\nLGA assignment test complete.\n")
cat("QA saved to:", file.path(output_paths$qa, "lga_assignment_validation_summary.csv"), "\n")
cat("Counts saved to:", file.path(output_paths$tables, "lga_event_counts_random_tests.csv"), "\n")
cat("Unmatched summary saved to:", file.path(output_paths$qa, "lga_assignment_unmatched_summary.csv"), "\n")
cat("Map saved to:", file.path(output_paths$maps, "lga_assignment_random_tests.png"), "\n")