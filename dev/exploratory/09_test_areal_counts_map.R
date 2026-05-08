# 1 ------------------------------------------------------------------------
# 09_test_areal_counts_map.R
# Purpose: Join assigned point counts back to LGA polygons and create choropleth

library(arcgisbinding)
library(sf)
library(dplyr)
library(ggplot2)
library(janitor)
library(readr)
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

lga <- geom_repair(lga, quiet = FALSE)

pts <- pts |>
  mutate(point_id = row_number())


# 4 ------------------------------------------------------------------------
# Assign points to LGA

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

lga_counts <- events_lga |>
  st_drop_geometry() |>
  count(
    lga_code_2025,
    lga_name_2025,
    name = "event_count"
  )


# 6 ------------------------------------------------------------------------
# Join counts back to LGA polygons

lga_event_surface <- lga |>
  left_join(
    lga_counts,
    by = c(
      "lga_code_2025",
      "lga_name_2025"
    )
  ) |>
  mutate(
    event_count = tidyr::replace_na(event_count, 0L),
    event_count_class = case_when(
      event_count == 0 ~ "0",
      event_count <= 2 ~ "1–2",
      event_count <= 5 ~ "3–5",
      event_count <= 10 ~ "6–10",
      TRUE ~ "11+"
    ),
    event_count_class = factor(
      event_count_class,
      levels = c("0", "1–2", "3–5", "6–10", "11+")
    )
  )


# 7 ------------------------------------------------------------------------
# QA

qa_areal_counts <- lga_event_surface |>
  st_drop_geometry() |>
  summarise(
    total_lgas = n(),
    lgas_with_events = sum(event_count > 0),
    lgas_without_events = sum(event_count == 0),
    total_events = sum(event_count),
    max_lga_count = max(event_count)
  )

print(qa_areal_counts)

top_lgas <- lga_event_surface |>
  st_drop_geometry() |>
  arrange(desc(event_count)) |>
  select(lga_code_2025, lga_name_2025, event_count) |>
  slice_head(n = 10)

print(top_lgas)


# 8 ------------------------------------------------------------------------
# Choropleth map - continuous count

p_count <- ggplot() +
  geom_sf(
    data = lga_event_surface,
    aes(fill = event_count),
    colour = "grey70",
    linewidth = 0.2
  ) +
  labs(
    title = "Random test event counts by LGA",
    subtitle = "Point assignment using sj_join(join_mode = 'intersect_nearest')",
    fill = "Event count"
  ) +
  theme_minimal()


# 9 ------------------------------------------------------------------------
# Choropleth map - classified count

p_class <- ggplot() +
  geom_sf(
    data = lga_event_surface,
    aes(fill = event_count_class),
    colour = "grey70",
    linewidth = 0.2
  ) +
  labs(
    title = "Random test event count classes by LGA",
    subtitle = "Counts joined back to lga_aust_vic_2025",
    fill = "Event count"
  ) +
  theme_minimal()


print(p_count)
print(p_class)


# 10 -----------------------------------------------------------------------
# Save outputs

dir.create(output_paths$maps, recursive = TRUE, showWarnings = FALSE)
dir.create(output_paths$tables, recursive = TRUE, showWarnings = FALSE)
dir.create(output_paths$qa, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(
  lga_counts,
  file.path(output_paths$tables, "lga_counts_random_100_outside_test.csv")
)

readr::write_csv(
  qa_areal_counts,
  file.path(output_paths$qa, "qa_lga_counts_random_100_outside_test.csv")
)

readr::write_csv(
  top_lgas,
  file.path(output_paths$qa, "top_lgas_random_100_outside_test.csv")
)

ggsave(
  filename = file.path(output_paths$maps, "lga_choropleth_event_count_continuous.png"),
  plot = p_count,
  width = 10,
  height = 7,
  dpi = 300
)

ggsave(
  filename = file.path(output_paths$maps, "lga_choropleth_event_count_classed.png"),
  plot = p_class,
  width = 10,
  height = 7,
  dpi = 300
)


# 11 -----------------------------------------------------------------------
# Done

cat("\nAreal count choropleth test complete.\n")
cat("Total events mapped:", qa_areal_counts$total_events, "\n")
cat("Maps saved to:", output_paths$maps, "\n")