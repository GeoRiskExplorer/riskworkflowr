# 1 ------------------------------------------------------------------------
# 13_test_join_census_lga.R
# Purpose: Join ABS 2021 Census LGA population fields to LGA polygons

library(arcgisbinding)
library(sf)
library(dplyr)
library(janitor)
library(readr)
library(stringr)
library(here)

arc.check_product()

source(here::here("dev", "01_set_project_paths.R"))
source(here::here("R", "geom_repair.R"))

# 2 ------------------------------------------------------------------------
# Paths

census_path <- "E:/ABS_Geography/2021_GCP_all_for_VIC_short-header/2021 Census GCP All Geographies for VIC/LGA/VIC/2021Census_G01_VIC_LGA.csv"

# 3 ------------------------------------------------------------------------
# Helper: read ArcGIS feature class

read_arcgis_fc <- function(fc_path) {
  arc_obj <- arc.open(fc_path)
  arc_df <- arc.select(arc_obj, fields = "*")

  arc.data2sf(arc_df) |>
    janitor::clean_names()
}

# 4 ------------------------------------------------------------------------
# Read LGA polygons

lga <- read_arcgis_fc(fc_paths$lga_aust_vic_2025)

sf::st_geometry(lga) <- "geom"

lga <- lga |>
  sf::st_transform(7899) |>
  geom_repair(quiet = TRUE) |>
  mutate(
    lga_code_2025 = as.character(lga_code_2025)
  )

# 5 ------------------------------------------------------------------------
# Read and prepare Census table

census_lga <- readr::read_csv(
  census_path,
  show_col_types = FALSE
) |>
  janitor::clean_names() |>
  dplyr::select(
    lga_code_2021,
    tot_p_m,
    tot_p_f,
    tot_p_p
  ) |>
  dplyr::mutate(
    lga_code_2021 = stringr::str_remove(
      as.character(lga_code_2021),
      "^LGA"
    ),
    dplyr::across(
      c(tot_p_m, tot_p_f, tot_p_p),
      as.numeric
    )
  )


# 5b -----------------------------------------------------------------------
# Census-to-current LGA code crosswalk
# Moreland 2021 Census code 25250 maps to current Merri-bek 24700

lga_code_crosswalk <- tibble::tribble(
  ~lga_code_2021, ~lga_code_2025,
  "25250",        "24700"
)

census_lga_join <- census_lga |>
  dplyr::left_join(
    lga_code_crosswalk,
    by = "lga_code_2021"
  ) |>
  dplyr::mutate(
    lga_code_join = dplyr::coalesce(
      lga_code_2025,
      lga_code_2021
    )
  ) |>
  dplyr::select(
    lga_code_join,
    tot_p_m,
    tot_p_f,
    tot_p_p
  )


# 6 ------------------------------------------------------------------------
# Join Census table to LGA polygons

lga_census <- lga |>
  dplyr::left_join(
    census_lga_join,
    by = c("lga_code_2025" = "lga_code_join")
  )

# 7 ------------------------------------------------------------------------
# QA: join coverage

qa_join <- lga_census |>
  st_drop_geometry() |>
  summarise(
    total_lgas = n(),
    matched_population = sum(!is.na(tot_p_p)),
    missing_population = sum(is.na(tot_p_p))
  )

cat("\n--- QA: Census Join Coverage ---\n")
cat("Total LGAs:", qa_join$total_lgas, "\n")
cat("Matched Population Rows:", qa_join$matched_population, "\n")
cat("Missing Population Rows:", qa_join$missing_population, "\n")

# 8 ------------------------------------------------------------------------
# QA: population summary

qa_population <- lga_census |>
  st_drop_geometry() |>
  summarise(
    total_population = sum(tot_p_p, na.rm = TRUE),
    total_male = sum(tot_p_m, na.rm = TRUE),
    total_female = sum(tot_p_f, na.rm = TRUE),
    min_population = min(tot_p_p, na.rm = TRUE),
    median_population = median(tot_p_p, na.rm = TRUE),
    mean_population = round(mean(tot_p_p, na.rm = TRUE), 1),
    max_population = max(tot_p_p, na.rm = TRUE)
  )

cat("\n--- QA: Census Population Summary ---\n")
cat("Total Population:", qa_population$total_population, "\n")
cat("Total Male:", qa_population$total_male, "\n")
cat("Total Female:", qa_population$total_female, "\n")
cat("Min Population:", qa_population$min_population, "\n")
cat("Median Population:", qa_population$median_population, "\n")
cat("Mean Population:", qa_population$mean_population, "\n")
cat("Max Population:", qa_population$max_population, "\n")

# 9 ------------------------------------------------------------------------
# QA: missing records

missing_census <- lga_census |>
  st_drop_geometry() |>
  filter(is.na(tot_p_p)) |>
  select(
    lga_code_2025,
    lga_name_2025
  )

cat("\n--- QA: Missing Census Matches ---\n")

if (nrow(missing_census) == 0) {
  cat("Missing Matches: 0\n")
} else {
  print(missing_census)
}

# 10 -----------------------------------------------------------------------
# Save outputs

dir.create(output_paths$tables, recursive = TRUE, showWarnings = FALSE)
dir.create(output_paths$qa, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(
  census_lga,
  file.path(output_paths$tables, "census_2021_lga_population_clean.csv")
)

readr::write_csv(
  lga_census |>
    st_drop_geometry(),
  file.path(output_paths$tables, "lga_2025_with_census_2021_population.csv")
)

readr::write_csv(
  missing_census,
  file.path(output_paths$qa, "qa_lga_census_missing_matches.csv")
)

cat("\nCensus LGA join test complete.\n")

# 11 -----------------------------------------------------------------------
# Build unified LGA analysis object
# Requires:
# - lga_census = LGA polygons + census fields
# - lga_counts = event counts from risk_count_units()

lga_counts_sf <- risk_build_counts(
  points = pts,
  units = lga_census,
  unit_id_col = "lga_code_2025",
  unit_name_col = "lga_name_2025",
  join_mode = "intersect_nearest",
  count_col = "event_count",
  missing_count_value = 0,
  repair_units = TRUE,
  return = "units"
)

lga_analysis <- lga_counts_sf |>
  dplyr::select(
    objectid,
    lga_code_2025,
    lga_name_2025,
    event_count,
    tot_p_m,
    tot_p_f,
    tot_p_p,
    geom
  )

qs2::qs_save(
  lga_analysis,
  here::here("outputs", "tables", "lga_analysis_population.qs2")
)