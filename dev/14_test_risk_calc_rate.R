# 1 ------------------------------------------------------------------------
# 14_test_risk_calc_rate.R
# Purpose: Rebuild LGA analysis object and calculate event rate

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
source(here::here("R", "sj_join.R"))
source(here::here("R", "risk_count_units.R"))
source(here::here("R", "risk_join_counts.R"))
source(here::here("R", "risk_build_counts.R"))
source(here::here("R", "risk_calc_rate.R"))
source(here::here("R", "utils_qa_print.R"))

# 2 ------------------------------------------------------------------------
# Helper

read_arcgis_fc <- function(fc_path) {
  arc_obj <- arc.open(fc_path)
  arc_df <- arc.select(arc_obj, fields = "*")

  arc.data2sf(arc_df) |>
    janitor::clean_names()
}

# 3 ------------------------------------------------------------------------
# Paths

census_path <- "E:/ABS_Geography/2021_GCP_all_for_VIC_short-header/2021 Census GCP All Geographies for VIC/LGA/VIC/2021Census_G01_VIC_LGA.csv"

# 4 ------------------------------------------------------------------------
# Read spatial data

lga <- read_arcgis_fc(fc_paths$lga_aust_vic_2025)

pts <- read_arcgis_fc(
  file.path(gdb_path, "events", "random_100_outside_test")
)

sf::st_geometry(lga) <- "geom"
sf::st_geometry(pts) <- "geom"

lga <- sf::st_transform(lga, 7899)
pts <- sf::st_transform(pts, 7899)

# 5 ------------------------------------------------------------------------
# Read Census table

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
    lga_code_2021 = stringr::str_remove(as.character(lga_code_2021), "^LGA"),
    dplyr::across(c(tot_p_m, tot_p_f, tot_p_p), as.numeric)
  )

lga_code_crosswalk <- tibble::tribble(
  ~lga_code_2021, ~lga_code_2025,
  "25250",        "24700"
)

census_lga_join <- census_lga |>
  dplyr::left_join(lga_code_crosswalk, by = "lga_code_2021") |>
  dplyr::mutate(
    lga_code_join = dplyr::coalesce(lga_code_2025, lga_code_2021)
  ) |>
  dplyr::select(
    lga_code_join,
    tot_p_m,
    tot_p_f,
    tot_p_p
  )

# 6 ------------------------------------------------------------------------
# Join Census to LGA

lga_census <- lga |>
  dplyr::mutate(lga_code_2025 = as.character(lga_code_2025)) |>
  dplyr::left_join(
    census_lga_join,
    by = c("lga_code_2025" = "lga_code_join")
  )

# 7 ------------------------------------------------------------------------
# Build counted LGA object

lga_analysis <- risk_build_counts(
  points = pts,
  units = lga_census,
  unit_id_col = "lga_code_2025",
  unit_name_col = "lga_name_2025",
  join_mode = "intersect_nearest",
  count_col = "event_count",
  missing_count_value = 0,
  repair_units = TRUE,
  return = "units"
) |>
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

# 8 ------------------------------------------------------------------------
# Calculate rate

lga_analysis_rate <- risk_calc_rate(
  data = lga_analysis,
  count_col = "event_count",
  denominator_col = "tot_p_p",
  rate_col = "event_rate_per_10000",
  multiplier = 10000
)

# 9 ------------------------------------------------------------------------
# QA

qa_print_areal_counts(lga_analysis_rate, count_col = "event_count")

rate_df <- lga_analysis_rate |>
  sf::st_drop_geometry()

cat("\n--- QA: Rate Summary ---\n")
cat("Rate Column: event_rate_per_10000\n")
cat("Multiplier: 10000\n")
cat("Missing Rates:", sum(is.na(rate_df$event_rate_per_10000)), "\n")
cat("Min Rate:", round(min(rate_df$event_rate_per_10000, na.rm = TRUE), 3), "\n")
cat("Median Rate:", round(median(rate_df$event_rate_per_10000, na.rm = TRUE), 3), "\n")
cat("Mean Rate:", round(mean(rate_df$event_rate_per_10000, na.rm = TRUE), 3), "\n")
cat("Max Rate:", round(max(rate_df$event_rate_per_10000, na.rm = TRUE), 3), "\n")

cat("\n--- QA: Top 10 LGAs by Rate ---\n")

top_rates <- rate_df |>
  dplyr::arrange(dplyr::desc(event_rate_per_10000)) |>
  dplyr::select(
    lga_code_2025,
    lga_name_2025,
    event_count,
    tot_p_p,
    event_rate_per_10000
  ) |>
  dplyr::slice_head(n = 10)

print(top_rates)

# 10 -----------------------------------------------------------------------
# Validation

stopifnot(sum(lga_analysis_rate$event_count, na.rm = TRUE) == 100)
stopifnot("event_rate_per_10000" %in% names(lga_analysis_rate))

cat("\nrisk_calc_rate() test passed.\n")