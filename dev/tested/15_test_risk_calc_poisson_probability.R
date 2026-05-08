# 1 ------------------------------------------------------------------------
# 15_test_risk_calc_poisson_probability.R

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
source(here::here("R", "risk_calc_poisson_probability.R"))
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
# Paths and parameters

census_path <- "E:/ABS_Geography/2021_GCP_all_for_VIC_short-header/2021 Census GCP All Geographies for VIC/LGA/VIC/2021Census_G01_VIC_LGA.csv"

analysis_period_years <- 2

# 4 ------------------------------------------------------------------------
# Build final analysis object in one block to reduce data pane clutter

lga_analysis_prob <- {

  lga <- read_arcgis_fc(fc_paths$lga_aust_vic_2025)

  pts <- read_arcgis_fc(
    file.path(gdb_path, "events", "random_100_outside_test")
  )

  sf::st_geometry(lga) <- "geom"
  sf::st_geometry(pts) <- "geom"

  lga <- sf::st_transform(lga, 7899)
  pts <- sf::st_transform(pts, 7899)

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

  lga_census <- lga |>
    dplyr::mutate(
      lga_code_2025 = as.character(lga_code_2025)
    ) |>
    dplyr::left_join(
      census_lga_join,
      by = c("lga_code_2025" = "lga_code_join")
    )

  risk_build_counts(
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
    ) |>
    risk_calc_rate(
      count_col = "event_count",
      denominator_col = "tot_p_p",
      rate_col = "event_rate_per_10000",
      multiplier = 10000
    ) |>
    risk_calc_poisson_probability(
      count_col = "event_count",
      period_value = analysis_period_years,
      lambda_col = "lambda_annual",
      probability_col = "prob_ge_1_annual",
      probability_pct_col = "prob_ge_1_annual_pct",
      output = "both"
    )
}

# 5 ------------------------------------------------------------------------
# QA

qa_print_areal_counts(
  lga_analysis_prob,
  count_col = "event_count"
)

df <- lga_analysis_prob |>
  sf::st_drop_geometry()

prob_col <- if ("prob_ge_1_annual_pct" %in% names(df)) {
  "prob_ge_1_annual_pct"
} else {
  "prob_ge_1_annual"
}

prob_label <- if (prob_col == "prob_ge_1_annual_pct") {
  "Probability %"
} else {
  "Probability Proportion"
}

prob_values <- df[[prob_col]]

cat("\n--- QA: Poisson Probability Summary ---\n")
cat("Count Column: event_count\n")
cat("Analysis Period Years:", analysis_period_years, "\n")
cat("Lambda Column: lambda_annual\n")
cat("Probability Column:", prob_col, "\n")
cat("Probability Output:", prob_label, "\n")
cat("Total Observed Events:", sum(df$event_count, na.rm = TRUE), "\n")
cat("Total Annual Lambda:", round(sum(df$lambda_annual, na.rm = TRUE), 6), "\n")
cat("Missing Probabilities:", sum(is.na(prob_values)), "\n")

if (all(is.na(prob_values))) {
  cat("All probability values are NA\n")
} else {
  cat("Min:", round(min(prob_values, na.rm = TRUE), 3), "\n")
  cat("Median:", round(stats::median(prob_values, na.rm = TRUE), 3), "\n")
  cat("Mean:", round(mean(prob_values, na.rm = TRUE), 3), "\n")
  cat("Max:", round(max(prob_values, na.rm = TRUE), 3), "\n")
}

cat("\n--- QA: Top 10 LGAs by Poisson Probability ---\n")

top_prob <- df |>
  dplyr::arrange(dplyr::desc(.data[[prob_col]])) |>
  dplyr::select(
    lga_code_2025,
    lga_name_2025,
    event_count,
    lambda_annual,
    dplyr::all_of(prob_col)
  ) |>
  dplyr::slice_head(n = 10)

print(top_prob)
print(top_prob)

# 6 ------------------------------------------------------------------------
# Validation

stopifnot(sum(df$event_count, na.rm = TRUE) == 100)
stopifnot(round(sum(df$lambda_annual, na.rm = TRUE), 6) == 50)
stopifnot(all(df$prob_ge_1_annual >= 0, na.rm = TRUE))
stopifnot(all(df$prob_ge_1_annual <= 1, na.rm = TRUE))

cat("\nrisk_calc_poisson_probability() test passed.\n")