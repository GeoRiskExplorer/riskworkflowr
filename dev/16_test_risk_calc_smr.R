# 1 ------------------------------------------------------------------------
# 16_test_risk_calc_smr.R
# Purpose: Test SMR calculation using LGA counts + Census population

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
source(here::here("R", "risk_calc_smr.R"))
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
# Build final analysis object in one block

lga_analysis_smr <- {

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
    ) |>
    risk_calc_smr(
      observed_col = "event_count",
      denominator_col = "tot_p_p",
      expected_col = "expected_count",
      smr_col = "smr",
      global_rate_col = "global_event_rate",
      ci_method = "exact",
      conf_level = 0.95,
      smr_lower_col = "smr_lower",
      smr_upper_col = "smr_upper",
      smr_ci_flag_col = "smr_ci_flag"
    )
}

# 5 ------------------------------------------------------------------------
# QA

qa_print_areal_counts(
  lga_analysis_smr,
  count_col = "event_count"
)

df <- lga_analysis_smr |>
  sf::st_drop_geometry()

cat("\n--- QA: SMR Summary ---\n")
cat("Observed Column: event_count\n")
cat("Denominator Column: tot_p_p\n")
cat("Expected Column: expected_count\n")
cat("SMR Column: smr\n")
cat("CI Method: exact Poisson\n")
cat("Confidence Level: 0.95\n")
cat("Total Observed Events:", sum(df$event_count, na.rm = TRUE), "\n")
cat("Total Expected Events:", round(sum(df$expected_count, na.rm = TRUE), 6), "\n")
cat("Global Event Rate:", round(unique(df$global_event_rate)[1], 10), "\n")
cat("Missing SMR:", sum(is.na(df$smr)), "\n")
cat("Missing SMR CI Lower:", sum(is.na(df$smr_lower)), "\n")
cat("Missing SMR CI Upper:", sum(is.na(df$smr_upper)), "\n")
cat("Min SMR:", round(min(df$smr, na.rm = TRUE), 3), "\n")
cat("Median SMR:", round(stats::median(df$smr, na.rm = TRUE), 3), "\n")
cat("Mean SMR:", round(mean(df$smr, na.rm = TRUE), 3), "\n")
cat("Max SMR:", round(max(df$smr, na.rm = TRUE), 3), "\n")

cat("\n--- QA: SMR CI Flags ---\n")

smr_flag_counts <- df |>
  dplyr::count(smr_ci_flag)

for (i in seq_len(nrow(smr_flag_counts))) {
  cat(
    tools::toTitleCase(smr_flag_counts$smr_ci_flag[i]),
    ": ",
    smr_flag_counts$n[i],
    "\n",
    sep = ""
  )
}

cat("\n--- QA: Top 10 LGAs by SMR ---\n")

top_smr <- df |>
  dplyr::arrange(dplyr::desc(smr)) |>
  dplyr::select(
    lga_code_2025,
    lga_name_2025,
    event_count,
    tot_p_p,
    expected_count,
    smr,
    smr_lower,
    smr_upper,
    smr_ci_flag,
    event_rate_per_10000
  ) |>
  dplyr::slice_head(n = 10)

print(top_smr)

# 6 ------------------------------------------------------------------------
# Validation

stopifnot(sum(df$event_count, na.rm = TRUE) == 100)
stopifnot(round(sum(df$expected_count, na.rm = TRUE), 6) == 100)
stopifnot("smr" %in% names(df))
stopifnot("smr_lower" %in% names(df))
stopifnot("smr_upper" %in% names(df))
stopifnot("smr_ci_flag" %in% names(df))
stopifnot(all(df$smr >= 0, na.rm = TRUE))
stopifnot(all(df$smr_lower <= df$smr_upper, na.rm = TRUE))

cat("\nrisk_calc_smr() test passed.\n")