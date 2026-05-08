# 1 ------------------------------------------------------------------------
# 17_test_map_risk_choropleth.R
# Purpose: Test classed Brewer-style choropleth maps

library(arcgisbinding)
library(sf)
library(dplyr)
library(ggplot2)
library(janitor)
library(readr)
library(stringr)
library(here)

arc.check_product()

source(here::here("dev", "01_set_project_paths.R"))
source(here::here("R", "utils.R"))
source(here::here("R", "geom_repair.R"))
source(here::here("R", "sj_join.R"))
source(here::here("R", "risk_count_units.R"))
source(here::here("R", "risk_join_counts.R"))
source(here::here("R", "risk_build_counts.R"))
source(here::here("R", "risk_calc_rate.R"))
source(here::here("R", "risk_calc_poisson_probability.R"))
source(here::here("R", "risk_calc_smr.R"))
source(here::here("R", "map_risk_choropleth.R"))

# 2 ------------------------------------------------------------------------
# Helper

read_arcgis_fc <- function(fc_path) {
  arc_obj <- arc.open(fc_path)
  arc_df <- arc.select(arc_obj, fields = "*")

  arc.data2sf(arc_df) |>
    janitor::clean_names()
}

# 3 ------------------------------------------------------------------------
# Parameters

census_path <- "E:/ABS_Geography/2021_GCP_all_for_VIC_short-header/2021 Census GCP All Geographies for VIC/LGA/VIC/2021Census_G01_VIC_LGA.csv"

analysis_period_years <- 2

# 4 ------------------------------------------------------------------------
# Build analysis object

lga_analysis_map <- {

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

cat("\n--- QA: Map Data ---\n")
cat("Total Units:", nrow(lga_analysis_map), "\n")
cat("Total Events:", sum(lga_analysis_map$event_count, na.rm = TRUE), "\n")
cat("Geometry Column:", attr(lga_analysis_map, "sf_column"), "\n")

# 6 ------------------------------------------------------------------------
# Create maps

p_count <- map_risk_choropleth(
  data = lga_analysis_map,
  fill_col = "event_count",
  map_type = "count",
  classification = "quantile",
  n_classes = 5,
  title = "Event counts by LGA",
  subtitle = "Classed choropleth using quantile breaks",
  fill_label = "Events"
)

p_rate <- map_risk_choropleth(
  data = lga_analysis_map,
  fill_col = "event_rate_per_10000",
  map_type = "rate",
  classification = "quantile",
  n_classes = 5,
  title = "Event rate by LGA",
  subtitle = "Events per 10,000 population; quantile classes",
  fill_label = "Rate per 10,000"
)

p_prob <- map_risk_choropleth(
  data = lga_analysis_map,
  fill_col = "prob_ge_1_annual_pct",
  map_type = "probability",
  classification = "quantile",
  n_classes = 5,
  title = "Probability of at least one event by LGA",
  subtitle = "Poisson probability based on annualised observed history",
  fill_label = "Probability (%)"
)

p_smr <- map_risk_choropleth(
  data = lga_analysis_map,
  fill_col = "smr",
  map_type = "smr",
  classification = "smr_default",
  title = "SMR by LGA",
  subtitle = "Observed vs expected events; fixed SMR classes",
  fill_label = "SMR"
)

p_smr_flag <- map_risk_choropleth(
  data = lga_analysis_map,
  fill_col = "smr_ci_flag",
  map_type = "category",
  classification = "none",
  palette = "Set2",
  title = "SMR confidence interval flag by LGA",
  subtitle = "Exact Poisson confidence interval classification",
  fill_label = "SMR CI flag"
)

# 7 ------------------------------------------------------------------------
# Print maps

print(p_count)
print(p_rate)
print(p_prob)
print(p_smr)
print(p_smr_flag)

# 8 ------------------------------------------------------------------------
# Save maps

dir.create(output_paths$maps, recursive = TRUE, showWarnings = FALSE)

ggplot2::ggsave(
  filename = file.path(output_paths$maps, "map_event_count_quantile.png"),
  plot = p_count,
  width = 10,
  height = 7,
  dpi = 300
)

ggplot2::ggsave(
  filename = file.path(output_paths$maps, "map_event_rate_quantile.png"),
  plot = p_rate,
  width = 10,
  height = 7,
  dpi = 300
)

ggplot2::ggsave(
  filename = file.path(output_paths$maps, "map_poisson_probability_quantile.png"),
  plot = p_prob,
  width = 10,
  height = 7,
  dpi = 300
)

ggplot2::ggsave(
  filename = file.path(output_paths$maps, "map_smr_default_classes.png"),
  plot = p_smr,
  width = 10,
  height = 7,
  dpi = 300
)

ggplot2::ggsave(
  filename = file.path(output_paths$maps, "map_smr_ci_flag.png"),
  plot = p_smr_flag,
  width = 10,
  height = 7,
  dpi = 300
)

cat("\nmap_risk_choropleth() test complete.\n")