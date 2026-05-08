# 1 ------------------------------------------------------------------------
# 19_test_hexbin_counts.R
# Purpose: Test risk_build_counts() using clustered H3 hexbins

library(arcgisbinding)
library(sf)
library(dplyr)
library(janitor)
library(here)

arc.check_product()

source(here::here("dev", "01_set_project_paths.R"))
source(here::here("R", "geom_repair.R"))
source(here::here("R", "sj_join.R"))
source(here::here("R", "risk_count_units.R"))
source(here::here("R", "risk_join_counts.R"))
source(here::here("R", "risk_build_counts.R"))
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
# Read hexbin test data

hex_bins <- read_arcgis_fc(
  file.path(gdb_path, "h3", "h3_r12_cluster_sample_200")
)

pts <- read_arcgis_fc(
  file.path(gdb_path, "events", "hex_cluster_test_points_250")
)

sf::st_geometry(hex_bins) <- "geom"
sf::st_geometry(pts) <- "geom"

hex_bins <- hex_bins |>
  sf::st_transform(7899) |>
  geom_repair(quiet = TRUE)

pts <- pts |>
  sf::st_transform(7899)

# 4 ------------------------------------------------------------------------
# Check fields

cat("\n--- Hexbin Fields ---\n")
print(names(hex_bins))

cat("\n--- Point Fields ---\n")
print(names(pts))

# 5 ------------------------------------------------------------------------
# Ensure hex_id exists

if (!"hex_id" %in% names(hex_bins)) {
  hex_bins <- hex_bins |>
    dplyr::mutate(hex_id = paste0("hex_", dplyr::row_number()))
}

# 6 ------------------------------------------------------------------------
# Build hexbin counts

hex_counts_result <- risk_build_counts(
  points = pts,
  units = hex_bins,
  unit_id_col = "hex_id",
  unit_name_col = NULL,
  join_mode = "intersect_nearest",
  count_col = "event_count",
  missing_count_value = 0,
  repair_units = TRUE,
  return = "all"
)

hex_counts <- hex_counts_result$units

# 7 ------------------------------------------------------------------------
# QA

qa_print_areal_counts(
  hex_counts,
  count_col = "event_count"
)

qa_print_join_methods(
  hex_counts_result$joined_points
)

hex_df <- hex_counts |>
  sf::st_drop_geometry()

cat("\n--- QA: Hexbin Counts ---\n")
cat("Total Hexbins:", nrow(hex_df), "\n")
cat("Hexbins with Events:", sum(hex_df$event_count > 0), "\n")
cat("Hexbins without Events:", sum(hex_df$event_count == 0), "\n")
cat("Total Events:", sum(hex_df$event_count), "\n")
cat("Max Events in One Hexbin:", max(hex_df$event_count), "\n")

cat("\n--- QA: Point Type Assignment ---\n")

point_type_qa <- hex_counts_result$joined_points |>
  sf::st_drop_geometry() |>
  dplyr::count(point_type, join_method)

print(point_type_qa)

# 8 ------------------------------------------------------------------------
# Validation

stopifnot(nrow(hex_counts) == 200)
stopifnot(sum(hex_counts$event_count, na.rm = TRUE) == 250)
stopifnot(sum(hex_counts_result$joined_points$join_method == "nearest") > 0)
stopifnot(sum(hex_counts_result$joined_points$join_method == "intersect") > 0)

cat("\nHexbin count workflow test passed.\n")


# -------------------------------------------------------------------------
# Add synthetic population

set.seed(999)

hex_counts <- hex_counts |>
  dplyr::mutate(
    pop = sample(10:100, size = n(), replace = TRUE)
  )

cat("\n--- QA: Synthetic Population ---\n")
cat("Min:", min(hex_counts$pop), "\n")
cat("Max:", max(hex_counts$pop), "\n")
cat("Mean:", round(mean(hex_counts$pop), 1), "\n")

hex_analysis <- hex_counts |>
  risk_calc_rate(
    count_col = "event_count",
    denominator_col = "pop",
    rate_col = "event_rate_per_100",
    multiplier = 100
  ) |>
  risk_calc_poisson_probability(
    count_col = "event_count",
    period_value = 2,
    lambda_col = "lambda",
    probability_col = "prob_ge_1",
    probability_pct_col = "prob_ge_1_pct",
    output = "both"
  ) |>
  risk_calc_smr(
    observed_col = "event_count",
    denominator_col = "pop",
    expected_col = "expected_count",
    smr_col = "smr",
    ci_method = "exact"
  )

source(here::here("R", "map_risk_choropleth.R"))

# Counts
p_count <- map_risk_choropleth(
  data = hex_analysis,
  fill_col = "event_count",
  map_type = "count",
  classification = "quantile",
  title = "Hexbin Event Counts",
  fill_label = "Events"
)

# Rate
p_rate <- map_risk_choropleth(
  data = hex_analysis,
  fill_col = "event_rate_per_100",
  map_type = "rate",
  classification = "quantile",
  title = "Hexbin Event Rate",
  fill_label = "Rate per 100"
)

# Poisson
p_prob <- map_risk_choropleth(
  data = hex_analysis,
  fill_col = "prob_ge_1_pct",
  map_type = "probability",
  classification = "quantile",
  title = "Probability of ≥1 Event",
  fill_label = "Probability (%)"
)

# SMR
p_smr <- map_risk_choropleth(
  data = hex_analysis,
  fill_col = "smr",
  map_type = "smr",
  classification = "smr_default",
  title = "Hexbin SMR",
  fill_label = "SMR"
)

# SMR flags
p_flag <- map_risk_choropleth(
  data = hex_analysis,
  fill_col = "smr_ci_flag",
  map_type = "category",
  classification = "none",
  title = "SMR Classification",
  fill_label = "SMR Flag"
)

print(p_count)
print(p_rate)
print(p_prob)
print(p_smr)
print(p_flag)