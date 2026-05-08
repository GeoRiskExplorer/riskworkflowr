# 1 ------------------------------------------------------------------------
# 21_tidy_package_workspace.R
# Purpose: Tidy dev workspace before package hardening

library(here)
library(fs)
library(dplyr)
library(stringr)

project_root <- here::here()

# 2 ------------------------------------------------------------------------
# Create clean dev subfolders

dev_dirs <- c(
  "dev/setup",
  "dev/tested",
  "dev/exploratory",
  "dev/archive"
)

purrr::walk(
  file.path(project_root, dev_dirs),
  fs::dir_create
)

# 3 ------------------------------------------------------------------------
# Files to move

setup_files <- c(
  "00_setup_workspace.R",
  "01_set_project_paths.R",
  "02_list_dev_files.R",
  "08_list_R_files.R",
  "20_audit_package_files.R",
  "21_tidy_package_workspace.R"
)

tested_files <- c(
  "05_test_geom_repair.R",
  "06_test_sj_join.R",
  "10_test_risk_count_units.R",
  "11_test_risk_join_counts.R",
  "12_test_risk_build_counts.R",
  "14_test_risk_calc_rate.R",
  "15_test_risk_calc_poisson_probability.R",
  "16_test_risk_calc_smr.R",
  "17_test_map_risk_choropleth.R",
  "19_test_hexbin_counts.R"
)

exploratory_files <- c(
  "03_test_lga_assignment.R",
  "04_scratch_compare_lga_closest_join.R",
  "04_scratch_lga_predicate_debug.R",
  "07_create_random_outside_test_points.R",
  "09_test_areal_counts_map.R",
  "13_test_join_census_lga.R",
  "18_create_hex_sample_test_data.R"
)

# 4 ------------------------------------------------------------------------
# Move helper

move_if_exists <- function(files, target_dir) {
  for (file in files) {
    from <- here::here("dev", file)
    to <- here::here(target_dir, file)

    if (file.exists(from)) {
      fs::file_move(from, to)
      cat("Moved:", file, "->", target_dir, "\n")
    }
  }
}

move_if_exists(setup_files, "dev/setup")
move_if_exists(tested_files, "dev/tested")
move_if_exists(exploratory_files, "dev/exploratory")

# 5 ------------------------------------------------------------------------
# Update .gitignore to protect local/private material

gitignore_path <- here::here(".gitignore")

gitignore_add <- c(
  "",
  "# Local/private project paths",
  "config/project_paths.yml",
  "dev/setup/01_set_project_paths.R",
  "",
  "# Generated outputs",
  "outputs/",
  "",
  "# Large/local spatial data",
  "*.gdb/",
  "*.gdb",
  "*.duckdb",
  "*.gpkg",
  "*.qs2",
  "",
  "# R/session files",
  ".Rproj.user",
  ".Rhistory",
  ".RData",
  ".Ruserdata"
)

existing <- if (file.exists(gitignore_path)) {
  readLines(gitignore_path, warn = FALSE)
} else {
  character()
}

updated <- unique(c(existing, gitignore_add))

writeLines(updated, gitignore_path)

# 6 ------------------------------------------------------------------------
# Print summary

cat("\n--- Package Workspace Tidy Summary ---\n")
cat("Project Root:", project_root, "\n")
cat("Setup scripts:", length(list.files(here::here("dev/setup"), pattern = '\\\\.R$')), "\n")
cat("Tested scripts:", length(list.files(here::here("dev/tested"), pattern = '\\\\.R$')), "\n")
cat("Exploratory scripts:", length(list.files(here::here("dev/exploratory"), pattern = '\\\\.R$')), "\n")
cat("\nReview moved files before committing.\n")