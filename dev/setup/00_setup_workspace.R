# 00_setup_workspace.R

# 1. Package setup ---------------------------------------------------------

required_packages <- c(
  "usethis",
  "devtools",
  "testthat",
  "roxygen2",
  "sf",
  "dplyr",
  "ggplot2",
  "janitor",
  "here"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# 2. Create folder structure ----------------------------------------------

project_root <- "E:/Packages/riskworkflowr"

# 2. Create folder structure ----------------------------------------------

folders <- c(
  "R",
  "dev",
  "config",
  "data-raw",
  "data-raw/synthetic",
  "inst",
  "inst/extdata",
  "tests",
  "tests/testthat",
  "man",
  "vignettes",
  "outputs",
  "outputs/maps",
  "outputs/tables",
  "outputs/qa",
  "notes"
)

dir.create(project_root, recursive = TRUE, showWarnings = FALSE)

for (folder in folders) {
  dir.create(file.path(project_root, folder), recursive = TRUE, showWarnings = FALSE)
}

# 3. Create starter files --------------------------------------------------

starter_files <- c(
  "README.md",
  "DESCRIPTION",
  "NAMESPACE",
  ".gitignore",
  "notes/function_scope.md",
  "notes/naming_conventions.md",
  "notes/package_build_notes.md",
  "data-raw/README.md",
  "inst/extdata/README.md"
)

for (file in starter_files) {
  file_path <- file.path(project_root, file)

  if (!file.exists(file_path)) {
    file.create(file_path)
  }
}

# 4. Create R function placeholder files ----------------------------------

r_files <- c(
  "risk_prepare.R",
  "risk_validate.R",
  "smr_expected.R",
  "smr_calc.R",
  "smr_classify.R",
  "unit_recommend.R",
  "grid_build.R",
  "grid_assign_points.R",
  "sj_intersect.R",
  "sj_nearest.R",
  "sj_intersect_nearest.R",
  "risk_qa_summary.R",
  "map_risk.R",
  "utils.R"
)

for (file in r_files) {
  file_path <- file.path(project_root, "R", file)

  if (!file.exists(file_path)) {
    writeLines(
      c(
        paste0("# ", file),
        "# Placeholder for package function development.",
        ""
      ),
      file_path
    )
  }
}

# 5. Create development scripts -------------------------------------------

dev_files <- c(
  "01_test_gdb_connection.R",
  "02_import_test_data.R",
  "03_build_test_smr_surface.R",
  "04_plot_test_outputs.R"
)

for (file in dev_files) {
  file_path <- file.path(project_root, "dev", file)

  if (!file.exists(file_path)) {
    writeLines(
      c(
        paste0("# ", file),
        "# Development script.",
        ""
      ),
      file_path
    )
  }
}

# 6. Write .gitignore ------------------------------------------------------

gitignore_path <- file.path(project_root, ".gitignore")

writeLines(
  c(
    ".Rproj.user",
    ".Rhistory",
    ".RData",
    ".Ruserdata",
    "outputs/",
    "data-raw/gdb/*.gdb/",
    "config/project_paths.yml",
    "*.qs2",
    "*.duckdb",
    "*.gpkg"
  ),
  gitignore_path
)

message("Workspace created at: ", project_root)