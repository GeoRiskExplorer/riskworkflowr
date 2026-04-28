# 1 ------------------------------------------------------------------------
# 01_set_project_paths.R
# Purpose: Central project paths for riskworkflowr dev scripts

library(here)

project_root <- here::here()

arcgis_project_name <- "riskworkflowr_spatial"

arcgis_project_root <- file.path(
  "E:/ArcGIS_Projects",
  arcgis_project_name
)

gdb_path <- file.path(
  arcgis_project_root,
  paste0(arcgis_project_name, ".gdb")
)

standard_crs <- list(
  epsg = 7899,
  name = "GDA2020_Vicgrid"
)

fc_paths <- list(
  lga_aust_vic_2025 = file.path(
    gdb_path,
    "asgs",
    "lga_aust_vic_2025"
  ),

  ste_2021_aust_vic = file.path(
    gdb_path,
    "asgs",
    "ste_2021_aust_vic"
  ),

  random_100 = file.path(
    gdb_path,
    "events",
    "random_100"
  ),

  random_1000 = file.path(
    gdb_path,
    "events",
    "random_1000"
  ),

  vic_buffer_10km = file.path(
    gdb_path,
    "scratch",
    "vic_buffer_10km"
  ),

  h3_r12 = file.path(
    gdb_path,
    "h3",
    "h3_r12"
  )
)

output_paths <- list(
  maps = here::here("outputs", "maps"),
  tables = here::here("outputs", "tables"),
  qa = here::here("outputs", "qa")
)

dir.create(output_paths$maps, recursive = TRUE, showWarnings = FALSE)
dir.create(output_paths$tables, recursive = TRUE, showWarnings = FALSE)
dir.create(output_paths$qa, recursive = TRUE, showWarnings = FALSE)

cat("\nProject root:", project_root)
cat("\nArcGIS project root:", arcgis_project_root)
cat("\nGDB path:", gdb_path, "\n")