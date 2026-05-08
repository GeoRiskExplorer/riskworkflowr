# 1 ------------------------------------------------------------------------
# generate_example_data.R
# Purpose: Generate reproducible synthetic example datasets for package use

library(sf)
library(dplyr)
library(tibble)
library(here)
library(usethis)

set.seed(123)

# 2 ------------------------------------------------------------------------
# Example synthetic areal dataset

example_lga_analysis <- tibble::tibble(
  lga_code = sprintf("%05d", 1:10),
  lga_name = paste("LGA", 1:10),
  event_count = sample(0:20, 10, replace = TRUE),
  pop = sample(1000:10000, 10, replace = TRUE)
)

# 3 ------------------------------------------------------------------------
# Example synthetic hex dataset

hex_polys <- sf::st_make_grid(
  sf::st_as_sfc(
    sf::st_bbox(
      c(
        xmin = 0,
        ymin = 0,
        xmax = 10000,
        ymax = 10000
      ),
      crs = 3857
    )
  ),
  n = c(10, 10),
  square = FALSE
)

example_hex_analysis <- sf::st_sf(
  hex_id = paste0("hex_", seq_along(hex_polys)),
  event_count = sample(0:5, length(hex_polys), replace = TRUE),
  pop = sample(10:100, length(hex_polys), replace = TRUE),
  geometry = hex_polys
)

# 4 ------------------------------------------------------------------------
# Save package datasets

usethis::use_data(
  example_lga_analysis,
  overwrite = TRUE
)

usethis::use_data(
  example_hex_analysis,
  overwrite = TRUE
)

# 5 ------------------------------------------------------------------------
# QA

cat("\n--- Example Data QA ---\n")

cat("\nLGA example rows:\n")
print(nrow(example_lga_analysis))

cat("\nHex example rows:\n")
print(nrow(example_hex_analysis))

cat("\nSynthetic example datasets created.\n")