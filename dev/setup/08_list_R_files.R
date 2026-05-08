# 1 ------------------------------------------------------------------------
# 08_list_R_files.R
# Purpose: List all files in R/ folder

library(here)
library(dplyr)
library(stringr)

r_path <- here::here("R")

r_files <- list.files(
  path = r_path,
  pattern = "\\.R$",
  full.names = TRUE
)

r_summary <- tibble::tibble(
  file_name = basename(r_files),
  full_path = r_files
) |>
  mutate(
    order = as.integer(stringr::str_extract(file_name, "^[0-9]+")),
    file_type = case_when(
      str_detect(file_name, "geom") ~ "geometry",
      str_detect(file_name, "sj_") ~ "spatial_join",
      TRUE ~ "other"
    )
  ) |>
  arrange(file_type, file_name)

print(r_summary)