# 1 ------------------------------------------------------------------------
# 02_list_dev_files.R
# Purpose: List and summarise dev scripts

library(here)
library(dplyr)
library(stringr)

dev_path <- here::here("dev")

dev_files <- list.files(
  path = dev_path,
  pattern = "\\.R$",
  full.names = TRUE
)

dev_summary <- tibble::tibble(
  file_name = basename(dev_files),
  full_path = dev_files
) |>
  mutate(
    order = as.integer(str_extract(file_name, "^[0-9]+")),
    script_type = case_when(
      str_detect(file_name, "setup") ~ "setup",
      str_detect(file_name, "paths") ~ "paths",
      str_detect(file_name, "list") ~ "utility",
      str_detect(file_name, "test") ~ "test",
      TRUE ~ "other"
    )
  ) |>
  arrange(order)

print(dev_summary)