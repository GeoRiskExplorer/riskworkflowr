# 1 ------------------------------------------------------------------------
# 20_audit_package_files.R
# Purpose: Audit R/ and dev/ scripts before package cleanup

library(here)
library(dplyr)
library(stringr)
library(readr)
library(tibble)

# 2 ------------------------------------------------------------------------
# Paths

r_path <- here::here("R")
dev_path <- here::here("dev")

# 3 ------------------------------------------------------------------------
# Helper: audit files

audit_scripts <- function(path) {
  files <- list.files(
    path = path,
    pattern = "\\.R$",
    full.names = TRUE
  )

  tibble(
    file_name = basename(files),
    full_path = files
  ) |>
    mutate(
      file_text = lapply(full_path, readLines, warn = FALSE),
      n_lines = lengths(file_text),
      has_placeholder = purrr::map_lgl(
        file_text,
        ~ any(str_detect(.x, regex("placeholder|development script", ignore_case = TRUE)))
      ),
      has_function = purrr::map_lgl(
        file_text,
        ~ any(str_detect(.x, "<- function\\("))
      ),
      function_names = purrr::map_chr(
        file_text,
        ~ {
          fn <- str_match(.x, "^([A-Za-z0-9_\\.]+)\\s*<-\\s*function\\(")[, 2]
          fn <- fn[!is.na(fn)]
          if (length(fn) == 0) "" else paste(fn, collapse = ", ")
        }
      ),
      has_source = purrr::map_lgl(
        file_text,
        ~ any(str_detect(.x, "source\\("))
      )
    ) |>
    select(-file_text) |>
    arrange(file_name)
}

# 4 ------------------------------------------------------------------------
# Audit R folder

r_audit <- audit_scripts(r_path)

cat("\n--- AUDIT: R/ package files ---\n")
print(r_audit)

cat("\n--- R files likely placeholders / empty shells ---\n")
print(
  r_audit |>
    filter(has_placeholder | !has_function)
)

# 5 ------------------------------------------------------------------------
# Audit dev folder

dev_audit <- audit_scripts(dev_path)

cat("\n--- AUDIT: dev/ scripts ---\n")
print(dev_audit)

# 6 ------------------------------------------------------------------------
# Dev tested workflow list

tested_dev_scripts <- tibble::tribble(
  ~script, ~status, ~purpose,
  "05_test_geom_repair.R", "passed", "Validated invalid LGA geometry repair",
  "06_test_sj_join.R", "passed", "Validated intersect / nearest / intersect_nearest spatial join modes",
  "10_test_risk_count_units.R", "passed", "Validated count aggregation by unit",
  "11_test_risk_join_counts.R", "passed", "Validated joining counts back to polygons",
  "12_test_risk_build_counts.R", "passed", "Validated wrapper for build counts workflow",
  "14_test_risk_calc_rate.R", "passed", "Validated rate per denominator calculation",
  "15_test_risk_calc_poisson_probability.R", "passed", "Validated observed-count Poisson probability",
  "16_test_risk_calc_smr.R", "passed", "Validated SMR + exact Poisson CI",
  "17_test_map_risk_choropleth.R", "passed", "Validated classed Brewer-style choropleth maps",
  "19_test_hexbin_counts.R", "passed", "Validated same count workflow for H3 hexbins"
)

cat("\n--- DEV scripts manually confirmed as tested ---\n")
print(tested_dev_scripts)

# 7 ------------------------------------------------------------------------
# Save audit outputs

dir.create(here::here("outputs", "qa"), recursive = TRUE, showWarnings = FALSE)

readr::write_csv(
  r_audit,
  here::here("outputs", "qa", "audit_r_files.csv")
)

readr::write_csv(
  dev_audit,
  here::here("outputs", "qa", "audit_dev_files.csv")
)

readr::write_csv(
  tested_dev_scripts,
  here::here("outputs", "qa", "audit_tested_dev_scripts.csv")
)

cat("\nAudit complete. Outputs saved to outputs/qa.\n")