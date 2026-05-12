# Identify distinctive risk categories by spatial unit

Calculates category-specific comparative SMR-style values and identifies
the highest and optionally lowest distinctive category for each unit.

## Usage

``` r
risk_distinct_category(
  data,
  unit_id_col,
  category_col,
  observed_col = "event_count",
  denominator_col,
  min_count = 1,
  include_lowest = TRUE,
  highest_category_col = "highest_category",
  highest_smr_col = "highest_smr",
  highest_count_col = "highest_event_count",
  lowest_category_col = "lowest_category",
  lowest_smr_col = "lowest_smr",
  lowest_count_col = "lowest_event_count"
)
```

## Arguments

- data:

  A data frame containing unit, category, count, and denominator
  columns.

- unit_id_col:

  Name of the spatial unit identifier column.

- category_col:

  Name of the category/group column.

- observed_col:

  Name of the observed count column. Defaults to `"event_count"`.

- denominator_col:

  Name of the denominator/exposure column.

- min_count:

  Minimum observed count required for a category to be considered.

- include_lowest:

  Logical. If TRUE, also returns the lowest eligible category.

- highest_category_col:

  Output column for highest category.

- highest_smr_col:

  Output column for highest SMR.

- highest_count_col:

  Output column for highest category count.

- lowest_category_col:

  Output column for lowest category.

- lowest_smr_col:

  Output column for lowest SMR.

- lowest_count_col:

  Output column for lowest category count.

## Value

A data frame with one row per unit and distinctive category outputs.

\#' The `insufficient_count_flag` column is `TRUE` where no category
within a unit met the minimum count threshold set by `min_count`.

## Details

This function is intended for exploratory grouped/category comparative
risk profiling. It identifies categories with the highest relative
observed-versus-expected value within each spatial unit.

Results should be interpreted carefully where counts are low,
denominators are unstable, or categories are inconsistently coded.

## Examples

``` r
data <- data.frame(
  unit_id = c("A", "A", "B", "B"),
  category = c("Falls", "Water", "Falls", "Water"),
  event_count = c(10, 2, 3, 8),
  exposure = c(1000, 1000, 800, 800)
)

risk_distinct_category(
  data = data,
  unit_id_col = "unit_id",
  category_col = "category",
  denominator_col = "exposure"
)
#>   unit_id highest_category highest_smr highest_event_count lowest_category
#> 1       A            Falls    1.384615                  10           Water
#> 2       B            Water    1.800000                   8           Falls
#>   lowest_smr lowest_event_count category_count_used insufficient_count_flag
#> 1  0.3600000                  2                   2                   FALSE
#> 2  0.5192308                  3                   2                   FALSE
```
