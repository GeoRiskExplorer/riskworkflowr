# Count events within spatial units

Aggregates assigned event records by polygon, hexbin, or other spatial
unit.

## Usage

``` r
risk_count_units(
  data,
  unit_id_col,
  unit_name_col = NULL,
  count_col = "event_count",
  drop_geometry = TRUE
)
```

## Arguments

- data:

  A data frame or sf object containing joined event records.

- unit_id_col:

  Name of the unit identifier column.

- unit_name_col:

  Optional name of the unit name column.

- count_col:

  Name of the output count column.

- drop_geometry:

  Logical; if TRUE and `data` is an sf object, geometry is dropped
  before counting.

## Value

A data frame containing event counts by unit.

## Details

This function is intended for risk analysis workflows where events have
already been assigned to administrative areas, statistical regions,
sites, or grid systems using a spatial join workflow such as
[`sj_join()`](https://GeoRiskExplorer.github.io/riskworkflowr/reference/sj_join.md).

## Examples

``` r
data <- data.frame(
  unit_id = c("A", "A", "B", "C", "C", "C")
)

risk_count_units(
  data = data,
  unit_id_col = "unit_id"
)
#>   unit_id event_count
#> 1       C           3
#> 2       A           2
#> 3       B           1
```
