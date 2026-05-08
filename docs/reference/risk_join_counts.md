# Join event counts back to spatial units

Joins a count table back to polygon or hexbin units and optionally
replaces missing count values with a chosen value, usually zero.

## Usage

``` r
risk_join_counts(
  units,
  counts,
  unit_id_col,
  unit_name_col = NULL,
  count_col = "event_count",
  missing_count_value = 0
)
```

## Arguments

- units:

  An sf object containing polygon or hexbin units.

- counts:

  A data frame containing counts by unit.

- unit_id_col:

  Name of the shared unit identifier column.

- unit_name_col:

  Optional shared unit name column.

- count_col:

  Name of the count column.

- missing_count_value:

  Value used to replace missing counts. Use `NULL` to preserve missing
  values.

## Value

An sf object with counts joined to the input units.

## Examples

``` r
units <- data.frame(
  unit_id = c("A", "B", "C"),
  unit_name = c("Area A", "Area B", "Area C")
)

counts <- data.frame(
  unit_id = c("A", "C"),
  event_count = c(3, 7)
)

risk_join_counts(
  units = units,
  counts = counts,
  unit_id_col = "unit_id"
)
#> Error: `units` must be an sf object.
```
