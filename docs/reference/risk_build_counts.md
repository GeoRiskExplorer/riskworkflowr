# Build event counts for spatial units

Runs the core count-building workflow: spatially joins points to units,
counts events by unit, and joins the counts back to the original units.

## Usage

``` r
risk_build_counts(
  points,
  units,
  unit_id_col,
  unit_name_col = NULL,
  join_mode = "intersect_nearest",
  count_col = "event_count",
  missing_count_value = 0,
  repair_units = TRUE,
  return = c("units", "all")
)
```

## Arguments

- points:

  An sf point object.

- units:

  An sf polygon or hexbin object.

- unit_id_col:

  Name of the unit identifier column.

- unit_name_col:

  Optional unit name column.

- join_mode:

  Spatial join mode passed to
  [`sj_join()`](https://GeoRiskExplorer.github.io/riskworkflowr/reference/sj_join.md).

- count_col:

  Name of the output count column.

- missing_count_value:

  Value used for units with no events.

- repair_units:

  Logical; if TRUE, repair unit geometry before spatial joins.

- return:

  Return `"units"` for counted units only, or `"all"` for a list
  containing joined points, counts, and counted units.

## Value

An sf object, or a list when `return = "all"`.

## Details

This function works for administrative areas, custom polygons, and
hexbins.
