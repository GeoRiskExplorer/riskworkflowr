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
if (requireNamespace("sf", quietly = TRUE)) {
  units <- sf::st_sf(
    unit_id = c("A", "B", "C"),
    unit_name = c("Area A", "Area B", "Area C"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
      sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0)))),
      sf::st_polygon(list(rbind(c(2, 0), c(3, 0), c(3, 1), c(2, 1), c(2, 0))))
    ),
    crs = 4326
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
}
#> Simple feature collection with 3 features and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 3 ymax: 1
#> Geodetic CRS:  WGS 84
#>   unit_id unit_name event_count                       geometry
#> 1       A    Area A           3 POLYGON ((0 0, 1 0, 1 1, 0 ...
#> 2       B    Area B           0 POLYGON ((1 0, 2 0, 2 1, 1 ...
#> 3       C    Area C           7 POLYGON ((2 0, 3 0, 3 1, 2 ...
```
