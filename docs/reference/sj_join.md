# Spatially join points to polygon units

Assigns point features to polygon units using intersect, nearest, or
intersect-nearest logic.

## Usage

``` r
sj_join(
  points,
  polygons,
  id_col,
  name_col = NULL,
  join_mode = c("intersect", "nearest", "intersect_nearest"),
  repair_polygons = TRUE
)
```

## Arguments

- points:

  An sf point object.

- polygons:

  An sf polygon object containing target join units.

- id_col:

  Name of the polygon identifier column to attach to points.

- name_col:

  Optional polygon name column to attach to points.

- join_mode:

  Spatial join mode. One of `"intersect"`, `"nearest"`, or
  `"intersect_nearest"`.

- repair_polygons:

  Logical; if TRUE, repair polygon geometry before spatial predicates
  are evaluated.

## Value

An sf object of joined point features with `join_method` and
`join_distance_m` columns.

## Details

This function is intended for reproducible spatial assignment workflows
supporting areal units, custom polygons, and hexagonal grids.

The output includes `join_method` and `join_distance_m` fields to
support auditability of spatial assignment decisions.

## Examples

``` r
if (FALSE) { # \dontrun{
joined <- sj_join(
  points = pts,
  polygons = polygons,
  id_col = "lga_code",
  name_col = "lga_name",
  join_mode = "intersect_nearest"
)
} # }
```
