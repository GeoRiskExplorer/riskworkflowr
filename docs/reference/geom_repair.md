# Repair invalid geometry for spatial processing

Repairs invalid geometries using
[`sf::st_make_valid()`](https://rdrr.io/pkg/sf/man/valid.html) so
downstream spatial predicates and joins behave more reliably.

## Usage

``` r
geom_repair(x, quiet = FALSE)
```

## Arguments

- x:

  An sf object.

- quiet:

  Logical; if TRUE, suppress repair summary messages.

## Value

An sf object with repaired geometry.

## Details

This function is intended to support reliable spatial operations across
sf, GEOS, DuckDB, and ArcGIS-derived workflows where geometry validity
may affect processing.

## Examples

``` r
if (FALSE) { # \dontrun{
repaired_sf <- geom_repair(my_sf)
} # }
```
