# Create a classed choropleth risk map

Creates a choropleth map for areal units or hexbins using classed colour
schemes and cartographic defaults appropriate for spatial risk analysis.

## Usage

``` r
map_risk_choropleth(
  data,
  fill_col,
  map_type = c("count", "rate", "probability", "smr", "category"),
  classification = NULL,
  n_classes = 5,
  palette = NULL,
  reverse_palette = FALSE,
  title = NULL,
  subtitle = NULL,
  fill_label = NULL,
  border_colour = "grey70",
  border_width = 0.2,
  na_colour = "grey90"
)
```

## Arguments

- data:

  An sf object.

- fill_col:

  Name of the variable to map.

- map_type:

  Type of mapped variable. Used to select default palettes and
  classification behaviour.

- classification:

  Classification method.

- n_classes:

  Number of classes for classified maps.

- palette:

  Colour palette name.

- reverse_palette:

  Logical; if TRUE, reverse palette direction.

- title:

  Optional plot title.

- subtitle:

  Optional plot subtitle.

- fill_label:

  Legend label.

- border_colour:

  Polygon border colour.

- border_width:

  Polygon border width.

- na_colour:

  Colour used for missing values.

## Value

A ggplot object.

## Details

Supports sequential, diverging, and qualitative colour schemes using
Cynthia Brewer-style palettes through `RColorBrewer`.

## References

Brewer CA. Designing Better Maps: A Guide for GIS Users.
