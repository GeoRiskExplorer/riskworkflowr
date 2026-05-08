# Example Hexbin Analysis Dataset

Synthetic example hexbin spatial dataset containing example event counts
and population values.

## Usage

``` r
example_hex_analysis
```

## Format

An sf object with hexagonal geometries and attributes:

- hex_id:

  Synthetic hexagon identifier

- event_count:

  Synthetic event count

- pop:

  Synthetic population value

- geometry:

  Hexagonal polygon geometry

## Source

Synthetic data generated using `data-raw/generate_example_data.R`

## Details

This dataset is included for demonstration, testing, and vignette
purposes only. Values are synthetic and generated using fixed random
seeds for reproducibility.
