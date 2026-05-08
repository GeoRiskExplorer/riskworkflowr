# riskworkflowr

Reproducible spatial risk analysis workflows for areal units and H3
hexagonal grids, including spatial assignment, aggregation, risk
metrics, SMR analysis, and cartographic outputs.

## Installation

``` r

# install.packages("pak")

pak::pak("GeoRiskExplorer/riskworkflowr")
```

## Why riskworkflowr?

`riskworkflowr` provides a reproducible workflow for:

``` text
points/events
→ spatial assignment
→ aggregation/counts
→ risk metrics
→ cartographic outputs
```

The same workflow works for:

- areal units
- administrative boundaries
- custom polygons
- H3 hexagonal grids

## Calculate rates

``` r

library(riskworkflowr)

data <- data.frame(
  event_count = c(5, 10, 15),
  population = c(1000, 2000, 3000)
)

risk_calc_rate(
  data = data,
  count_col = "event_count",
  denominator_col = "population"
)
#>   event_count population rate_per_10000
#> 1           5       1000             50
#> 2          10       2000             50
#> 3          15       3000             50
```

## Calculate SMR

``` r

data <- data.frame(
  event_count = c(5, 10, 20),
  population = c(1000, 2000, 3000)
)

risk_calc_smr(
  data = data,
  observed_col = "event_count",
  denominator_col = "population"
)
#>   event_count population expected_count       smr smr_lower smr_upper
#> 1           5       1000       5.833333 0.8571429 0.2783120  2.000285
#> 2          10       2000      11.666667 0.8571429 0.4110333  1.576316
#> 3          20       3000      17.500000 1.1428571 0.6980868  1.765050
#>             smr_ci_flag
#> 1 not_clearly_different
#> 2 not_clearly_different
#> 3 not_clearly_different
```

## Spatial risk workflow

``` text
point events
→ spatial assignment
→ event counts
→ join back to polygons
→ risk metrics
→ mapping
```

## Package philosophy

`riskworkflowr` is designed as a reproducible spatial risk workflow
framework rather than a generic GIS toolbox.

Core principles:

- reproducibility
- defensible spatial assignment
- auditability of joins
- transparent risk metrics
- workflow consistency across areal units and H3 grids

## Example datasets

The package includes synthetic example datasets for demonstration and
testing.

No private or operational data are included.
