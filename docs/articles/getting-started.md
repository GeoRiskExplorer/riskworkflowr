# Getting Started with riskworkflowr

## Overview

`riskworkflowr` provides reproducible workflows for spatial risk
analysis, including:

- spatial assignment
- event aggregation
- risk metrics
- SMR analysis
- cartographic outputs

The package is designed to support consistent workflows across:

- administrative boundaries
- custom areal units
- H3 hexagonal grids

## Why riskworkflowr?

Many spatial risk workflows require analysts to repeatedly combine:

- spatial joins
- event aggregation
- comparative risk metrics
- choropleth mapping
- reproducible analytical workflows

`riskworkflowr` aims to provide a consistent framework for these
commonly repeated tasks while remaining compatible with the broader R
spatial ecosystem.

## Core workflow

``` text
point events
→ spatial assignment
→ aggregation/counts
→ risk metrics
→ mapping
```

A core design principle of the package is:

``` text
"units are units"
```

The same analytical workflow should operate consistently across:

- administrative boundaries
- custom polygons
- H3 hexagonal grids
- other areal unit systems

## Install package

``` r

pak::pak("GeoRiskExplorer/riskworkflowr")
```

## Load package

``` r

library(riskworkflowr)
```

## Create example data

``` r

data <- data.frame(
  event_count = c(5, 10, 15),
  population = c(1000, 2000, 3000)
)
```

## Calculate rates

``` r

risk_calc_rate(
  data = data,
  count_col = "event_count",
  denominator_col = "population"
)
```

    ##   event_count population rate_per_10000
    ## 1           5       1000             50
    ## 2          10       2000             50
    ## 3          15       3000             50

## Calculate SMR

``` r

risk_calc_smr(
  data = data,
  observed_col = "event_count",
  denominator_col = "population"
)
```

    ##   event_count population expected_count smr smr_lower smr_upper
    ## 1           5       1000              5   1 0.3246973  2.333666
    ## 2          10       2000             10   1 0.4795389  1.839036
    ## 3          15       3000             15   1 0.5596924  1.649348
    ##             smr_ci_flag
    ## 1 not_clearly_different
    ## 2 not_clearly_different
    ## 3 not_clearly_different

## Methodological scope

The package primarily focuses on practical and reproducible workflows
for exploratory spatial risk analysis and communication.

The implemented methods should not be interpreted as replacing more
advanced epidemiological, spatial statistical, or inferential modelling
approaches where such methods are appropriate and supported by the
available data.

## Important assumptions

This package implements practical and reproducible spatial risk
workflows commonly used in operational and applied analytical contexts.

Alternative epidemiological and spatial statistical approaches may be
more appropriate depending on:

- data availability
- denominator quality
- covariate availability
- inferential objectives
- spatial scale
- analytical assumptions

## Next steps

Additional vignettes provide detail on:

- spatial assignment
- aggregation workflows
- SMR analysis
- H3 workflows
- mapping
- methodological considerations
