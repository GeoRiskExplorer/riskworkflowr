# Counts and Aggregation

## Overview

Many spatial risk workflows ultimately rely on counts aggregated to
analysis units.

Typical workflows involve:

``` text
point events
→ spatial assignment
→ counts by unit
→ join counts back to polygons
→ calculate risk metrics
```

`riskworkflowr` provides helper functions to support reproducible and
consistent aggregation workflows.

## Core functions

The main aggregation functions include:

``` text
risk_count_units()
risk_join_counts()
risk_build_counts()
```

## Counting events by unit

[`risk_count_units()`](https://GeoRiskExplorer.github.io/riskworkflowr/reference/risk_count_units.md)
summarises events by analysis unit.

``` r

data <- data.frame(
  unit_id = c("A", "A", "B", "C", "C")
)

risk_count_units(
  data = data,
  unit_id_col = "unit_id"
)
```

    ##   unit_id event_count
    ## 1       A           2
    ## 2       C           2
    ## 3       B           1

## Joining counts back to spatial units

[`risk_join_counts()`](https://GeoRiskExplorer.github.io/riskworkflowr/reference/risk_join_counts.md)
joins aggregated counts back to spatial analysis units.

The `units` input is expected to be an `sf` object. The example below is
not evaluated in this vignette because it uses placeholder spatial data.

``` r

risk_join_counts(
  units = analysis_units,
  counts = unit_counts,
  unit_id_col = "unit_id"
)
```

## End-to-end workflow

[`risk_build_counts()`](https://GeoRiskExplorer.github.io/riskworkflowr/reference/risk_build_counts.md)
combines:

- spatial assignment
- aggregation
- count joins

into a single workflow.

``` r

risk_build_counts(
  points = event_points,
  units = analysis_units,
  unit_id_col = "unit_id"
)
```

## Zero-count handling

Zero-event areas are important in spatial risk analysis.

`riskworkflowr` supports workflows where:

- areas with no events are retained
- missing counts are converted to zero
- full spatial coverage is preserved

This is important for:

- rate calculations
- SMR workflows
- choropleth mapping
- H3 analyses

## Reproducibility and consistency

The aggregation workflow is designed to encourage:

- reproducible outputs
- consistent naming
- explicit join logic
- transparent handling of missing counts
- compatibility across areal unit systems

A core package principle is:

``` text
"units are units"
```

The same workflow should operate consistently across:

- administrative boundaries
- custom polygons
- H3 grids
- future spatial indexing systems

## Assumptions

Aggregation workflows assume:

- events have already been appropriately assigned
- unit identifiers are unique and stable
- geometry validity is appropriate for the processing context
- count logic matches the analytical objective

## Limitations and pitfalls

Potential issues include:

- duplicate assignments
- inconsistent identifiers
- missing joins
- invalid geometry
- sparse data
- unstable counts in small areas

Analysts should carefully review join logic and QA outputs before
interpretation.

## Alternative approaches

Alternative workflows may include:

- weighted assignment
- probabilistic assignment
- temporal aggregation
- hierarchical aggregation
- dynamic or moving spatial windows
- Bayesian smoothing
- spatial interpolation approaches

The appropriate approach depends on the analytical context and decision
requirements.

## Future development

Future versions of `riskworkflowr` may include:

- richer QA summaries
- duplicate assignment diagnostics
- temporal aggregation workflows
- uncertainty indicators
- grouped/category aggregation workflows
- improved H3 aggregation utilities
