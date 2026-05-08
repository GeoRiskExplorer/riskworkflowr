# H3 and Hexagonal Workflows

## Overview

`riskworkflowr` is designed so that the same core workflow can be
applied to different spatial unit systems.

This includes:

- administrative boundaries
- custom polygons
- regular hexagonal grids
- H3 hexagonal grids

A core package principle is:

``` text
"units are units"
```

If a spatial unit layer contains valid geometry and a stable unit
identifier, it should be usable in the same analytical workflow.

## Why use hexagons or H3?

Hexagonal grids can be useful when analysts want a spatial framework
that is less tied to administrative boundaries.

Potential advantages include:

- consistent spatial units
- repeatable spatial indexing
- reduced dependence on changing administrative boundaries
- support for localised spatial pattern detection
- compatibility with gridded exposure or environmental datasets

H3 grids may be especially useful where a consistent global spatial
indexing system is required.

## When hexagons may not be appropriate

Hexagons are not automatically better than areal units.

They may be inappropriate where:

- event counts are very low
- denominators are unstable
- spatial units become too small for meaningful interpretation
- operational decisions are made using administrative or management
  boundaries
- privacy, confidentiality, or suppression rules apply
- stakeholders require outputs aligned to known regions

In some workflows, an administrative boundary may be more interpretable
and defensible than a fine-scale hexagonal grid.

## Core workflow

The intended workflow is the same:

``` text
point events
→ spatial assignment to units
→ counts by unit
→ risk metrics
→ mapping
```

The key difference is that the analysis unit layer may be a hexagonal or
H3 grid rather than an administrative boundary layer.

## Example workflow

``` r

hex_counts <- risk_build_counts(
  points = event_points,
  units = h3_grid,
  unit_id_col = "hex_id"
)
```

``` r

hex_rates <- risk_calc_rate(
  data = hex_counts,
  count_col = "event_count",
  denominator_col = "exposure",
  rate_col = "rate_per_10000"
)
```

``` r

map_risk_choropleth(
  data = hex_rates,
  fill_col = "rate_per_10000",
  map_type = "rate"
)
```

## Choosing an appropriate resolution

H3 and hexagonal workflows require careful resolution selection.

A grid that is too coarse may hide meaningful local variation.

A grid that is too fine may produce:

- many zero-count cells
- unstable rates
- misleading hotspots
- sparse denominators
- noisy maps

Resolution should be selected based on:

- event density
- denominator availability
- intended use
- privacy requirements
- operational interpretability
- computational performance

## Counts, denominators, and exposure

Hexagonal workflows require particular care with denominators.

A meaningful denominator may be based on:

- resident population
- visitor exposure
- asset counts
- network length
- area
- time at risk
- activity participation
- modelled exposure surfaces

The choice of denominator should match the risk question being asked.

## Assumptions

H3 and hex workflows assume:

- the grid resolution is appropriate
- the unit identifier is stable
- denominator data can be meaningfully assigned to grid cells
- sparse cells are interpreted carefully
- the workflow supports the decision context

## Limitations and pitfalls

Potential issues include:

- false precision
- unstable rates in sparse cells
- denominator mismatch
- edge effects
- inappropriate resolution choice
- loss of administrative context
- overinterpretation of fine-scale patterns

Hexagonal outputs should usually be interpreted as exploratory spatial
summaries rather than definitive evidence of localised risk.

## Relationship to areal unit workflows

The purpose of supporting H3 and hexagonal grids is not to replace areal
unit analysis.

Instead, these workflows provide an additional analytical option.

In some cases:

- administrative units are better
- H3 grids are better
- both should be compared
- one may be better for analysis while the other is better for
  communication

The appropriate unit system depends on the question, data, and decision
context.

## Future development

Future versions of `riskworkflowr` may include:

- H3-specific helper functions
- resolution suitability checks
- compact hexagon workflows
- sparse-cell diagnostics
- denominator assignment helpers
- sensitivity testing across resolutions
- comparison tools for areal units versus H3 grids
