# Spatial Assignment Workflows

## Overview

Spatial assignment is the process of linking point-based events to
spatial analysis units.

In `riskworkflowr`, this usually means assigning point events to:

- administrative boundaries
- custom polygons
- operational areas
- H3 or hexagonal grids

This step is important because all downstream counts, rates, SMRs, and
maps depend on the assignment logic used.

## Why spatial assignment matters

Spatial joins can appear simple, but different assignment choices can
produce different analytical outputs.

For example:

- a point may fall inside a polygon
- a point may fall just outside a polygon due to boundary or geocoding
  uncertainty
- a point may need to be assigned to the nearest unit
- a point may require an auditable fallback rule

`riskworkflowr` supports transparent assignment logic so that analysts
can understand how events were allocated.

## Supported join modes

The main spatial assignment function is:

``` r

sj_join()
```

It supports three join modes:

``` text
intersect
nearest
intersect_nearest
```

## Intersect assignment

Intersect assignment links each point to the polygon it falls within.

This is usually appropriate when:

- point locations are accurate
- polygon boundaries are meaningful
- events should only be assigned if they fall inside a unit

``` r

sj_join(
  points = event_points,
  polygons = analysis_units,
  id_col = "unit_id",
  join = "intersect"
)
```

## Nearest assignment

Nearest assignment links each point to the closest polygon.

This may be useful when:

- points are near but not inside a polygon
- boundary precision is imperfect
- geocoding uncertainty exists
- every event must be assigned to a unit

``` r

sj_join(
  points = event_points,
  polygons = analysis_units,
  id_col = "unit_id",
  join = "nearest"
)
```

## Intersect-nearest assignment

The intersect-nearest workflow first attempts an intersect join and then
uses nearest assignment as a fallback.

This is often useful in operational spatial risk workflows because it
provides a practical balance between strict spatial containment and
complete event allocation.

``` r

sj_join(
  points = event_points,
  polygons = analysis_units,
  id_col = "unit_id",
  join = "intersect_nearest"
)
```

## Join auditability

Spatial assignment should be auditable.

Where applicable, `riskworkflowr` records fields such as:

``` text
join_method
join_distance_m
```

These fields help distinguish events assigned by direct intersection
from events assigned using a nearest fallback.

This is important for review, QA, and communication.

## Assumptions

Spatial assignment assumes that:

- the point geometry is suitable for analysis
- the analysis unit geometry is valid for the processing context
- the coordinate reference system is appropriate for distance
  calculations
- the selected join rule matches the analytical question

## Limitations and pitfalls

Spatial assignment may be affected by:

- geocoding uncertainty
- boundary misalignment
- invalid or complex geometry
- points located near polygon edges
- use of geographic CRS for distance calculations
- inappropriate fallback assignment rules

Nearest assignment should be used carefully where distance or boundary
meaning is important.

## Alternative approaches

Other valid approaches may include:

- manual review of uncertain points
- distance thresholds
- buffer-based assignment
- probabilistic assignment
- hierarchical assignment rules
- exclusion of uncertain locations
- sensitivity testing across multiple assignment methods

The appropriate method depends on the data, spatial scale, uncertainty,
and decision context.

## Future development

Future versions of `riskworkflowr` may include additional support for:

- distance thresholds for nearest joins
- clearer QA summaries of fallback assignments
- assignment uncertainty flags
- sensitivity testing across join modes
- richer diagnostics for boundary and CRS issues
