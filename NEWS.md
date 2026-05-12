# riskworkflowr news

## riskworkflowr 0.0.0.9000

### New features

- Added `risk_distinct_category()` for grouped/category comparative spatial risk profiling.
- Added support for identifying highest and lowest distinctive categories by spatial unit.
- Added configurable minimum count thresholds for category eligibility.
- Added category-oriented outputs supporting exploratory choropleth workflows.

### Documentation

- Added pkgdown documentation website.
- Added workflow vignettes covering:
  - spatial assignment
  - aggregation workflows
  - risk metrics
  - SMR analysis
  - H3 workflows
  - choropleth mapping
  - package philosophy and assumptions

### Changes

- Removed `risk_calc_location_quotient()`.
- Replaced location quotient style workflows with grouped/category comparative workflows via `risk_distinct_category()`.
- Improved comparative workflow terminology and documentation.