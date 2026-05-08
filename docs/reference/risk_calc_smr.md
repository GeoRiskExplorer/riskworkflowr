# Calculate Standardised Mortality Ratio (SMR)

Calculates expected counts and Standardised Mortality Ratios (SMR) using
a global reference rate derived from the input dataset.

## Usage

``` r
risk_calc_smr(
  data,
  observed_col = "event_count",
  denominator_col,
  expected_col = "expected_count",
  smr_col = "smr",
  global_rate_col = NULL,
  ci_method = c("exact", "none"),
  conf_level = 0.95,
  smr_lower_col = "smr_lower",
  smr_upper_col = "smr_upper",
  smr_ci_flag_col = "smr_ci_flag",
  zero_expected_value = NA_real_
)
```

## Arguments

- data:

  A data frame or sf object.

- observed_col:

  Name of observed event count column.

- denominator_col:

  Name of denominator, population, or exposure column.

- expected_col:

  Name of expected count output column.

- smr_col:

  Name of SMR output column.

- global_rate_col:

  Name of global reference rate output column.

- ci_method:

  Confidence interval method. Currently supports `"exact"`.

- conf_level:

  Confidence level for intervals.

- smr_lower_col:

  Name of lower confidence interval column.

- smr_upper_col:

  Name of upper confidence interval column.

- smr_ci_flag_col:

  Name of SMR interpretation/classification column.

- zero_expected_value:

  Value returned when expected count is missing, zero, or negative.

## Value

Input data with expected counts, SMR values, and optional confidence
intervals added.

## Details

Optional confidence intervals are calculated using Poisson-based
methods, appropriate for rare-event and small-area analysis.

## References

Poisson confidence interval approaches commonly used in epidemiological
small-area and rare-event analysis.

\#' @examples data \<- data.frame( event_count = c(5, 10, 20),
population = c(1000, 2000, 3000) )

risk_calc_smr( data = data, observed_col = "event_count",
denominator_col = "population" )
