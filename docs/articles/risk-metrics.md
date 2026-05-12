# Risk Metrics

## Overview

`riskworkflowr` includes simple helper functions for calculating
commonly used spatial risk metrics.

The current metric functions include:

``` text
risk_calc_rate()
risk_calc_poisson_probability()
risk_calc_smr()
risk_calc_location_quotient()
```

These functions are intended to support transparent and reproducible
analytical workflows rather than replace specialist epidemiological or
spatial statistical methods.

## Example data

``` r

data <- data.frame(
  event_count = c(5, 10, 20),
  population = c(1000, 2000, 3000)
)
```

## Rates

[`risk_calc_rate()`](https://GeoRiskExplorer.github.io/riskworkflowr/reference/risk_calc_rate.md)
calculates a simple rate:

``` text
observed events / denominator × multiplier
```

``` r

risk_calc_rate(
  data = data,
  count_col = "event_count",
  denominator_col = "population"
)
```

    ##   event_count population rate_per_10000
    ## 1           5       1000       50.00000
    ## 2          10       2000       50.00000
    ## 3          20       3000       66.66667

## Poisson probability

[`risk_calc_poisson_probability()`](https://GeoRiskExplorer.github.io/riskworkflowr/reference/risk_calc_poisson_probability.md)
estimates the probability of one or more events occurring over a defined
period using an observed event frequency.

``` r

poisson_data <- data.frame(
  event_count = c(1, 5, 10),
  years = c(1, 2, 5)
)

risk_calc_poisson_probability(
  data = poisson_data,
  count_col = "event_count",
  period_col = "years",
  output = "both"
)
```

    ##   event_count years lambda prob_event_ge_1 prob_event_ge_1_pct
    ## 1           1     1    1.0       0.6321206            63.21206
    ## 2           5     2    2.5       0.9179150            91.79150
    ## 3          10     5    2.0       0.8646647            86.46647

## Standardised mortality ratio style workflows

[`risk_calc_smr()`](https://GeoRiskExplorer.github.io/riskworkflowr/reference/risk_calc_smr.md)
calculates expected counts and standardised mortality ratio style
outputs.

Although named using SMR terminology, the same general structure may be
useful for other observed-versus-expected spatial risk workflows where
an appropriate denominator is available.

``` r

risk_calc_smr(
  data = data,
  observed_col = "event_count",
  denominator_col = "population"
)
```

    ##   event_count population expected_count       smr smr_lower smr_upper
    ## 1           5       1000       5.833333 0.8571429 0.2783120  2.000285
    ## 2          10       2000      11.666667 0.8571429 0.4110333  1.576316
    ## 3          20       3000      17.500000 1.1428571 0.6980868  1.765050
    ##             smr_ci_flag
    ## 1 not_clearly_different
    ## 2 not_clearly_different
    ## 3 not_clearly_different

## Distinctive category workflows

[`risk_distinct_category()`](https://GeoRiskExplorer.github.io/riskworkflowr/reference/risk_distinct_category.md)
supports grouped/category comparative risk profiling.

It is intended for datasets where events are grouped by both spatial
unit and category, such as injury mechanism, incident type, hazard
class, or activity type.

``` r

category_data <- data.frame(
  unit_id = c("A", "A", "B", "B"),
  category = c("Falls", "Water", "Falls", "Water"),
  event_count = c(10, 2, 3, 8),
  population = c(1000, 1000, 800, 800)
)

risk_distinct_category(
  data = category_data,
  unit_id_col = "unit_id",
  category_col = "category",
  observed_col = "event_count",
  denominator_col = "population",
  min_count = 1
)
```

    ##   unit_id highest_category highest_smr highest_event_count lowest_category
    ## 1       A            Falls    1.384615                  10           Water
    ## 2       B            Water    1.800000                   8           Falls
    ##   lowest_smr lowest_event_count category_count_used low_count_flag
    ## 1  0.3600000                  2                   2          FALSE
    ## 2  0.5192308                  3                   2          FALSE

The output identifies the highest comparative category for each unit
and, optionally, the lowest comparative category.

This workflow is intended for exploratory spatial risk profiling and
should be interpreted carefully where counts are low, denominators are
unstable, or categories are inconsistently coded.

## Assumptions

Risk metric workflows assume:

- event counts are suitable for the intended analysis
- denominators are appropriate and interpretable
- spatial units are meaningful for the question being asked
- low counts and small denominators are interpreted carefully
- outputs are reviewed alongside context and data quality

## Limitations and pitfalls

Potential limitations include:

- unstable rates in small areas
- misleading interpretation of rare events
- denominator uncertainty
- inconsistent event coding
- ecological interpretation risks
- overinterpretation of exploratory outputs

## Alternative approaches

Depending on the analytical objective and available data, alternative
methods may include:

- direct standardisation
- indirect standardisation
- age-sex stratified SMR
- Bayesian smoothing
- empirical Bayes methods
- Poisson or negative binomial regression
- spatial regression
- cluster detection methods
- hierarchical models

These methods may be preferable where sufficient covariate information,
stable denominators, or specific inferential objectives exist.

## Future development

Future development priorities include:

- grouped/category SMR workflows
- clearer distinction between SMR and location quotient style
  calculations
- category dominance outputs for mapping
- optional low-count thresholds
- clearer interpretation flags
- enhanced examples and visual outputs
