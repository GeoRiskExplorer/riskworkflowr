# SMR Analysis

## Overview

Standardised mortality ratio (SMR) style workflows compare observed
event counts against expected event counts.

In practical spatial risk analysis workflows, these approaches may help
identify areas with comparatively elevated or reduced event occurrence
relative to a broader reference population.

Although the terminology originates from mortality analysis, similar
observed-versus-expected approaches may also be applied to:

- injury events
- operational incidents
- environmental hazards
- infrastructure failures
- other spatial event datasets

## Core workflow

The simplified workflow implemented in `riskworkflowr` is:

``` text
observed events
→ expected events
→ observed / expected ratio
→ comparative interpretation
```

The main helper function is:

``` r

risk_calc_smr()
```

## Example data

``` r

data <- data.frame(
  event_count = c(5, 10, 20),
  population = c(1000, 2000, 3000)
)
```

## Calculate SMR

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

## Interpreting SMR values

In general:

``` text
SMR = 1
```

suggests observed events are similar to expected events.

``` text
SMR > 1
```

suggests observed events are higher than expected.

``` text
SMR < 1
```

suggests observed events are lower than expected.

Interpretation should always consider:

- count size
- denominator quality
- spatial scale
- event rarity
- analytical context

## Confidence intervals

`riskworkflowr` supports optional confidence interval outputs for SMR
calculations.

Confidence intervals may help identify areas where observed differences
are more likely to reflect meaningful variation rather than random
fluctuation.

However, confidence intervals alone should not be treated as definitive
evidence of causation or operational significance.

## Why use SMR-style workflows?

SMR-style workflows can be useful because they:

- standardise observed-versus-expected comparisons
- support exploratory comparison across areas
- work with relatively simple inputs
- integrate naturally into spatial workflows
- support choropleth mapping and communication

These workflows are often practical in operational settings where:

- point event data exist
- denominators are available
- detailed covariates are limited
- rapid exploratory analysis is required

## Important limitations

SMR workflows have important limitations.

### Small numbers instability

Small counts or small denominators may produce unstable ratios.

Rare events can generate apparently extreme SMR values that may not
represent meaningful differences.

### Ecological interpretation

SMR outputs describe patterns at the spatial unit level and should not
be interpreted as individual-level risk.

### Denominator assumptions

The quality and appropriateness of the denominator strongly influence
interpretation.

### Coding consistency

Differences in event coding, reporting, or classification practices may
influence outputs.

## Unstratified versus stratified workflows

The current implementation primarily supports unstratified workflows.

This reflects many operational spatial risk datasets where event
locations are available but detailed covariates such as:

- age
- sex
- demographic factors

may be absent or incomplete.

More advanced approaches may include:

- direct standardisation
- indirect standardisation
- age-sex stratified SMR
- hierarchical models
- Bayesian smoothing approaches

These approaches may be preferable where sufficient covariate
information exists.

## Exploratory versus inferential analysis

The workflows implemented in `riskworkflowr` are primarily intended to
support:

- exploratory spatial analysis
- operational review
- comparative spatial profiling
- communication workflows

They should not be interpreted as replacing formal inferential
epidemiological or spatial statistical modelling approaches where those
methods are appropriate.

## Alternative approaches

Depending on the analytical objective, alternative methods may include:

- Poisson regression
- negative binomial regression
- geographically weighted regression
- Bayesian disease mapping
- empirical Bayes smoothing
- cluster detection methods
- spatiotemporal models

The most appropriate method depends on:

- analytical objectives
- data quality
- denominator stability
- covariate availability
- spatial scale
- operational requirements

## Future development

Future development priorities include:

- grouped/category comparative SMR workflows
- distinctive category identification
- improved confidence interval workflows
- low-count stability diagnostics
- optional smoothing approaches
- richer mapping integration
