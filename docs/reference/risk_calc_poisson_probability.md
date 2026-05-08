# Calculate Poisson probability of one or more events

Calculates the Poisson probability of observing one or more events based
on observed event counts and an analysis period.

## Usage

``` r
risk_calc_poisson_probability(
  data,
  count_col = "event_count",
  period_col = NULL,
  period_value = 1,
  lambda_col = "lambda",
  probability_col = "prob_event_ge_1",
  probability_pct_col = "prob_event_ge_1_pct",
  output = c("proportion", "percent", "both")
)
```

## Arguments

- data:

  A data frame or sf object.

- count_col:

  Name of the observed count column.

- period_col:

  Optional column containing analysis periods.

- period_value:

  Fixed analysis period used when `period_col` is NULL.

- lambda_col:

  Name of the output lambda column.

- probability_col:

  Name of the output probability proportion column.

- probability_pct_col:

  Name of the output probability percent column.

- output:

  One of `"proportion"`, `"percent"`, or `"both"`.

## Value

Input data with Poisson probability outputs added.

## Details

This implementation uses observed historical event frequency to derive a
Poisson lambda value:

\$\$ P(X \geq 1) = 1 - e^{-\lambda} \$\$

where \\\lambda\\ is the average event count for the target period.

## References

Standard Poisson probability relationships commonly used in
epidemiological and event-frequency modelling.

\#' @examples data \<- data.frame( event_count = c(1, 5, 10), years =
c(1, 2, 5) )

risk_calc_poisson_probability( data = data, count_col = "event_count",
period_col = "years", output = "both" )
