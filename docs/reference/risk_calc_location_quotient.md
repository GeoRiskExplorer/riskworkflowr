# Calculate a location quotient / distinctive risk ratio

Calculates a location quotient as the local rate divided by the
reference rate. This follows the general approach used by Boscoe and
Pradhan (2015), who mapped the most distinctive cause of death by
comparing state-specific age-adjusted mortality rates with national
age-adjusted mortality rates.

## Usage

``` r
risk_calc_location_quotient(
  data,
  observed_col = "event_count",
  denominator_col,
  reference_rate = NULL,
  lq_col = "location_quotient",
  local_rate_col = "local_rate",
  min_count = 0
)
```

## Arguments

- data:

  A data frame or sf object.

- observed_col:

  Column containing observed counts.

- denominator_col:

  Column containing population, exposure, or denominator.

- reference_rate:

  Optional fixed reference rate. If NULL, the reference rate is
  calculated from all rows.

- lq_col:

  Name of output location quotient column.

- local_rate_col:

  Name of local rate output column.

- min_count:

  Minimum observed count required before calculating LQ.

## Value

Input data with local rate and location quotient columns.

## References

Boscoe FP, Pradhan E. The Most Distinctive Causes of Death by State,
2001–2010. Preventing Chronic Disease. 2015;12:E75.
doi:10.5888/pcd12.140395.

## Examples

``` r
data <- data.frame(
  event_count = c(5, 10, 20),
  population = c(1000, 2000, 3000)
)

risk_calc_location_quotient(
  data = data,
  observed_col = "event_count",
  denominator_col = "population"
)
#>   event_count population  local_rate location_quotient
#> 1           5       1000 0.005000000         0.8571429
#> 2          10       2000 0.005000000         0.8571429
#> 3          20       3000 0.006666667         1.1428571
```
