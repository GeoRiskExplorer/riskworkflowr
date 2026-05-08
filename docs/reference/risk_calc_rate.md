# Calculate event rate

Calculates a simple event rate using an observed count column and
denominator column, with a user-defined multiplier.

## Usage

``` r
risk_calc_rate(
  data,
  count_col,
  denominator_col,
  rate_col = "rate_per_10000",
  multiplier = 10000,
  zero_denominator_value = NA_real_
)
```

## Arguments

- data:

  A data frame or sf object.

- count_col:

  Name of the observed count column.

- denominator_col:

  Name of the denominator, population, or exposure column.

- rate_col:

  Name of the output rate column.

- multiplier:

  Rate multiplier. Defaults are user-controlled, such as 100, 1,000,
  10,000, or 100,000.

- zero_denominator_value:

  Value returned when denominator is missing, zero, or negative.

## Value

Input data with an added rate column.

## Details

For example, with `multiplier = 10000`, the function calculates events
per 10,000 population or exposure units.

## Examples

``` r
data <- data.frame(
  event_count = c(5, 10, 15),
  population = c(1000, 2000, 3000)
)

risk_calc_rate(
  data = data,
  count_col = "event_count",
  denominator_col = "population"
)
#>   event_count population rate_per_10000
#> 1           5       1000             50
#> 2          10       2000             50
#> 3          15       3000             50
```
