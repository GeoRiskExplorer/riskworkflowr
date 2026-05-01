# 1 ------------------------------------------------------------------------
# risk_calc_rate.R
# Purpose: Calculate event/risk rate using count and denominator columns

risk_calc_rate <- function(
  data,
  count_col,
  denominator_col,
  rate_col = "rate_per_10000",
  multiplier = 10000,
  zero_denominator_value = NA_real_
) {

  if (!count_col %in% names(data)) {
    stop("`count_col` not found in data.", call. = FALSE)
  }

  if (!denominator_col %in% names(data)) {
    stop("`denominator_col` not found in data.", call. = FALSE)
  }

  data |>
    dplyr::mutate(
      "{rate_col}" := dplyr::if_else(
        is.na(.data[[denominator_col]]) | .data[[denominator_col]] <= 0,
        zero_denominator_value,
        (.data[[count_col]] / .data[[denominator_col]]) * multiplier
      )
    )
}