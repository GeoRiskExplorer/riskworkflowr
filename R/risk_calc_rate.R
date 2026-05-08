# 1 ------------------------------------------------------------------------
# risk_calc_rate.R
# Purpose: Calculate event/risk rate using count and denominator columns

#' Calculate event rate
#'
#' Calculates a simple event rate using an observed count column and denominator
#' column, with a user-defined multiplier.
#'
#' For example, with `multiplier = 10000`, the function calculates events per
#' 10,000 population or exposure units.
#'
#' @param data A data frame or sf object.
#' @param count_col Name of the observed count column.
#' @param denominator_col Name of the denominator, population, or exposure column.
#' @param rate_col Name of the output rate column.
#' @param multiplier Rate multiplier. Defaults are user-controlled, such as
#'   100, 1,000, 10,000, or 100,000.
#' @param zero_denominator_value Value returned when denominator is missing,
#'   zero, or negative.
#'
#' @return Input data with an added rate column.
#'
#' @examples
#' data <- data.frame(
#'   event_count = c(5, 10, 15),
#'   population = c(1000, 2000, 3000)
#' )
#'
#' risk_calc_rate(
#'   data = data,
#'   count_col = "event_count",
#'   denominator_col = "population"
#' )
#' 
#' @export

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