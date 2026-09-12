# 1 ------------------------------------------------------------------------
# risk_calc_rate.R
# Purpose: Calculate event/risk rate using count and denominator columns


#' Calculate event rate
#'
#' Calculates an event rate from an observed count and a denominator, using a
#' user-defined multiplier.
#'
#' For example, with `multiplier = 10000`, the function calculates events per
#' 10,000 population or exposure units.
#'
#' Missing or zero denominators return `zero_denominator_value`. Negative
#' denominators are treated as invalid input.
#'
#' @param data A data frame or sf object.
#' @param count_col Name of the observed count column.
#' @param denominator_col Name of the denominator, population, or exposure
#'   column.
#' @param rate_col Name of the output rate column.
#' @param multiplier Positive numeric rate multiplier. For example, `100`,
#'   `1000`, `10000`, or `100000`.
#' @param zero_denominator_value Numeric value returned when the denominator is
#'   missing or zero. Defaults to `NA_real_`.
#'
#' @return The input data with an added rate column.
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

  # 1. Validate column names -------------------------------------------------

  for (x in list(
    count_col = count_col,
    denominator_col = denominator_col,
    rate_col = rate_col
  )) {
    if (
      length(x) != 1L ||
      is.na(x) ||
      !is.character(x) ||
      !nzchar(x)
    ) {
      stop(
        "Column names must be single, non-empty character values.",
        call. = FALSE
      )
    }
  }

  if (!count_col %in% names(data)) {
    stop("`count_col` not found in data.", call. = FALSE)
  }

  if (!denominator_col %in% names(data)) {
    stop("`denominator_col` not found in data.", call. = FALSE)
  }

  if (rate_col %in% names(data)) {
    stop(
      "`rate_col` already exists in data.",
      call. = FALSE
    )
  }


  # 2. Validate multiplier ---------------------------------------------------

  if (
    length(multiplier) != 1L ||
    !is.numeric(multiplier) ||
    is.na(multiplier) ||
    !is.finite(multiplier) ||
    multiplier <= 0
  ) {
    stop(
      "`multiplier` must be one finite positive numeric value.",
      call. = FALSE
    )
  }


  # 3. Validate fallback value -----------------------------------------------

  if (
    length(zero_denominator_value) != 1L ||
    !is.numeric(zero_denominator_value) ||
    (
      !is.na(zero_denominator_value) &&
        !is.finite(zero_denominator_value)
    )
  ) {
    stop(
      "`zero_denominator_value` must be one finite numeric value or NA.",
      call. = FALSE
    )
  }


  # 4. Validate analytical inputs -------------------------------------------

  count <- data[[count_col]]
  denominator <- data[[denominator_col]]

  if (!is.numeric(count)) {
    stop(
      "`count_col` must contain numeric values.",
      call. = FALSE
    )
  }

  if (!is.numeric(denominator)) {
    stop(
      "`denominator_col` must contain numeric values.",
      call. = FALSE
    )
  }

  if (any(!is.finite(count) & !is.na(count))) {
    stop(
      "`count_col` must contain finite values or NA.",
      call. = FALSE
    )
  }

  if (any(!is.finite(denominator) & !is.na(denominator))) {
    stop(
      "`denominator_col` must contain finite values or NA.",
      call. = FALSE
    )
  }

  if (any(count < 0, na.rm = TRUE)) {
    stop(
      "`count_col` cannot contain negative values.",
      call. = FALSE
    )
  }

  if (any(denominator < 0, na.rm = TRUE)) {
    stop(
      "`denominator_col` cannot contain negative values.",
      call. = FALSE
    )
  }


# 5. Calculate rate --------------------------------------------------------

out <- data

rate <- rep(NA_real_, length(denominator))

valid_denominator <-
  !is.na(denominator) &
  denominator > 0

rate[valid_denominator] <-
  (count[valid_denominator] /
     denominator[valid_denominator]) *
  multiplier

invalid_denominator <-
  is.na(denominator) |
  denominator == 0

rate[invalid_denominator] <-
  zero_denominator_value

out[[rate_col]] <- rate

out
  
}