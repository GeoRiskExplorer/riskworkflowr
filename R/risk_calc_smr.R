# 1 ------------------------------------------------------------------------
# risk_calc_smr.R
# Purpose: Calculate SMR / relative risk with optional exact Poisson CI

#' Calculate Standardised Mortality Ratio (SMR)
#'
#' Calculates expected counts and Standardised Mortality Ratios (SMR) using a
#' global reference rate derived from the input dataset.
#'
#' Optional confidence intervals are calculated using Poisson-based methods,
#' appropriate for rare-event and small-area analysis.
#'
#' @param data A data frame or sf object.
#' @param observed_col Name of observed event count column.
#' @param denominator_col Name of denominator, population, or exposure column.
#' @param expected_col Name of expected count output column.
#' @param smr_col Name of SMR output column.
#' @param global_rate_col Name of global reference rate output column.
#' @param ci_method Confidence interval method. Currently supports `"exact"`.
#' @param conf_level Confidence level for intervals.
#' @param smr_lower_col Name of lower confidence interval column.
#' @param smr_upper_col Name of upper confidence interval column.
#' @param smr_ci_flag_col Name of SMR interpretation/classification column.
#' @param zero_expected_value Value returned when expected count is missing,
#'   zero, or negative.
#'
#' @return Input data with expected counts, SMR values, and optional confidence
#' intervals added.
#'
#' @references
#' Poisson confidence interval approaches commonly used in epidemiological
#' small-area and rare-event analysis.
#'
#' @export

risk_calc_smr <- function(
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
) {

  ci_method <- match.arg(ci_method)

  if (!observed_col %in% names(data)) {
    stop("`observed_col` not found in data.", call. = FALSE)
  }

  if (!denominator_col %in% names(data)) {
    stop("`denominator_col` not found in data.", call. = FALSE)
  }

  if (conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be between 0 and 1.", call. = FALSE)
  }

  total_observed <- sum(data[[observed_col]], na.rm = TRUE)
  total_denominator <- sum(data[[denominator_col]], na.rm = TRUE)

  if (total_denominator <= 0) {
    stop("Total denominator must be greater than zero.", call. = FALSE)
  }

  global_rate <- total_observed / total_denominator

  out <- data |>
    dplyr::mutate(
      "{expected_col}" := .data[[denominator_col]] * global_rate,
      "{smr_col}" := dplyr::if_else(
        is.na(.data[[expected_col]]) | .data[[expected_col]] <= 0,
        zero_expected_value,
        .data[[observed_col]] / .data[[expected_col]]
      )
    )

  if (!is.null(global_rate_col)) {
    out <- out |>
      dplyr::mutate(
        "{global_rate_col}" := global_rate
      )
  }

  if (ci_method == "exact") {

    alpha <- 1 - conf_level

    out <- out |>
      dplyr::mutate(
        .observed_tmp = .data[[observed_col]],
        .expected_tmp = .data[[expected_col]],

        .lower_observed_tmp = dplyr::if_else(
          .observed_tmp == 0,
          0,
          stats::qchisq(alpha / 2, 2 * .observed_tmp) / 2
        ),

        .upper_observed_tmp = stats::qchisq(
          1 - alpha / 2,
          2 * (.observed_tmp + 1)
        ) / 2,

        "{smr_lower_col}" := dplyr::if_else(
          is.na(.expected_tmp) | .expected_tmp <= 0,
          NA_real_,
          .lower_observed_tmp / .expected_tmp
        ),

        "{smr_upper_col}" := dplyr::if_else(
          is.na(.expected_tmp) | .expected_tmp <= 0,
          NA_real_,
          .upper_observed_tmp / .expected_tmp
        ),

        "{smr_ci_flag_col}" := dplyr::case_when(
          is.na(.data[[smr_col]]) ~ "missing",
          .data[[smr_lower_col]] > 1 ~ "above_expected",
          .data[[smr_upper_col]] < 1 ~ "below_expected",
          TRUE ~ "not_clearly_different"
        )
      ) |>
      dplyr::select(
        -dplyr::all_of(
          c(
            ".observed_tmp",
            ".expected_tmp",
            ".lower_observed_tmp",
            ".upper_observed_tmp"
          )
        )
      )
  }

  out
}