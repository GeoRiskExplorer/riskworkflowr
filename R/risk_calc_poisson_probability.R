risk_calc_poisson_probability <- function(
  data,
  count_col = "event_count",
  period_col = NULL,
  period_value = 1,
  lambda_col = "lambda",
  probability_col = "prob_event_ge_1",
  probability_pct_col = "prob_event_ge_1_pct",
  output = c("proportion", "percent", "both")
) {

  output <- match.arg(output)

  if (!count_col %in% names(data)) {
    stop("`count_col` not found in data.", call. = FALSE)
  }

  if (!is.null(period_col) && !period_col %in% names(data)) {
    stop("`period_col` not found in data.", call. = FALSE)
  }

  if (!is.null(period_col)) {
    out <- data |>
      dplyr::mutate(
        "{lambda_col}" := dplyr::if_else(
          is.na(.data[[period_col]]) | .data[[period_col]] <= 0,
          NA_real_,
          .data[[count_col]] / .data[[period_col]]
        )
      )
  } else {

    if (length(period_value) != 1 || is.na(period_value) || period_value <= 0) {
      stop("`period_value` must be > 0.", call. = FALSE)
    }

    out <- data |>
      dplyr::mutate(
        "{lambda_col}" := .data[[count_col]] / period_value
      )
  }

  # probability (always calculated internally)
  out <- out |>
    dplyr::mutate(
      "{probability_col}" := 1 - exp(-.data[[lambda_col]])
    )

  # output control
  if (output == "percent") {
    out <- out |>
      dplyr::mutate(
        "{probability_pct_col}" := .data[[probability_col]] * 100
      ) |>
      dplyr::select(-all_of(probability_col))
  }

  if (output == "both") {
    out <- out |>
      dplyr::mutate(
        "{probability_pct_col}" := .data[[probability_col]] * 100
      )
  }

  out
}