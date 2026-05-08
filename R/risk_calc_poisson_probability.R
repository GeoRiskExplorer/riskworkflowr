#' Calculate Poisson probability of one or more events
#'
#' Calculates the Poisson probability of observing one or more events based on
#' observed event counts and an analysis period.
#'
#' This implementation uses observed historical event frequency to derive a
#' Poisson lambda value:
#'
#' \deqn{
#' P(X \geq 1) = 1 - e^{-\lambda}
#' }
#'
#' where \eqn{\lambda} is the average event count for the target period.
#'
#' @param data A data frame or sf object.
#' @param count_col Name of the observed count column.
#' @param period_col Optional column containing analysis periods.
#' @param period_value Fixed analysis period used when `period_col` is NULL.
#' @param lambda_col Name of the output lambda column.
#' @param probability_col Name of the output probability proportion column.
#' @param probability_pct_col Name of the output probability percent column.
#' @param output One of `"proportion"`, `"percent"`, or `"both"`.
#'
#' @return Input data with Poisson probability outputs added.
#'
#' @references
#' Standard Poisson probability relationships commonly used in epidemiological
#' and event-frequency modelling.
#' 
#' #' @examples
#' data <- data.frame(
#'   event_count = c(1, 5, 10),
#'   years = c(1, 2, 5)
#' )
#'
#' risk_calc_poisson_probability(
#'   data = data,
#'   count_col = "event_count",
#'   period_col = "years",
#'   output = "both"
#' )
#'
#' @export
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
      stop("`period_value` must be a single number greater than zero.", call. = FALSE)
    }

    out <- data |>
      dplyr::mutate(
        "{lambda_col}" := .data[[count_col]] / period_value
      )
  }

  out <- out |>
    dplyr::mutate(
      "{probability_col}" := 1 - exp(-.data[[lambda_col]])
    )

  if (output == "percent") {
    out <- out |>
      dplyr::mutate(
        "{probability_pct_col}" := .data[[probability_col]] * 100
      ) |>
      dplyr::select(
        -dplyr::all_of(probability_col)
      )
  }

  if (output == "both") {
    out <- out |>
      dplyr::mutate(
        "{probability_pct_col}" := .data[[probability_col]] * 100
      )
  }

  out
}