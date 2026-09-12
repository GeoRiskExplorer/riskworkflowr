#' Calculate Poisson probability of at least k events
#'
#' Calculates the probability of observing at least `k` events under a
#' Poisson distribution:
#'
#' \deqn{P(X \ge k)}
#'
#' where \eqn{X \sim Poisson(\lambda)}.
#'
#' The supplied lambda must already represent the expected mean number of
#' events for the target period of interest. This function does not derive,
#' annualise, rescale, or otherwise transform lambda.
#'
#' @param data A data frame or `sf` object.
#' @param lambda_col Character string giving the name of the numeric column
#'   containing the expected mean event count, lambda, for the target period.
#'   Values must be greater than or equal to zero. Missing values are allowed
#'   and produce missing probabilities.
#' @param k A single whole number greater than or equal to 1. Defines the
#'   minimum event count of interest. For example, `k = 2` calculates
#'   \eqn{P(X \ge 2)}.
#' @param probability_col Character string giving the name of the output
#'   probability column. Defaults to `"poisson_probability"`.
#'
#' @return The input data with one additional numeric probability column.
#'   Probabilities are returned as proportions between 0 and 1. For `sf`
#'   input, geometry and coordinate reference system are preserved.
#'
#' @details
#' The analytical contract is deliberately narrow. This function calculates
#' only the Poisson exceedance probability \eqn{P(X \ge k)}.
#'
#' Lambda and `k` must refer to the same target period. For example, if
#' `lambda_col` contains expected annual event counts, the returned probability
#' is the probability of observing at least `k` events within one year.
#'
#' Use [risk_annualise()] upstream when observed counts need to be converted
#' to an expected annual event frequency before calculating probability.
#'
#' @examples
#' risk_data <- data.frame(
#'   area = c("A", "B", "C"),
#'   lambda_annual = c(0.5, 1, 2)
#' )
#'
#' risk_poisson_probability(
#'   risk_data,
#'   lambda_col = "lambda_annual",
#'   k = 2,
#'   probability_col = "prob_ge_2"
#' )
#'
#' @export
risk_poisson_probability <- function(
  data,
  lambda_col,
  k = 1,
  probability_col = "poisson_probability"
) {

  .risk_validate_poisson_col_name(lambda_col, "lambda_col")
  .risk_validate_poisson_col_name(probability_col, "probability_col")

  if (!lambda_col %in% names(data)) {
    stop(
      "`lambda_col` not found in data.",
      call. = FALSE
    )
  }

  if (probability_col %in% names(data)) {
    stop(
      "`probability_col` already exists in data.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(k) ||
    length(k) != 1L ||
    is.na(k) ||
    !is.finite(k) ||
    k < 1 ||
    k != floor(k)
  ) {
    stop(
      "`k` must be a single whole number greater than or equal to 1.",
      call. = FALSE
    )
  }

  lambda <- data[[lambda_col]]

  if (!is.numeric(lambda)) {
    stop(
      "`lambda_col` must be numeric.",
      call. = FALSE
    )
  }

  if (any(!is.na(lambda) & !is.finite(lambda))) {
    stop(
      "`lambda_col` must contain only finite values or NA.",
      call. = FALSE
    )
  }

  if (any(!is.na(lambda) & lambda < 0)) {
    stop(
      "`lambda_col` must contain values greater than or equal to zero.",
      call. = FALSE
    )
  }

  data[[probability_col]] <- stats::ppois(
    q = k - 1,
    lambda = lambda,
    lower.tail = FALSE
  )

  data
}


.risk_validate_poisson_col_name <- function(x, arg) {

  if (
    !is.character(x) ||
    length(x) != 1L ||
    is.na(x) ||
    !nzchar(x)
  ) {
    stop(
      paste0("`", arg, "` must be a single non-empty column name."),
      call. = FALSE
    )
  }

  invisible(TRUE)
}