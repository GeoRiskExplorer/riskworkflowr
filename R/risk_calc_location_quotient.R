#' Calculate a location quotient / distinctive risk ratio
#'
#' Calculates a location quotient as the local rate divided by the reference
#' rate. This follows the general approach used by Boscoe and Pradhan (2015),
#' who mapped the most distinctive cause of death by comparing state-specific
#' age-adjusted mortality rates with national age-adjusted mortality rates.
#'
#' @param data A data frame or sf object.
#' @param observed_col Column containing observed counts.
#' @param denominator_col Column containing population, exposure, or denominator.
#' @param reference_rate Optional fixed reference rate. If NULL, the reference
#'   rate is calculated from all rows.
#' @param lq_col Name of output location quotient column.
#' @param local_rate_col Name of local rate output column.
#' @param min_count Minimum observed count required before calculating LQ.
#'
#' @return Input data with local rate and location quotient columns.
#'
#' @references
#' Boscoe FP, Pradhan E. The Most Distinctive Causes of Death by State,
#' 2001–2010. Preventing Chronic Disease. 2015;12:E75.
#' doi:10.5888/pcd12.140395.
#'
#' @export
risk_calc_location_quotient <- function(
  data,
  observed_col = "event_count",
  denominator_col,
  reference_rate = NULL,
  lq_col = "location_quotient",
  local_rate_col = "local_rate",
  min_count = 0
) {

  if (!observed_col %in% names(data)) {
    stop("`observed_col` not found in data.", call. = FALSE)
  }

  if (!denominator_col %in% names(data)) {
    stop("`denominator_col` not found in data.", call. = FALSE)
  }

  if (is.null(reference_rate)) {
    total_observed <- sum(data[[observed_col]], na.rm = TRUE)
    total_denominator <- sum(data[[denominator_col]], na.rm = TRUE)

    if (total_denominator <= 0) {
      stop("Total denominator must be greater than zero.", call. = FALSE)
    }

    reference_rate <- total_observed / total_denominator
  }

  data |>
    dplyr::mutate(
      "{local_rate_col}" := dplyr::if_else(
        is.na(.data[[denominator_col]]) | .data[[denominator_col]] <= 0,
        NA_real_,
        .data[[observed_col]] / .data[[denominator_col]]
      ),
      "{lq_col}" := dplyr::case_when(
        is.na(.data[[local_rate_col]]) ~ NA_real_,
        .data[[observed_col]] < min_count ~ NA_real_,
        reference_rate <= 0 ~ NA_real_,
        TRUE ~ .data[[local_rate_col]] / reference_rate
      )
    )
}