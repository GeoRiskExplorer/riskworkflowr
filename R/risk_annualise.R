# 1 ------------------------------------------------------------------------
# risk_annualise.R
# Purpose: Annualise observed event counts over an explicit observation period


# 1.1 ----------------------------------------------------------------------
# Internal helper: validate a single column name

.risk_validate_col_name <- function(x, arg) {

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


# 1.2 ----------------------------------------------------------------------
# Public function

#' Annualise observed event counts
#'
#' Standardises observed event counts to an annual expected frequency using an
#' explicit observation period.
#'
#' The observation period can be supplied as:
#'
#' - a fixed number of years using `period_value`;
#' - a numeric column of years using `period_col`;
#' - a fixed start and end date; or
#' - row-specific start and end date columns.
#'
#' Date-based periods are calculated as inclusive elapsed calendar days divided
#' by `year_length`. The default year length is 365.2425 days.
#'
#' The function performs no grouping or aggregation. It is intended to operate
#' on already-prepared analytical units and can be piped into downstream risk
#' functions such as Poisson probability calculations.
#'
#' @param data A data frame, tibble, or sf object.
#' @param count_col Name of the observed event count column.
#' @param period_col Optional numeric column containing observation periods in
#'   years.
#' @param period_value Optional fixed observation period in years.
#' @param start_col Optional column containing observation start dates.
#' @param end_col Optional column containing observation end dates.
#' @param start_date Optional fixed observation start date.
#' @param end_date Optional fixed observation end date.
#' @param annualised_col Name of the output annualised count column.
#' @param period_years_col Optional output column containing the calculated
#'   observation period in years.
#' @param year_length Number of days used to represent one year. Defaults to
#'   365.2425.
#'
#' @return The input object with an annualised count column and, optionally,
#'   the calculated observation period in years.
#'
#' @examples
#' x <- data.frame(
#'   event_count = c(0, 5, 10)
#' )
#'
#' risk_annualise(
#'   x,
#'   count_col = "event_count",
#'   period_value = 5
#' )
#'
#' risk_annualise(
#'   x,
#'   count_col = "event_count",
#'   start_date = as.Date("2021-07-01"),
#'   end_date = as.Date("2026-06-30")
#' )
#'
#' @export

risk_annualise <- function(
  data,
  count_col = "event_count",
  period_col = NULL,
  period_value = NULL,
  start_col = NULL,
  end_col = NULL,
  start_date = NULL,
  end_date = NULL,
  annualised_col = "annualised_count",
  period_years_col = NULL,
  year_length = 365.2425
) {

  # 2 ------------------------------------------------------------------------
  # Validate input object

  if (!inherits(data, "data.frame")) {
    stop(
      "`data` must be a data.frame, tibble, or sf object.",
      call. = FALSE
    )
  }

  .risk_validate_col_name(
    count_col,
    "count_col"
  )

  if (!count_col %in% names(data)) {
    stop(
      "`count_col` not found in data.",
      call. = FALSE
    )
  }

  count <- data[[count_col]]

  if (!is.numeric(count)) {
    stop(
      "`count_col` must be numeric.",
      call. = FALSE
    )
  }

  if (any(!is.finite(count) & !is.na(count))) {
    stop(
      "`count_col` cannot contain infinite values.",
      call. = FALSE
    )
  }

  if (any(count < 0, na.rm = TRUE)) {
    stop(
      "`count_col` cannot contain negative values.",
      call. = FALSE
    )
  }


  # 3 ------------------------------------------------------------------------
  # Validate output columns

  .risk_validate_col_name(
    annualised_col,
    "annualised_col"
  )

  if (annualised_col %in% names(data)) {
    stop(
      paste0(
        "Output column `",
        annualised_col,
        "` already exists."
      ),
      call. = FALSE
    )
  }

  if (!is.null(period_years_col)) {

    .risk_validate_col_name(
      period_years_col,
      "period_years_col"
    )

    if (period_years_col %in% names(data)) {
      stop(
        paste0(
          "Output column `",
          period_years_col,
          "` already exists."
        ),
        call. = FALSE
      )
    }

    if (identical(period_years_col, annualised_col)) {
      stop(
        "`period_years_col` and `annualised_col` must differ.",
        call. = FALSE
      )
    }
  }


  # 4 ------------------------------------------------------------------------
  # Validate year length

  if (
    !is.numeric(year_length) ||
    length(year_length) != 1L ||
    is.na(year_length) ||
    !is.finite(year_length) ||
    year_length <= 0
  ) {
    stop(
      "`year_length` must be one finite number greater than zero.",
      call. = FALSE
    )
  }


  # 5 ------------------------------------------------------------------------
  # Determine observation-period mode

  mode_period_col <- !is.null(period_col)
  mode_period_value <- !is.null(period_value)

  mode_date_cols <-
    !is.null(start_col) ||
    !is.null(end_col)

  mode_date_values <-
    !is.null(start_date) ||
    !is.null(end_date)

  mode_count <- sum(
    mode_period_col,
    mode_period_value,
    mode_date_cols,
    mode_date_values
  )

  if (mode_count != 1L) {
    stop(
      paste(
        "Supply exactly one observation-period specification:",
        "`period_col`, `period_value`,",
        "`start_col` + `end_col`, or",
        "`start_date` + `end_date`."
      ),
      call. = FALSE
    )
  }


  # 6 ------------------------------------------------------------------------
  # Calculate observation period in years

  n <- nrow(data)

  period_years <- rep(
    NA_real_,
    n
  )


  # 6.1 ----------------------------------------------------------------------
  # Numeric period column

  if (mode_period_col) {

    .risk_validate_col_name(
      period_col,
      "period_col"
    )

    if (!period_col %in% names(data)) {
      stop(
        "`period_col` not found in data.",
        call. = FALSE
      )
    }

    if (!is.numeric(data[[period_col]])) {
      stop(
        "`period_col` must contain numeric years.",
        call. = FALSE
      )
    }

    period_years <- as.numeric(
      data[[period_col]]
    )
  }


  # 6.2 ----------------------------------------------------------------------
  # Fixed numeric period

  if (mode_period_value) {

    if (
      !is.numeric(period_value) ||
      length(period_value) != 1L ||
      is.na(period_value) ||
      !is.finite(period_value)
    ) {
      stop(
        "`period_value` must be one finite numeric value in years.",
        call. = FALSE
      )
    }

    period_years <- rep(
      as.numeric(period_value),
      n
    )
  }


  # 6.3 ----------------------------------------------------------------------
  # Row-specific date columns

  if (mode_date_cols) {

    if (is.null(start_col) || is.null(end_col)) {
      stop(
        "Both `start_col` and `end_col` must be supplied.",
        call. = FALSE
      )
    }

    .risk_validate_col_name(
      start_col,
      "start_col"
    )

    .risk_validate_col_name(
      end_col,
      "end_col"
    )

    if (!start_col %in% names(data)) {
      stop(
        "`start_col` not found in data.",
        call. = FALSE
      )
    }

    if (!end_col %in% names(data)) {
      stop(
        "`end_col` not found in data.",
        call. = FALSE
      )
    }

    start <- data[[start_col]]
    end <- data[[end_col]]

    valid_start <-
      inherits(start, "Date") ||
      inherits(start, "POSIXt")

    valid_end <-
      inherits(end, "Date") ||
      inherits(end, "POSIXt")

    if (!valid_start || !valid_end) {
      stop(
        "`start_col` and `end_col` must contain Date or POSIXt values.",
        call. = FALSE
      )
    }

    start <- as.Date(start)
    end <- as.Date(end)

    invalid_order <-
      !is.na(start) &
      !is.na(end) &
      end < start

    if (any(invalid_order)) {
      stop(
        "`end_col` contains dates earlier than `start_col`.",
        call. = FALSE
      )
    }

    period_years <-
      (as.numeric(end - start) + 1) /
      year_length
  }


  # 6.4 ----------------------------------------------------------------------
  # Fixed date window

  if (mode_date_values) {

    if (is.null(start_date) || is.null(end_date)) {
      stop(
        "Both `start_date` and `end_date` must be supplied.",
        call. = FALSE
      )
    }

    if (
      length(start_date) != 1L ||
      length(end_date) != 1L
    ) {
      stop(
        "`start_date` and `end_date` must each contain one date.",
        call. = FALSE
      )
    }

    start <- tryCatch(
      as.Date(start_date),
      error = function(e) NA
    )

    end <- tryCatch(
      as.Date(end_date),
      error = function(e) NA
    )

    if (
      length(start) != 1L ||
      length(end) != 1L ||
      is.na(start) ||
      is.na(end)
    ) {
      stop(
        "`start_date` and `end_date` must be valid dates.",
        call. = FALSE
      )
    }

    if (end < start) {
      stop(
        "`end_date` cannot be earlier than `start_date`.",
        call. = FALSE
      )
    }

    period_years <- rep(
      (as.numeric(end - start) + 1) /
        year_length,
      n
    )
  }


  # 7 ------------------------------------------------------------------------
  # Validate observation periods

  invalid_period <-
    !is.na(period_years) &
    (
      !is.finite(period_years) |
        period_years <= 0
    )

  if (any(invalid_period)) {
    stop(
      "Observation periods must be finite and greater than zero.",
      call. = FALSE
    )
  }


  # 8 ------------------------------------------------------------------------
  # Calculate annualised frequency

  annualised <-
    as.numeric(count) /
    period_years


  # 9 ------------------------------------------------------------------------
  # Append outputs

  out <- data

  out[[annualised_col]] <-
    annualised

  if (!is.null(period_years_col)) {
    out[[period_years_col]] <-
      period_years
  }

  out
}