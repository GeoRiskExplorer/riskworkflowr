# 1 ------------------------------------------------------------------------
# risk_calc_smr.R
# Purpose: Calculate SMR / relative risk with optional exact Poisson CI


#' Calculate Standardised Mortality Ratio (SMR)
#'
#' Calculates expected counts and Standardised Mortality Ratios (SMR) using a
#' global reference rate derived from the input dataset.
#'
#' The global reference rate is calculated as:
#'
#' `sum(observed) / sum(denominator)`
#'
#' Expected counts are then calculated for each row as:
#'
#' `denominator * global reference rate`
#'
#' and SMR as:
#'
#' `observed / expected`
#'
#' Optional exact confidence intervals are calculated from the Poisson
#' distribution for the observed event count.
#'
#' @param data A data frame or sf object.
#' @param observed_col Name of observed event count column.
#' @param denominator_col Name of denominator, population, or exposure column.
#' @param expected_col Name of expected count output column.
#' @param smr_col Name of SMR output column.
#' @param global_rate_col Optional name of global reference rate output column.
#' @param ci_method Confidence interval method. One of `"exact"` or `"none"`.
#' @param conf_level Confidence level for intervals.
#' @param smr_lower_col Name of lower confidence interval column.
#' @param smr_upper_col Name of upper confidence interval column.
#' @param smr_ci_flag_col Name of SMR interpretation/classification column.
#' @param zero_expected_value Value returned when expected count is missing,
#'   zero, or negative.
#'
#' @return Input data with expected counts, SMR values, and optional confidence
#' interval columns added.
#'
#' @details
#' Observed counts must be numeric, finite, and non-negative. When
#' `ci_method = "exact"`, observed counts must also be whole numbers because
#' the interval is based on an exact Poisson count model.
#'
#' Denominators must be numeric, finite, and non-negative.
#'
#' Missing observed or denominator values are permitted and propagate to
#' row-level derived values.
#'
#' @examples
#' data <- data.frame(
#'   event_count = c(5, 10, 20),
#'   population = c(1000, 2000, 3000)
#' )
#'
#' risk_calc_smr(
#'   data = data,
#'   observed_col = "event_count",
#'   denominator_col = "population"
#' )
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

  # ------------------------------------------------------------------------
  # 01. ARGUMENT VALIDATION
  # ------------------------------------------------------------------------

  ci_method <- match.arg(ci_method)

  required_names <- c(
    observed_col,
    denominator_col,
    expected_col,
    smr_col
  )

  if (any(
    !vapply(
      required_names,
      function(x) {
        is.character(x) &&
          length(x) == 1L &&
          !is.na(x) &&
          nzchar(x)
      },
      logical(1)
    )
  )) {
    stop(
      "Input and output column names must be single non-empty strings.",
      call. = FALSE
    )
  }

  if (!is.null(global_rate_col)) {
    if (
      !is.character(global_rate_col) ||
      length(global_rate_col) != 1L ||
      is.na(global_rate_col) ||
      !nzchar(global_rate_col)
    ) {
      stop(
        "`global_rate_col` must be NULL or a single non-empty column name.",
        call. = FALSE
      )
    }
  }

  if (!observed_col %in% names(data)) {
    stop("`observed_col` not found in data.", call. = FALSE)
  }

  if (!denominator_col %in% names(data)) {
    stop("`denominator_col` not found in data.", call. = FALSE)
  }

  if (
    !is.numeric(conf_level) ||
    length(conf_level) != 1L ||
    is.na(conf_level) ||
    !is.finite(conf_level) ||
    conf_level <= 0 ||
    conf_level >= 1
  ) {
    stop(
      "`conf_level` must be a single numeric value between 0 and 1.",
      call. = FALSE
    )
  }


  # ------------------------------------------------------------------------
  # 02. INPUT VALUE VALIDATION
  # ------------------------------------------------------------------------

  observed <- data[[observed_col]]
  denominator <- data[[denominator_col]]

  if (!is.numeric(observed)) {
    stop("`observed_col` must be numeric.", call. = FALSE)
  }

  if (!is.numeric(denominator)) {
    stop("`denominator_col` must be numeric.", call. = FALSE)
  }

  if (any(!is.na(observed) & !is.finite(observed))) {
    stop(
      "`observed_col` must contain only finite values or NA.",
      call. = FALSE
    )
  }

  if (any(!is.na(denominator) & !is.finite(denominator))) {
    stop(
      "`denominator_col` must contain only finite values or NA.",
      call. = FALSE
    )
  }

  if (any(!is.na(observed) & observed < 0)) {
    stop(
      "`observed_col` must contain values greater than or equal to zero.",
      call. = FALSE
    )
  }

  if (any(!is.na(denominator) & denominator < 0)) {
    stop(
      "`denominator_col` must contain values greater than or equal to zero.",
      call. = FALSE
    )
  }

  if (
    ci_method == "exact" &&
    any(!is.na(observed) & observed != floor(observed))
  ) {
    stop(
      paste(
        "`observed_col` must contain whole-number counts when",
        "`ci_method = \"exact\"`."
      ),
      call. = FALSE
    )
  }


  # ------------------------------------------------------------------------
  # 03. OUTPUT-COLUMN COLLISION CHECK
  # ------------------------------------------------------------------------

  output_cols <- c(
    expected_col,
    smr_col
  )

  if (!is.null(global_rate_col)) {
    output_cols <- c(
      output_cols,
      global_rate_col
    )
  }

  if (ci_method == "exact") {
    output_cols <- c(
      output_cols,
      smr_lower_col,
      smr_upper_col,
      smr_ci_flag_col
    )
  }

  duplicate_outputs <- unique(
    output_cols[output_cols %in% names(data)]
  )

  if (length(duplicate_outputs) > 0L) {
    stop(
      paste0(
        "Output column",
        if (length(duplicate_outputs) > 1L) "s " else " ",
        paste(
          paste0("`", duplicate_outputs, "`"),
          collapse = ", "
        ),
        " already exist",
        if (length(duplicate_outputs) == 1L) "s" else "",
        " in data."
      ),
      call. = FALSE
    )
  }


  # ------------------------------------------------------------------------
  # 04. ZERO-ROW INPUT
  # ------------------------------------------------------------------------

  if (nrow(data) == 0L) {

    data[[expected_col]] <- numeric(0)
    data[[smr_col]] <- numeric(0)

    if (!is.null(global_rate_col)) {
      data[[global_rate_col]] <- numeric(0)
    }

    if (ci_method == "exact") {
      data[[smr_lower_col]] <- numeric(0)
      data[[smr_upper_col]] <- numeric(0)
      data[[smr_ci_flag_col]] <- character(0)
    }

    return(data)
  }


  # ------------------------------------------------------------------------
  # 05. GLOBAL REFERENCE RATE
  # ------------------------------------------------------------------------

  total_observed <- sum(
    observed,
    na.rm = TRUE
  )

  total_denominator <- sum(
    denominator,
    na.rm = TRUE
  )

  if (total_denominator <= 0) {
    stop(
      "Total denominator must be greater than zero.",
      call. = FALSE
    )
  }

  global_rate <- total_observed / total_denominator


  # ------------------------------------------------------------------------
  # 06. EXPECTED COUNT AND SMR
  # ------------------------------------------------------------------------

  out <- data

  out[[expected_col]] <-
    denominator * global_rate

  out[[smr_col]] <- ifelse(
    is.na(out[[expected_col]]) |
      out[[expected_col]] <= 0,
    zero_expected_value,
    observed / out[[expected_col]]
  )

  if (!is.null(global_rate_col)) {
    out[[global_rate_col]] <- global_rate
  }


  # ------------------------------------------------------------------------
  # 07. EXACT POISSON CONFIDENCE INTERVAL
  # ------------------------------------------------------------------------

  if (ci_method == "exact") {

    alpha <- 1 - conf_level

    lower_observed <- ifelse(
      is.na(observed),
      NA_real_,
      ifelse(
        observed == 0,
        0,
        stats::qchisq(
          alpha / 2,
          2 * observed
        ) / 2
      )
    )

    upper_observed <- ifelse(
      is.na(observed),
      NA_real_,
      stats::qchisq(
        1 - alpha / 2,
        2 * (observed + 1)
      ) / 2
    )

    valid_expected <-
      !is.na(out[[expected_col]]) &
      out[[expected_col]] > 0

    out[[smr_lower_col]] <- ifelse(
      valid_expected,
      lower_observed / out[[expected_col]],
      NA_real_
    )

    out[[smr_upper_col]] <- ifelse(
      valid_expected,
      upper_observed / out[[expected_col]],
      NA_real_
    )

    out[[smr_ci_flag_col]] <- dplyr::case_when(
      is.na(out[[smr_col]]) ~ "missing",
      out[[smr_lower_col]] > 1 ~ "above_expected",
      out[[smr_upper_col]] < 1 ~ "below_expected",
      TRUE ~ "not_clearly_different"
    )
  }


  # ------------------------------------------------------------------------
  # 08. RETURN
  # ------------------------------------------------------------------------

  out
}