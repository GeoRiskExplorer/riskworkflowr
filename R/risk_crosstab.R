#' Build a categorical risk cross-tab
#'
#' Builds a complete analytical cross-tab from categorical row and column
#' variables. The function can count records or sum a supplied count field,
#' optionally annualise observed counts, attach supplied expected counts, and
#' calculate a Poisson exceedance probability using
#' [risk_poisson_probability()].
#'
#' `risk_crosstab()` does not derive expected counts. If `expected_data` is
#' not supplied, `expected_count` and `poisson_probability` are returned as
#' missing.
#'
#' @param data A data frame or `sf` object.
#' @param rows Single character string naming the row variable.
#' @param cols Single character string naming the column variable.
#' @param value_col Optional character string naming the value field used when
#'   `measure = "sum"`.
#' @param measure Either `"count"` to count records or `"sum"` to sum
#'   `value_col`.
#' @param start_date,end_date Optional analysis-period dates passed to
#'   [risk_annualise()]. Supply both or neither.
#' @param expected_data Optional data frame containing expected counts keyed by
#'   `rows` and `cols`.
#' @param expected_col Character string naming the expected-count field in
#'   `expected_data`.
#' @param poisson_k Whole-number Poisson exceedance threshold. Defaults to `1`,
#'   giving `P(X >= 1)`. For example, `poisson_k = 2` gives `P(X >= 2)`.
#' @param include_missing Logical. If `TRUE`, observed missing row/column
#'   categories are retained as analytical `NA` values.
#' @param complete_rows Logical. If `TRUE`, unused factor levels in the row
#'   variable are retained. If `FALSE`, only row groups observed in the input
#'   are retained. Column factor levels are always retained.
#'
#' @return A non-spatial data frame containing the row and column keys,
#'   `event_count`, `period_years`, `annualised_count`, `expected_count`,
#'   `poisson_k`, and `poisson_probability`.
#'
#' @export
risk_crosstab <- function(
  data,
  rows,
  cols,
  value_col = NULL,
  measure = c("count", "sum"),
  start_date = NULL,
  end_date = NULL,
  expected_data = NULL,
  expected_col = "expected_count",
  poisson_k = 1,
  include_missing = TRUE,
  complete_rows = TRUE
) {

  # ---------------------------------------------------------------------------
  # 1. Validate inputs
  # ---------------------------------------------------------------------------

  measure <- match.arg(measure)

  if (!inherits(data, "data.frame")) {
    stop("data must be a data frame or sf object.", call. = FALSE)
  }

  if (!is.character(rows) || length(rows) != 1L || !nzchar(rows)) {
    stop("rows must be a single column name.", call. = FALSE)
  }

  if (!is.character(cols) || length(cols) != 1L || !nzchar(cols)) {
    stop("cols must be a single column name.", call. = FALSE)
  }

  missing_fields <- setdiff(c(rows, cols), names(data))

  if (length(missing_fields) > 0L) {
    stop(
      "rows and cols must name columns present in data.",
      call. = FALSE
    )
  }

  if (!is.logical(include_missing) || length(include_missing) != 1L ||
      is.na(include_missing)) {
    stop("include_missing must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(complete_rows) || length(complete_rows) != 1L ||
      is.na(complete_rows)) {
    stop("complete_rows must be TRUE or FALSE.", call. = FALSE)
  }

  if (
    !is.numeric(poisson_k) ||
    length(poisson_k) != 1L ||
    is.na(poisson_k) ||
    !is.finite(poisson_k) ||
    poisson_k < 1 ||
    poisson_k != floor(poisson_k)
  ) {
    stop(
      "poisson_k must be a single whole number greater than or equal to 1.",
      call. = FALSE
    )
  }

  poisson_k <- as.integer(poisson_k)

  if (measure == "sum") {
    if (is.null(value_col) ||
        !is.character(value_col) ||
        length(value_col) != 1L ||
        !value_col %in% names(data)) {
      stop(
        "value_col must name a column in data when measure = \"sum\".",
        call. = FALSE
      )
    }

    if (!is.numeric(data[[value_col]])) {
      stop("value_col must be numeric.", call. = FALSE)
    }

    bad_values <- !is.na(data[[value_col]]) &
      (!is.finite(data[[value_col]]) | data[[value_col]] < 0)

    if (any(bad_values)) {
      stop(
        "value_col must contain only non-negative finite values or NA.",
        call. = FALSE
      )
    }
  }

  one_date_only <- xor(is.null(start_date), is.null(end_date))

  if (one_date_only) {
    stop(
      "start_date and end_date must be supplied together.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 2. Drop geometry without changing the analytical records
  # ---------------------------------------------------------------------------

  if (inherits(data, "sf")) {
    data <- sf::st_drop_geometry(data)
  }

  # ---------------------------------------------------------------------------
  # 3. Preserve categorical domains
  # ---------------------------------------------------------------------------

  row_input <- data[[rows]]
  col_input <- data[[cols]]

  row_levels <- if (is.factor(row_input) && complete_rows) {
    levels(row_input)
  } else {
    unique(as.character(row_input[!is.na(row_input)]))
  }

  col_levels <- if (is.factor(col_input)) {
    levels(col_input)
  } else {
    unique(as.character(col_input[!is.na(col_input)]))
  }

  observed_row_na <- any(is.na(row_input))
  observed_col_na <- any(is.na(col_input))

  # ---------------------------------------------------------------------------
  # 4. Aggregate observed events
  # ---------------------------------------------------------------------------

  work <- data

  if (!include_missing) {
    work <- work[
      !is.na(work[[rows]]) & !is.na(work[[cols]]),
      ,
      drop = FALSE
    ]
  }

  if (measure == "count") {
    observed <- work |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(c(rows, cols))),
        .drop = FALSE
      ) |>
      dplyr::summarise(
        event_count = dplyr::n(),
        .groups = "drop"
      )
  } else {
    observed <- work |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(c(rows, cols))),
        .drop = FALSE
      ) |>
      dplyr::summarise(
        event_count = sum(.data[[value_col]], na.rm = TRUE),
        .groups = "drop"
      )
  }

  # ---------------------------------------------------------------------------
  # 5. Build complete row x column analytical grid
  # ---------------------------------------------------------------------------

  row_domain <- row_levels
  col_domain <- col_levels

  if (include_missing && observed_row_na) {
    row_domain <- c(row_domain, NA_character_)
  }

  if (include_missing && observed_col_na) {
    col_domain <- c(col_domain, NA_character_)
  }

  grid <- tidyr::expand_grid(
    .row_key = row_domain,
    .col_key = col_domain
  )

  names(grid) <- c(rows, cols)

  # Match join types explicitly. Character storage retains analytical NA while
  # factor ordering is restored below.
  observed[[rows]] <- as.character(observed[[rows]])
  observed[[cols]] <- as.character(observed[[cols]])

  out <- dplyr::left_join(
    grid,
    observed,
    by = c(rows, cols)
  )

  out$event_count[is.na(out$event_count)] <- 0

  # Restore factor ordering where the source variables were factors.
  if (is.factor(row_input)) {
    out[[rows]] <- factor(
      out[[rows]],
      levels = levels(row_input),
      ordered = is.ordered(row_input)
    )
  }

  if (is.factor(col_input)) {
    out[[cols]] <- factor(
      out[[cols]],
      levels = levels(col_input),
      ordered = is.ordered(col_input)
    )
  }

  # ---------------------------------------------------------------------------
  # 6. Annualise observed counts when a period is supplied
  # ---------------------------------------------------------------------------

  if (!is.null(start_date) && !is.null(end_date)) {
    out <- risk_annualise(
      data = out,
      count_col = "event_count",
      start_date = start_date,
      end_date = end_date,
      annualised_col = "annualised_count",
      period_years_col = "period_years"
    )
  } else {
    out$period_years <- NA_real_
    out$annualised_count <- NA_real_
  }

  # ---------------------------------------------------------------------------
  # 7. Attach supplied expected counts
  # ---------------------------------------------------------------------------

  if (is.null(expected_data)) {
    out$expected_count <- NA_real_
  } else {

    if (!inherits(expected_data, "data.frame")) {
      stop("expected_data must be a data frame.", call. = FALSE)
    }

    required_expected <- c(rows, cols, expected_col)

    if (!all(required_expected %in% names(expected_data))) {
      stop(
        "expected_data must contain rows, cols, and expected_col.",
        call. = FALSE
      )
    }

    expected <- expected_data[
      ,
      required_expected,
      drop = FALSE
    ]

    if (inherits(expected, "sf")) {
      expected <- sf::st_drop_geometry(expected)
    }

    if (!is.numeric(expected[[expected_col]])) {
      stop("expected_col must be numeric.", call. = FALSE)
    }

    bad_expected <- !is.na(expected[[expected_col]]) &
      (!is.finite(expected[[expected_col]]) | expected[[expected_col]] < 0)

    if (any(bad_expected)) {
      stop(
        "expected_col must contain only non-negative finite values or NA.",
        call. = FALSE
      )
    }

    duplicate_keys <- duplicated(expected[c(rows, cols)]) |
      duplicated(expected[c(rows, cols)], fromLast = TRUE)

    if (any(duplicate_keys)) {
      stop(
        "expected_data must contain unique row and column keys.",
        call. = FALSE
      )
    }

    expected[[rows]] <- as.character(expected[[rows]])
    expected[[cols]] <- as.character(expected[[cols]])

    out_join <- out
    out_join[[rows]] <- as.character(out_join[[rows]])
    out_join[[cols]] <- as.character(out_join[[cols]])

    names(expected)[names(expected) == expected_col] <- "expected_count"

    out <- dplyr::left_join(
      out_join,
      expected,
      by = c(rows, cols)
    )

    if (is.factor(row_input)) {
      out[[rows]] <- factor(
        out[[rows]],
        levels = levels(row_input),
        ordered = is.ordered(row_input)
      )
    }

    if (is.factor(col_input)) {
      out[[cols]] <- factor(
        out[[cols]],
        levels = levels(col_input),
        ordered = is.ordered(col_input)
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 8. Poisson exceedance probability
  # ---------------------------------------------------------------------------

  out$poisson_k <- poisson_k

  if (is.null(expected_data)) {
    out$poisson_probability <- NA_real_
  } else {
    out <- risk_poisson_probability(
      data = out,
      lambda_col = "expected_count",
      k = poisson_k,
      probability_col = "poisson_probability"
    )
  }

  # ---------------------------------------------------------------------------
  # 9. Standard analytical schema
  # ---------------------------------------------------------------------------

  out <- out[
    ,
    c(
      rows,
      cols,
      "event_count",
      "period_years",
      "annualised_count",
      "expected_count",
      "poisson_k",
      "poisson_probability"
    ),
    drop = FALSE
  ]

  rownames(out) <- NULL
  out
}
