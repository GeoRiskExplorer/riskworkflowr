#' Build grouped categorical risk cross-tabs
#'
#' Repeats [risk_crosstab()] within one or more grouping variables. This is a
#' thin orchestration wrapper; all cross-tab calculations remain authoritative
#' in [risk_crosstab()].
#'
#' @param data A data frame or `sf` object.
#' @param by Character vector naming one or more grouping variables.
#' @inheritParams risk_crosstab
#'
#' @return A non-spatial data frame containing grouping keys followed by the
#'   standard [risk_crosstab()] analytical columns.
#'
#' @export
risk_crosstab_by <- function(
  data,
  by,
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
  # 1. Validate
  # ---------------------------------------------------------------------------

  measure <- match.arg(measure)

  if (!inherits(data, "data.frame")) {
    stop("data must be a data frame or sf object.", call. = FALSE)
  }

  if (!is.character(by) || length(by) < 1L || any(!nzchar(by))) {
    stop("by must contain one or more column names.", call. = FALSE)
  }

  if (!is.character(rows) || length(rows) != 1L || !nzchar(rows)) {
    stop("rows must be a single column name.", call. = FALSE)
  }

  if (!is.character(cols) || length(cols) != 1L || !nzchar(cols)) {
    stop("cols must be a single column name.", call. = FALSE)
  }

  required <- unique(c(by, rows, cols))
  missing_fields <- setdiff(required, names(data))

  if (length(missing_fields) > 0L) {
    stop(
      "by, rows, and cols must name columns present in data.",
      call. = FALSE
    )
  }

  if (rows %in% by || cols %in% by) {
    stop(
      "by columns must be different from rows and cols.",
      call. = FALSE
    )
  }

  if (identical(rows, cols)) {
    stop(
      "rows and cols must name different columns.",
      call. = FALSE
    )
  }

  if (inherits(data, "sf")) {
    data <- sf::st_drop_geometry(data)
  }

  # ---------------------------------------------------------------------------
  # 2. Split into reporting groups
  # ---------------------------------------------------------------------------

  group_keys <- data |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(by))
    )

  results <- vector("list", nrow(group_keys))

  for (i in seq_len(nrow(group_keys))) {

    key <- group_keys[i, , drop = FALSE]

    keep <- rep(TRUE, nrow(data))

    for (field in by) {
      key_value <- key[[field]][1]

      if (is.na(key_value)) {
        keep <- keep & is.na(data[[field]])
      } else {
        keep <- keep &
          !is.na(data[[field]]) &
          data[[field]] == key_value
      }
    }

    group_data <- data[keep, , drop = FALSE]

    # -------------------------------------------------------------------------
    # 2a. Subset expected counts to the same reporting group
    # -------------------------------------------------------------------------

    group_expected <- NULL

    if (!is.null(expected_data)) {

      if (!inherits(expected_data, "data.frame")) {
        stop("expected_data must be a data frame.", call. = FALSE)
      }

      if (!all(by %in% names(expected_data))) {
        stop(
          "Grouped expected_data must contain all by columns.",
          call. = FALSE
        )
      }

      expected_keep <- rep(TRUE, nrow(expected_data))

      for (field in by) {
        key_value <- key[[field]][1]

        if (is.na(key_value)) {
          expected_keep <- expected_keep & is.na(expected_data[[field]])
        } else {
          expected_keep <- expected_keep &
            !is.na(expected_data[[field]]) &
            expected_data[[field]] == key_value
        }
      }

      group_expected <- expected_data[
        expected_keep,
        ,
        drop = FALSE
      ]
    }

    # -------------------------------------------------------------------------
    # 2b. Delegate analytical work to risk_crosstab()
    # -------------------------------------------------------------------------

    result <- risk_crosstab(
      data = group_data,
      rows = rows,
      cols = cols,
      value_col = value_col,
      measure = measure,
      start_date = start_date,
      end_date = end_date,
      expected_data = group_expected,
      expected_col = expected_col,
      poisson_k = poisson_k,
      include_missing = include_missing,
      complete_rows = complete_rows
    )

    # -------------------------------------------------------------------------
    # 2c. Restore grouping keys
    # -------------------------------------------------------------------------

    for (field in rev(by)) {
      result <- dplyr::relocate(
        dplyr::mutate(
          result,
          !!field := key[[field]][1]
        ),
        dplyr::all_of(field)
      )
    }

    results[[i]] <- result
  }

  # ---------------------------------------------------------------------------
  # 3. Combine
  # ---------------------------------------------------------------------------

  out <- dplyr::bind_rows(results)
  rownames(out) <- NULL
  out
}
