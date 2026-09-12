# 1 ------------------------------------------------------------------------
# risk_count_units.R
# Purpose: Count assigned event records by analytical unit


#' Count event records by analytical unit
#'
#' Counts records that have already been assigned to an analytical unit.
#' Records with a missing unit identifier are treated as unmatched and are
#' excluded from the count table.
#'
#' This function does not perform spatial assignment and does not create
#' zero-event units. Use [sj_join()] before this function and
#' [risk_join_counts()] afterwards when a complete spatial-unit population is
#' required.
#'
#' @param data A data frame or sf object containing assigned event records.
#' @param unit_id_col Name of the unit identifier column.
#' @param unit_name_col Optional unit name column.
#' @param count_col Name of the output count column.
#' @param drop_geometry Logical; if TRUE and `data` is an sf object, geometry is
#'   dropped before counting.
#'
#' @return A data frame containing one row per represented analytical unit.
#'
#' @export

risk_count_units <- function(
  data,
  unit_id_col,
  unit_name_col = NULL,
  count_col = "event_count",
  drop_geometry = TRUE
) {

  # 1. Validate inputs -------------------------------------------------------

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or sf object.", call. = FALSE)
  }

  validate_name <- function(x, arg, allow_null = FALSE) {

    if (allow_null && is.null(x)) {
      return(invisible(TRUE))
    }

    if (
      length(x) != 1L ||
      !is.character(x) ||
      is.na(x) ||
      !nzchar(x)
    ) {
      stop(
        paste0("`", arg, "` must be one non-empty character value."),
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  validate_name(unit_id_col, "unit_id_col")
  validate_name(unit_name_col, "unit_name_col", allow_null = TRUE)
  validate_name(count_col, "count_col")

  if (!unit_id_col %in% names(data)) {
    stop("`unit_id_col` not found in data.", call. = FALSE)
  }

  if (
    !is.null(unit_name_col) &&
      !unit_name_col %in% names(data)
  ) {
    stop("`unit_name_col` not found in data.", call. = FALSE)
  }

  if (count_col %in% c(unit_id_col, unit_name_col)) {
    stop(
      "`count_col` cannot be the same as a grouping column.",
      call. = FALSE
    )
  }

  if (
    length(drop_geometry) != 1L ||
    !is.logical(drop_geometry) ||
    is.na(drop_geometry)
  ) {
    stop("`drop_geometry` must be TRUE or FALSE.", call. = FALSE)
  }


  # 2. Prepare data ----------------------------------------------------------

  out_data <- data

  if (inherits(out_data, "sf") && isTRUE(drop_geometry)) {
    out_data <- sf::st_drop_geometry(out_data)
  }

  group_cols <- c(
    unit_id_col,
    unit_name_col
  )


  # 3. Count represented units ----------------------------------------------

  out <- out_data |>
    dplyr::filter(
      !is.na(.data[[unit_id_col]])
    ) |>
    dplyr::count(
      dplyr::across(
        dplyr::all_of(group_cols)
      ),
      name = count_col
    ) |>
    dplyr::arrange(
      dplyr::desc(.data[[count_col]])
    )

  out
}
