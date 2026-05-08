# 1 ------------------------------------------------------------------------
# risk_join_counts.R
# Purpose: Join event/count table back to areal unit polygons

#' Join event counts back to spatial units
#'
#' Joins a count table back to polygon or hexbin units and optionally replaces
#' missing count values with a chosen value, usually zero.
#'
#' @param units An sf object containing polygon or hexbin units.
#' @param counts A data frame containing counts by unit.
#' @param unit_id_col Name of the shared unit identifier column.
#' @param unit_name_col Optional shared unit name column.
#' @param count_col Name of the count column.
#' @param missing_count_value Value used to replace missing counts. Use `NULL`
#'   to preserve missing values.
#'
#' @return An sf object with counts joined to the input units.
#'
#' @examples
#' units <- data.frame(
#'   unit_id = c("A", "B", "C"),
#'   unit_name = c("Area A", "Area B", "Area C")
#' )
#'
#' counts <- data.frame(
#'   unit_id = c("A", "C"),
#'   event_count = c(3, 7)
#' )
#'
#' risk_join_counts(
#'   units = units,
#'   counts = counts,
#'   unit_id_col = "unit_id"
#' )
#'
#' @export

risk_join_counts <- function(
  units,
  counts,
  unit_id_col,
  unit_name_col = NULL,
  count_col = "event_count",
  missing_count_value = 0
) {

  if (!inherits(units, "sf")) {
    stop("`units` must be an sf object.", call. = FALSE)
  }

  if (!unit_id_col %in% names(units)) {
    stop("`unit_id_col` not found in units.", call. = FALSE)
  }

  if (!unit_id_col %in% names(counts)) {
    stop("`unit_id_col` not found in counts.", call. = FALSE)
  }

  if (!count_col %in% names(counts)) {
    stop("`count_col` not found in counts.", call. = FALSE)
  }

  join_cols <- unit_id_col

  if (!is.null(unit_name_col)) {
    if (!unit_name_col %in% names(units)) {
      stop("`unit_name_col` not found in units.", call. = FALSE)
    }

    if (!unit_name_col %in% names(counts)) {
      stop("`unit_name_col` not found in counts.", call. = FALSE)
    }

    join_cols <- c(unit_id_col, unit_name_col)
  }

  out <- units |>
    dplyr::left_join(
      counts,
      by = join_cols
    )

  if (!is.null(missing_count_value)) {
    out[[count_col]] <- dplyr::coalesce(
      out[[count_col]],
      missing_count_value
    )
  }

  out
}
