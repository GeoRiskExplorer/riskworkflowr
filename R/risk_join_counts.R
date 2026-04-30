# 1 ------------------------------------------------------------------------
# risk_join_counts.R
# Purpose: Join event/count table back to areal unit polygons

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
