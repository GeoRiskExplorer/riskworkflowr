# 1 ------------------------------------------------------------------------
# risk_count_units.R
# Purpose: Count events/records by analysis unit


#' Count events within spatial units
#'
#' Aggregates point event counts by polygon or hexagonal units.
#'
#' Intended for risk analysis workflows where events are assigned
#' to administrative areas, statistical regions, or grid systems.
#'
#' @param joined_points An sf object of joined point features.
#' @param unit_id_col Column containing unit identifiers.
#' @param count_col Name of output count column.
#'
#' @return A data frame containing event counts by unit.
#'
#' @examples
#' \dontrun{
#' counts <- risk_count_units(
#'   joined_points = joined_sf,
#'   unit_id_col = "hex_id"
#' )
#' }
#'
#' @export

risk_count_units <- function(
  data,
  unit_id_col,
  unit_name_col = NULL,
  count_col = "event_count",
  drop_geometry = TRUE
) {

  if (!unit_id_col %in% names(data)) {
    stop("`unit_id_col` not found in data.", call. = FALSE)
  }

  if (!is.null(unit_name_col) && !unit_name_col %in% names(data)) {
    stop("`unit_name_col` not found in data.", call. = FALSE)
  }

  if (inherits(data, "sf") && isTRUE(drop_geometry)) {
    data <- sf::st_drop_geometry(data)
  }

  group_cols <- c(unit_id_col, unit_name_col)
  group_cols <- group_cols[!is.null(group_cols)]

  out <- data |>
    dplyr::filter(!is.na(.data[[unit_id_col]])) |>
    dplyr::count(
      dplyr::across(dplyr::all_of(group_cols)),
      name = count_col
    ) |>
    dplyr::arrange(
      dplyr::desc(.data[[count_col]])
    )

  out
}