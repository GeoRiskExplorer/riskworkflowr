# 1 ------------------------------------------------------------------------
# risk_count_units.R
# Purpose: Count events/records by analysis unit


#' Count events within spatial units
#'
#' Aggregates assigned event records by polygon, hexbin, or other spatial unit.
#'
#' This function is intended for risk analysis workflows where events have
#' already been assigned to administrative areas, statistical regions, sites,
#' or grid systems using a spatial join workflow such as [sj_join()].
#'
#' @param data A data frame or sf object containing joined event records.
#' @param unit_id_col Name of the unit identifier column.
#' @param unit_name_col Optional name of the unit name column.
#' @param count_col Name of the output count column.
#' @param drop_geometry Logical; if TRUE and `data` is an sf object, geometry is
#'   dropped before counting.
#'
#' @return A data frame containing event counts by unit.
#'
#' @examples
#' \dontrun{
#' counts <- risk_count_units(
#'   data = joined_sf,
#'   unit_id_col = "hex_id",
#'   count_col = "event_count"
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