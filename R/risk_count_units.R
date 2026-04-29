# 1 ------------------------------------------------------------------------
# risk_count_units.R
# Purpose: Count events/records by analysis unit

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