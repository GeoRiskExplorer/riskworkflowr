# 1 ------------------------------------------------------------------------
# risk_build_counts.R
# Purpose: Build counted polygon units from point events and analysis units

#' Build event counts for spatial units
#'
#' Runs the core count-building workflow: spatially joins points to units,
#' counts events by unit, and joins the counts back to the original units.
#'
#' This function works for administrative areas, custom polygons, and hexbins.
#'
#' @param points An sf point object.
#' @param units An sf polygon or hexbin object.
#' @param unit_id_col Name of the unit identifier column.
#' @param unit_name_col Optional unit name column.
#' @param join_mode Spatial join mode passed to [sj_join()].
#' @param count_col Name of the output count column.
#' @param missing_count_value Value used for units with no events.
#' @param repair_units Logical; if TRUE, repair unit geometry before spatial joins.
#' @param return Return `"units"` for counted units only, or `"all"` for a list
#'   containing joined points, counts, and counted units.
#'
#' @return An sf object, or a list when `return = "all"`.
#'
#' @export

risk_build_counts <- function(
  points,
  units,
  unit_id_col,
  unit_name_col = NULL,
  join_mode = "intersect_nearest",
  count_col = "event_count",
  missing_count_value = 0,
  repair_units = TRUE,
  return = c("units", "all")
) {

  return <- match.arg(return)

  joined_points <- sj_join(
    points = points,
    polygons = units,
    id_col = unit_id_col,
    name_col = unit_name_col,
    join_mode = join_mode,
    repair_polygons = repair_units
  )

  counts <- risk_count_units(
    data = joined_points,
    unit_id_col = unit_id_col,
    unit_name_col = unit_name_col,
    count_col = count_col
  )

  counted_units <- risk_join_counts(
    units = if (repair_units) geom_repair(units, quiet = TRUE) else units,
    counts = counts,
    unit_id_col = unit_id_col,
    unit_name_col = unit_name_col,
    count_col = count_col,
    missing_count_value = missing_count_value
  )

  if (return == "units") {
    return(counted_units)
  }

  list(
    joined_points = joined_points,
    counts = counts,
    units = counted_units
  )
}