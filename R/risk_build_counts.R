# 1 ------------------------------------------------------------------------
# risk_build_counts.R
# Purpose: Build counted polygon units from point events and analysis units

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