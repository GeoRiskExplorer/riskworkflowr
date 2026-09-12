# 1 ------------------------------------------------------------------------
# risk_build_counts.R
# Purpose: Build complete spatial event counts from point events and units


#' Build event counts for spatial units
#'
#' Runs the complete spatial count-building workflow:
#'
#' 1. assign point events to analytical units with [sj_join()]
#' 2. count assigned events with [risk_count_units()]
#' 3. join counts back to the complete unit population with
#'    [risk_join_counts()]
#'
#' Unmatched events are retained in `joined_points` when `return = "all"` but
#' are not included in unit counts.
#'
#' @param points An sf POINT object.
#' @param units An sf POLYGON or MULTIPOLYGON object.
#' @param unit_id_col Name of the unique unit identifier column.
#' @param unit_name_col Optional unit name column.
#' @param join_mode Spatial assignment mode passed to [sj_join()].
#' @param max_distance_m Optional maximum nearest-assignment distance in metres.
#' @param count_col Name of the output count column.
#' @param missing_count_value Value used for units with no assigned events.
#' @param repair_units Logical; if TRUE, repair unit geometry before spatial
#'   assignment and return repaired geometry.
#' @param return Return `"units"` for counted units only, or `"all"` for a list
#'   containing joined points, represented-unit counts, and complete counted
#'   units.
#'
#' @return An sf object, or a list when `return = "all"`.
#'
#' @export

risk_build_counts <- function(
  points,
  units,
  unit_id_col,
  unit_name_col = NULL,
  join_mode = c(
    "intersect",
    "nearest",
    "intersect_nearest"
  ),
  max_distance_m = NULL,
  count_col = "event_count",
  missing_count_value = 0,
  repair_units = TRUE,
  return = c("units", "all")
) {

  # 1. Match controlled arguments ------------------------------------------

  join_mode <- match.arg(join_mode)
  return <- match.arg(return)


  # 2. Prepare authoritative unit geometry ----------------------------------

  counted_unit_base <-
    if (isTRUE(repair_units)) {
      geom_repair(
        units,
        quiet = TRUE
      )
    } else {
      units
    }


  # 3. Spatial assignment ---------------------------------------------------

  joined_points <- sj_join(
    points = points,
    polygons = counted_unit_base,
    id_col = unit_id_col,
    name_col = unit_name_col,
    join_mode = join_mode,
    repair_polygons = FALSE,
    max_distance_m = max_distance_m
  )


  # 4. Count represented units ----------------------------------------------

  counts <- risk_count_units(
    data = joined_points,
    unit_id_col = unit_id_col,
    unit_name_col = unit_name_col,
    count_col = count_col
  )


  # 5. Join counts to complete unit population ------------------------------

  counted_units <- risk_join_counts(
    units = counted_unit_base,
    counts = counts,
    unit_id_col = unit_id_col,
    unit_name_col = unit_name_col,
    count_col = count_col,
    missing_count_value = missing_count_value
  )


  # 6. Internal reconciliation contract -------------------------------------

  assigned_events <-
    sum(
      !is.na(
        joined_points[[unit_id_col]]
      )
    )

  counted_events <-
    if (nrow(counts) == 0L) {
      0
    } else {
      sum(
        counts[[count_col]]
      )
    }

  if (!identical(
    as.numeric(assigned_events),
    as.numeric(counted_events)
  )) {
    stop(
      "Internal count reconciliation failed: assigned events do not equal counted events.",
      call. = FALSE
    )
  }


  # 7. Return ---------------------------------------------------------------

  if (return == "units") {
    return(counted_units)
  }

  list(
    joined_points = joined_points,
    counts = counts,
    units = counted_units
  )
}
