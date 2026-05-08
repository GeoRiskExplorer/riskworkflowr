# 1 ------------------------------------------------------------------------
# sj_join.R
# Purpose: Spatial join helper with explicit join modes


#' Spatially join points to polygon units
#'
#' Assigns point features to polygon units using intersect, nearest, or
#' intersect-nearest logic.
#'
#' This function is intended for reproducible spatial assignment workflows
#' supporting areal units, custom polygons, and hexagonal grids.
#'
#' The output includes `join_method` and `join_distance_m` fields to support
#' auditability of spatial assignment decisions.
#'
#' @param points An sf point object.
#' @param polygons An sf polygon object containing target join units.
#' @param id_col Name of the polygon identifier column to attach to points.
#' @param name_col Optional polygon name column to attach to points.
#' @param join_mode Spatial join mode. One of `"intersect"`, `"nearest"`,
#'   or `"intersect_nearest"`.
#' @param repair_polygons Logical; if TRUE, repair polygon geometry before
#'   spatial predicates are evaluated.
#'
#' @return An sf object of joined point features with `join_method` and
#' `join_distance_m` columns.
#'
#' @examples
#' \dontrun{
#' joined <- sj_join(
#'   points = pts,
#'   polygons = polygons,
#'   id_col = "lga_code",
#'   name_col = "lga_name",
#'   join_mode = "intersect_nearest"
#' )
#' }
#'
#' @export
sj_join <- function(
  points,
  polygons,
  id_col,
  name_col = NULL,
  join_mode = c("intersect", "nearest", "intersect_nearest"),
  repair_polygons = TRUE
) {

  join_mode <- match.arg(join_mode)

  if (!inherits(points, "sf")) {
    stop("`points` must be an sf object.", call. = FALSE)
  }

  if (!inherits(polygons, "sf")) {
    stop("`polygons` must be an sf object.", call. = FALSE)
  }

  if (!id_col %in% names(polygons)) {
    stop("`id_col` not found in polygons.", call. = FALSE)
  }

  if (!is.null(name_col) && !name_col %in% names(polygons)) {
    stop("`name_col` not found in polygons.", call. = FALSE)
  }

  if (repair_polygons) {
    polygons <- geom_repair(polygons, quiet = TRUE)
  }

  polygon_geom_col <- attr(polygons, "sf_column")

  keep_cols <- c(id_col, name_col)
  keep_cols <- keep_cols[!is.null(keep_cols)]

  polygon_lookup <- polygons |>
    dplyr::select(
      dplyr::all_of(keep_cols),
      dplyr::all_of(polygon_geom_col)
    )

  # 2 ----------------------------------------------------------------------
  # Mode 1: strict intersect

  if (join_mode == "intersect") {

    joined <- sf::st_join(
      points,
      polygon_lookup,
      join = sf::st_intersects,
      left = TRUE
    )

    joined$join_method <- ifelse(
      is.na(joined[[id_col]]),
      "unmatched",
      "intersect"
    )

    joined$join_distance_m <- ifelse(
      joined$join_method == "intersect",
      0,
      NA_real_
    )

    return(joined)
  }

  # 3 ----------------------------------------------------------------------
  # Mode 2: nearest for every point

  if (join_mode == "nearest") {

    nearest_idx <- sf::st_nearest_feature(
      points,
      polygon_lookup
    )

    nearest_polygons <- polygon_lookup[nearest_idx, ]

    nearest_dist <- sf::st_distance(
      points,
      nearest_polygons,
      by_element = TRUE
    )

    joined <- points

    joined[[id_col]] <- nearest_polygons[[id_col]]

    if (!is.null(name_col)) {
      joined[[name_col]] <- nearest_polygons[[name_col]]
    }

    joined$join_method <- "nearest"
    joined$join_distance_m <- as.numeric(nearest_dist)

    return(joined)
  }

  # 4 ----------------------------------------------------------------------
  # Mode 3: intersect first, nearest fallback

  if (join_mode == "intersect_nearest") {

    joined <- sf::st_join(
      points,
      polygon_lookup,
      join = sf::st_intersects,
      left = TRUE
    )

    joined$join_method <- ifelse(
      is.na(joined[[id_col]]),
      "unmatched",
      "intersect"
    )

    joined$join_distance_m <- ifelse(
      joined$join_method == "intersect",
      0,
      NA_real_
    )

    unmatched_idx <- which(is.na(joined[[id_col]]))

    if (length(unmatched_idx) > 0) {

      nearest_idx <- sf::st_nearest_feature(
        joined[unmatched_idx, ],
        polygon_lookup
      )

      nearest_polygons <- polygon_lookup[nearest_idx, ]

      nearest_dist <- sf::st_distance(
        joined[unmatched_idx, ],
        nearest_polygons,
        by_element = TRUE
      )

      joined[[id_col]][unmatched_idx] <- nearest_polygons[[id_col]]

      if (!is.null(name_col)) {
        joined[[name_col]][unmatched_idx] <- nearest_polygons[[name_col]]
      }

      joined$join_method[unmatched_idx] <- "nearest"
      joined$join_distance_m[unmatched_idx] <- as.numeric(nearest_dist)
    }

    return(joined)
  }
}