# 1 ------------------------------------------------------------------------
# sj_join.R
# Purpose: Spatially assign point events to polygon units with auditable
#          intersect / nearest / intersect-nearest behaviour


#' Spatially join points to polygon units
#'
#' Assigns point features to polygon units using strict spatial intersection,
#' nearest-unit assignment, or intersection with nearest-unit fallback.
#'
#' The function preserves one output row per input point. Ambiguous
#' intersections, where a point intersects more than one polygon, are rejected
#' rather than silently duplicating event records.
#'
#' Nearest assignments can optionally be limited using `max_distance_m`.
#'
#' The output includes:
#'
#' - `join_method`: `"intersect"`, `"nearest"`, or `"unmatched"`
#' - `join_distance_m`: distance to the assigned or nearest polygon in metres
#'
#' @param points An sf POINT object.
#' @param polygons An sf POLYGON or MULTIPOLYGON object containing target
#'   analytical units.
#' @param id_col Name of the polygon identifier column to attach to points.
#' @param name_col Optional polygon name column to attach to points.
#' @param join_mode Spatial assignment mode. One of `"intersect"`, `"nearest"`,
#'   or `"intersect_nearest"`.
#' @param repair_polygons Logical; if `TRUE`, repair polygon geometry before
#'   spatial predicates are evaluated.
#' @param max_distance_m Optional maximum nearest-assignment distance in metres.
#'   `NULL` permits unrestricted nearest assignment. Points farther than the
#'   supplied distance remain unmatched.
#'
#' @return An sf object containing one row per input point, with polygon
#'   attributes and spatial-assignment provenance.
#'
#' @export

sj_join <- function(
  points,
  polygons,
  id_col,
  name_col = NULL,
  join_mode = c("intersect", "nearest", "intersect_nearest"),
  repair_polygons = TRUE,
  max_distance_m = NULL
) {

  # 1. Validate arguments ---------------------------------------------------

  join_mode <- match.arg(join_mode)

  if (!inherits(points, "sf")) {
    stop("`points` must be an sf object.", call. = FALSE)
  }

  if (!inherits(polygons, "sf")) {
    stop("`polygons` must be an sf object.", call. = FALSE)
  }

  if (
    length(id_col) != 1L ||
    !is.character(id_col) ||
    is.na(id_col) ||
    !nzchar(id_col)
  ) {
    stop(
      "`id_col` must be one non-empty character value.",
      call. = FALSE
    )
  }

  if (
    !is.null(name_col) &&
      (
        length(name_col) != 1L ||
        !is.character(name_col) ||
        is.na(name_col) ||
        !nzchar(name_col)
      )
  ) {
    stop(
      "`name_col` must be NULL or one non-empty character value.",
      call. = FALSE
    )
  }

  if (!id_col %in% names(polygons)) {
    stop("`id_col` not found in polygons.", call. = FALSE)
  }

  if (
    !is.null(name_col) &&
      !name_col %in% names(polygons)
  ) {
    stop("`name_col` not found in polygons.", call. = FALSE)
  }

  if (any(is.na(polygons[[id_col]]))) {
    stop(
      "`id_col` cannot contain missing polygon identifiers.",
      call. = FALSE
    )
  }

  if (anyDuplicated(polygons[[id_col]]) > 0L) {
    stop(
      "`id_col` must uniquely identify polygon units.",
      call. = FALSE
    )
  }

  protected_output_cols <- c(
    id_col,
    name_col,
    "join_method",
    "join_distance_m"
  )

  protected_output_cols <-
    protected_output_cols[
      !is.na(protected_output_cols)
    ]

  existing_collisions <-
    intersect(
      protected_output_cols,
      names(points)
    )

  if (length(existing_collisions) > 0L) {
    stop(
      paste0(
        "Output column already exists in points: ",
        paste(existing_collisions, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (
    length(repair_polygons) != 1L ||
    !is.logical(repair_polygons) ||
    is.na(repair_polygons)
  ) {
    stop(
      "`repair_polygons` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.null(max_distance_m)) {

    if (
      length(max_distance_m) != 1L ||
      !is.numeric(max_distance_m) ||
      is.na(max_distance_m) ||
      !is.finite(max_distance_m) ||
      max_distance_m < 0
    ) {
      stop(
        "`max_distance_m` must be NULL or one finite non-negative numeric value.",
        call. = FALSE
      )
    }
  }


  # 2. Validate geometry ----------------------------------------------------

  point_types <-
    unique(
      as.character(
        sf::st_geometry_type(
          points,
          by_geometry = TRUE
        )
      )
    )

  polygon_types <-
    unique(
      as.character(
        sf::st_geometry_type(
          polygons,
          by_geometry = TRUE
        )
      )
    )

  if (
    length(point_types) > 0L &&
      !all(point_types %in% "POINT")
  ) {
    stop(
      "`points` must contain POINT geometries.",
      call. = FALSE
    )
  }

  if (
    length(polygon_types) > 0L &&
      !all(
        polygon_types %in%
          c("POLYGON", "MULTIPOLYGON")
      )
  ) {
    stop(
      "`polygons` must contain POLYGON or MULTIPOLYGON geometries.",
      call. = FALSE
    )
  }

  if (nrow(polygons) == 0L) {
    stop(
      "`polygons` must contain at least one feature.",
      call. = FALSE
    )
  }


  # 3. Validate CRS ---------------------------------------------------------

  point_crs <- sf::st_crs(points)
  polygon_crs <- sf::st_crs(polygons)

  if (is.na(point_crs)) {
    stop(
      "`points` must have a defined CRS.",
      call. = FALSE
    )
  }

  if (is.na(polygon_crs)) {
    stop(
      "`polygons` must have a defined CRS.",
      call. = FALSE
    )
  }

  if (point_crs != polygon_crs) {
    stop(
      "`points` and `polygons` must use the same CRS.",
      call. = FALSE
    )
  }


  # 4. Repair polygon geometry ---------------------------------------------

  if (repair_polygons) {
    polygons <-
      geom_repair(
        polygons,
        quiet = TRUE
      )
  }


  # 5. Prepare polygon lookup ----------------------------------------------

  keep_cols <- c(
    id_col,
    name_col
  )

  keep_cols <-
    keep_cols[
      !is.na(keep_cols)
    ]

  polygon_lookup <-
    polygons[
      ,
      keep_cols,
      drop = FALSE
    ]


  # 6. Initialise output ----------------------------------------------------

out <- points

out[[id_col]] <-
  polygons[[id_col]][
    rep(
      NA_integer_,
      nrow(points)
    )
  ]

if (!is.null(name_col)) {
  out[[name_col]] <-
    polygons[[name_col]][
      rep(
        NA_integer_,
        nrow(points)
      )
    ]
}

out$join_method <-
  rep(
    "unmatched",
    nrow(points)
  )

out$join_distance_m <-
  rep(
    NA_real_,
    nrow(points)
  )


  # 7. Helper: intersect assignment ----------------------------------------

  assign_intersections <- function(out) {

    hits <-
      sf::st_intersects(
        points,
        polygon_lookup
      )

    hit_count <-
      lengths(hits)

    ambiguous <-
      which(hit_count > 1L)

    if (length(ambiguous) > 0L) {
      stop(
        paste0(
          "Spatial assignment is ambiguous: ",
          length(ambiguous),
          " point(s) intersect more than one polygon."
        ),
        call. = FALSE
      )
    }

    matched <-
      which(hit_count == 1L)

    if (length(matched) > 0L) {

      polygon_idx <-
        vapply(
          hits[matched],
          `[`,
          integer(1),
          1L
        )

      out[[id_col]][matched] <-
        polygon_lookup[[id_col]][polygon_idx]

      if (!is.null(name_col)) {
        out[[name_col]][matched] <-
          polygon_lookup[[name_col]][polygon_idx]
      }

      out$join_method[matched] <-
        "intersect"

      out$join_distance_m[matched] <-
        0
    }

    out
  }


  # 8. Helper: nearest assignment ------------------------------------------

  assign_nearest <- function(
    out,
    rows
  ) {

    if (length(rows) == 0L) {
      return(out)
    }

    candidate_points <-
      points[rows, ]

    nearest_idx <-
      sf::st_nearest_feature(
        candidate_points,
        polygon_lookup
      )

    nearest_polygons <-
      polygon_lookup[
        nearest_idx,
      ]

    nearest_dist <-
      as.numeric(
        sf::st_distance(
          candidate_points,
          nearest_polygons,
          by_element = TRUE
        )
      )

    eligible <-
      rep(
        TRUE,
        length(rows)
      )

    if (!is.null(max_distance_m)) {
      eligible <-
        nearest_dist <=
        max_distance_m
    }

    # Retain nearest distance even where the assignment is rejected.
    out$join_distance_m[rows] <-
      nearest_dist

    assigned_rows <-
      rows[eligible]

    assigned_polygon_idx <-
      nearest_idx[eligible]

    if (length(assigned_rows) > 0L) {

      out[[id_col]][assigned_rows] <-
        polygon_lookup[[id_col]][assigned_polygon_idx]

      if (!is.null(name_col)) {
        out[[name_col]][assigned_rows] <-
          polygon_lookup[[name_col]][assigned_polygon_idx]
      }

      out$join_method[assigned_rows] <-
        "nearest"
    }

    out
  }


  # 9. Intersect ------------------------------------------------------------

  if (join_mode == "intersect") {

    out <-
      assign_intersections(out)

    return(out)
  }


  # 10. Nearest -------------------------------------------------------------

  if (join_mode == "nearest") {

    out <-
      assign_nearest(
        out,
        rows = seq_len(nrow(points))
      )

    return(out)
  }


  # 11. Intersect + nearest fallback ----------------------------------------

  if (join_mode == "intersect_nearest") {

    out <-
      assign_intersections(out)

    unmatched_rows <-
      which(
        out$join_method ==
          "unmatched"
      )

    out <-
      assign_nearest(
        out,
        rows = unmatched_rows
      )

    return(out)
  }
}