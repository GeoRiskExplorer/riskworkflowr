# ============================================================================
# geom_repair.R
# Purpose: Repair invalid sf geometry for reliable spatial processing
# ============================================================================


#' Repair invalid geometry for spatial processing
#'
#' Repairs invalid geometries using [sf::st_make_valid()] so downstream
#' spatial predicates, joins, and analytical workflows behave more reliably.
#'
#' @param x An `sf` object.
#' @param quiet Logical. If `TRUE` (default), suppress repair summary messages.
#'
#' @return An `sf` object with repaired geometry.
#'
#' A `geom_repair_summary` attribute is attached containing:
#'
#' - `n_features`
#' - `n_invalid_before`
#' - `n_unknown_before`
#' - `n_invalid_after`
#' - `n_unknown_after`
#'
#' @details
#' Geometry validity is assessed with [sf::st_is_valid()].
#'
#' If any geometry is invalid or has indeterminate validity, the geometry
#' column is passed to [sf::st_make_valid()].
#'
#' `st_make_valid()` may legitimately change geometry type where required to
#' produce valid geometry, for example from `POLYGON` to `MULTIPOLYGON`.
#' Attributes, row count, active geometry column, and CRS are preserved.
#'
#' Empty geometries are not automatically considered errors; their treatment
#' follows `sf` validity semantics.
#'
#' @examples
#' \dontrun{
#' repaired_sf <- geom_repair(my_sf)
#'
#' attr(
#'   repaired_sf,
#'   "geom_repair_summary"
#' )
#' }
#'
#' @export
geom_repair <- function(
  x,
  quiet = TRUE
) {

  # ==========================================================================
  # 01. VALIDATE INPUT
  # ==========================================================================

  if (!inherits(x, "sf")) {
    stop(
      "`x` must be an sf object.",
      call. = FALSE
    )
  }

  if (
    length(quiet) != 1L ||
    !is.logical(quiet) ||
    is.na(quiet)
  ) {
    stop(
      "`quiet` must be TRUE or FALSE.",
      call. = FALSE
    )
  }


  # ==========================================================================
  # 02. RECORD SPATIAL CONTRACT
  # ==========================================================================

  geom_col <- attr(
    x,
    "sf_column"
  )

  crs_before <- sf::st_crs(x)
  n_before <- nrow(x)


  # ==========================================================================
  # 03. VALIDITY BEFORE REPAIR
  # ==========================================================================

  valid_before <- sf::st_is_valid(x)

  n_invalid_before <-
    sum(
      valid_before %in% FALSE
    )

  n_unknown_before <-
    sum(
      is.na(valid_before)
    )


  # ==========================================================================
  # 04. REPAIR WHEN REQUIRED
  # ==========================================================================

  repair_required <-
    n_invalid_before > 0L ||
    n_unknown_before > 0L

  if (repair_required) {

    repaired_geometry <-
      sf::st_make_valid(
        sf::st_geometry(x)
      )

    sf::st_geometry(x) <-
      repaired_geometry
  }


  # ==========================================================================
  # 05. VALIDITY AFTER REPAIR
  # ==========================================================================

  valid_after <- sf::st_is_valid(x)

  n_invalid_after <-
    sum(
      valid_after %in% FALSE
    )

  n_unknown_after <-
    sum(
      is.na(valid_after)
    )


  # ==========================================================================
  # 06. INTERNAL PRESERVATION CONTRACT
  # ==========================================================================

  if (nrow(x) != n_before) {
    stop(
      "Geometry repair unexpectedly changed the number of features.",
      call. = FALSE
    )
  }

  if (!identical(
    sf::st_crs(x),
    crs_before
  )) {
    stop(
      "Geometry repair unexpectedly changed the CRS.",
      call. = FALSE
    )
  }

  if (!identical(
    attr(x, "sf_column"),
    geom_col
  )) {
    stop(
      "Geometry repair unexpectedly changed the active geometry column.",
      call. = FALSE
    )
  }


  # ==========================================================================
  # 07. QA SUMMARY
  # ==========================================================================

  summary <- list(
    n_features = nrow(x),
    n_invalid_before = n_invalid_before,
    n_unknown_before = n_unknown_before,
    n_invalid_after = n_invalid_after,
    n_unknown_after = n_unknown_after
  )

  attr(
    x,
    "geom_repair_summary"
  ) <- summary


  # ==========================================================================
  # 08. OPTIONAL CONSOLE SUMMARY
  # ==========================================================================

  if (!quiet) {

    message(
      "Geometry repair summary: ",
      n_invalid_before,
      " invalid before; ",
      n_invalid_after,
      " invalid after; ",
      n_unknown_before,
      " unknown before; ",
      n_unknown_after,
      " unknown after."
    )
  }


  # ==========================================================================
  # 09. RETURN
  # ==========================================================================

  x
}
