# 1 ------------------------------------------------------------------------
# Repair invalid sf geometry

geom_repair <- function(x, quiet = FALSE) {

  if (!inherits(x, "sf")) {
    stop("`x` must be an sf object.", call. = FALSE)
  }

  geom_col <- attr(x, "sf_column")

  valid_before <- sf::st_is_valid(x)
  n_invalid_before <- sum(!valid_before, na.rm = TRUE)

  if (!quiet) {
    message("Invalid geometries before repair: ", n_invalid_before)
  }

  if (n_invalid_before > 0) {
    x[[geom_col]] <- sf::st_make_valid(x[[geom_col]])
    sf::st_geometry(x) <- geom_col
  }

  valid_after <- sf::st_is_valid(x)
  n_invalid_after <- sum(!valid_after, na.rm = TRUE)

  if (!quiet) {
    message("Invalid geometries after repair: ", n_invalid_after)
  }

  attr(x, "geom_repair_summary") <- list(
    n_features = nrow(x),
    n_invalid_before = n_invalid_before,
    n_invalid_after = n_invalid_after
  )

  x
}