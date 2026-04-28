# 1 ------------------------------------------------------------------------
# Get active geometry column name

geom_col_name <- function(x) {
  geom_col <- attr(x, "sf_column")

  if (is.null(geom_col) || !geom_col %in% names(x)) {
    stop("No valid sf geometry column found.", call. = FALSE)
  }

  geom_col
}