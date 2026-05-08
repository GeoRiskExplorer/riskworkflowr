# 1 ------------------------------------------------------------------------
# Null coalesce helper

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

