#' Identify distinctive risk categories by spatial unit
#'
#' Calculates category-specific comparative SMR-style values and identifies
#' the highest and optionally lowest distinctive category for each unit.
#'
#' @param data A data frame containing unit, category, count, and denominator columns.
#' @param unit_id_col Name of the spatial unit identifier column.
#' @param category_col Name of the category/group column.
#' @param observed_col Name of the observed count column. Defaults to `"event_count"`.
#' @param denominator_col Name of the denominator/exposure column.
#' @param min_count Minimum observed count required for a category to be considered.
#' @param include_lowest Logical. If TRUE, also returns the lowest eligible category.
#' @param highest_category_col Output column for highest category.
#' @param highest_smr_col Output column for highest SMR.
#' @param highest_count_col Output column for highest category count.
#' @param lowest_category_col Output column for lowest category.
#' @param lowest_smr_col Output column for lowest SMR.
#' @param lowest_count_col Output column for lowest category count.
#'
#' @return A data frame with one row per unit and distinctive category outputs.
#' 
#' #' The `insufficient_count_flag` column is `TRUE` where no category within
#' a unit met the minimum count threshold set by `min_count`.
#'
#' @details
#' This function is intended for exploratory grouped/category comparative
#' risk profiling. It identifies categories with the highest relative
#' observed-versus-expected value within each spatial unit.
#'
#' Results should be interpreted carefully where counts are low,
#' denominators are unstable, or categories are inconsistently coded.
#'
#' @examples
#' data <- data.frame(
#'   unit_id = c("A", "A", "B", "B"),
#'   category = c("Falls", "Water", "Falls", "Water"),
#'   event_count = c(10, 2, 3, 8),
#'   exposure = c(1000, 1000, 800, 800)
#' )
#'
#' risk_distinct_category(
#'   data = data,
#'   unit_id_col = "unit_id",
#'   category_col = "category",
#'   denominator_col = "exposure"
#' )
#'
#' @export
risk_distinct_category <- function(data,
                                   unit_id_col,
                                   category_col,
                                   observed_col = "event_count",
                                   denominator_col,
                                   min_count = 1,
                                   include_lowest = TRUE,
                                   highest_category_col = "highest_category",
                                   highest_smr_col = "highest_smr",
                                   highest_count_col = "highest_event_count",
                                   lowest_category_col = "lowest_category",
                                   lowest_smr_col = "lowest_smr",
                                   lowest_count_col = "lowest_event_count") {

  required_cols <- c(unit_id_col, category_col, observed_col, denominator_col)
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.numeric(data[[observed_col]])) {
    stop("`observed_col` must be numeric.", call. = FALSE)
  }

  if (!is.numeric(data[[denominator_col]])) {
    stop("`denominator_col` must be numeric.", call. = FALSE)
  }

  if (!is.numeric(min_count) || length(min_count) != 1 || min_count < 0) {
    stop("`min_count` must be a single non-negative number.", call. = FALSE)
  }

  x <- data

  # Category-specific global rates
  category_totals <- stats::aggregate(
    x = x[, c(observed_col, denominator_col)],
    by = list(category = x[[category_col]]),
    FUN = sum,
    na.rm = TRUE
  )

  names(category_totals) <- c(
    category_col,
    "category_observed_total",
    "category_denominator_total"
  )

  category_totals$category_reference_rate <- ifelse(
    category_totals$category_denominator_total > 0,
    category_totals$category_observed_total /
      category_totals$category_denominator_total,
    NA_real_
  )

  x <- merge(
    x,
    category_totals[, c(category_col, "category_reference_rate")],
    by = category_col,
    all.x = TRUE,
    sort = FALSE
  )

  x$expected_count <- x[[denominator_col]] * x$category_reference_rate

  x$category_smr <- ifelse(
    x$expected_count > 0,
    x[[observed_col]] / x$expected_count,
    NA_real_
  )

  x$eligible_category <- !is.na(x$category_smr) &
    x[[observed_col]] >= min_count

  eligible <- x[x$eligible_category, , drop = FALSE]

  units <- unique(x[[unit_id_col]])

  out <- data.frame(
    unit_id_tmp = units,
    stringsAsFactors = FALSE
  )

  names(out) <- unit_id_col

  highest <- lapply(units, function(u) {
    y <- eligible[eligible[[unit_id_col]] == u, , drop = FALSE]

    if (nrow(y) == 0) {
      return(data.frame(
        highest_category = NA_character_,
        highest_smr = NA_real_,
        highest_event_count = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    y <- y[order(-y$category_smr, -y[[observed_col]], y[[category_col]]), ]
    data.frame(
      highest_category = as.character(y[[category_col]][1]),
      highest_smr = y$category_smr[1],
      highest_event_count = y[[observed_col]][1],
      stringsAsFactors = FALSE
    )
  })

  highest <- do.call(rbind, highest)

  names(highest) <- c(
    highest_category_col,
    highest_smr_col,
    highest_count_col
  )

  out <- cbind(out, highest)

  if (isTRUE(include_lowest)) {
    lowest <- lapply(units, function(u) {
      y <- eligible[eligible[[unit_id_col]] == u, , drop = FALSE]

      if (nrow(y) == 0) {
        return(data.frame(
          lowest_category = NA_character_,
          lowest_smr = NA_real_,
          lowest_event_count = NA_real_,
          stringsAsFactors = FALSE
        ))
      }

      y <- y[order(y$category_smr, -y[[observed_col]], y[[category_col]]), ]
      data.frame(
        lowest_category = as.character(y[[category_col]][1]),
        lowest_smr = y$category_smr[1],
        lowest_event_count = y[[observed_col]][1],
        stringsAsFactors = FALSE
      )
    })

    lowest <- do.call(rbind, lowest)

    names(lowest) <- c(
      lowest_category_col,
      lowest_smr_col,
      lowest_count_col
    )

    out <- cbind(out, lowest)
  }

  category_counts <- stats::aggregate(
    x = list(category_count_used = eligible$eligible_category),
    by = list(unit_id_tmp = eligible[[unit_id_col]]),
    FUN = length
  )

  names(category_counts)[1] <- unit_id_col

  out <- merge(
    out,
    category_counts,
    by = unit_id_col,
    all.x = TRUE,
    sort = FALSE
  )

  out$category_count_used[is.na(out$category_count_used)] <- 0L
  out$insufficient_count_flag <- out$category_count_used == 0L

  out
}