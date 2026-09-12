# ============================================================================
# risk_distinct_category.R
# Purpose: Identify distinctive risk categories by analytical unit
# ============================================================================


#' Identify distinctive risk categories by spatial unit
#'
#' Calculates category-specific comparative SMR-style values and identifies
#' the highest and optionally lowest distinctive category for each unit.
#'
#' @param data A data frame or sf object containing one row per
#'   unit-category combination.
#' @param unit_id_col Name of the spatial unit identifier column.
#' @param category_col Name of the category/group column.
#' @param observed_col Name of the observed count column. Defaults to
#'   `"event_count"`.
#' @param denominator_col Name of the denominator/exposure column.
#' @param min_count Minimum observed count required for a category to be
#'   considered. Must be one non-negative whole number.
#' @param include_lowest Logical. If TRUE, also returns the lowest eligible
#'   category.
#' @param highest_category_col Output column for highest category.
#' @param highest_smr_col Output column for highest comparative SMR.
#' @param highest_count_col Output column for highest category count.
#' @param lowest_category_col Output column for lowest category.
#' @param lowest_smr_col Output column for lowest comparative SMR.
#' @param lowest_count_col Output column for lowest category count.
#'
#' @return A non-spatial data frame with one row per unit and distinctive
#'   category outputs.
#'
#' The `insufficient_count_flag` column is `TRUE` where no category within
#' a unit met the minimum count threshold set by `min_count`.
#'
#' @details
#' The input must contain one row per unit-category combination. Duplicate
#' unit-category rows are rejected because summing denominators across
#' duplicated rows can distort category-specific reference rates.
#'
#' For each category, a reference rate is calculated across all units as:
#'
#' \deqn{reference\ rate_c = \frac{\sum observed_c}{\sum denominator_c}}
#'
#' The expected count for each unit-category row is:
#'
#' \deqn{expected_{uc} = denominator_{uc} \times reference\ rate_c}
#'
#' and the comparative SMR-style value is:
#'
#' \deqn{SMR_{uc} = \frac{observed_{uc}}{expected_{uc}}}
#'
#' Categories are eligible for ranking when the comparative SMR is defined
#' and observed count is at least `min_count`.
#'
#' Ties are resolved deterministically by:
#'
#' 1. comparative SMR,
#' 2. observed count,
#' 3. category label.
#'
#' Results should be interpreted carefully where counts are low,
#' denominators are unstable, or categories are inconsistently coded.
#'
#' @references
#' Boscoe, F. P., & Pradhan, E. (2015). The Most Distinctive Causes
#' of Death by State, 2001-2010. Preventing Chronic Disease, 12, E75.
#' https://doi.org/10.5888/pcd12.140395
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
risk_distinct_category <- function(
  data,
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
  lowest_count_col = "lowest_event_count"
) {

  # ==========================================================================
  # 01. VALIDATE CONTAINER
  # ==========================================================================

  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data frame or sf object.",
      call. = FALSE
    )
  }


  # ==========================================================================
  # 02. VALIDATE COLUMN-NAME ARGUMENTS
  # ==========================================================================

  validate_name <- function(
    x,
    arg
  ) {

    if (
      length(x) != 1L ||
      !is.character(x) ||
      is.na(x) ||
      !nzchar(x)
    ) {
      stop(
        paste0(
          "`",
          arg,
          "` must be one non-empty character value."
        ),
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  validate_name(
    unit_id_col,
    "unit_id_col"
  )

  validate_name(
    category_col,
    "category_col"
  )

  validate_name(
    observed_col,
    "observed_col"
  )

  validate_name(
    denominator_col,
    "denominator_col"
  )

  output_cols <- c(
    highest_category_col,
    highest_smr_col,
    highest_count_col,
    if (isTRUE(include_lowest)) {
      c(
        lowest_category_col,
        lowest_smr_col,
        lowest_count_col
      )
    },
    "category_count_used",
    "insufficient_count_flag"
  )

  invisible(
    lapply(
      output_cols,
      validate_name,
      arg = "output column name"
    )
  )

  if (anyDuplicated(output_cols) > 0L) {
    stop(
      "Output column names must be unique.",
      call. = FALSE
    )
  }

  required_cols <- c(
    unit_id_col,
    category_col,
    observed_col,
    denominator_col
  )

  if (anyDuplicated(required_cols) > 0L) {
    stop(
      "Input role columns must be distinct.",
      call. = FALSE
    )
  }

  missing_cols <- setdiff(
    required_cols,
    names(data)
  )

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required column(s): ",
      paste(
        missing_cols,
        collapse = ", "
      ),
      call. = FALSE
    )
  }


  # ==========================================================================
  # 03. VALIDATE CONTROL ARGUMENTS
  # ==========================================================================

  if (
    length(min_count) != 1L ||
    !is.numeric(min_count) ||
    is.na(min_count) ||
    !is.finite(min_count) ||
    min_count < 0 ||
    min_count != floor(min_count)
  ) {
    stop(
      "`min_count` must be one non-negative whole number.",
      call. = FALSE
    )
  }

  if (
    length(include_lowest) != 1L ||
    !is.logical(include_lowest) ||
    is.na(include_lowest)
  ) {
    stop(
      "`include_lowest` must be TRUE or FALSE.",
      call. = FALSE
    )
  }


  # ==========================================================================
  # 04. PREPARE NON-SPATIAL ANALYTICAL DATA
  # ==========================================================================

  x <-
    if (inherits(data, "sf")) {
      sf::st_drop_geometry(data)
    } else {
      data
    }


  # ==========================================================================
  # 05. VALIDATE IDENTIFIERS AND CATEGORIES
  # ==========================================================================

  if (any(is.na(x[[unit_id_col]]))) {
    stop(
      "`unit_id_col` cannot contain missing values.",
      call. = FALSE
    )
  }

  if (any(is.na(x[[category_col]]))) {
    stop(
      "`category_col` cannot contain missing values.",
      call. = FALSE
    )
  }

  duplicate_key <-
    duplicated(
      x[
        ,
        c(
          unit_id_col,
          category_col
        ),
        drop = FALSE
      ]
    ) |
    duplicated(
      x[
        ,
        c(
          unit_id_col,
          category_col
        ),
        drop = FALSE
      ],
      fromLast = TRUE
    )

  if (any(duplicate_key)) {
    stop(
      "`data` must contain at most one row per unit-category combination.",
      call. = FALSE
    )
  }


  # ==========================================================================
  # 06. VALIDATE OBSERVED COUNTS
  # ==========================================================================

  observed <- x[[observed_col]]

  if (!is.numeric(observed)) {
    stop(
      "`observed_col` must be numeric.",
      call. = FALSE
    )
  }

  if (any(is.na(observed))) {
    stop(
      "`observed_col` cannot contain missing values.",
      call. = FALSE
    )
  }

  if (any(!is.finite(observed))) {
    stop(
      "`observed_col` must contain finite values.",
      call. = FALSE
    )
  }

  if (any(observed < 0)) {
    stop(
      "`observed_col` cannot contain negative values.",
      call. = FALSE
    )
  }

  if (any(observed != floor(observed))) {
    stop(
      "`observed_col` must contain whole-number counts.",
      call. = FALSE
    )
  }


  # ==========================================================================
  # 07. VALIDATE DENOMINATORS
  # ==========================================================================

  denominator <- x[[denominator_col]]

  if (!is.numeric(denominator)) {
    stop(
      "`denominator_col` must be numeric.",
      call. = FALSE
    )
  }

  if (any(is.na(denominator))) {
    stop(
      "`denominator_col` cannot contain missing values.",
      call. = FALSE
    )
  }

  if (any(!is.finite(denominator))) {
    stop(
      "`denominator_col` must contain finite values.",
      call. = FALSE
    )
  }

  if (any(denominator < 0)) {
    stop(
      "`denominator_col` cannot contain negative values.",
      call. = FALSE
    )
  }


  # ==========================================================================
  # 08. ZERO-ROW INPUT
  # ==========================================================================

  if (nrow(x) == 0L) {

    out <- x[
      ,
      unit_id_col,
      drop = FALSE
    ]

    out[[highest_category_col]] <- character()
    out[[highest_smr_col]] <- numeric()
    out[[highest_count_col]] <- integer()

    if (isTRUE(include_lowest)) {
      out[[lowest_category_col]] <- character()
      out[[lowest_smr_col]] <- numeric()
      out[[lowest_count_col]] <- integer()
    }

    out$category_count_used <- integer()
    out$insufficient_count_flag <- logical()

    return(out)
  }


  # ==========================================================================
  # 09. CATEGORY-SPECIFIC REFERENCE RATES
  # ==========================================================================

  categories <-
    unique(
      x[[category_col]]
    )

  category_totals <-
    lapply(
      categories,
      function(category_value) {

        idx <-
          x[[category_col]] ==
          category_value

        observed_total <-
          sum(
            x[[observed_col]][idx]
          )

        denominator_total <-
          sum(
            x[[denominator_col]][idx]
          )

        data.frame(
          category_tmp =
            as.character(category_value),
          category_observed_total =
            observed_total,
          category_denominator_total =
            denominator_total,
          category_reference_rate =
            if (
              denominator_total > 0
            ) {
              observed_total /
                denominator_total
            } else {
              NA_real_
            },
          stringsAsFactors = FALSE
        )
      }
    ) |>
    do.call(
      what = rbind
    )

  names(category_totals)[1] <-
    category_col

  x[[category_col]] <-
    as.character(
      x[[category_col]]
    )

  x <-
    dplyr::left_join(
      x,
      category_totals[
        ,
        c(
          category_col,
          "category_reference_rate"
        ),
        drop = FALSE
      ],
      by = category_col
    )


  # ==========================================================================
  # 10. EXPECTED COUNTS AND COMPARATIVE SMR
  # ==========================================================================

  x$expected_count <-
    x[[denominator_col]] *
    x$category_reference_rate

  x$category_smr <-
    ifelse(
      !is.na(x$expected_count) &
        x$expected_count > 0,
      x[[observed_col]] /
        x$expected_count,
      NA_real_
    )

  x$eligible_category <-
    !is.na(x$category_smr) &
    x[[observed_col]] >=
      min_count


  # ==========================================================================
  # 11. PRESERVE AUTHORITATIVE UNIT ORDER
  # ==========================================================================

  units <-
    unique(
      x[[unit_id_col]]
    )

  out <-
    data.frame(
      unit_id_tmp = units,
      stringsAsFactors = FALSE
    )

  names(out) <-
    unit_id_col


  # ==========================================================================
  # 12. RANK DISTINCTIVE CATEGORIES
  # ==========================================================================

  extract_ranked <- function(
    unit_value,
    highest = TRUE
  ) {

    y <-
      x[
        x[[unit_id_col]] ==
          unit_value &
          x$eligible_category,
        ,
        drop = FALSE
      ]

    if (nrow(y) == 0L) {
      return(
        list(
          category = NA_character_,
          smr = NA_real_,
          count = NA_integer_
        )
      )
    }

    if (isTRUE(highest)) {
      ord <-
        order(
          -y$category_smr,
          -y[[observed_col]],
          as.character(
            y[[category_col]]
          )
        )
    } else {
      ord <-
        order(
          y$category_smr,
          -y[[observed_col]],
          as.character(
            y[[category_col]]
          )
        )
    }

    y <- y[ord, , drop = FALSE]

    list(
      category =
        as.character(
          y[[category_col]][1]
        ),
      smr =
        y$category_smr[1],
      count =
        as.integer(
          y[[observed_col]][1]
        )
    )
  }


  # ==========================================================================
  # 13. HIGHEST DISTINCTIVE CATEGORY
  # ==========================================================================

  highest <-
    lapply(
      units,
      extract_ranked,
      highest = TRUE
    )

  out[[highest_category_col]] <-
    vapply(
      highest,
      `[[`,
      character(1),
      "category"
    )

  out[[highest_smr_col]] <-
    vapply(
      highest,
      `[[`,
      numeric(1),
      "smr"
    )

  out[[highest_count_col]] <-
    vapply(
      highest,
      `[[`,
      integer(1),
      "count"
    )


  # ==========================================================================
  # 14. LOWEST DISTINCTIVE CATEGORY
  # ==========================================================================

  if (isTRUE(include_lowest)) {

    lowest <-
      lapply(
        units,
        extract_ranked,
        highest = FALSE
      )

    out[[lowest_category_col]] <-
      vapply(
        lowest,
        `[[`,
        character(1),
        "category"
      )

    out[[lowest_smr_col]] <-
      vapply(
        lowest,
        `[[`,
        numeric(1),
        "smr"
      )

    out[[lowest_count_col]] <-
      vapply(
        lowest,
        `[[`,
        integer(1),
        "count"
      )
  }


  # ==========================================================================
  # 15. CATEGORY ELIGIBILITY COUNTS
  # ==========================================================================

  out$category_count_used <-
    vapply(
      units,
      function(unit_value) {
        sum(
          x[[unit_id_col]] ==
            unit_value &
            x$eligible_category
        )
      },
      integer(1)
    )

  out$insufficient_count_flag <-
    out$category_count_used ==
    0L


  # ==========================================================================
  # 16. RETURN
  # ==========================================================================

  out
}

