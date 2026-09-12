# 1 ------------------------------------------------------------------------
# risk_join_counts.R
# Purpose: Join validated count tables back to complete spatial units


#' Join event counts back to spatial units
#'
#' Joins one validated count row per analytical unit back to the complete
#' spatial-unit population. Units absent from the count table can be assigned
#' a chosen missing-count value, normally zero.
#'
#' The unit identifier is the authoritative join key. When `unit_name_col` is
#' supplied, names are validated for consistency but are not used as a second
#' join key.
#'
#' @param units An sf object containing polygon or hexbin analytical units.
#' @param counts A data frame containing one count row per represented unit.
#' @param unit_id_col Name of the shared unique unit identifier column.
#' @param unit_name_col Optional unit name column used for consistency QA.
#' @param count_col Name of the count column.
#' @param missing_count_value Value assigned to units absent from `counts`.
#'   Use `NULL` to preserve missing values.
#'
#' @return The original `units` sf object with the count column appended.
#'
#' @export

risk_join_counts <- function(
  units,
  counts,
  unit_id_col,
  unit_name_col = NULL,
  count_col = "event_count",
  missing_count_value = 0
) {

  # 1. Validate container types ---------------------------------------------

  if (!inherits(units, "sf")) {
    stop("`units` must be an sf object.", call. = FALSE)
  }

  if (!is.data.frame(counts)) {
    stop("`counts` must be a data frame.", call. = FALSE)
  }


  # 2. Validate names --------------------------------------------------------

  validate_name <- function(x, arg, allow_null = FALSE) {

    if (allow_null && is.null(x)) {
      return(invisible(TRUE))
    }

    if (
      length(x) != 1L ||
      !is.character(x) ||
      is.na(x) ||
      !nzchar(x)
    ) {
      stop(
        paste0("`", arg, "` must be one non-empty character value."),
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  validate_name(unit_id_col, "unit_id_col")
  validate_name(unit_name_col, "unit_name_col", allow_null = TRUE)
  validate_name(count_col, "count_col")

  if (!unit_id_col %in% names(units)) {
    stop("`unit_id_col` not found in units.", call. = FALSE)
  }

  if (!unit_id_col %in% names(counts)) {
    stop("`unit_id_col` not found in counts.", call. = FALSE)
  }

  if (!count_col %in% names(counts)) {
    stop("`count_col` not found in counts.", call. = FALSE)
  }

  if (count_col %in% names(units)) {
    stop(
      "`count_col` already exists in units.",
      call. = FALSE
    )
  }


  # 3. Validate unit identifiers --------------------------------------------

  if (any(is.na(units[[unit_id_col]]))) {
    stop(
      "`unit_id_col` cannot contain missing identifiers in units.",
      call. = FALSE
    )
  }

  if (anyDuplicated(units[[unit_id_col]]) > 0L) {
    stop(
      "`unit_id_col` must uniquely identify rows in units.",
      call. = FALSE
    )
  }

  if (any(is.na(counts[[unit_id_col]]))) {
    stop(
      "`unit_id_col` cannot contain missing identifiers in counts.",
      call. = FALSE
    )
  }

  if (anyDuplicated(counts[[unit_id_col]]) > 0L) {
    stop(
      "`counts` must contain at most one row per unit identifier.",
      call. = FALSE
    )
  }

  unknown_ids <-
    setdiff(
      counts[[unit_id_col]],
      units[[unit_id_col]]
    )

  if (length(unknown_ids) > 0L) {
    stop(
      "`counts` contains unit identifiers not present in `units`.",
      call. = FALSE
    )
  }


  # 4. Validate optional names ----------------------------------------------

  if (!is.null(unit_name_col)) {

    if (!unit_name_col %in% names(units)) {
      stop("`unit_name_col` not found in units.", call. = FALSE)
    }

    if (!unit_name_col %in% names(counts)) {
      stop("`unit_name_col` not found in counts.", call. = FALSE)
    }

    unit_name_lookup <-
      stats::setNames(
        as.character(units[[unit_name_col]]),
        as.character(units[[unit_id_col]])
      )

    expected_names <-
      unname(
        unit_name_lookup[
          as.character(
            counts[[unit_id_col]]
          )
        ]
      )

    observed_names <-
      as.character(
        counts[[unit_name_col]]
      )

    mismatch <-
      !is.na(expected_names) &
      !is.na(observed_names) &
      expected_names != observed_names

    if (any(mismatch)) {
      stop(
        "`unit_name_col` is inconsistent between counts and units.",
        call. = FALSE
      )
    }
  }


  # 5. Validate counts -------------------------------------------------------

  count_values <- counts[[count_col]]

  if (!is.numeric(count_values)) {
    stop("`count_col` must be numeric.", call. = FALSE)
  }

  if (any(is.na(count_values))) {
    stop(
      "`count_col` cannot contain missing values in counts.",
      call. = FALSE
    )
  }

  if (any(!is.finite(count_values))) {
    stop(
      "`count_col` must contain finite values.",
      call. = FALSE
    )
  }

  if (any(count_values < 0)) {
    stop(
      "`count_col` cannot contain negative values.",
      call. = FALSE
    )
  }

  if (any(count_values != floor(count_values))) {
    stop(
      "`count_col` must contain whole-number record counts.",
      call. = FALSE
    )
  }


  # 6. Validate missing-count fallback --------------------------------------

  if (!is.null(missing_count_value)) {

    if (
      length(missing_count_value) != 1L ||
      !is.numeric(missing_count_value) ||
      is.na(missing_count_value) ||
      !is.finite(missing_count_value)
    ) {
      stop(
        "`missing_count_value` must be NULL or one finite numeric value.",
        call. = FALSE
      )
    }
  }


  # 7. Join counts -----------------------------------------------------------

  count_lookup <-
    counts[
      ,
      c(
        unit_id_col,
        count_col
      ),
      drop = FALSE
    ]

  out <- units |>
    dplyr::left_join(
      count_lookup,
      by = unit_id_col
    )


  # 8. Populate zero / configured missing counts ----------------------------

  if (!is.null(missing_count_value)) {

    missing_rows <-
      is.na(
        out[[count_col]]
      )

    out[[count_col]][missing_rows] <-
      missing_count_value
  }


  # 9. Preserve whole-number type when appropriate --------------------------

  if (
    !is.null(missing_count_value) &&
      missing_count_value == floor(missing_count_value) &&
      all(
        out[[count_col]] ==
          floor(out[[count_col]])
      )
  ) {
    out[[count_col]] <-
      as.integer(
        out[[count_col]]
      )
  }

  out
}
