#' Format an analytical cross-tab for reporting
#'
#' Converts long-form output from [risk_crosstab()] or [risk_crosstab_by()]
#' into a wide reporting table. This function is presentation-only and does
#' not alter the underlying analytical values.
#'
#' @param data Analytical cross-tab data.
#' @param rows Single character string naming the row variable.
#' @param cols Single character string naming the column variable.
#' @param display Either `"detailed"` or `"count"`.
#' @param missing_label Label used to display analytical missing categories.
#' @param suppress_below Optional positive whole-number disclosure threshold.
#'   Positive observed cells below this value are masked in presentation only.
#' @param row_total Logical. Add a `Total` column when `TRUE`. Totals are
#'   omitted when suppression is active because visible totals can permit
#'   inference of suppressed cells.
#'
#' @return A non-spatial wide data frame with reporting metadata retained as
#'   attributes.
#'
#' @export
risk_crosstab_table <- function(
  data,
  rows,
  cols,
  display = c("detailed", "count"),
  missing_label = "Unassigned",
  suppress_below = NULL,
  row_total = FALSE
) {

  # ---------------------------------------------------------------------------
  # 1. Validate
  # ---------------------------------------------------------------------------

  display <- match.arg(display)

  if (!inherits(data, "data.frame")) {
    stop("data must be a data frame.", call. = FALSE)
  }

  required <- c(
    rows,
    cols,
    "event_count",
    "annualised_count",
    "poisson_probability"
  )

  if (!all(required %in% names(data))) {
    stop(
      "data does not contain the required analytical cross-tab columns.",
      call. = FALSE
    )
  }

  if (!is.character(missing_label) ||
      length(missing_label) != 1L ||
      !nzchar(missing_label)) {
    stop("missing_label must be a single non-empty character value.", call. = FALSE)
  }

  if (!is.logical(row_total) || length(row_total) != 1L || is.na(row_total)) {
    stop("row_total must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.null(suppress_below)) {
    if (
      !is.numeric(suppress_below) ||
      length(suppress_below) != 1L ||
      is.na(suppress_below) ||
      !is.finite(suppress_below) ||
      suppress_below < 1 ||
      suppress_below != floor(suppress_below)
    ) {
      stop(
        "suppress_below must be NULL or a positive whole number.",
        call. = FALSE
      )
    }

    suppress_below <- as.integer(suppress_below)

    if (row_total) {
      warning(
        "Row totals are omitted when suppression is active because totals may permit inference of suppressed cells.",
        call. = FALSE
      )
      row_total <- FALSE
    }
  }

  # ---------------------------------------------------------------------------
  # 2. Reporting metadata
  # ---------------------------------------------------------------------------

  poisson_k <- NULL

  if ("poisson_k" %in% names(data)) {
    k_values <- unique(data$poisson_k[!is.na(data$poisson_k)])

    if (length(k_values) == 1L) {
      poisson_k <- as.integer(k_values)
    }
  }

  # ---------------------------------------------------------------------------
  # 3. Presentation labels
  # ---------------------------------------------------------------------------

  work <- data
  work$.row_display <- as.character(work[[rows]])
  work$.col_display <- as.character(work[[cols]])

  work$.row_display[is.na(work[[rows]])] <- missing_label
  work$.col_display[is.na(work[[cols]])] <- missing_label

  format_annualised <- function(x) {
    ifelse(
      is.na(x),
      NA_character_,
      formatC(x, format = "f", digits = 1)
    )
  }

  format_probability <- function(x) {
    ifelse(
      is.na(x),
      NA_character_,
      formatC(x, format = "f", digits = 3)
    )
  }

  work$.cell <- NA_character_

  for (i in seq_len(nrow(work))) {

    n <- work$event_count[i]
    annual <- work$annualised_count[i]
    prob <- work$poisson_probability[i]

    suppressed <- !is.null(suppress_below) &&
      !is.na(n) &&
      n > 0 &&
      n < suppress_below

    if (suppressed) {
      work$.cell[i] <- paste0("<", suppress_below)
      next
    }

    if (display == "count") {
      work$.cell[i] <- if (is.na(n)) NA_character_ else format(n, trim = TRUE)
      next
    }

    if (is.na(n)) {
      work$.cell[i] <- NA_character_
    } else if (n == 0) {
      work$.cell[i] <- "0 events"
    } else if (!is.na(prob) && !is.na(annual)) {
      work$.cell[i] <- paste0(
        "P = ", format_probability(prob),
        "\n",
        "n = ", format(n, trim = TRUE),
        " | ", format_annualised(annual), "/year"
      )
    } else if (!is.na(prob)) {
      work$.cell[i] <- paste0(
        "P = ", format_probability(prob),
        "\n",
        "n = ", format(n, trim = TRUE)
      )
    } else if (!is.na(annual)) {
      work$.cell[i] <- paste0(
        "n = ", format(n, trim = TRUE),
        " | ", format_annualised(annual), "/year"
      )
    } else {
      work$.cell[i] <- paste0(
        "n = ", format(n, trim = TRUE)
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 4. Wide reporting table
  # ---------------------------------------------------------------------------

  col_order <- unique(work$.col_display)

 out <- work |>
  dplyr::select(
    dplyr::all_of(
      c(
        ".row_display",
        ".col_display",
        ".cell"
      )
    )
  ) |>
  tidyr::pivot_wider(
    names_from = dplyr::all_of(".col_display"),
    values_from = dplyr::all_of(".cell")
  )

  names(out)[names(out) == ".row_display"] <- rows

  # Preserve analytical column ordering.
  display_cols <- intersect(col_order, names(out))
  out <- out[, c(rows, display_cols), drop = FALSE]

  # ---------------------------------------------------------------------------
  # 5. Optional row totals
  # ---------------------------------------------------------------------------

  if (row_total) {

    totals <- work |>
      dplyr::group_by(.data$.row_display) |>
      dplyr::summarise(
        .total_n = sum(.data$event_count, na.rm = TRUE),
        .total_annual = if (all(is.na(.data$annualised_count))) {
          NA_real_
        } else {
          sum(.data$annualised_count, na.rm = TRUE)
        },
        .groups = "drop"
      )

    if (display == "count") {
      totals$Total <- format(totals$.total_n, trim = TRUE)
    } else {
      totals$Total <- ifelse(
        is.na(totals$.total_annual),
        paste0("n = ", format(totals$.total_n, trim = TRUE)),
        paste0(
          "n = ", format(totals$.total_n, trim = TRUE),
          " | ", formatC(totals$.total_annual, format = "f", digits = 1),
          "/year"
        )
      )
    }

    total_lookup <- totals$Total
    names(total_lookup) <- totals$.row_display

    out$Total <- unname(total_lookup[as.character(out[[rows]])])
  }

  # ---------------------------------------------------------------------------
  # 6. Retain lightweight reporting metadata
  # ---------------------------------------------------------------------------

  attr(out, "display") <- display
  attr(out, "poisson_k") <- poisson_k
  attr(out, "suppressed_below") <- suppress_below

  rownames(out) <- NULL
  out
}
