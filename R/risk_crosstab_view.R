#' Render a polished cross-tab reporting table
#'
#' Creates a restrained APA-inspired `gt` table from output produced by
#' [risk_crosstab_table()]. The analytical Poisson threshold is read from the
#' table metadata so detailed-table notes stay aligned with the calculation.
#'
#' @param data A wide reporting table from [risk_crosstab_table()].
#' @param rows Character string naming the row-label column.
#' @param group Optional character string naming a row-group column.
#' @param title Optional table title.
#' @param subtitle Optional table subtitle.
#' @param note Optional footer note. When `NULL`, a standard analytical note is
#'   generated automatically for detailed Poisson tables.
#' @param total_col Character string naming the total column.
#'
#' @return A `gt_tbl`.
#'
#' @export
risk_crosstab_view <- function(
  data,
  rows,
  group = NULL,
  title = NULL,
  subtitle = NULL,
  note = NULL,
  total_col = "Total"
) {

  # ---------------------------------------------------------------------------
  # 1. Validate
  # ---------------------------------------------------------------------------

  if (!requireNamespace("gt", quietly = TRUE)) {
    stop(
      "Package 'gt' is required for risk_crosstab_view().",
      call. = FALSE
    )
  }

  if (!inherits(data, "data.frame")) {
    stop("data must be a data frame.", call. = FALSE)
  }

  if (!is.character(rows) || length(rows) != 1L || !rows %in% names(data)) {
    stop("rows must name a column present in data.", call. = FALSE)
  }

  if (!is.null(group)) {
    if (!is.character(group) ||
        length(group) != 1L ||
        !group %in% names(data)) {
      stop("group must be NULL or name a column present in data.", call. = FALSE)
    }
  }

  # ---------------------------------------------------------------------------
  # 2. Automatic analytical note
  # ---------------------------------------------------------------------------

  if (is.null(note)) {

    display_mode <- attr(data, "display")
    poisson_k <- attr(data, "poisson_k")

    if (
      identical(display_mode, "detailed") &&
      length(poisson_k) == 1L &&
      !is.null(poisson_k) &&
      !is.na(poisson_k)
    ) {
      note <- paste0(
        "Note. P is the Poisson exceedance probability, P(X >= ",
        poisson_k,
        "), based on the supplied expected count. ",
        "n is the observed event count; /year is annualised."
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 3. Build gt object
  # ---------------------------------------------------------------------------

  if (is.null(group)) {
    tab <- gt::gt(
      data = data,
      rowname_col = rows
    )
  } else {
    tab <- gt::gt(
      data = data,
      rowname_col = rows,
      groupname_col = group
    )
  }

  if (!is.null(title) || !is.null(subtitle)) {
    tab <- gt::tab_header(
      tab,
      title = title,
      subtitle = subtitle
    )
  }

  if (!is.null(note)) {
    tab <- gt::tab_source_note(
      tab,
      source_note = note
    )
  }

  # Render embedded line breaks in detailed cells.
  value_cols <- setdiff(names(data), c(rows, group))

  if (length(value_cols) > 0L) {
    tab <- gt::fmt_markdown(
      tab,
      columns = dplyr::all_of(value_cols)
    )
  }

  # ---------------------------------------------------------------------------
  # 4. Restrained APA-inspired styling
  # ---------------------------------------------------------------------------

  tab <- gt::opt_table_lines(
    tab,
    extent = "default"
  )

  tab <- gt::tab_options(
    tab,
    table.font.size = gt::px(13),
    heading.align = "left",
    heading.title.font.size = gt::px(18),
    heading.subtitle.font.size = gt::px(13),
    column_labels.font.weight = "bold",
    column_labels.border.top.width = gt::px(1),
    column_labels.border.bottom.width = gt::px(1),
    table_body.border.bottom.width = gt::px(1),
    row_group.font.weight = "bold",
    row_group.border.top.width = gt::px(1),
    source_notes.font.size = gt::px(11),
    source_notes.padding = gt::px(8),
    data_row.padding = gt::px(7)
  )

  if (length(value_cols) > 0L) {
    tab <- gt::cols_align(
      tab,
      align = "center",
      columns = dplyr::all_of(value_cols)
    )
  }

  if (total_col %in% names(data)) {
    tab <- gt::tab_style(
      tab,
      style = list(
        gt::cell_text(weight = "bold"),
        gt::cell_fill(color = "#F2F2F2")
      ),
      locations = gt::cells_body(
        columns = dplyr::all_of(total_col)
      )
    )

    tab <- gt::tab_style(
      tab,
      style = list(
        gt::cell_text(weight = "bold"),
        gt::cell_fill(color = "#F2F2F2")
      ),
      locations = gt::cells_column_labels(
        columns = dplyr::all_of(total_col)
      )
    )
  }

  tab
}
