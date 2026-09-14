# =============================================================================
# RISK EXPORT WORKBOOK
# =============================================================================


#' Export risk analysis outputs to a styled Excel workbook
#'
#' Creates a professional multi-sheet Excel workbook from one or more
#' analytical tables. The workbook can include a summary sheet, data
#' dictionary, methods and quality-assurance information.
#'
#' @param tables Named list of data frames or tibbles to export.
#' @param path Output `.xlsx` file path.
#' @param title Optional workbook title.
#' @param description Optional workbook subtitle or description.
#' @param context Optional workbook context object or compatible named list.
#' @param data_dictionary Optional data frame describing workbook fields.
#' @param methods Optional methods data frame.
#' @param qa Optional QA data frame.
#' @param group_cols Optional named character vector identifying grouping
#'   columns for selected analytical tables. Names must match names in
#'   `tables`, for example
#'   `c(Grouped_Counts = "operation", Grouped_Poisson = "operation")`.
#' @param total_col Name of a totals column to emphasise when present.
#' @param overwrite Logical. Overwrite an existing workbook?
#'
#' @return The normalised workbook path, invisibly.
#'
#' @export
risk_export_workbook <- function(
  tables,
  path,
  title = NULL,
  description = NULL,
  context = NULL,
  data_dictionary = NULL,
  methods = NULL,
  qa = NULL,
  group_cols = NULL,
  total_col = "Total",
  overwrite = FALSE
) {

  # ---------------------------------------------------------------------------
  # 1. Dependencies
  # ---------------------------------------------------------------------------

  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop(
      "Package 'openxlsx2' is required to export Excel workbooks.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 2. Validate tables
  # ---------------------------------------------------------------------------

  if (
  !is.list(tables) ||
  inherits(tables, "data.frame") ||
  length(tables) == 0L
) {
  stop(
    "tables must be a non-empty named list.",
    call. = FALSE
  )
}

  if (
    is.null(names(tables)) ||
    any(!nzchar(names(tables))) ||
    anyDuplicated(names(tables))
  ) {
    stop(
      "tables must have unique, non-empty names.",
      call. = FALSE
    )
  }

  valid_tables <- vapply(
    tables,
    inherits,
    logical(1),
    what = "data.frame"
  )

  if (!all(valid_tables)) {
    stop(
      "Every element of tables must be a data frame or tibble.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 3. Validate path
  # ---------------------------------------------------------------------------

  if (
    !is.character(path) ||
    length(path) != 1L ||
    is.na(path) ||
    !nzchar(path)
  ) {
    stop(
      "path must be a single non-empty character value.",
      call. = FALSE
    )
  }

  if (tolower(tools::file_ext(path)) != "xlsx") {
    stop(
      "path must use the .xlsx extension.",
      call. = FALSE
    )
  }

  if (file.exists(path) && !isTRUE(overwrite)) {
    stop(
      "Output workbook already exists. Set overwrite = TRUE to replace it.",
      call. = FALSE
    )
  }

  output_dir <- dirname(path)

  if (!dir.exists(output_dir)) {
    dir.create(
      output_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 4. Validate optional frames
  # ---------------------------------------------------------------------------

  optional_frames <- list(
    data_dictionary = data_dictionary,
    methods = methods,
    qa = qa
  )

  invalid_optional <- vapply(
    optional_frames,
    function(x) {
      !is.null(x) && !inherits(x, "data.frame")
    },
    logical(1)
  )

  if (any(invalid_optional)) {
    stop(
      paste0(
        names(optional_frames)[invalid_optional][1],
        " must be NULL or a data frame."
      ),
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 5. Validate group-column specification
  # ---------------------------------------------------------------------------

  if (!is.null(group_cols)) {

    if (
      !is.character(group_cols) ||
      is.null(names(group_cols)) ||
      any(!nzchar(names(group_cols)))
    ) {
      stop(
        "group_cols must be a named character vector.",
        call. = FALSE
      )
    }

    unknown_tables <- setdiff(
      names(group_cols),
      names(tables)
    )

    if (length(unknown_tables) > 0L) {
      stop(
        "All names in group_cols must match names in tables.",
        call. = FALSE
      )
    }

    for (table_name in names(group_cols)) {

      group_col <- group_cols[[table_name]]

      if (
        length(group_col) != 1L ||
        !group_col %in% names(tables[[table_name]])
      ) {
        stop(
          paste0(
            "Grouping column '",
            group_col,
            "' was not found in table '",
            table_name,
            "'."
          ),
          call. = FALSE
        )
      }
    }
  }


  # ---------------------------------------------------------------------------
  # 6. Workbook metadata
  # ---------------------------------------------------------------------------

  package_version <- tryCatch(
    as.character(
      utils::packageVersion("riskworkflowr")
    ),
    error = function(e) {
      "development"
    }
  )

generated_date <- sub(
  "^0",
  "",
  format(
    Sys.Date(),
    "%d %B %Y"
  )
)

  if (is.null(title)) {
    title <- "Risk Analysis Workbook"
  }

  if (is.null(description)) {
    description <- "Analytical outputs prepared using riskworkflowr."
  }


  # ---------------------------------------------------------------------------
  # 7. Workbook
  # ---------------------------------------------------------------------------

  wb <- openxlsx2::wb_workbook(
    creator = "riskworkflowr"
  )


  # ---------------------------------------------------------------------------
  # 8. Summary
  # ---------------------------------------------------------------------------

  if (!is.null(context)) {

    summary_sections <- .risk_xlsx_context_sections(
      context = context,
      generated_date = generated_date,
      package_version = package_version
    )

    wb <- .risk_xlsx_add_summary(
      wb = wb,
      title = title,
      description = description,
      sections = summary_sections
    )
  }


  # ---------------------------------------------------------------------------
  # 9. Analytical tables
  # ---------------------------------------------------------------------------

  for (i in seq_along(tables)) {

    table_name <- names(tables)[[i]]

    table_data <- tables[[i]]

    group_col <- NULL

    if (
      !is.null(group_cols) &&
      table_name %in% names(group_cols)
    ) {
      group_col <- group_cols[[table_name]]
    }

    sheet_name <- .risk_xlsx_sheet_name(
      paste0(
        sprintf("%02d", i),
        "_",
        table_name
      )
    )

    wb <- .risk_xlsx_add_table_sheet(
      wb = wb,
      sheet = sheet_name,
      title = .risk_xlsx_pretty_name(table_name),
      subtitle = description,
      data = table_data,
      group_col = group_col,
      total_col = total_col
    )
  }


  # ---------------------------------------------------------------------------
  # 10. Supporting sheets
  # ---------------------------------------------------------------------------

  next_index <- length(tables) + 1L

  if (!is.null(data_dictionary)) {

    wb <- .risk_xlsx_add_table_sheet(
      wb = wb,
      sheet = .risk_xlsx_sheet_name(
        paste0(
          sprintf("%02d", next_index),
          "_Data_Dictionary"
        )
      ),
      title = "Data Dictionary",
      subtitle = paste(
        "Definitions and interpretation guidance for fields represented",
        "in this workbook."
      ),
      data = data_dictionary,
      text_heavy = TRUE
    )

    next_index <- next_index + 1L
  }


  if (!is.null(methods)) {

    wb <- .risk_xlsx_add_table_sheet(
      wb = wb,
      sheet = .risk_xlsx_sheet_name(
        paste0(
          sprintf("%02d", next_index),
          "_Methods"
        )
      ),
      title = "Methods",
      subtitle = paste(
        "High-level analytical methods used to produce the workbook",
        "outputs."
      ),
      data = methods,
      text_heavy = TRUE
    )

    next_index <- next_index + 1L
  }


  if (!is.null(qa)) {

    wb <- .risk_xlsx_add_table_sheet(
      wb = wb,
      sheet = .risk_xlsx_sheet_name(
        paste0(
          sprintf("%02d", next_index),
          "_QA"
        )
      ),
      title = "Quality Assurance",
      subtitle = "Key analytical and structural quality-assurance checks.",
      data = qa,
      qa_sheet = TRUE
    )
  }


  # ---------------------------------------------------------------------------
  # 11. Save
  # ---------------------------------------------------------------------------

  openxlsx2::wb_save(
    wb,
    file = path,
    overwrite = overwrite
  )

  if (!file.exists(path) || file.info(path)$size <= 0) {
    stop(
      "Workbook export did not produce a valid output file.",
      call. = FALSE
    )
  }

  invisible(
    normalizePath(
      path,
      winslash = "/",
      mustWork = TRUE
    )
  )
}


# =============================================================================
# INTERNAL HELPERS
# =============================================================================


# -----------------------------------------------------------------------------
# A. Colours
# -----------------------------------------------------------------------------

.risk_xlsx_palette <- function() {

  list(
    title = openxlsx2::wb_color(hex = "FF1F4E78"),
    section = openxlsx2::wb_color(hex = "FF4472C4"),
    label = openxlsx2::wb_color(hex = "FFD9EAF7"),
    white = openxlsx2::wb_color(hex = "FFFFFFFF"),
    dark = openxlsx2::wb_color(hex = "FF262626"),
    mid_dark = openxlsx2::wb_color(hex = "FF404040"),
    mid = openxlsx2::wb_color(hex = "FF595959"),
    light_text = openxlsx2::wb_color(hex = "FF666666"),
    alt_row = openxlsx2::wb_color(hex = "FFF7F9FC"),
    total = openxlsx2::wb_color(hex = "FFE7E6E6"),
    group = openxlsx2::wb_color(hex = "FFEAF2F8"),
    pass = openxlsx2::wb_color(hex = "FFE2F0D9")
  )
}


# -----------------------------------------------------------------------------
# B. Sheet names
# -----------------------------------------------------------------------------

.risk_xlsx_sheet_name <- function(x) {

  x <- gsub(
    "[\\\\/:*?\\[\\]]",
    "_",
    x
  )

  x <- substr(
    x,
    1L,
    31L
  )

  if (!nzchar(x)) {
    x <- "Sheet"
  }

  x
}


.risk_xlsx_pretty_name <- function(x) {

  x <- gsub(
    "_",
    " ",
    x,
    fixed = TRUE
  )

  tools::toTitleCase(x)
}


# -----------------------------------------------------------------------------
# C. Title styling
# -----------------------------------------------------------------------------

.risk_xlsx_style_title <- function(
  wb,
  sheet,
  dims
) {

  pal <- .risk_xlsx_palette()

  wb <- openxlsx2::wb_add_fill(
    wb,
    sheet = sheet,
    dims = dims,
    color = pal$title,
    pattern = "solid"
  )

  wb <- openxlsx2::wb_add_font(
    wb,
    sheet = sheet,
    dims = dims,
    name = "Aptos",
    size = 18,
    bold = TRUE,
    color = pal$white
  )

  openxlsx2::wb_add_cell_style(
    wb,
    sheet = sheet,
    dims = dims,
    vertical = "center",
    wrap_text = TRUE
  )
}


.risk_xlsx_style_subtitle <- function(
  wb,
  sheet,
  dims
) {

  pal <- .risk_xlsx_palette()

  wb <- openxlsx2::wb_add_font(
    wb,
    sheet = sheet,
    dims = dims,
    name = "Aptos",
    size = 10,
    color = pal$mid
  )

  openxlsx2::wb_add_cell_style(
    wb,
    sheet = sheet,
    dims = dims,
    vertical = "center",
    wrap_text = TRUE
  )
}


# -----------------------------------------------------------------------------
# D. Table styling
# -----------------------------------------------------------------------------

.risk_xlsx_style_header <- function(
  wb,
  sheet,
  dims
) {

  pal <- .risk_xlsx_palette()

  wb <- openxlsx2::wb_add_fill(
    wb,
    sheet = sheet,
    dims = dims,
    color = pal$section,
    pattern = "solid"
  )

  wb <- openxlsx2::wb_add_font(
    wb,
    sheet = sheet,
    dims = dims,
    name = "Aptos",
    size = 10,
    bold = TRUE,
    color = pal$white
  )

  openxlsx2::wb_add_cell_style(
    wb,
    sheet = sheet,
    dims = dims,
    horizontal = "center",
    vertical = "center",
    wrap_text = TRUE
  )
}


.risk_xlsx_style_body <- function(
  wb,
  sheet,
  dims,
  wrap = FALSE
) {

  pal <- .risk_xlsx_palette()

  wb <- openxlsx2::wb_add_font(
    wb,
    sheet = sheet,
    dims = dims,
    name = "Aptos",
    size = 10,
    color = pal$dark
  )

  openxlsx2::wb_add_cell_style(
    wb,
    sheet = sheet,
    dims = dims,
    vertical = "top",
    wrap_text = wrap
  )
}


# -----------------------------------------------------------------------------
# E. Grouped presentation
# -----------------------------------------------------------------------------

.risk_xlsx_prepare_grouped <- function(
  data,
  group_col
) {

  if (is.null(group_col) || nrow(data) == 0L) {
    return(
      list(
        data = data,
        starts = integer()
      )
    )
  }

  group_values <- as.character(
    data[[group_col]]
  )

  starts <- which(
    c(
      TRUE,
      group_values[-1L] != group_values[-length(group_values)]
    )
  )

  display_data <- data

  repeated <- seq_len(nrow(data)) %in% starts == FALSE

  display_data[[group_col]][repeated] <- ""

  list(
    data = display_data,
    starts = starts
  )
}


.risk_xlsx_style_group_starts <- function(
  wb,
  sheet,
  rows,
  start_col,
  end_col,
  group_excel_col
) {

  if (length(rows) == 0L) {
    return(wb)
  }

  pal <- .risk_xlsx_palette()

  for (row_i in rows) {

    row_dims <- paste0(
      openxlsx2::int2col(start_col),
      row_i,
      ":",
      openxlsx2::int2col(end_col),
      row_i
    )

    wb <- openxlsx2::wb_add_fill(
      wb,
      sheet = sheet,
      dims = row_dims,
      color = pal$group,
      pattern = "solid"
    )

    group_dims <- paste0(
      openxlsx2::int2col(group_excel_col),
      row_i
    )

    wb <- openxlsx2::wb_add_font(
      wb,
      sheet = sheet,
      dims = group_dims,
      name = "Aptos",
      size = 10,
      bold = TRUE,
      color = pal$mid_dark
    )
  }

  wb
}


# -----------------------------------------------------------------------------
# F. Add analytical/support table sheet
# -----------------------------------------------------------------------------

.risk_xlsx_add_table_sheet <- function(
  wb,
  sheet,
  title,
  subtitle,
  data,
  group_col = NULL,
  total_col = "Total",
  text_heavy = FALSE,
  qa_sheet = FALSE
) {

  pal <- .risk_xlsx_palette()

  grouped <- .risk_xlsx_prepare_grouped(
    data = data,
    group_col = group_col
  )

  display_data <- grouped$data


  wb <- openxlsx2::wb_add_worksheet(
    wb,
    sheet = sheet,
    grid_lines = FALSE,
    zoom = 90
  )


  # ---------------------------------------------------------------------------
  # Title area
  # ---------------------------------------------------------------------------

  last_excel_col <- max(
    8L,
    ncol(display_data) + 1L
  )

  title_dims <- paste0(
    "B2:",
    openxlsx2::int2col(last_excel_col),
    "3"
  )

  wb <- openxlsx2::wb_add_data(
    wb,
    sheet = sheet,
    x = title,
    start_col = 2,
    start_row = 2,
    col_names = FALSE
  )

  wb <- openxlsx2::wb_merge_cells(
    wb,
    sheet = sheet,
    dims = title_dims
  )

  wb <- .risk_xlsx_style_title(
    wb,
    sheet,
    title_dims
  )


  subtitle_dims <- paste0(
    "B4:",
    openxlsx2::int2col(last_excel_col),
    "4"
  )

  wb <- openxlsx2::wb_add_data(
    wb,
    sheet = sheet,
    x = subtitle,
    start_col = 2,
    start_row = 4,
    col_names = FALSE
  )

  wb <- openxlsx2::wb_merge_cells(
    wb,
    sheet = sheet,
    dims = subtitle_dims
  )

  wb <- .risk_xlsx_style_subtitle(
    wb,
    sheet,
    subtitle_dims
  )


  # ---------------------------------------------------------------------------
  # Data
  # ---------------------------------------------------------------------------

  table_row <- 6L
  first_col <- 2L
  last_col <- first_col + ncol(display_data) - 1L

  wb <- openxlsx2::wb_add_data(
    wb,
    sheet = sheet,
    x = display_data,
    start_col = first_col,
    start_row = table_row,
    col_names = TRUE
  )

  header_dims <- paste0(
    openxlsx2::int2col(first_col),
    table_row,
    ":",
    openxlsx2::int2col(last_col),
    table_row
  )

  wb <- .risk_xlsx_style_header(
    wb,
    sheet,
    header_dims
  )


  # ---------------------------------------------------------------------------
  # Body
  # ---------------------------------------------------------------------------

  if (nrow(display_data) > 0L) {

    first_body_row <- table_row + 1L
    last_body_row <- table_row + nrow(display_data)

    body_dims <- paste0(
      openxlsx2::int2col(first_col),
      first_body_row,
      ":",
      openxlsx2::int2col(last_col),
      last_body_row
    )

    wb <- .risk_xlsx_style_body(
      wb,
      sheet,
      body_dims,
      wrap = text_heavy
    )


# -------------------------------------------------------------------------
# Alternating row treatment
# -------------------------------------------------------------------------

if (last_body_row >= first_body_row + 1L) {

  alt_rows <- seq(
    first_body_row + 1L,
    last_body_row,
    by = 2L
  )

  for (row_i in alt_rows) {

    dims <- paste0(
      openxlsx2::int2col(first_col),
      row_i,
      ":",
      openxlsx2::int2col(last_col),
      row_i
    )

    wb <- openxlsx2::wb_add_fill(
      wb,
      sheet = sheet,
      dims = dims,
      color = pal$alt_row,
      pattern = "solid"
    )
  }
}


    # -------------------------------------------------------------------------
    # Multiline cells
    # -------------------------------------------------------------------------

    newline_rows <- which(
      apply(
        display_data,
        1,
        function(x) {
          any(
            grepl(
              "\n",
              as.character(x),
              fixed = TRUE
            )
          )
        }
      )
    )

    if (length(newline_rows) > 0L) {

      excel_rows <- table_row + newline_rows

      for (row_i in excel_rows) {

        dims <- paste0(
          openxlsx2::int2col(first_col),
          row_i,
          ":",
          openxlsx2::int2col(last_col),
          row_i
        )

        wb <- openxlsx2::wb_add_cell_style(
          wb,
          sheet = sheet,
          dims = dims,
          vertical = "top",
          wrap_text = TRUE
        )

        wb <- openxlsx2::wb_set_row_heights(
          wb,
          sheet = sheet,
          rows = row_i,
          heights = 38
        )
      }
    }


    # -------------------------------------------------------------------------
    # Group starts
    # -------------------------------------------------------------------------

    if (
      !is.null(group_col) &&
      length(grouped$starts) > 0L
    ) {

      group_excel_col <- first_col +
        match(group_col, names(display_data)) - 1L

      group_excel_rows <- table_row + grouped$starts

      wb <- .risk_xlsx_style_group_starts(
        wb = wb,
        sheet = sheet,
        rows = group_excel_rows,
        start_col = first_col,
        end_col = last_col,
        group_excel_col = group_excel_col
      )
    }


    # -------------------------------------------------------------------------
    # Total column
    # -------------------------------------------------------------------------

    if (
      !is.null(total_col) &&
      total_col %in% names(display_data)
    ) {

      total_excel_col <- first_col +
        match(total_col, names(display_data)) - 1L

      total_dims <- paste0(
        openxlsx2::int2col(total_excel_col),
        table_row,
        ":",
        openxlsx2::int2col(total_excel_col),
        last_body_row
      )

      wb <- openxlsx2::wb_add_fill(
        wb,
        sheet = sheet,
        dims = total_dims,
        color = pal$total,
        pattern = "solid"
      )

      wb <- openxlsx2::wb_add_font(
        wb,
        sheet = sheet,
        dims = total_dims,
        name = "Aptos",
        size = 10,
        bold = TRUE,
        color = pal$dark
      )
    }


    # -------------------------------------------------------------------------
    # QA PASS column
    # -------------------------------------------------------------------------

    if (
      isTRUE(qa_sheet) &&
      "status" %in% names(display_data)
    ) {

      status_col <- first_col +
        match("status", names(display_data)) - 1L

      pass_rows <- which(
        toupper(as.character(display_data$status)) == "PASS"
      )

      for (i in pass_rows) {

        row_i <- table_row + i

        dims <- paste0(
          openxlsx2::int2col(status_col),
          row_i
        )

        wb <- openxlsx2::wb_add_fill(
          wb,
          sheet = sheet,
          dims = dims,
          color = pal$pass,
          pattern = "solid"
        )

        wb <- openxlsx2::wb_add_font(
          wb,
          sheet = sheet,
          dims = dims,
          name = "Aptos",
          size = 10,
          bold = TRUE,
          color = pal$dark
        )
      }
    }
  }


  # ---------------------------------------------------------------------------
  # Column widths
  # ---------------------------------------------------------------------------

  wb <- openxlsx2::wb_set_col_widths(
    wb,
    sheet = sheet,
    cols = 1,
    widths = 3
  )

  for (j in seq_len(ncol(display_data))) {

    excel_col <- first_col + j - 1L

    values <- as.character(
      display_data[[j]]
    )

    max_chars <- max(
      nchar(
        c(
          names(display_data)[j],
          values
        )
      ),
      na.rm = TRUE
    )

    width <- min(
      max(
        12,
        max_chars + 2
      ),
      if (text_heavy) 45 else 26
    )

    wb <- openxlsx2::wb_set_col_widths(
      wb,
      sheet = sheet,
      cols = excel_col,
      widths = width
    )
  }


  # ---------------------------------------------------------------------------
  # Row heights / freeze panes
  # ---------------------------------------------------------------------------

  wb <- openxlsx2::wb_set_row_heights(
    wb,
    sheet = sheet,
    rows = 2:3,
    heights = 24
  )

  wb <- openxlsx2::wb_set_row_heights(
    wb,
    sheet = sheet,
    rows = 4,
    heights = 24
  )

  wb <- openxlsx2::wb_set_row_heights(
    wb,
    sheet = sheet,
    rows = table_row,
    heights = 26
  )

  wb <- openxlsx2::wb_freeze_pane(
    wb,
    sheet = sheet,
    first_active_row = table_row + 1L,
    first_active_col = 2
  )

  wb
}


# -----------------------------------------------------------------------------
# G. Context conversion
# -----------------------------------------------------------------------------

.risk_xlsx_context_sections <- function(
  context,
  generated_date,
  package_version
) {

  if (!is.list(context)) {
    stop(
      "context must be NULL or a compatible named list.",
      call. = FALSE
    )
  }

  overview <- context$overview
  scope <- context$scope
  filters <- context$filters
  interpretation <- context$interpretation
  provenance <- context$provenance


  make_rows <- function(items, descriptions) {

    keep <- !vapply(
      descriptions,
      function(x) {
        is.null(x) ||
          length(x) == 0L ||
          all(is.na(x)) ||
          !nzchar(paste(x, collapse = ""))
      },
      logical(1)
    )

    data.frame(
      item = items[keep],
      description = vapply(
        descriptions[keep],
        function(x) {
          paste(x, collapse = "; ")
        },
        character(1)
      ),
      stringsAsFactors = FALSE
    )
  }


  out <- list()


  if (!is.null(overview)) {

    out$overview <- make_rows(
      c(
        "Purpose",
        "Objective",
        "Dataset represents"
      ),
      list(
        overview$purpose,
        overview$objective,
        overview$dataset_represents
      )
    )
  }


  if (!is.null(scope)) {

    analysis_period <- NULL

    if (
      !is.null(scope$analysis_start) &&
      !is.null(scope$analysis_end)
    ) {

      analysis_period <- paste(
        format(scope$analysis_start, "%d %B %Y"),
        "to",
        format(scope$analysis_end, "%d %B %Y")
      )
    }

    out$scope <- make_rows(
      c(
        "Analysis period",
        "Geographic scope",
        "Unit of analysis"
      ),
      list(
        analysis_period,
        scope$geographic_scope,
        scope$unit_of_analysis
      )
    )

    out$inclusions <- scope$inclusions
    out$exclusions <- scope$exclusions
  }


  out$filters <- filters

  if (!is.null(interpretation)) {
    out$interpretation <- interpretation$notes
  }


  provenance_rows <- make_rows(
    c(
      "Source",
      "Owner",
      "Generated",
      "Package",
      "Version"
    ),
    list(
      provenance$source,
      provenance$owner,
      generated_date,
      "riskworkflowr",
      package_version
    )
  )

  out$provenance <- provenance_rows

  out
}


# -----------------------------------------------------------------------------
# H. Summary section writer
# -----------------------------------------------------------------------------

.risk_xlsx_add_summary_section <- function(
  wb,
  sheet,
  start_row,
  heading,
  data,
  bullet = FALSE
) {

  if (is.null(data) || length(data) == 0L) {
    return(
      list(
        wb = wb,
        next_row = start_row
      )
    )
  }


  if (!inherits(data, "data.frame")) {

    data <- data.frame(
      item = "\u2022",
      description = as.character(data),
      stringsAsFactors = FALSE
    )

    bullet <- TRUE
  }


  if (nrow(data) == 0L) {
    return(
      list(
        wb = wb,
        next_row = start_row
      )
    )
  }


  pal <- .risk_xlsx_palette()

  heading_dims <- paste0(
    "B",
    start_row,
    ":H",
    start_row
  )

  wb <- openxlsx2::wb_add_data(
    wb,
    sheet = sheet,
    x = heading,
    start_col = 2,
    start_row = start_row,
    col_names = FALSE
  )

  wb <- openxlsx2::wb_merge_cells(
    wb,
    sheet = sheet,
    dims = heading_dims
  )

  wb <- openxlsx2::wb_add_fill(
    wb,
    sheet = sheet,
    dims = heading_dims,
    color = pal$section,
    pattern = "solid"
  )

  wb <- openxlsx2::wb_add_font(
    wb,
    sheet = sheet,
    dims = heading_dims,
    name = "Aptos",
    size = 11,
    bold = TRUE,
    color = pal$white
  )


  body_start <- start_row + 1L

  for (i in seq_len(nrow(data))) {

    row_i <- body_start + i - 1L

    label <- data$item[[i]]
    value <- data$description[[i]]

    wb <- openxlsx2::wb_add_data(
      wb,
      sheet = sheet,
      x = label,
      start_col = 2,
      start_row = row_i,
      col_names = FALSE
    )

    wb <- openxlsx2::wb_add_data(
      wb,
      sheet = sheet,
      x = value,
      start_col = 3,
      start_row = row_i,
      col_names = FALSE
    )

    wb <- openxlsx2::wb_merge_cells(
      wb,
      sheet = sheet,
      dims = paste0(
        "C",
        row_i,
        ":H",
        row_i
      )
    )

    if (!bullet) {

      wb <- openxlsx2::wb_add_fill(
        wb,
        sheet = sheet,
        dims = paste0("B", row_i),
        color = pal$label,
        pattern = "solid"
      )

      wb <- openxlsx2::wb_add_font(
        wb,
        sheet = sheet,
        dims = paste0("B", row_i),
        name = "Aptos",
        size = 10,
        bold = TRUE,
        color = pal$mid_dark
      )
    }

    wb <- openxlsx2::wb_add_cell_style(
      wb,
      sheet = sheet,
      dims = paste0(
        "B",
        row_i,
        ":H",
        row_i
      ),
      vertical = "top",
      wrap_text = TRUE
    )
  }


  list(
    wb = wb,
    next_row = body_start + nrow(data) + 1L
  )
}


# -----------------------------------------------------------------------------
# I. Summary sheet
# -----------------------------------------------------------------------------

.risk_xlsx_add_summary <- function(
  wb,
  title,
  description,
  sections
) {

  sheet <- "00_Summary"

  wb <- openxlsx2::wb_add_worksheet(
    wb,
    sheet = sheet,
    grid_lines = FALSE,
    zoom = 90
  )


  wb <- openxlsx2::wb_add_data(
    wb,
    sheet = sheet,
    x = title,
    start_col = 2,
    start_row = 2,
    col_names = FALSE
  )

  wb <- openxlsx2::wb_merge_cells(
    wb,
    sheet = sheet,
    dims = "B2:H3"
  )

  wb <- .risk_xlsx_style_title(
    wb,
    sheet,
    "B2:H3"
  )


  wb <- openxlsx2::wb_add_data(
    wb,
    sheet = sheet,
    x = description,
    start_col = 2,
    start_row = 4,
    col_names = FALSE
  )

  wb <- openxlsx2::wb_merge_cells(
    wb,
    sheet = sheet,
    dims = "B4:H4"
  )

  wb <- .risk_xlsx_style_subtitle(
    wb,
    sheet,
    "B4:H4"
  )


  next_row <- 6L

  section_map <- list(
    Overview = sections$overview,
    Scope = sections$scope,
    Included = sections$inclusions,
    Excluded = sections$exclusions,
    `Filters Applied` = sections$filters,
    Interpretation = sections$interpretation,
    Provenance = sections$provenance
  )


  for (heading in names(section_map)) {

    x <- section_map[[heading]]

    if (
      identical(heading, "Filters Applied") &&
      inherits(x, "data.frame") &&
      all(c("field", "rule") %in% names(x))
    ) {

      x <- data.frame(
        item = x$field,
        description = x$rule,
        stringsAsFactors = FALSE
      )
    }

    result <- .risk_xlsx_add_summary_section(
      wb = wb,
      sheet = sheet,
      start_row = next_row,
      heading = heading,
      data = x,
      bullet = heading %in% c(
        "Included",
        "Excluded",
        "Interpretation"
      )
    )

    wb <- result$wb
    next_row <- result$next_row
  }


  wb <- openxlsx2::wb_set_col_widths(
    wb,
    sheet = sheet,
    cols = 1,
    widths = 3
  )

  wb <- openxlsx2::wb_set_col_widths(
    wb,
    sheet = sheet,
    cols = 2,
    widths = 22
  )

  wb <- openxlsx2::wb_set_col_widths(
    wb,
    sheet = sheet,
    cols = 3:8,
    widths = 14
  )

  wb <- openxlsx2::wb_freeze_pane(
    wb,
    sheet = sheet,
    first_active_row = 6
  )

  wb
}