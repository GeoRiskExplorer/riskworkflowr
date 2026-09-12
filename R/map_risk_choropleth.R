# ============================================================================
# map_risk_choropleth.R
# Purpose: Classed or continuous choropleth map for areal units / hexbins
# ============================================================================


#' Create a choropleth risk map
#'
#' Creates a choropleth map for areal units or hexbins using classed or
#' continuous colour schemes suitable for spatial risk analysis.
#'
#' @param data An `sf` object.
#' @param fill_col Name of the variable to map.
#' @param map_type Type of mapped variable: `"count"`, `"rate"`,
#'   `"probability"`, `"smr"`, or `"category"`.
#' @param classification Classification method. Defaults are selected from
#'   `map_type`. Supported values are `"quantile"`, `"equal_interval"`,
#'   `"jenks"`, `"pretty"`, `"smr_default"`, and `"none"`.
#' @param n_classes Number of classes for classified numeric maps.
#' @param palette RColorBrewer palette name.
#' @param reverse_palette Logical; if `TRUE`, reverse palette direction.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param fill_label Legend label. Defaults to `fill_col`.
#' @param border_colour Polygon border colour.
#' @param border_width Polygon border width.
#' @param na_colour Colour used for missing values.
#'
#' @return A `ggplot` object.
#'
#' @details
#' For numeric map types, `classification = "none"` produces a continuous
#' colour scale. Classified numeric maps produce discrete classes.
#'
#' `map_type = "category"` requires categorical data and defaults to
#' `classification = "none"` with a qualitative Brewer palette.
#'
#' The default SMR classification uses:
#'
#' - `0`
#' - `>0-0.75`
#' - `0.75-1.25`
#' - `1.25-2`
#' - `>2`
#'
#' @references
#' Brewer, C. A. Designing Better Maps: A Guide for GIS Users.
#'
#' @export
map_risk_choropleth <- function(
  data,
  fill_col,
  map_type = c(
    "count",
    "rate",
    "probability",
    "smr",
    "category"
  ),
  classification = NULL,
  n_classes = 5,
  palette = NULL,
  reverse_palette = FALSE,
  title = NULL,
  subtitle = NULL,
  fill_label = NULL,
  border_colour = "grey70",
  border_width = 0.2,
  na_colour = "grey90"
) {

  # ==========================================================================
  # 01. VALIDATE CORE INPUTS
  # ==========================================================================

  map_type <- match.arg(map_type)

  if (!inherits(data, "sf")) {
    stop(
      "`data` must be an sf object.",
      call. = FALSE
    )
  }

  if (
    length(fill_col) != 1L ||
    !is.character(fill_col) ||
    is.na(fill_col) ||
    !nzchar(fill_col)
  ) {
    stop(
      "`fill_col` must be one non-empty character value.",
      call. = FALSE
    )
  }

  if (!fill_col %in% names(data)) {
    stop(
      "`fill_col` not found in data.",
      call. = FALSE
    )
  }

  if (
    length(n_classes) != 1L ||
    !is.numeric(n_classes) ||
    is.na(n_classes) ||
    !is.finite(n_classes) ||
    n_classes < 2 ||
    n_classes != floor(n_classes)
  ) {
    stop(
      "`n_classes` must be one whole number >= 2.",
      call. = FALSE
    )
  }

  if (
    length(reverse_palette) != 1L ||
    !is.logical(reverse_palette) ||
    is.na(reverse_palette)
  ) {
    stop(
      "`reverse_palette` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (
    length(border_width) != 1L ||
    !is.numeric(border_width) ||
    is.na(border_width) ||
    !is.finite(border_width) ||
    border_width < 0
  ) {
    stop(
      "`border_width` must be one finite non-negative number.",
      call. = FALSE
    )
  }


  # ==========================================================================
  # 02. CHECK OPTIONAL MAPPING DEPENDENCIES
  # ==========================================================================

  if (!requireNamespace(
    "RColorBrewer",
    quietly = TRUE
  )) {
    stop(
      "Package `RColorBrewer` is required for `map_risk_choropleth()`.",
      call. = FALSE
    )
  }

  if (!requireNamespace(
    "classInt",
    quietly = TRUE
  )) {
    stop(
      "Package `classInt` is required for `map_risk_choropleth()`.",
      call. = FALSE
    )
  }


  # ==========================================================================
  # 03. RESOLVE DEFAULT CLASSIFICATION
  # ==========================================================================

  if (is.null(classification)) {

    classification <-
      switch(
        map_type,
        count = "quantile",
        rate = "quantile",
        probability = "quantile",
        smr = "smr_default",
        category = "none"
      )
  }

  classification <- match.arg(
    classification,
    choices = c(
      "quantile",
      "equal_interval",
      "jenks",
      "pretty",
      "smr_default",
      "none"
    )
  )

  if (
    map_type == "category" &&
    classification != "none"
  ) {
    stop(
      "`map_type = \"category\"` requires `classification = \"none\"`.",
      call. = FALSE
    )
  }

  if (
    map_type != "smr" &&
    classification == "smr_default"
  ) {
    stop(
      "`classification = \"smr_default\"` is only valid for `map_type = \"smr\"`.",
      call. = FALSE
    )
  }


  # ==========================================================================
  # 04. RESOLVE DEFAULT PALETTE
  # ==========================================================================

  if (is.null(palette)) {

    palette <-
      switch(
        map_type,
        count = "Blues",
        rate = "YlOrRd",
        probability = "PuRd",
        smr = "RdYlBu",
        category = "Set2"
      )
  }

  if (
    length(palette) != 1L ||
    !is.character(palette) ||
    is.na(palette) ||
    !nzchar(palette)
  ) {
    stop(
      "`palette` must be one non-empty character value.",
      call. = FALSE
    )
  }

  palette_info <-
    RColorBrewer::brewer.pal.info

  if (!palette %in% rownames(palette_info)) {
    stop(
      "Unknown RColorBrewer palette: ",
      palette,
      call. = FALSE
    )
  }


  # ==========================================================================
  # 05. VALIDATE FILL VALUES
  # ==========================================================================

  plot_data <- data
  values <- plot_data[[fill_col]]

  if (map_type == "category") {

    if (
      !is.character(values) &&
      !is.factor(values)
    ) {
      stop(
        "`map_type = \"category\"` requires a character or factor `fill_col`.",
        call. = FALSE
      )
    }

    plot_data[[fill_col]] <-
      as.factor(values)

  } else {

    if (!is.numeric(values)) {
      stop(
        "Numeric map types require a numeric `fill_col`.",
        call. = FALSE
      )
    }

    if (any(
      !is.na(values) &
      !is.finite(values)
    )) {
      stop(
        "`fill_col` cannot contain infinite values.",
        call. = FALSE
      )
    }

    if (
      map_type %in%
        c(
          "count",
          "rate",
          "probability",
          "smr"
        ) &&
      any(
        values < 0,
        na.rm = TRUE
      )
    ) {
      stop(
        paste0(
          "`map_type = \"",
          map_type,
          "\"` requires non-negative values."
        ),
        call. = FALSE
      )
    }

    if (
      map_type == "probability" &&
      any(
        values > 1,
        na.rm = TRUE
      )
    ) {
      stop(
        "`map_type = \"probability\"` requires values between 0 and 1.",
        call. = FALSE
      )
    }
  }


  # ==========================================================================
  # 06. CATEGORY MAP
  # ==========================================================================

  if (map_type == "category") {

    n_levels <-
      nlevels(
        droplevels(
          plot_data[[fill_col]]
        )
      )

    max_colours <-
      palette_info[
        palette,
        "maxcolors"
      ]

    if (n_levels > max_colours) {
      stop(
        "Palette `",
        palette,
        "` supports at most ",
        max_colours,
        " categories, but ",
        n_levels,
        " are present.",
        call. = FALSE
      )
    }

    return(
      ggplot2::ggplot() +
        ggplot2::geom_sf(
          data = plot_data,
          ggplot2::aes(
            fill = .data[[fill_col]]
          ),
          colour = border_colour,
          linewidth = border_width
        ) +
        ggplot2::scale_fill_brewer(
          palette = palette,
          direction =
            if (reverse_palette) {
              -1
            } else {
              1
            },
          na.value = na_colour,
          drop = FALSE
        ) +
        ggplot2::labs(
          title = title,
          subtitle = subtitle,
          fill =
            if (is.null(fill_label)) {
              fill_col
            } else {
              fill_label
            }
        ) +
        ggplot2::theme_minimal()
    )
  }


  # ==========================================================================
  # 07. CONTINUOUS NUMERIC MAP
  # ==========================================================================

  if (classification == "none") {

    return(
      ggplot2::ggplot() +
        ggplot2::geom_sf(
          data = plot_data,
          ggplot2::aes(
            fill = .data[[fill_col]]
          ),
          colour = border_colour,
          linewidth = border_width
        ) +
        ggplot2::scale_fill_distiller(
          palette = palette,
          direction =
            if (reverse_palette) {
              -1
            } else {
              1
            },
          na.value = na_colour
        ) +
        ggplot2::labs(
          title = title,
          subtitle = subtitle,
          fill =
            if (is.null(fill_label)) {
              fill_col
            } else {
              fill_label
            }
        ) +
        ggplot2::theme_minimal()
    )
  }


  # ==========================================================================
  # 08. BUILD DISCRETE NUMERIC CLASSES
  # ==========================================================================

  class_col <-
    paste0(
      fill_col,
      "_class"
    )

  if (class_col %in% names(plot_data)) {
    stop(
      "Derived class column already exists: ",
      class_col,
      call. = FALSE
    )
  }

  if (classification == "smr_default") {

    plot_data[[class_col]] <-
      cut(
        values,
        breaks = c(
          -Inf,
          0,
          0.75,
          1.25,
          2,
          Inf
        ),
        labels = c(
          "0",
          ">0-0.75",
          "0.75-1.25",
          "1.25-2",
          ">2"
        ),
        include.lowest = TRUE,
        right = TRUE
      )

    if (!reverse_palette) {
      reverse_palette <- TRUE
    }

  } else {

    valid_values <-
      values[
        !is.na(values)
      ]

    unique_values <-
      sort(
        unique(
          valid_values
        )
      )

    if (length(unique_values) == 0L) {

      plot_data[[class_col]] <-
        factor(
          rep(
            NA_character_,
            nrow(plot_data)
          )
        )

    } else if (length(unique_values) == 1L) {

      plot_data[[class_col]] <-
        factor(
          ifelse(
            is.na(values),
            NA_character_,
            as.character(values)
          )
        )

    } else {

    classes_used <-
  min(
    as.integer(n_classes),
    length(unique_values) - 1L
  )

classes_used <-
  max(
    2L,
    classes_used
  )

      class_style <-
        switch(
          classification,
          quantile = "quantile",
          equal_interval = "equal",
          jenks = "jenks",
          pretty = "pretty"
        )

      class_breaks <-
        classInt::classIntervals(
          valid_values,
          n = classes_used,
          style = class_style
        )$brks

      class_breaks <-
        unique(
          class_breaks
        )

      if (length(class_breaks) < 2L) {

        plot_data[[class_col]] <-
          factor(
            ifelse(
              is.na(values),
              NA_character_,
              as.character(values)
            )
          )

      } else {

        plot_data[[class_col]] <-
          cut(
            values,
            breaks = class_breaks,
            include.lowest = TRUE,
            dig.lab = 6
          )
      }
    }
  }


  # ==========================================================================
  # 09. VALIDATE DISCRETE PALETTE CAPACITY
  # ==========================================================================

  n_levels <-
    nlevels(
      droplevels(
        plot_data[[class_col]]
      )
    )

  max_colours <-
    palette_info[
      palette,
      "maxcolors"
    ]

  if (n_levels > max_colours) {
    stop(
      "Palette `",
      palette,
      "` supports at most ",
      max_colours,
      " classes, but ",
      n_levels,
      " were produced.",
      call. = FALSE
    )
  }


  # ==========================================================================
  # 10. BUILD CLASSIFIED MAP
  # ==========================================================================

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = plot_data,
      ggplot2::aes(
        fill = .data[[class_col]]
      ),
      colour = border_colour,
      linewidth = border_width
    ) +
    ggplot2::scale_fill_brewer(
      palette = palette,
      direction =
        if (reverse_palette) {
          -1
        } else {
          1
        },
      na.value = na_colour,
      drop = FALSE
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      fill =
        if (is.null(fill_label)) {
          fill_col
        } else {
          fill_label
        }
    ) +
    ggplot2::theme_minimal()
}
