# 1 ------------------------------------------------------------------------
# map_risk_choropleth.R
# Purpose: Classed choropleth map for areal units / hexbins

map_risk_choropleth <- function(
  data,
  fill_col,
  map_type = c("count", "rate", "probability", "smr", "category"),
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

  map_type <- match.arg(map_type)

  if (!inherits(data, "sf")) {
    stop("`data` must be an sf object.", call. = FALSE)
  }

  if (!fill_col %in% names(data)) {
    stop("`fill_col` not found in data.", call. = FALSE)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package `ggplot2` is required.", call. = FALSE)
  }

  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop("Package `RColorBrewer` is required.", call. = FALSE)
  }

  if (!requireNamespace("classInt", quietly = TRUE)) {
    stop("Package `classInt` is required.", call. = FALSE)
  }

  if (is.null(classification)) {
    classification <- dplyr::case_when(
      map_type %in% c("count", "rate", "probability") ~ "quantile",
      map_type == "smr" ~ "smr_default",
      map_type == "category" ~ "none",
      TRUE ~ "quantile"
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

  if (is.null(palette)) {
    palette <- dplyr::case_when(
      map_type == "count" ~ "Blues",
      map_type == "rate" ~ "YlOrRd",
      map_type == "probability" ~ "PuRd",
      map_type == "smr" ~ "RdYlBu",
      map_type == "category" ~ "Set2",
      TRUE ~ "Blues"
    )
  }

  plot_data <- data

  class_col <- paste0(fill_col, "_class")

  values <- plot_data[[fill_col]]

  if (classification == "none") {

    plot_data[[class_col]] <- values

  } else if (classification == "smr_default") {

    plot_data[[class_col]] <- cut(
      values,
      breaks = c(-Inf, 0, 0.75, 1.25, 2, Inf),
      labels = c(
        "0",
        ">0–0.75",
        "0.75–1.25",
        "1.25–2",
        ">2"
      ),
      include.lowest = TRUE,
      right = TRUE
    )

  } else {

    valid_values <- values[
      !is.na(values) &
        is.finite(values)
    ]

    if (length(unique(valid_values)) < 2) {
      plot_data[[class_col]] <- as.factor(values)
    } else {

      class_style <- dplyr::case_when(
        classification == "quantile" ~ "quantile",
        classification == "equal_interval" ~ "equal",
        classification == "jenks" ~ "jenks",
        classification == "pretty" ~ "pretty",
        TRUE ~ "quantile"
      )

      class_breaks <- classInt::classIntervals(
        valid_values,
        n = n_classes,
        style = class_style
      )$brks

      class_breaks <- unique(class_breaks)

      plot_data[[class_col]] <- cut(
        values,
        breaks = class_breaks,
        include.lowest = TRUE,
        dig.lab = 6
      )
    }
  }

  if (classification == "smr_default" && reverse_palette == FALSE) {
    reverse_palette <- TRUE
  }

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = plot_data,
      ggplot2::aes(fill = .data[[class_col]]),
      colour = border_colour,
      linewidth = border_width
    ) +
    ggplot2::scale_fill_brewer(
      palette = palette,
      direction = ifelse(reverse_palette, -1, 1),
      na.value = na_colour,
      drop = FALSE
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      fill = fill_label %||% fill_col
    ) +
    ggplot2::theme_minimal()
}