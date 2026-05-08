# 1 ------------------------------------------------------------------------
# utils_qa_print.R
# Purpose: Print QA summaries in readable console format

#' Print QA summary for areal or hexbin counts
#'
#' Prints a simple console summary of event counts and unit coverage.
#'
#' @param units A data frame or sf object containing counted units.
#' @param count_col Name of count column.
#'
#' @return Invisibly returns NULL.

qa_print_areal_counts <- function(units, count_col = "event_count") {

  df <- sf::st_drop_geometry(units)

  total_units <- nrow(df)
  units_with_events <- sum(df[[count_col]] > 0, na.rm = TRUE)
  units_without_events <- sum(df[[count_col]] == 0, na.rm = TRUE)
  total_events <- sum(df[[count_col]], na.rm = TRUE)
  missing_event_count <- sum(is.na(df[[count_col]]))

  cat("\n--- QA: Areal Counts Summary ---\n")
  cat("Total Units:", total_units, "\n")
  cat("Units with Events:", units_with_events, "\n")
  cat("Units without Events:", units_without_events, "\n")
  cat("Total Events:", total_events, "\n")
  cat("Missing Event Count:", missing_event_count, "\n")
}


qa_print_join_methods <- function(joined_points) {

  df <- sf::st_drop_geometry(joined_points)

  counts <- df |>
    dplyr::count(join_method)

  cat("\n--- QA: Join Method ---\n")

  for (i in seq_len(nrow(counts))) {
    cat(
      paste0(
        tools::toTitleCase(counts$join_method[i]),
        ": ",
        counts$n[i],
        "\n"
      )
    )
  }
}
