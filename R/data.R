#' Example LGA Analysis Dataset
#'
#' Synthetic example Local Government Area (LGA) analysis dataset
#' containing example event counts and population values.
#'
#' This dataset is included for demonstration, testing, and vignette
#' purposes only. Values are synthetic and generated using fixed random
#' seeds for reproducibility.
#'
#' @format A data frame with 10 rows and 4 variables:
#' \describe{
#'   \item{lga_code}{Synthetic LGA code}
#'   \item{lga_name}{Synthetic LGA name}
#'   \item{event_count}{Synthetic event count}
#'   \item{pop}{Synthetic population value}
#' }
#'
#' @source Synthetic data generated using
#' `data-raw/generate_example_data.R`
#'
"example_lga_analysis"


#' Example Hexbin Analysis Dataset
#'
#' Synthetic example hexbin spatial dataset containing example event
#' counts and population values.
#'
#' This dataset is included for demonstration, testing, and vignette
#' purposes only. Values are synthetic and generated using fixed random
#' seeds for reproducibility.
#'
#' @format An sf object with hexagonal geometries and attributes:
#' \describe{
#'   \item{hex_id}{Synthetic hexagon identifier}
#'   \item{event_count}{Synthetic event count}
#'   \item{pop}{Synthetic population value}
#'   \item{geometry}{Hexagonal polygon geometry}
#' }
#'
#' @source Synthetic data generated using
#' `data-raw/generate_example_data.R`
#'
"example_hex_analysis"