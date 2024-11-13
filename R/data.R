#' Table of US location names and codes
#'
#' A table of US location names and codes, assembled from a
#' US census reference table with the addition of "US" for the
#' United States as a whole.
#'
#' @format ## `us_location_table`
#' A tibble with 58 rows and 3 columns:
#' \describe{
#'   \item{location_code}{2-character location code}
#'   \item{short_name}{2-character location abbreviation}
#'   \item{yearlong_name}{Full name of the location}
#' }
#' @source <https://www2.census.gov/geo/docs/reference/state.txt>
"us_location_table"
