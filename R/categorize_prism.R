#' Get PRISM activity level cutpoints for given
#' diseases and locations
#'
#' @param diseases disease(s) for which to return the cutpoints.
#' One of `"ARI"`, `"COVID-19"`, `"Influenza"`, or `"RSV"`, or
#' an array of those values.
#' @param locations location(s) for which to return the cutpoints.
#' A location two-letter abbreviation as in the `short_name`
#' column of [forecasttools::us_location_table], or an array
#' of such abbreviations.
#' @return The cutpoints, as a single vector or array of vectors.
#'
#' @examples
#' get_prism_cutpoints("Influenza", "WA")
#'
#' get_prism_cutpoints("COVID-19", c("US", "WA"))
#'
#' get_prism_cutpoints(c("ARI", "RSV"), c("US", "WA"))
#' @export
get_prism_cutpoints <- function(diseases, locations) {
  checkmate::assert_names(
    diseases,
    subset.of =
      dimnames(forecasttools::prism_thresholds)$disease,
    what = "disease"
  )
  checkmate::assert_names(
    locations,
    subset.of =
      dimnames(forecasttools::prism_thresholds)$location,
    what = "location"
  )
  return(forecasttools::prism_thresholds[locations, diseases, ])
}
