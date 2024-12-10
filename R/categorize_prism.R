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
#' @details Note that if both `locations` and `diseases` are vectors,
#' the result will be a 3-dimensional array whose first dimension
#' is location, second is disease, and third is the cutpoints.
#'
#' @examples
#' get_prism_cutpoints("WA", "Influenza")
#'
#' get_prism_cutpoints(c("US", "WA"), "COVID-19")
#'
#' get_prism_cutpoints(c("US", "WA"), c("ARI", "RSV"))
#' @export
get_prism_cutpoints <- function(locations, diseases) {
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
