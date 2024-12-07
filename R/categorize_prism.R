#' Get PRISM activity level cutpoints for a given
#' disease and location
#'
#' @param disease disease(s) for which to return the cutpoints.
#' One of `"ARI"`, `"COVID-19"`, `"Influenza"`, or `"RSV"`, or
#' an array of those values.
#' @param location location(s) for which to return the cutpoints.
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
get_prism_cutpoints <- function(disease, location) {
  checkmate::assert_names(
    disease,
    subset.of =
      dimnames(forecasttools::prism_thresholds)$disease,
    what = "disease"
  )
  checkmate::assert_names(
    location,
    subset.of =
      dimnames(forecasttools::prism_thresholds)$location,
    what = "location"
  )
  return(forecasttools::prism_thresholds[location, disease, ])
}
