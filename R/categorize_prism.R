#' Get PRISM activity level cutpoints for given
#' diseases and locations
#'
#' @param diseases disease(s) for which to return the cutpoints.
#' One of `"ARI"`, `"COVID-19"`, `"Influenza"`, or `"RSV"`, or
#' an array of those values.
#' @param locations location(s) for which to return the cutpoints.
#' A location two-letter abbreviation as in the `short_name`
#' column of [forecasttools::us_location_table], or an array
#' of those abbreviations.
#' @return The cutpoints, as an ordered list of vectors.
#'
#' @examples
#' get_prism_cutpoints("WA", "Influenza")
#'
#' get_prism_cutpoints(c("US", "WA"), "COVID-19")
#'
#' get_prism_cutpoints(c("US", "WA"), c("ARI", "RSV"))
#' @export
get_prism_cutpoints <- function(locations, diseases) {
  locations <- stringr::str_to_lower(locations)
  diseases <- stringr::str_to_lower(diseases)
  checkmate::assert_names(
    diseases,
    subset.of = dimnames(forecasttools::prism_thresholds)$disease,
    what = "disease"
  )
  checkmate::assert_names(
    locations,
    subset.of = dimnames(forecasttools::prism_thresholds)$location,
    what = "location"
  )

  return(purrr::map2(locations, diseases, \(x, y) {
    forecasttools::prism_thresholds[x, y, ]
  }))
}


#' Categorize a vector of values into PRISM
#' activity level bins.
#'
#' Uses [categorize_vector()] and [get_prism_cutpoints()].
#'
#' @param values values to categorize
#' @param locations vector of locations of length equal to
#' `values` or a single location for all `values`.
#' @param diseases vector of diseases of length equal to
#' `values` or a single disease for all `values`.
#' @param prism_bin_names Bin names for the PRISM bins.
#' in order from lowest to highest. Must be a vector of
#' length 5. `list(prism_bin_names)` will be passed as the
#' `label_sets` argument to [categorize_vector()].
#' Defaults to the standard PRISM bin names in title case:
#' `c("Very Low", "Low", "Moderate", "High", "Very High")`.
#' @return A factor vector of category labels, equal in length
#' to the input vector `values`.
#' @export
categorize_prism <- function(
  values,
  locations,
  diseases,
  prism_bin_names = c(
    "Very Low",
    "Low",
    "Moderate",
    "High",
    "Very High"
  )
) {
  cutpoints <- get_prism_cutpoints(locations, diseases)

  return(categorize_vector(
    values,
    break_sets = cutpoints,
    label_sets = list(prism_bin_names)
  ))
}
