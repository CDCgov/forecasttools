#' Get PRISM activity level cutpoints for given
#' disease and location
#'
#' @param disease disease(s) for which to return the cutpoints.
#' One of `"ARI"`, `"COVID-19"`, `"Influenza"`, or `"RSV"`, or
#' an array of those value.
#' @param location location(s) for which to return the cutpoints, as
#' a two-letter abbreviation. Use [forecasttools::us_location_recode] with
#' `location_output_format = "abbr"` to convert to this format.
#' @param as_of date for which the cutpoints are valid. Defaults to today.
#' @return The cutpoints, as a list of vectors.
#'
#' @examples
#' get_prism_cutpoints("WA", "Influenza")
#'
#' get_prism_cutpoints(c("US", "WA"), "COVID-19")
#'
#' get_prism_cutpoints(c("US", "WA"), c("ARI", "RSV"), as.Date("2025-01-01"))
#' @export
get_prism_cutpoints <- function(location, disease, as_of = lubridate::today()) {
  target_location <- stringr::str_to_lower(location)
  target_disease <- stringr::str_to_lower(disease)

  as_of <- lubridate::as_date(as_of)

  checkmate::assert_scalar(as_of)
  available_as_ofs <-
    forecasttools::prism_thresholds |>
    dimnames() |>
    purrr::pluck("as_of") |>
    lubridate::as_date()

  target_as_of <- max(available_as_ofs[as_of >= available_as_ofs]) |>
    as.character()

  if (target_as_of == "-Inf") {
    stop(
      "No available PRISM cutpoints for as_of date ",
      as.character(as_of),
      ". Earliest available date is ",
      as.character(min(available_as_ofs)),
      "."
    )
  }

  checkmate::assert_names(
    target_disease,
    subset.of = dimnames(forecasttools::prism_thresholds)$disease,
    what = "disease"
  )
  checkmate::assert_names(
    target_location,
    subset.of = dimnames(forecasttools::prism_thresholds)$location,
    what = "location"
  )

  return(purrr::map2(target_disease, target_location, \(x, y) {
    forecasttools::prism_thresholds[, x, y, target_as_of]
  }))
}

#' Categorize a vector of value into PRISM
#' activity level bins.
#'
#' Uses [categorize_vector()] and [get_prism_cutpoints()].
#'
#' @param value value to categorize
#' @param location vector of location of length equal to
#' `value` or a single location for all `value`.
#' @param disease vector of disease of length equal to
#' `value` or a single disease for all `value`.
#' @param prism_bin_names Bin names for the PRISM bins.
#' in order from lowest to highest. Must be a vector of
#' length 5. `list(prism_bin_names)` will be passed as the
#' `label_sets` argument to [categorize_vector()].
#' Defaults to the standard PRISM bin names in title case:
#' `default_prism_bin_names`.
#' @return A factor vector of category labels, equal in length
#' to the input vector `value`.
#' @export
categorize_prism <- function(
  value,
  location,
  disease,
  prism_bin_names = default_prism_bin_names
) {
  cutpoints <- get_prism_cutpoints(location, disease)

  return(categorize_vector(
    value,
    break_sets = cutpoints,
    label_sets = list(prism_bin_names)
  ))
}
