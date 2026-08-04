#' Lookup one set of cutpoints in a signal's threshold
#' array, using that array's `unit` dimension (only if
#' it has one).
#'
#' @param thresholds one signal's cutpoint array.
#' @param disease disease to look up.
#' @param location location to look up.
#' @param as_of `as_of` date to look up, as a string.
#' @param unit unit to look up.
#' @return the cutpoints, as a numeric vector.
index_prism_thresholds <- function(
  thresholds,
  disease,
  location,
  as_of,
  unit
) {
  index <- list(thresholds, TRUE, disease, location, as_of)

  if ("unit" %in% names(dimnames(thresholds))) {
    index <- c(index, list(unit))
  }

  return(do.call("[", index))
}


#' Get PRISM activity level cutpoints given
#' disease and location.
#'
#' @param disease disease(s) for which to return the
#' cutpoints. One of `"ARI"`, `"COVID-19"`,
#' `"Influenza"`, or `"RSV"`, or an array of those
#' values. NHSN provides no `"ARI"` thresholds.
#' @param location location(s) for which to return the
#' cutpoints, as a two-letter abbreviation. Use
#' [forecasttools::us_location_recode] with
#' `location_output_format = "abbr"` to convert to this
#' format. N.B. NHSN covers fewer locations than NSSP.
#' @param as_of date for which the cutpoints are valid.
#' Defaults to today.
#' @param signal surveillance signal for which to
#' return the cutpoints. One of `"NSSP"` (proportions
#' of emergency department visits) or `"NHSN"` (weekly
#' hospital admissions). If not given, defaults to
#' `"NSSP"` with a deprecation warning; a future
#' version will require it.
#' @param unit unit in which to return the cutpoints.
#' `"prop"` for NSSP; `"count"` (weekly hospital
#' admissions) or `"rate"` (weekly hospital admissions
#' per 100K population) for NHSN. If `NULL` (the
#' default), uses the default unit for `signal` given
#' in [default_prism_units]. CDC defines the NHSN cut
#' points on rates and scales them by population to get
#' counts, so `"count"` and `"rate"` assign identical
#' activity levels to the same underlying data.
#' @return The cutpoints, as a list of vectors.
#'
#' @export
get_prism_cutpoints <- function(
  location,
  disease,
  as_of = lubridate::today(),
  signal = lifecycle::deprecated(),
  unit = NULL
) {
  if (!lifecycle::is_present(signal)) {
    lifecycle::deprecate_warn(
      "0.1.8",
      "get_prism_cutpoints(signal = 'must be supplied')",
      details = prism_signal_deprecation_details
    )
    signal <- default_prism_signal
  }

  target_signal <- stringr::str_to_lower(signal)
  checkmate::assert_names(
    target_signal,
    subset.of = names(forecasttools::prism_thresholds),
    what = "signal"
  )
  thresholds <- forecasttools::prism_thresholds[[target_signal]]

  target_unit <- stringr::str_to_lower(
    unit %||% default_prism_units[[target_signal]]
  )
  checkmate::assert_names(
    target_unit,
    subset.of = dimnames(thresholds)$unit %||%
      default_prism_units[[target_signal]],
    what = "unit"
  )

  target_location <- stringr::str_to_lower(location)
  target_disease <- stringr::str_to_lower(disease)

  as_of <- lubridate::as_date(as_of)

  checkmate::assert_scalar(as_of)
  available_as_ofs <- thresholds |>
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
    subset.of = dimnames(thresholds)$disease,
    what = "disease"
  )
  checkmate::assert_names(
    target_location,
    subset.of = dimnames(thresholds)$location,
    what = "location"
  )

  return(purrr::map2(target_disease, target_location, \(x, y) {
    index_prism_thresholds(thresholds, x, y, target_as_of, target_unit)
  }))
}

#' Categorize a vector of value into PRISM
#' activity level bins.
#'
#' Uses [categorize_vector()] and [get_prism_cutpoints()].
#'
#' @param value value to categorize
#' @param location vector of location of length equal
#' to `value` or a single location for all `value`.
#' @param disease vector of disease of length equal to
#' `value` or a single disease for all `value`.
#' @param as_of date for which to get the PRISM
#' cutpoints. Defaults to today.
#' @param prism_bin_names Bin names for the PRISM bins.
#' in order from lowest to highest. Must be a vector of
#' length 5. `list(prism_bin_names)` will be passed as
#' the `label_sets` argument to [categorize_vector()].
#' Defaults to the standard PRISM bin names in title
#' case: `default_prism_bin_names`.
#' @inheritParams get_prism_cutpoints
#' @return A factor vector of category labels, equal in
#' length to the input vector `value`.
#'
#' @export
categorize_prism <- function(
  value,
  location,
  disease,
  as_of = lubridate::today(),
  prism_bin_names = default_prism_bin_names,
  signal = lifecycle::deprecated(),
  unit = NULL
) {
  if (!lifecycle::is_present(signal)) {
    lifecycle::deprecate_warn(
      "0.1.8",
      "categorize_prism(signal = 'must be supplied')",
      details = prism_signal_deprecation_details
    )
    signal <- default_prism_signal
  }

  cutpoints <- get_prism_cutpoints(
    location,
    disease,
    as_of,
    signal = signal,
    unit = unit
  )

  return(categorize_vector(
    value,
    break_sets = cutpoints,
    label_sets = list(prism_bin_names)
  ))
}
