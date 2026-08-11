default_prism_signal <- "nssp"

prism_signal_deprecation_details <- glue::glue(
  'Defaulting to `signal = "{default_prism_signal}"`. ',
  "PRISM thresholds are now available for both NSSP and NHSN."
)

prism_signal_as_ofs <- function() {
  as_ofs <- names(forecasttools::prism_thresholds)
  signals_by_as_of <- forecasttools::prism_thresholds |>
    purrr::map(\(x) dimnames(x)$signal)

  signals_by_as_of |>
    unlist(use.names = FALSE) |>
    unique() |>
    rlang::set_names() |>
    purrr::map(\(signal) {
      lubridate::as_date(as_ofs[
        purrr::map_lgl(signals_by_as_of, \(x) signal %in% x)
      ])
    })
}

prism_signals <- function() {
  names(prism_signal_as_ofs())
}

resolve_prism_as_of <- function(signal, as_of, as_of_index) {
  available_as_ofs <- as_of_index[[signal]]
  usable_as_ofs <- available_as_ofs[as_of >= available_as_ofs]

  if (length(usable_as_ofs) == 0) {
    stop(
      "No available PRISM cutpoints for signal ",
      signal,
      " as of date ",
      as.character(as_of),
      ". Earliest available date is ",
      as.character(min(available_as_ofs)),
      "."
    )
  }

  return(as.character(max(usable_as_ofs)))
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
#' format.
#' @param as_of date(s) for which the cutpoints are
#' valid. Defaults to today.
#' @param signal surveillance signal(s) for which to
#' return the cutpoints. One of `"NSSP"` (proportions
#' of emergency department visits) or `"NHSN"` (weekly
#' hospital admissions per 100k population), or an
#' array of those values. If not given, defaults to
#' `"NSSP"` with a deprecation warning (a future
#' version will require it).
#' @return The cutpoints, as a list of vectors, named
#' `very_low`, `low`, `moderate`, `high`, `very_high`,
#' and `upper_bound` for every signal.
#'
#' @export
get_prism_cutpoints <- function(
  location,
  disease,
  as_of = lubridate::today(),
  signal = lifecycle::deprecated()
) {
  if (!lifecycle::is_present(signal)) {
    lifecycle::deprecate_warn(
      "0.1.8",
      "get_prism_cutpoints(signal = 'must be supplied')",
      details = prism_signal_deprecation_details
    )
    signal <- default_prism_signal
  }

  as_of_index <- prism_signal_as_ofs()

  target_signal <- stringr::str_to_lower(signal)
  checkmate::assert_names(
    target_signal,
    subset.of = names(as_of_index),
    what = "signal"
  )

  target_location <- stringr::str_to_lower(location)
  target_disease <- stringr::str_to_lower(disease)

  as_of <- lubridate::as_date(as_of)

  return(purrr::pmap(
    list(target_disease, target_location, target_signal, as_of),
    \(disease, location, signal, as_of) {
      thresholds <- forecasttools::prism_thresholds[[
        resolve_prism_as_of(signal, as_of, as_of_index)
      ]]

      checkmate::assert_names(
        disease,
        subset.of = dimnames(thresholds)$disease,
        what = "disease"
      )
      checkmate::assert_names(
        location,
        subset.of = dimnames(thresholds)$location,
        what = "location"
      )

      thresholds[, disease, location, signal]
    }
  ))
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
  signal = lifecycle::deprecated()
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
    signal = signal
  )

  return(categorize_vector(
    value,
    break_sets = cutpoints,
    label_sets = list(prism_bin_names)
  ))
}
