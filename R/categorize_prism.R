default_prism_signal <- "nssp"

prism_signal_deprecation_details <- glue::glue(
  'Defaulting to `signal = "{default_prism_signal}"`. ',
  "PRISM thresholds are now available for both NSSP and NHSN."
)

prism_bin_names_from_cutpoints <- function(cutpoints) {
  return(
    names(cutpoints) |>
      utils::head(-1) |>
      stringr::str_replace_all("_", " ") |>
      stringr::str_to_title()
  )
}

get_single_prism_cutpoint <- function(signal, disease, location, as_of) {
  checkmate::assert_string(signal)
  checkmate::assert_string(disease)
  checkmate::assert_string(location)
  checkmate::assert_date(as_of, len = 1, any.missing = FALSE)

  candidates <- forecasttools::prism_thresholds |>
    dplyr::filter(
      .data$signal == !!signal,
      .data$disease == !!disease,
      .data$location == !!location,
      .data$as_of <= !!as_of
    )

  if (nrow(candidates) == 0) {
    cli::cli_abort(
      "No PRISM cutpoints for signal {.val {signal}}, disease
       {.val {disease}}, and location {.val {location}} as of {as_of}."
    )
  }

  matches <- candidates |>
    dplyr::filter(.data$as_of == max(.data$as_of))

  checkmate::assert_data_frame(matches, nrows = 1)

  return(matches$values[[1]])
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
#' @param as_of single date for which the cutpoints are
#' valid, applied to every `location`, `disease`, and
#' `signal`. Defaults to today.
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
#' @examples
#' get_prism_cutpoints("WA", "Influenza", signal = "NHSN")
#'
#' get_prism_cutpoints(c("US", "WA"), "COVID-19", signal = "NSSP")
#'
#' get_prism_cutpoints(
#'   c("US", "WA"),
#'   c("COVID-19", "RSV"),
#'   as.Date("2025-01-01"),
#'   signal = "NSSP"
#' )
#'
#' get_prism_cutpoints("WA", "Influenza", signal = c("NSSP", "NHSN"))
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

  target_signal <- stringr::str_to_lower(signal)
  target_location <- stringr::str_to_upper(location)
  target_disease <- stringr::str_to_lower(disease)

  as_of <- lubridate::as_date(as_of)

  return(purrr::pmap(
    list(target_disease, target_location, target_signal),
    \(disease, location, signal) {
      get_single_prism_cutpoint(signal, disease, location, as_of)
    }
  ))
}

#' Categorize a numeric vector into PRISM
#' activity level bins.
#'
#' Uses [categorize_vector()] and [get_prism_cutpoints()].
#'
#' @param value numeric vector to categorize
#' @param location vector of location of length equal
#' to `value` or a single location for all `value`.
#' @param disease vector of disease of length equal to
#' `value` or a single disease for all `value`.
#' @param prism_bin_names Bin names for the PRISM bins,
#' in order from lowest to highest. Must be a vector of
#' length 5. `list(prism_bin_names)` will be passed as
#' the `label_sets` argument to [categorize_vector()].
#' If `NULL` (the default), derived from the cutpoint
#' names by dropping the upper bound and converting to
#' title case, giving `"Very Low"`, `"Low"`,
#' `"Moderate"`, `"High"`, and `"Very High"`.
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
  prism_bin_names = NULL,
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

  if (is.null(prism_bin_names)) {
    prism_bin_names <- prism_bin_names_from_cutpoints(cutpoints[[1]])
  }

  return(categorize_vector(
    value,
    break_sets = cutpoints,
    label_sets = list(prism_bin_names)
  ))
}
