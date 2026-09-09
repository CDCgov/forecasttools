default_prism_signal <- "nssp"

prism_signal_deprecation_details <- glue::glue(
  'Defaulting to `signal = "{default_prism_signal}"`. ',
  "PRISM thresholds are now available for both NSSP and NHSN."
)

## current dplyr guidance for handling join_by expressions
## https://dplyr.tidyverse.org/articles/in-packages.html#join-helpers
utils::globalVariables(c("closest", "x", "y"))

prism_bin_names_from_cutpoints <- function(cutpoints) {
  return(
    names(cutpoints) |>
      utils::head(-1) |>
      stringr::str_replace_all("_", " ") |>
      stringr::str_to_title()
  )
}


#' Get PRISM activity level cutpoint sets.
#'
#' Cutpoint sets are specific to a particular
#' combination of disease, location, and signal.
#' They are also vintaged; you can look up the set of
#' cutpoints that were in place for a given disease,
#' location, and signal as of any particular date (with
#' an error if none were defined as of that date).
#'
#' This function is vectorized. It recycles
#' the `disease`, `location`, `signal``, and `as_of`
#' arguments to a common length and returns a
#' corresponding list of cutpoint vectors.
#'
#' @param disease disease for which to return the
#' cutpoints. Options are `"ARI"` (NSSP-only),
#' `"COVID-19"`, `"Influenza"`, and `"RSV"`.
#' @param location location for which to return the
#' cutpoints, as a two-letter abbreviation. Use
#' [forecasttools::us_location_recode] with
#' `location_output_format = "abbr"` to convert to this
#' format.
#' @param signal surveillance signal for which to
#' return the cutpoints. Options are `"NSSP"` (proportions
#' of emergency department visits) and `"NHSN"` (weekly
#' hospital admissions per 100k population).
#' If not specified, default to `"NSSP"` with a
#' deprecation warning.
#' @param as_of Retrieve cutpoints that were in place as of
#' this date. Defaults to today (current cutpoints).
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
#'   signal = "NSSP",
#'   as_of = as.Date("2025-01-01")
#' )
#'
#' get_prism_cutpoints("WA", "Influenza", signal = c("NSSP", "NHSN"))
#'
#' @export
get_prism_cutpoints <- function(
  location,
  disease,
  signal = lifecycle::deprecated(),
  as_of = lubridate::today()
) {
  if (!lifecycle::is_present(signal)) {
    lifecycle::deprecate_warn(
      "0.1.8",
      "get_prism_cutpoints(signal = 'must be supplied')",
      details = prism_signal_deprecation_details
    )
    signal <- default_prism_signal
  }

  desired_cutpoints <- tibble::tibble(
    signal = stringr::str_to_lower(signal),
    location = stringr::str_to_upper(location),
    disease = stringr::str_to_lower(disease),
    target_as_of = lubridate::as_date(as_of)
  )

  matches <- rlang::try_fetch(
    dplyr::inner_join(
      desired_cutpoints,
      forecasttools::prism_thresholds,
      by = dplyr::join_by(
        "location",
        "disease",
        "signal",
        closest(x$target_as_of >= y$as_of)
      ),
      unmatched = c("error", "drop"),
      relationship = "many-to-one"
    ),
    error = function(cnd) {
      .raise_prism_cutpoint_lookup_error(
        desired_cutpoints,
        cnd
      )
    }
  )

  return(matches$values)
}

#' Raise a more informative error when PRISM cutpoint lookup
#' fails. In particular, flag the missing cutpoints when possible.
#'
#' @noRd
.raise_prism_cutpoint_lookup_error <- function(desired_cutpoints, cnd) {
  fully_missing_cutpoints <- dplyr::anti_join(
    desired_cutpoints,
    forecasttools::prism_thresholds,
    by = c("location", "disease", "signal")
  )

  if (nrow(fully_missing_cutpoints) > 0) {
    ## cli::cli_abort doesn't yet print tibbles nicely
    ## https://github.com/r-lib/cli/issues/699
    rlang::abort(
      message = "At least one requested set of cutpoints not found for any as-of date",
      body = c(
        "Cutpoints not found:",
        utils::capture.output(fully_missing_cutpoints)
      ),
      parent = cnd
    )
  }

  no_vintage <- desired_cutpoints |>
    dplyr::anti_join(
      forecasttools::prism_thresholds,
      by = dplyr::join_by(
        "location",
        "disease",
        "signal",
        closest(x$target_as_of >= y$as_of)
      )
    )

  if (nrow(no_vintage) > 0) {
    rlang::abort(
      message = "At least one requested set of cutpoints does not have a vintage matching the requested as-of date.",
      body = c(
        "Cutpoints missing a requested vintage:",
        utils::capture.output(no_vintage)
      ),
      parent = cnd
    )
  }

  rlang::abort("Unexpected error retrieving PRISM cutpoints", parent = cnd)
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
