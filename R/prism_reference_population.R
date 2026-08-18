get_single_prism_reference_population <- function(location, as_of) {
  checkmate::assert_string(location)
  checkmate::assert_date(as_of, len = 1, any.missing = FALSE)

  candidates <- forecasttools::prism_rate_reference_populations |>
    dplyr::filter(
      .data$location == !!location,
      .data$as_of <= !!as_of
    )

  if (nrow(candidates) == 0) {
    cli::cli_abort(
      "No PRISM reference population for location {.val {location}}
       as of {as_of}."
    )
  }

  matches <- candidates |>
    dplyr::filter(.data$as_of == max(.data$as_of))

  checkmate::assert_data_frame(matches, nrows = 1)

  return(matches$population)
}

#' Get the PRISM reference population for a location.
#'
#' Returns the population denominator PRISM used to
#' derive its rate-scale activity level cutpoints.
#'
#' @param location location(s) for which to return the
#' reference population, as a two-letter abbreviation.
#' Use [forecasttools::us_location_recode] with
#' `location_output_format = "abbr"` to convert to this
#' format.
#' @param as_of single date for which the reference
#' population is valid, applied to every `location`.
#' Defaults to today.
#' @return The reference populations, as a numeric
#' vector with one entry per `location`.
#'
#' @examples
#' get_prism_reference_population("WA")
#'
#' get_prism_reference_population(c("WA", "CA"))
#'
#' get_prism_reference_population(
#'   "WA",
#'   as.Date("2026-08-13"))
#'
#' @seealso [prism_rate_reference_populations],
#' [get_prism_cutpoints()]
#'
#' @export
get_prism_reference_population <- function(
  location,
  as_of = lubridate::today()
) {
  target_location <- stringr::str_to_lower(location)

  as_of <- lubridate::as_date(as_of)

  return(purrr::map_dbl(
    target_location,
    \(location) {
      get_single_prism_reference_population(location, as_of)
    }
  ))
}
