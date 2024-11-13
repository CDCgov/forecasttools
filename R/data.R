#' Table of US location names and codes
#'
#' A table of US location names and codes, assembled from a
#' US census reference table with the addition of "US" for the
#' United States as a whole.
#'
#' @format
#' A tibble with 58 rows and 3 columns:
#' \describe{
#'   \item{location_code}{2-character location code}
#'   \item{short_name}{2-character location abbreviation}
#'   \item{long_name}{Full name of the location}
#' }
#' @source <https://www2.census.gov/geo/docs/reference/state.txt>, us_location_table.R
"us_location_table"

#' An example daily-resolution set of forecast trajectories for influenza.
#'
#' 53 US states and territories are represented, coded according to their
#' USPS two-letter abbreviations: the 50 US states, Puerto Rico,
#' the District of Columbia, and the US Virgin Islands. 100 randomly
#' chosen draws from an original MCMC sample of 4000 are provided
#'
#' @format
#' A tibble with 159,000 rows and 4 columns:
#' \describe{
#'   \item{.draw}{Unique identifier for an individual MCMC draw.}
#'   \item{date}{Date for a forecasted count.}
#'   \item{hosp}{Forecasted count of hospital admissions.}
#'   \item{location}{Location for a forecasted count.}
#' }
#' @source data-raw/example_daily_forecast_flu.R
"example_daily_forecast_flu"
