#' Table of US location names and codes
#'
#' A table of US location names and codes, assembled from a
#' US census reference table with the addition of "US" for the
#' United States as a whole.
#'
#' @format
#' A tibble with 58 rows and 4 columns:
#' \describe{
#'   \item{code}{2-character location code}
#'   \item{abbr}{2-character location abbreviation}
#'   \item{hrd}{location abbreviation as in the `jurisdiction` column of the NHSN Hospital Respiratory Data (HRD) dataset. Same as `abbr` for subnational jurisdictions, but `USA` for the United States as a whole.}
#'   \item{name}{Full name of the location}
#' }
#' @source <https://www2.census.gov/geo/docs/reference/state.txt>
#' @source data-raw/us_location_table.R
"us_location_table"

#' Population estimates for US locations
#'
#' `r lifecycle::badge("deprecated")`
#'
#' A table of population estimates for the United States
#' as a whole and for US states, using the most recent
#' Census Population Estimates Program vintage available
#' when the dataset was generated.
#'
#' @section Deprecation:
#' This table is a single, unvintaged snapshot of
#' whichever Census vintage was current when it was
#' last generated (cannot be aligned to point in time).
#'
#' @format
#' A tibble with 53 rows and 2 columns:
#' \describe{
#'   \item{name}{Full name of the location}
#'   \item{population}{Estimated population count}
#' }
#' @source Census Population Estimates Program via
#' `tidycensus::get_estimates()`
#' @source data-raw/us_location_pop.R
#' @seealso [prism_rate_reference_populations],
#' [get_prism_reference_population()]
"us_location_pop"

#' Reference populations underlying PRISM rate
#' cutpoints.
#'
#' The population denominators PRISM used to derive its
#' rate-scale activity level cutpoints, vintaged by
#' `as_of` in the same way as [prism_thresholds].
#'
#' @format
#' A tibble with 3 columns:
#' \describe{
#'   \item{location}{Two-letter location abbreviation, lowercase}
#'   \item{as_of}{Date the reference population took effect}
#'   \item{population}{Population denominator used by PRISM}
#' }
#' @source data-raw/prism_thresholds.R
#' @seealso [get_prism_reference_population()],
#' [prism_thresholds]
"prism_rate_reference_populations"

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

#' Example InferenceData DataFrame
#'
#' An InferenceData Dataframe with
#'
#' @format ## `ex_inferencedata_dataframe`
#' A data frame with 40 rows and 20 columns:
#' @source <data-raw/ex_inferencedata_dataframe.R>
"ex_inferencedata_dataframe"

#' PRISM respiratory virus activity level thresholds.
#'
#' A [`tibble`][tibble::tibble()] of PRISM respiratory
#' virus activity level thresholds (one row per
#' combination of `as_of`, `signal`, `disease`, and
#' `location`):
#' \describe{
#'   \item{as_of}{Date from which the row's thresholds
#'   are valid.}
#'   \item{signal}{Surveillance signal, `nssp`
#'   (emergency department visit thresholds, as
#'   proportions of visits, bounded above by 1) or
#'   `nhsn` (hospital admission thresholds, as weekly
#'   admissions per 100K population).}
#'   \item{disease}{`Influenza`, `COVID-19`, `RSV`, or
#'   `ARI` (acute respiratory infections).}
#'   \item{location}{US jurisdiction or the United
#'   States as a whole, using two-letter codes (the
#'   values of `abbr`) in [us_location_table].}
#'   \item{values}{List column of named numeric vectors
#'   of cutpoints, named `very_low`, `low`, `moderate`,
#'   `high`, `very_high`, and `upper_bound`.}
#' }
#'
#' @source <data-raw/prism_thresholds.R>
"prism_thresholds"
