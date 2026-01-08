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

#' PRISM respiratory virus activity level thresholds
#'
#' A multi-dimensional array with PRISM
#' respiratory virus activity level thresholds.
#' Dimensions, in order, are `location`,
#' `disease`, and `breaks`.
#'
#' Values of `disease` are `Influenza`, `COVID-19`,
#' `RSV`, and `ARI` (acute respiratory infections).
#'
#' Values of `breaks` are `prop_very_low`,
#' `prop_low`, `prop_moderate`, `prop_high`,
#' `prop_very_high`, and `prop_upper_bound`.
#'
#' Values of `location` are US jurisdictions
#' and the United States as a whole, using
#' USPS two-letter codes (the values of `abbr`)
#' in [us_location_table].
#'
#' @source <data-raw/prism_thresholds.R>
"prism_thresholds"
