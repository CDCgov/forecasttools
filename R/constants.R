#' Internal and user-exposed constants for forecasttools.

#' Names of the standard columns for hubverse tables.
#' Alias for [hubUtils::std_colnames].
#'
#' @export
hubverse_std_colnames <- hubUtils::std_colnames

#' Names of additional standard columns for
#' the CDC FluSight and COVID-19 Forecast Hubs
#' beyond those in [hubverse_std_colnames].
cdc_hub_additional_colnames <- c(
  "reference_date",
  "target",
  "horizon",
  "target_end_date",
  "location"
) |>
  purrr::set_names()

#' Names of standard columns for the
#' the CDC FluSight and COVID-19 Forecast Hubs
#' @export
cdc_hub_std_colnames <- c(hubverse_std_colnames, cdc_hub_additional_colnames)

#' Alias for [cdc_hub_std_colnames].
#' @rdname cdc_hub_std_colnames
#' @export
covidhub_std_colnames <- cdc_hub_std_colnames

#' Alias for [cdc_hub_std_colnames].
#' @rdname cdc_hub_std_colnames
#' @export
flusight_std_colnames <- cdc_hub_std_colnames

#' Integer start date of a USA MMWR epiweek in ISO numbering.
#' Value (7) corresponds to Sunday.
mmwr_epiweek_start <- 7L

#' Integer start date of an ISO in ISO numbering.
#' Value (1) corresponds to Monday.
isoweek_start <- 1L
