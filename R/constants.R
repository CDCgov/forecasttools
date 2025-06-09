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
#' the CDC FluSight and COVID-19 Forecast Hubs, with
#' convenient aliases.
#' @export
cdc_hub_std_colnames <- c(hubverse_std_colnames, cdc_hub_additional_colnames)

#' @rdname cdc_hub_std_colnames
#' @export
covidhub_std_colnames <- cdc_hub_std_colnames

#' @rdname cdc_hub_std_colnames
#' @export
flusight_std_colnames <- cdc_hub_std_colnames

#' Names of PRISM bins
#' @export
default_prism_bin_names <- c("Very Low", "Low", "Moderate", "High", "Very High")


#' Dataset IDs for`data.cdc.gov` datasets of interest:
#'
#' `nhsn_hist_daily`: National Healthcare Safety Network
#' (NHSN) daily-resolution incident hospital admissions timeseries,
#' for COVID-19 and later Influenza, reported until 28 June 2024.
#' Former target for the COVID-19 Forecast Hub.
#'
#' `nhsn_hrd_final`, `nhsn_hrd_prelim`: NHSN Hospital Respiratory Data
#' (HRD).
#' Epiweekly-resolution incident hospital admissions
#' timeseries for COVID-19, Influenza, and RSV.
#' `nhsn_hrd_final` points to the most recent final (Friday) weekly data
#' release. `nhsn_hrd_prelim` points the most recent preliminary (Wednesday)
#' weekly release.
#'
#' `nssp_prop_ed_visits`: National Syndromic Surveillance
#' Program (NSSP) data on proportion of incident emergency
#' deparment (ED) visits due to COVID-19, Influenza, and RSV.
#'
#' @export
data_cdc_gov_ids <- list(
  nhsn_hist_daily = "g62h-syeh",
  nhsn_hrd_prelim = "mpgq-jmmr",
  nhsn_hrd_final = "ua7e-t2fy",
  nssp_prop_ed_visits = "rdmq-nq56"
)
