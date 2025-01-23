#' Get a correctly-formatted set of hubverse
#' `target_end_date` values for the given reference
#' date and set of horizons.
#'
#' @param reference_date hub forecasting reference date
#' @param horizons Vector of forecast horizons to include,
#' in weeks ahead of the `reference_date`.
#' Default -1:3 (FluSight and Covidhub 2024/25 horizons).
#' @return a [`tibble`][tibble::tibble] with columns `reference_date`,
#' `horizon`, `target_end_date`,
#' `epiweek`, and `epiyear`.
#' @export
target_end_dates_from_horizons <- function(reference_date,
                                           horizons = -1:3) {
  return(tibble::tibble(
    reference_date = reference_date,
    horizon = horizons,
    target_end_date = lubridate::ymd(reference_date) +
      lubridate::weeks(horizons),
    epiweek = lubridate::epiweek((target_end_date)),
    epiyear = lubridate::epiyear((target_end_date))
  ))
}

#' Given a set of epiweekly quantile forecasts,
#' format them for a hubverse submission
#'
#' @param quantile_forecasts tidy data frame of
#' quantile forecasts to format,
#' which should have a location column, a column of
#' quantile values, a column of quantile levels,
#' a column indicating the epiweek, and a column
#' indicating the epiyear.
#' @param reference_date reference date for the hub forecast.
#' @param target_name Name of the target for the table.
#' @param quantile_value_col name of the column containing the quantile
#' values. Default `'quantile_value'` (the default output
#' column name from [trajectories_to_quantiles()]).
#' @param quantile_level_col name of the column indicating the
#' quantile level. Default `'quantile_level'` (the default output
#' column name from [trajectories_to_quantiles()]).
#' @param location_col name of the column containing the location
#' value. Default `'location'`.
#' @param epiweek_col name of the column containing the epiweek.
#' Default `'epiweek'`.
#' @param epiyear_col name of the column containing the epiyear.
#' Default `'epiyear'`.
#' @param horizons Vector of forecast horizons to include,
#' in weeks ahead of the `reference_date`.
#' Default -1:3 (FluSight and Covidhub 2024/25 horizons).
#' @param reference_dow Which day of the week should the reference_date
#' be, as an integer? Default 7 (the last day of the week), which
#' with the default value of `week_start` will be Saturday, i.e.
#' the last day of a USA epidemiological week.
#' @param week_start Starting day of the week relative to
#' Monday, as an integer. Default 7, i.e. weeks that start on
#' Sunday, as USA epidemiological weeks do. See the [lubridate::wday()]
#' documentation for details.
#' @param excluded_locations locations to exclude from the table
#' Default c("60", "78") (American Samoa and the US Virgin Islands,
#' for which FluSight does not currently accept forecasts).
#' @return a properly formatted table, as a [`tibble`][tibble::tibble].
#' @export
get_hubverse_table <- function(quantile_forecasts,
                               reference_date,
                               target_name,
                               quantile_value_col = "quantile_value",
                               quantile_level_col = "quantile_level",
                               location_col = "location",
                               epiweek_col = "epiweek",
                               epiyear_col = "epiyear",
                               horizons = -1:3,
                               reference_dow = 7,
                               week_start = 7,
                               excluded_locations = c("60", "78")) {
  dow_supplied <- lubridate::wday(reference_date,
    week_start = week_start,
    label = FALSE
  )
  reference_dow_correct <- dow_supplied == reference_dow
  if (!reference_dow_correct) {
    cli::cli_abort(message = paste0(
      "Expected `reference_date` to be day number {reference_dow} ",
      "of the week. Got {reference_date}, which is day number ",
      "{dow_supplied} of the week, given the provided `week_start` ",
      "value {week_start}."
    ))
  }

  targets <- target_end_dates_from_horizons(
    reference_date,
    horizons = horizons
  ) |>
    dplyr::mutate(
      target = !!target_name
    )

  quants <- quantile_forecasts |>
    dplyr::select(
      value = {{ quantile_value_col }},
      location = {{ location_col }},
      epiweek = {{ epiweek_col }},
      epiyear = {{ epiyear_col }},
      quantile_level = {{ quantile_level_col }}
    ) |>
    dplyr::filter(!(.data$location %in% !!excluded_locations))

  output_table <- dplyr::inner_join(targets,
    quants,
    by = c("epiweek", "epiyear")
  ) |>
    dplyr::mutate(
      output_type = "quantile",
      output_type_id = round(.data$quantile_level,
        digits = 4
      )
    ) |>
    dplyr::select(
      "reference_date",
      "target",
      "horizon",
      "target_end_date",
      "location",
      "output_type",
      "output_type_id",
      "value"
    ) |>
    dplyr::arrange(
      .data$location,
      .data$reference_date,
      .data$target,
      .data$horizon,
      .data$target_end_date,
      .data$output_type,
      .data$output_type_id
    )
  return(output_table)
}
