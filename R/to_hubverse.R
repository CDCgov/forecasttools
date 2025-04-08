#' Get a correctly-formatted set of hubverse
#' `target_end_date` values for the given reference
#' date and set of horizons.
#'
#' @param reference_date hub forecasting reference date
#' @param horizons Vector of forecast horizons to include,
#' in weeks ahead of the `reference_date`.
#' @return a vector of target end dates
#' @param horizon_timescale Either "days" or "weeks"
#' @export
target_end_dates_from_horizons <- function(reference_date,
                                           horizons,
                                           horizon_timescale =
                                             c("days", "weeks")) {
  rlang::arg_match(horizon_timescale)

  time_to_add <- switch(horizon_timescale,
    days = lubridate::days(horizons),
    weeks = lubridate::weeks(horizons)
  )
  target_end_dates <- lubridate::as_date(reference_date) + time_to_add

  return(target_end_dates)
}


#' Compute Horizon from Target End Dates
#'
#' @param reference_date hub forecasting reference date
#' @param target_end_dates vector of target end dates
#' @param horizon_timescale Either "days" or "weeks"
#'
#' @returns a vector of forecast horizons, in the specified timescale
#' @export
horizons_from_target_end_dates <- function(reference_date,
                                           target_end_dates,
                                           horizon_timescale) {
  horizons <- lubridate::time_length(
    as.Date(target_end_dates) - as.Date(reference_date),
    unit = horizon_timescale
  )
  checkmate::assert_integerish(horizons)
  return(horizons)
}

#' Format quantile forecasts for a hubverse submission
#'
#' @param quantile_forecasts tidy data frame of
#' quantile forecasts to format,
#' which should have a location column, a column of
#' quantile values, a column of quantile levels,
#' and columns indicating the target end date.
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
#' @param timepoint_col names of the columns indicating the target end date.
#' @param target_end_dates Vector of target end dates to include,
#' @param horizons Vector of forecast horizons to include,
#' in `horizon_timescale`s ahead of the `reference_date`.
#' @param horizon_timescale Either "days" or "weeks"
#' @param excluded_locations locations to exclude from the table
#' Default c("60", "78") (American Samoa and the US Virgin Islands,
#' for which FluSight does not currently accept forecasts).
#' @param quantile_tol Round quantile level values to this many digits
#' @return a properly formatted table, as a [`tibble`][tibble::tibble].
#' @name get_hubverse_quantile_table
#' @export
get_hubverse_quantile_table <- function(quantile_forecasts,
                                        reference_date,
                                        target_name,
                                        quantile_value_col = "quantile_value",
                                        quantile_level_col = "quantile_level",
                                        location_col = "location",
                                        timepoint_col = "target_end_date",
                                        target_end_dates = NULL,
                                        horizons = NULL,
                                        horizon_timescale =
                                          c("days", "weeks"),
                                        excluded_locations = NULL,
                                        quantile_tol = 4) {
  rlang::arg_match(horizon_timescale)

  checkmate::assert_names(colnames(quantile_forecasts),
    must.include = timepoint_col
  )

  if (is.null(target_end_dates) && is.null(horizons)) {
    cli::cli_abort(
      message =
        "Either `target_end_dates` or `horizons` must be provided."
    )
  } else if (!is.null(target_end_dates) && !is.null(horizons)) {
    cli::cli_abort(
      message =
        "Only one of `target_end_dates` or `horizons` can be provided."
    )
  } else if (is.null(target_end_dates)) {
    target_end_dates <- target_end_dates_from_horizons(reference_date,
      horizons = horizons,
      horizon_timescale = horizon_timescale
    )
  } else {
    horizons <- horizons_from_target_end_dates(reference_date,
      target_end_dates = target_end_dates,
      horizon_timescale = horizon_timescale
    )
  }

  horizon_date_tbl <- tibble::tibble(
    !!timepoint_col := target_end_dates,
    "horizon" = horizons
  )

  output_tbl <- quantile_forecasts |>
    dplyr::select(
      value = {{ quantile_value_col }},
      location = {{ location_col }},
      tidyselect::all_of(timepoint_col),
      quantile_level = {{ quantile_level_col }}
    ) |>
    dplyr::mutate(
      "reference_date" = reference_date,
      "target" = target_name,
      "horizon_timescale" = horizon_timescale
    ) |>
    dplyr::inner_join(horizon_date_tbl, by = timepoint_col) |>
    dplyr::filter(!(.data$location %in% !!excluded_locations)) |>
    dplyr::mutate(
      output_type = "quantile",
      output_type_id = round(.data$quantile_level,
        digits = quantile_tol
      )
    ) |>
    dplyr::select(
      "reference_date",
      "target",
      "horizon",
      "horizon_timescale",
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

  return(output_tbl)
}


#' @param reference_dow Which day of the week should the reference_date
#' be, as an integer?
#' @param week_start Starting day of the week relative to
#' Monday, as an integer. See the [lubridate::wday()] documentation for details.
#' @rdname get_hubverse_quantile_table
#' @export
get_epiweekly_hubverse_table <- function(quantile_forecasts,
                                         reference_date,
                                         target_name,
                                         quantile_value_col = "quantile_value",
                                         quantile_level_col = "quantile_level",
                                         location_col = "location",
                                         timepoint_col = "target_end_date",
                                         target_end_dates = NULL,
                                         horizons = NULL,
                                         reference_dow = NULL,
                                         week_start = NULL,
                                         excluded_locations = NULL,
                                         quantile_tol = 4) {
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

  get_hubverse_quantile_table(quantile_forecasts,
    reference_date,
    target_name,
    quantile_value_col,
    quantile_level_col,
    location_col,
    timepoint_col,
    target_end_dates,
    horizons,
    horizon_timescale = "weeks",
    excluded_locations,
    quantile_tol
  )
}

#' @rdname get_hubverse_quantile_table
#' @export
get_flusight_hub_table <- function(quantile_forecasts,
                                   reference_date,
                                   target_name,
                                   quantile_value_col =
                                     "quantile_value",
                                   quantile_level_col = "quantile_level",
                                   location_col = "location",
                                   timepoint_col = "target_end_date",
                                   quantile_tol = 4) {
  get_epiweekly_hubverse_table(
    quantile_forecasts,
    reference_date,
    target_name,
    quantile_value_col,
    quantile_level_col,
    location_col,
    timepoint_col,
    horizons = -1:3,
    reference_dow = 7,
    week_start = 7,
    excluded_locations = c("60", "78"),
    quantile_tol
  )
}

#' @rdname get_hubverse_quantile_table
#' @export
get_covid_hub_table <- function(quantile_forecasts,
                                reference_date,
                                target_name,
                                quantile_value_col =
                                  "quantile_value",
                                quantile_level_col = "quantile_level",
                                location_col = "location",
                                timepoint_col = "target_end_date",
                                quantile_tol = 4) {
  get_epiweekly_hubverse_table(
    quantile_forecasts,
    reference_date,
    target_name,
    quantile_value_col,
    quantile_level_col,
    location_col,
    timepoint_col,
    horizons = -1:3,
    reference_dow = 7,
    week_start = 7,
    excluded_locations = c("60", "78"),
    quantile_tol
  )
}
