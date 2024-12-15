#' Join hubverse quantile forecasts to observed data to
#' create a `scoringutils`-ready table.
#'
#' Expects quantile forecast output in hubverse format
#' (e.g. as created by [get_hubverse_table()])
#' and an observed data table with location, date, and value columns.
#' The column names in the observed data table can be configured;
#' defaults are `"location"`, `"date"`, and
#' `"value"`, respectively (i.e. direct correspondence with
#' standard hub target data tables).
#'
#' @param hubverse_quantile_table quantile forecasts,
#' as a hubverse-format [`tibble`][tibble::tibble()] produced by
#' `target_end_date`, `value`, and `horizon`
#' @param observation_table observations, as a [`tibble`][tibble::tibble()].
#' @param obs_value_column Name of the column containing
#' observed values in the `observed` table, as a string.
#' Default `"value"`
#' @param obs_location_column Name of the column containing
#' location values in the `observed` table, as a string.
#' Default `"location"`
#' @param obs_date_column Name of the column containing
#' date values in the `observed` table, as a string.
#' Default `"date"`
#' @return A [`data.table`][data.table::data.table()] for scoring,
#' as the output of [scoringutils::as_forecast_quantile()].
#' @export
quantile_table_to_scoreable <- function(hubverse_quantile_table,
                                        observation_table,
                                        obs_value_column = "value",
                                        obs_location_column = "location",
                                        obs_date_column = "date") {
  obs <- observation_table |>
    dplyr::select(
      location = .data[[obs_location_column]],
      target_end_date = .data[[obs_date_column]],
      observed = .data[[obs_value_column]]
    )

  scoreable <- hubverse_quantile_table |>
    dplyr::filter(.data$output_type == "quantile") |>
    dplyr::mutate(
      output_type_id = as.numeric(.data$output_type_id)
    ) |>
    dplyr::full_join(obs,
      by = c(
        "location",
        "target_end_date"
      )
    ) |>
    scoringutils::as_forecast_quantile(
      predicted = "value",
      observed = "observed",
      quantile_level = "output_type_id"
    )

  return(scoreable)
}


#' Create a table for scoring hub model
#' quantile forecasts from a local copy of
#' a hub.
#'
#' Requires a local version of the
#' forecast hub at `hub_path`.
#'
#' @param hub_path Local path to hubverse-style
#' forecast hub.
#' @param ... keyword arguments passed to
#' [quantile_table_to_scoreable()].
#' @return Scoreable table, as the output of
#' [scoringutils::as_forecast_quantile()].
#' @export
hub_to_scoreable_quantiles <- function(hub_path,
                                       ...) {
  quantile_forecasts <- gather_hub_quantile_forecasts(hub_path)
  target_data <- gather_hub_target_data(hub_path)
  scoreable <- quantile_table_to_scoreable(
    quantile_forecasts,
    target_data,
    ...
  )

  return(scoreable)
}
