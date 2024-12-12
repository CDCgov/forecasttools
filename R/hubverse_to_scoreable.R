#' Join hubverse forecast to observed data to
#' create a `scoringutils`-ready quantile forecast table.
#'
#' Expects forecast output in hubverse format (
#' e.g. as created by [get_hubverse_table()])
#' and an observed data table with location, date, and value columns.
#' The column names in the observed data table can be configured;
#' defaults are `"location"`, `"reference_date"`, and
#' `"value"`, respectively (i.e. direct correspondence with
#' standard hubverse format).
#'
#' @param hubverse_quantile_table quantile forecasts,
#' as a hubverse-format [`tibble`][tibble::tibble()] produced by
#' `target_end_date`, `value`, and `horizon`
#' @param observation_table observations, as a [`tibble`][tibble::tibble()].
#' @param observed_value_column Name of the column containing
#' observed values in the `observed` table, as a string.
#' Default `"value"`
#' @param observed_location_column Name of the column containing
#' location values in the `observed` table, as a string.
#' Default `"location"`
#' @param observed_date_column Name of the column containing
#' date values in the `observed` table, as a string.
#' Default `"reference_date"`
#' @return A [`data.table`][data.table::data.table()] for scoring,
#' as the output of [scoringutils::as_forecast_quantile()].
#' @export
hub_quantiles_to_scoreable <-
  function(hubverse_quantile_table,
           observation_table,
           observed_value_column = "value",
           observed_location_column = "location",
           observed_date_column = "reference_date") {
    obs <- observation_table |>
      dplyr::select(
        location = .data[[observed_location_column]],
        target_end_date = .data[[observed_date_column]],
        observed = .data[[observed_value_column]]
      )

    scoreable_table <- hubverse_quantile_table |>
      dplyr::inner_join(obs,
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

    return(scoreable_table)
  }
