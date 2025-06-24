#' Compute the set of forecasts present for a given value of a given
#' column.
#'
#' @param tbl Table of forecasts or scores to filter,
#' as a valid input to [scoringutils::get_forecast_unit()].
#' @param comparator_value Character scalar of comparator values
#' for which to compute shared forecasts.
#' @param compare Name of the column containing the comparator values.
#' Default `"model"`.
#' @return Table of forecast unit values for all and only the subset
#' forecasts, which can be joined to the original table for filtering
#' purposes (see [filter_to_subset_forecasts()]).
#'
#' @seealso [filter_to_subset_forecasts()]
#'
#' @examples
#'
#' get_subset_forecasts(scoringutils::example_quantile, "EuroCOVIDhub-ensemble")
#'
#' @export
get_subset_forecasts <- function(tbl, comparator_value, compare = "model") {
  forecast_unit <- scoringutils::get_forecast_unit(tbl)
  checkmate::assert_scalar(comparator_value)
  checkmate::assert_scalar(compare)
  checkmate::assert_names(names(tbl), must.include = compare)

  ## remove compare column from 'by' before grouping
  join_by <- setdiff(forecast_unit, compare)

  subset_forecasts <- tbl |>
    tibble::as_tibble() |>
    dplyr::filter(.data[[compare]] == !!comparator_value) |>
    dplyr::distinct(dplyr::across(tidyselect::all_of(join_by)))

  return(subset_forecasts)
}

#' Filter a table of forecasts to include only those present
#' for a value of a given column.
#'
#' @param tbl Table of forecasts or scores to filter,
#' as a valid input to [scoringutils::get_forecast_unit()].
#' @param comparator_value Character scalar of comparator value
#' for which to compute subset forecasts.
#' @param compare Name of the column containing the comparator value.
#' Default `"model"`.
#' @return Table filtered to containing only forecasts that are also
#' present in the `comparator_value` forecasts.
#'
#' @seealso [get_subset_forecasts()]
#'
#' @examples
#'
#' filter_to_subset_forecasts(
#'     scoringutils::example_quantile,
#'     "EuroCOVIDhub-ensemble")
#'
#' scoringutils::example_quantile |>
#'     scoringutils::score() |>
#'     filter_to_subset_forecasts("EuroCOVIDhub-ensemble")
#'
#' @export
filter_to_subset_forecasts <- function(
  tbl,
  comparator_value,
  compare = "model"
) {
  checkmate::assert_names(names(tbl), must.include = compare)
  checkmate::assert_scalar(comparator_value)
  checkmate::assert_scalar(compare)

  subset_forecasts <- get_subset_forecasts(
    tbl,
    comparator_value = comparator_value,
    compare = compare
  )

  return(dplyr::inner_join(tbl, subset_forecasts, by = names(subset_forecasts)))
}
