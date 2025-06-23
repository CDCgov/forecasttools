#' Compute the set of forecasts present for all values of a given
#' column.
#'
#' @param tbl Table of forecasts or scores to filter,
#' as a valid input to [scoringutils::get_forecast_unit()].
#' @param comparator_values Character vector of comparator values
#' for which to compute shared forecasts. If `NULL` (default), use all
#' unique values of the column specified in `compare`.
#' @param compare Name of the column containing the comparator values.
#' Default `"model"`.
#' @return Table of forecast unit values for all and only the shared
#' forecasts, which can be joined to the original table for filtering
#' purposes (see [filter_to_shared_forecasts()]).
#'
#' @seealso [filter_to_shared_forecasts()]
#'
#' @examples
#'
#' get_shared_forecasts(scoringutils::example_quantile,
#'                            c("EuroCOVIDhub-ensemble", "UMass-MechBayes"))
#'
#' @export
get_shared_forecasts <- function(
  tbl,
  comparator_values = NULL,
  compare = "model"
) {
  forecast_unit <- scoringutils::get_forecast_unit(tbl)
  checkmate::assert_names(names(tbl), must.include = compare)
  checkmate::assert_scalar(compare)

  if (is.null(comparator_values)) {
    comparator_values <- unique(tbl[[compare]])
  }

  ## remove compare column from 'by' before grouping
  join_by <- setdiff(forecast_unit, compare)

  shared_forecasts <- tbl |>
    tibble::as_tibble() |>
    dplyr::filter(.data[[compare]] %in% !!comparator_values) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(forecast_unit))) |>
    dplyr::summarise(
      models_present = list(.data$model),
      .by = dplyr::all_of(join_by)
    ) |>
    dplyr::filter(purrr::map_lgl(
      .data$models_present,
      \(x) setequal(comparator_values, x)
    )) |>
    dplyr::select(dplyr::all_of(join_by))

  return(shared_forecasts)
}

#' Filter a table of forecasts to include only those present
#' for all values of a given column.
#'
#' @param tbl Table of forecasts or scores to filter,
#' as a valid input to [scoringutils::get_forecast_unit()].
#' @param comparator_values Character vector of comparator values
#' for which to compute shared forecasts. If `NULL` (default), use all
#' unique values of the column specified in `compare`.
#' @param compare Name of the column containing the comparator values.
#' Default `"model"`.
#' @return Table filtered to containing only forecasts corresponding to
#' entries in `comparator_values` that are present for _all_ entries
#' in `comparator_values`.
#'
#' @seealso [get_shared_forecasts()]
#'
#' @examples
#'
#' filter_to_shared_forecasts(
#'     scoringutils::example_quantile,
#'     c("EuroCOVIDhub-ensemble", "UMass-MechBayes"))
#'
#' scoringutils::example_quantile |>
#'     scoringutils::score() |>
#'     filter_to_shared_forecasts(c("EuroCOVIDhub-ensemble",
#'                                  "epiforecasts-EpiNow2",
#'                                  "UMass-MechBayes"))
#' @export
filter_to_shared_forecasts <- function(
  tbl,
  comparator_values = NULL,
  compare = "model"
) {
  checkmate::assert_names(names(tbl), must.include = compare)
  checkmate::assert_scalar(compare)

  if (is.null(comparator_values)) {
    comparator_values <- unique(tbl[[compare]])
  }

  shared_forecasts <- get_shared_forecasts(
    tbl,
    comparator_values = comparator_values,
    compare = compare
  )

  return(dplyr::inner_join(
    dplyr::filter(tbl, .data[[compare]] %in% comparator_values),
    shared_forecasts,
    by = names(shared_forecasts)
  ))
}
