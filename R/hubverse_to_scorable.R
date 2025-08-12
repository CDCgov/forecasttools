#' Join hubverse forecasts to observed data
#'
#' Expects forecast output in hubverse format (e.g. as created by
#' [get_hubverse_quantile_table()]) and an observed data table with location,
#' date, and value columns. The names of these columns the observed
#' data table can be configured; defaults are `"location"`, `"date"`,
#' and `"value"`, respectively (direct correspondence with
#' standard Hub "target data" tables).
#' @param hubverse_forecast_table Forecasts, as a hubverse-format
#' [`tibble`][tibble::tibble()], for instance as produced by
#' [get_hubverse_quantile_table()], with columns including `target_end_date`,
#' and `location`.
#' @param observation_table observations, as a [`tibble`][tibble::tibble()].
#' @param obs_value_column Name of the column containing
#' observed values in the `observed` table, as a string.
#' Default `"value"`
#' @param obs_date_column Name of the column containing
#' date values in the `observed` table, as a string. Will
#' be joined to the `target_end_date` column in the hubverse
#' table. Default `"date"`
#' @param obs_value_name Name for the column of observed values
#' in the resulting table (since `"value"` clashes with the forecast
#' value column in a standard hubverse table. Default `"observed"`.
#' @param id_cols Additional ID columns to join on, in addition to
#' `obs_date_column`. Default `c("location", "target")`.
#' @param join Which SQL-style `dplyr` [mutating join][dplyr::mutate-joins]
#' function to use when joining the tables. Options are `"full"`
#' (for [dplyr::full_join()]), `"left"` ([dplyr::left_join()]),
#' `"right"` ([dplyr::right_join()]), and `"inner"`
#' ([dplyr::inner_join()]).
#' In the join, the hubverse forecast table is the left table
#' (`x`) and the observation table is the right table (`y`).
#' Default `"full"` (i.e. keep all forecasts and observations,
#' even if some forecasts do not have a corresponding observation or
#' some observations do not have a corresponding forecast).
#' @return A [`tibble`][tibble::tibble()] with the observed values
#' added.
#' @export
hubverse_table_with_obs <- function(
  hubverse_forecast_table,
  observation_table,
  obs_value_column = "value",
  obs_date_column = "date",
  obs_value_name = "observed",
  id_cols = c("location", "target"),
  join = "full"
) {
  join_funcs <- list(
    "full" = dplyr::full_join,
    "left" = dplyr::left_join,
    "right" = dplyr::right_join,
    "inner" = dplyr::inner_join
  )
  checkmate::assert_names(
    names(observation_table),
    must.include = c(
      id_cols,
      obs_date_column,
      obs_value_column
    )
  )
  checkmate::assert_names(
    names(hubverse_forecast_table),
    must.include = c(
      id_cols,
      "target_end_date"
    )
  )
  obs <- observation_table |>
    dplyr::select(
      target_end_date = !!obs_date_column,
      !!obs_value_name := !!obs_value_column,
      tidyselect::all_of(id_cols)
    )

  checkmate::assert_names(join, subset.of = names(join_funcs))
  checkmate::assert_names(
    obs_value_name,
    disjunct.from = names(hubverse_forecast_table)
  )
  join_func <- join_funcs[[join]]

  return(join_func(
    hubverse_forecast_table,
    obs,
    by = c(id_cols, "target_end_date")
  ))
}

#' Join hubverse oracle output to a hubverse forecast table for scoring.
#'
#' @param hubverse_table Hubverse format table of forecasts
#' @param oracle_output_table Oracle output table to join to the forecast table.
#' @return The joined table, as a [`tibble`][tibble::tibble()].
#' @examples
#'
#' with_hubverse_oracle_output(hubExamples::forecast_outputs,
#'   hubExamples::forecast_oracle_output)
#'
#' @export
with_hubverse_oracle_output <- function(hubverse_table, oracle_output_table) {
  ## check tables have compatible schemas
  checkmate::assert_names(
    names(hubverse_table),
    disjunct.from = "oracle_value"
  )
  checkmate::assert_names(
    names(oracle_output_table),
    subset.of = c(names(hubverse_table), "oracle_value")
  )
  assert_hubverse_output_types(hubverse_table$output_type)
  assert_hubverse_output_types(oracle_output_table$output_type)

  ## partition problem according to whether or not output_type_id should
  ## be a join column. In the standard hubverse schema NA output_type_id
  ## for an oracle output means that that the single oracle_value should
  ## be associated to any and all available output_type_id levels from
  ## the forecast (e.g. if output type = "quantile" or "sample").
  ## A non-NA oracle_output output_type_id value means that the oracle_value
  ## should be associated _only_ to that output_type_id level from the forecast.
  needs_id <- c(
    "pmf",
    "cdf"
  )
  std_join_cols <- setdiff(
    names(oracle_output_table),
    c("output_type_id", "oracle_value")
  )
  needs_id_rows <- function(x) {
    dplyr::filter(x, .data$output_type %in% !!needs_id)
  }
  no_needs_id_rows <- function(x) {
    dplyr::filter(x, !.data$output_type %in% !!needs_id)
  }
  needs_id_partition <- dplyr::left_join(
    needs_id_rows(hubverse_table),
    needs_id_rows(oracle_output_table),
    by = c(std_join_cols, "output_type_id")
  )
  no_needs_id_partition <- dplyr::left_join(
    no_needs_id_rows(hubverse_table),
    no_needs_id_rows(oracle_output_table) |>
      dplyr::select(-"output_type_id"),
    by = std_join_cols
  )
  return(dplyr::bind_rows(needs_id_partition, no_needs_id_partition))
}

#' Create a `scoringutils`-ready quantile table from a hubverse table
#'
#' Expects quantile forecast output in hubverse format
#' (e.g. as created by [get_hubverse_quantile_table()])
#' and an observed data table with location, date, and value columns.
#' The column names in the observed data table can be configured;
#' defaults are `"location"`, `"date"`, and
#' `"value"`, respectively (i.e. direct correspondence with
#' standard hub target data tables).
#'
#' @param hubverse_quantile_table quantile forecasts,
#' as a hubverse-format [`tibble`][tibble::tibble()], e.g.
#' as produced by [get_hubverse_quantile_table()].
#' @param quantile_tol Round quantile level values to this many
#' decimal places, to avoid problems with floating point number
#' equality comparisons. Passed as the `digits` argument to
#' [base::round()]. Default 10.
#' @return A [`data.table`][data.table::data.table()] for scoring,
#' as the output of [scoringutils::as_forecast_quantile()].
#' @export
quantile_table_to_scorable <- function(
  hubverse_quantile_table,
  hubverse_oracle_output_table,
  quantile_tol = 10
) {
  prep_table <- function(tbl) {
    return(
      dplyr::filter(tbl, .data$output_type == "quantile") |>
        dplyr::mutate(
          output_type_id = as.numeric(.data$output_type_id) |>
            round(digits = !!quantile_tol)
        )
    )
  }

  scorable <- hubverse_quantile_table |>
    prep_table() |>
    with_hubverse_oracle_output(prep_table(hubverse_oracle_output_table)) |>
    scoringutils::as_forecast_quantile(
      predicted = "value",
      observed = "observed",
      quantile_level = "output_type_id"
    )

  return(scorable)
}


#' Create a table for scoring hub model
#' quantile forecasts from a local copy of
#' a hub.
#'
#' Requires a local version of the
#' forecast hub at `hub_path`.
#'
#' @param hub_path Local path to a hubverse-style
#' forecast hub.
#' @param ... keyword arguments passed to
#' [quantile_table_to_scorable()].
#' @return Scorable table, as the output of
#' [scoringutils::as_forecast_quantile()].
#' @export
hub_to_scorable_quantiles <- function(
  hub_path,
  ...
) {
  quantile_forecasts <- gather_hub_quantile_forecasts(hub_path) |>
    dplyr::rename(model = "model_id")
  oracle_output <- hubData::connect_oracle_output(hub_path) |>
    dplyr::filter(output_type == "quantile") |>
    dplyr::collect()
  scorable <- quantile_table_to_scorable(
    quantile_forecasts,
    target_data,
    ...
  )

  return(scorable)
}
