#' Modify Reference Date
#'
#' `modify_reference_date` applies an arbitrary transformation to the
#' reference_date of a hubverse table and recomputes the horizon, based on the
#' new reference_date and existing `target_end_date`. A new unit for the horizon
#' can also be specified.
#'
#' @param original_hub_tbl A data frame containing the hub table.
#' @param horizon_timescale The timescale for the horizon. Must be a valid
#' argument to [horizons_from_target_end_dates()]. If `NULL`, the horizon from
#' `horizon_timescale_col` is used.
#' @param reference_date_transform A function to transform the reference date.
#' Default is `identity`.
#' @param reference_date_col The name of the reference date column. Default is
#' "reference_date".
#' @param target_end_date_col The name of the target end date column. Default is
#'  "target_end_date".
#' @param horizon_col The name of the horizon column. Default is "horizon".
#' @param horizon_timescale_col The name of the horizon timescale column.
#' Default is "horizon_timescale".
#'
#' @returns A modified data frame with the reference date transformed and
#' horizons recomputed
#' @export
#' @examples
#' example_daily_forecast_flu |>
#'   trajectories_to_quantiles(
#'     id_cols = "location",
#'     timepoint_col = "date",
#'     value_col = "hosp"
#'   ) |>
#'   dplyr::rename(target_end_date = date) |>
#'   get_hubverse_quantile_table(
#'     reference_date = as.Date("2023-10-20"),
#'     horizon_timescale = "weeks",
#'     target_name = "my_target",
#'     timepoint_col = "target_end_date",
#'     horizons = 1:4
#'   ) |>
#'   modify_reference_date(\(x) x - 1, horizon_timescale = "days")
modify_reference_date <- function(
  original_hub_tbl,
  horizon_timescale = NULL,
  reference_date_transform = identity,
  reference_date_col = "reference_date",
  target_end_date_col = "target_end_date",
  horizon_col = "horizon",
  horizon_timescale_col = "horizon_timescale"
) {
  checkmate::assert_names(
    colnames(original_hub_tbl),
    must.include = c(
      reference_date_col,
      target_end_date_col
    )
  )

  optional_cols_check <- checkmate::check_names(
    colnames(original_hub_tbl),
    must.include = c(
      horizon_col,
      horizon_timescale_col
    )
  )

  if (is.null(horizon_timescale)) {
    if (horizon_timescale_col %in% colnames(original_hub_tbl)) {
      horizon_timescale <- original_hub_tbl[[horizon_timescale_col]]
    } else {
      rlang::abort(paste(
        "horizon_timescale_col",
        "must be provided if horizon_timescale is not specified."
      ))
    }
  }

  if (!isTRUE(optional_cols_check)) {
    rlang::warn(paste(
      optional_cols_check,
      "Missing columns will be appended to the modified table.",
      sep = "\n"
    ))
  }

  original_hub_tbl |>
    dplyr::mutate(
      !!reference_date_col := reference_date_transform(.data[[
        reference_date_col
      ]])
    ) |>
    dplyr::mutate(
      !!horizon_col := horizons_from_target_end_dates(
        reference_date = .data[[reference_date_col]],
        target_end_dates = .data[[target_end_date_col]],
        horizon_timescale = !!horizon_timescale
      )
    ) |>
    dplyr::mutate(!!horizon_timescale_col := !!horizon_timescale)
}
