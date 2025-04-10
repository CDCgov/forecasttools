#' Modify Reference Date
#'
#' @param original_hub_tbl A data frame containing the hub table.
#' @param horizon_timescale The timescale for the horizon. Must be a valid
#' argument to [horizons_from_target_end_dates()].
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

modify_reference_date <- function(original_hub_tbl,
                                  horizon_timescale,
                                  reference_date_transform = identity,
                                  reference_date_col = "reference_date",
                                  target_end_date_col = "target_end_date",
                                  horizon_col = "horizon",
                                  horizon_timescale_col = "horizon_timescale") {
  checkmate::assert_names(
    colnames(original_hub_tbl),
    must.include = c(
      reference_date_col,
      target_end_date_col
    )
  )

  optional_cols_check <- checkmate::checkNames(colnames(original_hub_tbl),
    must.include = c(
      horizon_col,
      horizon_timescale_col
    )
  )

  if (!isTRUE(optional_cols_check)) {
    rlang::warn(paste(
      optional_cols_check,
      "Missing columns will be appended to the modified table.",
      sep = "\n"
    ))
  }

  original_hub_tbl |>
    dplyr::mutate(
      !!reference_date_col :=
        reference_date_transform(.data[[reference_date_col]])
    ) |>
    dplyr::mutate(!!horizon_col := horizons_from_target_end_dates(
      reference_date = .data[[reference_date_col]],
      target_end_dates = .data[[target_end_date_col]],
      horizon_timescale = !!horizon_timescale
    )) |>
    dplyr::mutate(!!horizon_timescale_col := !!horizon_timescale)
}
