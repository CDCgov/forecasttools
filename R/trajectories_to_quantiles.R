#' Aggregate individual trajectory
#' timeseries or forecasts to quantile
#' timeseries or forecasts
#'
#' Given a tidy data frame of
#' trajectories, aggregate it to
#' a quantile timeseries for the
#' given quantile values
#'
#' @param trajectories tidy data frame or tibble
#' of trajectories
#' @param quantiles quantiles to output for each
#' timepoint (default the FluSight/COVIDHub 2024-25 quantiles:
#' `c(0.01, 0.025, 1:19/20, 0.975, 0.99)`
#' @param timepoint_cols name of the column(s) in`trajectories`
#' that identifies unique timepoints. Default `timepoint`.
#' @param value_col name of the column in `trajectories`
#' with the trajectory values (for which we wish to
#' compute quantiles), e.g. `hosp`, `weekly_hosp`, `cases`,
#' etc. Default `value`.
#' @param quantile_value_name What to name
#' the column containing quantile values in
#' the output table. Default `"quantile_value"`
#' @param quantile_level_name What to name
#' the column containing quantile levels in
#' the output table. Default `"quantile_level"`
#' @param id_cols additional id columns in
#' `trajectories` to group by before aggregating,
#' e.g. a `location` column if `trajectories` contains
#' trajectories over the same time period for multiple
#' locations, such as different US States and Territories.
#' If NULL, ignored. Default NULL.
#' @export
trajectories_to_quantiles <- function(trajectories,
                                      quantiles = c(
                                        0.01, 0.025,
                                        1:19 / 20,
                                        0.975, 0.99
                                      ),
                                      timepoint_cols = "timepoint",
                                      value_col = "value",
                                      quantile_value_name =
                                        "quantile_value",
                                      quantile_level_name =
                                        "quantile_level",
                                      id_cols = NULL) {
  grouped_df <- trajectories |>
    dplyr::rename(
      value_col = {{ value_col }}
    ) |>
    dplyr::group_by(
      dplyr::across(c(
        {{ timepoint_cols }}, {{ id_cols }}
      ))
    )

  quant_df <- grouped_df |>
    dplyr::reframe(
      {{ quantile_value_name }} := quantile(value_col,
        probs = !!quantiles
      ),
      {{ quantile_level_name }} := !!quantiles
    )
  return(quant_df)
}
