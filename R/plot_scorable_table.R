#' Plot median predictions and prediction
#' intervals against observed data by forecast date.
#'
#' @param scorable_table quantile table with observations,
#' as the output of [scoringutils::as_forecast_quantile()],
#' or as a table coercible to a `scoringutils`-ready quantile
#' forecast via [scoringutils::as_forecast_quantile()]. Must
#' contain the median (0.5) quantile and the endpoint quantiles
#' for the equal-tailed prediction interval specified in
#' `prediction_interval_width`.
#' @param horizons Forecast horizons to plot. If `NULL`, plot all
#' available horizons. Default `NULL`.
#' @param prediction_interval_width Width of the equal-tailed prediction
#' interval to plot around the median. Must correspond to quantiles
#' available in `scorable_table`. Default `0.95` (plot the range from
#' quantile `0.025` to quantile `0.975`).
#' @param forecast_date_col Name of the column in `scorable_table`
#' giving the forecast date or forecast reference date.
#' Default `"reference_date"` (as in hubverse schema).
#' @param target_date_col Name of the column in `scorable_table`
#' giving the forecast target date for a given prediction.
#' Default `"target_end_date"` (as in hubverse schema).
#' @param predicted_col Name of the column in `scorable_table`
#' giving the predicted values. Default `"predicted"` (as in
#' the output of [scoringutils::as_forecast_quantile()]. Passed
#' as the `predicted` argument to [scoringutils::as_forecast_quantile()].
#' @param observed_col Name of the column in `scorable_table`
#' giving the observed values. Default `"observed"` (as in
#' the output of [scoringutils::as_forecast_quantile()]. Passed
#' as the `observed` argument to [scoringutils::as_forecast_quantile()].
#' @param quantile_level_col Name of the column in `scorable_table`
#' giving the quantile level for a given row. Default `"quantile_level"`
#' (as in the output of [scoringutils::as_forecast_quantile()].
#' Passed as the `quantile_level` argument to
#' [scoringutils::as_forecast_quantile()].
#' @param horizon_col Name of the column in `scorable_table` containing
#' the forecast horizon for a given row. Default `"horizon"` (as in
#' hubverse schema).
#' @param facet_columns Columns in `scorable_table` by which to
#' facet the resulting plot. Will always facet by `forecast_date_col`.
#' If `NULL`, facet by all the columns that make up the `scoringutils`
#' "forecast unit", as determined by [scoringutils::get_forecast_unit()],
#' except for `target_date_col` and `horizon`. This means that, by default,
#' two distinct predictions for a given target date and horizon should
#' not be visualized on the same facet. To facet _only_ by the forecast date
#' column, pass that column name again, or pass `""`.
#' @param x_label Label for the x axis in the plot. Default
#' `"Date"`.
#' @param y_label Label for the y axis in the plot. Default
#' `"Target"`.
#' @param y_transform Transformation for the y axis in the plot.
#' Passed as the `transform` argument to [ggplot2::scale_y_continuous()]
#' Default `"log10"`.
#' @param quantile_tol Round quantile level values to this many
#' decimal places, to avoid problems with floating point number
#' equality comparisons. Affects both the target quantile level
#' values determined from `prediction_interval_width` and the
#' quantile level values in the `quantile_level_col` column of
#' `scorable_table`. Passed as the `digits` argument to
#' [base::round()]. Default 10.
#' @return The plot, as a [ggplot2::ggplot()] object.
#' @export
#'
#' @examples
#' scoringutils::example_quantile |>
#'   dplyr::filter(
#'     location == "IT",
#'     target_type == "Cases",
#'     model == "EuroCOVIDhub-ensemble"
#'   ) |>
#'   plot_pred_obs_by_forecast_date(
#'     forecast_date_col = "forecast_date",
#'     facet_columns = "" # facet only by forecast date
#'   )
#'
plot_pred_obs_by_forecast_date <- function(
  scorable_table,
  horizons = NULL,
  prediction_interval_width = 0.95,
  forecast_date_col = "reference_date",
  target_date_col = "target_end_date",
  predicted_col = "predicted",
  observed_col = "observed",
  quantile_level_col = "quantile_level",
  horizon_col = "horizon",
  facet_columns = NULL,
  x_label = "Date",
  y_label = "Target",
  y_transform = "log10",
  quantile_tol = 10
) {
  ## ensure that forecast is a scoringutils-format
  ## quantile forecast
  scorable_table <- scorable_table |>
    dplyr::filter(nullable_comparison(
      .data[[horizon_col]],
      "%in%",
      !!horizons
    )) |>
    scoringutils::as_forecast_quantile(
      predicted = predicted_col,
      observed = observed_col,
      quantile_level = quantile_level_col
    )

  if (is.null(facet_columns)) {
    facet_columns <- scoringutils::get_forecast_unit(scorable_table) |>
      purrr::discard(~ .x %in% c(target_date_col, horizon_col))
  }

  ## no longer need scoringutils features, so coerce to
  ## tibble to prevent warnings
  to_plot <- tibble::as_tibble(scorable_table)

  ## compute needed predictive quantiles
  lower_ci <- (1 - prediction_interval_width) / 2
  upper_ci <- 1 - lower_ci
  quantiles_to_plot <- c(lower_ci, 0.5, upper_ci) |>
    round(digits = quantile_tol)

  checkmate::assert_subset(
    quantiles_to_plot,
    scorable_table$quantile_level,
    .var.name = "Quantiles to plot"
  )

  facet_columns <- unique(c(forecast_date_col, facet_columns)) |>
    purrr::discard(~ .x == "")

  to_plot <- to_plot |>
    dplyr::mutate(
      quantile_level = round(.data$quantile_level, digits = quantile_tol)
    ) |>
    dplyr::filter(.data$quantile_level %in% !!quantiles_to_plot) |>
    dplyr::mutate(q_rank = dplyr::dense_rank(.data$quantile_level))

  to_plot_obs <- to_plot |>
    dplyr::filter(.data$q_rank == 1) |>
    dplyr::select(
      dplyr::all_of(c(
        target_date_col,
        "observed",
        facet_columns
      )),
      -dplyr::all_of(forecast_date_col)
    )

  to_plot_forecast <- to_plot |>
    dplyr::select(-"quantile_level") |>
    tidyr::pivot_wider(
      names_from = "q_rank",
      names_glue = "q_{q_rank}",
      values_from = "predicted"
    )

  plot <- to_plot_forecast |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data[[target_date_col]],
      y = .data$q_2,
    )) +
    ggplot2::geom_point(color = "blue") +
    ggplot2::geom_line(
      color = "blue",
      linetype = "dashed"
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data$q_1,
        ymax = .data$q_3
      ),
      fill = "blue",
      alpha = 0.5
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = .data[[target_date_col]],
        y = .data$observed
      ),
      data = to_plot_obs
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(y = .data$observed),
      data = to_plot_obs
    ) +
    ggplot2::facet_wrap(facet_columns) +
    ggplot2::labs(
      x = x_label,
      y = y_label
    ) +
    ggplot2::scale_y_continuous(transform = y_transform) +
    theme_forecasttools()

  return(plot)
}


#' Plot median predictions and prediction
#' intervals as pointintervals against observed data
#'
#' @param scorable_table quantile table with observations,
#' as the output of [scoringutils::as_forecast_quantile()],
#' or as a table coercible to a `scoringutils`-ready quantile
#' forecast via [scoringutils::as_forecast_quantile()]. Must
#' contain the median (0.5) quantile and the endpoint quantiles
#' for the equal-tailed prediction interval specified in
#' `prediction_interval_width`. Must contain a column specifying
#' the forecast horizon. Plot will be faceted into rows and columns,
#' with rows representing different horizons and columns representing
#' all other forecast unit variables beside horizon and target date.
#' @param interval_widths Width(s) of the equal-tailed prediction
#' interval(s) to plot around the median. Must correspond to quantiles
#' available in `scorable_table`. Default `c(0.5, 0.95`)`.
#' @param target_date_col Name of the column in `scorable_table`
#' giving the forecast target date for a given prediction.
#' Default `"target_end_date"` (as in hubverse schema).
#' @param predicted_col Name of the column in `scorable_table`
#' giving the predicted values. Default `"predicted"` (as in
#' the output of [scoringutils::as_forecast_quantile()]. Passed
#' as the `predicted` argument to [scoringutils::as_forecast_quantile()].
#' @param observed_col Name of the column in `scorable_table`
#' giving the observed values. Default `"observed"` (as in
#' the output of [scoringutils::as_forecast_quantile()]. Passed
#' as the `observed` argument to [scoringutils::as_forecast_quantile()].
#' @param quantile_level_col Name of the column in `scorable_table`
#' giving the quantile level for a given row. Default `"quantile_level"`
#' (as in the output of [scoringutils::as_forecast_quantile()].
#' Passed as the `quantile_level` argument to
#' [scoringutils::as_forecast_quantile()].
#' @param horizon_col Name of the column in `scorable_table` containing
#' the forecast horizon for a given row. Default `"horizon"` (as in
#' hubverse schema).
#' @param facet_columns Columns in `scorable_table` by which to
#' facet the resulting plot. Will always facet by `forecast_date_col`.
#' If `NULL`, facet by all the columns that make up the `scoringutils`
#' "forecast unit", as determined by [scoringutils::get_forecast_unit()],
#' except for `target_date_col` and `horizon`. This means that, by default,
#' two distinct predictions for a given target date and horizon should
#' not be visualized on the same facet. To facet _only_ by the forecast date
#' column, pass that column name again, or pass `""`.
#' @param x_label Label for the x axis in the plot. Default
#' `"Date"`.
#' @param y_label Label for the y axis in the plot. Default
#' `"Target"`.
#' @param y_transform Transformation for the y axis in the plot.
#' Passed as the `transform` argument to [ggplot2::scale_y_continuous()]
#' Default `"log10"`.
#' @param quantile_tol Round quantile level values to this many
#' decimal places, to avoid problems with floating point number
#' equality comparisons. Affects both the target quantile level
#' values determined from `prediction_interval_width` and the
#' quantile level values in the `quantile_level_col` column of
#' `scorable_table`. Passed as the `digits` argument to
#' [base::round()]. Default 10.
#' @param predicted_point_size Size for the points showing
#' median predictions. Passed as the `point_size` argument to
#' [ggdist::geom_pointinterval()]. Default `3`.
#' @param predicted_point_shape Shape for the points showing
#' median predictions. Passed as the `shape` argument to
#' [ggdist::geom_pointinterval()]. Default `23` (filled diamonds).
#' @param predicted_point_fill Fill color for the points showing
#' median predictions. Passed as the `point_fill` argument to
#' [ggdist::geom_pointinterval()]. Default `"lightblue"`.
#' @param predicted_interval_color Color for the lines showing
#' predictions intervals. Passed as the `interval_color` argument
#' to [ggdist::geom_pointinterval()] Default `"darkblue"`.
#' @param observed_point_size Size for the points showing observed
#' values. Passed as the `size` argument to [geom_line_point()].
#' Default `3`.
#' @param observed_point_shape Shape for the points showing
#' observed values. Passed as the `shape` argument to
#' [geom_line_point()]. Default `21` (filled circles).
#' @param observed_point_fill Fill color for the points showing
#' observed values. Passed as the `fill` argument to
#' [geom_line_point()]. Default `"darkred"`.
#' @param observed_linetype Type of line to connect the timeseries
#' of observed values. Passed as the `"linetype"` argument to
#' [geom_line_point()]. Default `"solid"`.
#' @param observed_linewidth Width for the line connecting the
#' timeseries of observed values. Passed as the `"linewidth"`
#' argument to [geom_line_point()]. Default `1`.
#' @param observed_linecolor Color for the line connecting the
#' timeseries of observed values. Passed as the `"color"`
#' argument to [geom_line_point()]. Default `"black"`.
#' @return The plot, as a [ggplot2::ggplot()] object.
#' @export
#'
#' @examples
#' scoringutils::example_quantile |>
#'   dplyr::filter(
#'     location == "IT",
#'     target_type == "Cases",
#'     model == "EuroCOVIDhub-ensemble"
#'   ) |>
#'   dplyr::select(-"forecast_date") |>
#'   # otherwise will get single points per facet
#'   # since forecast_date will be treated as a faceting
#'   # variable independent of horizon
#'   plot_pred_obs_by_pointervals()
#'
#' scoringutils::example_quantile |>
#'   dplyr::filter(
#'     location == "IT",
#'     target_type == "Cases",
#'     model %in% c("EuroCOVIDhub-ensemble", "EuroCOVIDhub-baseline")
#'   ) |>
#'   dplyr::select(-"forecast_date") |>
#'   plot_pred_obs_by_pointervals()
#'
plot_pred_obs_pointintervals <- function(
  scorable_table,
  interval_widths = c(0.5, 0.95),
  horizon_col = "horizon",
  target_date_col = "target_end_date",
  predicted_col = "predicted",
  observed_col = "observed",
  quantile_level_col = "quantile_level",
  x_label = "Date",
  y_label = "Target",
  y_transform = "log10",
  quantile_tol = 10,
  predicted_point_size = 3,
  predicted_point_shape = 23,
  predicted_point_fill = "lightblue",
  predicted_interval_color = "darkblue",
  observed_point_size = 3,
  observed_point_shape = 21,
  observed_point_fill = "darkred",
  observed_linetype = "solid",
  observed_linewidth = 1,
  observed_linecolor = "black"
) {
  checkmate::assert_names(
    names(scorable_table),
    must.include = c(
      horizon_col,
      target_date_col,
      predicted_col,
      observed_col,
      quantile_level_col
    )
  )
  ## ensure that forecast is a scoringutils-format
  ## quantile forecast
  scorable_table <- scorable_table |>
    dplyr::rename(
      target_end_date = !!target_date_col,
      horizon = !!horizon_col
    ) |>
    scoringutils::as_forecast_quantile(
      predicted = predicted_col,
      observed = observed_col,
      quantile_level = quantile_level_col
    )

  forecast_unit <- scoringutils::get_forecast_unit(scorable_table)
  plot_group_cols <- purrr::discard(
    forecast_unit,
    ~ .x %in% c("target_end_date", "horizon")
  )
  ## no longer need scoringutils features, so coerce to
  ## tibble to prevent warnings and add grouping variable
  to_plot <- tibble::as_tibble(scorable_table) |>
    dplyr::mutate(plot_group = interaction(dplyr::across(plot_group_cols))) |>
    dplyr::filter(!is.na(plot_group))

  print(to_plot)

  qi <- quantile_table_to_median_qi(
    dplyr::select(to_plot, -"observed"),
    value_col = "predicted",
    quantile_level_col = "quantile_level",
    .width = interval_widths,
    require_all_medians = FALSE,
    require_all_widths = FALSE,
    quantile_tol = quantile_tol
  )
  ## workaround for overplotting issue with `geom_pointinterval` when there
  ## are infinite values, which can cause the mini-point to appear on top of
  ## the full size point, instead of behind.

  obs <- dplyr::select(
    to_plot,
    c("target_end_date", "observed", "plot_group", "horizon")
  )

  plot <- ggplot2::ggplot(
    data = qi,
    mapping = ggplot2::aes(x = .data$target_end_date, group = .data$plot_group)
  ) +
    ggdist::geom_pointinterval(
      mapping = ggplot2::aes(
        y = .data$predicted,
        ymin = .data$.lower,
        ymax = .data$.upper
      ),
      shape = predicted_point_shape,
      point_size = predicted_point_size,
      point_fill = predicted_point_fill,
      interval_color = predicted_interval_color
    ) +
    forecasttools::geom_line_point(
      data = obs,
      mapping = ggplot2::aes(y = .data$observed),
      shape = observed_point_shape,
      linetype = observed_linetype,
      linewidth = observed_linewidth,
      size = observed_point_size,
      fill = observed_point_fill,
      color = observed_linecolor
    ) +
    ggplot2::labs(x = x_label, y = y_label) +
    ggplot2::scale_y_continuous(transform = y_transform) +
    ggplot2::facet_grid(.data$horizon ~ .data$plot_group) +
    theme_forecasttools()

  return(plot)
}
