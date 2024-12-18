#' Plot a timeseries of quantiles.
#'
#' @param data Timeseries of quantiles as tidy data,
#' with one row per timepoint per quantile level.
#' @param time_column Name of the column in `data`
#' containing timepoint values, as a string.
#' @param value_column Name of the column
#' in data containing the timeseries values at the
#' given quantile levels, as a string.
#' @param quantile_level_column Name of the column in `data`
#' containing indicating which quantile level the row contains,
#' as a string.
#' @param linewidth `linewidth` parameter passed to
#' [ggplot2::geom_line()]. Default `2`.
#' @param pointsize `size` parameter passed to
#' [ggplot2::geom_point()] Default `4`.
#' @param linecolor `color` parameter passed to
#' [ggplot2::geom_line()]. Default `"darkblue"`.
#' @param pointcolor `color` parameter passed to
#' [ggplot2::geom_point()] Default `"darkblue"`.
#' @return The plot, as a ggplot object.
#' @export
plot_quantile_timeseries <- function(data,
                                     time_column,
                                     value_column,
                                     quantile_level_column,
                                     linewidth = 2,
                                     pointsize = 4,
                                     pointcolor = "darkblue",
                                     linecolor = "darkblue") {
  return(ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = .data[[time_column]],
      y = .data[[value_column]],
      group = .data[[quantile_level_column]],
      alpha = 1 - abs(.data[[quantile_level_column]] - 0.5)
    ),
    data = data
  ) +
    ggplot2::geom_line(
      linewidth = linewidth,
      color = linecolor
    ) +
    ggplot2::geom_point(
      size = pointsize,
      color = pointcolor
    ) +
    ggplot2::scale_alpha_continuous(guide = NULL))
}
