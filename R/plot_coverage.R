#' Plot of empirical forecast coverage by
#' reference date.
#'
#' @param scored Output of [scoringutils::score()], not yet
#' summarized, containing a column for coverage at the required
#' coverage level.
#' @param coverage_level Decimal coverage level to plot, e.g.
#' `0.95` or `0.5`.
#' @param coverage_col Name of the column corresponding to that
#' coverage level in `scored`. Default `interval_coverage_<x>`
#' where <x> is the coverage level as a percentage, e.g. if
#' `coverage_level = 0.95`, then if `coverage_col` is not specified,
#' `plot_coverage_by_date` will look for a column named
#' `interval_coverage_95`, as this is the default name for
#' interval coverage columns produced by [scoringutils::score()] and
#' [scoringutils::summarise_scores()]
#' @param date_col Column containing dates, which will become the
#' x-axis in the empirical coverage by date plot. This can be
#' a target date, but more commonly it will be a forecast date,
#' or `reference_date` indicating when the forecast was produced.
#' Default `"reference_date"`, the standard name for a forecast
#' date in the hubverse schema.
#' @param group_cols Other columns to group by, in addition to forecast
#' date. These will become facets in the output ggplot. Default
#' `c("target", "horizon")` (i.e. group by forecasting target and
#' forecast horizon.
#' @param y_transform transform for the y axis, a string. Passed
#' as the `transform` argument to [ggplot2::scale_y_continuous()].
#' Default `"identity"`.
#' @param y_labels labeling scheme for the y axis. Passed as
#' the `labels` argument to [ggplot2::scale_y_continuous()]. Default
#' [scales::label_percent()].
#' @return A ggplot of the empirical coverage.
#' @export
plot_coverage_by_date <- function(scored,
                                  coverage_level,
                                  coverage_col = NULL,
                                  date_col = "reference_date",
                                  group_cols = c("target", "horizon"),
                                  y_transform = "identity",
                                  y_labels = scales::label_percent()) {
  if (is.null(coverage_col)) {
    coverage_col <-
      glue::glue("interval_coverage_{coverage_level * 100}")
  }

  summarized <- scored |>
    scoringutils::summarise_scores(
      by = c(date_col, group_cols)
    ) |>
    tibble::tibble()

  fig <- ggplot2::ggplot(
    data = summarized,
    mapping = ggplot2::aes(
      x = .data[[date_col]],
      y = .data[[coverage_col]],
    )
  ) +
    ggplot2::geom_hline(yintercept = coverage_level) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_line(linewidth = 2) +
    ggplot2::scale_y_continuous(
      transform = y_transform,
      labels = y_labels
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 1))

  ## facet wrap if one or many group cols, facet grid
  ## if exactly two
  if (length(group_cols) == 1 || length(group_cols > 2)) {
    fig <- fig + ggplot2::facet_wrap(group_cols)
  } else if (length(group_cols) == 2) {
    fig <- fig + ggplot2::facet_grid(reformulate(
      group_cols[1], group_cols[2]
    ))
  }
  return(fig)
}
