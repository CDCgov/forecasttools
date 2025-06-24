#' Plot a table of hubverse-formatted forecasts
#' as pointintervals
#'
#' @param hubverse_table Hubverse table, as a [`tibble`][tibble::tibble()]
#' @param horizons horizons to plot, as a vector of integers. If `NULL`,
#' plot all available, each in its own facet. Default `NULL`.
#' @param point_estimate_quantile Quantile to plot as the point estimate.
#' Default `0.5`, the median.
#' @param lower_limit_quantile Quantile to plot as the lower bound of
#' the interval. Default `0.025`.
#' @param upper_limit_quantile Quantile to plot as the upper bound of
#' the interval. Default `0.975`.
#' @param location_input_format Format of the hubverse table
#' `location` column. See [to_us_location_table_column()]
#' for valid formats.
#' @param location_output_format How to code locations in the output plot.
#' See [to_us_location_table_column()] for valid formats.
#' @return A ggplot2 plot of the forecasts as pointintervals
#' @export
plot_hubverse_pointintervals <- function(
  hubverse_table,
  horizons = NULL,
  point_estimate_quantile = 0.5,
  lower_limit_quantile = 0.025,
  upper_limit_quantile = 0.975,
  location_input_format = "hub",
  location_output_format = "abbr"
) {
  if (is.null(horizons)) {
    horizons <- unique(hubverse_table$horizon)
  }

  pivoted <- hubverse_table |>
    pivot_hubverse_quantiles_wider(
      pivot_quantiles = c(
        "point" = point_estimate_quantile,
        "lower" = lower_limit_quantile,
        "upper" = upper_limit_quantile
      )
    ) |>
    dplyr::mutate(
      "location" = us_location_recode(
        .data$location,
        location_input_format,
        location_output_format
      )
    ) |>
    dplyr::filter(.data$horizon %in% horizons)

  ## order by the median at the first horizon plotted
  loc_levels <- pivoted |>
    dplyr::filter(
      .data$horizon == min(.data$horizon)
    ) |>
    dplyr::arrange(.data$point) |>
    dplyr::pull("location")

  pivoted <- pivoted |>
    dplyr::mutate(
      location = factor(
        .data$location,
        levels = !!loc_levels,
        ordered = TRUE
      )
    )

  plot <- pivoted |>
    ggplot2::ggplot(ggplot2::aes_string(
      y = "location",
      x = "point",
      xmin = "lower",
      xmax = "upper"
    )) +
    ggdist::geom_pointinterval() +
    ggplot2::facet_wrap(~horizon ~ target) +
    ggplot2::labs(x = "target value") +
    theme_forecasttools()

  return(plot)
}
