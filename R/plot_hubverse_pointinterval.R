#' Plot a table of hubverse-formatted forecasts
#' as pointintervals
#'
#' @param hubverse_table Hubverse table, as a [`tibble`][tibble::tibble()]
#' @param horizon horizons to plot, as a vector of integers. If `NULL`,
#' plot all available, each in its own facet. Default `NULL`.
#' @param locations set of locations to plot. If NULL,
#' all locations are plotted. Otherwise, a vector
#' of location values to plot, as USPS-style
#' abbreviations (e.g. `c("US", "AL", "AK"`),
#' US hubverse submission location codes (
#' e.g. `c("US", 01, 02)`), or full English
#' jurisdiction names
#' (e.g. `c("United States, "Alabama", "Alaska")`.
#' Default `NULL`.
#' @param location_column_format format of the hubverse table
#' `location` column.
#' Permitted formats are `"abbr"` (state/territory
#' or nation two letter USPS abbreviation), `"hub"` (
#' legacy 2-digit FIPS code for states and territories, `US`
#' for the USA as a whole), and `"long_name"` (full English
#' jurisdiction names; not recommended). Default `"hub"`.
#' @param location_output_format how to code locations in the output plot.
#' Permitted formats are `"abbr"` (state/territory
#' or nation two letter USPS abbreviation), `"hub"` (
#' legacy 2-digit FIPS code for states and territories, `US`
#' #' for the USA as a whole), and `"long_name"` (full English
#' jurisdiction names). Default `"abbr"`.
#' @return A ggplot2 plot of the forecasts as pointintervals
#' @export
plot_hubverse_pointintervals <- function(hubverse_table,
                                         horizons = NULL,
                                         point_estimate_quantile = 0.5,
                                         lower_limit_quantile = 0.025,
                                         upper_limit_quantile = 0.975,
                                         location_input_format = "hub",
                                         location_output_format = "abbr") {
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
      "location" = location_lookup(
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
    dplyr::mutate("location" = factor(
      location,
      levels = loc_levels,
      ordered = TRUE
    ))

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
