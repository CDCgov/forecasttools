#' Plot a hubverse formatted quantile forecast timeseries
#' for a single location.
#'
#' Given a tibble of data properly formatted
#' for a hubverse schema forecast hub submission,
#' plot a timeseries of quantile predictions for
#' a given location.
#'
#' @param location location to plot
#' @param forecast_data hubverse format quantile
#' forecast data, as a [`tibble`][tibble::tibble].
#' @param observed_data observed data,
#' as a [`tibble`][tibble::tibble].
#' @param location_format format of the provided location.
#' Permitted formats are `"abbr"` (state/territory
#' or nation two letter USPS abbreviation), `"hub"` (
#' legacy 2-digit FIPS code for states and territories, `US`
#' for the USA as a whole), and `"long_name"` (full English
#' jurisdiction names; not recommended). Default `"abbr"`.
#' @param y_transform axis transform passed as the
#' `transform` argument to [ggplot2::scale_y_continuous()].
#' Default `"log10"`.
#' @param linewidth `linewidth` parameter passed to
#' [ggplot2::geom_line()]. Default `2`.
#' @param pointsize `size` parameter passed to
#' [ggplot2::geom_point()]. Default `4`.
#' @param forecast_linecolor `color` parameter passed to
#' [ggplot2::geom_line()] for plotting forecasts.
#' Default `"darkblue"`.
#' @param forecast_pointcolor `color` parameter passed to
#' [ggplot2::geom_point()] for plotting forecasts.
#' Default `"darkblue"`.
#' @param obs_linecolor `color` parameter passed to
#' [ggplot2::geom_line()] for plotting observed data.
#' Default `"black"`.
#' @param obs_pointcolor `color` parameter passed to
#' [ggplot2::geom_point()] for plotting observed data.
#' Default `"black"`.
#' @param target_name Name of the forecast target,
#' for labeling the plot y-axis. Default `NULL`,
#' in which case a value from the `target` column
#' in `forecast_data` will be used.
#' @param autotitle Boolean. Generate an automatic
#' title for the plot from the location name
#' and reference date? Default `TRUE`.
#' @return the plot as a ggplot object
#' @export
plot_hubverse_loc_quant_ts <- function(
  location,
  forecast_data,
  observed_data,
  location_format = "abbr",
  y_transform = "log10",
  linewidth = 2,
  pointsize = 4,
  forecast_linecolor = "darkblue",
  forecast_pointcolor = "darkblue",
  obs_linecolor = "black",
  obs_pointcolor = "black",
  target_name = NULL,
  autotitle = TRUE
) {
  loc <- location_lookup(location, location_format, "hub")
  loc_data <- forecast_data |>
    dplyr::filter(
      .data$location == !!loc,
      .data$output_type == "quantile"
    ) |>
    dplyr::rename(date = "target_end_date")
  loc_obs <- observed_data |>
    dplyr::filter(.data$location == !!loc)

  if (autotitle) {
    loc_name <- location_lookup(location, location_format, "name")
    plot_date <- loc_data$reference_date[1]
    plot_title <- stringr::str_glue(
      "{loc_name} forecasts of {plot_date}"
    )
  } else {
    plot_title <- NULL
  }

  if (is.null(target_name)) {
    target_name <- loc_data$target[1]
  }

  plot <- loc_data |>
    plot_quantile_timeseries(
      time_column = "date",
      value_column = "value",
      quantile_level_column = "output_type_id",
      linewidth = linewidth,
      pointsize = pointsize,
      linecolor = forecast_linecolor,
      pointcolor = forecast_pointcolor
    ) +
    ggplot2::geom_line(
      data = loc_obs,
      mapping = ggplot2::aes(
        x = .data$date,
        y = .data$value
      ),
      alpha = 1,
      linewidth = linewidth,
      color = obs_linecolor,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = loc_obs,
      mapping = ggplot2::aes(
        x = .data$date,
        y = .data$value
      ),
      alpha = 1,
      size = pointsize,
      color = obs_pointcolor,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_y_continuous(
      transform = y_transform
    ) +
    ggplot2::labs(
      y = target_name,
      x = "Date"
    ) +
    ggplot2::ggtitle(plot_title) +
    theme_forecasttools()

  return(plot)
}


#' Plot hubverse formatted forecasts
#' for all or a subset of forecasted
#' locations.
#'
#' Plot quantiles given hubverse
#' formatted data for all
#' locations in the dataset or
#' a subset of them.
#'
#' @param forecast_file_path path to hubverse-
#' formatted forecast data, as a single `.csv` file.
#' @param locations set of locations to plot. If NULL,
#' all locations are plotted. Otherwise, a vector
#' of location values to plot, as USPS-style
#' abbreviations (e.g. `c("US", "AL", "AK"`),
#' US hubverse submission location codes (
#' e.g. `c("US", 01, 02)`), or full English
#' jurisdiction names
#' (e.g. `c("United States, "Alabama", "Alaska")`.
#' Default `NULL`.
#' @param location_input_format format of the provided location
#' vector if it is provided.
#' Permitted formats are `"abbr"` (state/territory
#' or nation two letter USPS abbreviation), `"hub"` (
#' legacy 2-digit FIPS code for states and territories, `US`
#' for the USA as a whole), and `"long_name"` (full English
#' jurisdiction names; not recommended). Default `"abbr"`.
#' @param location_output_format how to code locations for
#' the output vector.
#' Permitted formats are `"abbr"` (state/territory
#' or nation two letter USPS abbreviation), `"hub"` (
#' legacy 2-digit FIPS code for states and territories, `US`
#' for the USA as a whole), and `"long_name"` (full English
#' jurisdiction names; not recommended). Default `"abbr"`.
#' @param observed_data_path path to observed data
#' to plot alongside the forecast quantiles. If `NULL`,
#' only the forecast quantiles will be plotted. Default `NULL`.
#' @param start_date first date to plot. If NULL, defaults
#' to the earliest date found between the forecast timeseries (
#' obtained from `forecast_file_path`) and the observed
#' data timeseries (obtained from `observed_data_path`, if
#' provided). Default NULL.
#' @param end_date final date to plot. If NULL, defaults
#' to the latest date found between the forecast timeseries (
#' obtained from `forecast_file_path`) and the observed
#' data timeseries (obtained from `observed_data_path`, if
#' provided). Default NULL.
#' @param location_input_format Format of the provided location
#' vector. See [to_location_table_column()] for valid formats.
#' @param location_output_format Location format for naming the
#' entries of the output list. See [to_location_table_column()]
#' for valid formats.
#' @param y_transform axis transform passed as the `transform`
#' argument to [ggplot2::scale_y_continuous()]. Default `"log10"`.
#' @param linewidth `linewidth` parameter passed to
#' [ggplot2::geom_line()]. Default `2`.
#' @param pointsize `size` parameter passed to
#' [ggplot2::geom_point()]. Default `4`.
#' @param forecast_linecolor `color` parameter
#' passed to [ggplot2::geom_line()] for plotting forecasts.
#' Default `"darkblue"`.
#' @param forecast_pointcolor `color` parameter passed to
#' [ggplot2::geom_point()] for plotting forecasts.
#' Default `"darkblue"`.
#' @param obs_linecolor `color` parameter passed to [ggplot2::geom_line()]
#' for plotting observed data. Default `"black"`.
#' @param obs_pointcolor `color` parameter passed to
#' [ggplot2::geom_point()] for plotting observed data. Default `"black"`.
#' @param autotitle Generate a title for the individual
#' plots from the hubverse `reference_date` and the location
#' name? Boolean, default `TRUE`.
#' @return a list of ggplot objects of the plots created,
#' one for each location
#' @export
plot_hubverse_file_quantiles <- function(
  forecast_file_path,
  locations = NULL,
  observed_data_path = NULL,
  start_date = NULL,
  end_date = NULL,
  location_input_format = "abbr",
  location_output_format = "abbr",
  y_transform = "log10",
  linewidth = 2,
  pointsize = 4,
  forecast_linecolor = "darkblue",
  forecast_pointcolor = "darkblue",
  obs_linecolor = "black",
  obs_pointcolor = "black",
  autotitle = TRUE
) {
  start_date <- if (!is.null(start_date)) as.Date(start_date) else NULL
  end_date <- if (!is.null(end_date)) as.Date(end_date) else NULL
  hubverse_cols <- readr::cols(
    reference_date = readr::col_date(),
    target = readr::col_character(),
    horizon = readr::col_integer(),
    target_end_date = readr::col_date(),
    location = readr::col_character(),
    output_type = readr::col_character(),
    output_type_id = readr::col_double(),
    value = readr::col_double()
  )
  forecast_data <- readr::read_csv(
    forecast_file_path,
    col_types = hubverse_cols
  ) |>
    dplyr::filter(
      .data$output_type == "quantile",
    ) |>
    dplyr::mutate(output_type_id = as.numeric(.data$output_type_id))

  if (!is.null(observed_data_path)) {
    observed_cols <- readr::cols_only(
      date = readr::col_date(),
      location = readr::col_character(),
      value = readr::col_double()
    )
    obs_data <- readr::read_csv(observed_data_path, col_types = observed_cols)
  } else {
    obs_data <- tibble::tibble(
      date = as.Date(numeric(0)),
      location = character(0),
      value = numeric(0)
    )
  }

  forecast_data <- forecast_data |>
    dplyr::filter(
      nullable_comparison(.data$target_end_date, ">=", !!start_date),
      nullable_comparison(.data$target_end_date, "<=", !!end_date)
    )

  obs_data <- obs_data |>
    dplyr::filter(
      nullable_comparison(.data$date, ">=", !!start_date),
      nullable_comparison(.data$date, "<=", !!end_date)
    )

  if (is.null(locations)) {
    locations <- forecast_data |>
      dplyr::distinct(.data$location) |>
      dplyr::pull()
    location_vector <- location_lookup(locations, "hub", location_output_format)
  } else {
    locations <- base::unique(locations)
    location_vector <- location_lookup(
      locations,
      location_input_format,
      location_output_format
    )
  }

  list_of_plots <- purrr::map(
    location_vector |> purrr::set_names(),
    \(loc) {
      plot_hubverse_loc_quant_ts(
        loc,
        forecast_data = forecast_data,
        observed_data = obs_data,
        location_format = location_output_format,
        y_transform = y_transform,
        linewidth = linewidth,
        pointsize = pointsize,
        forecast_pointcolor = forecast_pointcolor,
        forecast_linecolor = forecast_linecolor,
        obs_pointcolor = obs_pointcolor,
        obs_linecolor = obs_linecolor,
        autotitle = autotitle
      )
    }
  )

  return(list_of_plots)
}
