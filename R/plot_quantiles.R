#' Plot a timeseries of quantiles
#'
#' @param data timeseries of quantiles as tidy data,
#' with one row per timepoint per quantile level
#' @param time_column name of the column in `data`
#' containing timepoints
#' @param observation_column name of the column
#' in data containing observed values at the
#' given quantile levels
#' @param quantile_level_column name of the column
#' in `data` giving the quantile level (e.g.
#' `0.01` for the 0.01 quantile / 1st percentile)
#' @param linesize `size` parameter passed to [ggplot2::geom_line()].
#' Default 2.
#' @param pointsize `size` parameter passed to [ggplot2::geom_point()]
#' Default 4.
#' @param linecolor `color` parameter passed to [ggplot2::geom_line()].
#' Default `"darkblue"`.
#' @param pointcolor `color` parameter passed to [ggplot2::geom_point()]
#' Default `"darkblue"`.
#' @return the resultant plot, as a ggplot object.
#' @export
plot_forecast_quantiles <- function(data,
                                    time_column,
                                    observation_column,
                                    quantile_level_column,
                                    linesize = 2,
                                    pointsize = 4,
                                    pointcolor = "darkblue",
                                    linecolor = "darkblue") {
  return(ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = {{ time_column }},
      y = {{ observation_column }},
      group = {{ quantile_level_column }},
      alpha = 1 - abs({{ quantile_level_column }} - 0.5)
    ),
    data = data
  ) +
    ggplot2::geom_line(
      size = linesize,
      color = linecolor
    ) +
    ggplot2::geom_point(
      size = pointsize,
      color = pointcolor
    ) +
    ggplot2::scale_alpha_continuous(guide = NULL))
}


#' Plot hubverse formatted quantile forecasts for a given
#' location.
#'
#' Given a tibble of data properly formatted
#' for a hubverse schema forecast hub submission,
#' plot a timeseries quantile predictions for
#' a given location.
#'
#' @param location location to plot
#' @param forecast_data hubverse format quantile
#' forecast data, as a [`tibble`][tibble::tibble].
#' @param truth_data hubverse format truth data,
#' as a [`tibble`][tibble::tibble].
#' @param location_format format of the provided location.
#' Permitted formats are `"abbr"` (state/territory
#' or nation two letter USPS abbreviation), `"hub"` (
#' legacy 2-digit FIPS code for states and territories, `US`
#' for the USA as a whole), and `"long_name"` (full English
#' jurisdiction names; not recommended). Default `"abbr"`.
#' @param ytrans axis transform passed to
#' [ggplot2::scale_y_continuous()]. Default `'log10'`.
#' @param linesize `size` parameter passed to
#' [ggplot2::geom_line()]. Default 2.
#' @param pointsize `size` parameter passed to
#' [ggplot2::geom_point()]. Default 4.
#' @param forecast_linecolor `color` parameter passed to
#' [ggplot2::geom_line()] for plotting forecasts.
#' Default "darkblue".
#' @param forecast_pointcolor `color` parameter passed to
#' [ggplot2::geom_point()] for plotting forecasts.
#' Default "darkblue".
#' @param truth_linecolor `color` parameter passed to
#' [ggplot2::geom_line()] for plotting truth data.
#' Default "black".
#' @param truth_pointcolor `color` parameter passed to
#' [ggplot2::geom_point()] for plotting truth data.
#' Default "black".
#' @param target_name Name of the forecast target,
#' for labeling the plot y-axis. Default `NULL`,
#' in which case a value from the `target` column
#' in `forecast_data` will be used.
#' @param autotitle Boolean. Generate an automatic
#' title for the plot from the location name
#' and reference date? Default `TRUE`.
#' @return the plot as a ggplot object
#' @export
plot_hubverse_quantiles_loc <- function(location,
                                        forecast_data,
                                        truth_data,
                                        location_format = "abbr",
                                        ytrans = "log10",
                                        linesize = 2,
                                        pointsize = 4,
                                        forecast_linecolor = "darkblue",
                                        forecast_pointcolor = "darkblue",
                                        truth_linecolor = "black",
                                        truth_pointcolor = "black",
                                        target_name = NULL,
                                        autotitle = TRUE) {
  loc_table <- location_lookup(location, location_format)
  loc_data <- forecast_data |>
    dplyr::filter(
      location == !!loc_table$location_code,
      output_type == "quantile"
    ) |>
    dplyr::rename("date" = "target_end_date")
  loc_truth <- truth_data |>
    dplyr::filter(location == !!loc_table$location_code)


  if (autotitle) {
    loc_name <- loc_table$long_name[1]
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

  plot <- (
    plot_forecast_quantiles(
      loc_data,
      date,
      value,
      output_type_id,
      linesize = linesize,
      pointsize = pointsize,
      linecolor = forecast_linecolor,
      pointcolor = forecast_pointcolor
    ) +
      ggplot2::geom_line(
        data = loc_truth,
        mapping = ggplot2::aes(
          x = date,
          y = value
        ),
        alpha = 1,
        size = linesize,
        color = truth_linecolor,
        inherit.aes = FALSE
      ) +
      ggplot2::geom_point(
        data = loc_truth,
        mapping = ggplot2::aes(
          x = date,
          y = value
        ),
        alpha = 1,
        size = pointsize,
        color = truth_pointcolor,
        inherit.aes = FALSE
      ) +
      ggplot2::scale_y_continuous(
        trans = ytrans
      ) +
      ggplot2::labs(
        y = target_name,
        x = "Date"
      ) +
      ggplot2::ggtitle(plot_title) +
      theme_forecasttools()

  )

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
#' @param forecast_data_path path to hubverse-
#' formatted forecast data, as a single `.csv`.
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
#' @param truth_data_path path to hubverse formatted truth data
#' to plot alongside the forecast quantiles. If NULL,
#' only the forecast quantiles will be plotted. Default NULL.
#' @param start_date first date to plot. If NULL, defaults
#' to the earliest date found between the forecast timeseries (
#' obtained from `forecast_data_path`) and the truth
#' data timeseries (obtained from `truth_data_path`, if
#' provided). Default NULL.
#' @param end_date final date to plot. If NULL, defaults
#' to the latest date found between the forecast timeseries (
#' obtained from `forecast_data_path`) and the truth
#' data timeseries (obtained from `truth_data_path`, if
#' provided). Default NULL.
#' @param location_input_format format of the provided location
#' vector. Permitted formats are `"abbr"` (state/territory
#' or nation two letter USPS abbreviation), `"hub"` (
#' legacy 2-digit FIPS code for states and territories, `US`
#' for the USA as a whole), and `"long_name"` (full English
#' jurisdiction names; not recommended). Default `"abbr"`.
#' @param location_output_format Location format for naming the
#' entries of the output list. Accepts the same string
#' keys as `location_input_format`.
#' @param ytrans axis transform passed to
#' [ggplot2::scale_y_continuous()]. Default `'log10'`.
#' @param linesize `size` parameter passed to
#' [ggplot2::geom_line()]. Default 2.
#' @param pointsize `size` parameter passed to
#' [ggplot2::geom_point()]. Default 4.
#' @param forecast_linecolor `color` parameter
#' passed to [ggplot2::geom_line()] for plotting forecasts.
#' Default `"darkblue"`.
#' @param forecast_pointcolor `color` parameter passed to
#' [ggplot2::geom_point()] for plotting forecasts.
#' Default "darkblue".
#' @param truth_linecolor `color` parameter passed to [ggplot2::geom_line()]
#' for plotting truth data. Default `"black"`.
#' @param truth_pointcolor `color` parameter passed to [ggplot2::geom_point()]
#' for plotting truth data. Default `"black"`.
#' @param autotitle Boolean. Generate a title for the individual
#' plots from the hubverse `reference_date` and the location
#' name? Default `TRUE`.
#' @return a list of ggplot objects of the plots created,
#' one for each location
#' @export
plot_hubverse_quantiles <- function(forecast_data_path,
                                    locations = NULL,
                                    truth_data_path = NULL,
                                    start_date = NULL,
                                    end_date = NULL,
                                    location_input_format = "abbr",
                                    location_output_format = "abbr",
                                    ytrans = "log10",
                                    linesize = 2,
                                    pointsize = 4,
                                    forecast_linecolor = "darkblue",
                                    forecast_pointcolor = "darkblue",
                                    truth_linecolor = "black",
                                    truth_pointcolor = "black",
                                    autotitle = TRUE) {
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
    forecast_data_path,
    col_types = hubverse_cols
  ) |>
    dplyr::filter(
      output_type == "quantile",
    ) |>
    dplyr::mutate(output_type_id = as.numeric(output_type_id))

  if (!is.null(truth_data_path)) {
    truth_cols <- readr::cols_only(
      date = readr::col_date(),
      location = readr::col_character(),
      value = readr::col_double()
    )
    truth_data <- readr::read_csv(truth_data_path,
      col_types = truth_cols
    )
  } else {
    truth_data <- tibble::tibble(
      date = as.Date(numeric(0)),
      location = character(0),
      value = numeric(0)
    )
  }

  if (is.null(locations)) {
    locations <- forecast_data |>
      dplyr::distinct(location) |>
      dplyr::pull()
    loc_table <- location_lookup(locations, "hub")
  } else {
    locations <- base::unique(locations)
    loc_table <- location_lookup(locations, location_input_format)
  }


  if (!is.null(start_date)) {
    truth_data <- truth_data |>
      dplyr::filter(date >= as.Date(!!start_date))
    forecast_data <- forecast_data |>
      dplyr::filter(target_end_date >= as.Date(!!start_date))
  }

  if (!is.null(end_date)) {
    truth_data <- truth_data |>
      dplyr::filter(date <= as.Date(!!end_date))
    forecast_data <- forecast_data |>
      dplyr::filter(target_end_date <= as.Date(!!end_date))
  }

  location_vector <- loc_table |>
    dplyr::pull(!!to_location_table_column(location_output_format))

  result <- lapply(
    location_vector,
    plot_hubverse_quantiles_loc,
    forecast_data = forecast_data,
    truth_data = truth_data,
    location_format = location_output_format,
    ytrans = ytrans,
    linesize = linesize,
    pointsize = pointsize,
    forecast_pointcolor = forecast_pointcolor,
    forecast_linecolor = forecast_linecolor,
    truth_pointcolor = truth_pointcolor,
    truth_linecolor = truth_linecolor,
    autotitle = autotitle
  )
  names(result) <- location_vector

  return(result)
}


#' Save a list of plots as a PDF, with a
#' grid of `nrow` by `ncol` plots per page
#'
#' @param list_of_plots list of plots to save to PDF
#' @param save_path path to which to save the plots
#' @param nrow Number of rows of plots per page
#' (passed to [gridExtra::marrangeGrob()])
#' Default `1`.
#' @param ncol Number of columns of plots per page
#' (passed to [gridExtra::marrangeGrob()]).
#' Default `1`.
#' @param width page width in device units (passed to
#' [ggplot2::ggsave()]). Default `8.5`.
#' @param height page height in device units (passed to
#' [ggplot2::ggsave()]). Default `11`.
#' @return `TRUE` on success.
#' @export
plots_to_pdf <- function(list_of_plots,
                         save_path,
                         nrow = 1,
                         ncol = 1,
                         width = 8.5,
                         height = 11) {
  if (!stringr::str_ends(
    save_path, ".pdf"
  )) {
    cli::cli_abort("Filepath must end with `.pdf`")
  }
  cli::cli_inform("Saving plots to {save_path}")
  ggplot2::ggsave(
    filename = save_path,
    plot = gridExtra::marrangeGrob(list_of_plots,
      nrow = nrow,
      ncol = ncol
    ),
    width = width,
    height = height
  )
  return(TRUE)
}
