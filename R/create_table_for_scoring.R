#' Download a forecast hub from `hub_url` to directory `hub_path`.
#'
#' @param hub_path Path to where forecast hub should be downloaded to.
#' @param force If TRUE then if `hub_path` directory already exists,
#' first delete it and then download the forecast hub.
#' @param hub_url URL to forecast hub on github.
#'
#' @return Absolute path to downloaded hub.
#' @export
download_hub <- function(hub_url, hub_path, force = FALSE) {
  if (fs::dir_exists(hub_path) && !force) {
    cfl <- hub_path
    cli::cli_abort(
      paste0(
        "Directory at {.path {cfl}} exists, ",
        "if you want to replace this directory ",
        "then use `force = TRUE`."
      )
    )
  }
  if (fs::dir_exists(hub_path) && force) {
    fs::dir_delete(hub_path)
  }
  return(gert::git_clone(hub_url, hub_path))
}

#' Update the forecast hub located at the path `hub_path`.
#'
#' @param hub_path Path to forecast hub.
#'
#' @export
update_hub <- function(hub_path) {
  if (fs::dir_exists(hub_path)) {
    gert::git_fetch(repo = I(hub_path), prune = TRUE)
  } else {
    cfl <- hub_path
    cli::cli_abort(
      paste0(
        "Cannot find directory at {.path {cfl}}. ",
        "If you wish to clone a repo you can use ",
        "`forecasttools::download_hub`."
      )
    )
  }
}

#' Collect and reformat hub forecasts for scoring.
#'
#' Gather hub forecasts with stripped out `horizon == -1`
#' and rename variables in a way to match `scoringutils`
#' expectations.
#'
#' @param hub_path Local path to hub
#'
#' @return Forecast data as a table.
#' @export
gather_hub_forecast_data <- function(hub_path) {
  hub_connection <- hubData::connect_hub(hub_path)
  forecasts <- hub_connection |>
    dplyr::filter(.data$output_type == "quantile") |>
    dplyr::filter(.data$horizon >= 0) |>
      dplyr::rename("prediction" = "value",
                    "forecast_date" = "reference_date",
                    "quantile" = "output_type_id") |>
    dplyr::collect()
  return(forecasts)
}

#' Gather location data from the forecast hub.
#'
#' This includes population
#' size for the states.
#'
#' @param hub_path Local path to forecast hub.
#' @param local_datapath The local path to the data file for
#' location data relative to forecast hub directory.
#' Defaults to expected path in the FluSight forecast hub.
#'
#' @return Table of location data.
#' @export
gather_location_data <-
  function(hub_path,
           local_datapath = fs::path("auxiliary-data", "locations.csv")) {
    location_data_path <- fs::path(hub_path, local_datapath)
    if (location_data_path |> fs::file_exists()) {
      location_data <- readr::read_csv(location_data_path)
      return(location_data)
    } else {
      cfl <- location_data_path
      cli::cli_alert_danger("Cannot find location data file at {.path {cfl}}.")
    }
  }

#' Gather target truth data
#' from the forecast hub and prepare it
#' for use with `scoringutils`.
#'
#' This requires renaming the `value` col to
#' `truth_value` to comply with `scoringutils`
#' expectations.
#'
#' @param hub_path Local path to hub.
#' @param local_datapath The local path to the truth data file for
#' relative to forecast hub directory.
#' Defaults to expected path in the FluSight forecast hub.
#'
#' @return Table of target truth data
#' @export
gather_target_data <- function(hub_path,
                               local_datapath = fs::path(
                                 "target-data",
                                 "target-hospital-admissions.csv"
                               )) {
  truth_data_path <- fs::path(hub_path, local_datapath)
  if (truth_data_path |> fs::file_exists()) {
    target_data <-
      readr::read_csv(truth_data_path) |>
      dplyr::rename("true_value" = "value")
    return(target_data)
  } else {
    cfl <- truth_data_path
    cli::cli_alert_danger("Cannot find truth data file at {.path {cfl}}.")
  }
}

#' Create a table for scoring hub models
#' with `scoringutils`
#'
#' Create a table for scoring models
#' submitted to a forecast hub using
#' `scoringutils` by doing needed formatting
#' and attaching truth data.
#'
#' Requires a local version of the
#' forecast hub at `hub_path`.
#'
#' @param hub_path Local path to forecast hub.
#'
#' @return Table with submitted model forecasts
#' and truth data, as a [tibble::tibble()]
#' @export
create_table_for_scoring <- function(hub_path) {
  forecasts_only <- gather_hub_forecast_data(hub_path)
  target_data <- gather_target_data(hub_path)
  forecasts_and_targets <- dplyr::left_join(
    forecasts_only,
    target_data,
    by = dplyr::join_by(
      "location" == "location",
      "target_end_date" == "date"
      )
  ) |>
    dplyr::rename("model" = "model_id") |>
    dplyr::mutate("target_end_date" = as.Date(.data$target_end_date)) |>
    dplyr::mutate("quantile" = as.numeric(.data$quantile)) |>
    dplyr::select(
      "forecast_date",
      "target_end_date",
      "horizon",
      "location",
      "quantile",
      "prediction",
      "location_name",
      "true_value",
      "model"
    )

  return(forecasts_and_targets)
}
