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
#' Gather hub quantile forecasts.
#'
#' @param hub_path Local path to hub
#'
#' @return Forecast data as a table.
#' @export
gather_hub_quantile_forecasts <- function(hub_path) {
  hub_connection <- hubData::connect_hub(hub_path)
  forecasts <- hub_connection |>
    dplyr::filter(output_type == "quantile") |>
    hubData::collect_hub()
  return(forecasts)
}

#' Gather location data from a forecast hub.
#'
#' @param hub_path Local path to forecast hub.
#' @param location_file The path to the data file for
#' location data relative to forecast hub directory.
#' Defaults to expected path in the FluSight forecast hub.
#'
#' @return Table of location data.
#' @export
gather_hub_location_data <- function(hub_path,
                                     location_file =
                                       fs::path(
                                         "auxiliary-data",
                                         "locations.csv"
                                       )) {
  location_data_path <- fs::path(hub_path, location_file)
  if (location_data_path |> fs::file_exists()) {
    location_data <- readr::read_csv(location_data_path)
    return(location_data)
  } else {
    cfl <- location_data_path
    cli::cli_alert_danger("Cannot find location data file at {.path {cfl}}.")
  }
}

#' Gather target truth data from a forecast hub.
#'
#' @param hub_path Local path to hub.
#' @param local_datapath The local path to the truth data file for
#' relative to forecast hub directory.
#' Defaults to expected path in the FluSight forecast hub.
#'
#' @return Table of target truth data
#' @export
gather_hub_target_data <- function(hub_path,
                                   local_datapath = fs::path(
                                     "target-data",
                                     "target-hospital-admissions.csv"
                                   )) {
  truth_data_path <- fs::path(hub_path, local_datapath)
  if (truth_data_path |> fs::file_exists()) {
    target_data <- readr::read_csv(truth_data_path)
    return(target_data)
  } else {
    cfl <- truth_data_path
    cli::cli_alert_danger("Cannot find truth data file at {.path {cfl}}.")
  }
}
